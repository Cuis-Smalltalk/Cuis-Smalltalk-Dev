Class {
	#name : 'TonelReader',
	#superclass : 'Object',
	#instVars : [
		'stream',
		'currentLine'
	],
	#category : 'Tonel',
	#package : 'Tonel'
}

{ #category : 'instance creation' }
TonelReader class >> readStream: aStream [
	^ self new
		stream: aStream;
		parse
]

{ #category : 'parsing' }
TonelReader >> metadataAt: key in: lines [
	| prefix line raw |
	prefix := '#', key, ' : '.
	line := lines detect: [ :each | each withBlanksTrimmed beginsWith: prefix ] ifNone: [ ^ nil ].
	raw := (line copyFrom: ((line findString: ': ' startingAt: 1) + 2) to: line size) withBlanksTrimmed.
	(raw endsWith: ',') ifTrue: [ raw := raw allButLast ].
	^ self unquote: raw
]

{ #category : 'parsing' }
TonelReader >> metadataListAt: key in: lines [
	| prefix startIndex values index line trimmed |
	prefix := '#', key, ' : ['.
	startIndex := lines findFirst: [ :each | each withBlanksTrimmed beginsWith: prefix ].
	startIndex = 0 ifTrue: [ ^ #() ].
	values := OrderedCollection new.
	index := startIndex + 1.
	[ index <= lines size ] whileTrue: [
		line := lines at: index.
		trimmed := line withBlanksTrimmed.
		trimmed = ']' ifTrue: [ ^ values asArray ].
		trimmed = '],' ifTrue: [ ^ values asArray ].
		trimmed notEmpty ifTrue: [
			(trimmed endsWith: ',') ifTrue: [ trimmed := trimmed allButLast ].
			values add: (self unquote: trimmed) ].
		index := index + 1 ].
	^ values asArray
]

{ #category : 'parsing' }
TonelReader >> parse [
	| definition lines line |
	definition := TonelClassDefinition new.
	lines := OrderedCollection new.
	line := stream nextLine.
	line = '"' ifTrue: [
		| commentStream next |
		commentStream := WriteStream on: ''.
		[ (next := stream nextLine) = '"' ] whileFalse: [
			commentStream nextPutAll: next.
			stream atEnd ifFalse: [ commentStream newLine ] ].
		definition comment: commentStream contents.
		line := stream nextLine ].
	[ line notNil and: [ (line withBlanksTrimmed beginsWith: 'Class {') not ] ] whileTrue: [
		line := stream nextLine ].
	lines add: line.
	[ stream atEnd ] whileFalse: [
		line := stream nextLine.
		lines add: line.
		line withBlanksTrimmed = '}' ifTrue: [ ^ self parseMethodsInto: (self parseTypeDefinitionFrom: lines into: definition) ] ].
	^ definition
]

{ #category : 'parsing' }
TonelReader >> parseMethodHeader: headerLine metadata: metadataLine into: definition [
	| protocol declaration method sourceStream classSide className selectorSource depth inString inComment |
	protocol := self unquote: ((metadataLine copyAfter: $:) copyUpTo: $}) withBlanksTrimmed.
	declaration := (headerLine copyUpToLast: $[) withBlanksTrimmed.
	classSide := declaration includesSubString: ' class >> '.
	classSide
		ifTrue: [
			| markerStart |
			markerStart := declaration findString: ' class >> ' startingAt: 1.
			className := declaration copyFrom: 1 to: markerStart - 1.
			selectorSource := declaration copyFrom: markerStart + ' class >> ' size to: declaration size ]
		ifFalse: [
			| markerStart |
			markerStart := declaration findString: ' >> ' startingAt: 1.
			className := declaration copyFrom: 1 to: markerStart - 1.
			selectorSource := declaration copyFrom: markerStart + ' >> ' size to: declaration size ].
	sourceStream := WriteStream on: ''.
	sourceStream nextPutAll: selectorSource.
	depth := 1.
	inString := false.
	inComment := false.
	[ stream atEnd ] whileFalse: [
		| line index |
		line := stream nextLine.
		sourceStream newLine.
		index := 1.
		[ index <= line size ] whileTrue: [
			| character handled |
			character := line at: index.
			handled := false.
			inString ifTrue: [
				sourceStream nextPut: character.
				character = $' ifTrue: [
					(index < line size and: [ (line at: index + 1) = $' ])
						ifTrue: [ index := index + 1. sourceStream nextPut: $' ]
						ifFalse: [ inString := false ] ].
				handled := true ].
			(handled not and: [ inComment ]) ifTrue: [
				sourceStream nextPut: character.
				character = $" ifTrue: [
					(index < line size and: [ (line at: index + 1) = $" ])
						ifTrue: [ index := index + 1. sourceStream nextPut: $" ]
						ifFalse: [ inComment := false ] ].
				handled := true ].
			(handled not and: [ character = $$ ]) ifTrue: [
				sourceStream nextPut: character.
				index < line size ifTrue: [
					index := index + 1.
					sourceStream nextPut: (line at: index) ].
				handled := true ].
			(handled not and: [ character = $' ]) ifTrue: [
				inString := true.
				sourceStream nextPut: character.
				handled := true ].
			(handled not and: [ character = $" ]) ifTrue: [
				inComment := true.
				sourceStream nextPut: character.
				handled := true ].
			(handled not and: [ character = $[ ]) ifTrue: [
				depth := depth + 1.
				sourceStream nextPut: character.
				handled := true ].
			(handled not and: [ character = $] ]) ifTrue: [
				depth := depth - 1.
				depth = 0 ifTrue: [
					method := TonelMethodDefinition new.
					method
						className: className asSymbol;
						classSide: classSide;
						protocol: protocol asSymbol;
						source: sourceStream contents;
						selector: (self selectorFromMethodSource: sourceStream contents).
					definition methods add: method.
					^ self ].
				sourceStream nextPut: character.
				handled := true ].
			handled ifFalse: [ sourceStream nextPut: character ].
			index := index + 1 ] ]
]

{ #category : 'parsing' }
TonelReader >> parseMethodsInto: definition [
	| line metadata |
	[ stream atEnd ] whileFalse: [
		line := stream nextLine.
		(line notNil and: [ line withBlanksTrimmed beginsWith: '{' ]) ifTrue: [
			metadata := line.
			self parseMethodHeader: stream nextLine metadata: metadata into: definition ] ].
	^ definition
]

{ #category : 'parsing' }
TonelReader >> parseTypeDefinitionFrom: lines into: definition [
	definition
		className: (self metadataAt: 'name' in: lines) asSymbol;
		superclassName: (self metadataAt: 'superclass' in: lines) asSymbol;
		instanceVariableNames: (self metadataListAt: 'instVars' in: lines);
		classVariableNames: (self metadataListAt: 'classVars' in: lines);
		poolDictionaries: (self metadataListAt: 'pools' in: lines);
		classInstanceVariableNames: (self metadataListAt: 'classInstVars' in: lines);
		category: (self metadataAt: 'category' in: lines) asSymbol;
		package: (self metadataAt: 'package' in: lines) asSymbol.
	(self metadataAt: 'tag' in: lines) ifNotNil: [ :tagName | definition tag: tagName asSymbol ].
	^ definition
]

{ #category : 'parsing' }
TonelReader >> selectorFromMethodSource: sourceString [
	| firstLine tokens selector |
	firstLine := sourceString readStream nextLine.
	tokens := firstLine substrings.
	(tokens anySatisfy: [ :each | each includes: $: ]) ifTrue: [
		selector := WriteStream on: ''.
		tokens do: [ :each | (each includes: $:) ifTrue: [ selector nextPutAll: each ] ].
		^ selector contents asSymbol ].
	^ tokens first asSymbol
]

{ #category : 'parsing' }
TonelReader >> stream: aStream [
	stream := aStream
]

{ #category : 'parsing' }
TonelReader >> unquote: aString [
	| trimmed |
	trimmed := aString withBlanksTrimmed.
	((trimmed size >= 2) and: [ trimmed first = $' and: [ trimmed last = $' ] ])
		ifFalse: [ ^ trimmed ].
	^ (trimmed copyFrom: 2 to: trimmed size - 1) copyReplaceAll: '''''' with: ''''
]
