'From Cuis7.7 [latest update: #7777] on 22 May 2026 at 00:00:00 am'!
'Description Cuis-native Tonel export/import support based on the Tonel v3 source format.'!
!provides: 'Tonel' 1 1!
!requires: 'Cuis-Base' 60 5557 nil!
SystemOrganization addCategory: #Tonel!


!classDefinition: #TonelClassDefinition category: #Tonel!
Object subclass: #TonelClassDefinition
	instanceVariableNames: 'comment className superclassName instanceVariableNames classVariableNames poolDictionaries classInstanceVariableNames category package tag methods'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tonel'!
!classDefinition: 'TonelClassDefinition class' category: #Tonel!
TonelClassDefinition class
	instanceVariableNames: ''!

!classDefinition: #TonelImageExporter category: #Tonel!
Object subclass: #TonelImageExporter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tonel'!
!classDefinition: 'TonelImageExporter class' category: #Tonel!
TonelImageExporter class
	instanceVariableNames: ''!

!classDefinition: #TonelImporter category: #Tonel!
Object subclass: #TonelImporter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tonel'!
!classDefinition: 'TonelImporter class' category: #Tonel!
TonelImporter class
	instanceVariableNames: ''!

!classDefinition: #TonelMethodDefinition category: #Tonel!
Object subclass: #TonelMethodDefinition
	instanceVariableNames: 'className selector protocol classSide source'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tonel'!
!classDefinition: 'TonelMethodDefinition class' category: #Tonel!
TonelMethodDefinition class
	instanceVariableNames: ''!

!classDefinition: #TonelReader category: #Tonel!
Object subclass: #TonelReader
	instanceVariableNames: 'stream currentLine'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tonel'!
!classDefinition: 'TonelReader class' category: #Tonel!
TonelReader class
	instanceVariableNames: ''!

!classDefinition: #TonelWriter category: #Tonel!
Object subclass: #TonelWriter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tonel'!
!classDefinition: 'TonelWriter class' category: #Tonel!
TonelWriter class
	instanceVariableNames: ''!


!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
category
	^ category! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
category: aSymbol
	category := aSymbol! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
classInstanceVariableNames
	^ classInstanceVariableNames ifNil: [ #() ]! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
classInstanceVariableNames: aCollection
	classInstanceVariableNames := aCollection asArray! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
className
	^ className! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
className: aSymbol
	className := aSymbol! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
classVariableNames
	^ classVariableNames ifNil: [ #() ]! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
classVariableNames: aCollection
	classVariableNames := aCollection asArray! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
comment
	^ comment ifNil: [ '' ]! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
comment: aString
	comment := aString! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
instanceVariableNames
	^ instanceVariableNames ifNil: [ #() ]! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
instanceVariableNames: aCollection
	instanceVariableNames := aCollection asArray! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
methods
	^ methods ifNil: [ methods := OrderedCollection new ]! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
package
	^ package! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
package: aSymbol
	package := aSymbol! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
poolDictionaries
	^ poolDictionaries ifNil: [ #() ]! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
poolDictionaries: aCollection
	poolDictionaries := aCollection asArray! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
superclassName
	^ superclassName! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
superclassName: aSymbol
	superclassName := aSymbol! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
tag
	^ tag! !

!TonelClassDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
tag: aSymbol
	tag := aSymbol! !

!TonelMethodDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
className
	^ className! !

!TonelMethodDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
className: aSymbol
	className := aSymbol! !

!TonelMethodDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
classSide
	^ classSide ifNil: [ false ]! !

!TonelMethodDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
classSide: aBoolean
	classSide := aBoolean! !

!TonelMethodDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
protocol
	^ protocol! !

!TonelMethodDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
protocol: aSymbol
	protocol := aSymbol! !

!TonelMethodDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
selector
	^ selector! !

!TonelMethodDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
selector: aSymbol
	selector := aSymbol! !

!TonelMethodDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
source
	^ source! !

!TonelMethodDefinition methodsFor: 'accessing' stamp: 'FJG 5/22/2026 00:00:00'!
source: aString
	source := aString! !

!TonelReader methodsFor: 'parsing' stamp: 'FJG 5/22/2026 00:00:00'!
metadataAt: key in: lines
	| prefix line raw |
	prefix := '#', key, ' : '.
	line := lines detect: [ :each | each withBlanksTrimmed beginsWith: prefix ] ifNone: [ ^ nil ].
	raw := (line copyFrom: ((line findString: ': ' startingAt: 1) + 2) to: line size) withBlanksTrimmed.
	(raw endsWith: ',') ifTrue: [ raw := raw allButLast ].
	^ self unquote: raw! !

!TonelReader methodsFor: 'parsing' stamp: 'FJG 5/22/2026 00:00:00'!
metadataListAt: key in: lines
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
	^ values asArray! !

!TonelReader methodsFor: 'parsing' stamp: 'FJG 5/22/2026 00:00:00'!
parse
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
	^ definition! !

!TonelReader methodsFor: 'parsing' stamp: 'FJG 5/22/2026 00:00:00'!
parseMethodHeader: headerLine metadata: metadataLine into: definition
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
			index := index + 1 ] ]! !

!TonelReader methodsFor: 'parsing' stamp: 'FJG 5/22/2026 00:00:00'!
parseMethodsInto: definition
	| line metadata |
	[ stream atEnd ] whileFalse: [
		line := stream nextLine.
		(line notNil and: [ line withBlanksTrimmed beginsWith: '{' ]) ifTrue: [
			metadata := line.
			self parseMethodHeader: stream nextLine metadata: metadata into: definition ] ].
	^ definition! !

!TonelReader methodsFor: 'parsing' stamp: 'FJG 5/22/2026 00:00:00'!
parseTypeDefinitionFrom: lines into: definition
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
	^ definition! !

!TonelReader methodsFor: 'parsing' stamp: 'FJG 5/22/2026 00:00:00'!
selectorFromMethodSource: sourceString
	| firstLine tokens selector |
	firstLine := sourceString readStream nextLine.
	tokens := firstLine substrings.
	(tokens anySatisfy: [ :each | each includes: $: ]) ifTrue: [
		selector := WriteStream on: ''.
		tokens do: [ :each | (each includes: $:) ifTrue: [ selector nextPutAll: each ] ].
		^ selector contents asSymbol ].
	^ tokens first asSymbol! !

!TonelReader methodsFor: 'parsing' stamp: 'FJG 5/22/2026 00:00:00'!
stream: aStream
	stream := aStream! !

!TonelReader methodsFor: 'parsing' stamp: 'FJG 5/22/2026 00:00:00'!
unquote: aString
	| trimmed |
	trimmed := aString withBlanksTrimmed.
	((trimmed size >= 2) and: [ trimmed first = $' and: [ trimmed last = $' ] ])
		ifFalse: [ ^ trimmed ].
	^ (trimmed copyFrom: 2 to: trimmed size - 1) copyReplaceAll: '''''' with: ''''! !

!TonelReader class methodsFor: 'instance creation' stamp: 'FJG 5/22/2026 00:00:00'!
readStream: aStream
	^ self new
		stream: aStream;
		parse! !

!TonelWriter class methodsFor: 'writing' stamp: 'FJG 5/22/2026 00:00:00'!
methodCategoryOf: selector in: aClassOrMetaclass
	^ (aClassOrMetaclass organization categoryOfElement: selector) ifNil: [ #'as yet unclassified' ]! !

!TonelWriter class methodsFor: 'writing' stamp: 'FJG 5/22/2026 00:00:00'!
packageNameForCategory: aCategoryName
	| category dashIndex |
	category := aCategoryName asString.
	dashIndex := category findString: '-' startingAt: 1.
	dashIndex = 0 ifTrue: [ ^ category ].
	^ category copyFrom: 1 to: dashIndex - 1! !

!TonelWriter class methodsFor: 'writing' stamp: 'FJG 5/22/2026 00:00:00'!
sourceForClass: aClass
	^ String streamContents: [ :stream | self writeClass: aClass on: stream ]! !

!TonelWriter class methodsFor: 'writing' stamp: 'FJG 5/22/2026 00:00:00'!
stonKey: key value: value on: stream last: isLast
	stream
		tab; nextPut: $#; nextPutAll: key; nextPutAll: ' : ';
		nextPut: $'; nextPutAll: (self stonString: value); nextPut: $'.
	isLast ifFalse: [ stream nextPut: $, ].
	stream newLine! !

!TonelWriter class methodsFor: 'writing' stamp: 'FJG 5/22/2026 00:00:00'!
stonListKey: key values: values on: stream
	values isEmpty ifTrue: [ ^ self ].
	stream tab; nextPut: $#; nextPutAll: key; nextPutAll: ' : ['; newLine.
	values do: [ :each |
		stream tab; tab; nextPut: $'; nextPutAll: (self stonString: each); nextPut: $'.
		each == values last ifFalse: [ stream nextPut: $, ].
		stream newLine ].
	stream tab; nextPutAll: '],'; newLine! !

!TonelWriter class methodsFor: 'writing' stamp: 'FJG 5/22/2026 00:00:00'!
stonString: aString
	^ aString asString copyReplaceAll: '''' with: ''''''! !

!TonelWriter class methodsFor: 'writing' stamp: 'FJG 5/22/2026 00:00:00'!
writeClass: aClass on: stream
	| comment category package tag classVars pools |
	comment := aClass organization classComment.
	comment isEmptyOrNil ifFalse: [
		stream nextPut: $"; newLine; nextPutAll: comment; newLine; nextPut: $"; newLine ].
	category := aClass category asString.
	package := self packageNameForCategory: category.
	tag := category = package ifTrue: [ nil ] ifFalse: [ category copyFrom: package size + 2 to: category size ].
	stream nextPutAll: 'Class {'; newLine.
	self stonKey: 'name' value: aClass name on: stream last: false.
	self stonKey: 'superclass' value: (aClass superclass ifNil: [ nil ] ifNotNil: [ :parentClass | parentClass name ]) on: stream last: false.
	self stonListKey: 'instVars' values: aClass instVarNames on: stream.
	classVars := aClass classPool keys asArray sort.
	self stonListKey: 'classVars' values: classVars on: stream.
	pools := (aClass sharedPools collect: [ :pool | Smalltalk keyAtIdentityValue: pool ]) asArray sort.
	self stonListKey: 'pools' values: pools on: stream.
	self stonListKey: 'classInstVars' values: aClass class instVarNames on: stream.
	self stonKey: 'category' value: category on: stream last: false.
	self stonKey: 'package' value: package on: stream last: tag isNil.
	tag ifNotNil: [ self stonKey: 'tag' value: tag on: stream last: true ].
	stream nextPutAll: '}'; newLine.
	self writeMethodsOf: aClass class classSide: true className: aClass name on: stream.
	self writeMethodsOf: aClass classSide: false className: aClass name on: stream! !

!TonelWriter class methodsFor: 'writing' stamp: 'FJG 5/22/2026 00:00:00'!
writeMethodsOf: aClassOrMetaclass classSide: classSide className: className on: stream
	| selectors |
	selectors := aClassOrMetaclass selectors asArray sort.
	selectors do: [ :selector |
		| source category sourceStream declarationLine body |
		source := aClassOrMetaclass sourceCodeAt: selector.
		sourceStream := source readStream.
		declarationLine := sourceStream nextLine.
		body := sourceStream upToEnd.
		category := self methodCategoryOf: selector in: aClassOrMetaclass.
		stream newLine.
		stream nextPutAll: '{ #category : '''; nextPutAll: (self stonString: category); nextPutAll: ''' }'; newLine.
		stream nextPutAll: className.
		classSide ifTrue: [ stream nextPutAll: ' class' ].
		stream nextPutAll: ' >> '; nextPutAll: declarationLine; nextPutAll: ' ['.
		stream newLine; nextPutAll: body; newLine; nextPutAll: ']'; newLine ]! !

!TonelImporter class methodsFor: 'importing' stamp: 'FJG 5/22/2026 00:00:00'!
importDefinition: definition
	| superClassToUse targetClass |
	targetClass := Smalltalk classNamed: definition className.
	targetClass ifNil: [
		superClassToUse := Smalltalk classNamed: definition superclassName.
		superClassToUse ifNil: [ self error: 'Missing superclass ', definition superclassName ].
		targetClass := superClassToUse
			subclass: definition className
			instanceVariableNames: (self namesStringFrom: definition instanceVariableNames)
			classVariableNames: (self namesStringFrom: definition classVariableNames)
			poolDictionaries: (self namesStringFrom: definition poolDictionaries)
			category: definition category.
		definition classInstanceVariableNames isEmpty ifFalse: [
			ClassBuilder new class: targetClass class instanceVariableNames: (self namesStringFrom: definition classInstanceVariableNames) ] ].
	definition comment isEmptyOrNil ifFalse: [
		targetClass classComment: definition comment stamp: '<tonel>' ].
	definition methods do: [ :method |
		(method classSide ifTrue: [ targetClass class ] ifFalse: [ targetClass ])
			compile: method source
			classified: method protocol ].
	^ targetClass! !

!TonelImporter class methodsFor: 'importing' stamp: 'FJG 5/22/2026 00:00:00'!
importDirectory: aDirectoryEntry
	aDirectoryEntry children do: [ :entry |
		entry isDirectoryEntry
			ifTrue: [ self importDirectory: entry ]
			ifFalse: [ (entry name endsWith: '.st') ifTrue: [ self importFile: entry ] ] ]! !

!TonelImporter class methodsFor: 'importing' stamp: 'FJG 5/22/2026 00:00:00'!
importFile: aFileEntry
	aFileEntry readStreamDo: [ :stream | ^ self importStream: stream ]! !

!TonelImporter class methodsFor: 'importing' stamp: 'FJG 5/22/2026 00:00:00'!
importStream: aStream
	^ self importDefinition: (TonelReader readStream: aStream)! !

!TonelImporter class methodsFor: 'importing' stamp: 'FJG 5/22/2026 00:00:00'!
namesStringFrom: aCollection
	^ String streamContents: [ :stream |
		aCollection do: [ :each |
			stream nextPutAll: each asString.
			each == aCollection last ifFalse: [ stream space ] ] ]! !

!TonelImageExporter class methodsFor: 'exporting' stamp: 'FJG 5/22/2026 00:00:00'!
exportAllTo: aDirectoryEntry
	^ self exportClasses: (Smalltalk allClasses asArray sort: [ :left :right | left name < right name ]) to: aDirectoryEntry! !

!TonelImageExporter class methodsFor: 'exporting' stamp: 'FJG 5/22/2026 00:00:00'!
exportClasses: classes to: aDirectoryEntry
	| outputRoot srcRoot classCount methodCount packages |
	outputRoot := aDirectoryEntry assureExistence.
	srcRoot := (outputRoot / 'src') assureExistence.
	(srcRoot // '.properties') forceWriteStreamDo: [ :stream |
		stream nextPutAll: '{'; newLine; tab; nextPutAll: '#format : #tonel'; newLine; nextPutAll: '}'; newLine ].
	classCount := 0.
	methodCount := 0.
	packages := Set new.
	classes do: [ :class |
		| package packageDir file |
		package := self packageNameForCategory: class category.
		packages add: package.
		packageDir := (srcRoot / package) assureExistence.
		file := packageDir // (self fileNameForClass: class).
		file forceWriteStreamDo: [ :stream | TonelWriter writeClass: class on: stream ].
		classCount := classCount + 1.
		methodCount := methodCount + class selectors size + class class selectors size ].
	self
		writeManifestTo: outputRoot
		classCount: classCount
		methodCount: methodCount
		packages: packages.
	^ outputRoot! !

!TonelImageExporter class methodsFor: 'exporting' stamp: 'FJG 5/22/2026 00:00:00'!
fileNameForClass: aClass
	^ aClass name asFileName, '.class.st'! !

!TonelImageExporter class methodsFor: 'exporting' stamp: 'FJG 5/22/2026 00:00:00'!
packageNameForCategory: aCategoryName
	^ TonelWriter packageNameForCategory: aCategoryName! !

!TonelImageExporter class methodsFor: 'exporting' stamp: 'FJG 5/22/2026 00:00:00'!
writeManifestTo: outputRoot classCount: classCount methodCount: methodCount packages: packages
	(outputRoot // 'manifest.txt') forceWriteStreamDo: [ :stream |
		stream nextPutAll: 'Tonel image export manifest'; newLine.
		stream nextPutAll: 'Generated: '; nextPutAll: DateAndTime now printString; newLine.
		stream nextPutAll: 'Image name: '; nextPutAll: Smalltalk imageName; newLine.
		stream nextPutAll: 'Image version: '; nextPutAll: SystemVersion current versionString; newLine.
		stream nextPutAll: 'Exported class count: '; nextPutAll: classCount printString; newLine.
		stream nextPutAll: 'Exported method count: '; nextPutAll: methodCount printString; newLine.
		stream nextPutAll: 'Package directory count: '; nextPutAll: packages size printString; newLine.
		stream nextPutAll: 'Format: Tonel v3-compatible source'; newLine.
		stream newLine.
		stream nextPutAll: 'Packages:'; newLine.
		(packages asArray sort) do: [ :package |
			stream nextPutAll: '- '; nextPutAll: package; newLine ] ]! !
