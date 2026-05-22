"
A ChangeRecord represents a change recorded on a file in fileOut format.
It includes a type (more needs to be done here), and additional information
for certain types such as method defs which need class and category.
"
Class {
	#name : 'ChangeRecord',
	#superclass : 'ChangeListElement',
	#instVars : [
		'file',
		'position',
		'type',
		'class',
		'category',
		'meta',
		'stamp',
		'prior',
		'libraryName'
	],
	#category : 'Tools-Changes',
	#package : 'Tools',
	#tag : 'Changes'
}

{ #category : 'accessing' }
ChangeRecord >> category [
	^category
]

{ #category : 'accessing' }
ChangeRecord >> changeClass [
	| methodClassName methodClass |
	(#(method classComment classDefinition) includes: type) ifFalse: [ ^ nil ].
	methodClassName := class substrings
		ifEmpty: [ ^ nil ]
		ifNotEmpty: [ :parts |
			parts first asSymbol ].
	(Smalltalk includesKey: methodClassName) ifFalse: [ ^ nil ].
	methodClass := Smalltalk at: methodClassName.
	^ meta
		ifTrue: [ methodClass class ]
		ifFalse: [ methodClass ]
]

{ #category : 'accessing' }
ChangeRecord >> changeClassName [
	^class
]

{ #category : 'accessing' }
ChangeRecord >> changeType [
	^ type
]

{ #category : 'accessing' }
ChangeRecord >> compilerClass [
	^ (Smalltalk at: class ifAbsent: [^ Compiler]) compilerClass
]

{ #category : 'accessing' }
ChangeRecord >> dateAndTime [

	^CompiledMethod authorAndStamp: self stamp partsDo: [ :authorInitials :dateAndTime | dateAndTime ]
]

{ #category : 'initialization' }
ChangeRecord >> file: aFile position: aPosition type: aType [

	file := aFile.
	position := aPosition.
	type := aType.
"
file closed ifFalse: [
	'' print.
	file print.
	self print.
	thisContext printStack: 10 ]
"
]

{ #category : 'initialization' }
ChangeRecord >> file: aFile position: aPosition type: aType class: aClassName category: aClassCategory meta: isMeta stamp: aStamp [

	self 
		file: aFile 
		position: aPosition 
		type: aType 
		class: aClassName 
		category: aClassCategory 
		meta: isMeta 
		stamp: aStamp 
		prior: nil
		libraryName: nil.

]

{ #category : 'initialization' }
ChangeRecord >> file: aFile position: aPosition type: aType class: aClassName category: aClassCategory meta: isMeta stamp: aStamp prior: aPrior libraryName: aString [

	self file: aFile position: aPosition type: aType.
	class := aClassName.
	category := aClassCategory.
	meta := isMeta.
	stamp := aStamp.
	prior := aPrior.
	libraryName := aString.
]

{ #category : 'initialization' }
ChangeRecord >> fileIn [
	"File the receiver in.  If I represent a method or a class-comment, file the method in and make a note of it in the recent-submissions list; if I represent a do-it, then, well, do it."
	| s |
	type == #method ifTrue: [
		self changeClass ifNotNil: [ :methodClass |
			methodClass
				compile: self text
				classified: category
				withStamp: stamp
				libraryName: libraryName
				notifying: nil ]].
	self isDoIt ifTrue: [
		((s := self string) beginsWith: '----') ifFalse: [ Compiler evaluate: s ]].
	type == #classDefinition ifTrue: [ Compiler evaluate: self string ].
	type == #classComment ifTrue: [
		(Smalltalk at: class)
			comment: self text
			stamp: stamp ]
]

{ #category : 'accessing' }
ChangeRecord >> fileName [
	^(file ifNotNil: [ file name ]) 
			ifNil: [ '<no file>' ]
]

{ #category : 'accessing' }
ChangeRecord >> fileOutOn: aFileStream [
	"File the receiver out on the given file stream"

	type == #method
		ifTrue: [
			self changeClass printCategoryChunk: category on: aFileStream
				withStamp: stamp libraryName: libraryName priorMethod: nil.
			aFileStream newLine ].

	type == #preamble ifTrue: [ aFileStream nextPut: $! ].

	type == #classComment
		ifTrue: [
			aFileStream nextPut: $!.
			aFileStream nextChunkPut: class asString, ' commentStamp: ', stamp storeString.
			aFileStream newLine ].
		
	type == #classDefinition ifTrue: [
		aFileStream nextPut: $!.
		aFileStream nextChunkPut: 
			'classDefinition: ', 
			(self isMetaClassChange ifTrue: [self changeClassName, ' class'] ifFalse: [self changeClassName]) printString,
			' category: ', self category printString.
		aFileStream newLine ].

	aFileStream nextChunkPut: self string.
	
	type == #method ifTrue: [ aFileStream nextChunkPut: ' '; newLine ].
	type == #classComment ifTrue: [ aFileStream newLine ].
	aFileStream newLine.
]

{ #category : 'testing' }
ChangeRecord >> isDoIt [

	^type = #doIt
]

{ #category : 'accessing' }
ChangeRecord >> isMetaClassChange [
	^meta
]

{ #category : 'accessing' }
ChangeRecord >> methodSelector [
	^type == #method ifTrue:
		[(Smalltalk at: class ifAbsent: [Object]) parserClass new parseSelector: self string]
]

{ #category : 'printing' }
ChangeRecord >> printOn: aStream [

	super printOn: aStream.
	aStream 
		nextPutAll: ' - type: ';
		nextPutAll: type 
]

{ #category : 'accessing' }
ChangeRecord >> prior [

	^prior
]

{ #category : 'accessing' }
ChangeRecord >> stamp [
	^ stamp
]

{ #category : 'accessing' }
ChangeRecord >> stamp: threePartString [

	stamp := threePartString
]

{ #category : 'accessing' }
ChangeRecord >> string [
	"The file is usually closed. But if it happens to be open, leave it like that."
	| string mustOpenAndClose |
	mustOpenAndClose := file closed.
	mustOpenAndClose ifTrue: [
		file openReadOnly ].
	file position: position.
	string := file nextChunk withCuisLineEndings.
	mustOpenAndClose ifTrue: [
		file close].
	^ string
]
