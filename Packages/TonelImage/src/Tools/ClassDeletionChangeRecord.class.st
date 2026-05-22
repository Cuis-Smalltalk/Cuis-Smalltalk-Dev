"
Polymorphic with ChangeRecord. Used to represent classes that are not part of a CodePackageFile, but are part of the corresponding CodePackage: Classes that will be deleted on install.
"
Class {
	#name : 'ClassDeletionChangeRecord',
	#superclass : 'ChangeListElement',
	#instVars : [
		'className',
		'doItOnlyIfInBaseSystem',
		'classDefinition',
		'stamp'
	],
	#category : 'Tools-Changes',
	#package : 'Tools',
	#tag : 'Changes'
}

{ #category : 'instance creation' }
ClassDeletionChangeRecord class >> className: aClassName definition: aClassDefinition doItOnlyIfInBaseSystem: aDoItOnlyIfInBaseSystem stamp: aStamp  [

	^self new 
		initializeClassName: aClassName 
		definition: aClassDefinition 
		doItOnlyIfInBaseSystem: aDoItOnlyIfInBaseSystem 
		stamp: aStamp 
]

{ #category : 'accessing' }
ClassDeletionChangeRecord >> changeClass [
	^Smalltalk at: className ifAbsent: nil
]

{ #category : 'accessing' }
ClassDeletionChangeRecord >> changeClassName [
	^className
]

{ #category : 'accessing' }
ClassDeletionChangeRecord >> changeType [

	^ #classRemoval
]

{ #category : 'accessing' }
ClassDeletionChangeRecord >> className: aSymbol [

	className := aSymbol
]

{ #category : 'services' }
ClassDeletionChangeRecord >> command [

	^doItOnlyIfInBaseSystem
		ifTrue: [ 'Smalltalk removeClassNamedIfInBaseSystem: #', className ]
		ifFalse: [ 'Smalltalk removeClassNamed: #', className ].

]

{ #category : 'accessing' }
ClassDeletionChangeRecord >> doItOnlyIfInBaseSystem: aBoolean [
	doItOnlyIfInBaseSystem := aBoolean
]

{ #category : 'fileIn/fileOut' }
ClassDeletionChangeRecord >> fileIn [

	doItOnlyIfInBaseSystem
		ifTrue: [ Smalltalk removeClassNamedIfInBaseSystem: className]
		ifFalse: [ self changeClass ifNotNil: [ :aClass | aClass removeFromSystem ] ]
]

{ #category : 'fileIn/fileOut' }
ClassDeletionChangeRecord >> fileOutOn: stream [
	"File the receiver out on the given file stream"

	| record |
		
	record := String streamContents: [ :recordStream |
		recordStream
		 	nextPutAll: (doItOnlyIfInBaseSystem ifTrue: [ 'classMoveToSomePackage: #' ] ifFalse: ['classRemoval: #']);
			nextPutAll: className;
			nextPutAll: ' stamp: ';
			print: stamp ].
		
	stream 
		nextPut: $!;
		nextChunkPut: record;
		newLine;
		nextChunkPut: self command;
		newLine; newLine.
	
]

{ #category : 'initialization' }
ClassDeletionChangeRecord >> initializeClassName: aClassName definition: aClassDefinition doItOnlyIfInBaseSystem: aDoItOnlyIfInBaseSystem stamp: aStamp  [
	
	className := aClassName.
	classDefinition := aClassDefinition.
	doItOnlyIfInBaseSystem := aDoItOnlyIfInBaseSystem.
	stamp := aStamp.
]

{ #category : 'testing' }
ClassDeletionChangeRecord >> isClassDeletion [
	^true
]

{ #category : 'accessing' }
ClassDeletionChangeRecord >> isMetaClassChange [
	^false
]

{ #category : 'accessing' }
ClassDeletionChangeRecord >> stamp [

	^stamp
]

{ #category : 'accessing' }
ClassDeletionChangeRecord >> string [
	
	^classDefinition ifNil: [ '' ]
]
