"
Polymorphic with ChangeRecord. Used to represent methods that are not part of a CodePackageFile, but are part of the corresponding CodePackage: Methods that will be deleted on install.
"
Class {
	#name : 'MethodDeletionChangeRecord',
	#superclass : 'ChangeListElement',
	#instVars : [
		'methodReference',
		'doItOnlyIfInBaseSystem',
		'sourceCode',
		'stamp'
	],
	#category : 'Tools-Changes',
	#package : 'Tools',
	#tag : 'Changes'
}

{ #category : 'instance creation' }
MethodDeletionChangeRecord class >> methodReference: aMethodReference doItOnlyIfInBaseSystem: aDoit source: aSourceCode stamp: aStamp  [

	^self new 
		initializeMethodReference: aMethodReference 
		doItOnlyIfInBaseSystem: aDoit 
		source: aSourceCode 
		stamp: aStamp 
]

{ #category : 'accessing' }
MethodDeletionChangeRecord >> changeClass [
	^methodReference actualClass
]

{ #category : 'accessing' }
MethodDeletionChangeRecord >> changeClassName [
	^methodReference classIsMeta
		ifFalse: [ methodReference classSymbol ]
		ifTrue: [ methodReference classSymbol, ' class' ]
]

{ #category : 'accessing' }
MethodDeletionChangeRecord >> changeType [
	^ #method
]

{ #category : 'services' }
MethodDeletionChangeRecord >> command [

	^String streamContents: [ :stream |
		stream
			nextPutAll: self changeClassName;
			space;
			nextPutAll: (doItOnlyIfInBaseSystem ifTrue: ['removeSelectorIfInBaseSystem:'] ifFalse: ['removeSelector:']);
			space;
			nextPutAll: self methodSelector storeString ]
]

{ #category : 'accessing' }
MethodDeletionChangeRecord >> doItOnlyIfInBaseSystem: aBoolean [
	doItOnlyIfInBaseSystem := aBoolean
]

{ #category : 'fileIn/fileOut' }
MethodDeletionChangeRecord >> fileIn [

	doItOnlyIfInBaseSystem
		ifTrue: [ methodReference actualClass removeSelectorIfInBaseSystem: self methodSelector ]
		ifFalse: [ methodReference actualClass removeSelector: self methodSelector ]
]

{ #category : 'fileIn/fileOut' }
MethodDeletionChangeRecord >> fileOutOn: stream [
	"File the receiver out on the given file stream"

	| record |

	record := String streamContents: [ :recordStream |
		recordStream
		 	nextPutAll: (doItOnlyIfInBaseSystem ifTrue: [ 'methodMoveToSomePackage: ' ] ifFalse: ['methodRemoval: ']);
			nextPutAll: self changeClassName;
			space;
			nextPutAll: self methodSelector storeString;
			nextPutAll: ' stamp: ';
			print: stamp ].
		
	stream 
		nextPut: $!;
		nextChunkPut: record;
		newLine;
		nextChunkPut: self command;
		newLine
	

]

{ #category : 'initialization' }
MethodDeletionChangeRecord >> initializeMethodReference: aMethodReference doItOnlyIfInBaseSystem: aDoit source: aSourceCode stamp: aStamp   [
	
	methodReference := aMethodReference.
	doItOnlyIfInBaseSystem := aDoit.
	sourceCode := aSourceCode.
	stamp := aStamp 
]

{ #category : 'accessing' }
MethodDeletionChangeRecord >> isMetaClassChange [
	^methodReference classIsMeta
]

{ #category : 'testing' }
MethodDeletionChangeRecord >> isMethodDeletion [
	^true
]

{ #category : 'accessing' }
MethodDeletionChangeRecord >> methodReference: aMethodReference [
	methodReference := aMethodReference
]

{ #category : 'accessing' }
MethodDeletionChangeRecord >> methodSelector [
	^methodReference selector
]

{ #category : 'perform' }
MethodDeletionChangeRecord >> performOn: aCodeFile [

	^aCodeFile removedMethod: self command with: self 
]

{ #category : 'accessing' }
MethodDeletionChangeRecord >> stamp [

	^stamp
]

{ #category : 'accessing' }
MethodDeletionChangeRecord >> string [

	^sourceCode ifNil: [ '' ]
]
