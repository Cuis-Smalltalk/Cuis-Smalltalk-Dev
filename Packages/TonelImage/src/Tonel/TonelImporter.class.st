Class {
	#name : 'TonelImporter',
	#superclass : 'Object',
	#category : 'Tonel',
	#package : 'Tonel'
}

{ #category : 'importing' }
TonelImporter class >> importDefinition: definition [
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
	^ targetClass
]

{ #category : 'importing' }
TonelImporter class >> importDirectory: aDirectoryEntry [
	aDirectoryEntry children do: [ :entry |
		entry isDirectoryEntry
			ifTrue: [ self importDirectory: entry ]
			ifFalse: [ (entry name endsWith: '.st') ifTrue: [ self importFile: entry ] ] ]
]

{ #category : 'importing' }
TonelImporter class >> importFile: aFileEntry [
	aFileEntry readStreamDo: [ :stream | ^ self importStream: stream ]
]

{ #category : 'importing' }
TonelImporter class >> importStream: aStream [
	^ self importDefinition: (TonelReader readStream: aStream)
]

{ #category : 'importing' }
TonelImporter class >> namesStringFrom: aCollection [
	^ String streamContents: [ :stream |
		aCollection do: [ :each |
			stream nextPutAll: each asString.
			each == aCollection last ifFalse: [ stream space ] ] ]
]
