Class {
	#name : 'SystemChangeNotifier',
	#superclass : 'ActiveModel',
	#instVars : [
		'silenceLevel'
	],
	#classVars : [
		'UniqueInstance'
	],
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'class initialization' }
SystemChangeNotifier class >> initialize [
	"
	SystemChangeNotifier initialize
	"
	SystemChangeNotifier uniqueInstance removeActionsWithReceiver: Smalltalk.
	SystemChangeNotifier uniqueInstance
		when: #classRemoved send: #classRemoved:fromCategory: to: Smalltalk;
		when: #evaluated send: #evaluated:context: to: Smalltalk;
		when: #methodRemoved send: #methodRemoved:selector:inProtocol:class: to: Smalltalk
]

{ #category : 'instance creation' }
SystemChangeNotifier class >> new [

	^self error: 'This is a singleton implementation, so you are not allowed to create instances yourself. Use #uniqueInstance to access the instance.'
]

{ #category : 'public' }
SystemChangeNotifier class >> uniqueInstance [

	UniqueInstance ifNil: [ UniqueInstance := self  basicNew initialize ].
	^UniqueInstance
]

{ #category : 'system triggers' }
SystemChangeNotifier >> aboutToRenameClass: aClass from: oldClassName to: newClassName inCategory: aCategoryName [

	self
		triggerEvent: #aboutToRenameClass
		withArguments: { aClass . oldClassName . newClassName . aCategoryName }
]

{ #category : 'system triggers' }
SystemChangeNotifier >> classAdded: aClass inCategory: aCategoryName [

	self
		triggerEvent: #classAdded
		withArguments: { aClass . aCategoryName }
]

{ #category : 'system triggers' }
SystemChangeNotifier >> classCommented: aClass [
	"A class with the given name was commented in the system."

	self
		triggerEvent: #classCommented
		with: aClass
]

{ #category : 'system triggers' }
SystemChangeNotifier >> classDefinitionChangedFrom: oldClass to: newClass oldClassDefinition: oldClassDefinition [

	self
		triggerEvent: #classDefinitionChanged
		withArguments: { oldClass . newClass . oldClassDefinition }
]

{ #category : 'system triggers' }
SystemChangeNotifier >> classRecategorized: aClass from: oldCategory to: newCategory [

	self
		triggerEvent: #classRecategorized
		withArguments: { aClass . oldCategory . newCategory }
]

{ #category : 'system triggers' }
SystemChangeNotifier >> classRemoved: aClass fromCategory: aCategoryName [

	self
		triggerEvent: #classRemoved
		withArguments: { aClass . aCategoryName }
]

{ #category : 'system triggers' }
SystemChangeNotifier >> classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName [

	self
		triggerEvent: #classRenamed
		withArguments: { aClass . oldClassName . newClassName . aCategoryName }
]

{ #category : 'system triggers' }
SystemChangeNotifier >> classReorganized: aClass [

	self triggerEvent: #classReorganized with: aClass
]

{ #category : 'public' }
SystemChangeNotifier >> doSilently: aBlock [
	"Perform the block, and ensure that no system notification are broadcasted while doing so."

	silenceLevel := silenceLevel + 1.
	^aBlock ensure: [silenceLevel > 0 ifTrue: [silenceLevel := silenceLevel - 1]]
]

{ #category : 'system triggers' }
SystemChangeNotifier >> evaluated: expression context: aContext [

	self
		triggerEvent: #evaluated
		withArguments: { expression . aContext }
]

{ #category : 'initialization' }
SystemChangeNotifier >> initialize [

	silenceLevel := 0
]

{ #category : 'public' }
SystemChangeNotifier >> isBroadcasting [

	^ silenceLevel = 0
]

{ #category : 'system triggers' }
SystemChangeNotifier >> methodAdded: aMethod selector: aSymbol inProtocol: aCategoryName class: aClass methodTimeStamp: aTimeStampString requestor: requestor [
	"A method with the given selector was added to aClass in protocol aCategoryName."

	self
		triggerEvent: #methodAddedInProtocolTimeStamp
		withArguments: { aMethod . aSymbol . aCategoryName . aClass . aTimeStampString. requestor }
]

{ #category : 'system triggers' }
SystemChangeNotifier >> methodChangedFrom: oldMethod to: newMethod selector: aSymbol inClass: aClass requestor: requestor [

	self
		triggerEvent: #methodChanged
		withArguments: { oldMethod . newMethod . aSymbol . aClass . requestor }
]

{ #category : 'system triggers' }
SystemChangeNotifier >> methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass  [
	"A method with the given selector was removed from the class."

	self
		triggerEvent: #methodRemoved
		withArguments: { aMethod . aSymbol . protocol . aClass }
]

{ #category : 'system triggers' }
SystemChangeNotifier >> selectorRecategorized: selector from: oldCategory to: newCategory inClass: aClass [

	self
		triggerEvent: #selectorRecategorized
		withArguments: { selector . oldCategory . newCategory . aClass }
]

{ #category : 'private' }
SystemChangeNotifier >> triggerEvent: anEventSelector [

	self isBroadcasting ifTrue: [
		^super triggerEvent: anEventSelector ]
]

{ #category : 'private' }
SystemChangeNotifier >> triggerEvent: anEventSelector with: anObject [

	self isBroadcasting ifTrue: [
		^super triggerEvent: anEventSelector with: anObject ]
]

{ #category : 'private' }
SystemChangeNotifier >> triggerEvent: anEventSelector withArguments: anArgumentList [

	self isBroadcasting ifTrue: [
		^super triggerEvent: anEventSelector withArguments: anArgumentList ]
]
