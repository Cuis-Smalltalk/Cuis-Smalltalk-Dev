Class {
	#name : 'PushDownMethodToOneSubclass',
	#superclass : 'PushDownMethod',
	#instVars : [
		'subclassToPushDownTo',
		'implementSubclassResponsibility'
	],
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
PushDownMethodToOneSubclass class >> assert: aSubclassToPushDownTo isSubclassOf: aClassToRefactor [

	(aClassToRefactor subclasses includes: aSubclassToPushDownTo) ifFalse: [
		self signal: aSubclassToPushDownTo isNotSubclassOf: aClassToRefactor ]
]

{ #category : 'exceptions' }
PushDownMethodToOneSubclass class >> errorMessage: aSubclassToPushDownTo isNotSubclassOf: aClassToRefactor  [
	
	^'Can not push down method because ', aSubclassToPushDownTo name, ' is not sublcass of ', aClassToRefactor name
]

{ #category : 'instance creation' }
PushDownMethodToOneSubclass class >> for: aMethodToPushDown to: aSubclassToPushDownTo  [

	| allSiblingsImplementMessage shouldImplementSubclassResponsibility |
	
	self assert: aSubclassToPushDownTo isSubclassOf: aMethodToPushDown methodClass.
	self assert: aMethodToPushDown hasNoShadowedInstVarsIn: { aSubclassToPushDownTo }.
	self warnIf: aMethodToPushDown existsOn: { aSubclassToPushDownTo }.
	allSiblingsImplementMessage := self warnIf: aMethodToPushDown isNotDefinedInOtherSubclassesBut: aSubclassToPushDownTo.
	shouldImplementSubclassResponsibility := allSiblingsImplementMessage and: [ aMethodToPushDown methodClass subclasses size > 1 ].

	^self new initializeFor: aMethodToPushDown to: aSubclassToPushDownTo implementingSubclassResponsibility: shouldImplementSubclassResponsibility
	
]

{ #category : 'exceptions' }
PushDownMethodToOneSubclass class >> signal: aSubclassToPushDownTo isNotSubclassOf: aClassToRefactor  [
	
	self refactoringError: (self errorMessage: aSubclassToPushDownTo isNotSubclassOf: aClassToRefactor)
]

{ #category : 'exceptions' }
PushDownMethodToOneSubclass class >> signalNotARefactoringBecauseNotImplementedIn: siblingsNotImplementing  [
	
	self refactoringWarning: (self warningMessageNotARefactoringBecauseNotImplementedIn: siblingsNotImplementing)
]

{ #category : 'pre-conditions' }
PushDownMethodToOneSubclass class >> warnIf: aMethodToPushDown isNotDefinedInOtherSubclassesBut: aSubclassToPushDownTo  [
	
	| siblings siblingsNotImplementing |
	
	siblings := aMethodToPushDown methodClass subclasses asOrderedCollection remove: aSubclassToPushDownTo; yourself.
	siblingsNotImplementing := siblings reject: [ :aSibling | aSibling includesSelector: aMethodToPushDown selector ].
	
	siblingsNotImplementing ifNotEmpty: [ self signalNotARefactoringBecauseNotImplementedIn: siblingsNotImplementing ].
	^siblingsNotImplementing isEmpty
]

{ #category : 'warnings' }
PushDownMethodToOneSubclass class >> warningMessageNotARefactoringBecauseNotImplementedIn: siblingsNotImplementing  [
	
	^String streamContents: [ :stream |
		stream
			nextPutAll: 'This is not a refactoring because the message is not implemented in:';
			newLine.

		siblingsNotImplementing asCommaSeparated: [ :aClass | stream print: aClass ] on: stream.

		stream
			newLine;
			nextPutAll: 'Existing code may not work if applied' ].
]

{ #category : 'initialization' }
PushDownMethodToOneSubclass >> initializeFor: aMethodToPushDown to: aSubclassToPushDown implementingSubclassResponsibility: shouldImplementSubclassResponsibility  [

	self initializeFor: aMethodToPushDown.
	subclassToPushDownTo := aSubclassToPushDown.
	implementSubclassResponsibility := shouldImplementSubclassResponsibility 
]

{ #category : 'applying - private' }
PushDownMethodToOneSubclass >> moveMethod [

	subclassToPushDownTo 
		compile: method sourceCode 
		classified: self methodCategory.
	

]

{ #category : 'applying - private' }
PushDownMethodToOneSubclass >> removeMethod [

	implementSubclassResponsibility
		ifTrue: [ self implementSubclassResponsibility ]
		ifFalse: [ super removeMethod ]
]
