Class {
	#name : 'PushDownMethodToSubclasses',
	#superclass : 'PushDownMethod',
	#category : 'Tools-Refactoring',
	#package : 'Tools',
	#tag : 'Refactoring'
}

{ #category : 'pre-conditions' }
PushDownMethodToSubclasses class >> assertHasSubclassesToPushDownMethodTo: aMethodToPushDown [
		
	aMethodToPushDown methodClass hasSubclasses ifFalse: [ self signalNoSubclassToPushDownMethodTo ]
]

{ #category : 'pre-conditions' }
PushDownMethodToSubclasses class >> assertIsValidToPushDownMethod: aMethodToPushDown [

	self assertHasSubclassesToPushDownMethodTo: aMethodToPushDown.
	self assert: aMethodToPushDown hasNoShadowedInstVarsIn: aMethodToPushDown methodClass subclasses
		
]

{ #category : 'exceptions' }
PushDownMethodToSubclasses class >> errorMessageNoSubclassToPushDownMethodTo [

	^'There are no subclasses to push down the method to'
]

{ #category : 'instance creation' }
PushDownMethodToSubclasses class >> for: aMethodToPushDown [

	self assertIsValidToPushDownMethod: aMethodToPushDown.
	self warnIf: aMethodToPushDown existsOn: aMethodToPushDown methodClass subclasses.

	^self new initializeFor: aMethodToPushDown 
]

{ #category : 'exceptions' }
PushDownMethodToSubclasses class >> signalNoSubclassToPushDownMethodTo [

	self refactoringError: self errorMessageNoSubclassToPushDownMethodTo
]

{ #category : 'applying - private' }
PushDownMethodToSubclasses >> moveMethod [

	| methodCategory sourceCode |

	methodCategory := self methodCategory.
	sourceCode := method sourceCode.

	method methodClass subclassesDo: [:subclass |
		subclass
			compile: sourceCode
			classified: methodCategory.
	].

]

{ #category : 'applying - private' }
PushDownMethodToSubclasses >> removeMethod [

	self implementSubclassResponsibility
							
]
