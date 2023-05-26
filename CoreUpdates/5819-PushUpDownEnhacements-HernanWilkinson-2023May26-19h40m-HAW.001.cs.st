'From Cuis 6.0 [latest update: #5818] on 26 May 2023 at 8:49:28 pm'!

!PushDownMethod class methodsFor: 'pre-conditions' stamp: 'HAW 5/26/2023 19:58:18'!
assertHasNoShadowedInstVars: aMethodToPushDown

	| subclassesWithShadowedInstVarNames |

	subclassesWithShadowedInstVarNames := Dictionary new.

	aMethodToPushDown methodClass subclassesDo: [:subclass |
		self addShadowedInstVarsOf: subclass in: aMethodToPushDown to: subclassesWithShadowedInstVarNames ].

	subclassesWithShadowedInstVarNames ifNotEmpty: [
		self signalMethodCannotShadowInstVarOfSubclasses: subclassesWithShadowedInstVarNames]! !

!PushDownMethod class methodsFor: 'pre-conditions' stamp: 'HAW 5/26/2023 19:59:34'!
assertHasSubclassesToPushDownMethodTo: aMethodToPushDown
		
	aMethodToPushDown methodClass hasSubclasses ifFalse: [ self signalNoSubclassToPushDownMethodTo ]! !

!PushDownMethod class methodsFor: 'exceptions' stamp: 'HAW 5/26/2023 20:00:38'!
errorMessageNoSubclassToPushDownMethodTo

	^'There are no subclasses to push down the method to'! !

!PushDownMethod class methodsFor: 'exceptions' stamp: 'HAW 5/26/2023 20:00:06'!
signalNoSubclassToPushDownMethodTo

	self refactoringError: self errorMessageNoSubclassToPushDownMethodTo! !


!PushDownMethod class methodsFor: 'pre-conditions' stamp: 'HAW 5/26/2023 19:58:39'!
assertIsValidToPushDownMethod: aMethodToPushDown

	self assertHasSubclassesToPushDownMethodTo: aMethodToPushDown.
	self assertHasNoShadowedInstVars: aMethodToPushDown
		! !


!PushUpMethod class methodsFor: 'warnings' stamp: 'HAW 5/26/2023 19:53:44'!
methodToPushUpExistOnSuperclassWarningMessage

	^'Method with different implementation already exist in superclass'.

	! !

!PushUpMethod class methodsFor: 'warnings' stamp: 'HAW 5/26/2023 19:52:06'!
warnIfExistMethodToPushUpOnSuperClass: aMethodToPushUp

	aMethodToPushUp methodClass superclass 
		compiledMethodAt: aMethodToPushUp selector
		ifPresent: [ :superclassMethod | (superclassMethod equivalentTo: aMethodToPushUp)
			ifFalse: [ self refactoringWarning: self methodToPushUpExistOnSuperclassWarningMessage ]]
		ifAbsent: []! !

