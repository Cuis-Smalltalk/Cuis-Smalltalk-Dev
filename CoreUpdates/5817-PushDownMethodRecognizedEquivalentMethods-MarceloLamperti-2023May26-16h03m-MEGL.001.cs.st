'From Cuis 6.0 [latest update: #5813] on 26 May 2023 at 4:31:28 pm'!

!PushDownMethod class methodsFor: 'pre-conditions' stamp: 'MEGL 5/26/2023 16:29:59'!
is: subclass equivalentlyImplementedIn: aMethodToPushDown

	^ subclass 
		compiledMethodAt: aMethodToPushDown selector
		ifPresent: [ :methodInSubclass | (methodInSubclass equivalentTo: aMethodToPushDown) not ]
		ifAbsent: [ false ]! !


!PushDownMethod class methodsFor: 'pre-conditions' stamp: 'MEGL 5/26/2023 16:29:47'!
warnIfExistMethodToPushDownOnSubClass: aMethodToPushDown

	| subclassesImplementingMessage |

	subclassesImplementingMessage := aMethodToPushDown methodClass subclasses select: [:subclass |
		self is: subclass equivalentlyImplementedIn: aMethodToPushDown].
		
	subclassesImplementingMessage ifNotEmpty: [
		self warnMessageAlreadyImplementedIn: subclassesImplementingMessage ]! !


!PushDownMethod class reorganize!
('instance creation' for:)
('pre-conditions' addShadowedInstVarsOf:in:to: assertIsValidToPushDownMethod: is:equivalentlyImplementedIn: warnIfExistMethodToPushDownOnSubClass:)
('exceptions' errorMessageCanNotPushDownWithShadowedInstVarsOf: signalMethodCannotShadowInstVarOfSubclasses:)
('warnings' warnMessageAlreadyImplementedIn: warningMesssageForMessageAlreadyImplementedIn:)
!

