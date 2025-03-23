'From Cuis7.3 [latest update: #7084] on 23 March 2025 at 10:41:50 am'!

!PushUpMethod methodsFor: 'initialization' stamp: 'HAW 3/23/2025 10:36:01'!
initializeForAll: aMethods of: aMethodToPushUp additionallyRemovingFrom: aSubclassesToRemoveMethodFrom

	self initializeFor: aMethodToPushUp.
	methods := aMethods.
	subclassesToRemoveMethodFrom := aSubclassesToRemoveMethodFrom.! !


!PushDownMethodToOneSubclass methodsFor: 'initialization' stamp: 'HAW 3/23/2025 10:38:12'!
initializeFor: aMethodToPushDown to: aSubclassToPushDown implementingSubclassResponsibility: shouldImplementSubclassResponsibility 

	self initializeFor: aMethodToPushDown.
	subclassToPushDownTo := aSubclassToPushDown.
	implementSubclassResponsibility := shouldImplementSubclassResponsibility ! !


!PushUpMethod methodsFor: 'applying - private' stamp: 'HAW 3/23/2025 10:36:30'!
pushUpAccessedVariables

	|instanceVariablesToPushUp methodClass|
	
	instanceVariablesToPushUp := self class selectInstanceVariablesToPushUpFrom: methods.
	methodClass := method methodClass.

	instanceVariablesToPushUp do: [:var | (PushUpInstanceVariable named: var from: methodClass) apply ]! !


!PushUpMethod class methodsFor: 'instance creation' stamp: 'HAW 3/23/2025 10:35:44'!
for: aMethodToPushUp additionallyRemovingFrom: subclassesToRemoveMethodFrom pushingUp: dependantMethods
	
	dependantMethods add: aMethodToPushUp.

	self assertIsValidToPushUp: dependantMethods.
	self assertIsValidToRemove: subclassesToRemoveMethodFrom.
	
	^self new 
		initializeForAll: dependantMethods 
		of: aMethodToPushUp 
		additionallyRemovingFrom: subclassesToRemoveMethodFrom.! !

!PushUpMethod class methodsFor: 'warnings' stamp: 'HAW 3/23/2025 10:26:28'!
warnIfDifferentImplementationInSuperclass: methods

	| methodsImplementedInSuperclass |
	
	methodsImplementedInSuperclass := methods 	
		select: [ :method | self checkIfMethodIsImplementedInSuperclass: method ]
		thenCollect: [ :method | method selector].
		
	methodsImplementedInSuperclass ifNotEmpty: [ 
		self refactoringWarning: (self methodWithDifferentImplementationInSuperclass: methodsImplementedInSuperclass)].
	
	
! !

!methodRemoval: PushUpMethod #initializeForAll:additionallyRemovingFrom: stamp: 'HAW 3/23/2025 10:35:33'!
PushUpMethod removeSelector: #initializeForAll:additionallyRemovingFrom:!
!methodRemoval: PushUpMethod #initializeFor:additionallyRemovingFrom: stamp: 'HAW 3/23/2025 10:34:16'!
PushUpMethod removeSelector: #initializeFor:additionallyRemovingFrom:!
!methodRemoval: PushUpMethod #selector stamp: 'HAW 3/23/2025 10:36:10'!
PushUpMethod removeSelector: #selector!

!PushUpMethod reorganize!
('applying - private' compileMethodInSuperclass: moveMethod pushUpAccessedVariables pushUpMethods removeMethod)
('initialization' initializeForAll:of:additionallyRemovingFrom:)
!


!PushUpMethod class reorganize!
('instance creation' for: for:additionallyRemovingFrom:pushingUp:)
('pre-conditions' assert:areSiblingsOf: assert:isEquivalentlyImplementedIn: assert:isImplementedIn: assert:isValidToRemoveFrom: assertIsValidToPushUp: assertIsValidToRemove:)
('warnings' accessingInstanceVariableWarningMessage: methodWithDifferentImplementationInSuperclass: warnIfDifferentImplementationInSuperclass: warnIfReferencesInstanceVariables:)
('exceptions' errorMessageForMethodCannotBeRemovedMethodFrom: errorMessageForNoSiblings: errorMessageForNotEquivalentMethods: signalMethodCannotBeRemovedMethodFrom: signalNoSiblings: signalNotEquivalentMethodsFor:)
('instance variables' selectAccessedInstanceVariables: selectAccessedInstanceVariablesThatCanBePushedUpFrom: selectInstanceVariablesToPushUpFrom:)
('implemented superclass' checkIfMethodIsImplementedInSuperclass:)
('equivalent methods' equivalentMethodsOfSiblingsOfAll: is:equivalentIn:)
('dependant methods' selectAllMessageSendsToSelfNotImplementedInSuperClassFrom: selectDependantSelectorsFrom:using: selectorsSentToSelf:)
!

