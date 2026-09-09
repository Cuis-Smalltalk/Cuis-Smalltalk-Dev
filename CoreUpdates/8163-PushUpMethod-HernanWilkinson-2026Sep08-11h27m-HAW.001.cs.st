'From Cuis7.9 [latest update: #8160] on 9 September 2026 at 8:52:27 am'!

!PushUpMethod class methodsFor: 'dependant methods' stamp: 'HAW 9/Sep/2026, 11:49:42 am (UTC)'!
dependantMethodsOf: aMethod

	^(PushUpMethod selectAllMessageSendsToSelfNotImplementedInSuperClassFrom: aMethod)
		collect: [ :eachSelector | aMethod methodClass >> eachSelector ]! !


!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 9/Sep/2026, 11:50:45 am (UTC)'!
selectDependantMethods

	| methods |

	methods := self refactoringClass dependantMethodsOf: methodToTransfer.

	methods ifNotEmpty: [ self askMethodsToPushUpFrom: methods ].! !


!PushUpMethod class reorganize!
('instance creation' for: for:additionallyRemovingFrom:pushingUp:)
('pre-conditions' assert:areSiblingsOf: assert:isEquivalentlyImplementedIn: assert:isImplementedIn: assert:isValidToRemoveFrom: assertIsValidToPushUp: assertIsValidToRemove:)
('warnings' accessingInstanceVariableWarningMessage: methodWithDifferentImplementationInSuperclass: warnIfDifferentImplementationInSuperclass: warnIfReferencesInstanceVariables:)
('exceptions' errorMessageForMethodCannotBeRemovedMethodFrom: errorMessageForNoSiblings: errorMessageForNotEquivalentMethods: signalMethodCannotBeRemovedMethodFrom: signalNoSiblings: signalNotEquivalentMethodsFor:)
('instance variables' selectAccessedInstanceVariables: selectAccessedInstanceVariablesThatCanBePushedUpFrom: selectInstanceVariablesToPushUpFrom:)
('implemented superclass' checkIfMethodIsImplementedInSuperclass:)
('equivalent methods' equivalentMethodsOfSiblingsOfAll: is:equivalentIn:)
('dependant methods' dependantMethodsOf: selectAllMessageSendsToSelfNotImplementedInSuperClassFrom: selectDependantSelectorsFrom:using: selectorsSentToSelf:)
!

