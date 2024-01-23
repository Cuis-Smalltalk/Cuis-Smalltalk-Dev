'From Cuis6.3 [latest update: #6167] on 23 January 2024 at 3:42:59 pm'!

!PushUpMethod class methodsFor: 'warnings' stamp: 'HAW 1/23/2024 15:00:48'!
methodWithDifferentImplementationInSuperclass: methods

	^String streamContents: [ :stream |
		stream 
			nextPutAll: 'The method(s): ';
			nextPutAll: methods asCommaStringAnd;
			nextPutAll: ' have an implementation not equivalent in the superclass and will be pushed up' ]! !

!PushUpMethod class methodsFor: 'warnings' stamp: 'HAW 1/23/2024 15:00:54'!
warnIfDifferentImplementationInSuperclass: methods

	| methodsImplementedInSuperclass |
	
	methodsImplementedInSuperclass := methods 	
		select: [ :method |  		self checkIfMethodIsImplementedInSuperclass: method ]
		thenCollect: [ :method | method selector].
		
	methodsImplementedInSuperclass ifNotEmpty: [ 
		self refactoringWarning: (self methodWithDifferentImplementationInSuperclass: methodsImplementedInSuperclass)].
	
	
! !

!PushUpMethod class methodsFor: 'warnings' stamp: 'HAW 1/23/2024 14:58:36'!
warnIfReferencesInstanceVariables: methods

	| instanceVariablesToPushUp |
	
	instanceVariablesToPushUp := self selectInstanceVariablesToPushUpFrom: methods.
	
	instanceVariablesToPushUp ifNotEmpty: [ 
		self refactoringWarning: (self accessingInstanceVariableWarningMessage: instanceVariablesToPushUp)].

! !

!PushUpMethod class methodsFor: 'implemented superclass' stamp: 'HAW 1/23/2024 15:11:42'!
checkIfMethodIsImplementedInSuperclass: aMethodToTransfer

	^aMethodToTransfer methodClass superclass compiledMethodAt: aMethodToTransfer selector
		ifPresent: [ :superClassMethod | (self shouldWarnIf: aMethodToTransfer isImplementedInSuperclassAs: superClassMethod) not]
		ifAbsent: [ false ]! !

!PushUpMethod class methodsFor: 'equivalent methods' stamp: 'HAW 1/23/2024 15:21:15'!
equivalentMethodsOfSiblingsOfAll: methods

	| allEquivalentMethods |
	
	allEquivalentMethods := Dictionary new.
	
	methods do: [:method | | equivalentMethods |
		equivalentMethods := method methodClass superclass subclasses select: [ :aSubclass | self is: method equivalentIn: aSubclass ].
		equivalentMethods ifNotEmpty: [ allEquivalentMethods at: method put: equivalentMethods ]].

	^allEquivalentMethods ! !

!PushUpMethod class methodsFor: 'equivalent methods' stamp: 'HAW 1/23/2024 15:21:01'!
is: aMethod equivalentIn: aSubclass

	^ aSubclass ~= aMethod methodClass and: [
		aSubclass
			compiledMethodAt: aMethod selector
			ifPresent: [ :subclassMethod | subclassMethod equivalentTo: aMethod ]
			ifAbsent: [ false ]]! !


!PushUpMethod methodsFor: 'applying - private' stamp: 'HAW 1/23/2024 15:31:30'!
removeMethod

	subclassesToRemoveMethodFrom keysAndValuesDo: [ :methodWithSelectorToRemove :classesToRemoveMethodFrom |
		classesToRemoveMethodFrom do: [ :aClassToRemoveMethodFrom | 
			aClassToRemoveMethodFrom removeSelector: methodWithSelectorToRemove selector ]].
	
	! !


!PushUpMethod class methodsFor: 'instance creation' stamp: 'HAW 1/23/2024 15:36:00'!
for: aMethodToPushUp

	^self for: aMethodToPushUp additionallyRemovingFrom: Dictionary new pushingUp: Set new.! !

!PushUpMethod class methodsFor: 'pre-conditions' stamp: 'HAW 1/23/2024 15:01:14'!
assertIsValidToPushUp: methods

	self warnIfReferencesInstanceVariables: methods.
	self warnIfDifferentImplementationInSuperclass: methods! !

!PushUpMethod class methodsFor: 'instance variables' stamp: 'HAW 1/23/2024 15:10:16'!
selectInstanceVariablesToPushUpFrom: methods

	^ (methods collect: [:method | self selectAccessedInstanceVariablesThatCanBePushedUpFrom: method ]) asArray flatten asSet 
! !

!PushUpMethod class methodsFor: 'dependant methods' stamp: 'HAW 1/23/2024 15:22:52'!
selectDependantSelectorsFrom: aMethod using: visitedSelectors

	| selectors |

	visitedSelectors add: aMethod selector.

	selectors := self selectorsSentToSelf: aMethod.

	(selectors difference: visitedSelectors) do: [:selector | 
		self selectDependantSelectorsFrom: aMethod methodClass >> selector using: visitedSelectors]! !


!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/23/2024 15:40:29'!
requestRefactoringParameters

	self 
		selectDependantMethods;
		selectSiblingsWithEquivalentMethod
		! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/23/2024 15:41:47'!
selectDependantMethods

	| methods |

	methods := ((self refactoringClass selectAllMessageSendsToSelfNotImplementedInSuperClassFrom: methodToTransfer )
	                   collect: [ :selector | methodToTransfer methodClass >> selector ]) asSet.

	methods ifNotEmpty: [ self askMethodsToPushUpFrom: methods ].! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/23/2024 15:41:56'!
selectSiblingsWithEquivalentMethod

	| methods withEquivalentMethod |
	
	methods := Set withAll: dependantMethods.
	methods add: methodToTransfer.
	
	withEquivalentMethod := self refactoringClass equivalentMethodsOfSiblingsOfAll: methods.

	withEquivalentMethod ifNotEmpty: [ self askMethodsToRemoveFrom: withEquivalentMethod ].
! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/23/2024 12:58:14'!
withDependantMethodsMessage: aDependantMethods

	^ String streamContents: [ :stream |
		stream
			nextPutAll: methodToTransfer selector;
			nextPutAll: ' has the following dependant method(s): ';
			newLine;
			nextPutAll: (aDependantMethods collect: [:method | method selector ] ) asCommaStringAnd;
			newLine;
			nextPutAll: 'How do you want to proceed?' ]! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 1/23/2024 12:54:23'!
withEquivalentMethodsMessage: withEquivalentMethod

	^ String streamContents: [ :stream |		
		withEquivalentMethod associationsDo: [:assoc |
			stream
				nextPutAll: assoc key selector;
				nextPutAll: ' has equivalent methods declared in ';
				nextPutAll: assoc value asCommaStringAnd;
				newLine.
		].
		stream nextPutAll: 'How do you want to proceed?' ].! !

!PushUpMethodApplier methodsFor: 'initialization' stamp: 'HAW 1/23/2024 12:48:06'!
initialize 

	subclassesToRemoveMethodsFrom := Dictionary new.
	dependantMethods := Set new.! !

!methodRemoval: PushUpMethodApplier #withEquivalentMethodMessage: stamp: 'HAW 1/23/2024 12:57:15'!
PushUpMethodApplier removeSelector: #withEquivalentMethodMessage:!
!methodRemoval: PushUpMethod class #assert:implementsEquivalentMethodIn: stamp: 'HAW 1/23/2024 15:21:01'!
PushUpMethod class removeSelector: #assert:implementsEquivalentMethodIn:!
!methodRemoval: PushUpMethod class #selectSiblingsWithEquivalentMethod: stamp: 'HAW 1/23/2024 15:19:56'!
PushUpMethod class removeSelector: #selectSiblingsWithEquivalentMethod:!
!methodRemoval: PushUpMethod class #errorMessageForMethodCannotAccessInstanceVariable stamp: 'HAW 1/23/2024 15:03:39'!
PushUpMethod class removeSelector: #errorMessageForMethodCannotAccessInstanceVariable!
!methodRemoval: PushUpMethod class #warnIfMethodAccessInstanceVariable: stamp: 'HAW 1/23/2024 15:02:11'!
PushUpMethod class removeSelector: #warnIfMethodAccessInstanceVariable:!
!methodRemoval: PushUpMethod class #for:additionallyRemovingFrom: stamp: 'HAW 1/23/2024 14:54:49'!
PushUpMethod class removeSelector: #for:additionallyRemovingFrom:!
!methodRemoval: PushUpMethod class #methodWithDifferentImplementationInSuperClass: stamp: 'HAW 1/23/2024 15:00:48'!
PushUpMethod class removeSelector: #methodWithDifferentImplementationInSuperClass:!
!methodRemoval: PushUpMethod class #signalMethodCannotAccessInstanceVariable stamp: 'HAW 1/23/2024 15:03:49'!
PushUpMethod class removeSelector: #signalMethodCannotAccessInstanceVariable!
!methodRemoval: PushUpMethod class #checkIfMethodIsImplementedInSuperClass: stamp: 'HAW 1/23/2024 15:00:24'!
PushUpMethod class removeSelector: #checkIfMethodIsImplementedInSuperClass:!
!methodRemoval: PushUpMethod #initializeFor: stamp: 'HAW 1/23/2024 15:24:59'!
PushUpMethod removeSelector: #initializeFor:!
!methodRemoval: PushUpMethod #moveMethodAfterMovingAccessedVariables stamp: 'HAW 1/23/2024 15:26:46'!
PushUpMethod removeSelector: #moveMethodAfterMovingAccessedVariables!

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


!PushUpMethodApplier reorganize!
('refactoring - creation' createRefactoring refactoringClass)
('refactoring - parameters request' askMethodsToPushUpFrom: askMethodsToRemoveFrom: implementsEquivalentMethod: requestRefactoringParameters selectDependantMethods selectSiblingsWithEquivalentMethod withDependantMethodsMessage: withEquivalentMethodsMessage:)
('initialization' initialize)
!

