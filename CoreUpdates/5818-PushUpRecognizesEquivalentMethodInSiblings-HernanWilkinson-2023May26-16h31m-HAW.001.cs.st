'From Cuis 6.0 [latest update: #5813] on 26 May 2023 at 5:56:01 pm'!
!classDefinition: #PushUpMethod category: #'Tools-Refactoring'!
MoveMethod subclass: #PushUpMethod
	instanceVariableNames: 'subclassesToRemoveMethodFrom '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #PushUpMethodApplier category: #'Tools-Refactoring'!
MoveMethodApplier subclass: #PushUpMethodApplier
	instanceVariableNames: 'subclassesToRemoveMethodsFrom '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!PushUpMethod methodsFor: 'applying - private' stamp: 'MEGL 5/26/2023 16:53:37'!
removeMethod

	super removeMethod.
	subclassesToRemoveMethodFrom do: [:class | class removeSelector: method selector ].! !

!PushUpMethod methodsFor: 'initialization' stamp: 'MEGL 5/26/2023 16:53:37'!
initializeFor: aMethodToPushup additionallyRemovingFrom: aSubclassesToRemoveMethodFrom

	method := aMethodToPushup.
	subclassesToRemoveMethodFrom := aSubclassesToRemoveMethodFrom.! !


!PushUpMethod class methodsFor: 'instance creation' stamp: 'HAW 5/26/2023 17:25:47'!
for: aMethodToPushUp additionallyRemovingFrom: subclassesToRemoveMethodFrom

	self assertIsValidToPushUpMethod: aMethodToPushUp.
	self assert: subclassesToRemoveMethodFrom areSiblingsOf: aMethodToPushUp methodClass.
	self assert: aMethodToPushUp isImplementedIn: subclassesToRemoveMethodFrom.
	self assert: aMethodToPushUp isEquivalentlyImplementedIn: subclassesToRemoveMethodFrom.
	self warnIfExistMethodToPushUpOnSuperClass: aMethodToPushUp.
	
	^self new initializeFor: aMethodToPushUp additionallyRemovingFrom: subclassesToRemoveMethodFrom.! !

!PushUpMethod class methodsFor: 'pre-conditions' stamp: 'HAW 5/26/2023 17:19:38'!
assert: subclassesToRemoveMethodFrom areSiblingsOf: aClassToPushUpMethodFrom

	| noSiblings |
	
	noSiblings := subclassesToRemoveMethodFrom reject: [ :aSubclass |
		aSubclass superclass = aClassToPushUpMethodFrom superclass ].
	
	noSiblings ifNotEmpty: [ self signalNoSiblings: noSiblings ].! !

!PushUpMethod class methodsFor: 'pre-conditions' stamp: 'HAW 5/26/2023 17:29:39'!
assert: aMethodToPushUp isEquivalentlyImplementedIn: subclassesToRemoveMethodFrom
	
	| notEquivalent |
	
	notEquivalent := subclassesToRemoveMethodFrom reject: [ :aSubclass |
		aSubclass >> aMethodToPushUp selector equivalentTo: aMethodToPushUp ].
	
	notEquivalent ifNotEmpty: [ self signalNotEquivalentMethodsFor: notEquivalent ].! !

!PushUpMethod class methodsFor: 'pre-conditions' stamp: 'HAW 5/26/2023 17:10:49'!
assert: aMethodToPushUp isImplementedIn: subclassesToRemoveMethodFrom
	
	| subclassesNotImplementing |
	
	subclassesNotImplementing := subclassesToRemoveMethodFrom reject: [ :aSubclass | 
		aSubclass includesSelector: aMethodToPushUp selector ].
	
	subclassesNotImplementing ifNotEmpty: [
		self signalMethodCannotBeRemovedMethodFrom: subclassesNotImplementing ]
	! !

!PushUpMethod class methodsFor: 'exceptions' stamp: 'HAW 5/26/2023 17:14:45'!
errorMessageForMethodCannotBeRemovedMethodFrom: subclassesNotImplementing

	^String streamContents: [ :stream |
		stream
			nextPutAll: 'Can not remove method because is not implemented in:';
			newLine.

		subclassesNotImplementing asCommaSeparated: [ :aClass | stream print: aClass ] on: stream ].
		! !

!PushUpMethod class methodsFor: 'exceptions' stamp: 'HAW 5/26/2023 17:24:10'!
errorMessageForNoSiblings: noSiblings

	^String streamContents: [ :stream |
		stream
			nextPutAll: 'Can not remove method from classes that are not siblings:';
			newLine.

		noSiblings asCommaSeparated: [ :aClass | stream print: aClass ] on: stream ].
			! !

!PushUpMethod class methodsFor: 'exceptions' stamp: 'HAW 5/26/2023 17:29:24'!
errorMessageForNotEquivalentMethods: notEquivalent

	^String streamContents: [ :stream |
		stream
			nextPutAll: 'Can not remove not equivalent methods from';
			newLine.

		notEquivalent asCommaSeparated: [ :aClass | stream print: aClass ] on: stream ].
			! !

!PushUpMethod class methodsFor: 'exceptions' stamp: 'HAW 5/26/2023 17:11:17'!
signalMethodCannotBeRemovedMethodFrom: subclassesNotImplementing
	
	self refactoringError: (self errorMessageForMethodCannotBeRemovedMethodFrom: subclassesNotImplementing)! !

!PushUpMethod class methodsFor: 'exceptions' stamp: 'HAW 5/26/2023 17:24:10'!
signalNoSiblings: noSiblings 

	self refactoringError: (self errorMessageForNoSiblings: noSiblings)! !

!PushUpMethod class methodsFor: 'exceptions' stamp: 'HAW 5/26/2023 17:29:39'!
signalNotEquivalentMethodsFor: notEquivalent

	self refactoringError: (self errorMessageForNotEquivalentMethods: notEquivalent)! !


!PushUpMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 5/26/2023 17:40:09'!
createRefactoring

	^self refactoringClass for: methodToMove additionallyRemovingFrom: subclassesToRemoveMethodsFrom ! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 5/26/2023 17:43:18'!
askMethodsToRemoveFrom: withEquivalentMethod

	| answer |
	
	answer := PopUpMenu 
		withCaption: (self withEquivalentMethodMessage: withEquivalentMethod) 
		chooseFrom: #('Only PushUp' 'PushUp and delete all equivalent methods').

	answer = 1 ifTrue: [ ^subclassesToRemoveMethodsFrom := #() ].
	answer = 2 ifTrue: [ ^subclassesToRemoveMethodsFrom := withEquivalentMethod ].
	self endRequest ! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 5/26/2023 17:43:29'!
implementsEquivalentMethod: aSubclass

	^ aSubclass ~= methodToMove methodClass and: [
		aSubclass 
			compiledMethodAt: methodToMove selector
			ifPresent: [ :subclassMethod | subclassMethod equivalentTo: methodToMove ]
			ifAbsent: [ false ]]! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 5/26/2023 17:43:54'!
requestRefactoringParameters

	| withEquivalentMethod |

	withEquivalentMethod := methodToMove methodClass superclass subclasses select: [ :aSubclass |
		self implementsEquivalentMethod: aSubclass].
	
	withEquivalentMethod ifNotEmpty: [ self askMethodsToRemoveFrom: withEquivalentMethod ]! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 5/26/2023 17:55:07'!
withEquivalentMethodMessage: withEquivalentMethod

	^ String streamContents: [ :stream |
		stream
			nextPutAll: methodToMove selector;
			nextPutAll: ' has equivalent methods declared in '.
		withEquivalentMethod asCommaSeparated:  [:aClass| stream print: aClass] on: stream.
		stream 
			newLine;
			nextPutAll: 'How do you want to proceed?' ]! !

!PushUpMethodApplier methodsFor: 'initialization' stamp: 'HAW 5/26/2023 17:45:14'!
initialize 

	subclassesToRemoveMethodsFrom := #()! !


!PushDownMethod methodsFor: 'applying - private' stamp: 'MEGL 5/26/2023 16:55:43'!
removeMethod

	super removeMethod.
	method methodClass compile: self subclassResponsibilitySourceCode
							! !

!PushDownMethod methodsFor: 'applying - private' stamp: 'HAW 5/26/2023 17:50:47'!
subclassResponsibilitySourceCode

	^ String streamContents: [ :stream |
		stream
			nextPutAll: method selector;
			newLine; newLine;
			tab;
			nextPutAll: 'self ';
			nextPutAll: #subclassResponsibility asString ]! !


!PushUpMethod class methodsFor: 'instance creation' stamp: 'MEGL 5/26/2023 16:52:20'!
for: aMethodToPushUp

	^self for: aMethodToPushUp additionallyRemovingFrom: #()! !

!methodRemoval: PushUpMethod class #signalNotEquivalentFor: stamp: 'HAW 5/26/2023 17:29:39'!
PushUpMethod class removeSelector: #signalNotEquivalentFor:!
!methodRemoval: PushUpMethod class #errorMessageFoNoSiblings: stamp: 'HAW 5/26/2023 17:24:10'!
PushUpMethod class removeSelector: #errorMessageFoNoSiblings:!
!methodRemoval: PushUpMethod class #findEquivalentMethodsOnASiblingClass: stamp: 'HAW 5/26/2023 17:54:13'!
PushUpMethod class removeSelector: #findEquivalentMethodsOnASiblingClass:!
!methodRemoval: PushUpMethod #initializeFor:removingFrom: stamp: 'MEGL 5/26/2023 16:52:59'!
PushUpMethod removeSelector: #initializeFor:removingFrom:!
!classDefinition: #PushUpMethod category: #'Tools-Refactoring'!
MoveMethod subclass: #PushUpMethod
	instanceVariableNames: 'subclassesToRemoveMethodFrom'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!PushUpMethod reorganize!
('applying - private' moveMethod removeMethod)
('initialization' initializeFor:additionallyRemovingFrom:)
!


!PushUpMethod class reorganize!
('instance creation' for: for:additionallyRemovingFrom:)
('pre-conditions' assert:areSiblingsOf: assert:isEquivalentlyImplementedIn: assert:isImplementedIn: assertIsNotAccessingInstanceVariable: assertIsValidToPushUpMethod:)
('warnings' methodToPushUpExistOnSuperclassWarningMessage warnIfExistMethodToPushUpOnSuperClass:)
('exceptions' errorMessageForMethodCannotAccessInstanceVariable errorMessageForMethodCannotBeRemovedMethodFrom: errorMessageForNoSiblings: errorMessageForNotEquivalentMethods: signalMethodCannotAccessInstanceVariable signalMethodCannotBeRemovedMethodFrom: signalNoSiblings: signalNotEquivalentMethodsFor:)
!

!classDefinition: #PushUpMethodApplier category: #'Tools-Refactoring'!
MoveMethodApplier subclass: #PushUpMethodApplier
	instanceVariableNames: 'subclassesToRemoveMethodsFrom'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!PushUpMethodApplier reorganize!
('refactoring - creation' createRefactoring refactoringClass)
('refactoring - parameters request' askMethodsToRemoveFrom: implementsEquivalentMethod: requestRefactoringParameters withEquivalentMethodMessage:)
('initialization' initialize)
!

