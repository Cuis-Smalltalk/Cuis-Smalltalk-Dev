'From Cuis 6.0 [latest update: #5908] on 12 July 2023 at 9:19:16 pm'!
!classDefinition: #AddAsSubclassResponsability category: #'Tools-Refactoring'!
TransferMethod subclass: #AddAsSubclassResponsability
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #TransferMethodApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #TransferMethodApplier
	instanceVariableNames: 'browser methodToMove methodToTransfer '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #AddAsSubclassResponsabilityApplier category: #'Tools-Refactoring'!
TransferMethodApplier subclass: #AddAsSubclassResponsabilityApplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!TransferMethod methodsFor: 'applying - private' stamp: 'HAW 7/12/2023 20:41:49'!
implementSubclassResponsibilityIn: aClass

	^ aClass
		compile: self subclassResponsibilitySourceCode
		classified: self methodCategory! !

!TransferMethod methodsFor: 'applying - private' stamp: 'HAW 7/12/2023 20:30:37'!
subclassResponsibilitySourceCode

	^ String streamContents: [ :stream |
		stream
			nextPutAll: method selectorAndArgumentsAsString;
			newLine; newLine;
			tab;
			nextPutAll: 'self ';
			nextPutAll: #subclassResponsibility asString ]! !


!TransferMethod class methodsFor: 'warnings' stamp: 'HAW 7/12/2023 20:37:43'!
methodImplementedInSuperclassWarningMessage

	^'Method with different implementation already exist in superclass'.

	! !

!TransferMethod class methodsFor: 'warnings' stamp: 'HAW 7/12/2023 20:45:25'!
shouldWarnIf: aMethodToTransfer isImplementedInSuperclassAs: superclassMethod

	^ (superclassMethod equivalentTo: aMethodToTransfer) or: [ superclassMethod isAbstract ]! !

!TransferMethod class methodsFor: 'warnings' stamp: 'HAW 7/12/2023 20:45:25'!
warnIfMethodIsImplementedInSuperClass: aMethodToTransfer

	aMethodToTransfer methodClass superclass
		compiledMethodAt: aMethodToTransfer selector
		ifPresent: [ :superclassMethod | (self shouldWarnIf: aMethodToTransfer isImplementedInSuperclassAs: superclassMethod)
			ifFalse: [ self refactoringWarning: self methodImplementedInSuperclassWarningMessage ]]
		ifAbsent: []! !


!AddAsSubclassResponsability methodsFor: 'applying - private' stamp: 'HAW 7/12/2023 20:26:10'!
removeMethod

	! !


!PushDownMethod methodsFor: 'applying - private' stamp: 'HAW 7/12/2023 20:42:47'!
implementSubclassResponsibility

	^ self implementSubclassResponsibilityIn: method methodClass
	! !


!TransferMethod methodsFor: 'initialization' stamp: 'HAW 7/12/2023 20:51:47'!
initializeFor: aMethodToTransfer

	method := aMethodToTransfer! !


!AddAsSubclassResponsability methodsFor: 'applying - private' stamp: 'HAW 7/12/2023 20:43:17'!
moveMethod

	self implementSubclassResponsibilityIn: method methodClass superclass
	! !


!AddAsSubclassResponsability class methodsFor: 'instance creation' stamp: 'HAW 7/12/2023 20:36:51'!
for: aMethod

	self warnIfMethodIsImplementedInSuperClass: aMethod.

	^self new initializeFor: aMethod! !


!PushDownMethodToOneSubclass methodsFor: 'applying - private' stamp: 'HAW 7/12/2023 21:07:34'!
removeMethod

	implementSubclassResponsibility
		ifTrue: [ self implementSubclassResponsibility ]
		ifFalse: [ super removeMethod ]! !


!PushDownMethodToSubclasses methodsFor: 'applying - private' stamp: 'HAW 7/12/2023 21:05:04'!
removeMethod

	self implementSubclassResponsibility
							! !


!PushUpMethod methodsFor: 'initialization' stamp: 'HAW 7/12/2023 20:53:08'!
initializeFor: aMethodToPushUp additionallyRemovingFrom: aSubclassesToRemoveMethodFrom

	method := aMethodToPushUp.
	subclassesToRemoveMethodFrom := aSubclassesToRemoveMethodFrom.! !


!PushUpMethod class methodsFor: 'instance creation' stamp: 'HAW 7/12/2023 20:32:58'!
for: aMethodToPushUp additionallyRemovingFrom: subclassesToRemoveMethodFrom

	self warnIfMethodAccessInstanceVariable: aMethodToPushUp.
	self assert: subclassesToRemoveMethodFrom areSiblingsOf: aMethodToPushUp methodClass.
	self assert: aMethodToPushUp isImplementedIn: subclassesToRemoveMethodFrom.
	self assert: aMethodToPushUp isEquivalentlyImplementedIn: subclassesToRemoveMethodFrom.
	self warnIfMethodIsImplementedInSuperClass: aMethodToPushUp.

	^self new initializeFor: aMethodToPushUp additionallyRemovingFrom: subclassesToRemoveMethodFrom.! !


!TransferMethodApplier methodsFor: 'initialization' stamp: 'HAW 7/12/2023 20:47:51'!
initializeOn: aBrowser for: aMethodToTransfer

	browser := aBrowser.
	methodToTransfer := aMethodToTransfer.! !

!TransferMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 7/12/2023 20:47:51'!
createRefactoring

	^self refactoringClass for: methodToTransfer.! !


!TransferMethodApplier class methodsFor: 'instance creation' stamp: 'HAW 7/12/2023 20:48:14'!
on: aBrowser for: aMethodToTransfer

	^self new initializeOn: aBrowser for: aMethodToTransfer ! !


!MoveToInstanceOrClassMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 7/12/2023 20:47:51'!
methodHasSenders

	^(methodToTransfer methodClass whichSelectorsReferTo: methodToTransfer selector) isEmpty not.! !


!PushDownMethodToOneSubclassApplier methodsFor: 'refactoring - creation' stamp: 'HAW 7/12/2023 20:47:51'!
createRefactoring

	^self refactoringClass for: methodToTransfer to: subclassToPushDownTo
	! !

!PushDownMethodToOneSubclassApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 7/12/2023 20:47:51'!
requestRefactoringParameters

	subclassToPushDownTo := (SelectionMenu selections: methodToTransfer methodClass subclasses)
		startUpWithCaption: 'Select subclass to push down method to'.
	subclassToPushDownTo ifNil: [ self endRequest ]! !


!PushUpMethodApplier methodsFor: 'refactoring - creation' stamp: 'HAW 7/12/2023 20:47:51'!
createRefactoring

	^self refactoringClass for: methodToTransfer additionallyRemovingFrom: subclassesToRemoveMethodsFrom ! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 7/12/2023 20:47:51'!
implementsEquivalentMethod: aSubclass

	^ aSubclass ~= methodToTransfer methodClass and: [
		aSubclass
			compiledMethodAt: methodToTransfer selector
			ifPresent: [ :subclassMethod | subclassMethod equivalentTo: methodToTransfer ]
			ifAbsent: [ false ]]! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 7/12/2023 20:47:51'!
requestRefactoringParameters

	| withEquivalentMethod |

	withEquivalentMethod := methodToTransfer methodClass superclass subclasses select: [ :aSubclass |
		self implementsEquivalentMethod: aSubclass].

	withEquivalentMethod ifNotEmpty: [ self askMethodsToRemoveFrom: withEquivalentMethod ]! !

!PushUpMethodApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 7/12/2023 20:47:51'!
withEquivalentMethodMessage: withEquivalentMethod

	^ String streamContents: [ :stream |
		stream
			nextPutAll: methodToTransfer selector;
			nextPutAll: ' has equivalent methods declared in '.
		withEquivalentMethod asCommaSeparated:  [:aClass| stream print: aClass] on: stream.
		stream
			newLine;
			nextPutAll: 'How do you want to proceed?' ]! !

!methodRemoval: AddAsSubclassResponsabilityApplier class #on:for: stamp: 'HAW 7/12/2023 20:48:22'!
AddAsSubclassResponsabilityApplier class removeSelector: #on:for:!
!methodRemoval: AddAsSubclassResponsabilityApplier #implementsEquivalentMethod: stamp: 'HAW 7/12/2023 20:49:21'!
AddAsSubclassResponsabilityApplier removeSelector: #implementsEquivalentMethod:!
!methodRemoval: AddAsSubclassResponsabilityApplier #initializeOn:for: stamp: 'HAW 7/12/2023 20:49:48'!
AddAsSubclassResponsabilityApplier removeSelector: #initializeOn:for:!
!methodRemoval: AddAsSubclassResponsabilityApplier #showChanges stamp: 'HAW 7/12/2023 20:49:31'!
AddAsSubclassResponsabilityApplier removeSelector: #showChanges!
!methodRemoval: AddAsSubclassResponsabilityApplier #createRefactoring stamp: 'HAW 7/12/2023 20:48:42'!
AddAsSubclassResponsabilityApplier removeSelector: #createRefactoring!
!methodRemoval: AddAsSubclassResponsabilityApplier #requestRefactoringParameters stamp: 'HAW 7/12/2023 20:49:54'!
AddAsSubclassResponsabilityApplier removeSelector: #requestRefactoringParameters!
!methodRemoval: AddAsSubclassResponsabilityApplier #informChangesToBrowser stamp: 'HAW 7/12/2023 20:49:28'!
AddAsSubclassResponsabilityApplier removeSelector: #informChangesToBrowser!
!methodRemoval: PushUpMethod class #warnIfMethodIsImplementedInSuperClass: stamp: 'HAW 7/12/2023 20:34:24'!
PushUpMethod class removeSelector: #warnIfMethodIsImplementedInSuperClass:!
!methodRemoval: PushUpMethod class #warnIfExistMethodToPushUpOnSuperClass: stamp: 'HAW 7/12/2023 20:32:58'!
PushUpMethod class removeSelector: #warnIfExistMethodToPushUpOnSuperClass:!
!methodRemoval: PushUpMethod class #methodToPushUpExistOnSuperclassWarningMessage stamp: 'HAW 7/12/2023 20:34:48'!
PushUpMethod class removeSelector: #methodToPushUpExistOnSuperclassWarningMessage!
!methodRemoval: PushDownMethod #subclassResponsibilitySourceCode stamp: 'HAW 7/12/2023 20:30:37'!
PushDownMethod removeSelector: #subclassResponsibilitySourceCode!
!methodRemoval: AddAsSubclassResponsability class #warnIfExistMethodToAddAsSubclassResponsabilityOnSuperClass: stamp: 'HAW 7/12/2023 20:36:51'!
AddAsSubclassResponsability class removeSelector: #warnIfExistMethodToAddAsSubclassResponsabilityOnSuperClass:!
!methodRemoval: AddAsSubclassResponsability class #methodToAddAsSubclassResponsabilityExistOnSuperclassWarningMessage stamp: 'HAW 7/12/2023 20:37:05'!
AddAsSubclassResponsability class removeSelector: #methodToAddAsSubclassResponsabilityExistOnSuperclassWarningMessage!
!methodRemoval: AddAsSubclassResponsability #initializeFor: stamp: 'HAW 7/12/2023 20:29:21'!
AddAsSubclassResponsability removeSelector: #initializeFor:!
!methodRemoval: AddAsSubclassResponsability #methodCategory stamp: 'HAW 7/12/2023 20:28:41'!
AddAsSubclassResponsability removeSelector: #methodCategory!
!methodRemoval: AddAsSubclassResponsability #apply stamp: 'HAW 7/12/2023 20:25:54'!
AddAsSubclassResponsability removeSelector: #apply!
!methodRemoval: AddAsSubclassResponsability #subclassResponsibilitySourceCode stamp: 'HAW 7/12/2023 20:30:37'!
AddAsSubclassResponsability removeSelector: #subclassResponsibilitySourceCode!
!methodRemoval: AddAsSubclassResponsability #implementSubclassResponsibility stamp: 'HAW 7/12/2023 20:27:00'!
AddAsSubclassResponsability removeSelector: #implementSubclassResponsibility!
!methodRemoval: TransferMethod class #methodToPushUpExistOnSuperclassWarningMessage stamp: 'HAW 7/12/2023 20:37:43'!
TransferMethod class removeSelector: #methodToPushUpExistOnSuperclassWarningMessage!
!methodRemoval: TransferMethod #implementSubclassResponsibility stamp: 'HAW 7/12/2023 20:42:52'!
TransferMethod removeSelector: #implementSubclassResponsibility!

!TransferMethod reorganize!
('initialization' initializeFor:)
('applying - private' implementSubclassResponsibilityIn: methodCategory moveMethod removeMethod subclassResponsibilitySourceCode)
('applying' apply)
!


!TransferMethod class reorganize!
('warnings' methodImplementedInSuperclassWarningMessage shouldWarnIf:isImplementedInSuperclassAs: warnIfMethodIsImplementedInSuperClass:)
!

!classDefinition: #AddAsSubclassResponsability category: #'Tools-Refactoring'!
TransferMethod subclass: #AddAsSubclassResponsability
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!AddAsSubclassResponsability reorganize!
('applying - private' moveMethod removeMethod)
!


!AddAsSubclassResponsability class reorganize!
('instance creation' for:)
!


!PushUpMethod class reorganize!
('instance creation' for: for:additionallyRemovingFrom:)
('pre-conditions' assert:areSiblingsOf: assert:isEquivalentlyImplementedIn: assert:isImplementedIn:)
('warnings' accessingInstanceVariableWarningMessage: selectAccessedInstanceVariables: warnIfMethodAccessInstanceVariable:)
('exceptions' errorMessageForMethodCannotAccessInstanceVariable errorMessageForMethodCannotBeRemovedMethodFrom: errorMessageForNoSiblings: errorMessageForNotEquivalentMethods: signalMethodCannotAccessInstanceVariable signalMethodCannotBeRemovedMethodFrom: signalNoSiblings: signalNotEquivalentMethodsFor:)
!

!classDefinition: #TransferMethodApplier category: #'Tools-Refactoring'!
RefactoringApplier subclass: #TransferMethodApplier
	instanceVariableNames: 'browser methodToTransfer'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!TransferMethodApplier reorganize!
('initialization' initializeOn:for:)
('refactoring - creation' createRefactoring refactoringClass)
('refactoring - parameters request' requestRefactoringParameters)
('refactoring - changes' informChangesToBrowser showChanges)
!

!classDefinition: #AddAsSubclassResponsabilityApplier category: #'Tools-Refactoring'!
TransferMethodApplier subclass: #AddAsSubclassResponsabilityApplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!AddAsSubclassResponsabilityApplier reorganize!
('refactoring - creation' refactoringClass)
!


!AddAsSubclassResponsabilityApplier class reorganize!
('as yet unclassified')
!


!MoveToInstanceOrClassMethodApplier reorganize!
('refactoring - creation' refactoringClass)
('refactoring - parameters request' confirmationMessageText methodHasSenders requestRefactoringParameters)
!


!PushDownMethodToOneSubclassApplier reorganize!
('refactoring - creation' createRefactoring refactoringClass)
('refactoring - parameters request' requestRefactoringParameters)
!


!PushUpMethodApplier reorganize!
('refactoring - creation' createRefactoring refactoringClass)
('refactoring - parameters request' askMethodsToRemoveFrom: implementsEquivalentMethod: requestRefactoringParameters withEquivalentMethodMessage:)
('initialization' initialize)
!
