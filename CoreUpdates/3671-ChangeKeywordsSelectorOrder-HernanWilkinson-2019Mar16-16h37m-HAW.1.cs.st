'From Cuis 5.0 of 7 November 2016 [latest update: #3665] on 17 March 2019 at 6:57:41 am'!
!classDefinition: #ChangeSelectorKeepingParameters category: #'Tools-Refactoring'!
ChangeSelector subclass: #ChangeSelectorKeepingParameters
	instanceVariableNames: 'newSelectorKeywords currentImplementorMethodNode'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ChangeKeywordsSelectorOrder category: #'Tools-Refactoring'!
ChangeSelectorKeepingParameters subclass: #ChangeKeywordsSelectorOrder
	instanceVariableNames: 'changedOrder'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RenameSelector category: #'Tools-Refactoring'!
ChangeSelectorKeepingParameters subclass: #RenameSelector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ChangeSelectorKeepingParametersApplier category: #'Tools-Refactoring'!
ChangeSelectorApplier subclass: #ChangeSelectorKeepingParametersApplier
	instanceVariableNames: 'newSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #ChangeKeywordsSelectorOrderApplier category: #'Tools-Refactoring'!
ChangeSelectorKeepingParametersApplier subclass: #ChangeKeywordsSelectorOrderApplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!
!classDefinition: #RenameSelectorApplier category: #'Tools-Refactoring'!
ChangeSelectorKeepingParametersApplier subclass: #RenameSelectorApplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!SequenceableCollection methodsFor: 'enumerating' stamp: 'HAW 3/17/2019 05:43:39'!
withIndexDo: elementAndIndexBlock separatedBy: separatorBlock
	"Evaluate the elementAndIndexBlock for all elements in the receiver,
	and evaluate the separatorBlock between."

	1 to: self size do: [:index |
		index = 1 ifFalse: [separatorBlock value].
		elementAndIndexBlock value: (self at: index) value: index]! !


!CodeWindow methodsFor: 'as yet unclassified' stamp: 'HAW 3/16/2019 17:58:08'!
changeKeywordOrder

	model selectedMessageName ifNotNil: [ :oldSelector |
		ChangeKeywordsSelectorOrderApplier createAndValueHandlingExceptions: [
			ChangeKeywordsSelectorOrderApplier on: model for: oldSelector in: model selectedClassOrMetaClass ]].! !


!TestCase methodsFor: 'assertions' stamp: 'HAW 3/17/2019 06:29:53'!
shouldntFail: aBlock

	self shouldnt: aBlock raise: Error! !


!ChangeSelectorKeepingParameters methodsFor: 'create new implementors - private' stamp: 'HAW 3/17/2019 06:24:07'!
addImplementorSelectorRanges: aKeywordRange at: index to: rangesToNewKeywords

	self subclassResponsibility ! !

!ChangeSelectorKeepingParameters methodsFor: 'create new implementors - private' stamp: 'HAW 3/17/2019 06:24:07'!
implementorNewSourceCodeOf: anImplementor

	|  newSource rangesToNewKeywords |

	rangesToNewKeywords := OrderedCollection new.
	currentImplementorMethodNode := anImplementor methodNode.

	currentImplementorMethodNode selectorKeywordsPositions withIndexDo: [ :aKeywordRange :index |
		self addImplementorSelectorRanges: aKeywordRange at: index to: rangesToNewKeywords ].

	newSource := anImplementor sourceCode copyReplacing: rangesToNewKeywords.
	^newSource! !

!ChangeSelectorKeepingParameters methodsFor: 'rename senders - private' stamp: 'HAW 3/17/2019 06:02:54'!
addMessageSendSelectorKeywordRangesOf: aMethodNode to: rangesToKeywords

	| oldSelectorKeywordsRanges |

	oldSelectorKeywordsRanges := aMethodNode messageSendSelectorKeywordPositionsOf: oldSelector ifAbsent: [ #() ].
	oldSelectorKeywordsRanges do: [ :aMessageSendSelectorRanges |
		aMessageSendSelectorRanges withIndexDo: [ :aRange :index | rangesToKeywords add: aRange -> (newSelectorKeywords at: index) ]].
	! !

!ChangeSelectorKeepingParameters methodsFor: 'initialization' stamp: 'HAW 3/17/2019 06:03:03'!
initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders

	super initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders.

	newSelectorKeywords := newSelector keywords.
	! !


!ChangeSelectorKeepingParameters class methodsFor: 'instance creation' stamp: 'HAW 3/17/2019 05:58:22'!
from: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders

	self assertIsValidToRenameFrom: anOldSelector to: aNewSelector.
	self assertIsNotEmpty: aCollectionOfImplementors signalMessageText: self implementorsCanNotBeEmptyErrorMessage.
	self assertAllImplementors: aCollectionOfImplementors haveSame: anOldSelector.
	self assertNoImplementorClassIn: aCollectionOfImplementors implements: aNewSelector.
	self assertAllSenders: aCollectionOfSenders send: anOldSelector.

	self warnIfImplementionsOf: aNewSelector overridesImplementationInSuperclassesOf: aCollectionOfImplementors.

	^self new initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders
	! !

!ChangeSelectorKeepingParameters class methodsFor: 'instance creation' stamp: 'HAW 3/17/2019 05:58:34'!
from: anOldSelector to: aNewSelector in: aClassToRefactor

	| implementors senders |

	implementors := OrderedCollection new.
	senders := OrderedCollection new.

	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders forClassAndMetaOf: aClassToRefactor theNonMetaClass.

	^self from: anOldSelector to: aNewSelector implementors: implementors senders: senders ! !

!ChangeSelectorKeepingParameters class methodsFor: 'instance creation' stamp: 'HAW 3/17/2019 05:58:38'!
from: anOldSelector to: aNewSelector inCategoriesAndHierarchyOf: aClass organizedBy: anOrganization

	| implementors senders |

	implementors := IdentitySet new.
	senders := IdentitySet new.

	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategoriesAndHierarchyOf: aClass organizedBy: anOrganization.

	^self from: anOldSelector to: aNewSelector implementors: implementors senders: senders
! !

!ChangeSelectorKeepingParameters class methodsFor: 'instance creation' stamp: 'HAW 3/17/2019 05:58:43'!
from: anOldSelector to: aNewSelector inCategoryOf: aClass organizedBy: anOrganization

	| implementors senders |

	implementors := OrderedCollection new.
	senders := OrderedCollection new.

	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inCategory: aClass category organizedBy: anOrganization.

	^self from: anOldSelector to: aNewSelector implementors: implementors senders: senders
! !

!ChangeSelectorKeepingParameters class methodsFor: 'instance creation' stamp: 'HAW 3/17/2019 05:58:47'!
from: anOldSelector to: aNewSelector inHierarchyOf: aClass

	| implementors senders |

	implementors := OrderedCollection new.
	senders := OrderedCollection new.
	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inHierarchyOf: aClass.

	^self from: anOldSelector to: aNewSelector implementors: implementors senders: senders

	! !

!ChangeSelectorKeepingParameters class methodsFor: 'instance creation' stamp: 'HAW 3/17/2019 05:58:52'!
from: anOldSelector to: aNewSelector inSystem: aSystem

	| implementors senders |

	implementors := OrderedCollection new.
	senders := OrderedCollection new.

	self addImplementorsOf: anOldSelector to: implementors andSendersTo: senders inSystem: aSystem.

	^self from: anOldSelector to: aNewSelector implementors: implementors senders: senders
	! !

!ChangeSelectorKeepingParameters class methodsFor: 'pre-conditions' stamp: 'HAW 3/17/2019 05:59:02'!
assert: aNewSelector hasTheSameNumberOfArgumentsAs: anOldSelector

	aNewSelector numArgs ~= anOldSelector numArgs ifTrue: [ self signalNewSelector: aNewSelector doesNotHaveSameNumberOfArgumentsAs: anOldSelector ]! !

!ChangeSelectorKeepingParameters class methodsFor: 'pre-conditions' stamp: 'HAW 3/17/2019 05:59:07'!
assert: aNewSelector isNotEqualTo: anOldSelector

	 aNewSelector = anOldSelector ifTrue: [ self signalNewSelectorEqualToOldSelector]! !

!ChangeSelectorKeepingParameters class methodsFor: 'pre-conditions' stamp: 'HAW 3/17/2019 05:59:44'!
assert: aNewSelector isOfSameTypeAs: anOldSelector

	(self isRenamingBetweenBinary: anOldSelector andKeywordOfOneParameter: aNewSelector) ifTrue: [ ^self ].
	(self isRenamingBetweenBinary: aNewSelector andKeywordOfOneParameter: anOldSelector) ifTrue: [ ^self ].

	aNewSelector precedence ~= anOldSelector precedence ifTrue: [
		self signalNewSelector: aNewSelector isNotOfSameTypeAs: anOldSelector ]! !

!ChangeSelectorKeepingParameters class methodsFor: 'pre-conditions' stamp: 'HAW 3/17/2019 05:59:14'!
assertIsNotEmpty: aCollection signalMessageText: errorMessageText

	aCollection isEmpty ifTrue: [ self refactoringError: errorMessageText ].! !

!ChangeSelectorKeepingParameters class methodsFor: 'pre-conditions' stamp: 'HAW 3/17/2019 05:59:19'!
assertIsValidToRenameFrom: anOldSelector to: aNewSelector

	self assertIsNotEmpty: anOldSelector signalMessageText: self oldSelectorCanNotBeEmptyErrorMessage.
	self assertIsNotEmpty: aNewSelector signalMessageText: self newSelectorCanNotBeEmptyErrorMessage.
	self assert: aNewSelector isNotEqualTo: anOldSelector.
	self assert: aNewSelector isOfSameTypeAs: anOldSelector.
	self assert: aNewSelector hasTheSameNumberOfArgumentsAs: anOldSelector.
	! !

!ChangeSelectorKeepingParameters class methodsFor: 'pre-conditions' stamp: 'HAW 3/17/2019 05:59:23'!
assertNoImplementorClassIn: implementorsCollection implements: aNewSelector

	| classesImplementingNewSelector |

	classesImplementingNewSelector := implementorsCollection
		select: [ :anImplementor | anImplementor methodClass includesSelector: aNewSelector ]
		thenCollect: [ :anImplementor | anImplementor methodClass ].

	classesImplementingNewSelector notEmpty ifTrue: [ self signalNewSelector: aNewSelector implementedInAll: classesImplementingNewSelector ]! !

!ChangeSelectorKeepingParameters class methodsFor: 'pre-conditions' stamp: 'HAW 3/17/2019 05:59:50'!
isRenamingBetweenBinary: aPotentiallyBinarySelector andKeywordOfOneParameter: aPotentiallyKeywordSelector

	^aPotentiallyBinarySelector isInfix
		and: [ aPotentiallyKeywordSelector isKeyword
		and: [ aPotentiallyKeywordSelector numArgs = 1 ]]
! !

!ChangeSelectorKeepingParameters class methodsFor: 'pre-conditions' stamp: 'HAW 3/17/2019 05:59:54'!
warnIfImplementionsOf: aNewSelector overridesImplementationInSuperclassesOf: implementorsCollection

	implementorsCollection do: [:anImplementor |
		anImplementor methodClass
			withSuperclassThatIncludesSelector: aNewSelector
			do: [ :aSuperclass | self warnImplementionOf: aNewSelector in: anImplementor methodClass willOverrideImplementationIn: aSuperclass ]
			ifNone: []]! !

!ChangeSelectorKeepingParameters class methodsFor: 'exceptions' stamp: 'HAW 3/17/2019 06:00:00'!
errorMessageForNewSelector: aNewSelector doesNotHaveSameNumberOfArgumentsAs: anOldSelector

	^'New selector #', aNewSelector, ' does not have the same number of arguments as #', anOldSelector ! !

!ChangeSelectorKeepingParameters class methodsFor: 'exceptions' stamp: 'HAW 3/17/2019 06:00:04'!
errorMessageForNewSelector: aNewSelector implementedInAll: classesImplementingNewSelector

	^'Can not rename because #', aNewSelector, ' is implemented in: ', classesImplementingNewSelector asCommaStringAnd ! !

!ChangeSelectorKeepingParameters class methodsFor: 'exceptions' stamp: 'HAW 3/17/2019 06:00:07'!
errorMessageForNewSelector: aNewSelector isNotOfSameTypeAs: anOldSelector

	^'New selector #', aNewSelector, ' is not of same type as #', anOldSelector ! !

!ChangeSelectorKeepingParameters class methodsFor: 'exceptions' stamp: 'HAW 3/17/2019 06:00:11'!
implementorsCanNotBeEmptyErrorMessage

	^'There are no methods to rename'! !

!ChangeSelectorKeepingParameters class methodsFor: 'exceptions' stamp: 'HAW 3/17/2019 06:00:16'!
newSelectorCanNotBeEmptyErrorMessage

	^'New selector can not be empty'! !

!ChangeSelectorKeepingParameters class methodsFor: 'exceptions' stamp: 'HAW 3/17/2019 06:00:19'!
newSelectorEqualToOldSelectorErrorMessage

	^'There is nothing to rename when new selector is equals to old selector'! !

!ChangeSelectorKeepingParameters class methodsFor: 'exceptions' stamp: 'HAW 3/17/2019 06:00:23'!
oldSelectorCanNotBeEmptyErrorMessage

	^'Old selector can not be empty'! !

!ChangeSelectorKeepingParameters class methodsFor: 'exceptions' stamp: 'HAW 3/17/2019 06:00:27'!
signalNewSelector: aNewSelector doesNotHaveSameNumberOfArgumentsAs: anOldSelector

	self refactoringError: (self errorMessageForNewSelector: aNewSelector doesNotHaveSameNumberOfArgumentsAs: anOldSelector).! !

!ChangeSelectorKeepingParameters class methodsFor: 'exceptions' stamp: 'HAW 3/17/2019 06:00:30'!
signalNewSelector: aNewSelector implementedInAll: classesImplementingNewSelector

	self refactoringError: (self errorMessageForNewSelector: aNewSelector implementedInAll: classesImplementingNewSelector).! !

!ChangeSelectorKeepingParameters class methodsFor: 'exceptions' stamp: 'HAW 3/17/2019 06:00:33'!
signalNewSelector: aNewSelector isNotOfSameTypeAs: anOldSelector

	self refactoringError: (self errorMessageForNewSelector: aNewSelector isNotOfSameTypeAs: anOldSelector).! !

!ChangeSelectorKeepingParameters class methodsFor: 'exceptions' stamp: 'HAW 3/17/2019 06:00:37'!
signalNewSelectorEqualToOldSelector

	self refactoringError: self newSelectorEqualToOldSelectorErrorMessage.! !

!ChangeSelectorKeepingParameters class methodsFor: 'warnings' stamp: 'HAW 3/17/2019 06:00:42'!
warnImplementionOf: aNewSelector in: aClass willOverrideImplementationIn: aSuperclass

	self refactoringWarning: (self warningMessageForImplementationOf: aNewSelector in: aClass willOverrideImplementationIn: aSuperclass).! !

!ChangeSelectorKeepingParameters class methodsFor: 'warnings' stamp: 'HAW 3/17/2019 06:00:46'!
warningMessageForImplementationOf: aNewSelector in: aClass willOverrideImplementationIn: aSuperclass

	^'Implemention of #', aNewSelector, ' in ', aClass name, ' will override implementation in ', aSuperclass name! !


!ChangeKeywordsSelectorOrder methodsFor: 'initialization' stamp: 'HAW 3/17/2019 06:14:09'!
initializeChangedOrder: aChangeOrder

	changedOrder := aChangeOrder ! !

!ChangeKeywordsSelectorOrder methodsFor: 'create new implementors - private' stamp: 'HAW 3/17/2019 06:24:07'!
addImplementorSelectorRanges: aKeywordRange at: index to: rangesToNewKeywords

	rangesToNewKeywords add: aKeywordRange -> (newSelectorKeywords at: index).
	rangesToNewKeywords add:
		(currentImplementorMethodNode parameterDefinitionPositionAt: index) ->
		(currentImplementorMethodNode argumentNames at: (changedOrder at: index))! !


!ChangeKeywordsSelectorOrder class methodsFor: 'instance creation' stamp: 'HAW 3/17/2019 06:11:48'!
from: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders

	| changedOrder |

	self assertIsKeywordWithMoreThanOneParameter: anOldSelector.
	changedOrder := self changedOrderFrom: anOldSelector to: aNewSelector.

	^ (super from: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders)
		initializeChangedOrder: changedOrder
! !

!ChangeKeywordsSelectorOrder class methodsFor: 'pre-conditions' stamp: 'HAW 3/17/2019 06:18:21'!
assertIsKeywordWithMoreThanOneParameter: anOldSelector

	(anOldSelector isKeyword and: [ anOldSelector numArgs > 1 ]) ifFalse: [ self signalSelectorToChangeIsNotKeywordWithMoreThanOneParameter ]! !

!ChangeKeywordsSelectorOrder class methodsFor: 'exceptions' stamp: 'HAW 3/16/2019 17:45:39'!
newSelectorDoesNotIncludeOldSelectorKeywordsErrorMessage

	^'New selector keywords do not include old selector keywords'! !

!ChangeKeywordsSelectorOrder class methodsFor: 'exceptions' stamp: 'HAW 3/17/2019 06:18:58'!
selectorToChangeIsNotKeywordWithMoreThanOneParameterErrorMessage

	^'Can only change a selector order for keyword messages with more that one parameter'! !

!ChangeKeywordsSelectorOrder class methodsFor: 'exceptions' stamp: 'HAW 3/16/2019 17:45:39'!
signalNewSelectorDoesNotIncludeOldSelectorKeywords

	self refactoringError: self newSelectorDoesNotIncludeOldSelectorKeywordsErrorMessage! !

!ChangeKeywordsSelectorOrder class methodsFor: 'exceptions' stamp: 'HAW 3/17/2019 06:18:58'!
signalSelectorToChangeIsNotKeywordWithMoreThanOneParameter

	self refactoringError: self selectorToChangeIsNotKeywordWithMoreThanOneParameterErrorMessage! !

!ChangeKeywordsSelectorOrder class methodsFor: 'changed order' stamp: 'HAW 3/17/2019 06:13:43'!
changedOrderFrom: anOldSelector to: aNewSelector

	| changedOrder newSelectorKeywords |

	changedOrder := Dictionary new.
	newSelectorKeywords := aNewSelector keywords.
	anOldSelector keywords withIndexDo: [ :keyword :index |
		changedOrder
			at: (newSelectorKeywords indexOf: keyword ifAbsent: [ self signalNewSelectorDoesNotIncludeOldSelectorKeywords ])
			put: index ].

	^changedOrder ! !


!RenameSelector methodsFor: 'create new implementors - private' stamp: 'HAW 3/17/2019 06:24:07'!
addImplementorSelectorRanges: aKeywordRange at: index to: rangesToNewKeywords

	rangesToNewKeywords add: aKeywordRange -> (newSelectorKeywords at: index) 	! !


!ChangeSelectorKeepingParametersApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/17/2019 06:52:15'!
askNewSelector

	| enteredString |

	enteredString := self request: 'Enter new selector:' initialAnswer: oldSelector.
	newSelector := enteredString withBlanksTrimmed asSymbol.

! !

!ChangeSelectorKeepingParametersApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/17/2019 06:52:23'!
assertCanRenameSelector

	self refactoringClass assertIsValidToRenameFrom: oldSelector to: newSelector.
	! !

!ChangeSelectorKeepingParametersApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 3/17/2019 06:52:38'!
requestRefactoringParameters

	self
		askNewSelector;
		assertCanRenameSelector
		! !

!ChangeSelectorKeepingParametersApplier methodsFor: 'refactoring - creation' stamp: 'HAW 3/17/2019 06:52:28'!
createRefactoring

	^self refactoringClass from: oldSelector to: newSelector implementors: implementors senders: senders.
	! !


!ChangeKeywordsSelectorOrderApplier methodsFor: 'refactoring - creation' stamp: 'HAW 3/16/2019 17:55:35'!
refactoringClass

	^ChangeKeywordsSelectorOrder ! !


!ChangeKeywordsSelectorOrderApplier class methodsFor: 'pre-conditions' stamp: 'HAW 3/17/2019 06:50:17'!
assertCanApplyRefactoringFor: aSelector in: aClass

	ChangeKeywordsSelectorOrder assertIsKeywordWithMoreThanOneParameter: aSelector ! !


!RefactoringMenues class methodsFor: 'browser menues' stamp: 'HAW 3/17/2019 06:46:00'!
messsageRefactoringMenuOptions

	^ `{
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		10.
				#label 			-> 		'rename... (R)'.
				#selector 		-> 		#renameSelector.
				#icon 			-> 		#saveAsIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		15.
				#label 			-> 		'change keyword order...'.
				#selector 		-> 		#changeKeywordOrder.
				#icon 			-> 		#sendReceiveIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		20.
				#label 			-> 		'add parameter...'.
				#selector 		-> 		#addParameter.
				#icon 			-> 		#listAddIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		30.
				#label 			-> 		'remove parameter...'.
				#selector 		-> 		#removeParameter.
				#icon 			-> 		#listRemoveIcon
			} asDictionary.
			{
				#itemGroup 	-> 		10.
				#itemOrder 		-> 		40.
				#label 			-> 		'push up'.
				#selector 		-> 		#pushUpSelector.
				#icon 			-> 		#goTopIcon
			} asDictionary.
	}`.

	! !

!methodRemoval: RenameSelectorApplier #askNewSelector!
RenameSelectorApplier removeSelector: #askNewSelector!
!methodRemoval: RenameSelectorApplier #assertCanRenameSelector!
RenameSelectorApplier removeSelector: #assertCanRenameSelector!
!methodRemoval: RenameSelectorApplier #createRefactoring!
RenameSelectorApplier removeSelector: #createRefactoring!
!methodRemoval: RenameSelectorApplier #requestRefactoringParameters!
RenameSelectorApplier removeSelector: #requestRefactoringParameters!
!methodRemoval: ChangeKeywordsSelectorOrderApplier class #m1!
ChangeKeywordsSelectorOrderApplier class removeSelector: #m1!
!methodRemoval: RenameSelector class #assert:hasTheSameNumberOfArgumentsAs:!
RenameSelector class removeSelector: #assert:hasTheSameNumberOfArgumentsAs:!
!methodRemoval: RenameSelector class #assert:isNotEqualTo:!
RenameSelector class removeSelector: #assert:isNotEqualTo:!
!methodRemoval: RenameSelector class #assert:isOfSameTypeAs:!
RenameSelector class removeSelector: #assert:isOfSameTypeAs:!
!methodRemoval: RenameSelector class #assertIsNotEmpty:signalMessageText:!
RenameSelector class removeSelector: #assertIsNotEmpty:signalMessageText:!
!methodRemoval: RenameSelector class #assertIsValidToRenameFrom:to:!
RenameSelector class removeSelector: #assertIsValidToRenameFrom:to:!
!methodRemoval: RenameSelector class #assertNoImplementorClassIn:implements:!
RenameSelector class removeSelector: #assertNoImplementorClassIn:implements:!
!methodRemoval: RenameSelector class #errorMessageForNewSelector:doesNotHaveSameNumberOfArgumentsAs:!
RenameSelector class removeSelector: #errorMessageForNewSelector:doesNotHaveSameNumberOfArgumentsAs:!
!methodRemoval: RenameSelector class #errorMessageForNewSelector:implementedInAll:!
RenameSelector class removeSelector: #errorMessageForNewSelector:implementedInAll:!
!methodRemoval: RenameSelector class #errorMessageForNewSelector:isNotOfSameTypeAs:!
RenameSelector class removeSelector: #errorMessageForNewSelector:isNotOfSameTypeAs:!
!methodRemoval: RenameSelector class #from:to:implementors:senders:!
RenameSelector class removeSelector: #from:to:implementors:senders:!
!methodRemoval: RenameSelector class #from:to:in:!
RenameSelector class removeSelector: #from:to:in:!
!methodRemoval: RenameSelector class #from:to:inCategoriesAndHierarchyOf:organizedBy:!
RenameSelector class removeSelector: #from:to:inCategoriesAndHierarchyOf:organizedBy:!
!methodRemoval: RenameSelector class #from:to:inCategoryOf:organizedBy:!
RenameSelector class removeSelector: #from:to:inCategoryOf:organizedBy:!
!methodRemoval: RenameSelector class #from:to:inHierarchyOf:!
RenameSelector class removeSelector: #from:to:inHierarchyOf:!
!methodRemoval: RenameSelector class #from:to:inSystem:!
RenameSelector class removeSelector: #from:to:inSystem:!
!methodRemoval: RenameSelector class #implementorsCanNotBeEmptyErrorMessage!
RenameSelector class removeSelector: #implementorsCanNotBeEmptyErrorMessage!
!methodRemoval: RenameSelector class #isRenamigBetweenBinary:andKeywordOfOneParameter:!
RenameSelector class removeSelector: #isRenamigBetweenBinary:andKeywordOfOneParameter:!
!methodRemoval: RenameSelector class #isRenamingBetweenBinary:andKeywordOfOneParameter:!
RenameSelector class removeSelector: #isRenamingBetweenBinary:andKeywordOfOneParameter:!
!methodRemoval: RenameSelector class #newSelectorCanNotBeEmptyErrorMessage!
RenameSelector class removeSelector: #newSelectorCanNotBeEmptyErrorMessage!
!methodRemoval: RenameSelector class #newSelectorEqualToOldSelectorErrorMessage!
RenameSelector class removeSelector: #newSelectorEqualToOldSelectorErrorMessage!
!methodRemoval: RenameSelector class #oldSelectorCanNotBeEmptyErrorMessage!
RenameSelector class removeSelector: #oldSelectorCanNotBeEmptyErrorMessage!
!methodRemoval: RenameSelector class #signalNewSelector:doesNotHaveSameNumberOfArgumentsAs:!
RenameSelector class removeSelector: #signalNewSelector:doesNotHaveSameNumberOfArgumentsAs:!
!methodRemoval: RenameSelector class #signalNewSelector:implementedInAll:!
RenameSelector class removeSelector: #signalNewSelector:implementedInAll:!
!methodRemoval: RenameSelector class #signalNewSelector:isNotOfSameTypeAs:!
RenameSelector class removeSelector: #signalNewSelector:isNotOfSameTypeAs:!
!methodRemoval: RenameSelector class #signalNewSelectorEqualToOldSelector!
RenameSelector class removeSelector: #signalNewSelectorEqualToOldSelector!
!methodRemoval: RenameSelector class #warnIfImplementionsOf:overridesImplementationInSuperclassesOf:!
RenameSelector class removeSelector: #warnIfImplementionsOf:overridesImplementationInSuperclassesOf:!
!methodRemoval: RenameSelector class #warnImplementionOf:in:willOverrideImplementationIn:!
RenameSelector class removeSelector: #warnImplementionOf:in:willOverrideImplementationIn:!
!methodRemoval: RenameSelector class #warningMessageForImplementationOf:in:willOverrideImplementationIn:!
RenameSelector class removeSelector: #warningMessageForImplementationOf:in:willOverrideImplementationIn:!
!methodRemoval: RenameSelector #addMessageSendSelectorKeywordRangesOf:to:!
RenameSelector removeSelector: #addMessageSendSelectorKeywordRangesOf:to:!
!methodRemoval: RenameSelector #addRanges:at:to:!
RenameSelector removeSelector: #addRanges:at:to:!
!methodRemoval: RenameSelector #implementorNewSourceCodeOf:!
RenameSelector removeSelector: #implementorNewSourceCodeOf:!
!methodRemoval: RenameSelector #initializeFrom:to:implementors:senders:!
RenameSelector removeSelector: #initializeFrom:to:implementors:senders:!
!methodRemoval: ChangeKeywordsSelectorOrder class #assertIsNotUnary:!
ChangeKeywordsSelectorOrder class removeSelector: #assertIsNotUnary:!
!methodRemoval: ChangeKeywordsSelectorOrder class #assertIsValidToRenameFrom:to:!
ChangeKeywordsSelectorOrder class removeSelector: #assertIsValidToRenameFrom:to:!
!methodRemoval: ChangeKeywordsSelectorOrder class #canChangeOrderOfKeywordsInKeywordMessagesErrorDescription!
ChangeKeywordsSelectorOrder class removeSelector: #canChangeOrderOfKeywordsInKeywordMessagesErrorDescription!
!methodRemoval: ChangeKeywordsSelectorOrder class #canOnlyChangeSelectorOrderForKeywordMessagesWithMoreThanOneParameterErrorDescription!
ChangeKeywordsSelectorOrder class removeSelector: #canOnlyChangeSelectorOrderForKeywordMessagesWithMoreThanOneParameterErrorDescription!
!methodRemoval: ChangeKeywordsSelectorOrder class #cannotChangeSelectorOrderForAUnaryMessage!
ChangeKeywordsSelectorOrder class removeSelector: #cannotChangeSelectorOrderForAUnaryMessage!
!methodRemoval: ChangeKeywordsSelectorOrder class #cannotChangeSelectorOrderForAnUnaryMessage!
ChangeKeywordsSelectorOrder class removeSelector: #cannotChangeSelectorOrderForAnUnaryMessage!
!methodRemoval: ChangeKeywordsSelectorOrder class #newSelectorDoesNotIncludeOldSelectorKeywords!
ChangeKeywordsSelectorOrder class removeSelector: #newSelectorDoesNotIncludeOldSelectorKeywords!
!methodRemoval: ChangeKeywordsSelectorOrder class #signalCanChangeOrderOfKeywordsInKeywordMessages!
ChangeKeywordsSelectorOrder class removeSelector: #signalCanChangeOrderOfKeywordsInKeywordMessages!
!methodRemoval: ChangeKeywordsSelectorOrder class #signalCannotChangeSelectorOrderForAUnaryMessage!
ChangeKeywordsSelectorOrder class removeSelector: #signalCannotChangeSelectorOrderForAUnaryMessage!
!methodRemoval: ChangeKeywordsSelectorOrder class #signalCannotChangeSelectorOrderForAnUnaryMessage!
ChangeKeywordsSelectorOrder class removeSelector: #signalCannotChangeSelectorOrderForAnUnaryMessage!
!methodRemoval: ChangeKeywordsSelectorOrder #addRanges:at:to:!
ChangeKeywordsSelectorOrder removeSelector: #addRanges:at:to:!
!methodRemoval: ChangeKeywordsSelectorOrder #apply!
ChangeKeywordsSelectorOrder removeSelector: #apply!
!methodRemoval: ChangeKeywordsSelectorOrder #implementorNewSourceCodeOf:!
ChangeKeywordsSelectorOrder removeSelector: #implementorNewSourceCodeOf:!
!methodRemoval: ChangeKeywordsSelectorOrder #initializeChangeOrder:!
ChangeKeywordsSelectorOrder removeSelector: #initializeChangeOrder:!
!methodRemoval: ChangeKeywordsSelectorOrder #initializeRenameRefactoring:!
ChangeKeywordsSelectorOrder removeSelector: #initializeRenameRefactoring:!
!methodRemoval: ChangeSelectorKeepingParameters #addRanges:at:to:!
ChangeSelectorKeepingParameters removeSelector: #addRanges:at:to:!

!ChangeSelectorKeepingParameters reorganize!
('create new implementors - private' addImplementorSelectorRanges:at:to: implementorNewSourceCodeOf:)
('rename senders - private' addMessageSendSelectorKeywordRangesOf:to:)
('initialization' initializeFrom:to:implementors:senders:)
!


!ChangeSelectorKeepingParameters class reorganize!
('instance creation' from:to:implementors:senders: from:to:in: from:to:inCategoriesAndHierarchyOf:organizedBy: from:to:inCategoryOf:organizedBy: from:to:inHierarchyOf: from:to:inSystem:)
('pre-conditions' assert:hasTheSameNumberOfArgumentsAs: assert:isNotEqualTo: assert:isOfSameTypeAs: assertIsNotEmpty:signalMessageText: assertIsValidToRenameFrom:to: assertNoImplementorClassIn:implements: isRenamingBetweenBinary:andKeywordOfOneParameter: warnIfImplementionsOf:overridesImplementationInSuperclassesOf:)
('exceptions' errorMessageForNewSelector:doesNotHaveSameNumberOfArgumentsAs: errorMessageForNewSelector:implementedInAll: errorMessageForNewSelector:isNotOfSameTypeAs: implementorsCanNotBeEmptyErrorMessage newSelectorCanNotBeEmptyErrorMessage newSelectorEqualToOldSelectorErrorMessage oldSelectorCanNotBeEmptyErrorMessage signalNewSelector:doesNotHaveSameNumberOfArgumentsAs: signalNewSelector:implementedInAll: signalNewSelector:isNotOfSameTypeAs: signalNewSelectorEqualToOldSelector)
('warnings' warnImplementionOf:in:willOverrideImplementationIn: warningMessageForImplementationOf:in:willOverrideImplementationIn:)
!


!ChangeKeywordsSelectorOrder reorganize!
('initialization' initializeChangedOrder:)
('create new implementors - private' addImplementorSelectorRanges:at:to:)
!


!ChangeKeywordsSelectorOrder class reorganize!
('instance creation' from:to:implementors:senders:)
('pre-conditions' assertIsKeywordWithMoreThanOneParameter:)
('exceptions' newSelectorDoesNotIncludeOldSelectorKeywordsErrorMessage selectorToChangeIsNotKeywordWithMoreThanOneParameterErrorMessage signalNewSelectorDoesNotIncludeOldSelectorKeywords signalSelectorToChangeIsNotKeywordWithMoreThanOneParameter)
('changed order' changedOrderFrom:to:)
!

!classDefinition: #RenameSelector category: #'Tools-Refactoring'!
ChangeSelectorKeepingParameters subclass: #RenameSelector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!RenameSelector reorganize!
('create new implementors - private' addImplementorSelectorRanges:at:to:)
!


!RenameSelector class reorganize!
('as yet unclassified')
!


!ChangeKeywordsSelectorOrderApplier reorganize!
('refactoring - creation' refactoringClass)
!


!ChangeKeywordsSelectorOrderApplier class reorganize!
('pre-conditions' assertCanApplyRefactoringFor:in:)
!

!classDefinition: #RenameSelectorApplier category: #'Tools-Refactoring'!
ChangeSelectorKeepingParametersApplier subclass: #RenameSelectorApplier
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Refactoring'!

!RenameSelectorApplier reorganize!
('refactoring - creation' refactoringClass)
!
