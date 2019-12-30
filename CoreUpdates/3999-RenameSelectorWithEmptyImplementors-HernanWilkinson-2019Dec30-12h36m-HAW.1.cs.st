'From Cuis 5.0 [latest update: #3997] on 30 December 2019 at 6:23:44 pm'!

!ChangeSelector class methodsFor: 'pre-conditions' stamp: 'HAW 12/30/2019 18:11:22'!
assertIsNotEmpty: aCollection signalMessageText: errorMessageText

	aCollection isEmpty ifTrue: [ self refactoringError: errorMessageText ].! !


!ChangeSelectorWizardStepWindow methodsFor: 'compile methods' stamp: 'HAW 12/30/2019 17:03:12'!
compiledMethodsFrom: methodReferences

	^ methodReferences 
		select: [ :aMethodReference | aMethodReference isValid ]
		thenCollect: [:aMethodReference | aMethodReference compiledMethod ]! !


!ChangeSelectorKeepingParameters class methodsFor: 'instance creation' stamp: 'HAW 12/30/2019 17:55:51'!
from: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders

	self assertIsValidToRenameFrom: anOldSelector to: aNewSelector.
	self assertAllImplementors: aCollectionOfImplementors haveSame: anOldSelector.
	self assertNoImplementorClassIn: aCollectionOfImplementors implements: aNewSelector.
	self assertAllSenders: aCollectionOfSenders send: anOldSelector.

	self warnIfImplementionsOf: aNewSelector overridesImplementationInSuperclassesOf: aCollectionOfImplementors.

	^self new initializeFrom: anOldSelector to: aNewSelector implementors: aCollectionOfImplementors senders: aCollectionOfSenders
	! !


!RefactoringApplier class methodsFor: 'value handling exceptions' stamp: 'HAW 12/30/2019 16:46:47'!
createAndValueHandlingExceptions: creationBlock

	| applier |
	
	applier := creationBlock 
		on: Refactoring refactoringErrorClass 
		do: [ :refactoringError | ^self inform: refactoringError messageText ].

	applier value ! !

!methodRemoval: ChangeSelectorKeepingParameters class #implementorsCanNotBeEmptyErrorMessage stamp: 'HAW 12/30/2019 18:02:07'!
ChangeSelectorKeepingParameters class removeSelector: #implementorsCanNotBeEmptyErrorMessage!
!methodRemoval: ChangeSelectorKeepingParameters class #assertCanChangeSelectorFrom:to:implementors:senders: stamp: 'HAW 12/30/2019 17:56:45'!
ChangeSelectorKeepingParameters class removeSelector: #assertCanChangeSelectorFrom:to:implementors:senders:!
!methodRemoval: ChangeSelectorKeepingParameters class #assertIsNotEmpty:signalMessageText: stamp: 'HAW 12/30/2019 18:11:22'!
ChangeSelectorKeepingParameters class removeSelector: #assertIsNotEmpty:signalMessageText:!
!methodRemoval: ChangeSelectorKeepingParameters class #doesNotMakeSenseToApplyRefactoringWithOutImplementors stamp: 'HAW 12/30/2019 18:03:04'!
ChangeSelectorKeepingParameters class removeSelector: #doesNotMakeSenseToApplyRefactoringWithOutImplementors!
!methodMoveToSomePackage: ChangeSelector class #doesNotMakeSenseToApplyRefactoringWithOutImplementors stamp: 'HAW 12/30/2019 18:04:34'!
ChangeSelector class removeSelectorIfInBaseSystem: #doesNotMakeSenseToApplyRefactoringWithOutImplementors!

!ChangeSelector class reorganize!
('pre-conditions' assertAllImplementors:haveSame: assertAllSenders:send: assertIsNotEmpty:signalMessageText:)
('exceptions' errorMessageForInvalidImplementors: errorMessageForInvalidSenders:of: signalInvalidImplementors: signalInvalidSenders:of:)
('implementors and senders' addImplementorsOf:to:andSendersTo:forClassAndMetaOf: addImplementorsOf:to:andSendersTo:inCategoriesAndHierarchyOf:organizedBy: addImplementorsOf:to:andSendersTo:inCategory:organizedBy: addImplementorsOf:to:andSendersTo:inHierarchyOf: addImplementorsOf:to:andSendersTo:inHierarchyOf:doingPerClass: addImplementorsOf:to:andSendersTo:inSystem: addImplementorsOf:to:andSendersTo:of:)
('*LiveTyping' addActualImplementorsOf:forTypes:to:andActualSendersTo:inSystem: addActualImplementorsOf:in:to:andActualSendersTo:inSystem:)
('*LiveTyping-pre-conditions' assertReceiverTypesOfAll:areIncludedInAllTypesOf:)
('*LiveTyping-exceptions' doesNotMakeSenseToApplyRefactoringWithOutImplementors errorDescriptionForReceiverTypesNotIncludedInImplementorTypes: signalReceiverTypesNotIncludedInImplementorTypes:)
!


!ChangeSelectorKeepingParameters class reorganize!
('instance creation' from:to:implementors:senders: from:to:in: from:to:inCategoriesAndHierarchyOf:organizedBy: from:to:inCategoryOf:organizedBy: from:to:inHierarchyOf: from:to:inSystem:)
('pre-conditions' assert:hasTheSameNumberOfArgumentsAs: assert:isNotEqualTo: assert:isOfSameTypeAs: assertIsValidToRenameFrom:to: assertNoImplementorClassIn:implements: isRenamingBetweenBinary:andKeywordOfOneParameter: warnIfImplementionsOf:overridesImplementationInSuperclassesOf:)
('exceptions' errorMessageForNewSelector:doesNotHaveSameNumberOfArgumentsAs: errorMessageForNewSelector:implementedInAll: errorMessageForNewSelector:isNotOfSameTypeAs: newSelectorCanNotBeEmptyErrorMessage newSelectorEqualToOldSelectorErrorMessage oldSelectorCanNotBeEmptyErrorMessage signalNewSelector:doesNotHaveSameNumberOfArgumentsAs: signalNewSelector:implementedInAll: signalNewSelector:isNotOfSameTypeAs: signalNewSelectorEqualToOldSelector)
('warnings' warnImplementionOf:in:willOverrideImplementationIn: warningMessageForImplementationOf:in:willOverrideImplementationIn:)
!

