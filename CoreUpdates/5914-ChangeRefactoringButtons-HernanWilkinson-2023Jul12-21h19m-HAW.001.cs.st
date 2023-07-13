'From Cuis 6.0 [latest update: #5908] on 12 July 2023 at 10:46:24 pm'!

!PluggableButtonMorph class methodsFor: 'instance creation' stamp: 'HAW 7/12/2023 21:59:32'!
model: anObject stateGetter: getStateSel action: actionSel label: aString ballonText: aBallonText
	^ (self 
		model: anObject
		stateGetter: getStateSel
		action: actionSel
		label: aString)
		setBalloonText: aBallonText;
		yourself! !


!RefactoringApplier class methodsFor: 'common buttons' stamp: 'HAW 7/12/2023 22:38:16'!
createCancelButtonOn: aModel
	
	^PluggableButtonMorph 
		model: aModel
		stateGetter: nil
		action: #delete
		label: 'Cancel'
		ballonText: 'Do not apply the refactoring'
! !

!RefactoringApplier class methodsFor: 'common buttons' stamp: 'HAW 7/12/2023 22:38:39'!
createJustRefactorButtonOn: aModel

	^PluggableButtonMorph 
		model: aModel
		stateGetter: nil
		action: #justRefactor
		label: 'Refactor'
		ballonText: 'Applies the refactoring without showing the changes'! !

!RefactoringApplier class methodsFor: 'common buttons' stamp: 'HAW 7/12/2023 22:38:57'!
createRefactorButtonOn: aModel

	^PluggableButtonMorph 
		model: aModel
		stateGetter: nil
		action: #refactor
		label: 'Refactor...'
		ballonText: 'Applies the refactoring and shows the changes'! !

!RefactoringApplier class methodsFor: 'common buttons' stamp: 'HAW 7/12/2023 22:39:10'!
createRemoveButtonOn: aModel

	^PluggableButtonMorph
		model: aModel
		stateGetter: nil
		action: #remove
		label: 'Remove'
		ballonText: 'Removes the selected element from the refactoring'
! !

!RefactoringApplier class methodsFor: 'common buttons' stamp: 'HAW 7/12/2023 22:39:19'!
createSeeImplementorsButtonOn: aModel

	^PluggableButtonMorph
		model: aModel
		stateGetter: nil
		action: #seeImplementors
		label: 'See Implementors'
		ballonText: 'Shows the implementors that will be affected by the refactoring'
! !


!ChangeSelectorWizardStepWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:02:02'!
createAddButton
	
	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #add
		label: 'Add'
		ballonText: 'Add a new method to refactor'! !

!ChangeSelectorWizardStepWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:38:16'!
createCancelButton
	
	^RefactoringApplier createCancelButtonOn: self! !

!ChangeSelectorWizardStepWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:38:39'!
createJustRefactorButton

	^RefactoringApplier createJustRefactorButtonOn: self ! !

!ChangeSelectorWizardStepWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:38:57'!
createRefactorButton

	^RefactoringApplier createRefactorButtonOn: self! !

!ChangeSelectorWizardStepWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:39:10'!
createRemoveButton
	
	^RefactoringApplier createRemoveButtonOn: self! !


!ChangeSelectorImplementorsStepWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:02:27'!
createSeeSendersButton
	
	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #seeSenders
		label: 'See Senders'
		ballonText: 'Shows the senders that will be affected by the refactoring'

! !


!ChangeSelectorSendersStepWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:39:19'!
createSeeImplementorsButton

	^RefactoringApplier createSeeImplementorsButtonOn: self! !


!ExtractMethodReplacementsWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:38:16'!
createCancelButton
	
	^RefactoringApplier createCancelButtonOn: self! !

!ExtractMethodReplacementsWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:04:57'!
createExtractAllInClassButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #extractAllInClass
		label: 'In Class'
		ballonText: 'Applies the refactoring to all methods defined in the class'
! !

!ExtractMethodReplacementsWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:06:31'!
createExtractInMethodOnlyButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #extractInMethodOnly
		label: 'In Method'
		ballonText: 'Applies the refactoring to all repetitions in the source method'
! !

!ExtractMethodReplacementsWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:06:57'!
createExtractSelectionOnlyButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #extractSelectionOnly
		label: 'Selection Only'
		ballonText: 'Applies the refactoring to the selected code only'
! !

!ExtractMethodReplacementsWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:07:50'!
createRefactorButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #refactor
		label: 'Refactor'
		ballonText: 'Applies the refactoring to all places without showing the changes'! !

!ExtractMethodReplacementsWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:39:10'!
createRemoveButton
	
	^RefactoringApplier createRemoveButtonOn: self! !


!InlineMethodWizardStepWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:38:16'!
createCancelButton

	^RefactoringApplier createCancelButtonOn: self
! !

!InlineMethodWizardStepWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:38:39'!
createJustRefactorButton

	^RefactoringApplier createJustRefactorButtonOn: self ! !

!InlineMethodWizardStepWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:38:57'!
createRefactorButton

	^RefactoringApplier createRefactorButtonOn: self! !

!InlineMethodWizardStepWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:39:10'!
createRemoveButton

	^RefactoringApplier createRemoveButtonOn: self! !


!InlineMethodImplementorsStepWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:12:45'!
createSelectImplementorButton

	^PluggableButtonMorph
		model: self
		stateGetter: #isMessageSelected
		action: #selectImplementor
		label: 'Select'
		ballonText: 'Applies the refactoring in the selected place'
! !


!InlineMethodImplementorsWithShowUsagesStepWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:13:21'!
createSelectAndSeeUsagesButton

	^PluggableButtonMorph
		model: self
		stateGetter: nil
		action: #selectAndSeeUsages
		label: 'Select and see usages'
		ballonText: 'Selects the replacement to refactor and shows the usages'


! !


!InlineMethodUsagesWithShowImplementorsStepWindow methodsFor: 'button creation' stamp: 'HAW 7/12/2023 22:39:19'!
createSeeImplementorsButton

	^RefactoringApplier createSeeImplementorsButtonOn: self! !

!methodRemoval: RefactoringApplier class #createRemoveButton stamp: 'HAW 7/12/2023 22:37:07'!
RefactoringApplier class removeSelector: #createRemoveButton!
!methodRemoval: RefactoringApplier class #createRefactorButton: stamp: 'HAW 7/12/2023 22:38:57'!
RefactoringApplier class removeSelector: #createRefactorButton:!
!methodRemoval: RefactoringApplier class #createCancelButton: stamp: 'HAW 7/12/2023 22:38:16'!
RefactoringApplier class removeSelector: #createCancelButton:!
!methodRemoval: RefactoringApplier class #createSeeImplementorsButton: stamp: 'HAW 7/12/2023 22:39:19'!
RefactoringApplier class removeSelector: #createSeeImplementorsButton:!
!methodRemoval: RefactoringApplier class #createRemoveButton: stamp: 'HAW 7/12/2023 22:39:10'!
RefactoringApplier class removeSelector: #createRemoveButton:!
!methodRemoval: RefactoringApplier class #createSeeImplementorsButton stamp: 'HAW 7/12/2023 22:37:42'!
RefactoringApplier class removeSelector: #createSeeImplementorsButton!
!methodRemoval: RefactoringApplier class #createRefactorButton stamp: 'HAW 7/12/2023 22:36:00'!
RefactoringApplier class removeSelector: #createRefactorButton!
!methodRemoval: RefactoringApplier class #createJustRefactorButton: stamp: 'HAW 7/12/2023 22:38:39'!
RefactoringApplier class removeSelector: #createJustRefactorButton:!
!methodRemoval: RefactoringApplier class #createJustRefactorButton stamp: 'HAW 7/12/2023 22:36:34'!
RefactoringApplier class removeSelector: #createJustRefactorButton!

!ChangeSelectorWizardStepWindow reorganize!
('actions' add addToList: do:withEnteredClassLabeled: inform:doesNotImplement: justRefactor refactor remove withClassNamed:do:)
('GUI building' addButton:to:color: addButtonsTo:color: buildLowerPanes buttonsRow)
('compile methods' compiledMethodsFrom:)
('button creation' createAddButton createCancelButton createJustRefactorButton createRefactorButton createRemoveButton)
('initialization' initializeFrom:)
('accessing' oldSelector)
!


!ChangeSelectorSendersStepWindow reorganize!
('actions' add askAndAddSenderOf: changeSenders refactor seeImplementors)
('GUI building' addButtonsTo:color:)
('button creation' createSeeImplementorsButton)
!


!ExtractMethodReplacementsWindow reorganize!
('actions' closeAfter: extractAllInClass extractInMethodOnly extractSelectionOnly refactor remove)
('GUI building' addButton:to:color: addButtonsTo:color: buildLowerPanes buttonsRow)
('button creation' createCancelButton createExtractAllInClassButton createExtractInMethodOnlyButton createExtractSelectionOnlyButton createRefactorButton createRemoveButton)
('initialization' initializeFrom:with:)
('as yet unclassified' createCodePaneMorph)
!


!InlineMethodWizardStepWindow reorganize!
('actions' do:withEnteredClassLabeled: inform:doesNotImplement: justRefactor refactor withClassNamed:do:)
('GUI building' addButton:to:color: addButtonsTo:color: buildLowerPanes buttonsRow)
('compile methods' compiledMethodsFrom:)
('button creation' createCancelButton createJustRefactorButton createRefactorButton createRemoveButton)
('initialization' initializeFrom:)
('testing' isMessageSelected)
('accessing' selectorToInline)
!


!InlineMethodUsagesWithShowImplementorsStepWindow reorganize!
('button creation' addButtonsTo:color: createSeeImplementorsButton)
('actions' seeImplementors)
!

