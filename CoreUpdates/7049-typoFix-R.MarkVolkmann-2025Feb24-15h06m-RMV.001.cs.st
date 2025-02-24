'From Cuis7.3 [latest update: #7048] on 24 February 2025 at 3:06:52 pm'!

!PluggableButtonMorph class methodsFor: 'instance creation' stamp: 'RMV 2/1/2025 16:21:32'!
model: anObject stateGetter: getStateSel action: actionSel label: aString balloonText: aBallonText
	^ (self 
		model: anObject
		stateGetter: getStateSel
		action: actionSel
		label: aString)
		setBalloonText: aBallonText;
		yourself! !


!ChangeSelectorWizardStepWindow methodsFor: 'button creation' stamp: 'RMV 2/1/2025 16:21:32'!
createAddButton
	
	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #add
		label: 'Add'
		balloonText: 'Add a new method to refactor'! !


!ChangeSelectorImplementorsStepWindow methodsFor: 'button creation' stamp: 'RMV 2/1/2025 16:21:32'!
createSeeSendersButton
	
	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #seeSenders
		label: 'See Senders'
		balloonText: 'Shows the senders that will be affected by the refactoring'

! !


!ExtractMethodReplacementsWindow methodsFor: 'button creation' stamp: 'RMV 2/1/2025 16:21:32'!
createExtractAllInClassButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #extractAllInClass
		label: 'In Class'
		balloonText: 'Applies the refactoring to all methods defined in the class'
! !

!ExtractMethodReplacementsWindow methodsFor: 'button creation' stamp: 'RMV 2/1/2025 16:21:32'!
createExtractInMethodOnlyButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #extractInMethodOnly
		label: 'In Method'
		balloonText: 'Applies the refactoring to all repetitions in the source method'
! !

!ExtractMethodReplacementsWindow methodsFor: 'button creation' stamp: 'RMV 2/1/2025 16:21:32'!
createExtractSelectionOnlyButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #extractSelectionOnly
		label: 'Selection Only'
		balloonText: 'Applies the refactoring to the selected code only'
! !

!ExtractMethodReplacementsWindow methodsFor: 'button creation' stamp: 'RMV 2/1/2025 16:21:32'!
createRefactorButton

	^PluggableButtonMorph 
		model: self
		stateGetter: nil
		action: #refactor
		label: 'Refactor'
		balloonText: 'Applies the refactoring to all places without showing the changes'! !


!InlineMethodImplementorsStepWindow methodsFor: 'button creation' stamp: 'RMV 2/1/2025 16:21:32'!
createSelectImplementorButton

	^PluggableButtonMorph
		model: self
		stateGetter: #isMessageSelected
		action: #selectImplementor
		label: 'Select'
		balloonText: 'Applies the refactoring in the selected place'
! !


!InlineMethodImplementorsWithShowUsagesStepWindow methodsFor: 'button creation' stamp: 'RMV 2/1/2025 16:21:32'!
createSelectAndSeeUsagesButton

	^PluggableButtonMorph
		model: self
		stateGetter: nil
		action: #selectAndSeeUsages
		label: 'Select and see usages'
		balloonText: 'Selects the replacement to refactor and shows the usages'


! !


!RefactoringApplier class methodsFor: 'common buttons' stamp: 'RMV 2/1/2025 16:21:32'!
createCancelButtonOn: aModel
	
	^PluggableButtonMorph 
		model: aModel
		stateGetter: nil
		action: #delete
		label: 'Cancel'
		balloonText: 'Do not apply the refactoring'
! !

!RefactoringApplier class methodsFor: 'common buttons' stamp: 'RMV 2/1/2025 16:21:32'!
createJustRefactorButtonOn: aModel

	^PluggableButtonMorph 
		model: aModel
		stateGetter: nil
		action: #justRefactor
		label: 'Refactor'
		balloonText: 'Applies the refactoring without showing the changes'! !

!RefactoringApplier class methodsFor: 'common buttons' stamp: 'RMV 2/1/2025 16:21:32'!
createRefactorButtonOn: aModel

	^PluggableButtonMorph 
		model: aModel
		stateGetter: nil
		action: #refactor
		label: 'Refactor...'
		balloonText: 'Applies the refactoring and shows the changes'! !

!RefactoringApplier class methodsFor: 'common buttons' stamp: 'RMV 2/1/2025 16:21:32'!
createRemoveButtonOn: aModel

	^PluggableButtonMorph
		model: aModel
		stateGetter: nil
		action: #remove
		label: 'Remove'
		balloonText: 'Removes the selected element from the refactoring'
! !

!RefactoringApplier class methodsFor: 'common buttons' stamp: 'RMV 2/1/2025 16:21:32'!
createSeeImplementorsButtonOn: aModel

	^PluggableButtonMorph
		model: aModel
		stateGetter: nil
		action: #seeImplementors
		label: 'See Implementors'
		balloonText: 'Shows the implementors that will be affected by the refactoring'
! !

!methodRemoval: PluggableButtonMorph class #model:stateGetter:action:label:ballonText: stamp: 'RMV 2/24/2025 15:06:22'!
PluggableButtonMorph class removeSelector: #model:stateGetter:action:label:ballonText:!
