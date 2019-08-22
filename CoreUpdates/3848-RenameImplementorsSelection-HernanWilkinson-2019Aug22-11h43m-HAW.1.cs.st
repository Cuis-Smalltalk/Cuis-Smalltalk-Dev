'From Cuis 5.0 [latest update: #3846] on 22 August 2019 at 3:48:21 pm'!

!ChangeSelectorWizardStepWindow class methodsFor: 'instance creation' stamp: 'HAW 8/22/2019 15:41:07'!
openFrom: aChangeSelectorApplier methods: methods label: aLabel selecting: somethingToSelect 

	| window |
	
	window := self openMessageList: (self methodReferencesOf: methods) label: aLabel autoSelect: somethingToSelect.
	window initializeFrom: aChangeSelectorApplier.
	
	^window 

! !


!ChangeSelectorImplementorsStepWindow class methodsFor: 'instance creation' stamp: 'HAW 8/22/2019 15:41:48'!
openFrom: aChangeSelectorApplier

	^self 
		openFrom: aChangeSelectorApplier 
		methods: aChangeSelectorApplier implementors 
		label: 'Implementors of #', aChangeSelectorApplier oldSelector, ' to Refactor' 
		selecting: nil  
! !


!ChangeSelectorSendersStepWindow class methodsFor: 'instance creation' stamp: 'HAW 8/22/2019 15:41:39'!
openFrom: aChangeSelectorApplier

	^self 
		openFrom: aChangeSelectorApplier 
		methods: aChangeSelectorApplier senders 
		label: 'Senders of #', aChangeSelectorApplier oldSelector, ' to Refactor' 
		selecting: aChangeSelectorApplier oldSelector ! !

!methodRemoval: ChangeSelectorWizardStepWindow class #openFrom:methods:label:!
ChangeSelectorWizardStepWindow class removeSelector: #openFrom:methods:label:!

!ChangeSelectorImplementorsStepWindow class reorganize!
('instance creation' openFrom:)
!


!ChangeSelectorSendersStepWindow class reorganize!
('instance creation' openFrom:)
('*LiveTyping' openForActualScopeFrom:)
!

