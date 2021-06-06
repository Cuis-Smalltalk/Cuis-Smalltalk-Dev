'From Cuis 5.0 [latest update: #4532] on 5 June 2021 at 8:38:51 pm'!

!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 6/5/2021 20:32:51'!
implementorsIsOnlyOneTestMethod

	^ implementors size = 1 and: [ implementors anyOne isTestMethod ]! !


!ChangeSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 6/5/2021 20:32:58'!
startWizard
	
	self implementorsIsOnlyOneTestMethod
		ifTrue: [ self doNotShowChanges; wizardEnded ]
		ifFalse: [ ChangeSelectorImplementorsStepWindow openFrom: self ]! !

!ChangeSelectorApplier methodsFor: 'refactoring - changes' stamp: 'HAW 6/5/2021 20:27:47'!
closeBrowser
	
	wizardStepWindow ifNotNil: [ wizardStepWindow delete ]	! !

!ChangeSelectorApplier methodsFor: 'evaluation' stamp: 'HAW 6/5/2021 20:12:33'!
value

	requestExitBlock := [ ^self ].
		
	self requestRefactoringParametersHandlingRefactoringExceptions.
	
	self 
		ifHasNoSendersAndOneImplementor: [ :anImplementor | 
			self createAndApplyRefactoringWhenNoSendersAndOneImplementor: anImplementor ]
		ifNot: [ self askForImplementosAndSenders ]! !

