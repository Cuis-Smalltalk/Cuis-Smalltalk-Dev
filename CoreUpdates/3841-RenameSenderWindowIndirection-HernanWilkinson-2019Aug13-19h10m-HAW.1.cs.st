'From Cuis 5.0 [latest update: #3839] on 17 August 2019 at 11:24:04 am'!

!RenameSelectorApplier methodsFor: 'refactoring - parameters request' stamp: 'HAW 8/17/2019 11:17:16'!
openChangeSelectorSendersStepWindow

	ChangeSelectorSendersStepWindow openFrom: self ! !


!ChangeSelectorImplementorsStepWindow methodsFor: 'actions' stamp: 'HAW 8/17/2019 11:23:47'!
seeSenders

	self changeImplementors.
	self delete.	
	
	"Necesary indirection to support actual senders in LiveTyping - Hernan"
	applier openChangeSelectorSendersStepWindow! !


!AddInstanceVariable methodsFor: 'applying' stamp: 'HAW 8/16/2019 11:24:02'!
apply
	
	classToRefactor addInstVarName: newVariable.
	! !

