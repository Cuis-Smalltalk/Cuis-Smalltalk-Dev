'From Cuis7.1 [latest update: #6803] on 31 October 2024 at 9:43:21 am'!

!PluggableButtonMorph class methodsFor: 'instance creation' stamp: 'jmv 10/30/2024 12:51:20'!
model: anObject action: actionSel actionArgument: actionArg

	^ self new
		model: anObject
		stateGetter: nil
		stateArgument: nil
		action: actionSel
		actionArgument: actionArg
		label: nil
! !

!PluggableButtonMorph class methodsFor: 'instance creation' stamp: 'jmv 10/30/2024 12:52:36'!
model: anObject action: actionSel actionArgument: actionArg label: aString

	^ self new
		model: anObject
		stateGetter: nil
		stateArgument: nil
		action: actionSel
		actionArgument: actionArg
		label: aString
! !

