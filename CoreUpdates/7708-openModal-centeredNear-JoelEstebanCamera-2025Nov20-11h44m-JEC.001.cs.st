'From Cuis7.5 [latest update: #7706] on 20 November 2025 at 11:55:57 am'!

!ModalDialog class methodsFor: 'instance creation' stamp: 'JEC 11/20/2025 11:48:43'!
openModal: aModalDialog centeredNear: aWorldPosition
	"Open a Modal Dialog. Answer user's response (if OK) or nil (if Cancel)
	This is an abstract class. Better play with SampleModalDialog. Inspect the result of:
		SampleModalDialog openModal
	Note: a ModalDialog can also be opened in non-modal mode calling #open.
		No answer will be returned, so #ok and #cancel need to do any appropriate actions.
	"
	| world answer prevKbdFocus |
	world := self runningWorld ifNil: [UISupervisor ui].
	prevKbdFocus := world activeHand keyboardFocus.
	world addMorph: aModalDialog centeredNear: aWorldPosition.
	answer := aModalDialog waitForUserResponse.
	prevKbdFocus ifNotNil: [ world activeHand newKeyboardFocus: prevKbdFocus ].
	^answer! !


!ModalDialog class methodsFor: 'instance creation' stamp: 'JEC 11/20/2025 11:47:42'!
openModal
	
	| world |
	world := self runningWorld ifNil: [UISupervisor ui].
	^ self 
		openModal: self new beColumn buildWidgets
		centeredNear: world activeHand morphPosition! !

