'From Cuis7.9 [latest update: #7989] on 9 June 2026 at 4:18:54 pm'!

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/Jun/2026 16:18:15'!
processMouseDown: aMouseButtonEvent localPosition: localEventPosition
	"A #mouseDown MouseButtonEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true.
	Notes:
		- MouseButtonEvent are sent to the current mouse focus, and if none,
			to the topmost morph under the hand that handles it."
	
	aMouseButtonEvent hand removePendingBalloon; deleteBalloon.
	aMouseButtonEvent wasHandled: true.
	self activateWindow.

	aMouseButtonEvent mouseButton3Pressed ifTrue: [
		self mouseButton3Down: aMouseButtonEvent localPosition: localEventPosition.
		^self ].
	
	aMouseButtonEvent hand newMouseFocus: self.		"Mouse down sets mouse focus"

	aMouseButtonEvent mouseButton2Pressed ifTrue: [
		self mouseButton2Down: aMouseButtonEvent localPosition: localEventPosition.
		^self ].

	(Preferences at: #focusFollowsMouse)
		ifTrue: [" MouseEnterToFocus. Clicks have no relation to keyboard focus. Just deliver the event."
			self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ]
		ifFalse: [ "ClickToFocus. Still two different cases. Some morphs 'ignore' focus setting MouseDown events."
			self clickToFocusConsumesEvent
				ifTrue: [
					"#clickToFocusConsumesEvent means: Don't deliver a MouseDown if the
					soon-to-be MouseUp will set keyboard focus on us (see #processMouseUp:localPosition:).
					So, deliver it if we already have keyboard focus."
					self hasKeyboardFocus ifTrue: [
						self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ]]
				ifFalse: [
					"(#clickToFocusConsumesEvent not) means: A keyboard focus setting MouseDown is also a
					regular MouseDown (after setting keyboard focus)"
					self hasKeyboardFocus ifFalse: [
						aMouseButtonEvent hand newKeyboardFocus: self ].
					self mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition ]].

	(self handlesMouseStillDown: aMouseButtonEvent) ifTrue: [
		self startStepping: #processMouseStillDown
			in: self mouseStillDownThreshold
			stepTime: self mouseStillDownStepRate ].! !

!methodRemoval: HandMorph #removeHaloFromClick:on: stamp: 'jmv 9/Jun/2026 16:18:19'!
HandMorph removeSelector: #removeHaloFromClick:on:!
!methodRemoval: HaloMorph #staysUpWhenMouseIsDownIn: stamp: 'jmv 9/Jun/2026 16:18:20'!
HaloMorph removeSelector: #staysUpWhenMouseIsDownIn:!
