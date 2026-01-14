'From Cuis7.7 [latest update: #7797] on 14 January 2026 at 3:09:12 pm'!

!Morph methodsFor: 'events-processing' stamp: 'len 14/Jan/2026 03:07:11'!
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
		aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.
		^self ].

	aMouseButtonEvent hand removeHaloFromClick: aMouseButtonEvent on: self.

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

!Morph methodsFor: 'events-processing' stamp: 'len 13/Jan/2026 19:11:52'!
processMouseLeave: aMouseLeaveEvent localPosition: localEventPosition
	"A MouseLeaveEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	aMouseLeaveEvent hand removePendingBalloon; deleteBalloon.
	aMouseLeaveEvent hand hasSubmorphs ifTrue:
		[(self handlesDragOver: aMouseLeaveEvent) ifTrue:
			[aMouseLeaveEvent wasHandled: true.
			self dragLeave: aMouseLeaveEvent].
		^ self].
	(self handlesMouseOver: aMouseLeaveEvent) ifTrue: [
		aMouseLeaveEvent wasHandled: true.
		self mouseLeave: aMouseLeaveEvent ].! !


!HandMorph methodsFor: 'geometry' stamp: 'hlsf 24/Jun/2022 09:31:43'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	| prevTranslation |
	prevTranslation := location translation.
	location := location withTranslation: aPoint.
	"Ask if translation effectively changed, after possible conversion to 32 bit Float in AffineTransformation. "
	location translation = prevTranslation ifFalse: [
		self isDrawnBySoftware
			ifTrue: [
				(Preferences at: #cacheDisplayContentWhenMovingMorphs)
					ifTrue: [
						"We are caching whatever is in the Display below us. Thefore, there's no need
						to do an invalidation that would trigger the redraw of everything below us."
						self needsRedraw: true ]
					ifFalse:  [
						"No caching of stuff below us. Just invalidate and redraw."
						self redrawNeeded ]]
			ifFalse: [
				lastPosition := nil.		"Not nil if carrying morphs at that moment"
				prevFullBounds := nil "Any saved patch is no longer relevant"]].! !

