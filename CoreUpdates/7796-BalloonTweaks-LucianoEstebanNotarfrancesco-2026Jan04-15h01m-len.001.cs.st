'From Cuis7.7 [latest update: #7783] on 12 January 2026 at 6:53:13 pm'!

!Morph methodsFor: 'events-processing' stamp: 'len 12/Jan/2026 16:07:12'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"A MouseMoveEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true.
	
	Notes:
		- MouseMoveEvents are not sent if the hand is carrying morphs around.
		- MouseMoveEvents are sent to the current mouse focus, and if none, to the topmost
			morph under the hand that handles it."

	aMouseMoveEvent hand removePendingBalloon.
	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	aMouseMoveEvent wasHandled: true.

	self wantsBalloon ifTrue: [aMouseMoveEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].

	(self clickToFocusConsumesEvent and: [ (Preferences at: #focusFollowsMouse) not ])
		ifTrue: [ "ClickToFocus, and focus setting MouseDown was ignored. (see #processMouseDown:localPosition:)"
			self hasKeyboardFocus ifTrue: [
				"If we already have keyboardFocus, the MouseDown was delivered. Deliver also the MouseMove."
				self mouseMove: aMouseMoveEvent localPosition: localEventPosition  ]]
		ifFalse: [
			"See #processMouseDown:localPosition: Mouse down was a regular event. Deliver also the MouseMove."
			self mouseMove: aMouseMoveEvent localPosition: localEventPosition ].

	(aMouseMoveEvent anyButtonPressed and: [ self hasMouseFocus ]) ifFalse: [ ^self ].
	(self handlesMouseStillDown: aMouseMoveEvent) ifTrue: [
		"Step at the new location"
		self startStepping: #processMouseStillDown stepTime: 1 ].! !

!Morph methodsFor: 'halos and balloon help' stamp: 'len 12/Jan/2026 16:21:02'!
showBalloon
	"Pop up a balloon containing the receiver #balloonText."

	self world ifNotNil: [ :w | | balloon |
		balloon := HoverHelpMorph target: self getter: #balloonText.

		"Do it in a while. In some cases, processing the event that
		might have triggered us might also remove any Help Balloon"
		UISupervisor whenUIinSafeState: [ balloon popUpForHand: w activeHand ]].! !


!HandMorph methodsFor: 'balloon help' stamp: 'len 12/Jan/2026 18:50:57'!
removePendingBalloon
	"Get rid of pending balloon help."
	self removeAlarm: #spawnBalloonFor:! !

!HandMorph methodsFor: 'balloon help' stamp: 'len 12/Jan/2026 18:51:26'!
triggerBalloonFor: aMorph after: timeOut
	"Trigger balloon help after the given time out for some morph."
	self removePendingBalloon.
	self addAlarm: #spawnBalloonFor: with: aMorph after: timeOut.! !

!HandMorph methodsFor: 'geometry' stamp: 'len 12/Jan/2026 18:52:33'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	| prevTranslation |
	prevTranslation := location translation.
	location := location withTranslation: aPoint.
	"Ask if translation effectively changed, after possible conversion to 32 bit Float in AffineTransformation. "
	location translation = prevTranslation ifFalse: [
		self deleteBalloon. "if the hand really moved, delete any existing balloon"
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

