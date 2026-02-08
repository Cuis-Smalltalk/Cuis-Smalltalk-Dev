'From Cuis7.7 [latest update: #7834] on 8 February 2026 at 10:33:03 am'!

!HandMorph methodsFor: 'balloon help' stamp: 'jmv 8/Feb/2026 10:32:06'!
scheduleBalloonFor: aMorph
	"Trigger balloon help after the given time out for some morph."
	aMorph wantsBalloon ifTrue: [
		^self addAlarm: #spawnBalloonFor: with: aMorph after: self balloonHelpDelayTime ].
	aMorph owner ifNotNil: [ :itsOwner |
		self scheduleBalloonFor: itsOwner ].! !

!HandMorph methodsFor: 'balloon help' stamp: 'jmv 8/Feb/2026 10:32:21'!
triggerBalloonFor: aMorph
	"Trigger balloon help after the given time out for some morph."
	self removePendingBalloon.
	self scheduleBalloonFor: aMorph! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 8/Feb/2026 10:32:30'!
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

	"Show our balloon, or that of some owner that provides one."
	aMouseMoveEvent hand triggerBalloonFor: self.

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

!methodRemoval: HandMorph #triggerBalloonFor:after: stamp: 'jmv 8/Feb/2026 10:32:35'!
HandMorph removeSelector: #triggerBalloonFor:after:!
