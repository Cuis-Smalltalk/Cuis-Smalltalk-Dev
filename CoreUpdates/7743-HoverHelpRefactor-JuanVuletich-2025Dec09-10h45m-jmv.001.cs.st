'From Cuis7.5 [latest update: #7742] on 9 December 2025 at 11:02:19 am'!
!classDefinition: #HoverHelpMorph category: #'Morphic-Composite Widgets'!
ColoredBoxMorph subclass: #HoverHelpMorph
	instanceVariableNames: 'contents textComposition getter target '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 9/Dec/2025 10:36:11'!
showBalloon
	"Pop up a balloon containing the receiver #balloonText,
	first removing any existing BalloonMorphs in the world."

	self world ifNotNil: [ :w | | balloon |

		w activeHand deleteBalloon.
		balloon := HoverHelpMorph target: self getter: #balloonText.

		"Do it in a while. In some cases, processing the event that
		might have triggered us might also remove any Help Balloon"
		UISupervisor whenUIinSafeState: [
			balloon popUpForHand: w activeHand ]].! !


!HoverHelpMorph methodsFor: 'accessing' stamp: 'jmv 9/Dec/2025 10:30:57'!
target
	^target! !

!HoverHelpMorph methodsFor: 'accessing' stamp: 'jmv 9/Dec/2025 09:59:48'!
target: aTargetMorph getter: aSelector
	target := aTargetMorph.
	getter := aSelector.! !


!HoverHelpMorph class methodsFor: 'instance creation' stamp: 'jmv 9/Dec/2025 10:00:16'!
target: aTargetMorph getter: aSelector
	"Make up and return a HoverHelp for morph.
	"

	^self new target: aTargetMorph getter: aSelector! !


!WorldMorph methodsFor: 'halos and balloon help' stamp: 'jmv 9/Dec/2025 10:21:41'!
deleteBalloon
	"Delete any existing balloon help."

	activeHand deleteBalloon! !


!HandMorph methodsFor: 'balloon help' stamp: 'jmv 9/Dec/2025 10:20:51'!
deleteBalloon
	"Delete any existing balloon help."
	
	self balloonHelp: nil! !

!HandMorph methodsFor: 'balloon help' stamp: 'jmv 9/Dec/2025 10:24:44'!
removePendingBalloon
	"Get rid of pending balloon help."

	self removeAlarm: #spawnBalloonFor:.
	self deleteBalloon.! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 9/Dec/2025 10:24:58'!
processMouseDown: aMouseButtonEvent localPosition: localEventPosition
	"A #mouseDown MouseButtonEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true.
	Notes:
		- MouseButtonEvent are sent to the current mouse focus, and if none,
			to the topmost morph under the hand that handles it."
	
	aMouseButtonEvent hand removePendingBalloon.
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

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/Dec/2025 10:57:12'!
processMouseEnter: aMouseEnterEvent localPosition: localEventPosition
	"A MouseEnterEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."
	aMouseEnterEvent hand hasSubmorphs ifTrue:
		[(self handlesDragOver: aMouseEnterEvent) ifTrue:
			[aMouseEnterEvent wasHandled: true.
			self dragEnter: aMouseEnterEvent].
		^ self].
	"Moved to #processMouseMove:localPosition:"
	"self wantsBalloon ifTrue: [
		aMouseEnterEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	"
	(self handlesMouseOver: aMouseEnterEvent) ifTrue: [
		aMouseEnterEvent wasHandled: true.
		self mouseEnter: aMouseEnterEvent ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/Dec/2025 10:27:56'!
processMouseLeave: aMouseLeaveEvent localPosition: localEventPosition
	"A MouseLeaveEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true."

	aMouseLeaveEvent hand removePendingBalloon.
	aMouseLeaveEvent hand hasSubmorphs ifTrue:
		[(self handlesDragOver: aMouseLeaveEvent) ifTrue:
			[aMouseLeaveEvent wasHandled: true.
			self dragLeave: aMouseLeaveEvent].
		^ self].
	(self handlesMouseOver: aMouseLeaveEvent) ifTrue: [
		aMouseLeaveEvent wasHandled: true.
		self mouseLeave: aMouseLeaveEvent ].! !

!Morph methodsFor: 'events-processing' stamp: 'jmv 9/Dec/2025 10:25:16'!
processMouseMove: aMouseMoveEvent localPosition: localEventPosition
	"A MouseMoveEvent was delivered to us.
	If appropriate, handle it and mark it as #wasHandled: true.
	
	Notes:
		- MouseMoveEvents are not sent if the hand is carrying morphs around.
		- MouseMoveEvents are sent to the current mouse focus, and if none, to the topmost
			morph under the hand that handles it."

	aMouseMoveEvent hand hasSubmorphs ifTrue: [ ^self ].
	aMouseMoveEvent wasHandled: true.

	self wantsBalloon ifTrue: [
		aMouseMoveEvent hand removePendingBalloon.
		aMouseMoveEvent hand triggerBalloonFor: self after: self balloonHelpDelayTime].
	
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

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 9/Dec/2025 10:38:02'!
balloonHelpDelayTime
	"Return the number of milliseconds before a balloon help should be put up on the receiver. The balloon help will only be put up if the receiver responds to #wantsBalloon by returning true."
	^300! !

!Morph methodsFor: 'halos and balloon help' stamp: 'jmv 9/Dec/2025 10:02:49'!
mouseDownOnHelpHandle: anEvent
	"The mouse went down in the show-balloon handle"
	
	| str |
	anEvent shiftPressed ifTrue: [^ self editBalloonHelpText].
	str := self balloonText.
	str ifNil: [str := self noHelpString].
	self showBalloon
! !


!HoverHelpMorph methodsFor: 'initialization' stamp: 'jmv 9/Dec/2025 09:53:16'!
popUpForHand: aHand
	"Pop up the receiver as balloon help for the given hand"

	| newPos x y |
	(contents isNil or: [ contents isEmpty ]) ifTrue: [
		self
			contents: (target ifNotNil: [getter ifNotNil: [target perform: getter] ifNil: [target balloonText]])
			wrappedTo: nil ].
	x := aHand morphPosition x - 20.
	y := aHand morphPosition y + 20.
	x + extent x > aHand world morphWidth ifTrue: [
		x := aHand world morphWidth - extent x ].
	y + extent y > aHand world morphHeight ifTrue: [
		y := aHand morphPosition y - extent y - 12 ].
	newPos := x@y.
	aHand world addMorphFront: self position: newPos.
	aHand balloonHelp: self! !


!WorldMorph methodsFor: 'halos and balloon help' stamp: 'jmv 4/Sep/2024 14:36:26'!
deleteBalloonTarget: aMorph
	"Delete the balloon help targeting the given morph"
	activeHand deleteBalloonTarget: aMorph.! !


!HandMorph methodsFor: 'balloon help' stamp: 'jmv 9/Dec/2025 10:31:31'!
deleteBalloonTarget: aMorph
	"Delete any existing balloon help."
	
	| h |
	h := self balloonHelp ifNil: [^ self].
	h target == aMorph ifTrue: [self deleteBalloon]! !

!HandMorph methodsFor: 'balloon help' stamp: 'jmv 9/Dec/2025 10:02:39'!
spawnBalloonFor: aMorph
	aMorph showBalloon! !

!methodRemoval: HandMorph #removePendingBalloonFor: stamp: 'jmv 9/Dec/2025 10:53:17'!
HandMorph removeSelector: #removePendingBalloonFor:!
!classDefinition: #HoverHelpMorph category: #'Morphic-Composite Widgets'!
ColoredBoxMorph subclass: #HoverHelpMorph
	instanceVariableNames: 'target getter contents textComposition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Composite Widgets'!
