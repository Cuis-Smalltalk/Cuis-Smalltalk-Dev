'From Cuis 5.0 [latest update: #5024] on 30 December 2021 at 1:16:26 pm'!
!classDefinition: #HandMorph category: #'Morphic-Kernel'!
BoxedMorph subclass: #HandMorph
	instanceVariableNames: 'mouseFocus keyboardFocus mouseClickState mouseOverHandler lastMouseEvent hasChanged savedPatch lastEventBuffer lastKeyDownValue lastMouseEventTime prevFullBounds grabMorphData lastPosition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!HandMorph commentStamp: '<historical>' prior: 0!
The cursor may be thought of as the HandMorph.  The hand's submorphs hold anything being carried by dragging.  

There is some minimal support for multiple hands in the same world.

grabMorphData is a dictionary of Morph->{prevOwner,prevPos} for morphs being carried.!

!classDefinition: #PasteUpMorph category: #'Morphic-Kernel'!
BoxedMorph subclass: #PasteUpMorph
	instanceVariableNames: 'backgroundImage backgroundImageData'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!PasteUpMorph commentStamp: '<historical>' prior: 0!
My instances are free areas where you can play with Morphs. Most important, the World is an instance of my subclass WorldMorph.!

!classDefinition: #WorldMorph category: #'Morphic-Kernel'!
PasteUpMorph subclass: #WorldMorph
	instanceVariableNames: 'activeHand hands canvas damageRecorder stepList lastCycleTime alarms lastAlarmTime deferredUIMessages drawingFailingMorphs waitDelay pause lastCycleHadAnyEvent taskbar'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Kernel'!

!WorldMorph commentStamp: '<historical>' prior: 0!
A WorldMorph is a kind of PasteUpMorph that can act as the root morph of a Morphic system. Usually uses the whole Display.

activeHand 					-- a HandMorph
hands 					-- Array of HandMorphs (typically only one) representing the Cursor and its event queue.
canvas 					-- BitBlitCanvas on the DisplayScreen. (Display getCanvas)
damageRecorder 					-- the DamageRecorder for the Display
stepList 					-- a Heap of StepMessage.  Each morph which wants stepping has a StepMessage here.
								See comment in #Morph>>stepAt:
lastStepTime 					-- 'now' as last sampled at step. (Time localMillisecondClock)
lastStepMessage 					-- nil or a selector
lastCycleTime 					-- 'now' as last sampled (Time localMillisecondClock)
alarms 					-- a Heap of MorphicAlarm.  See comment in #Morph>>addAlarm:after:
								An _alarm_ is an action to be done once, vs a _step_ which is cycled.
lastAlarm 					-- 'now' as sampled at last alarm (Time localMillisecondClock).
drawingFailureMorphs 					-- a WeakIdentitySet of Morphs with drawing failures
waitDelay 					-- a Delay
pause 					-- A 'phase-locked loop' style value to help regularise the step/alarm/event service rate.
lastCycleHadAnyEvent 					-- a boolean
!


!HandMorph methodsFor: 'accessing' stamp: 'jmv 12/30/2021 13:11:16'!
lastMouseEvent
	^ lastMouseEvent! !

!HandMorph methodsFor: 'accessing' stamp: 'jmv 12/30/2021 13:11:16'!
lastMouseEventTime
	^ lastMouseEventTime! !

!HandMorph methodsFor: 'accessing' stamp: 'jmv 12/30/2021 13:11:16'!
mouseOverHandler
	^mouseOverHandler ifNil:[mouseOverHandler _ MouseOverHandler new].! !

!HandMorph methodsFor: 'balloon help' stamp: 'jmv 12/30/2021 13:11:16'!
balloonHelp
	"Return the balloon morph associated with this hand"
	^self valueOfProperty: #balloonHelpMorph! !

!HandMorph methodsFor: 'balloon help' stamp: 'jmv 12/30/2021 13:11:16'!
balloonHelp: aBalloonMorph
	"Return the balloon morph associated with this hand"
	| oldHelp |
	oldHelp _ self balloonHelp.
	oldHelp ifNotNil:[oldHelp delete].
	aBalloonMorph
		ifNil:[self removeProperty: #balloonHelpMorph]
		ifNotNil:[self setProperty: #balloonHelpMorph toValue: aBalloonMorph]! !

!HandMorph methodsFor: 'balloon help' stamp: 'jmv 12/30/2021 13:11:16'!
deleteBalloonTarget: aMorph
	"Delete any existing balloon help.  This is now done unconditionally, whether or not the morph supplied is the same as the current balloon target"
	
	self balloonHelp: nil

"	| h |
	h _ self balloonHelp ifNil: [^ self].
	h balloonOwner == aMorph ifTrue: [self balloonHelp: nil]"! !

!HandMorph methodsFor: 'balloon help' stamp: 'jmv 12/30/2021 13:11:16'!
removePendingBalloonFor: aMorph
	"Get rid of pending balloon help."
	self removeAlarm: #spawnBalloonFor:.
	self deleteBalloonTarget: aMorph.! !

!HandMorph methodsFor: 'balloon help' stamp: 'jmv 12/30/2021 13:11:16'!
spawnBalloonFor: aMorph
	aMorph showBalloon: aMorph balloonText hand: self.! !

!HandMorph methodsFor: 'balloon help' stamp: 'jmv 12/30/2021 13:11:16'!
triggerBalloonFor: aMorph after: timeOut
	"Trigger balloon help after the given time out for some morph"
	self addAlarm: #spawnBalloonFor: with: aMorph after: timeOut.! !

!HandMorph methodsFor: 'caching' stamp: 'jmv 12/30/2021 13:11:16'!
releaseCachedState
	| oo |
	super releaseCachedState.
	oo _ owner.
	self removeAllMorphs.
	self initialize.	"nuke everything"
	self privateOwner: oo.
	self releaseAllFoci.
	savedPatch _ nil.! !

!HandMorph methodsFor: 'classification' stamp: 'jmv 12/30/2021 13:13:00'!
is: aSymbol
	^ aSymbol == #HandMorph or: [ super is: aSymbol ]! !

!HandMorph methodsFor: 'double click support' stamp: 'jmv 12/30/2021 13:11:16'!
dontWaitForMoreClicks
	"Reset the double-click detection state to normal (i.e., not waiting for a double-click).
	This happens after timeout, regardless of multiple clicks having been detected or not."

	mouseClickState _ nil.! !

!HandMorph methodsFor: 'double click support' stamp: 'jmv 12/30/2021 13:11:16'!
waitForClicksOrDrag: aMorph event: evt clkSel: clkSel dblClkSel: dblClkSel

	"Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods." 

	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: nil
			click: clkSel
			clickAndHalf: nil 
			dblClick: dblClkSel
			dblClickAndHalf: nil
			tripleClick: nil
			event: evt
			sendMouseButton2Activity: false! !

!HandMorph methodsFor: 'double click support' stamp: 'jmv 12/30/2021 13:11:16'!
waitForClicksOrDrag: aMorph event: evt dragSel: dragSel clkSel: clkSel
"juntar los 3? documentar mejor? uno solo completo y shortcuts?"
	"Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods." 

	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: dragSel
			click: clkSel
			clickAndHalf: nil 
			dblClick: nil
			dblClickAndHalf: nil
			tripleClick: nil
			event: evt
			sendMouseButton2Activity: false! !

!HandMorph methodsFor: 'double click support' stamp: 'jmv 12/30/2021 13:11:16'!
waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel

	"Wait until the difference between click, or drag gesture is known, then inform the given morph what transpired." 

	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: nil
			click: clkSel
			clickAndHalf: nil
			dblClick: nil
			dblClickAndHalf: nil
			tripleClick: nil
			event: evt
			sendMouseButton2Activity: Preferences tapAndHoldEmulatesButton2.

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"! !

!HandMorph methodsFor: 'double click support' stamp: 'jmv 12/30/2021 13:11:16'!
waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel clkNHalf: clkNHalfSel dblClkSel: dblClkSel dblClkNHalfSel: dblClkNHalfSel tripleClkSel: tripleClkSel

	"Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods." 

	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: nil
			click: clkSel
			clickAndHalf: clkNHalfSel
			dblClick: dblClkSel
			dblClickAndHalf: dblClkNHalfSel
			tripleClick: tripleClkSel
			event: evt
			sendMouseButton2Activity: Preferences tapAndHoldEmulatesButton2.

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"! !

!HandMorph methodsFor: 'double click support' stamp: 'jmv 12/30/2021 13:11:16'!
waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel clkNHalf: clkNHalfSel dblClkSel: dblClkSel dblClkNHalfSel: dblClkNHalfSel tripleClkSel: tripleClkSel dragSel: dragSel
	
	mouseClickState _
		MouseClickState new
			client: aMorph
			drag: dragSel
			click: clkSel
			clickAndHalf: clkNHalfSel
			dblClick: dblClkSel
			dblClickAndHalf: dblClkNHalfSel
			tripleClick: tripleClkSel
			event: evt
			sendMouseButton2Activity: Preferences tapAndHoldEmulatesButton2.

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"! !

!HandMorph methodsFor: 'double click support' stamp: 'jmv 12/30/2021 13:11:16'!
waitingForMoreClicks
	"Answer true "

	^mouseClickState notNil! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 12/30/2021 13:11:16'!
drawOn: aCanvas
	"Draw the hand itself (i.e., the cursor)."
	"This method is only called when we are carrying morphs around..."

	aCanvas drawHand! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 12/30/2021 13:11:16'!
fullDrawHandOn: aCanvas
	"A HandMorph has an unusual drawing requirement:
		The hand itself (i.e., the cursor) appears in front of its submorphs
	The illusion is that the hand plucks up morphs and carries them above the world."

	submorphs reverseDo: [ :m | aCanvas fullDraw: m ].
	self drawOn: aCanvas.  "draw the hand itself in front of morphs"
	lastPosition _ self morphPosition. "We already know we are carrying morphs."! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 12/30/2021 13:11:16'!
hideHardwareCursor

	| blankCursor |
	blankCursor _ Cursor cursorAt: #blankCursor.
	Cursor currentCursor == blankCursor ifFalse: [
		blankCursor activateCursor ].! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 12/30/2021 13:11:16'!
isDrawnBySoftware
	"Return true if this hand must be drawn explicitely instead of being drawn via the hardware cursor.
	Note. We draw the hand as a regular morph (using #drawOn:), disabling the hardware cursor, when we carry submorphs. The reason is to lock the mouse pointer and the carried morph together. Otherwhise the carried morph would lag behind the mouse pointer.
	This method answers whether the regular #drawOn: drawing mechanism is used for us.
	
	Check senders. Hand drawing is handled explicitly by the world, because the Hand is not a submorph of the world!!"

	^ submorphs anySatisfy: [ :ea | ea visible ]! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 12/30/2021 13:11:16'!
restoreSavedPatchOn: aCanvas
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch."
	
	prevFullBounds ifNotNil: [
		aCanvas restorePatch: savedPatch bounds: prevFullBounds.
		submorphs isEmpty ifTrue: [
			prevFullBounds _ nil ]].! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 12/30/2021 13:11:16'!
savePatchFrom: aCanvas
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	| fullBounds answer |
	fullBounds _ self displayFullBoundsForPatch intersect: (`0@0` extent: aCanvas extent).
	savedPatch _ aCanvas savePatch: savedPatch bounds: fullBounds.
	answer _ fullBounds.
	prevFullBounds ifNotNil: [ answer _ answer quickMerge: prevFullBounds ].
	prevFullBounds _ fullBounds.
	^answer! !

!HandMorph methodsFor: 'drawing' stamp: 'jmv 12/30/2021 13:11:16'!
showHardwareCursor

	"Make the transition to using hardware cursor.
	Report one final damage rectangle to erase the image of the software cursor."
	self invalidateDisplayRect: self displayFullBoundsForPatch for: nil.
	Cursor currentCursor == (Cursor cursorAt: #blankCursor) ifTrue: [ 
		"show hardware cursor"
		Cursor defaultCursor activateCursor ].! !

!HandMorph methodsFor: 'event handling' stamp: 'jmv 12/30/2021 13:11:16'!
createEventFrom: eventBuffer ofType: type

	type = EventSensor eventTypeMouse ifTrue: [ ^self generateMouseEvent: eventBuffer ].
	type = EventSensor eventTypeMouseScroll ifTrue: [ ^self generateMouseScrollEvent: eventBuffer ].
	type = EventSensor eventTypeKeyboard ifTrue: [ ^self generateKeyboardEvent: eventBuffer ].
	type = EventSensor eventTypeWindow ifTrue: [ ^self generateWindowEvent: eventBuffer ].
	type = EventSensor eventTypeDragDropFiles ifTrue: [ ^self generateDropFilesEvent: eventBuffer ].
		
	"All other events are ignored"
	^nil ! !

!HandMorph methodsFor: 'event handling' stamp: 'jmv 12/30/2021 13:11:16'!
flushEvents
	"Flush any events that may be pending"
	self flag: #arNote. "Remove it and fix senders"
	Sensor flushEvents.! !

!HandMorph methodsFor: 'event handling' stamp: 'jmv 12/30/2021 13:11:16'!
noticeMouseOver: aMorph event: anEvent
	mouseOverHandler ifNil:[^self].
	mouseOverHandler noticeMouseOver: aMorph event: anEvent.! !

!HandMorph methodsFor: 'event handling' stamp: 'jmv 12/30/2021 13:11:16'!
processEventQueue
	"Process user input events from the local input devices.
	Answer true if any event was handled (but ignore MouseMove)"

	| evt evtBuf type hadAny mcs hadAnyMouseEvent hadAnyMouseMoveEvent |
	mcs _ mouseClickState.
	hadAny _ false.
	hadAnyMouseEvent _ false.
	hadAnyMouseMoveEvent _ false.
	[ (evtBuf := Sensor nextEvent) isNil ] whileFalse: [
		type _ evtBuf first.
		evt _ self createEventFrom: evtBuf ofType: type.
		evt ifNotNil: [
			"Finally, handle it. But process only up to one mouseMove per cycle. Discard the rest."
			(evt isMouseMove and: [ hadAnyMouseMoveEvent ]) ifFalse: [
				self startEventDispatch: evt.
				hadAny _ true.
				evt isMouse ifTrue: [
					hadAnyMouseEvent _ true.
					evt isMouseMove ifTrue: [
						hadAnyMouseMoveEvent _ true ]]]]].
	hadAnyMouseEvent ifFalse: [
		mcs 
			ifNotNil: [ 
				"No mouse events during this cycle. Make sure click states time out accordingly"
				mcs
					handleEvent: lastMouseEvent asMouseMove
					from: self ]].
	^hadAny! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 12/30/2021 13:11:16'!
startDropEventDispatch: aDropEvent

	owner dispatchEvent: aDropEvent.
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 12/30/2021 13:11:16'!
startDropFilesEventDispatch: aDropFilesEvent

	owner dispatchEvent: aDropFilesEvent.
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 12/30/2021 13:11:16'!
startEventDispatch: aMorphicEvent

	owner ifNil: [ ^ self ].
	aMorphicEvent startDispatchFrom: self! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 12/30/2021 13:11:16'!
startKeyboardDispatch: aKeyboardEvent

	| focusedElement |
	
	focusedElement _ self keyboardFocus ifNil: [ self world ].
	focusedElement handleFocusEvent: aKeyboardEvent.
	
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 12/30/2021 13:11:16'!
startMouseDispatch: aMouseEvent

	aMouseEvent isMouseOver ifTrue: [
		^self mouseFocus
			ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
			ifNil: [ owner dispatchEvent: aMouseEvent ]].

	"any mouse event but mouseOver"
	lastMouseEvent _ aMouseEvent.	
	lastMouseEventTime _ Time localMillisecondClock.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMouseEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			^self mouseOverHandler processMouseOver: lastMouseEvent  ]].

	aMouseEvent isMove
		ifTrue: [
			self morphPosition: aMouseEvent eventPosition.
			self mouseFocus
				ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
				ifNil: [ owner dispatchEvent: aMouseEvent ]
		] ifFalse: [
		aMouseEvent isMouseScroll ifTrue: [
			owner dispatchEvent: aMouseEvent
		] ifFalse: [
			"Issue a synthetic move event if we're not at the position of the event"
			aMouseEvent eventPosition = self morphPosition ifFalse: [
				"Issue a mouse move event to make the receiver appear at the given position"
				self startMouseDispatch: (MouseMoveEvent new
					setType: #mouseMove
					position: aMouseEvent eventPosition
					buttons: aMouseEvent buttons
					hand: self
					stamp: aMouseEvent timeStamp) ].
			"Drop submorphs on button events"
			self hasSubmorphs
				ifTrue: [
					"Not if we are grabbing them"
					mouseClickState ifNil: [
						"Want to drop on mouseUp, NOT mouseDown"
						aMouseEvent isMouseUp ifTrue: [
							self dropMorphs: aMouseEvent ]
					]
				] ifFalse: [
					self mouseFocus
						ifNotNil: [ mouseFocus handleFocusEvent: aMouseEvent ]
						ifNil: [ owner dispatchEvent: aMouseEvent ]]]].
		self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'events-processing' stamp: 'jmv 12/30/2021 13:11:16'!
startWindowEventDispatch: aWindowEvent

	owner dispatchEvent: aWindowEvent.
	self mouseOverHandler processMouseOver: lastMouseEvent! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/30/2021 13:11:16'!
activateNextWindow

	self nextFocusWindow ifNotNil: [ :w |
		w activateAndSendTopToBack: true ]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/30/2021 13:11:16'!
activatePreviousWindow

	self previousFocusWindow ifNotNil: [ :w |
		w activateAndSendTopToBack: false ]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/30/2021 13:11:16'!
keyboardFocus

	keyboardFocus ifNotNil: [
		keyboardFocus world
			ifNil: [ keyboardFocus _ nil ]].
	^ keyboardFocus! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/30/2021 13:11:16'!
keyboardFocusNext

	| m |
	m _ self nextFocusMorph.
	m ifNotNil: [
		m activateWindowAndSendTopToBack: false.
		self newKeyboardFocus: m ]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/30/2021 13:11:16'!
keyboardFocusPrevious

	| m |
	m _ self previousFocusMorph.
	m ifNotNil: [
		m activateWindowAndSendTopToBack: true.
		self newKeyboardFocus: m ]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/30/2021 13:11:16'!
mouseFocus

	mouseFocus ifNotNil: [
		(mouseFocus isWorldMorph or: [mouseFocus isInWorld not])
			ifTrue: [ mouseFocus _ nil ]].
	^mouseFocus! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/30/2021 13:11:16'!
newKeyboardFocus: aMorphOrNil
	"Make the given morph the new keyboard focus, canceling the previous keyboard focus if any. If the argument is nil, the current keyboard focus is cancelled."
	| oldFocus |
	oldFocus _ keyboardFocus.
	keyboardFocus _ aMorphOrNil.
	oldFocus ifNotNil: [oldFocus == aMorphOrNil ifFalse: [oldFocus keyboardFocusChange: false]].
	aMorphOrNil ifNotNil: [aMorphOrNil keyboardFocusChange: true].
! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/30/2021 13:11:16'!
newMouseFocus: aMorphOrNil
	"Make the given morph the new mouse focus, canceling the previous mouse focus if any. If the argument is nil, the current mouse focus is cancelled."
	mouseFocus _ aMorphOrNil.
! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/30/2021 13:11:16'!
nextFocusMorph
	"Or nil"

	^(keyboardFocus ifNil: [ self world ])
		previousMorphThat: [ :m |
			m handlesKeyboard and: [ m isReallyVisible ]]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/30/2021 13:11:16'!
nextFocusWindow
	"Or nil"

	^(SystemWindow topWindow ifNil: [ self world ])
		nextMorphThat: [ :m | 
			(m is: #SystemWindow) and: [ m isReallyVisible ]]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/30/2021 13:11:16'!
previousFocusMorph
	"Or nil"

	^ (keyboardFocus ifNil: [ self world ])
		nextMorphThat: [ :m | 
			m handlesKeyboard and: [ m isReallyVisible ]]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/30/2021 13:11:16'!
previousFocusWindow
	"Or nil"

	^ (SystemWindow topWindow ifNil: [ self world ])
		previousMorphThat: [ :m |
			(m is: #SystemWindow) and: [ m isReallyVisible ]]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/30/2021 13:11:16'!
releaseAllFoci
	mouseFocus _ nil.
	keyboardFocus _ nil.
! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/30/2021 13:11:16'!
releaseKeyboardFocus
	"Release the current keyboard focus unconditionally"
	self newKeyboardFocus: nil.
! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/30/2021 13:11:16'!
releaseKeyboardFocus: aMorph
	"If the given morph had the keyboard focus before, release it"
	keyboardFocus ifNotNil: [
		keyboardFocus withAllOwnersDo: [ :outerOwner |
			outerOwner == aMorph ifTrue: [self releaseKeyboardFocus]]]! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/30/2021 13:11:16'!
releaseMouseFocus
	"Release the current mouse focus unconditionally."
	self newMouseFocus: nil.! !

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 12/30/2021 13:11:16'!
releaseMouseFocus: aMorph
	"If the given morph had the mouse focus before, release it"
	self mouseFocus == aMorph ifTrue:[self releaseMouseFocus].! !

!HandMorph methodsFor: 'geometry' stamp: 'jmv 12/30/2021 13:11:16'!
basicDisplayBounds

	^ self boundsFinderCanvas displayBoundsForHand: self! !

!HandMorph methodsFor: 'geometry' stamp: 'jmv 12/30/2021 13:11:16'!
displayFullBoundsForPatch
	"Current full bounds of hand and carried submorphs.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around).
	Integer pixel coordinates!!"

	| handBounds fullBounds submorphBounds |
	handBounds _ self basicDisplayBounds.
	submorphBounds _ nil.
	self submorphsDo: [ :m |
		m displayFullBounds ifNotNil: [ :b |
			submorphBounds _ b quickMerge: submorphBounds ]].
	fullBounds _ handBounds quickMerge: submorphBounds.
	lastPosition
		ifNotNil: [ :lastPos |						"When already carrying morphs around. Update rectangle to current hand position."
			fullBounds _ fullBounds quickMerge: (submorphBounds translatedBy: self morphPosition-lastPos) ].
	^fullBounds encompassingIntegerRectangle! !

!HandMorph methodsFor: 'geometry' stamp: 'jmv 12/30/2021 13:11:16'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."

	| prevTranslation |
	prevTranslation _ location translation.
	location _ location withTranslation: aPoint.
	"Ask if translation effectively changed, after possible conversion to 32 bit Float in AffineTransformation. "
	location translation = prevTranslation ifFalse: [
		self isDrawnBySoftware
			ifTrue: [
				Preferences cacheDisplayContentWhenMovingMorphs
					ifTrue: [
						"We are caching whatever is in the Display below us. Thefore, there's no need
						to do an invalidation that would trigger the redraw of everything below us."
						self needsRedraw: true ]
					ifFalse:  [
						"No caching of stuff below us. Just invalidate and redraw."
						self redrawNeeded ]]
			ifFalse: [
				lastPosition _ nil.		"Not nil if carrying morphs at that moment"
				prevFullBounds _ nil "Any saved patch is no longer relevant"]].! !

!HandMorph methodsFor: 'geometry testing' stamp: 'jmv 12/30/2021 13:11:16'!
submorphsMightProtrude
	"Morphs we carry are usually larger than us."

	^true! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 12/30/2021 13:11:16'!
attachMorph: aMorph
	"Position the center of the given morph under this hand, then grab it.
	This method is used to grab far away or newly created morphs."

	^self grabMorph: aMorph moveUnderHand: true! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 12/30/2021 13:11:16'!
attachMorphBeside: aMorph

	"Position the given morph beside this hand, then grab it."

	aMorph aboutToBeGrabbedBy: self.
	self grabMorph: aMorph delta: extent x@0.
! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 12/30/2021 13:11:16'!
dropMorph: aMorph event: aMouseEvent
	"Drop the given morph which was carried by the hand"
	| morphData dropEvent |
	morphData := self grabMorphDataFor: aMorph.
	dropEvent _ DropEvent new 
			setPosition: self morphPosition 
			contents: aMorph 
			hand: self
			formerOwner: (morphData at: 1)
			formerPosition: (morphData at: 2).
	owner dispatchEvent: dropEvent.
	dropEvent wasHandled ifFalse: [ aMorph rejectDropMorphEvent: dropEvent ].
	self forgetGrabMorphDataFor: aMorph.
	self mouseOverHandler processMouseOver: aMouseEvent! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 12/30/2021 13:11:16'!
dropMorphs: anEvent
	"Drop the morphs at the hands position"

	self showHardwareCursor.
	self submorphsReverseDo: [ :m |
		"Drop back to front to maintain z-order"
		self dropMorph: m event: anEvent ].! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 12/30/2021 13:11:16'!
grabMorph: aMorph
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	^self grabMorph: aMorph moveUnderHand: false! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 12/30/2021 13:11:16'!
grabMorph: aMorph delta: delta
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner)."

	| formerOwner formerPositionInWorld |
	self releaseMouseFocus. "Break focus"

	"Grab the halo if present"
	self halo ifNotNil: [ :h |
		(h target == aMorph or: [h target hasOwner: aMorph]) ifTrue: [
			self addMorphBack: h position: h morphPositionInWorld - self morphPositionInWorld ]].

	"Remember previous owner and position, in case the drop is later rejected"
	formerOwner _ aMorph owner.
	formerOwner ifNotNil: [
		formerPositionInWorld _ aMorph morphPositionInWorld.
		grabMorphData
			at: aMorph 
			put: { formerOwner. formerPositionInWorld. } ].
	self addMorphBack: aMorph position: delta.

	aMorph displayBounds ifNotNil: [ :r | | d |
		d _ formerPositionInWorld ifNotNil: [aMorph morphPositionInWorld - formerPositionInWorld].
		(d isNil or: [d isZero not]) ifTrue: [
			aMorph adjustDisplayBoundsBy: d ]].

	aMorph justGrabbedFrom: formerOwner.
	formerOwner ifNotNil: [
		"Morph was in the world"
		lastPosition _ self morphPosition ].! !

!HandMorph methodsFor: 'grabbing/dropping' stamp: 'jmv 12/30/2021 13:11:16'!
grabMorph: aMorph moveUnderHand: moveUnderHand
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner).
	If moveUnderHand is requested or it seems neccesary anyway, move the grabbed morph under the hand."

	| grabbed positionInHandCoordinates tx bounds |
	self releaseMouseFocus.	"Break focus"
	grabbed _ aMorph.
	aMorph owner ifNotNil: [ :o | grabbed _ o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed _ grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].
	self hideHardwareCursor.
	self redrawNeeded.

	moveUnderHand ifTrue: [
		bounds _ nil.
		grabbed isInWorld ifTrue: [
			grabbed displayBounds ifNotNil: [ :r |
				bounds _ r translatedBy: grabbed morphPositionInWorld negated ]].
		bounds ifNil: [ bounds _ grabbed fullBoundsInOwner ].
		positionInHandCoordinates _ (bounds center + bounds bottomRight //2) negated.
		self grabMorph: grabbed delta: positionInHandCoordinates.
		^self ].

	positionInHandCoordinates _ (grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph])
		morphPositionInWorld - self morphPositionInWorld.

	tx _ GeometryTransformation identity.
	(grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph]) withAllOwnersDo: [ :o |
		tx _ o location composedWith: tx ].
	self withAllOwnersReverseDo: [ :o |
		tx _ o location inverseTransformation composedWith: tx ].
	self grabMorph: grabbed delta: positionInHandCoordinates.

	grabbed location: tx.! !

!HandMorph methodsFor: 'halo handling' stamp: 'jmv 12/30/2021 13:11:16'!
halo: newHalo
	"Set halo associated with this hand"
	| oldHalo |
	oldHalo _ self halo.
	(oldHalo isNil or:[oldHalo == newHalo]) ifFalse:[oldHalo delete].
	newHalo
		ifNil:[self removeProperty: #halo]
		ifNotNil:[self setProperty: #halo toValue: newHalo]! !

!HandMorph methodsFor: 'halo handling' stamp: 'jmv 12/30/2021 13:11:16'!
obtainHalo: aHalo
	"Used for transfering halos between hands"
	| formerOwner |
	self halo == aHalo ifTrue:[^self].
	"Find former owner"
	formerOwner _ self world hands detect: [ :h | h halo == aHalo] ifNone: nil.
	formerOwner ifNotNil:[formerOwner releaseHalo: aHalo].
	self halo: aHalo! !

!HandMorph methodsFor: 'halo handling' stamp: 'jmv 12/30/2021 13:11:16'!
releaseHalo: aHalo
	"Used for transfering halos between hands"
	self removeProperty: #halo! !

!HandMorph methodsFor: 'halo handling' stamp: 'jmv 12/30/2021 13:11:16'!
removeHaloFromClick: anEvent on: aMorph
	| halo |
	halo _ self halo ifNil:[^self].
	(halo target hasOwner: self) ifTrue:[^self].
	(halo staysUpWhenMouseIsDownIn: aMorph) ifFalse:[
		halo delete.
		self removeProperty: #halo.
	].! !

!HandMorph methodsFor: 'halos and balloon help' stamp: 'jmv 12/30/2021 13:11:16'!
halo
	"Return the halo associated with this hand, if any"
	^self valueOfProperty: #halo! !

!HandMorph methodsFor: 'initialization' stamp: 'jmv 12/30/2021 13:11:16'!
initForEvents
	mouseOverHandler _ nil.
	lastMouseEvent _ MouseEvent new setType: #mouseMove position: `0@0` buttons: 0 hand: self.
	lastMouseEventTime _ Time localMillisecondClock.
	lastEventBuffer _ {1. 0. 0. 0. 0. 0. nil. nil}.
	self dontWaitForMoreClicks! !

!HandMorph methodsFor: 'initialization' stamp: 'jmv 12/30/2021 13:11:16'!
initialize
	super initialize.
	self initForEvents.
	keyboardFocus _ nil.
	mouseFocus _ nil.
	extent _ CursorWithMask defaultCursor extent.
	grabMorphData _ IdentityDictionary new.
	self initForEvents.! !

!HandMorph methodsFor: 'testing' stamp: 'jmv 12/30/2021 13:11:16'!
isIncludedInTaskbar
	"Answer true a button for us should be added to any TaskbarMorph."

	^ false! !

!HandMorph methodsFor: 'objects from disk' stamp: 'jmv 12/30/2021 13:11:16'!
objectForDataStream: refStrm
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	| dp |
	dp _ DiskProxy global: #Smalltalk selector: #activeHand args: #().
	refStrm replace: self with: dp.
	^ dp! !

!HandMorph methodsFor: 'paste buffer' stamp: 'jmv 12/30/2021 13:11:16'!
pasteMorph

	| pastee |
	pastee _ Clipboard retrieveMorph.
	pastee ifNil: [^ self inform: 'Nothing to paste.'].
	self attachMorph: pastee! !

!HandMorph methodsFor: 'private' stamp: 'jmv 12/30/2021 13:11:16'!
forgetGrabMorphDataFor: aMorph

	grabMorphData 
		removeKey: aMorph 
		ifAbsent: [ "no error" ]! !

!HandMorph methodsFor: 'private' stamp: 'jmv 12/30/2021 13:11:16'!
grabMorphDataFor: aMorph

	^ grabMorphData at: aMorph ifAbsent: [ { nil. nil. } ]! !

!HandMorph methodsFor: 'private' stamp: 'jmv 12/30/2021 13:11:16'!
rememberGrabMorphDataFor: aMorph

	grabMorphData
		at: aMorph 
		put: { aMorph owner. aMorph morphPositionInWorld. }! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 12/30/2021 13:11:16'!
generateDropFilesEvent: evtBuf 
	"Generate the appropriate mouse event for the given raw event buffer"

	| position stamp numberOfFiles dragType |
	
	stamp := evtBuf second.
	stamp = 0 ifTrue: [stamp := Time millisecondClockValue]. "VMs report events using #millisecondClockValue"
	dragType := evtBuf third.
	position := evtBuf fourth @ evtBuf fifth.
	numberOfFiles := evtBuf seventh.
	
	^ dragType = 4 ifTrue: [ DropFilesEvent at: position with: numberOfFiles from: self].
! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 12/30/2021 13:11:16'!
generateKeyboardEvent: evtBuf
	"Generate the appropriate mouse event for the given raw event buffer"
	| buttons modifiers type keyValue pressType stamp mouseScrollDirection |
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [ stamp _ Time millisecondClockValue ]. "VMs report events using #millisecondClockValue"
	(evtBuf sixth <= 0 or: [ (keyValue _ Character iso8859s15CodeForUnicodeCodePoint: evtBuf sixth) isNil ]) ifTrue: [ keyValue _ Character macRomanToLatin1: evtBuf third ].
	Sensor peekEvent ifNotNil: [ :nxt |
		"start: Combining diacritical marks (i.e. accents in the Linux VM)"
		(nxt fourth = EventSensor eventKeyDown and: [ nxt third > 255 ]) ifTrue: [
			keyValue _ ((Character numericValue: keyValue) withDiacriticalMark: nxt third) iso8859s15Code.
			Sensor
				nextEvent;
				nextEvent;
				nextEvent ].
		"end: Combining diacritical marks (i.e. accents in the Linux VM)"
		"start: Spurious LF after CR on Ctrl-Enter on Windows VM"
		((evtBuf fourth = EventSensor eventKeyChar and: [ evtBuf third = 13 ]) and: [
			nxt fourth = EventSensor eventKeyChar and: [ nxt third = 10 ]]) ifTrue: [ Sensor nextEvent
			"print " ]].
	modifiers _ evtBuf fifth.
	pressType _ evtBuf fourth.
	pressType = EventSensor eventKeyDown ifTrue: [
		type _ #keyDown.
		lastKeyDownValue _ keyValue ].
	pressType = EventSensor eventKeyUp ifTrue: [
		(keyValue = 9 and: [(modifiers anyMask: 1) and: [Smalltalk platformName = 'unix']])
			ifTrue: [
				"Linux VMs don't generate shift-tab keystroke. Turn #keyUp into #keystroke"
				pressType _ EventSensor eventKeyChar ]
			ifFalse: [type _ #keyUp ]].
	pressType = EventSensor eventKeyChar ifTrue: [
		type _ #keystroke.
		"If Control key pressed, and the VM answers a code below 27,
		 it means it did the translation, convert it back to regular character:
		We want to handle the meaning of ctrl ourselves."
		(modifiers anyMask: 2) ifTrue: [ 											"Control key pressed"
			keyValue < 27 ifTrue: [ 										"But we don't want to do it for Home/End/PgUp/PgDn, just for alphabetic keys"
				lastKeyDownValue = keyValue ifFalse: [   									"If equal, real Home/End/PgUp/PgDn in Windows => don't translate"
					(keyValue + 64 = lastKeyDownValue or: [ "If Equal, Ctrl-alphabetic in Windows => do translate"
						lastKeyDownValue < 47 ]) ifTrue: [ 			"Not on windows. If less (not sure about the bound, but do not translate 48: tab on Mac), alphabetic on Mac => do translate"
						keyValue _ (modifiers anyMask: 1)
							ifTrue: [ keyValue + 64 ]
							ifFalse: [ keyValue + 96   						"shift not pressed: conver to lowercase letter" ]]]].
			"On Windows, ctrl-backSpace is reported as ctrl-forwardDelete. But keyDown is ok, so we can know and fix."
			(keyValue = 127 and: [ lastKeyDownValue = 8 ])
				ifTrue: [ keyValue _ 8 ].
			"Act as if command/alt was pressed for some usual Windows ctrl-key combinations"
			(self shouldControlEmulateAltFor: keyValue) ifTrue: [ modifiers _ modifiers bitOr: 8 ]].
		(modifiers anyMask: 8) ifTrue: [ 									"CmdAlt key pressed (or Control key pressed, and #shouldControlEmulateAltFor: just answered true)"
			(modifiers anyMask: 1) ifTrue: [								"Shift pressed"
				| i |
				"It seems that for ctrl-shifted keys and cmd-shifted keys, the VM incorrectly reports the UNSHIFTED character.
				Correct this, at least for common cmd-shortcuts, and for the US keyboard... Sigh...
				(This has only been observed on Mac VMs, but seems harmless if proper shifted character is reported (as in Linux), as this wil be NOP)
				(On Windows, the situation is even worse: ctrl-{ is not even reported as a keystroke event. Only keyDown and keyUp.)"
				"#($' $, $. $9 $0 $[ $]) -> #($'' $< $> $( $) ${) $}"
				i _ #[39 44 46 57 48 91 93 ] indexOf: keyValue.
				i > 0 ifTrue: [
					keyValue _ #[34 60 62 40 41 123 125] at: i ]]]].
	buttons _ modifiers bitShift: 3.
	"Linux and Windows VM send keyboard ctrl-upArrow and ctrl-downArrow when the user tries to scroll using the mouse wheel
	Mac VM sends cmd-option-ctrl-shift-upArrow and cmd-option-ctrl-shift-downArrow for trackpad vertical scroll gestures,
		and cmd-option-ctrl-shift-leftArrow and cmd-option-ctrl-shift-rightArrow for horizontal scroll gestures.
	This way of reporting scroll events by the VM also enables scrolling using the keyboard (actually, we can't tell if user gesture was on Mouse, Trackpad or Keyboard).
	But ctrl-shift and cmdAlt-shift are needed used for selecting while moving by word, line, etc.
	Additionally, #ctrlArrowsScrollHorizontally allows chosing between keyboard horizontal scroll and moving word by word in text editors."
	mouseScrollDirection _ nil.
	"Ctrl for Keyboard or Mouse wheel gestures. All modifiers for Trackpad gestures."
	(buttons = InputSensor controlKey or: [buttons = InputSensor cmdAltOptionCtrlShiftModifierKeys]) ifTrue: [
		keyValue = 30
			ifTrue: [mouseScrollDirection _ #up]
		ifFalse: [keyValue = 31
			ifTrue: [mouseScrollDirection _ #down]]].
	"Ctrl for Keyboard or Mouse wheel gestures, only if preference is set. All modifiers for Trackpad gestures."
	((buttons = InputSensor controlKey and: [Preferences ctrlArrowsScrollHorizontally]) or: [buttons = InputSensor cmdAltOptionCtrlShiftModifierKeys]) ifTrue: [
		keyValue = 28
			ifTrue: [mouseScrollDirection _ #left]
		ifFalse: [keyValue = 29
			ifTrue: [mouseScrollDirection _ #right]]].
	mouseScrollDirection ifNotNil: [
		^ MouseScrollEvent new
			setType: #mouseScroll
			position: self morphPosition
			direction: mouseScrollDirection
			buttons: buttons
			hand: self
			stamp: stamp ].
	^ KeyboardEvent new
		setType: type
		buttons: buttons
		position: self morphPosition
		keyValue: keyValue
		hand: self
		stamp: stamp! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 12/30/2021 13:11:16'!
generateMouseEvent: evtBuf 
	"Generate the appropriate mouse event for the given raw event buffer"

	| pos buttons modifiers type trail stamp oldButtons |
	stamp := evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time millisecondClockValue ]. "VMs report events using #millisecondClockValue"
	pos := evtBuf third @ evtBuf fourth.
	buttons := evtBuf fifth.
	modifiers := evtBuf sixth.
	type := buttons = 0 
		ifTrue: [
			lastEventBuffer fifth = 0 ifTrue: [#mouseMove] ifFalse: [#mouseUp]]
		ifFalse: [
			lastEventBuffer fifth = 0 
						ifTrue: [#mouseDown]
						ifFalse: [#mouseMove]].
	buttons := buttons bitOr: (modifiers bitShift: 3).
	oldButtons := lastEventBuffer fifth 
				bitOr: (lastEventBuffer sixth bitShift: 3).
	lastEventBuffer := evtBuf.
	type == #mouseMove 
		ifTrue: [
			trail := self mouseTrailFrom: evtBuf.
			^MouseMoveEvent new 
				setType: type
				position: trail last
				buttons: buttons
				hand: self
				stamp: stamp].
	^MouseButtonEvent new 
		setType: type
		position: pos
		which: (oldButtons bitXor: buttons)
		buttons: buttons
		hand: self
		stamp: stamp! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 12/30/2021 13:11:16'!
generateMouseScrollEvent: evtBuf
	"Generate the appropriate mouse wheel event for the given raw event buffer
	
	Copied from Sqeak 5.3 and modifed."

	| buttons modifiers stamp deltaX deltaY direction oldButtons |
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [stamp := Time millisecondClockValue ].
	deltaX _ evtBuf third.
	deltaY _ evtBuf fourth.
	"This implementation deliberatly ignores movements in both dimensions:"
	direction _ 
		deltaY negative
			ifTrue: [ #down ]
			ifFalse: [ deltaY strictlyPositive
				ifTrue: [ #up ]
				ifFalse: [ deltaX negative
					ifTrue: [ #left ]
					ifFalse: [ deltaX strictlyPositive
						ifTrue: [ #right ]
						ifFalse: [ ^ nil "No movement, bailing out" ] ] ] ].
	modifiers _ evtBuf fifth.
	buttons _ (modifiers bitShift: 3) bitOr: (lastMouseEvent buttons bitAnd: 7).
	oldButtons _ lastEventBuffer fifth 
		bitOr: (lastEventBuffer sixth bitShift: 3).
	lastEventBuffer := evtBuf.
	^ MouseScrollEvent new
		setType: #mouseScroll
		position: self morphPosition 
		direction: direction
		buttons: (oldButtons bitXor: buttons)
		hand: self
		stamp: stamp! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 12/30/2021 13:11:16'!
generateWindowEvent: evtBuf 
	"Generate the appropriate window event for the given raw event buffer"

	| evt |
	evt := WindowEvent new.
	evt setTimeStamp: evtBuf second.
	evt timeStamp = 0 ifTrue: [evt setTimeStamp: Time millisecondClockValue]. "VMs report events using #millisecondClockValue"
	evt windowAction: evtBuf third.
	evt rectangle: (Rectangle origin: evtBuf fourth @ evtBuf fifth corner: evtBuf sixth @ evtBuf seventh ).
	
	^evt! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 12/30/2021 13:11:16'!
mouseTrailFrom: currentBuf 
	"Current event, a mouse event buffer, is about to be processed.  If there are other similar mouse events queued up, then drop them from the queue, and report the positions inbetween."

	| nextEvent trail |
	trail := WriteStream on: (Array new: 1).
	trail nextPut: currentBuf third @ currentBuf fourth.
	[(nextEvent := Sensor peekEvent) isNil] whileFalse: 
			[nextEvent first = currentBuf first 
				ifFalse: [^trail contents	"different event type"].
			nextEvent fifth = currentBuf fifth 
				ifFalse: [^trail contents	"buttons changed"].
			nextEvent sixth = currentBuf sixth 
				ifFalse: [^trail contents	"modifiers changed"].
			"nextEvent is similar.  Remove it from the queue, and check the next."
			nextEvent := Sensor nextEvent.
			trail nextPut: nextEvent third @ nextEvent fourth].
	^trail contents! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 12/30/2021 13:11:16'!
shouldControlEmulateAltFor: keyValue
	"At least on Linux Windows, command key is usually ctrl, not alt.
	But not for arrow keys!! ctrl-left ~~ alt-left"
	^ keyValue > 32! !


!HandMorph class methodsFor: 'new-morph participation' stamp: 'jmv 12/30/2021 13:11:16'!
includeInNewMorphMenu
	"Not to be instantiated from the menu"
	^ false! !


!PasteUpMorph methodsFor: 'accessing' stamp: 'jmv 12/30/2021 13:11:25'!
color: aColor
	super color: aColor.
	self backgroundImageData: nil! !

!PasteUpMorph methodsFor: 'caching' stamp: 'jmv 12/30/2021 13:11:25'!
releaseCachedState
	super releaseCachedState.
	backgroundImage _ nil.! !

!PasteUpMorph methodsFor: 'drawing' stamp: 'jmv 12/30/2021 13:11:25'!
drawOn: aCanvas

	"draw background image."
	backgroundImage
		ifNotNil: [
			aCanvas image: backgroundImage at: `0@0` ]
		ifNil: [
			super drawOn: aCanvas ]! !

!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 12/30/2021 13:11:25'!
allowsMorphDrop
	"Answer whether we accept dropping morphs. By default answer false."

	^ true! !

!PasteUpMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 12/30/2021 13:11:25'!
allowsSubmorphDrag
	"Answer whether our morphs can just be grabbed with the hand, instead of requiring the use of the halo. By default answer false.
	Both 'aMorph allowsGrabWithHand' and 'aMorph owner allowsSubmorphDrag' must be true for aMorph to be grabbed by the hand. It is also required that 'aMorph handlesMouseDown:' be false."

	^ true! !

!PasteUpMorph methodsFor: 'events' stamp: 'jmv 12/30/2021 13:11:25'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	"Handle a mouse down event."

	super mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition.

	aMouseButtonEvent hand
		waitForClicksOrDragOrSimulatedMouseButton2: self 
		event: aMouseButtonEvent
		clkSel: #click:localPosition:.! !

!PasteUpMorph methodsFor: 'event handling testing' stamp: 'jmv 12/30/2021 13:11:25'!
handlesMouseDown: aMouseButtonEvent
	^true! !

!PasteUpMorph methodsFor: 'event handling' stamp: 'jmv 12/30/2021 13:11:25'!
wantsWindowEvent: anEvent
	^false! !

!PasteUpMorph methodsFor: 'geometry' stamp: 'jmv 12/30/2021 13:11:25'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."

	super extentChanged: oldExtent.
	self buildMagnifiedBackgroundImage.! !

!PasteUpMorph methodsFor: 'initialization' stamp: 'jmv 12/30/2021 13:11:25'!
defaultBorderColor
	"answer the default border color/fill style for the receiver"
	^ `Color
		r: 0.861
		g: 1.0
		b: 0.722`! !

!PasteUpMorph methodsFor: 'initialization' stamp: 'jmv 12/30/2021 13:11:25'!
defaultBorderWidth
	"answer the default border width for the receiver"
	^ 1! !

!PasteUpMorph methodsFor: 'initialization' stamp: 'jmv 12/30/2021 13:11:25'!
defaultColor
	"answer the default color/fill style for the receiver"
	^ `Color
		r: 0.8
		g: 1.0
		b: 0.6`! !

!PasteUpMorph methodsFor: 'misc' stamp: 'jmv 12/30/2021 13:11:25'!
backgroundImage
	^backgroundImage! !

!PasteUpMorph methodsFor: 'misc' stamp: 'jmv 12/30/2021 13:11:25'!
backgroundImageData: aByteArray
	"
	| filename |
	filename _ 'bg/free-3d-art-pictures-gallery-wallpaper-desktop-18.jpg'.
	filename _ 'bg/free-desktop-wallpaper.jpg'.
	filename _ 'bg/jellyfish-thumb.jpg'.
	filename _ 'bg/splash_by_beefpepsi.jpg'.
	filename _ 'bg/gray ocean and pier.jpg'.
	filename _ 'bg/newyork.jpg'.
	filename _ 'bg/download-free-desktop-wallpaper-nature-conquestofparadise-marirs-pic.jpg'.
	filename _ 'bg/desktop-wallpaper-tropical-1280x1024.jpg'.

	filename _ 'bg/free-3d-art-pictures-gallery-wallpaper-desktop-18.jpg'.
	self runningWorld backgroundImageData: (FileStream readOnlyFileNamed: filename) binary contentsOfEntireFile.
	"
	backgroundImageData _ aByteArray.
	self buildMagnifiedBackgroundImage! !

!PasteUpMorph methodsFor: 'misc' stamp: 'jmv 12/30/2021 13:11:25'!
buildMagnifiedBackgroundImage
	| image |
	backgroundImageData
		ifNil: [ backgroundImage _ nil ]
		ifNotNil: [
			[
				backgroundImage _ nil.
				Smalltalk primitiveGarbageCollect.
				image _ Form fromBinaryStream: backgroundImageData readStream.
				backgroundImage _ image magnifyTo: extent.
				backgroundImage _ backgroundImage orderedDither32To16 asColorFormOfDepth: 8.
				image _ nil.
				Smalltalk primitiveGarbageCollect.
				backgroundImage bits pin.
			] on: Error do: [backgroundImage := nil]. "Can happen if JPEG plugin not built"
			self redrawNeeded
		]! !

!PasteUpMorph methodsFor: 'stepping and presenter' stamp: 'jmv 12/30/2021 13:11:25'!
wantsSteps
	"Return true if the receiver wants to its #step or #stepAt: methods be run ALL THE TIME.
	Morphs that send #startStepping and #stopStepping at appropriate times (i.e. when they are already in the world!!) don't need to answer true to this message.
	jmv: Not really sure. Sub-world stepping needs some review."

	^true! !

!PasteUpMorph methodsFor: 'testing' stamp: 'jmv 12/30/2021 13:11:25'!
isReallyVisible
	"Answer true only if all the owner chain is visible (i.e. if we are really visible!!)"
	^self visible and: [owner isReallyVisible ]! !

!PasteUpMorph methodsFor: 'halos and balloon help' stamp: 'jmv 12/30/2021 13:11:25'!
wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"
	(#(addHelpHandle: addRotateHandle: addRecolorHandle:) statePointsTo: aSelector)
		ifTrue: ["FIXME - hack to disable for non-functional halo items"
			^ false].
	
	^super wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph! !


!PasteUpMorph class methodsFor: 'system startup' stamp: 'jmv 12/30/2021 13:11:25'!
initClassCachedState

	self allSubInstancesDo: [ :each |
		each buildMagnifiedBackgroundImage.
		each redrawNeeded ].
	Cursor defaultCursor activateCursor.! !

!PasteUpMorph class methodsFor: 'new-morph participation' stamp: 'jmv 12/30/2021 13:11:25'!
categoryInNewMorphMenu
	^ 'Worlds'! !


!WorldMorph methodsFor: 'accessing' stamp: 'jmv 12/30/2021 13:11:33'!
handlesKeyboard

	^ true ! !

!WorldMorph methodsFor: 'alarms' stamp: 'jmv 12/30/2021 13:11:33'!
addAlarm: aSelector withArguments: argArray for: aTarget at: scheduledTime
	"Add a new alarm with the given set of parameters"

	alarms add: 
		(MorphicAlarm 
			receiver: aTarget
			selector: aSelector
			arguments: argArray
			at: scheduledTime)! !

!WorldMorph methodsFor: 'alarms' stamp: 'jmv 12/30/2021 13:11:33'!
adjustAlarmTimes: nowTime
	"Adjust the alarm times after some clock weirdness (such as image-startup etc)"
	| deltaTime |
	deltaTime _ nowTime - lastAlarmTime.
	alarms do: [ :alarm |
		alarm scheduledTime: alarm scheduledTime + deltaTime ]! !

!WorldMorph methodsFor: 'alarms' stamp: 'jmv 12/30/2021 13:11:33'!
alarmSortBlock

	^[ :alarm1 :alarm2 | alarm1 scheduledTime < alarm2 scheduledTime ]! !

!WorldMorph methodsFor: 'alarms' stamp: 'jmv 12/30/2021 13:11:33'!
removeAlarm: aSelector for: aTarget 
	"Remove the alarm with the given selector"

	| alarm |
	alarm _ alarms 
				detect: [ :any | any receiver == aTarget and: [any selector == aSelector]]
				ifNone: nil.
	alarm ifNotNil: [ alarms remove: alarm ]! !

!WorldMorph methodsFor: 'alarms' stamp: 'jmv 12/30/2021 13:11:33'!
triggerAlarmsBefore: nowTime
	"Trigger all pending alarms that are to be executed before nowTime."

	nowTime - lastAlarmTime > 10000
		ifTrue: [ self adjustAlarmTimes: nowTime ].
	[ alarms notEmpty and: [ alarms first scheduledTime < nowTime ]]
		whileTrue: [ alarms removeFirst valueAtTime: nowTime ].
	lastAlarmTime _ nowTime! !

!WorldMorph methodsFor: 'caching' stamp: 'jmv 12/30/2021 13:11:33'!
releaseCachedState
	super releaseCachedState.
	self cleanseStepList.
	self clearCanvas.
	hands do: [ :h | h releaseCachedState ].! !

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 12/30/2021 13:11:33'!
canvas

	^ canvas! !

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 12/30/2021 13:11:33'!
clearCanvas
	canvas _ nil.
	damageRecorder _ nil.! !

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 12/30/2021 13:11:33'!
ensureNonDisplayCanvas
	(canvas isNil or: [
		canvas drawsOnDisplay or: [
		(canvas extent ~= extent) or: [
		canvas form depth ~= Display depth]]]) ifTrue: [
			"allocate a new offscreen canvas the size of the window"
			self setCanvas: (MorphicCanvas withExtent: extent depth: Display depth)]! !

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 12/30/2021 13:11:33'!
recordDamagedRect: damageRect for: aMorph

	damageRecorder ifNotNil: [
		damageRecorder recordInvalidRect: damageRect for: aMorph ]! !

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 12/30/2021 13:11:33'!
resetCanvas
	"To be called in case of possible inconsistency due to an exception during drawing.
	See #displayWorldSafely"

	canvas resetCanvas.
	damageRecorder _ DamageRecorder new.
	self redrawNeeded! !

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 12/30/2021 13:11:33'!
setCanvas: aMorphicCanvas
	canvas _ aMorphicCanvas.
	canvas world: self.
	damageRecorder
		ifNil: [ damageRecorder _ DamageRecorder new].! !

!WorldMorph methodsFor: 'canvas' stamp: 'jmv 12/30/2021 13:11:33'!
setMainCanvas
	"Deallocate before allocating could mean less memory stress."

	self clearCanvas.
	self setCanvas: Display getMainCanvas.
	self restoreDisplay.! !

!WorldMorph methodsFor: 'change reporting' stamp: 'jmv 12/30/2021 13:11:33'!
addedMorph: aMorph
	"Notify the receiver that the given morph was just added."
	super addedMorph: aMorph.
	self taskbar ifNotNil: [ :tb |
		tb wasOpened: aMorph ]! !

!WorldMorph methodsFor: 'change reporting' stamp: 'jmv 12/30/2021 13:11:33'!
invalidateDisplayRect: damageRect for: aMorph
        "Clip damage reports to my bounds, since drawing is _always_ clipped to my bounds."

	self recordDamagedRect: (damageRect intersect: self morphLocalBounds ) for: aMorph! !

!WorldMorph methodsFor: 'change reporting' stamp: 'jmv 12/30/2021 13:11:34'!
removedMorph: aMorph
	"Notify the receiver that aMorph was just removed from its children"
	super removedMorph: aMorph.
	self taskbar ifNotNil: [ :tb |
		tb wasDeleted: aMorph ]! !

!WorldMorph methodsFor: 'classification' stamp: 'jmv 12/30/2021 13:11:34'!
isWorldMorph

	^ true! !

!WorldMorph methodsFor: 'defaul desktop' stamp: 'jmv 12/30/2021 13:11:34'!
recreateDefaultDesktop
	| editor |
	self whenUIinSafeState: [
		self hideTaskbar.
		(submorphs
			select: [ :ea | ea class == SystemWindow or: [ea class == TranscriptWindow]])
				do: [ :ea | ea delete ].
		TranscriptWindow openTranscript
			morphPosition: 5 @ 283;
			morphExtent: 990 @ 400.
		editor _ TextEditor openTextEditor
			morphPosition: 456 @ 3;
			morphExtent: 900 @ 680.
		editor setLabel: 'About Cuis'.
		editor model actualContents: Utilities defaultTextEditorContents.
		Theme current useTaskbar ifTrue: [self showTaskbar].
	].! !

!WorldMorph methodsFor: 'defaul desktop' stamp: 'jmv 12/30/2021 13:11:34'!
tearDownDesktop
	self whenUIinSafeState: [
		self hideTaskbar.
		submorphs 
			do: [ :ea | (ea is: #SystemWindow) ifTrue: [ ea delete ]]].! !

!WorldMorph methodsFor: 'drawing' stamp: 'jmv 12/30/2021 13:11:34'!
displayWorld
	"Update this world's display."

	| deferredUpdateVMMode allDamage |
	self checkIfUpdateNeeded ifFalse: [ ^ self ].  "display is already up-to-date"

	"I (jmv) removed the call to 'deferUpdates: false' below. No more need to call this every time. ?? revisar."
	deferredUpdateVMMode _ self tryDeferredUpdatingAndSetCanvas.

	"Restore world canvas under hands and their carried morphs"
	Preferences cacheDisplayContentWhenMovingMorphs ifTrue: [
		hands do: [ :h | h restoreSavedPatchOn: canvas ]].

	"Update #displayBounds for all dirty morphs (marked as such with #redrawNeeded).
	Also add the updated bounds to aDamageRecorder, and update bounds of morphs carried by hand."
	canvas boundsFinderCanvas updateBoundsIn: self addDamageTo: damageRecorder.

	"repair world's damage on canvas"
	allDamage _ canvas drawWorld: self repair: damageRecorder.
	"allDamage ifNotNil: [Display border: allDamage width: 3 fillColor: Color random].	 'Debugging Aid'."
	canvas newClipRect: nil.

	"Check which hands need to be drawn.
	(they are not the hardware mouse pointer and carry morphs)"
	self handsToDrawForDamage: allDamage do: [ :h |
		Preferences cacheDisplayContentWhenMovingMorphs
			ifTrue: [ allDamage _ (h savePatchFrom: canvas) quickMerge: allDamage ]
			ifFalse: [ allDamage _ h displayFullBoundsForPatch quickMerge: allDamage ].
		canvas fullDrawHand: h .
		h needsRedraw: false ].

	"quickly copy altered rects of canvas to Display:"
	deferredUpdateVMMode ifFalse: [
		allDamage ifNotNil: [
			"Drawing was done to off-Display canvas. Copy content to Display"
			canvas showAt: self viewBox origin invalidRect: allDamage ]].

	"Display deferUpdates: false."
	"Display forceDisplayUpdate"
	allDamage ifNotNil: [
		DisplayScreen isDisplayExtentOk ifTrue: [
			Display forceToScreen: allDamage ]].! !

!WorldMorph methodsFor: 'drawing' stamp: 'jmv 12/30/2021 13:11:34'!
drawOn: aCanvas

	"draw background image."
	backgroundImage
		ifNotNil: [
			aCanvas image: backgroundImage at: `0@0` ]
		ifNil: [
			"draw background fill"
			(aCanvas drawsOnDisplay and: [ color mightBeTranslucent ])
				ifTrue: [
					"Special case so a translucent background on the Display allows you to see through the main Cuis Window.
					Requires proper handling of translucent Display in the VM.
					Seems to work only on Linux when using a composing window manager."
					(BitBlt toForm: Display) clipRect: aCanvas clipRect;
						copy: Display boundingBox
						from: `0@0` in: nil
						fillColor: color rule: Form over.
					Display forceToScreen]
				ifFalse: [ super drawOn: aCanvas ]]! !

!WorldMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 12/30/2021 13:11:34'!
allowsFilesDrop

	^ true! !

!WorldMorph methodsFor: 'dropping/grabbing' stamp: 'jmv 12/30/2021 13:11:34'!
dropFiles: aDropFilesEvent

	(DropFilesAction for: aDropFilesEvent) value! !

!WorldMorph methodsFor: 'errors on draw' stamp: 'jmv 12/30/2021 13:11:34'!
addKnownFailing: aMorph
	drawingFailingMorphs add: aMorph! !

!WorldMorph methodsFor: 'errors on draw' stamp: 'jmv 12/30/2021 13:11:34'!
isKnownFailing: aMorph
	^drawingFailingMorphs includes: aMorph! !

!WorldMorph methodsFor: 'errors on draw' stamp: 'jmv 12/30/2021 13:11:34'!
removeAllKnownFailing
	drawingFailingMorphs do: [ :m | m redrawNeeded ].
	drawingFailingMorphs _ WeakIdentitySet new.
! !

!WorldMorph methodsFor: 'errors on draw' stamp: 'jmv 12/30/2021 13:11:34'!
removeKnownFailing: aMorph
	drawingFailingMorphs remove: aMorph! !

!WorldMorph methodsFor: 'event handling' stamp: 'jmv 12/30/2021 13:11:34'!
mouseButton2Activity

	^self invokeWorldMenu! !

!WorldMorph methodsFor: 'event handling' stamp: 'jmv 12/30/2021 13:11:34'!
wantsWindowEvent: anEvent
	^true! !

!WorldMorph methodsFor: 'events' stamp: 'jmv 12/30/2021 13:11:34'!
click: aMouseButtonEvent localPosition: localEventPosition

	^self mouseButton2Activity.! !

!WorldMorph methodsFor: 'events' stamp: 'jmv 12/30/2021 13:11:34'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event."
	(aKeyboardEvent commandAltKeyPressed or: [ aKeyboardEvent controlKeyPressed ])
		ifTrue: [
			aKeyboardEvent keyCharacter = $b ifTrue: [ BrowserWindow openBrowser ].
			aKeyboardEvent keyCharacter = $f ifTrue: [ BrowserWindow findClass ].
			aKeyboardEvent keyCharacter = $F ifTrue: [ MessageSetWindow findInSourceCode ].
		].
	"aKeyboardEvent keyCharacter print."
	^ super keyStroke: aKeyboardEvent! !

!WorldMorph methodsFor: 'events' stamp: 'jmv 12/30/2021 13:11:34'!
windowEvent: aMorphicEvent

	aMorphicEvent windowEventType == #windowClose
		ifTrue: [
			^TheWorldMenu basicNew quitSession]
! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 12/30/2021 13:11:34'!
basicDisplayBounds
	^ 0@0 extent: extent! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 12/30/2021 13:11:34'!
extentChanged: oldExtent
	"Our extent changed. Must layout submorphs again."

	super extentChanged: oldExtent.
	taskbar ifNotNil: [ taskbar screenSizeChanged ].! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 12/30/2021 13:11:34'!
externalizeBoundsToWorld: aRectangle
	"aRectangle is in own coordinates. Answer is in world coordinates."

	^ aRectangle! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 12/30/2021 13:11:34'!
externalizeDistanceToWorld: aPoint
	"aPoint is a delta in own coordinates. Answer is in world coordinates."

	^ aPoint! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 12/30/2021 13:11:34'!
externalizeToWorld: aPoint
	"aPoint is in own coordinates. Answer is in world coordinates."

	^ aPoint! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 12/30/2021 13:11:34'!
internalizeDistanceFromWorld: aPoint
	"aPoint is a delta in World coordinates. Answer is in own coordinates."

	^ aPoint! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 12/30/2021 13:11:34'!
internalizeFromWorld: aPoint
	"aPoint is in World coordinates. Answer is in own coordinates."

	^ aPoint! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 12/30/2021 13:11:34'!
morphPositionInWorld
	"Answer is in world coordinates."

	^ `0@0`! !

!WorldMorph methodsFor: 'geometry' stamp: 'jmv 12/30/2021 13:11:34'!
viewBox

	^ self morphLocalBounds! !

!WorldMorph methodsFor: 'halos and balloon help' stamp: 'jmv 12/30/2021 13:11:34'!
deleteBalloonTarget: aMorph
	"Delete the balloon help targeting the given morph"
	self handsDo:[:h| h deleteBalloonTarget: aMorph].! !

!WorldMorph methodsFor: 'halos and balloon help' stamp: 'jmv 12/30/2021 13:11:34'!
wantsHaloHandleWithSelector: aSelector inHalo: aHaloMorph
	"Answer whether the receiver would like to offer the halo handle with the given selector (e.g. #addCollapseHandle:)"

	^#(addDebugHandle: addMenuHandle: addHelpHandle:)
		statePointsTo: aSelector! !

!WorldMorph methodsFor: 'hands' stamp: 'jmv 12/30/2021 13:11:34'!
activeHand
	^activeHand! !

!WorldMorph methodsFor: 'hands' stamp: 'jmv 12/30/2021 13:11:34'!
hands

	^ hands! !

!WorldMorph methodsFor: 'hands' stamp: 'jmv 12/30/2021 13:11:34'!
handsDo: aBlock

	^ hands do: aBlock! !

!WorldMorph methodsFor: 'hands' stamp: 'jmv 12/30/2021 13:11:34'!
handsReverseDo: aBlock

	^ hands reverseDo: aBlock! !

!WorldMorph methodsFor: 'hands' stamp: 'jmv 12/30/2021 13:11:34'!
handsToDrawForDamage: aRectangle do: aBlock
	"Select the set of hands that must be redrawn because either (a) the hand itself has changed or (b) the hand intersects some damage rectangle."

	hands do: [: hand |
		hand isDrawnBySoftware ifTrue: [
			hand isRedrawNeeded
				ifTrue: [aBlock value: hand]
				ifFalse: [
					aRectangle ifNotNil: [
						hand displayFullBounds ifNotNil: [ :handBounds |
							(aRectangle intersects: handBounds) ifTrue: [
								aBlock value: hand ]]]]]].! !

!WorldMorph methodsFor: 'initialization' stamp: 'jmv 12/30/2021 13:11:34'!
clearWaitDelay
	waitDelay ifNotNil: [
		waitDelay unschedule.
		waitDelay _ nil ].
	"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
	Might happen, especially on RasPi or other systems without an RTC"
	lastCycleTime _ Time localMillisecondClock.
	lastAlarmTime _ 0.! !

!WorldMorph methodsFor: 'initialization' stamp: 'jmv 12/30/2021 13:14:42'!
initialize

	super initialize.
	activeHand _ HandMorph new.
	hands _ { activeHand }.
	damageRecorder _ DamageRecorder new.
	stepList _ Heap sortBlock: self stepListSortBlock.
	alarms _ Heap sortBlock: self alarmSortBlock.
	lastAlarmTime _ 0.
	deferredUIMessages _ SharedQueue new.
	drawingFailingMorphs _ WeakIdentitySet new.
	pause _ 20.
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ false! !

!WorldMorph methodsFor: 'initialization' stamp: 'jmv 12/30/2021 13:11:34'!
stepListSortBlock

	^ [ :stepMsg1 :stepMsg2 |
		stepMsg1 scheduledTime <= stepMsg2 scheduledTime ]! !

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 12/30/2021 13:11:34'!
mainLoop

	
	self clearWaitDelay.
	canvas isNil ifTrue: [
		self setMainCanvas ].
	self redrawNeeded.
	[
		self doOneCycle.
		true ]
			whileTrue: []! !

!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 12/30/2021 13:11:34'!
runProcess
	
	| process |
	
	process _ [ self mainLoop ] newProcess.
	process
		priority: Processor userSchedulingPriority;
		name: 'Morphic UI';
		animatedUI: self.
	
	^ process! !

!WorldMorph methodsFor: 'menu & halo' stamp: 'jmv 12/30/2021 13:11:34'!
addCustomMenuItems: menu hand: aHandMorph 
	"Add morph-specific menu itemns to the menu for the hand"

	super addCustomMenuItems: menu hand: aHandMorph.
	menu 
		add: 'desktop menu...'
		target: self
		action: #invokeWorldMenu.
	menu addLine! !

!WorldMorph methodsFor: 'menu & halo' stamp: 'jmv 12/30/2021 13:11:34'!
addWorldHaloMenuItemsTo: aMenu hand: aHandMorph
	"Add standard halo items to the menu, given that the receiver is a World"

	| unlockables |
	self addColorMenuItems: aMenu hand: aHandMorph.

"	aMenu addLine.
	self addWorldToggleItemsToHaloMenu: aMenu."
	aMenu addLine.
	self addCopyItemsTo: aMenu.
	self addExportMenuItems: aMenu hand: aHandMorph.

	self addDebuggingItemsTo: aMenu hand: aHandMorph.

	aMenu addLine.
	aMenu defaultTarget: self.

	aMenu addLine.

	unlockables _ self submorphs select:
		[ :m | m isLocked].
	unlockables size = 1 ifTrue: [
		aMenu add: ('unlock "{1}"' format:{unlockables first shortPrintString})action: #unlockContents].
	unlockables size > 1 ifTrue: [
		aMenu add: 'unlock all contents' action: #unlockContents.
		aMenu add: 'unlock...' action: #unlockOneSubpart].

	aMenu defaultTarget: aHandMorph.
! !

!WorldMorph methodsFor: 'misc' stamp: 'jmv 12/30/2021 13:11:34'!
fillRects: rectangleList
	"For testing. Flashes the given list of rectangles on the Display so you can watch incremental redisplay at work."


	| blt screenRect |
	blt _ (BitBlt toForm: Display)
		sourceForm: nil;
		sourceOrigin: `0@0`;
		clipRect: self viewBox;
		combinationRule: Form over.

	rectangleList do: [:r |
		screenRect _ r translatedBy: self viewBox origin.
		blt fillColor: Color random.
		blt destRect: screenRect; copyBits.
		Display forceToScreen: screenRect ].

	(Delay forMilliseconds: 50) wait! !

!WorldMorph methodsFor: 'misc' stamp: 'jmv 12/30/2021 13:11:34'!
restoreDisplay
	self
		morphExtent: Display extent;
		handsDo: [ :h | h visible: true ];
		fullRepaintNeeded! !

!WorldMorph methodsFor: 'object serialization' stamp: 'jmv 12/30/2021 13:11:34'!
objectForDataStream: refStrm
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	| dp |
	dp _ DiskProxy global: #Smalltalk selector: #runningWorld args: #().
	refStrm replace: self with: dp.
	^ dp! !

!WorldMorph methodsFor: 'printing' stamp: 'jmv 12/30/2021 13:11:34'!
printOn: aStream
	"Reimplemented to add a tag showing that the receiver is currently functioning as a 'world', if it is"

	aStream nextPutAll: ' [world]'! !

!WorldMorph methodsFor: 'stepping' stamp: 'jmv 12/30/2021 13:11:34'!
cleanseStepList
	"Remove morphs from the step list that are not in this World."

	| deletions |
	deletions _ OrderedCollection new.
	stepList do: [ :entry |
		entry receiver world == self ifFalse: [
			deletions add: entry]].
	deletions do: [ :entry|
		stepList remove: entry ].

	deletions _ OrderedCollection new.
	alarms do: [ :entry |
		((entry receiver is: #Morph) and: [ entry receiver world == self ]) ifFalse: [
			deletions add: entry]].
	deletions do: [ :entry|
		alarms remove: entry ]! !

!WorldMorph methodsFor: 'stepping' stamp: 'jmv 12/30/2021 13:11:34'!
runLocalStepMethods: nowTime
	"Run morph 'step' methods whose time has come. Purge any morphs that are no longer in this world."

	| stepMessage |
	[ stepList notEmpty and: [ (stepMessage _ stepList first) scheduledTime <= nowTime ]] 
		whileTrue: [
			(stepMessage receiver shouldGetStepsFrom: self)
				ifFalse: [
					stepList remove: stepMessage ifAbsent: []]
				ifTrue: [
					stepMessage valueAtTime: nowTime.
					stepMessage rescheduleAfter: nowTime.
					"We've just updated the  scheduled time for stepMessage.
					It might have been that stepMessage was removed altogether from stepList.
					It also may be the case that stepList got added or removed other elements while on #valueAtTime:
					Just reSort. It will be ok in any case."
					stepList reSort ]].! !

!WorldMorph methodsFor: 'stepping' stamp: 'jmv 12/30/2021 13:11:34'!
runStepMethods
	"Perform periodic activity inbetween event cycles"
	| readyToProcess |

	"Processing the queue until empty is wrong if a block in it calls #addDeferredUIMessage: itself, because this loop will never end.
	Instead, process no more than entries already in queue befor we start iterating!!"
	readyToProcess _ deferredUIMessages size.
	readyToProcess timesRepeat: [
		deferredUIMessages nextOrNil ifNotNil: [ :block |
			block value
		]
	].
	self triggerAlarmsBefore: lastCycleTime.
	self runLocalStepMethods: lastCycleTime.

	"we are using a normal #step for these now"
	"aWorld allLowerWorldsDo: [ :each | each runLocalStepMethods ]."
! !

!WorldMorph methodsFor: 'stepping' stamp: 'jmv 12/30/2021 13:11:34'!
startStepping: aMorph at: scheduledTime selector: aSelector stepTime: stepTimeOrNil
	"Add the given morph to the step list"

	self stopStepping: aMorph selector: aSelector.
	stepList add: (
		StepMessage 
			receiver: aMorph
			selector: aSelector
			at: scheduledTime
			stepTime: stepTimeOrNil)! !

!WorldMorph methodsFor: 'stepping' stamp: 'jmv 12/30/2021 13:11:34'!
stopStepping: aMorph selector: aSelector
	"Remove the given morph from the step list."
	stepList removeAll: (stepList select:[:stepMsg| stepMsg receiver == aMorph and: [ stepMsg selector == aSelector ]])! !

!WorldMorph methodsFor: 'stepping' stamp: 'jmv 12/30/2021 13:11:34'!
stopSteppingMorph: aMorph
	"Remove the given morph from the step list."
	stepList removeAll: (stepList select: [ :stepMsg | stepMsg receiver == aMorph])! !

!WorldMorph methodsFor: 'structure' stamp: 'jmv 12/30/2021 13:11:34'!
topmostWorld
	"Answer the last morph in the owner chain (i.e. the morph without owner) if it is a WorldMorph, or nil."
	owner
		ifNotNil: [ ^owner topmostWorld ].
	^self! !

!WorldMorph methodsFor: 'structure' stamp: 'jmv 12/30/2021 13:11:34'!
world

	^ self! !

!WorldMorph methodsFor: 'submorphs-accessing' stamp: 'jmv 12/30/2021 13:11:34'!
allMorphsDo: aBlock
	"Enumerate all morphs in the world, including those held in hands."

	super allMorphsDo: aBlock.
	self handsReverseDo: [:h | h allMorphsDo: aBlock].! !

!WorldMorph methodsFor: 'submorphs-accessing' stamp: 'jmv 12/30/2021 13:11:34'!
canHandle: aMorph

	^ canvas canDraw: aMorph! !

!WorldMorph methodsFor: 'submorphs-accessing' stamp: 'jmv 12/30/2021 13:11:34'!
firstHand

	^ hands first! !

!WorldMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 12/30/2021 13:11:34'!
addMorph: aMorph centeredNear: aPoint
	"Add the given morph to this world, attempting to keep its center as close to the given point possible while also keeping the it entirely within the bounds of this world."

	| trialRect delta |
	trialRect _ Rectangle center: aPoint extent: aMorph morphExtent.
	delta _ trialRect amountToTranslateWithin: self displayBounds.
	self addMorph: aMorph.
	aMorph morphPositionInWorld: trialRect origin + delta.! !

!WorldMorph methodsFor: 'taskbar' stamp: 'jmv 12/30/2021 13:11:34'!
hideTaskbar
	taskbar ifNotNil: [
		taskbar delete.
		taskbar _ nil ]! !

!WorldMorph methodsFor: 'taskbar' stamp: 'jmv 12/30/2021 13:11:34'!
showTaskbar

	taskbar ifNil: [
		taskbar _ TaskbarMorph newRow.
		taskbar openInWorld: self ]! !

!WorldMorph methodsFor: 'taskbar' stamp: 'jmv 12/30/2021 13:11:34'!
taskbar
	^taskbar! !

!WorldMorph methodsFor: 'taskbar' stamp: 'jmv 12/30/2021 13:11:34'!
taskbarDeleted
	taskbar _ nil! !

!WorldMorph methodsFor: 'testing' stamp: 'jmv 12/30/2021 13:11:34'!
isReallyVisible
	"Answer true only if all the owner chain is visible (i.e. if we are really visible!!)"
	^self visible! !

!WorldMorph methodsFor: 'ui services' stamp: 'jmv 12/30/2021 13:11:34'!
request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock
	"This is deprecated because using it requires blocking the user interface until the user provides a response. Please use the variants that are not modal."
	^ StringRequestMorph request: queryString initialAnswer: defaultAnswer orCancel: cancelBlock! !

!WorldMorph methodsFor: 'ui services' stamp: 'jmv 12/30/2021 13:11:34'!
request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock
	^ StringRequestMorph request: queryString initialAnswer: defaultAnswer verifying: validationBlock do: acceptBlock orCancel: cancelBlock! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 12/30/2021 13:11:34'!
checkForNewScreenSize
	"Check whether the screen size has changed and if so take appropriate actions"

	DisplayScreen isDisplayExtentOk ifFalse: [
		"Minimize the risk of going out of memory:
		- First clear existing canvas, to free the memory it uses.
		- Then, setup the display.
		- Then set up new canvas."
		self clearCanvas.
		DisplayScreen setupDisplay.
		self setMainCanvas.
		self whenUIinSafeState: [ Cursor defaultCursor activateCursor ]].! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 12/30/2021 13:11:34'!
checkIfUpdateNeeded

	self isSubmorphRedrawNeeded ifTrue: [ ^true ].
	damageRecorder updateIsNeeded ifTrue: [^true].
	hands do: [:h | (h isRedrawNeeded | h isSubmorphRedrawNeeded and: [h isDrawnBySoftware]) ifTrue: [^true]].
	^false  "display is already up-to-date"
! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 12/30/2021 13:11:34'!
displayWorldSafely
	"Update this world's display and keep track of errors during draw methods."

	[self displayWorld] on: Error, Halt do: [ :ex |
		"Handle a drawing error"
		canvas currentMorphDrawingFails.
		"Creating a new canvas here could be dangerous, as code signaling the exception will be resumed."
		self resetCanvas.
		"Install the old error handler, so we can re-raise the error"
		ex receiver error: ex description.
	]! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 12/30/2021 13:11:34'!
doOneCycle
	"Do one cycle of the interaction loop. This method is called repeatedly when the world is running.
	
	Make for low cpu usage if the ui is inactive, but quick response when ui is in use.
	However, after some inactivity, there will be a larger delay before the ui gets responsive again."

	| wait waitUntil |
	"
	'Debugging Aid. Declare Delta either as a class variable or as a global variable. Declare delta and r as locals'.
	delta _ Time localMillisecondClock - lastCycleTime.
	r _ 15@1515 extent: 60@30.
	Delta _ Delta ifNil: [ delta ] ifNotNil: [ Delta * 0.9 + (delta * 0.1) ].
	Random next > 0.9 ifTrue: [
		Display fill: r fillColor: Color white.
		(Delta printStringFractionDigits: 1) displayAt: 20@1520.
		Display forceToScreen: r ].
	"
	waitDelay ifNil: [ waitDelay _ Delay forMilliseconds: 50 ].
	(lastCycleHadAnyEvent or: [ deferredUIMessages isEmpty not ])
		ifTrue: [
			pause _ 20.				"This value will only be used later, when there are no more events to serve or deferred UI messages to process."
			wait _ 0.				"Don't wait this time"]
		ifFalse: [
			"wait between 20 and 200 milliseconds"
			(hands anySatisfy: [ :h | h waitingForMoreClicks ])
				ifTrue: [ pause _ 20 ]
				ifFalse: [ pause < 200 ifTrue: [ pause _ pause * 21//20 ] ].
			waitUntil _ lastCycleTime + pause.
			"Earlier if steps"
			stepList isEmpty not ifTrue: [
				waitUntil _ waitUntil min: stepList first scheduledTime ].
			"Earlier if alarms"
			alarms ifNotNil: [
				alarms isEmpty not ifTrue: [
					waitUntil _ waitUntil min: alarms first scheduledTime ]].
			wait _ waitUntil - Time localMillisecondClock max: 0 ].
	Preferences serverMode
		ifTrue: [ wait _ wait max: 50 ].	"Always wait at least a bit on servers, even if this makes the UI slow."
	wait = 0
		ifTrue: [ Processor yield ]
		ifFalse: [
			waitDelay beingWaitedOn
				ifFalse: [ waitDelay setDelay: wait; wait ]
				ifTrue: [
					"If we are called from a different process than that of the main UI, we might be called in the main
					interCyclePause. In such case, use a new Delay to avoid 'This Delay has already been scheduled' errors"
					(Delay forMilliseconds: wait) wait ]].

	"Record start time of this cycle, and do cycle"
	lastCycleTime _ Time localMillisecondClock.
	lastCycleHadAnyEvent _ self doOneCycleNow.! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 12/30/2021 13:11:34'!
doOneCycleNow
	"Immediately do one cycle of the interaction loop.
	Only used for a few tests."
	"See #eventTickler"
	| hadAny |
	Cursor currentCursor = (Cursor cursorAt: #waitCursor) ifTrue: [ Cursor defaultCursor activateCursor ].
	"Repair visual damage."
	self checkForNewScreenSize.
	self displayWorldSafely.
	"Run steps, alarms and deferred UI messages"
	self runStepMethods.
	"Process user input events. Run all event triggered code."
	hadAny _ false.
	self handsDo: [ :h |
		activeHand _ h.
		hadAny _ hadAny | h processEventQueue.
		activeHand _ nil ].
	"The default is the primary hand"
	activeHand _ self hands first.
	^ hadAny.! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 12/30/2021 13:11:34'!
doOneMinimalCycleNow
	"Immediately do one cycle of the interaction loop.
	Only repair display and process events. For modal menus and such."

	"Repair visual damage."
	self displayWorldSafely.

	"Process user input events. Run all event triggered code."
	^activeHand processEventQueue! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 12/30/2021 13:11:34'!
handleFatalDrawingError: errMsg
	"Handle a fatal drawing error."

	self primitiveError: 
		'Fatal Morphic drawing error', String newLineString,
		errMsg.

	"Hm... we should jump into a 'safe' WorldMorph here, but how do we find it?!!"! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 12/30/2021 13:11:34'!
tryDeferredUpdatingAndSetCanvas
        "If this platform supports deferred updates, then make my canvas be the Display (or a rectangular portion of it), set the Display to deferred update mode, and answer true. Otherwise, set a non-Disply canvas and answer false.."
	| properDisplay |
	
	"As this is the only sender of #deferUpdates: , this could be done in Morphic or image startup, and never efterwards..."
	(Display deferUpdates: true) ifNil: [
		"deferred updates not supported by the VM, do them in the image!!"
		self ensureNonDisplayCanvas.
		^ false].
	
	"if no canvas, or canvas was offscreen, from a platform that didnt support defers, then fix it"
	properDisplay _ canvas notNil and: [canvas drawsOnDisplay].
	properDisplay ifFalse: [
		self morphPosition: `0@0` extent: Display extent.
		self setCanvas: Display getMainCanvas.
	].
	^ true! !

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 12/30/2021 13:11:34'!
whenUIinSafeState: evaluableObject
	"Please call
	UISupervisor whenUIinSafeState: evaluableObject
	"
	deferredUIMessages nextPut: evaluableObject! !

!WorldMorph methodsFor: 'windows' stamp: 'jmv 12/30/2021 13:11:34'!
findATranscript
	"Locate a transcript, open it, and bring it to the front.  Create one if necessary"

	self
		findAWindowSatisfying: [ :aWindow | aWindow model == Transcript]
		orMakeOneUsing: [ TranscriptWindow openTranscript ]! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 12/30/2021 13:11:34'!
bringWindowsFullOnscreen
	"Make ever SystemWindow on the desktop be totally on-screen, whenever possible."
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible ]) do: [ :each |
		each makeMeFullyVisible ]! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 12/30/2021 13:11:34'!
closeUnchangedWindows
	"Present a menu of window titles for all windows with changes,
	and activate the one that gets chosen."
	(SelectionMenu confirm: 'Do you really want to close all windows
except those with unaccepted edits?') ifFalse: [ ^ self ].
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible and: [ w canDiscardEdits ]]) do: [ :w |
		w delete ]! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 12/30/2021 13:11:34'!
collapseAll
	"Collapse all windows"
	(SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed not ])
		reverseDo: [ :w | w collapse.  self displayWorld].
	self collapseNonWindows! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 12/30/2021 13:11:34'!
collapseNonWindows
	self allNonWindowRelatedSubmorphs do: [ :m |
		m collapse]! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 12/30/2021 13:11:34'!
deleteNonWindows
	(SelectionMenu confirm:
'Do you really want to discard all objects
that are not in windows?')
		ifFalse: [^ self].

	self allNonWindowRelatedSubmorphs do: [:m |
		m delete ]! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 12/30/2021 13:11:34'!
findAChangeSorter: evt
	"Locate a change sorter, open it, and bring it to the front.  Create one if necessary"
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model isMemberOf: ChangeSorter]
		orMakeOneUsing: [ ChangeSorterWindow open: ChangeSorter new label: nil ]! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 12/30/2021 13:11:34'!
findAFileList: evt
	"Locate a file list, open it, and bring it to the front.
	Create one if necessary, respecting the Preference."
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model class == FileList ]
		orMakeOneUsing: [
			FileListWindow openFileList ]! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 12/30/2021 13:11:34'!
findAMessageNamesWindow: evt
	"Locate a MessageNames tool, open it, and bring it to the front.  Create one if necessary"
	self
		findAWindowSatisfying: [ :aWindow |
			aWindow model class == MessageNames ]
		orMakeOneUsing: [
			MessageNamesWindow open: MessageNames new label: 'Message Names' ]! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 12/30/2021 13:11:34'!
findATranscript: evt
	"Locate a transcript, open it, and bring it to the front.  Create one if necessary"

	self findATranscript! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 12/30/2021 13:11:34'!
findAWindowSatisfying: qualifyingBlock orMakeOneUsing: makeBlock
	"Locate a window satisfying a block, open it, and bring it to the front.  Create one if necessary, by using the makeBlock"
	| aWindow |
	submorphs do: [ :aMorph |
		(((aWindow _ aMorph) is: #SystemWindow) and: [ qualifyingBlock value: aWindow ]) ifTrue: [
			aWindow isCollapsed ifTrue: [ aWindow expand ].
			aWindow activateAndForceLabelToShow.
			^ self ]].
	"None found, so create one"
	makeBlock value.! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 12/30/2021 13:11:34'!
findDirtyBrowsers: evt
	"Present a menu of window titles for browsers with changes,
	and activate the one that gets chosen."
	| menu |
	menu _ MenuMorph new.
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible and: [
				(w model is: #CodeProvider) and: [ w canDiscardEdits not ]]]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activate ].
	menu submorphs notEmpty ifTrue: [ menu popUpInWorld: self ]! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 12/30/2021 13:11:34'!
findDirtyWindows: evt
	"Present a menu of window titles for all windows with changes,
	and activate the one that gets chosen."
	| menu |
	menu _ MenuMorph new.
	(SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible and: [ w canDiscardEdits not ]]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activate ].
	menu submorphs notEmpty ifTrue: [ menu popUpInWorld: self ]! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 12/30/2021 13:11:34'!
findWindow: evt
	"Present a menu names of windows and naked morphs, and activate the one that gets chosen.  Collapsed windows appear below line, expand if chosen; naked morphs appear below second line; if any of them has been given an explicit name, that is what's shown, else the class-name of the morph shows; if a naked morph is chosen, bring it to front and have it don a halo."
	| menu expanded collapsed nakedMorphs |
	menu _ MenuMorph new.
	expanded _ SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed not ].
	collapsed _ SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed ].
	nakedMorphs _ self submorphsSatisfying: [ :m |
		(m is: #SystemWindow) not ].
	expanded isEmpty & (collapsed isEmpty & nakedMorphs isEmpty) ifTrue: [ ^ Smalltalk beep ].
	(expanded asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #activateAndForceLabelToShow.
		w canDiscardEdits ifFalse: [ menu lastItem color: `Color red` ]].
	expanded isEmpty | (collapsed isEmpty & nakedMorphs isEmpty) ifFalse: [ menu addLine ].
	(collapsed asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #expand.
		w canDiscardEdits ifFalse: [ menu lastItem color: `Color red` ]].
	nakedMorphs isEmpty ifFalse: [ menu addLine ].
	(nakedMorphs asArray sort: [ :w1 :w2 |
		w1 label caseInsensitiveLessOrEqual: w2 label ]) do: [ :w |
		menu
			add: w label
			target: w
			action: #comeToFrontAndAddHalo ].
	menu addTitle: 'find window'.
	menu popUpInWorld: self! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 12/30/2021 13:11:34'!
invokeWorldMenu
	"Put up the world menu, triggered by the passed-in event.
	Perhaps a good place to disable it if needed"

	| menu |
	menu _ (TheWorldMenu new 
		world: self
		hand: self activeHand) buildWorldMenu.
	menu addTitle: Preferences desktopMenuTitle.
	menu popUpInWorld: self! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 12/30/2021 13:11:34'!
restoreAll
	"Expand all windows to their previous state"
	taskbar
		ifNotNil: [ :tb | tb restoreAll ]! !

!WorldMorph methodsFor: 'world state' stamp: 'jmv 12/30/2021 13:11:34'!
allNonWindowRelatedSubmorphs
	"Answer all non-window submorphs that are not flap-related"

	^submorphs 
		reject: [ :m | (m is: #SystemWindow) or: [ m is: #TaskbarMorph ] ]! !

!WorldMorph methodsFor: 'world state' stamp: 'jmv 12/30/2021 13:11:34'!
deleteAllHalos
	| c |
	c _ OrderedCollection new.
	self haloMorphsDo: [ :halo | c add: halo ].
	self removeAllMorphsIn: c! !

!WorldMorph methodsFor: 'world state' stamp: 'jmv 12/30/2021 13:11:34'!
fullRepaintNeeded
	self redrawNeeded.
	SystemWindow
		windowsIn: self
		satisfying: [ :w |
			w visible ifTrue: [ w makeMeVisible ].
			false ]! !

!WorldMorph methodsFor: 'world state' stamp: 'jmv 12/30/2021 13:11:34'!
haloMorphsDo: aBlock
	self handsDo: [ :hand | hand halo ifNotNil: [ :halo | aBlock value: halo ]].! !


!WorldMorph class methodsFor: 'instance creation' stamp: 'jmv 12/30/2021 13:11:34'!
newWorld
	"
[
	UISupervisor stopUIProcess.
	UISupervisor spawnNewMorphicProcessFor: WorldMorph newWorld
] fork.
	"
	| w |
	w _ self new.
	w morphPosition: `0@0` extent: Display extent.
	w handsDo: [ :h |
		h privateOwner: w ].
	^w! !


!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 12/30/2021 13:12:38'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	WorldMorph allInstancesDo: [ :w |
		w backgroundImageData: nil.
		w  submorphsDo: [ :a | a delete ]].
	Preferences useNoMenuIcons.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	BitBltCanvas releaseClassCachedState.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	StrikeFont saveSpace.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(SpaceTally DynamicTypingSmalltalkCompleter).
	AppLauncher appGlobalName ifNotNil: [ :any |
		keep add: any ].
	unused := Smalltalk unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		n := Smalltalk removeAllUnSentMessages.
		unused := Smalltalk unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSet zapAllChangeSets.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	Smalltalk garbageCollect.
	oldDicts _ MethodDictionary allInstances.
	newDicts _ Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts _ newDicts _ nil.

	 SmalltalkCompleter initialize .

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   SmalltalkImage current fixObsoleteReferences
   Smalltalk browseAllUnimplementedCalls"! !


!Morph methodsFor: 'structure' stamp: 'jmv 12/30/2021 13:14:10'!
root
	"Return the root of the composite morph containing the receiver. The owner of the root is either nil, a WorldMorph, or a HandMorph. If the receiver's owner is nil, the root is the receiver itself. This method always returns a morph."

	(owner isNil or: [ self isOwnedByWorldOrHand ]) ifTrue: [ ^self ].
	^owner root! !

!Morph methodsFor: 'testing' stamp: 'jmv 12/30/2021 13:13:19'!
isOwnedByHand
	^(owner is: #OldHandMorph) or: [owner is: #HandMorph]! !


!HaloMorph methodsFor: 'handles' stamp: 'jmv 12/30/2021 13:15:01'!
addCollapseHandle: handleSpec
	"Add the collapse handle, with all of its event handlers set up, unless the target's owner is not the world or the hand."

	target owner
		ifNil: [ ^self ]	"nil happens, amazingly"
		ifNotNil: [ :to |
			(to isWorldMorph or: [ to is: #HandMorph ])
				ifFalse: [ ^self ]].
		
	(self addHandle: handleSpec)
		mouseDownSelector: #mouseDownInCollapseHandle:with:;
		mouseMoveSelector: #setDismissColor:with:;
		mouseUpSelector: #maybeCollapse:with:! !


!DamageRecorder methodsFor: 'recording' stamp: 'jmv 12/30/2021 13:15:34'!
damageReportedOther
	"Answer damage reported for no specific morph, for morphs that are not visible, and for morphs that are carried by the Hand.
	Answer might include nils. Skip them."
	| answer possiblyMoreRectsToMerge |
	answer _ OrderedCollection new.
	self pvtAccessProtect critical: [
		damageByRoot keysAndValuesDo: [ :m :r |
			(m visible not or: [m owner isNil or: [m isOwnedByHand ]]) ifTrue: [
				answer add: r]].
		otherDamage do: [ :r | answer add: r ].
	].
	possiblyMoreRectsToMerge _ true.
	[possiblyMoreRectsToMerge] whileTrue: [
		possiblyMoreRectsToMerge _ false.
		answer withIndexDo: [ :r1 :i |
			r1 ifNotNil: [
				i+1 to: answer size do: [ :j | | r2 |
					r2 _ answer at: j.
					r2 ifNotNil: [
						(r1 intersects: r2) ifTrue: [
							r1 updateMerging: r2.
							answer at: j put: nil.
							possiblyMoreRectsToMerge _ true ]]]]]].
	^answer! !

