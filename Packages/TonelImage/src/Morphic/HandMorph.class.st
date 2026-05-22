"
The cursor may be thought of as the HandMorph.  The hand's submorphs hold anything being carried by dragging.  

There is some minimal support for multiple hands in the same world.

grabMorphData is a dictionary of Morph->{prevOwner,prevPos} for morphs being carried.
"
Class {
	#name : 'HandMorph',
	#superclass : 'BoxMorph',
	#instVars : [
		'mouseFocus',
		'keyboardFocus',
		'mouseClickState',
		'mouseOverHandler',
		'lastMouseEvent',
		'hasChanged',
		'savedPatch',
		'lastEventBuffer',
		'lastKeyDownValue',
		'lastMouseEventTime',
		'prevFullBounds',
		'grabMorphData',
		'lastPosition'
	],
	#category : 'Morphic-Kernel',
	#package : 'Morphic',
	#tag : 'Kernel'
}

{ #category : 'new-morph participation' }
HandMorph class >> includeInNewMorphMenu [
	"Not to be instantiated from the menu"
	^ false
]

{ #category : 'focus handling' }
HandMorph >> activateNextWindow [

	self nextFocusWindow ifNotNil: [ :w |
		w activateAndSendTopToBack: true ]
]

{ #category : 'focus handling' }
HandMorph >> activatePreviousWindow [

	self previousFocusWindow ifNotNil: [ :w |
		w activateAndSendTopToBack: false ]
]

{ #category : 'grabbing/dropping' }
HandMorph >> attachMorph: aMorph [
	"Position the center of the given morph under this hand, then grab it.
	This method is used to grab far away or newly created morphs."

	^self grabMorph: aMorph moveUnderHand: true
]

{ #category : 'grabbing/dropping' }
HandMorph >> attachMorphBeside: aMorph [

	"Position the given morph beside this hand, then grab it."

	aMorph aboutToBeGrabbedBy: self.
	self grabMorph: aMorph delta: extent x@0.

]

{ #category : 'balloon help' }
HandMorph >> balloonHelp [
	"Return the balloon morph associated with this hand"
	^self valueOfProperty: #balloonHelpMorph
]

{ #category : 'balloon help' }
HandMorph >> balloonHelp: aBalloonMorph [
	"Return the balloon morph associated with this hand"
	| oldHelp |
	oldHelp := self balloonHelp.
	oldHelp ifNotNil:[oldHelp delete].
	aBalloonMorph
		ifNil:[self removeProperty: #balloonHelpMorph]
		ifNotNil:[self setProperty: #balloonHelpMorph toValue: aBalloonMorph]
]

{ #category : 'geometry' }
HandMorph >> basicDisplayBounds [

	^ self mainBoundsFinderCanvas displayBoundsForHand: self
]

{ #category : 'event handling' }
HandMorph >> createEventFrom: eventBuffer ofType: type [

	type = EventSensor eventTypeMouse ifTrue: [ ^self generateMouseEvent: eventBuffer ].
	type = EventSensor eventTypeMouseScroll ifTrue: [ ^self generateMouseScrollEvent: eventBuffer ].
	type = EventSensor eventTypeKeyboard ifTrue: [ ^self generateKeyboardEvent: eventBuffer ].
	type = EventSensor eventTypeWindow ifTrue: [ ^self generateWindowEvent: eventBuffer ].
	type = EventSensor eventTypeDragDropFiles ifTrue: [ ^self generateDropFilesEvent: eventBuffer ].
		
	"All other events are ignored"
	^nil 
]

{ #category : 'balloon help' }
HandMorph >> deleteBalloon [
	"Delete any existing balloon help."
	
	self balloonHelp: nil
]

{ #category : 'balloon help' }
HandMorph >> deleteBalloonTarget: aMorph [
	"Delete any existing balloon help."
	
	| h |
	h := self balloonHelp ifNil: [^ self].
	h target == aMorph ifTrue: [self deleteBalloon]
]

{ #category : 'events-processing' }
HandMorph >> dispatchMouseOverEvent [
	"Generate and dispatch a MouseOver event.
	If appropriate, also generate and dispatch MouseEnter and MouseLeave events."

	self mouseOverHandler dispatchMouseOverEvent: lastMouseEvent.
]

{ #category : 'geometry' }
HandMorph >> displayFullBoundsForPatchIn: aCanvas [
	"Current full bounds of hand and carried submorphs.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around).
	Integer pixel coordinates!"

	| canvasBounds fullBounds |
	canvasBounds := `0@0` extent: aCanvas extent.
	fullBounds := self basicDisplayBounds.
	self submorphsDo: [ :m |
		m displayFullBounds ifNotNil: [ :b |
			fullBounds := fullBounds quickMerge: b intersectionWith: canvasBounds ]].
	lastPosition
		ifNotNil: [ :lastPos | "When already carrying morphs around. Update rectangle to current hand position."
			fullBounds := fullBounds quickMerge: (fullBounds translatedBy: self morphPosition-lastPos) ].
	^fullBounds encompassingIntegerRectangle
]

{ #category : 'double click support' }
HandMorph >> dontWaitForMoreClicks [
	"Reset the double-click detection state to normal (i.e., not waiting for a double-click).
	This happens after timeout, regardless of multiple clicks having been detected or not."

	mouseClickState := nil.
]

{ #category : 'drawing' }
HandMorph >> drawOn: aCanvas [
	"Draw the hand itself (i.e., the cursor)."
	"This method is only called when we are carrying morphs around..."

	aCanvas drawHand
]

{ #category : 'grabbing/dropping' }
HandMorph >> dropMorph: aMorph event: aMouseEvent [
	"Drop the given morph which was carried by the hand"
	| morphData dropEvent |
	morphData := self grabMorphDataFor: aMorph.
	dropEvent := DropEvent new 
			setPosition: self morphPosition 
			contents: aMorph 
			hand: self
			formerOwner: (morphData at: 1)
			formerPosition: (morphData at: 2).
	dropEvent dispatchIn: owner.
	dropEvent wasHandled ifFalse: [ aMorph rejectDropMorphEvent: dropEvent ].
	self forgetGrabMorphDataFor: aMorph.
]

{ #category : 'grabbing/dropping' }
HandMorph >> dropMorphs: anEvent [
	"Drop the morphs at the hands position"

	self showHardwareCursor.
	self submorphsReverseDo: [ :m |
		"Drop back to front to maintain z-order"
		self dropMorph: m event: anEvent ].
]

{ #category : 'event handling' }
HandMorph >> flushEvents [
	"Flush any events that may be pending"
	self flag: #arNote. "Remove it and fix senders"
	Sensor flushEvents.
]

{ #category : 'private' }
HandMorph >> forgetGrabMorphDataFor: aMorph [

	grabMorphData 
		removeKey: aMorph 
		ifAbsent: nil "no error"
]

{ #category : 'drawing' }
HandMorph >> fullDrawHandOn: aCanvas [
	"A HandMorph has an unusual drawing requirement:
		The hand itself (i.e., the cursor) appears in front of its submorphs
	The illusion is that the hand plucks up morphs and carries them above the world."

	submorphs reverseDo: [ :m | aCanvas fullDraw: m ].
	self drawOn: aCanvas.  "draw the hand itself in front of morphs"
	lastPosition := self morphPosition. "We already know we are carrying morphs."
]

{ #category : 'private events' }
HandMorph >> generateDropFilesEvent: evtBuf  [
	"Generate the appropriate mouse event for the given raw event buffer"

	| position stamp numberOfFiles dragType |
	
	stamp := evtBuf second.
	stamp = 0 ifTrue: [stamp := Time millisecondClockValue]. "VMs report events using #millisecondClockValue"
	dragType := evtBuf third.
	position := evtBuf fourth @ evtBuf fifth.
	numberOfFiles := evtBuf seventh.
	
	^ dragType = 4 ifTrue: [ DropFilesEvent at: position with: numberOfFiles from: self].

]

{ #category : 'private events' }
HandMorph >> generateKeyboardEvent: evtBuf [
	"Generate the appropriate mouse event for the given raw event buffer"
	| buttons modifiers type keyValue pressType stamp mouseScrollDirection |
	stamp := evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time millisecondClockValue ]. "VMs report events using #millisecondClockValue"
	(evtBuf sixth <= 0 or: [ (keyValue := evtBuf sixth) isNil ]) ifTrue: [ keyValue := Character macRomanToLatin1: evtBuf third ].

	modifiers := evtBuf fifth.
	pressType := evtBuf fourth.
	pressType = EventSensor eventKeyDown ifTrue: [
		type := #keyDown.
		lastKeyDownValue := keyValue ].
	pressType = EventSensor eventKeyUp ifTrue: [
		(keyValue = 9 and: [(modifiers anyMask: 1) and: [Smalltalk platformName = 'unix']])
			ifTrue: [
				"Linux VMs don't generate shift-tab keystroke. Turn #keyUp into #keystroke"
				pressType := EventSensor eventKeyChar ]
			ifFalse: [type := #keyUp ]].
	pressType = EventSensor eventKeyChar ifTrue: [
		type := #keystroke.
		"If Control key pressed, and the VM answers a code below 27,
		 it means it did the translation, convert it back to regular character:
		We want to handle the meaning of ctrl ourselves."
		(modifiers anyMask: 2) ifTrue: [ 											"Control key pressed"
			keyValue < 27 ifTrue: [ 										"But we don't want to do it for Home/End/PgUp/PgDn, just for alphabetic keys"
				lastKeyDownValue = keyValue ifFalse: [   									"If equal, real Home/End/PgUp/PgDn in Windows => don't translate"
					(keyValue + 64 = lastKeyDownValue or: [ "If Equal, Ctrl-alphabetic in Windows => do translate"
						lastKeyDownValue < 47 ]) ifTrue: [ 			"Not on windows. If less (not sure about the bound, but do not translate 48: tab on Mac), alphabetic on Mac => do translate"
						keyValue := (modifiers anyMask: 1)
							ifTrue: [ keyValue + 64 ]
							ifFalse: [ keyValue + 96   						"shift not pressed: conver to lowercase letter" ]]]].
			"On Windows, ctrl-backSpace is reported as ctrl-forwardDelete. But keyDown is ok, so we can know and fix."
			(keyValue = 127 and: [ lastKeyDownValue = 8 ])
				ifTrue: [ keyValue := 8 ].
			"Act as if command/alt was pressed for some usual Windows ctrl-key combinations"
			(self shouldControlEmulateAltFor: keyValue) ifTrue: [ modifiers := modifiers bitOr: 8 ]].
		(modifiers anyMask: 8) ifTrue: [ 									"CmdAlt key pressed (or Control key pressed, and #shouldControlEmulateAltFor: just answered true)"
			(modifiers anyMask: 1) ifTrue: [								"Shift pressed"
				| i |
				"It seems that for ctrl-shifted keys and cmd-shifted keys, the VM incorrectly reports the UNSHIFTED character.
				Correct this, at least for common cmd-shortcuts, and for the US keyboard... Sigh...
				(This has only been observed on Mac VMs, but seems harmless if proper shifted character is reported (as in Linux), as this wil be NOP)
				(On Windows, the situation is even worse: ctrl-{ is not even reported as a keystroke event. Only keyDown and keyUp.)"
				"#($' $, $. $9 $0 $[ $]) -> #($'' $< $> $( $) ${) $}"
				i := #[39 44 46 57 48 91 93 ] indexOf: keyValue.
				i > 0 ifTrue: [
					keyValue := #[34 60 62 40 41 123 125] at: i ]]]].
	buttons := modifiers bitShift: 3.
	"Linux and Windows VM send keyboard ctrl-upArrow and ctrl-downArrow when the user tries to scroll using the mouse wheel
	Mac VM sends cmd-option-ctrl-shift-upArrow and cmd-option-ctrl-shift-downArrow for trackpad vertical scroll gestures,
		and cmd-option-ctrl-shift-leftArrow and cmd-option-ctrl-shift-rightArrow for horizontal scroll gestures.
	This way of reporting scroll events by the VM also enables scrolling using the keyboard (actually, we can't tell if user gesture was on Mouse, Trackpad or Keyboard).
	But ctrl-shift and cmdAlt-shift are needed used for selecting while moving by word, line, etc.
	Additionally, #ctrlArrowsScrollHorizontally allows chosing between keyboard horizontal scroll and moving word by word in text editors."
	mouseScrollDirection := nil.
	"Ctrl for Keyboard or Mouse wheel gestures. All modifiers for Trackpad gestures."
	(buttons = InputSensor controlKey or: [buttons = InputSensor cmdAltOptionCtrlShiftModifierKeys]) ifTrue: [
		keyValue = 30
			ifTrue: [mouseScrollDirection := #up]
		ifFalse: [keyValue = 31
			ifTrue: [mouseScrollDirection := #down]]].
	"Ctrl for Keyboard or Mouse wheel gestures, only if preference is set. All modifiers for Trackpad gestures."
	((buttons = InputSensor controlKey and: [Preferences at: #ctrlArrowsScrollHorizontally]) or: [buttons = InputSensor cmdAltOptionCtrlShiftModifierKeys]) ifTrue: [
		keyValue = 28
			ifTrue: [mouseScrollDirection := #left]
		ifFalse: [keyValue = 29
			ifTrue: [mouseScrollDirection := #right]]].
	mouseScrollDirection ifNotNil: [
		^ MouseScrollEvent new
			setPosition: self morphPosition
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
		stamp: stamp.
]

{ #category : 'private events' }
HandMorph >> generateMouseEvent: evtBuf  [
	"Generate the appropriate mouse event for the given raw event buffer"

	| stamp pos buttons oldButtons modifiers oldModifiers buttonsAndModifiers oldButtonsAndModifiers trail |
	stamp := evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time millisecondClockValue ]. "VMs report events using #millisecondClockValue"
	pos := evtBuf third @ evtBuf fourth.
	
	buttons := evtBuf fifth.
	modifiers := evtBuf sixth.
	buttonsAndModifiers := buttons bitOr: (modifiers bitShift: 3).
	oldButtons := lastEventBuffer fifth.
	oldModifiers := lastEventBuffer sixth.
	oldButtonsAndModifiers := oldButtons bitOr: (oldModifiers bitShift: 3).
	
	lastEventBuffer := evtBuf.

	buttons = oldButtons ifTrue: [
		trail := self mouseTrailFrom: evtBuf.
		^MouseMoveEvent new
			setPosition: trail last
			buttons: buttonsAndModifiers
			hand: self
			stamp: stamp].

	^buttons > oldButtons
		ifTrue: [
			MouseButtonEvent new
				mouseDownPosition: pos
				which: (buttonsAndModifiers bitXor: oldButtonsAndModifiers)
				buttons: buttonsAndModifiers
				hand: self
				stamp: stamp ]
		ifFalse: [
			MouseButtonEvent new
				mouseUpPosition: pos
				which: (buttonsAndModifiers bitXor: oldButtonsAndModifiers)
				buttons: buttonsAndModifiers
				hand: self
				stamp: stamp ].
]

{ #category : 'private events' }
HandMorph >> generateMouseScrollEvent: evtBuf [
	"Generate the appropriate mouse scroll event for the given raw event buffer
	Copied from Sqeak 5.3 and modifed."
	| buttons modifiers stamp deltaX deltaY direction oldButtons |
	stamp := evtBuf second.
	stamp = 0 ifTrue: [stamp := Time millisecondClockValue ].
	deltaX := evtBuf third.
	deltaY := evtBuf fourth.
	(deltaX = 0 and: [deltaY = 0]) ifTrue: [^ nil]. "No movement, bail out"
	"For the 'direction', we chooses either horizontal or vertical movement (whichever is greater):"
	direction := deltaY abs >= deltaX abs
		ifTrue: [deltaY negative ifTrue: [#down] ifFalse: [#up]]
		ifFalse: [deltaX negative ifTrue: [#left] ifFalse: [#right]].
	modifiers := evtBuf fifth.
	buttons := (modifiers bitShift: 3) bitOr: (lastMouseEvent buttons bitAnd: 7).
	oldButtons := lastEventBuffer fifth 
		bitOr: (lastEventBuffer sixth bitShift: 3).
	lastEventBuffer := evtBuf.
	^ MouseScrollEvent new
		setPosition: self morphPosition 
		direction: direction
		delta: deltaX @ deltaY "although we have choosen either a horizontal or vertical direction, we make available the original delta because it has legitimate use cases"
		buttons: (oldButtons bitXor: buttons)
		hand: self
		stamp: stamp
]

{ #category : 'private events' }
HandMorph >> generateWindowEvent: evtBuf  [
	"Generate the appropriate window event for the given raw event buffer"

	| evt |
	evt := WindowEvent new.
	evt setTimeStamp: evtBuf second.
	evt timeStamp = 0 ifTrue: [evt setTimeStamp: Time millisecondClockValue]. "VMs report events using #millisecondClockValue"
	evt windowAction: evtBuf third.
	evt rectangle: (Rectangle origin: evtBuf fourth @ evtBuf fifth corner: evtBuf sixth @ evtBuf seventh ).
	
	^evt
]

{ #category : 'grabbing/dropping' }
HandMorph >> grabMorph: aMorph [
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner) without changing its position. This is used to pick up a morph under the hand's current position, versus attachMorph: which is used to pick up a morph that may not be near this hand."

	^self grabMorph: aMorph moveUnderHand: false
]

{ #category : 'grabbing/dropping' }
HandMorph >> grabMorph: aMorph delta: delta [
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner)."

	| formerOwner formerPositionInWorld |
	self releaseMouseFocus. "Break focus"

	"Grab the halo if present"
	self halo ifNotNil: [ :h |
		(h target == aMorph or: [h target hasOwner: aMorph]) ifTrue: [
			self addMorphBack: h position: h positionInWorld - self positionInWorld ]].

	"Remember previous owner and position, in case the drop is later rejected"
	formerOwner := aMorph owner.
	formerOwner ifNotNil: [
		formerPositionInWorld := aMorph positionInWorld.
		grabMorphData
			at: aMorph 
			put: { formerOwner. formerPositionInWorld. } ].
	self addMorphBack: aMorph position: delta.

	aMorph displayBounds ifNotNil: [ :r | | d |
		d := formerPositionInWorld ifNotNil: [aMorph positionInWorld - formerPositionInWorld].
		(d isNil or: [d isZero not]) ifTrue: [
			aMorph adjustDisplayBoundsBy: d ]].

	aMorph justGrabbedFrom: formerOwner.
	formerOwner ifNotNil: [
		"Morph was in the world"
		lastPosition := self morphPosition ].
]

{ #category : 'grabbing/dropping' }
HandMorph >> grabMorph: aMorph moveUnderHand: moveUnderHand [
	"Grab the given morph (i.e., add it to this hand and remove it from its current owner).
	If moveUnderHand is requested or it seems neccesary anyway, move the grabbed morph under the hand."

	| grabbed positionInHandCoordinates tx bounds |
	self releaseMouseFocus.	"Break focus"
	grabbed := aMorph.
	aMorph owner ifNotNil: [ :o | grabbed := o aboutToGrab: aMorph ].
	grabbed ifNil: [ ^ self ].
	grabbed := grabbed aboutToBeGrabbedBy: self.
	grabbed ifNil: [ ^ self ].
	self hideHardwareCursor.
	self redrawNeeded.

	moveUnderHand ifTrue: [
		bounds := grabbed displayBounds ifNil: [grabbed fullBoundsInOwner ].
		positionInHandCoordinates := (bounds center + bounds bottomRight //2) negated + 
			grabbed morphPosition.
		self grabMorph: grabbed delta: positionInHandCoordinates.
		^self ].

	positionInHandCoordinates := (grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph])
		positionInWorld - self positionInWorld.

	tx := GeometryTransformation identity.
	(grabbed isInWorld ifTrue: [grabbed] ifFalse: [aMorph]) withAllOwnersDo: [ :o |
		tx := o location composedWith: tx ].
	self withAllOwnersReverseDo: [ :o |
		tx := o location inverseTransformation composedWith: tx ].
	self grabMorph: grabbed delta: positionInHandCoordinates.

	grabbed location: tx.
]

{ #category : 'private' }
HandMorph >> grabMorphDataFor: aMorph [

	^ grabMorphData at: aMorph ifAbsent: [ { nil. nil. } ]
]

{ #category : 'halos and balloon help' }
HandMorph >> halo [
	"Return the halo associated with this hand, if any"
	^self valueOfProperty: #halo
]

{ #category : 'halo handling' }
HandMorph >> halo: newHalo [
	"Set halo associated with this hand"
	| oldHalo |
	oldHalo := self halo.
	(oldHalo isNil or:[oldHalo == newHalo]) ifFalse:[oldHalo delete].
	newHalo
		ifNil:[self removeProperty: #halo]
		ifNotNil:[self setProperty: #halo toValue: newHalo]
]

{ #category : 'drawing' }
HandMorph >> hideHardwareCursor [

	| blankCursor |
	blankCursor := Cursor cursorAt: #blankCursor.
	Cursor currentCursor == blankCursor ifFalse: [
		blankCursor activateCursor ].
]

{ #category : 'initialization' }
HandMorph >> initForEvents [
	mouseOverHandler := nil.
	lastMouseEventTime := Time localMillisecondClock.
	lastMouseEvent := MouseMoveEvent new
			setPosition: `0@0`
			buttons: 0
			hand: self
			stamp: lastMouseEventTime.
	lastEventBuffer := {1. 0. 0. 0. 0. 0. nil. nil}.
	self dontWaitForMoreClicks.
]

{ #category : 'initialization' }
HandMorph >> initialize [
	super initialize.
	self initForEvents.
	keyboardFocus := nil.
	mouseFocus := nil.
	extent := CursorWithMask defaultCursor extent.
	grabMorphData := IdentityDictionary new.
	self initForEvents.
]

{ #category : 'classification' }
HandMorph >> is: aSymbol [
	^ aSymbol == #HandMorph or: [ super is: aSymbol ]
]

{ #category : 'drawing' }
HandMorph >> isDrawnBySoftware [
	"Return true if this hand must be drawn explicitely instead of being drawn via the hardware cursor.
	Note. We draw the hand as a regular morph (using #drawOn:), disabling the hardware cursor, when we carry submorphs. The reason is to lock the mouse pointer and the carried morph together. Otherwhise the carried morph would lag behind the mouse pointer.
	This method answers whether the regular #drawOn: drawing mechanism is used for us.
	
	Check senders. Hand drawing is handled explicitly by the world, because the Hand is not a submorph of the world!"

	^ submorphs anySatisfy: [ :ea | ea visible ]
]

{ #category : 'testing' }
HandMorph >> isIncludedInTaskbar [
	"Answer true a button for us should be added to any TaskbarMorph."

	^ false
]

{ #category : 'focus handling' }
HandMorph >> keyboardFocus [

	keyboardFocus ifNotNil: [
		keyboardFocus world
			ifNil: [ keyboardFocus := nil ]].
	^ keyboardFocus
]

{ #category : 'focus handling' }
HandMorph >> keyboardFocusNext [

	| m |
	"Right now, only do it within a Window.
	We could add a preference, though.
	See #keyboardFocusPrevious"
	true ifTrue: [
		^self keyboardFocusNextWithinWindow ].

	m := self nextFocusMorph.
	m ifNotNil: [
		m activateWindowAndSendTopToBack: false.
		self newKeyboardFocus: m.
		keyboardFocus keyboardUnfocused.
		m keyboardFocused ].
]

{ #category : 'focus handling' }
HandMorph >> keyboardFocusNextWithinWindow [
	| window |
	keyboardFocus ifNil: [^ false].
	window := keyboardFocus owningWindow.
	(keyboardFocus previousMorphThat: [ :m |
		m visible and: [
			(m handlesKeyboardFocus and: [ m isReallyVisible and: [ m isReallyUnlocked ]])
				and: [m owningWindow = window]]]) ifNotNil: [ :aMorph |
			self newKeyboardFocus: aMorph.
			keyboardFocus keyboardUnfocused.
			aMorph keyboardFocused.
			^true ].
	^false
]

{ #category : 'focus handling' }
HandMorph >> keyboardFocusPrevious [

	| m |
	"Right now, only do it within a Window.
	We could add a preference, though.
	See #keyboardFocusNext"
	true ifTrue: [
		^self keyboardFocusPreviousWithinWindow ].

	m := self previousFocusMorph.
	m ifNotNil: [
		m activateWindowAndSendTopToBack: true.
		self newKeyboardFocus: m.
		keyboardFocus keyboardUnfocused.
		m keyboardFocused ].
]

{ #category : 'focus handling' }
HandMorph >> keyboardFocusPreviousWithinWindow [
	| window |
	keyboardFocus ifNil: [^ false].
	window := keyboardFocus owningWindow.
	(keyboardFocus nextMorphThat: [ :m |
		m visible and: [
			(m handlesKeyboardFocus and: [ m isReallyVisible and: [ m isReallyUnlocked ]])
				and: [m owningWindow = window]]]) ifNotNil: [ :aMorph |
			self newKeyboardFocus: aMorph.
			keyboardFocus keyboardUnfocused.
			aMorph keyboardFocused.
			^true ].
	^false
]

{ #category : 'accessing' }
HandMorph >> lastMouseEvent [
	^ lastMouseEvent
]

{ #category : 'accessing' }
HandMorph >> lastMouseEventTime [
	^ lastMouseEventTime
]

{ #category : 'geometry' }
HandMorph >> morphPosition: aPoint [
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
				prevFullBounds := nil "Any saved patch is no longer relevant"]].
]

{ #category : 'focus handling' }
HandMorph >> mouseFocus [

	mouseFocus ifNotNil: [
		(mouseFocus isWorldMorph or: [mouseFocus isInWorld not])
			ifTrue: [ mouseFocus := nil ]].
	^mouseFocus
]

{ #category : 'accessing' }
HandMorph >> mouseOverHandler [
	^mouseOverHandler ifNil:[mouseOverHandler := MouseOverHandler new].
]

{ #category : 'private events' }
HandMorph >> mouseTrailFrom: currentBuf  [
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
	^trail contents
]

{ #category : 'focus handling' }
HandMorph >> newKeyboardFocus: aMorphOrNil [
	"Make the given morph the new keyboard focus, canceling the previous keyboard focus if any.
	If the argument is nil, the current keyboard focus is cancelled."
	| oldFocus |
	keyboardFocus == aMorphOrNil ifFalse: [
		oldFocus := keyboardFocus.
		keyboardFocus := aMorphOrNil.
		oldFocus ifNotNil: [oldFocus keyboardFocusChange: false].
		aMorphOrNil ifNotNil: [aMorphOrNil keyboardFocusChange: true]].
]

{ #category : 'focus handling' }
HandMorph >> newMouseFocus: aMorphOrNil [
	"Make the given morph the new mouse focus, canceling the previous mouse focus if any. If the argument is nil, the current mouse focus is cancelled."
	mouseFocus := aMorphOrNil.

]

{ #category : 'focus handling' }
HandMorph >> nextFocusMorph [
	"Or nil"

	^(keyboardFocus ifNil: [ self world ])
		previousMorphThat: [ :m |
			m handlesKeyboardFocus and: [ m isReallyVisible and: [ m isReallyUnlocked ]]]
	
	"revisar, probar"
]

{ #category : 'focus handling' }
HandMorph >> nextFocusWindow [
	"Or nil"

	^(SystemWindow topWindow ifNil: [ self world ])
		nextMorphThat: [ :m | 
			(m is: #SystemWindow) and: [ m isReallyVisible ]]
]

{ #category : 'event handling' }
HandMorph >> noticeMouseOver: aMorph event: aMouseOverEvent [
	
	self mouseOverHandler noticeMouseOver: aMorph event: aMouseOverEvent.
]

{ #category : 'objects from disk' }
HandMorph >> objectForDataStream: refStrm [
	"I am about to be written on an object file.  Write a path to me in the other system instead."

	| dp |
	dp := DiskProxy global: #Smalltalk selector: #activeHand args: #().
	refStrm replace: self with: dp.
	^ dp
]

{ #category : 'halo handling' }
HandMorph >> obtainHalo: aHalo [
	"Used for transfering halos between hands"

	self halo == aHalo ifTrue: [^self].
	"Find former owner"
	self world activeHand halo == aHalo ifTrue: [
		self world activeHand releaseHalo: aHalo].
	self halo: aHalo.
]

{ #category : 'paste buffer' }
HandMorph >> pasteMorph [

	| pastee |
	pastee := Clipboard retrieveMorph.
	pastee ifNil: [^ self inform: 'Nothing to paste.'].
	self attachMorph: pastee
]

{ #category : 'focus handling' }
HandMorph >> previousFocusMorph [
	"Or nil"

	^ (keyboardFocus ifNil: [ self world ])
		nextMorphThat: [ :m |
			m handlesKeyboardFocus and: [ m isReallyVisible and: [ m isReallyUnlocked ]]]
	
	"revisar, probar"
]

{ #category : 'focus handling' }
HandMorph >> previousFocusWindow [
	"Or nil"

	^ (SystemWindow topWindow ifNil: [ self world ])
		previousMorphThat: [ :m |
			(m is: #SystemWindow) and: [ m isReallyVisible ]]
]

{ #category : 'event handling' }
HandMorph >> processEventQueue [
	"Process user input events from the local input devices.
	Answer true if any event was handled (but ignore MouseMove)"

	| evt evtBuf type hadAny mcs hadAnyMouseEvent hadAnyMouseMoveEvent |
	mcs := mouseClickState.
	hadAny := false.
	hadAnyMouseEvent := false.
	hadAnyMouseMoveEvent := false.
	[ (evtBuf := Sensor nextEvent) isNil ] whileFalse: [
		type := evtBuf first.
		evt := self createEventFrom: evtBuf ofType: type.
		evt ifNotNil: [
			"Finally, handle it. But process only up to one mouseMove per cycle. Discard the rest."
			(evt isMouseMove and: [ hadAnyMouseMoveEvent ]) ifFalse: [
				evt startDispatchFromHand: self.
				(evt isMouse or: [ evt isKeyboard ]) ifTrue: [
					hadAny := true ].
				evt isMouse ifTrue: [
					hadAnyMouseEvent := true.
					evt isMouseMove ifTrue: [
						hadAnyMouseMoveEvent := true ]]]]].
	hadAnyMouseEvent ifFalse: [
		mcs 
			ifNotNil: [ 
				"No mouse events during this cycle. Make sure click states time out accordingly"
				mcs handleEvent: lastMouseEvent asMouseMove from: self ]].
	^hadAny
]

{ #category : 'focus handling' }
HandMorph >> releaseAllFoci [
	mouseFocus := nil.
	keyboardFocus := nil.

]

{ #category : 'caching' }
HandMorph >> releaseCachedState [
	| oo |
	super releaseCachedState.
	oo := owner.
	self removeAllMorphs.
	self initialize.	"nuke everything"
	self privateOwner: oo.
	self releaseAllFoci.
	savedPatch := nil.
]

{ #category : 'halo handling' }
HandMorph >> releaseHalo: aHalo [
	"Used for transfering halos between hands"
	self removeProperty: #halo
]

{ #category : 'focus handling' }
HandMorph >> releaseKeyboardFocus [
	"Release the current keyboard focus unconditionally"
	self newKeyboardFocus: nil.

]

{ #category : 'focus handling' }
HandMorph >> releaseKeyboardFocusIf: aMorph [
	"If the given morph had the keyboard focus before, release it"
	keyboardFocus == aMorph ifTrue: [self releaseKeyboardFocus]
]

{ #category : 'focus handling' }
HandMorph >> releaseKeyboardFocusIfOrOwnedBy: aMorph [
	"If the given morph had the keyboard focus before, release it"
	keyboardFocus ifNotNil: [
		keyboardFocus withAllOwnersDo: [ :outerOwner |
			outerOwner == aMorph ifTrue: [
				self releaseKeyboardFocus.
				"Try to find a good keyboard focus"
				self dispatchMouseOverEvent ]]]
]

{ #category : 'focus handling' }
HandMorph >> releaseMouseFocus [
	"Release the current mouse focus unconditionally."
	self newMouseFocus: nil.
]

{ #category : 'focus handling' }
HandMorph >> releaseMouseFocus: aMorph [
	"If the given morph had the mouse focus before, release it"
	self mouseFocus == aMorph ifTrue:[self releaseMouseFocus].
]

{ #category : 'private' }
HandMorph >> rememberGrabMorphDataFor: aMorph [

	grabMorphData
		at: aMorph 
		put: { aMorph owner. aMorph positionInWorld }
]

{ #category : 'halo handling' }
HandMorph >> removeHaloFromClick: anEvent on: aMorph [
	| halo |
	halo := self halo ifNil:[^self].
	(halo target hasOwner: self) ifTrue:[^self].
	(halo staysUpWhenMouseIsDownIn: aMorph) ifFalse:[
		halo delete.
		self removeProperty: #halo.
	].
]

{ #category : 'balloon help' }
HandMorph >> removePendingBalloon [
	"Get rid of pending balloon help."

	self removeAlarm: #spawnBalloonFor:.
	self deleteBalloon.
]

{ #category : 'drawing' }
HandMorph >> restoreSavedPatchOn: aCanvas [
	"Clear the changed flag and restore the part of the given canvas under this hand from the previously saved patch."
	
	prevFullBounds ifNotNil: [
		aCanvas restorePatch: savedPatch bounds: prevFullBounds.

		"A full redraw may be needed if any contained morph could draw outside the bounds it had
		when the patch was saved.
		Avoiding if for SystemWindows improves resposiveness of the UI when many windows are open.
		It can be considered safe, as this only assumes that SystemWindows
			- don't resize themselves when carried by the hand
			- don't have protruding submorphs (see #submorphsMightProtrude)"
		(prevFullBounds containsRect: self displayFullBounds) ifFalse: [
			(submorphs allSatisfy: [ :m | m is: #SystemWindow]) ifFalse: [
				self redrawNeeded ]].

		submorphs isEmpty ifTrue: [
			prevFullBounds := nil ]].
]

{ #category : 'drawing' }
HandMorph >> savePatchFrom: aCanvas [
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	| fullBounds answer |
	fullBounds := self displayFullBoundsForPatchIn: aCanvas.
	savedPatch := aCanvas savePatch: savedPatch bounds: fullBounds.
	answer := fullBounds.
	prevFullBounds ifNotNil: [ answer := answer quickMerge: prevFullBounds ].
	prevFullBounds := fullBounds.
	^answer
]

{ #category : 'private events' }
HandMorph >> shouldControlEmulateAltFor: keyValue [
	"At least on Linux Windows, command key is usually ctrl, not alt.
	But not for arrow keys! ctrl-left ~~ alt-left"
	^ keyValue > 32
]

{ #category : 'drawing' }
HandMorph >> showHardwareCursor [

	"Make the transition to using hardware cursor.
	Report one final damage rectangle to erase the image of the software cursor."
	self invalidateDisplayRect: self basicDisplayBounds for: nil.
	Cursor currentCursor == (Cursor cursorAt: #blankCursor) ifTrue: [ 
		"show hardware cursor"
		Cursor defaultCursor activateCursor ].
]

{ #category : 'balloon help' }
HandMorph >> spawnBalloonFor: aMorph [
	aMorph showBalloon
]

{ #category : 'events-processing' }
HandMorph >> startGenericEventDispatch: aMorphicEvent [

	aMorphicEvent dispatchIn: owner.
]

{ #category : 'events-processing' }
HandMorph >> startKeyboardDispatch: aKeyboardEvent [
	(aKeyboardEvent isEsc and: [self hasSubmorphs]) ifTrue: "give an opportunity to cancel drag with esc"
		[self submorphsReverseDo: [:each| each dragCancel].
		self hasSubmorphs ifFalse: [self showHardwareCursor].
		aKeyboardEvent wasHandled: true.
		^ self].
	aKeyboardEvent sendEventTo: (self keyboardFocus ifNil: [self world])
]

{ #category : 'events-processing' }
HandMorph >> startMouseButtonDispatch: aMouseButtonEvent [

	| closedSomeMenu |
	lastMouseEvent := aMouseButtonEvent.	
	lastMouseEventTime := Time localMillisecondClock.
	
	closedSomeMenu := false.
	aMouseButtonEvent mouseButton1Pressed ifTrue: [
		owner submorphsDo: [ :m |
			((m is: #MenuMorph) and: [m activeSubMenu isNil]) ifTrue: [
				(m includesPixel: aMouseButtonEvent eventPosition) ifFalse: [
					closedSomeMenu := m deleteIfPopUp: aMouseButtonEvent ]]].
		closedSomeMenu ifTrue: [
			"Used this click to close some menu(s). Do not further process this event."
			aMouseButtonEvent wasHandled: true.
			^self ]].

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMouseButtonEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			aMouseButtonEvent wasHandled: true.
			^self ]].

	"Issue a synthetic move event if we're not at the position of the event"
	aMouseButtonEvent eventPosition = self morphPosition ifFalse: [
		"Issue a mouse move event to make the receiver appear at the given position"
		(MouseMoveEvent new
			setPosition: aMouseButtonEvent eventPosition
			buttons: aMouseButtonEvent buttons
			hand: self
			stamp: aMouseButtonEvent timeStamp) startDispatchFromHand: self ].

	"Drop submorphs on button events"
	self hasSubmorphs
		ifTrue: [
			"Not if we are grabbing them"
			mouseClickState ifNil: [
				"Want to drop on mouseUp, NOT mouseDown"
				aMouseButtonEvent isMouseUp ifTrue: [
					self dropMorphs: aMouseButtonEvent ]]]
		ifFalse: [
			(self mouseFocus notNil and: [ aMouseButtonEvent mouseButton3Changed not ])
				"Don't forward halo events to mouseFocus morph"
				ifTrue: [
					mouseFocus handleMouseFocusEvent: aMouseButtonEvent ]
				ifFalse: [
					aMouseButtonEvent dispatchIn: owner ]].

	self hasSubmorphs ifFalse: [
		self dispatchMouseOverEvent ].
]

{ #category : 'events-processing' }
HandMorph >> startMouseDispatch: aMouseEvent [
	"For MouseEvents with default dispatch behavior"

	aMouseEvent dispatchIn: owner
]

{ #category : 'events-processing' }
HandMorph >> startMouseMoveDispatch: aMouseEvent [

	lastMouseEvent := aMouseEvent.	
	lastMouseEventTime := Time localMillisecondClock.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		mouseClickState handleEvent: aMouseEvent from: self ].

	self morphPosition: aMouseEvent eventPosition.
	self mouseFocus
		ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
		ifNil: [ aMouseEvent dispatchIn: owner ].

	self dispatchMouseOverEvent
]

{ #category : 'geometry testing' }
HandMorph >> submorphsMightProtrude [
	"Morphs we carry are usually larger than us."

	^true
]

{ #category : 'balloon help' }
HandMorph >> triggerBalloonFor: aMorph after: timeOut [
	"Trigger balloon help after the given time out for some morph"
	self addAlarm: #spawnBalloonFor: with: aMorph after: timeOut.
]

{ #category : 'double click support' }
HandMorph >> waitForClicksOrDrag: aMorph event: evt clkSel: clkSel dblClkSel: dblClkSel [

	"Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods." 

	mouseClickState :=
		MouseClickState new
			client: aMorph
			click: clkSel
			dblClick: dblClkSel
			event: evt
]

{ #category : 'double click support' }
HandMorph >> waitForClicksOrDrag: aMorph event: evt dragSel: dragSel clkSel: clkSel [

	"Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods." 

	mouseClickState :=
		MouseClickState new
			client: aMorph
			drag: dragSel
			click: clkSel
			event: evt
]

{ #category : 'double click support' }
HandMorph >> waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel [

	"Wait until the difference between click, or drag gesture is known, then inform the given morph what transpired." 

	mouseClickState :=
		MouseClickState new
			client: aMorph
			click: clkSel
			event: evt
			sendMouseButton2Activity: (Preferences at: #tapAndHoldEmulatesButton2).

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"
]

{ #category : 'double click support' }
HandMorph >> waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel clkNHalf: clkNHalfSel dblClkSel: dblClkSel dblClkNHalfSel: dblClkNHalfSel tripleClkSel: tripleClkSel [

	"Wait until the difference between click, double-click, or drag gesture is known, then inform the given morph what transpired. This message is sent when the given morph first receives a mouse-down event. If the mouse button goes up, then down again within DoubleClickTime, then 'doubleClick: evt' is sent to the morph. If the mouse button goes up but not down again within DoubleClickTime, then the message 'click: evt' is sent to the morph. Finally, if the button does not go up within DoubleClickTime, then 'drag: evt' is sent to the morph. In all cases, the event supplied is the original mouseDown event that initiated the gesture. mouseMove: and mouseUp: events are not sent to the morph until it becomes the mouse focus, which is typically done by the client in its click:, doubleClick:, or drag: methods." 

	mouseClickState :=
		MouseClickState new
			client: aMorph
			click: clkSel
			clickAndHalf: clkNHalfSel
			dblClick: dblClkSel
			dblClickAndHalf: dblClkNHalfSel
			tripleClick: tripleClkSel
			event: evt
			sendMouseButton2Activity: (Preferences at: #tapAndHoldEmulatesButton2).

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"
]

{ #category : 'double click support' }
HandMorph >> waitForClicksOrDragOrSimulatedMouseButton2: aMorph event: evt clkSel: clkSel clkNHalf: clkNHalfSel dblClkSel: dblClkSel dblClkNHalfSel: dblClkNHalfSel tripleClkSel: tripleClkSel dragSel: dragSel [
	
	mouseClickState :=
		MouseClickState new
			client: aMorph
			drag: dragSel
			click: clkSel
			clickAndHalf: clkNHalfSel
			dblClick: dblClkSel
			dblClickAndHalf: dblClkNHalfSel
			tripleClick: tripleClkSel
			event: evt
			sendMouseButton2Activity: (Preferences at: #tapAndHoldEmulatesButton2).

	"It seems the Mac VM may occasionally lose button up events triggering bogus activations.
	Hence Preferences tapAndHoldEmulatesButton2"
]

{ #category : 'double click support' }
HandMorph >> waitingForMoreClicks [
	"Answer true "

	^mouseClickState notNil
]
