'From Cuis7.5 [latest update: #7495] on 3 September 2025 at 8:14:02 pm'!
!classDefinition: #MouseEnterEvent category: #'Morphic-Events'!
MouseEvent subclass: #MouseEnterEvent
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!
!classDefinition: #MouseLeaveEvent category: #'Morphic-Events'!
MouseEvent subclass: #MouseLeaveEvent
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!
!classDefinition: #MouseOverEvent category: #'Morphic-Events'!
MouseEvent subclass: #MouseOverEvent
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Events'!

!MouseButtonEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 16:18:55'!
mouseDownPosition: evtPos which: changedButtonsAndModifiers buttons: evtButtons hand: evtHand stamp: stamp
	type := #mouseDown.
	position := evtPos.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	whichButton := changedButtonsAndModifiers.
	timeStamp := stamp.! !

!MouseButtonEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 16:19:01'!
mouseUpPosition: evtPos which: changedButtonsAndModifiers buttons: evtButtons hand: evtHand stamp: stamp
	type := #mouseUp.
	position := evtPos.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	whichButton := changedButtonsAndModifiers.
	timeStamp := stamp.! !


!MouseEnterEvent methodsFor: 'dispatching' stamp: 'jmv 9/3/2025 15:58:35'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph.
	See #asMouseEnter, #asMouseLeave, #asMouseMove, #asMouseOver."

	type == #mouseEnter ifTrue: [
		aMorph processMouseEnter: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].

	"Backstop. Just in case."
	super sendEventTo: aMorph.! !

!MouseEnterEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 15:55:53'!
setPosition: evtEnd buttons: evtButtons hand: evtHand stamp: stamp

	type := #mouseEnter.
	position := evtEnd.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	timeStamp := stamp.! !


!MouseLeaveEvent methodsFor: 'dispatching' stamp: 'jmv 9/3/2025 15:58:18'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph.
	See #asMouseEnter, #asMouseLeave, #asMouseMove, #asMouseOver."

	type == #mouseLeave ifTrue: [
		aMorph processMouseLeave: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].

	"Backstop. Just in case."
	super sendEventTo: aMorph.! !

!MouseLeaveEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 15:55:43'!
setPosition: evtEnd buttons: evtButtons hand: evtHand stamp: stamp

	type := #mouseLeave.
	position := evtEnd.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	timeStamp := stamp.! !


!MouseMoveEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 15:10:50'!
setPosition: evtEnd buttons: evtButtons hand: evtHand stamp: stamp

	type := #mouseMove.
	position := evtEnd.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	timeStamp := stamp.! !


!MouseOverEvent methodsFor: 'dispatching' stamp: 'jmv 9/3/2025 16:00:39'!
sendEventTo: aMorph
	"Dispatch the receiver into aMorph.
	See #asMouseEnter, #asMouseLeave, #asMouseMove, #asMouseOver."

	type == #mouseOver ifTrue: [
		aMorph processMouseOver: self localPosition: (aMorph internalizeFromWorld: position).
		^self ].

	"Backstop. Just in case."
	super sendEventTo: aMorph.! !

!MouseOverEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 16:00:07'!
setPosition: evtEnd buttons: evtButtons hand: evtHand stamp: stamp

	type := #mouseOver.
	position := evtEnd.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	timeStamp := stamp.! !


!MouseScrollEvent methodsFor: 'private' stamp: 'jmv 9/3/2025 15:12:53'!
setPosition: evtPos direction: evtDir buttons: evtButtons hand: evtHand stamp: stamp
	type := #mouseScroll.
	position := evtPos.
	buttons := evtButtons.
	source := evtHand.
	wasHandled := false.
	direction := evtDir.
	timeStamp := stamp.! !


!MouseEvent methodsFor: 'converting' stamp: 'jmv 9/3/2025 15:56:53'!
asMouseEnter

	^ MouseEnterEvent new
		setPosition: position
		buttons: buttons
		hand: source
		stamp: Time millisecondClockValue "VMs report events using #millisecondClockValue"! !

!MouseEvent methodsFor: 'converting' stamp: 'jmv 9/3/2025 15:56:41'!
asMouseLeave

	^ MouseLeaveEvent new
		setPosition: position
		buttons: buttons
		hand: source
		stamp: Time millisecondClockValue "VMs report events using #millisecondClockValue"! !

!MouseEvent methodsFor: 'converting' stamp: 'jmv 9/3/2025 15:11:12'!
asMouseMove

	^ MouseMoveEvent new
		setPosition: position
		buttons: buttons
		hand: source
		stamp: Time millisecondClockValue "VMs report events using #millisecondClockValue"! !

!MouseEvent methodsFor: 'converting' stamp: 'jmv 9/3/2025 16:01:47'!
asMouseOver
	"Convert the receiver into a mouse over event"

	^ MouseOverEvent new
		setPosition: position
		buttons: buttons
		hand: source
		stamp: Time millisecondClockValue "VMs report events using #millisecondClockValue"! !


!HandMorph methodsFor: 'events-processing' stamp: 'jmv 9/3/2025 15:11:52'!
startMouseDispatch: aMouseEvent

	aMouseEvent isMouseOver ifTrue: [
		self mouseFocus
			ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
			ifNil: [ aMouseEvent dispatchIn: owner ].
		^self].

	"any mouse event but mouseOver"
	lastMouseEvent := aMouseEvent.	
	lastMouseEventTime := Time localMillisecondClock.

	"Check for pending drag or double click operations."
	mouseClickState ifNotNil: [
		(mouseClickState handleEvent: aMouseEvent from: self) ifTrue: [
			"Possibly dispatched #click: or something. Do not further process this event."
			self mouseOverHandler processMouseOver: lastMouseEvent.
			^self ]].

	aMouseEvent isMove
		ifTrue: [
			self morphPosition: aMouseEvent eventPosition.
			self mouseFocus
				ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
				ifNil: [ aMouseEvent dispatchIn: owner ]
		] ifFalse: [
		aMouseEvent isMouseScroll ifTrue: [
			aMouseEvent dispatchIn: owner
		] ifFalse: [
			"Issue a synthetic move event if we're not at the position of the event"
			aMouseEvent eventPosition = self morphPosition ifFalse: [
				"Issue a mouse move event to make the receiver appear at the given position"
				self startMouseDispatch: (MouseMoveEvent new
					setPosition: aMouseEvent eventPosition
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
						ifNotNil: [ mouseFocus handleMouseFocusEvent: aMouseEvent ]
						ifNil: [ aMouseEvent dispatchIn: owner ]]]].
		self mouseOverHandler processMouseOver: lastMouseEvent.! !

!HandMorph methodsFor: 'initialization' stamp: 'jmv 9/3/2025 15:46:40'!
initForEvents
	mouseOverHandler := nil.
	lastMouseEventTime := Time localMillisecondClock.
	lastMouseEvent := MouseMoveEvent new
			setPosition: `0@0`
			buttons: 0
			hand: self
			stamp: lastMouseEventTime.
	lastEventBuffer := {1. 0. 0. 0. 0. 0. nil. nil}.
	self dontWaitForMoreClicks.! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 9/3/2025 15:13:07'!
generateKeyboardEvent: evtBuf
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
		stamp: stamp.! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 9/3/2025 15:41:30'!
generateMouseEvent: evtBuf 
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
				stamp: stamp ].! !

!HandMorph methodsFor: 'private events' stamp: 'jmv 9/3/2025 15:13:16'!
generateMouseScrollEvent: evtBuf
	"Generate the appropriate mouse wheel event for the given raw event buffer
	
	Copied from Sqeak 5.3 and modifed."

	| buttons modifiers stamp deltaX deltaY direction oldButtons |
	stamp := evtBuf second.
	stamp = 0 ifTrue: [stamp := Time millisecondClockValue ].
	deltaX := evtBuf third.
	deltaY := evtBuf fourth.
	"This implementation deliberatly ignores movements in both dimensions:"
	direction := 
		deltaY negative
			ifTrue: [ #down ]
			ifFalse: [ deltaY strictlyPositive
				ifTrue: [ #up ]
				ifFalse: [ deltaX negative
					ifTrue: [ #left ]
					ifFalse: [ deltaX strictlyPositive
						ifTrue: [ #right ]
						ifFalse: [ ^ nil "No movement, bailing out" ] ] ] ].
	modifiers := evtBuf fifth.
	buttons := (modifiers bitShift: 3) bitOr: (lastMouseEvent buttons bitAnd: 7).
	oldButtons := lastEventBuffer fifth 
		bitOr: (lastEventBuffer sixth bitShift: 3).
	lastEventBuffer := evtBuf.
	^ MouseScrollEvent new
		setPosition: self morphPosition 
		direction: direction
		buttons: (oldButtons bitXor: buttons)
		hand: self
		stamp: stamp! !

!methodRemoval: MouseScrollEvent #setType:position:direction:buttons:hand:stamp: stamp: 'jmv 9/3/2025 20:10:55'!
MouseScrollEvent removeSelector: #setType:position:direction:buttons:hand:stamp:!
!methodRemoval: MouseMoveEvent #setType:position:buttons:hand:stamp: stamp: 'jmv 9/3/2025 20:10:55'!
MouseMoveEvent removeSelector: #setType:position:buttons:hand:stamp:!
!methodRemoval: MouseButtonEvent #setType:position:which:buttons:hand:stamp: stamp: 'jmv 9/3/2025 20:10:55'!
MouseButtonEvent removeSelector: #setType:position:which:buttons:hand:stamp:!
!methodRemoval: MouseEvent #setType: stamp: 'jmv 9/3/2025 20:10:55'!
MouseEvent removeSelector: #setType:!
!methodRemoval: MouseEvent #setType:position:buttons:hand: stamp: 'jmv 9/3/2025 20:10:55'!
MouseEvent removeSelector: #setType:position:buttons:hand:!
!methodRemoval: MouseEvent #isMouseEnter stamp: 'jmv 9/3/2025 20:10:55'!
MouseEvent removeSelector: #isMouseEnter!
!methodRemoval: MouseEvent #isMouseLeave stamp: 'jmv 9/3/2025 20:10:55'!
MouseEvent removeSelector: #isMouseLeave!
