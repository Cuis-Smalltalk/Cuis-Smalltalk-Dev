'From Cuis 5.0 of 7 November 2016 [latest update: #3729] on 8 May 2019 at 9:12:09 am'!

!InputSensor class methodsFor: 'constants' stamp: 'jmv 5/8/2019 08:48:48'!
cmdAltOptionCtrlShiftModifierKeys
	"Include all of them"
	^ 16r78	"cmd | opt | ctrl | shft "! !


!Preferences class methodsFor: 'standard queries' stamp: 'jmv 5/8/2019 08:15:39'!
ctrlArrowsScrollHorizontally
	"Enables / disables Ctrl-ArrowKeys horizontal scroll.
	By default it is false, to enable ctrl-leftArrow and ctrl-rightArrow to move cursor word by word in text editors."
	^ self
		valueOfFlag: #ctrlArrowsScrollHorizontally
		ifAbsent: [ false ]! !


!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/1/2019 12:55:36'!
arrowKey: aKeyboardEvent

	"Handle a keyboard navigation event. Answer nil if not handled."
	aKeyboardEvent isArrowUp ifTrue: [ ^ self navigateUp ].
	aKeyboardEvent isArrowDown ifTrue: [ ^ self navigateDown ].
	aKeyboardEvent isArrowLeft ifTrue: [ ^ self navigateLeft ].
	aKeyboardEvent isArrowRight ifTrue: [ ^ self navigateRight ].
	aKeyboardEvent isHome ifTrue: [ ^ self navigateToTop ].
	aKeyboardEvent isEnd ifTrue: [ ^ self navigateToBottom ].
	aKeyboardEvent isPageUp ifTrue: [ ^ self navigateOnePageUp ].
	aKeyboardEvent isPageDown ifTrue: [ ^ self navigateOnePageDown ].
	^ nil! !


!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 5/8/2019 09:09:27'!
arrowKey: aKeyboardEvent

	"Handle a keyboard navigation event. Answer nil if not handled."
	aKeyboardEvent isArrowUp ifTrue: [ ^ self navigateUp ].
	aKeyboardEvent isArrowDown ifTrue: [ ^ self navigateDown ].
	aKeyboardEvent isArrowLeft ifTrue: [ ^ self navigateLeft ].
	aKeyboardEvent isArrowRight ifTrue: [ ^ self navigateRight ].
	aKeyboardEvent isHome ifTrue: [ ^ self navigateToTop ].
	aKeyboardEvent isEnd ifTrue: [ ^ self navigateToBottom ].
	aKeyboardEvent isPageUp ifTrue: [ ^ self navigateOnePageUp ].
	aKeyboardEvent isPageDown ifTrue: [ ^ self navigateOnePageDown ].
	^ nil! !


!HandMorph methodsFor: 'private events' stamp: 'jmv 5/8/2019 09:03:50'!
generateKeyboardEvent: evtBuf
	"Generate the appropriate mouse event for the given raw event buffer"
	| buttons modifiers type keyValue pressType stamp mouseScrollDirection |
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [ stamp _ Time localMillisecondClock ].
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
		(modifiers anyMask: 2) ifTrue: [
			"Control key pressed"
			keyValue < 27 ifTrue: [
				"But we don't want to do it for Home/End/PgUp/PgDn, just for alphabetic keys"
				lastKeyDownValue = keyValue ifFalse: [
					"If equal, real Home/End/PgUp/PgDn in Windows => don't translate"
					(keyValue + 64 = lastKeyDownValue or: [ "If Equal, Ctrl-alphabetic in Windows => do translate"
						lastKeyDownValue < 47 ]) ifTrue: [
						"Not on windows. If less (not sure about the bound, but do not translate 48: tab on Mac), alphabetic on Mac => do translate"
						keyValue _ (modifiers anyMask: 1)
							ifTrue: [ keyValue + 64 ]
							ifFalse: [ keyValue + 96
								"shift not pressed: conver to lowercase letter" ]]]].
			"Act as if command/alt was pressed for some usual Windows ctrl-key combinations"
			(self shouldControlEmulateAltFor: keyValue) ifTrue: [ modifiers _ modifiers bitOr: 8 ]]].
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

!methodRemoval: TextEditor #scrollBy:!
TextEditor removeSelector: #scrollBy:!
