'From Cuis 5.0 of 7 November 2016 [latest update: #3201] on 27 October 2017 at 9:58:10 am'!

!MouseScrollEvent commentStamp: '<historical>' prior: 0!
A MouseScrollEvent can be any type of secondary pointer movement (typically via a scroll wheel on a traditional mouse or a gesture on a trackpad).  Currently, events are extracted from KeyboardEvents (which is how the VM currently communicates things like scroll wheel events via ctl+arrow up/down).

It is also possible to generate these events with a keyboard, pressing ctrl-down or ctrl-up. Given this, we also added ctrl-left and ctrl-right, that can only be generated with a keyboard, to control horizontal scroll.!


!PluggableScrollPane methodsFor: 'events' stamp: 'jmv 10/27/2017 09:56:48'!
mouseScroll: aMouseEvent localPosition: localEventPosition
	aMouseEvent direction
		caseOf: {
			[ #up ] 		-> 		[  scrollBar scrollUp: 1 ].
			[ #down ] 	-> 		[ scrollBar scrollDown: 1 ].
			[ #left ] 		-> 		[  hScrollBar scrollUp: 1 ].
			[ #right ] 	-> 		[  hScrollBar scrollDown: 1 ] }! !

!PluggableScrollPane methodsFor: 'event handling testing' stamp: 'jmv 10/27/2017 09:47:51'!
handlesMouseScroll: aMouseScrollEvent
	"Only accept if we can actually do something useful with the event (i.e. not scrolling up when already at the top or down when already at the bottom) or if my owner chain doesn't want it"

	(aMouseScrollEvent direction = #up and: [ scrollBar scrollValue > 0 ])
		ifTrue: [ ^ true ].
	(aMouseScrollEvent direction = #down and: [ scrollBar scrollValue < 1 ])
		ifTrue: [ ^ true ].
	(aMouseScrollEvent direction = #left and: [ hScrollBar scrollValue > 0 ])
		ifTrue: [ ^ true ].
	(aMouseScrollEvent direction = #right and: [ hScrollBar scrollValue < 1 ])
		ifTrue: [ ^ true ].
	"Even if I don't want it, one of my owners might. (i.e. nested scroll panes)  If my owners don't want it, accept the event to make sure that morphs behind me doesn't get the event."
	(self privateAnyOwnerHandlesMouseScroll: aMouseScrollEvent)
		ifFalse: [ ^ true ].
	^ false! !


!HandMorph methodsFor: 'private events' stamp: 'jmv 10/27/2017 09:57:46'!
generateKeyboardEvent: evtBuf
	"Generate the appropriate mouse event for the given raw event buffer"
	| buttons modifiers type keyValue pressType stamp |
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
	pressType = EventSensor eventKeyUp ifTrue: [ type _ #keyUp ].
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
	(modifiers = 2 and: [
		keyValue between: 28 and: 31])
		ifTrue: [ "We're hijacking ctl-up/down arrow since that is what the VM sends us for scroll events.  We're going to convert these to MouseScrollEvent and throw away the keyboard event"
			"ThisPrecludes the use of up and down arrows with control, that are standard keystrokes in Windows to control the cursor.
			But as Linux, Mac and Windows VMs generate ctrl-up and ctrl-down for mouse wheel events, we must honor them.
			Some day, it would be good for the VMs to report mouse wheel events differently fom ctrl-up & ctrl-down..."
			"Also do ctrl-left and ctrl-right for horizontal scroll."
			^ MouseScrollEvent new
				setType: #mouseScroll
				position: self morphPosition
				direction:
					(keyValue caseOf: {
						"Implementing as a caseof to allow for easy adding of left/right/other events should they become available"
						[ 30 ] -> [ #up ].
						[ 31 ] -> [ #down ].
						[ 28 ] -> [ #left ].
						[ 29 ] -> [ #right ].
						})
				buttons: buttons
				hand: self
				stamp: stamp ]
		ifFalse: [ ^ KeyboardEvent new
				setType: type
				buttons: buttons
				position: self morphPosition
				keyValue: keyValue
				hand: self
				stamp: stamp ].! !


!MouseEvent methodsFor: 'comparing' stamp: 'jmv 10/27/2017 09:42:20'!
hash
	^ type hash bitXor: (position hash bitXor: buttons hash)! !


!MouseMoveEvent methodsFor: 'comparing' stamp: 'jmv 10/27/2017 09:42:03'!
hash
	^ position hash bitXor: buttons hash! !


!MouseScrollEvent methodsFor: 'comparing' stamp: 'jmv 10/27/2017 09:41:31'!
hash
	^ position hash bitXor: (buttons hash bitXor: direction hash)! !

