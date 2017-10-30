'From Cuis 5.0 of 7 November 2016 [latest update: #3202] on 30 October 2017 at 10:37:01 am'!

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 10/30/2017 10:36:16'!
ctrlArrowsScrollHorizontally
	^ self
		valueOfFlag: #ctrlArrowsScrollHorizontally
		ifAbsent: [ false ]! !


!HandMorph methodsFor: 'private events' stamp: 'jmv 10/30/2017 10:35:34'!
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
		keyValue
			between: (Preferences ctrlArrowsScrollHorizontally ifTrue: [28] ifFalse: [30])
			and: 31])
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

