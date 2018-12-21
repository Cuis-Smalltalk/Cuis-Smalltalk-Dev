'From Cuis 5.0 of 7 November 2016 [latest update: #3538] on 21 December 2018 at 3:11:09 pm'!

!TextEditor methodsFor: 'editing keys' stamp: 'AY 12/17/2018 17:05:42'!
tabKey: aKeyboardEvent
	"Add/remove a tab at the front of every line occupied by the selection if there is one; treat as a normal character otherwise."
	
	aKeyboardEvent shiftPressed 
		ifTrue: [ ^ self outdent: aKeyboardEvent ].
	
	^ self hasSelection 
		ifTrue: [ self indent: aKeyboardEvent ]
		ifFalse: [ self normalCharacter: aKeyboardEvent ]! !


!TextEditor class methodsFor: 'class initialization' stamp: 'AY 12/21/2018 13:10:43'!
initializeShortcuts
	
	super initializeShortcuts.
	shortcuts at: 9 + 1 put: #tabKey:.! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 12/21/2018 08:09:38'!
basicCmdShortcutsSpec
	"
	Editor initialize
	"

	"arranged in QWERTY keyboard order"
	^#(
		#(		$( 	#enclose:					'Enclose within ( and ), or remove enclosing ( and )')
		#(		$[ 	#enclose:					'Enclose within [ and ], or remove enclosing [ and ]')
		#(		${ 	#enclose:					'Enclose within { and }, or remove enclosing { and }')
		#(		$' 	#enclose:					'Enclose within single quotes, or remove enclosing single quotes')
		#(		$" 	#enclose:					'Enclose within double quotes, or remove enclosing double quotes')
		#(		$` 	#enclose:					'Enclose within backticks, or remove enclosing backticks')
		#(		$< 	#enclose:					'Enclose within < and >, or remove enclosing < and >')

		#(		$a 	#selectAll:					'Select all')
		#(		$f 	#find:						'Find')
		#(		$g 	#findAgain:					'Find again')
		#(		$h 	#help:						'Open this help')
		#(		$j 	#setSearchString:			'Set selection as search string for find again')

		#(		$z 	#undo:						'Undo (multiple levels)')
		#(		$x 	#cut:							'Cut selection and store it in the Clipboard')
		#(		$c 	#copySelection:			'Copy selection to the Clipboard')
		#(		$v 	#paste:						'Paste Clipboard contents')

		#(		$R		#indent:						'Indent (move selection one tab-stap right)')
		#(		$Y		#makeUppercase:			'Force selection to uppercase')
		#(		$U		#changeLineEndsToLf:		'Convert line endings to LF characters (Cuis convention) in selection')

		#(		$H		#cursorTopHome:			'Move cursor to start of text')
		#(		$L		#outdent:					'Outdent (move selection one tab-stop left)')

		#(		$Z		#redo:						'Redo (multiple levels)')
		#(		$X		#makeLowercase:			'Force selection to lowercase')
		#(		$C		#compareToClipboard:		'Compare argument to clipboard')

		#(		$ 		#selectWord:				'Select the current word as with double clicking')
	)! !


!Morph methodsFor: 'events-processing' stamp: 'jmv 12/21/2018 09:21:39'!
focusKeyboardFor: aKeyboardEvent

	"If aKeyboardEvent ctrl-tab or shift-ctrl-tab use it to navigate keyboard focus.
	Warning: This doesn't work on Windows... the event is not sent"
	(aKeyboardEvent keyValue = 9 and: [ aKeyboardEvent controlKeyPressed and: [ aKeyboardEvent rawMacOptionKeyPressed not ]])
		ifTrue: [
			aKeyboardEvent shiftPressed
				ifTrue: [ aKeyboardEvent hand keyboardFocusPrevious ]
				ifFalse: [ aKeyboardEvent hand keyboardFocusNext ].
			^ true ].
	"On Windows use at least some keystroke to navigate morphs... even shift-Tab that should navigate backwards"
"
	(aKeyboardEvent keyValue = 9 and: [ aKeyboardEvent shiftPressed and: [ aKeyboardEvent rawMacOptionKeyPressed not ]])
		ifTrue: [
			aKeyboardEvent hand keyboardFocusNext.
			^ true ].
"

	"Cycle through windows with cmdAlt + < and cmdAlt + >.
	VM and platform peculiarities are hidden in #isCmdAltLessThan and #isCmdAltGreaterThan"
	"This was done as an attempt to mimic the Mac OSX keystrokes for 'Move focus to next window in active application'. Unfortunately, it only works if OS X is set to use any other keys for this. If (as for example, with German defaults), OS-X uses these keystrokes, then they are not sent to the VM. This is a long standing issues in Chromium and PhotoShop, for example..."
	self disableCode: [
		aKeyboardEvent isCmdAltLessThan ifTrue: [
			aKeyboardEvent hand activatePreviousWindow.
			^true ].
		aKeyboardEvent isCmdAltGreaterThan ifTrue: [
			aKeyboardEvent hand activateNextWindow.
			^true ]].
	"Alternative for Mac OS-X: option-Tab and option-shift-Tab"
	(aKeyboardEvent keyValue = 9 and: [ aKeyboardEvent rawMacOptionKeyPressed ])
		ifTrue: [
			aKeyboardEvent shiftPressed
				ifTrue: [ aKeyboardEvent hand activatePreviousWindow ]
				ifFalse: [ aKeyboardEvent hand activateNextWindow ].
			^ true ].
	"Alternative for non-Mac OS-X: alt-< and alt->"
	(aKeyboardEvent commandAltKeyPressed and: [ aKeyboardEvent keyCharacter = $< ]) ifTrue: [
		aKeyboardEvent hand activatePreviousWindow.
		^true ].
	(aKeyboardEvent commandAltKeyPressed and: [ aKeyboardEvent keyCharacter = $> ]) ifTrue: [
		aKeyboardEvent hand activateNextWindow.
		^true ].
	^false! !


!HandMorph methodsFor: 'private events' stamp: 'jmv 12/21/2018 09:28:14'!
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
	((modifiers anyMask: 2) and: [
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


!AutoCompleter methodsFor: 'keyboard - private' stamp: 'AY 12/17/2018 16:57:52'!
shouldOpenMorph

	| currentPos currentChar |
	
	textMorph editor hasSelection ifTrue: [ ^ false ].
	
	currentPos _ textMorph editor startIndex-1.
	currentPos <= 0 ifTrue: [ ^ false ].
	currentChar _ model actualContents at: currentPos.
	
	^ currentChar = Character space
		ifTrue: [ self shouldOpenMorphWhenNoPrefixAt: currentPos-1 ]
		ifFalse: [ self shouldOpenMorphWhenPrefixAt: currentPos and: currentChar ].! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Editor initialize!

