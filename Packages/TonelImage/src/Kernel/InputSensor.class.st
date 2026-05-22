"
An InputSensor is an interface to the user input devices.
There is at least one (sub)instance of InputSensor named Sensor in the system.

Class variables:

ButtonDecodeTable <ByteArray> - maps mouse buttons as reported by the VM to ones reported in the events.

KeyDecodeTable <Dictionary<SmallInteger->SmallInteger>> - maps some keys and their modifiers to other keys (used for instance to map Ctrl-X to Alt-X)

InterruptSemaphore <Semaphore> - signalled by the the VM and/or the event loop upon receiving an interrupt keystroke.

InterruptWatcherProcess <Process> - waits on the InterruptSemaphore and then responds as appropriate.
"
Class {
	#name : 'InputSensor',
	#superclass : 'Object',
	#classVars : [
		'ButtonDecodeTable',
		'InterruptSemaphore',
		'InterruptWatcherProcess'
	],
	#category : 'Kernel-Processes',
	#package : 'Kernel',
	#tag : 'Processes'
}

{ #category : 'constants' }
InputSensor class >> anyModifierKey [
	"ignore, however, the shift keys 'cause that's not REALLY a command key"
	^ 16r70	"cmd | opt | ctrl"
]

{ #category : 'constants' }
InputSensor class >> anyMouseButton [
	^ 7
]

{ #category : 'constants' }
InputSensor class >> cmdAltOptionCtrlShiftModifierKeys [
	"Include all of them"
	^ 16r78	"cmd | opt | ctrl | shft "
]

{ #category : 'constants' }
InputSensor class >> commandAltKey [
	"command (Mac) / alt(Windows) key"
	^16r40
]

{ #category : 'constants' }
InputSensor class >> controlKey [
	"control / ctrl"
	^ 16r10
]

{ #category : 'class initialization' }
InputSensor class >> installMouseDecodeTable [
	"Create a decode table that swaps the lowest-order 2 bits if not on Windows.
	This is to make right mouse button be the mouseButton2, i.e. open menus, and middle (wheel) button open halos (mouseButton3).
	See #processEvent:"
	ButtonDecodeTable := Smalltalk platformName = 'Win32'
		ifTrue: [ ByteArray withAll: (0 to: 255) ]
		ifFalse: [
			ByteArray withAll:
				((0 to: 255) collect: [ :ea |
					((ea bitAnd: 1) << 1 bitOr: (ea bitAnd: 2) >> 1) bitOr: (ea bitAnd: 252) ]) ]
]

{ #category : 'constants' }
InputSensor class >> macOptionKey [
	"option (Mac only)  key"
	^16r20
]

{ #category : 'constants' }
InputSensor class >> mouseButton1 [
	^ 4
]

{ #category : 'constants' }
InputSensor class >> mouseButton2 [
	^ 2
]

{ #category : 'constants' }
InputSensor class >> mouseButton3 [
	^ 1
]

{ #category : 'constants' }
InputSensor class >> returnKey [
	"For historic reasons, return key is sent by the VM as character 13, even if Cuis uses Lf as the line terminator character"
	^ 13
]

{ #category : 'constants' }
InputSensor class >> shiftKey [
	"shift  key"
	^16r08
]

{ #category : 'system startup' }
InputSensor class >> shutDown [
	Sensor shutDownSensor
]

{ #category : 'system startup' }
InputSensor class >> startUp [
	
	self installMouseDecodeTable.
	EventSensor install
]

{ #category : 'modifier keys' }
InputSensor >> anyModifierKeyPressed [
	"ignore, however, the shift keys 'cause that's not REALLY a command key"

	^ self primMouseButtons anyMask: InputSensor anyModifierKey
]

{ #category : 'buttons' }
InputSensor >> buttons [
	"Answer the result of primMouseButtons, but swap the mouse  buttons if appropriate."
	^ ButtonDecodeTable at: self primMouseButtons + 1
]

{ #category : 'private' }
InputSensor >> characterForKeycode: keycode [
	"Map the given keycode to a Smalltalk character object. Encoding:
		A keycode is 12 bits:   <4 modifer bits><8 bit ISO character>
		Modifier bits are:       <command><option><control><shift>"

	"NOTE: the command and option keys are specific to the Macintosh and may not have equivalents on other platforms."

	keycode ifNil: [ ^nil ].
	^ Character codePoint: (keycode bitAnd: 16rFF)
]

{ #category : 'modifier keys' }
InputSensor >> commandAltKeyPressed [
	"Answer whether the command (Mac) / alt(Windows) key on the keyboard is being held down."

	^ self primMouseButtons anyMask: InputSensor commandAltKey
]

{ #category : 'modifier keys' }
InputSensor >> controlKeyPressed [
	"Answer whether the control/ctrl key on the keyboard is being held down."

	^ self primMouseButtons anyMask: InputSensor controlKey
]

{ #category : 'user interrupts' }
InputSensor >> eventTicklerProcess [
	"Answer my event tickler process, if any"
	^nil
]

{ #category : 'initialization' }
InputSensor >> flushEvents [
	"Do nothing"
]

{ #category : 'keyboard' }
InputSensor >> flushKeyboard [
	"Remove all characters from the keyboard buffer."

	[self keyboardPressed]
		whileTrue: [self keyboard]
]

{ #category : 'tablet' }
InputSensor >> hasTablet [
	"Answer true if there is a pen tablet available on this computer."

	^ (self primTabletGetParameters: 1) notNil

]

{ #category : 'user interrupts' }
InputSensor >> installInterruptWatcher [
	"Initialize the interrupt watcher process. Terminate the old process if any.
	This process simply waits for the VM to signal the 'user interrupt' semaphore, and opens a debugger."
	"
	Sensor installInterruptWatcher
	"

	InterruptWatcherProcess ifNotNil: [InterruptWatcherProcess terminate].
	InterruptSemaphore := (Smalltalk specialObjectsArray at: 31) ifNil: [Semaphore new].
	InterruptWatcherProcess := [self userInterruptWatcher] newProcess.
	InterruptWatcherProcess priority: Processor timingPriority-5.
	InterruptWatcherProcess name: '[system] User interrupt watcher'.
	InterruptWatcherProcess resume
]

{ #category : 'user interrupts' }
InputSensor >> interruptWatcherProcess [
	"Answer my interrupt watcher process, if any"
	^InterruptWatcherProcess
]

{ #category : 'mouse' }
InputSensor >> isAnyButtonPressed [
	"Answer whether at least one mouse button is currently being pressed."

	^ self primMouseButtons anyMask: InputSensor anyMouseButton

]

{ #category : 'mouse' }
InputSensor >> isMouseButton1Pressed [
	"Answer true if only the mouseButton1 is being pressed.
	This is the first mouse button, usually the one at the left."

	^ (self primMouseButtons bitAnd: 7) = InputSensor mouseButton1
]

{ #category : 'mouse' }
InputSensor >> isMouseButton2Pressed [
	"Answer whether only the mouseButton2 is being pressed. 
	This is the usually the right mouse button or option+click on the Mac."

	^ (self primMouseButtons bitAnd: 7) = InputSensor mouseButton2
]

{ #category : 'mouse' }
InputSensor >> isMouseButton3Pressed [
	"Answer whether only the mouseButton3 is being pressed. 
	This is the third mouse button, usually the wheel or button at the center, or cmd+click on the Mac."

	^ (self primMouseButtons bitAnd: 7) = InputSensor mouseButton3
]

{ #category : 'joystick' }
InputSensor >> joystickButtons: index [

	^ ((self primReadJoystick: index) bitShift: -22) bitAnd: 16r71F
	
]

{ #category : 'joystick' }
InputSensor >> joystickOn: index [

	^ (((self primReadJoystick: index) bitShift: -27) bitAnd: 1) ~= 0
	
]

{ #category : 'joystick' }
InputSensor >> joystickXY: index [

	| inputWord x y |
	inputWord := self primReadJoystick: index.
	x := (inputWord bitAnd: 16r7FF) - 16r400.
	y := ((inputWord bitShift: -11) bitAnd: 16r7FF) - 16r400.
	^ x@y
	
]

{ #category : 'keyboard' }
InputSensor >> keyboard [
	"Answer the next character from the keyboard."

	^ self characterForKeycode: self primKeyboardNext
]

{ #category : 'keyboard' }
InputSensor >> keyboardPeek [
	"Answer the next character in the keyboard buffer without removing it, or nil if it is empty."

	^ self characterForKeycode: self primKeyboardPeek
]

{ #category : 'keyboard' }
InputSensor >> keyboardPressed [
	"Answer true if keystrokes are available."

	^self primKeyboardPeek notNil
]

{ #category : 'keyboard' }
InputSensor >> keyboardTest [
	"
	Sensor keyboardTest
	"
	"This test routine will print the unmodified character, its keycode,
	and the OR of all its modifier bits, until the character x is typed

Also useful:
	Sensor test
	"
	| char |
	char := nil.
	[ char == $x ] whileFalse: [
		[ self keyboardPressed ] whileFalse.
		char := self keyboard.
		Display fill: `5@5 extent: 600@50` fillColor: `Color white`.
		(String streamContents: [ :s |
			s
				nextPut: $-;
				nextPut: char;
				nextPut: $-;
				space;
				nextPutAll: char printString;
				space;
				nextPut: $(;
				print: char codePoint;
				nextPut: $);
				space.
			self isMouseButton3Pressed ifTrue: [ s nextPutAll: ' mouseButton3/blue/tertiary/halo ' ].
			self isMouseButton2Pressed ifTrue: [ s nextPutAll: ' mouseButton2/yellow/secondary/menu ' ].
			self isMouseButton1Pressed ifTrue: [ s nextPutAll: ' mouseButton1/red/primary ' ].
			self shiftPressed ifTrue: [ s nextPutAll: ' shift ' ].
			self controlKeyPressed ifTrue: [ s nextPutAll: ' control/ctrl ' ].
			(self primMouseButtons anyMask: 32) ifTrue: [ s nextPutAll: ' macOption ' ].
			self commandAltKeyPressed ifTrue: [ s nextPutAll: ' macCommand/winAlt ' ].
			Time now printOn: s
			])
		displayAt: 10 @ 10 ].
]

{ #category : 'modifier keys' }
InputSensor >> macOptionKeyPressed [
	"Answer whether the option key on the Macintosh keyboard is being held down. Macintosh specific."

	self notify: 'Portability note:
InputSensor>>macOptionKeyPressed is not portable.
Please use InputSensor>>mouseButton2Pressed instead!'.

	^ self rawMacOptionKeyPressed
]

{ #category : 'mouse' }
InputSensor >> mouseButtons [
	"Answer a number from 0 to 7 that encodes the state of the three mouse buttons in its lowest 3 bits."

	^ self primMouseButtons bitAnd: 7

]

{ #category : 'mouse' }
InputSensor >> mousePoint [
	"Answer a Point indicating the coordinates of the current mouse location."
	^self primMousePt
]

{ #category : 'mouse' }
InputSensor >> noButtonPressed [
	"Answer whether any mouse button is not being pressed."

	^self isAnyButtonPressed not

]

{ #category : 'mouse' }
InputSensor >> peekButtons [
	^self primMouseButtons
]

{ #category : 'mouse' }
InputSensor >> peekMousePt [
	^self primMousePt
]

{ #category : 'private' }
InputSensor >> primKeyboardNext [
	<primitive: 108>
	^ nil
]

{ #category : 'private' }
InputSensor >> primKeyboardPeek [
	<primitive: 109>
	^ nil
]

{ #category : 'private' }
InputSensor >> primMouseButtons [
	<primitive: 107>
	^ 0
]

{ #category : 'private' }
InputSensor >> primMousePt [
	"Primitive. Poll the mouse to find out its position. Return a Point. Fail if
	event-driven tracking is used instead of polling. Optional. See Object
	documentation whatIsAPrimitive."

	<primitive: 90>
	^ `0@0`
]

{ #category : 'private' }
InputSensor >> primReadJoystick: index [
	"Return the joystick input word for the joystick with the given index in the range [1..16]. Returns zero if the index does not correspond to a currently installed joystick."

	<primitive: 'primitiveReadJoystick' module: 'JoystickTabletPlugin'>
	^ 0

	
]

{ #category : 'private' }
InputSensor >> primSetInterruptKey: anInteger [
	"Primitive. Register the given keycode as the user interrupt key. The low byte of the keycode is the ISO character and its next four bits are the Smalltalk modifer bits <cmd><opt><ctrl><shift>."

	<primitive: 133>
	^self primitiveFailed
"Note: This primitive is obsolete with the new event driven architecture in which EventSensor can handle the interrupts itself. However, for supporting older images running on newer VMs the primitive must still be implemented."
]

{ #category : 'private' }
InputSensor >> primTabletGetParameters: cursorIndex [
	"Answer the pen tablet parameters. For parameters that differ from cursor to cursor, answer those associated with the cursor having the given index. Answer nil if there is no pen tablet. The parameters are:
	1. tablet width, in tablet units
	2. tablet height, in tablet units
	3. number of tablet units per inch
	4. number of cursors (pens, pucks, etc; some tablets have more than one)
	5. this cursor index
	6. and 7. x scale and x offset for scaling tablet coordintes (e.g., to fit the screen)
	8. and 9. y scale and y offset for scaling tablet coordintes  (e.g., to fit the screen)
	10. number of pressure levels
	11. presure threshold needed close pen tip switch 
	12. number of pen tilt angles"

	<primitive: 'primitiveGetTabletParameters' module: 'JoystickTabletPlugin'>
	^ nil

]

{ #category : 'private' }
InputSensor >> primTabletRead: cursorIndex [
	"Answer the pen tablet data for the cursor having the given index. Answer nil if there is no pen tablet. The data is:
	1. index of the cursor to which this data applies
	2. timestamp of the last state chance for this cursor
	3., 4., and 5. x, y, and z coordinates of the cursor (z is typically 0)
	6. and 7. xTilt and yTilt of the cursor; (signed)
	8. type of cursor (0 = unknown, 1 = pen, 2 = puck, 3 = eraser)
	9. cursor buttons
	10. cursor pressure, downward
	11. cursor pressure, tangential
	12. flags"

	<primitive: 'primitiveReadTablet' module: 'JoystickTabletPlugin'>
	self primitiveFailed

]

{ #category : 'modifier keys' }
InputSensor >> rawMacOptionKeyPressed [
	"Answer whether the option key on the Macintosh keyboard is being held down. Macintosh specific.  Clients are discouraged from calling this directly, since it circumvents bert's attempt to eradicate option-key checks"

	^ self primMouseButtons anyMask: InputSensor macOptionKey
]

{ #category : 'user interrupts' }
InputSensor >> setInterruptKey: anInteger [
	"Register the given keycode as the user interrupt key."

	self primSetInterruptKey: anInteger.

]

{ #category : 'modifier keys' }
InputSensor >> shiftPressed [
	"Answer whether the shift key on the keyboard is being held down."

	^ self primMouseButtons anyMask: InputSensor shiftKey
]

{ #category : 'tablet' }
InputSensor >> tabletExtent [
	"Answer the full tablet extent in tablet coordinates."

	| params |
	params := self primTabletGetParameters: 1.
	params ifNil: [^ self error: 'no tablet available'].
	^ (params at: 1)@(params at: 2)

]

{ #category : 'tablet' }
InputSensor >> tabletPoint [
	"Answer the current position of the first tablet pointing device (pen, puck, or eraser) in tablet coordinates."

	| data |
	data := self primTabletRead: 1.  "state of first/primary pen"
	^ (data at: 3) @ (data at: 4)

]

{ #category : 'tablet' }
InputSensor >> tabletPressure [
	"Answer the current pressure of the first tablet pointing device (pen, puck, or eraser), a number between 0.0 (no pressure) and 1.0 (max pressure)"

	| params data |
	params := self primTabletGetParameters: 1.
	params ifNil: [^ self].
	data := self primTabletRead: 1.  "state of first/primary pen"
	^ (data at: 10) asFloat / ((params at: 10) - 1)

]

{ #category : 'tablet' }
InputSensor >> tabletTimestamp [
	"Answer the time (in tablet clock ticks) at which the tablet's primary pen last changed state. This can be used in polling loops; if this timestamp hasn't changed, then the pen state hasn't changed either."

	| data |
	data := self primTabletRead: 1.  "state of first/primary pen"
	^ data at: 2

]

{ #category : 'joystick' }
InputSensor >> testJoystick: index [
	"Sensor testJoystick: 3"

	| f pt buttons status |
	f := Form extent: 110@50 depth: 32.
	[Sensor isAnyButtonPressed] whileFalse: [
		pt := Sensor joystickXY: index.
		buttons := Sensor joystickButtons: index.
		status :=
'xy: ', pt printString, '
buttons: ', buttons hex.
		f fillWhite.
		status displayOn: f at: 10@10.
		f displayOn: Display at: 10@10.
	].

]

{ #category : 'user interrupts' }
InputSensor >> userInterruptWatcher [
	"Wait for user interrupts and open a notifier on the active process when one occurs."

	[ true ] whileTrue: [
		InterruptSemaphore wait.
"		Display deferUpdates: false."
		Smalltalk at: #SoundPlayer ifPresent: [ :cls | cls shutDown ].
		Smalltalk handleUserInterrupt]
]

{ #category : 'mouse' }
InputSensor >> waitButton [
	"Wait for the user to press any mouse button and then answer with the 
	current location of the cursor."

	| delay |
	delay := Delay forMilliseconds: 50.
	[ self isAnyButtonPressed ] whileFalse: [ delay wait ].
	^self mousePoint

]

{ #category : 'mouse' }
InputSensor >> waitButtonOrKeyboard [
	"Wait for the user to press either any mouse button or any key. 
	Answer the current cursor location or nil if a keypress occured."

	| delay |
	delay := Delay forMilliseconds: 50.
	[ self isAnyButtonPressed ]
		whileFalse: [
			delay wait.
			self keyboardPressed
				ifTrue: [^ nil]].
	^ self mousePoint

]

{ #category : 'mouse' }
InputSensor >> waitClickButton [
	"Wait for the user to click (press and then release) any mouse button and then
	answer with the current location of the cursor."
	self waitButton.
	^self waitNoButton
]

{ #category : 'mouse' }
InputSensor >> waitNoButton [
	"Wait for the user to release any mouse button and then answer the current location of the cursor."

	| delay |
	delay := Delay forMilliseconds: 50.
	[ self isAnyButtonPressed ] whileTrue: [ delay wait].
	^self mousePoint

]
