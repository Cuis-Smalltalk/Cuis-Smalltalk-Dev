'From Cuis 4.2 of 25 July 2013 [latest update: #1863] on 7 November 2013 at 9:20:45.294468 pm'!

!InputSensor methodsFor: 'keyboard' stamp: 'jmv 11/7/2013 21:17'!
kbdTest
	"
	Sensor kbdTest
	"
	"This test routine will print the unmodified character, its keycode,
	and the OR of all its modifier bits, until the character x is typed

Also useful:
	Sensor test
	"
	| char |
	char _ nil.
	[ char = $x ] whileFalse: [
		[ self keyboardPressed ] whileFalse.
		char _ self keyboard.
		Display fill: (5@5 extent: 400@20) fillColor: Color white.
		(String streamContents: [ :s |
			s
				nextPut: $-;
				nextPut: char;
				nextPut: $-;
				space;
				print: char asciiValue;
				space.
			self isMouseButton3Pressed ifTrue: [ s nextPutAll: ' mouseButton3/blue/tertiary/halo ' ].
			self isMouseButton2Pressed ifTrue: [ s nextPutAll: ' mouseButton2/yellow/secondary/menu ' ].
			self isMouseButton1Pressed ifTrue: [ s nextPutAll: ' mouseButton1/red/primary ' ].
			self shiftPressed ifTrue: [ s nextPutAll: ' shift ' ].
			self controlKeyPressed ifTrue: [ s nextPutAll: ' control/ctrl ' ].
			(self primMouseButtons anyMask: 32) ifTrue: [ s nextPutAll: ' macOption ' ].
			self commandAltKeyPressed ifTrue: [ s nextPutAll: ' macCommand/winAlt ' ]])
		displayAt: 10 @ 10 ].! !

!InputSensor methodsFor: 'private' stamp: 'jmv 11/7/2013 21:20'!
characterForKeycode: keycode
	"Map the given keycode to a Smalltalk character object. Encoding:
		A keycode is 12 bits:   <4 modifer bits><8 bit ISO character>
		Modifier bits are:       <command><option><control><shift>"

	"NOTE: the command and option keys are specific to the Macintosh and may not have equivalents on other platforms."

	keycode ifNil: [ ^nil ].
	^ Character value: (keycode bitAnd: 16rFF)! !


!Transcripter methodsFor: 'command line' stamp: 'jmv 11/7/2013 21:19'!
request: prompt
	| startPos char contents return |
	return _ Character value: InputSensor returnKey.
	self
		newLine;
		show: prompt.
	startPos _ position.
	[
		[ Sensor keyboardPressed ] whileFalse.
		(char _ Sensor keyboard) = return ] whileFalse: [
			char = Character backspace
				ifTrue: [ readLimit _ position _ position - 1 max: startPos ]
				ifFalse: [ self nextPut: char ].
			self endEntry ].
	contents _ self contents.
	^ contents
		copyFrom: startPos + 1
		to: contents size! !

