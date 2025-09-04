'From Cuis7.5 [latest update: #7477] on 2 September 2025 at 8:40:59 am'!

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2025 08:18:29'!
end

	^ `Character codePoint: 4`! !


!InputSensor methodsFor: 'keyboard' stamp: 'jmv 9/2/2025 08:19:42'!
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
		displayAt: 10 @ 10 ].! !


!Character class methodsFor: 'private' stamp: 'jmv 9/2/2025 08:18:34'!
constantNames
	^ #(backQuote backspace cr delete doubleQuote escape home end insert lf newPage noBreakSpace null pageUp singleQuote smalltalkDownArrow smalltalkLeftArrow smalltalkRightArrow smalltalkUpArrow space tab zeroWidthSpace)! !

