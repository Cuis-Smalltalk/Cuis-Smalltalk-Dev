'From Cuis7.5 [latest update: #7758] on 15 December 2025 at 9:59:11 am'!

!InputSensor methodsFor: 'keyboard' stamp: 'jmv 15/Dec/2025 09:56:47'!
keyboardTest
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
		displayAt: 10 @ 10 ].! !


!EventSensor methodsFor: 'private' stamp: 'jmv 15/Dec/2025 09:58:03'!
primKbdNext
	"Allows for use of old Sensor protocol to get at the keyboard,
	as when running keyboardTest or the InterpreterSimulator in Morphic"
	| evtBuf |
	self fetchMoreEvents.
	evtBuf := eventQueue nextOrNilSuchThat: [ :buf | self isKbdEvent: buf].
	self flushNonKbdEvents.
	^ evtBuf ifNotNil: [ evtBuf at: 3 ]! !

!EventSensor methodsFor: 'private' stamp: 'jmv 15/Dec/2025 09:58:33'!
primKbdPeek
	"Allows for use of old Sensor protocol to get at the keyboard,
	as when running keyboardTest or the InterpreterSimulator in Morphic"
	| char |
	self fetchMoreEvents.
	char := nil.
	"NOTE: must not return out of this block, so loop to end"
	eventQueue nextOrNilSuchThat: [ :buf |
		(self isKbdEvent: buf) ifTrue: [
			char ifNil: [ char := buf at: 3 ]].
		false  "NOTE: block value must be false so Queue won't advance"].
	^ char! !

!EventSensor methodsFor: 'test' stamp: 'jmv 15/Dec/2025 09:58:53'!
test
	"
	Sensor test

Also useful:
	Sensor keyboardTest
	"
	| c lastStamp stamp |
	c := nil.
	lastStamp := 0.
	[ c == $x asciiValue ] whileFalse: [
		Sensor nextEvent ifNotNil: [ :evt |
			stamp := evt at: 2.
			stamp - lastStamp > 300 ifTrue: [ ' ' print.' ' print ].
			Transcript show: evt; newLine.
			self printEventBuffer: evt.
			(evt first = 2 and: [ evt fourth = 0 ]) ifTrue: [
				c := evt third ].
			lastStamp := stamp]]! !


!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 15/Dec/2025 09:57:16'!
backspace
	"Answer the Character representing a backspace.
	This character is generally not used in text editors and files, and doesn't have a visible glyph.
	This is the code for the [<-] backspace key. See #keyboardTest See #primGetNextEvent:"

	^`Character codePoint: 8`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 15/Dec/2025 09:57:20'!
cr
	"Answer the Character representing a carriage return.
	This character is generally not used in text editors and files, because Cuis uses the LF line ending convention.
	It has a visible glyph, to make it easier to spot text that doesn't conform to the Cuis line ending convention.
	This is the code for the [ENTER] or [RETURN] key. See #keyboardTest See #primGetNextEvent:"

	^`Character codePoint: 13`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 15/Dec/2025 09:57:23'!
end
	"Answer the Character for the [END] key. See #keyboardTest See #primGetNextEvent:
	This character is generally not used in text editors and files, and doesn't have a visible glyph."

	^ `Character codePoint: 4`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 15/Dec/2025 09:57:26'!
escape
	"Answer the ASCII ESC character
	This character is generally not used in text editors and files, and doesn't have a visible glyph.
	This is the code for the [ESC] key. See #keyboardTest See #primGetNextEvent:"

	^ `Character codePoint: 27`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 15/Dec/2025 09:57:30'!
home
	"Answer the Character for the [HOME] key. See #keyboardTest See #primGetNextEvent:
	This character is generally not used in text editors and files, and doesn't have a visible glyph."

	^ `Character codePoint: 1`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 15/Dec/2025 09:57:33'!
insert
	"Answer the Character for the [INSERT] key. See #keyboardTest See #primGetNextEvent:
	This character is generally not used in text editors and files, and doesn't have a visible glyph."

	^ `Character codePoint: 5`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 15/Dec/2025 09:57:36'!
pageDown
	"Answer the Character for the [Page Down] key. See #keyboardTest See #primGetNextEvent:
	This character may be used in text files. The ASCII control character name for it is 'Form Feed'. See #newPage
	This character doesn't have a visible glyph."

	^ `Character codePoint: 12`! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 15/Dec/2025 09:57:39'!
pageUp
	"Answer the Character for the [Page Up] key. See #keyboardTest See #primGetNextEvent:
	This character is generally not used in text editors and files, and doesn't have a visible glyph."

	^ `Character codePoint: 11`! !

!methodRemoval: InputSensor #kbdTest stamp: 'jmv 15/Dec/2025 09:58:49'!
InputSensor removeSelector: #kbdTest!
