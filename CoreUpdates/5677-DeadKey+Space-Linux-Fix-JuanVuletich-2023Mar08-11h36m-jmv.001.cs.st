'From Cuis 6.0 [latest update: #5676] on 8 March 2023 at 11:38:43 am'!

!Editor methodsFor: 'accessing-selection' stamp: 'jmv 3/8/2023 11:18:38'!
markIndex

	self subclassResponsibility ! !

!Editor methodsFor: 'accessing-selection' stamp: 'jmv 3/8/2023 11:18:43'!
pointIndex

	self subclassResponsibility ! !

!Editor methodsFor: 'accessing-selection' stamp: 'jmv 3/8/2023 11:18:13'!
startIndex

	self subclassResponsibility ! !

!Editor methodsFor: 'accessing-selection' stamp: 'jmv 3/8/2023 11:18:21'!
stopIndex

	self subclassResponsibility ! !

!Editor methodsFor: 'private' stamp: 'jmv 3/8/2023 11:36:58'!
privateCurrentString

	self subclassResponsibility! !


!EventSensor methodsFor: 'test' stamp: 'jmv 3/8/2023 11:37:40'!
printEventBuffer: evtBuf
	"Print the event buffer, currently only used by the method `test`."

	| type buttons macRomanCode modifiers pressType stamp unicodeCodePoint |
	type _ evtBuf first.
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time millisecondClockValue ]. "VMs report events using #millisecondClockValue"
	type = EventSensor eventTypeMouse
		ifTrue: [ | position |
			position _ evtBuf third @ evtBuf fourth.
			buttons _ evtBuf fifth.
			modifiers _ evtBuf sixth.
			Transcript
				newLine;
				show: 'Mouse';
				show: ' position:', position printString;
				show: ' buttons:', buttons printString;
				show: ' modifiers:', modifiers printString.
			].
	type = EventSensor eventTypeMouseScroll
		ifTrue: [ | delta |
			delta _ evtBuf third @ evtBuf fourth.
			buttons _ evtBuf fifth.
			modifiers _ evtBuf sixth.
			Transcript
				newLine;
				show: 'Scroll';
				show: ' delta:', delta printString;
				show: ' buttons:', buttons printString;
				show: ' modifiers:', modifiers printString.
			].
	type = EventSensor eventTypeKeyboard 
		ifTrue: [
			macRomanCode _ evtBuf third.
			unicodeCodePoint _ evtBuf sixth.
			pressType _ evtBuf fourth.
			modifiers _ evtBuf fifth.
			pressType = EventSensor eventKeyDown ifTrue: [
				type _ #keyDown].
			pressType = EventSensor eventKeyUp ifTrue: [
				type _ #keyUp].
			pressType = EventSensor eventKeyChar ifTrue: [
				type _ #keystroke].
			{type. 'macRoman:'. macRomanCode. macRomanCode hex.
				'unicodeCodePoint:'. unicodeCodePoint. unicodeCodePoint hex. UnicodeCodePoint codePoint: unicodeCodePoint} print.
			Transcript nextPutAll: ' modifiers: ', modifiers printString.
			(modifiers anyMask: 8) ifTrue: [ Transcript nextPutAll: ' [commandWinAlt]' ].
			(modifiers anyMask: 4) ifTrue: [ Transcript nextPutAll: ' [macOption]' ].
			(modifiers anyMask: 2) ifTrue: [ Transcript nextPutAll: ' [control]' ].
			(modifiers anyMask: 1) ifTrue: [ Transcript nextPutAll: ' [shift]' ].
			''print.
			].! !

!EventSensor methodsFor: 'test' stamp: 'jmv 3/8/2023 10:26:28'!
test
	"
	Sensor test

Also useful:
	Sensor kbdTest
	"
	| c lastStamp stamp |
	c _ nil.
	lastStamp _ 0.
	[ c = $x asciiValue ] whileFalse: [
		Sensor nextEvent ifNotNil: [ :evt |
			stamp _ evt at: 2.
			stamp - lastStamp > 300 ifTrue: [ ' ' print.' ' print ].
			Transcript show: evt; newLine.
			self printEventBuffer: evt.
			(evt first = 2 and: [ evt fourth = 0 ]) ifTrue: [
				c _ evt third ].
			lastStamp _ stamp]]! !


!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 3/8/2023 11:38:15'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| character i replacement |
	character _ aKeyboardEvent keyCharacter.
	"Linux VMs don't automatically replace combining marks + space with ASCII,
	as Windows and Mac VMs do. It seems it is an app responsability to do that in Linux."
	character isDiacriticalMark ifTrue: [
		i _ self pointIndex-1.
		(i > 0 and: [ (self privateCurrentString at: i) = $  ]) ifTrue: [
			replacement _ character caseOf: {
				[ `UnicodeCodePoint codePoint: 16r300` ] -> [ '`' ].
				[ `UnicodeCodePoint codePoint: 16r301` ] -> [ '''' ].
				[ `UnicodeCodePoint codePoint: 16r302` ] -> [ '^' ].
				[ `UnicodeCodePoint codePoint: 16r303` ] -> [ '~' ].
				[ `UnicodeCodePoint codePoint: 16r308` ] -> [ '"' ].
			} otherwise: [].
			replacement notNil ifTrue: [
				self selectFrom: i to: i; replaceSelectionWith: replacement.
				^ false ]
			]
		].
	self addString: (UnicodeString with: character).
	^false! !

