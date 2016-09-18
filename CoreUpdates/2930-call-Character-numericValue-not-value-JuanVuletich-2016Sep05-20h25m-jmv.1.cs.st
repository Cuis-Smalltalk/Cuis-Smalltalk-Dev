'From Cuis 4.2 of 25 July 2013 [latest update: #2929] on 5 September 2016 at 8:39:51 pm'!

!Character methodsFor: 'accessing' stamp: 'jmv 9/2/2016 10:46:56'!
iso8859s15Code
	"Answer the value of the receiver that represents its ISO 8859-15 (Latin-9) encoding.
	Any implementation of Character like object beyond ISO-8859-15 should answer nil.
	Senders, please consider that this method might answer nil.
	See #iso8859s15CodeForUnicodeCodePoint:"

	^ self numericValue! !

!Character methodsFor: 'converting' stamp: 'jmv 9/2/2016 11:04:41'!
asUnaccented
	"
	$A asUnaccented
	$¡ asUnaccented
	(0 to: 255) collect: [ :i | (Character numericValue: i) asUnaccented ]
	(0 to: 255) collect: [ :i | (Character numericValue: i) asUnaccented  asLowercase]
	"
	^ Character
		numericValue: (UnaccentedTable at: self numericValue + 1)! !

!Character methodsFor: 'converting' stamp: 'jmv 9/2/2016 11:15:59'!
asUppercase
	"If the receiver is lowercase, answer its matching uppercase Character."
	
	^ Character
		numericValue: (((ClassificationTable at: self numericValue + 1)
				bitShift: -8)
				bitAnd: 255)! !

!Character methodsFor: 'converting' stamp: 'jmv 9/2/2016 11:00:54'!
to: other
	"Answer with a collection in ascii order -- $a to: $z"
	^ (self numericValue to: other numericValue) collect:
				[:ascii | Character numericValue: ascii]! !


!Character class methodsFor: 'class initialization' stamp: 'jmv 9/2/2016 11:01:58'!
initializeUnicodeCodePoints
	"
	String streamContents: [ :strm | 28 to: 255 do: [ :i | strm nextPut: (Character numericValue: i) ]]
	"


	"Initialize the table of Unicode code points"
	UnicodeCodePoints _ Array new: 256.
	0 to: 255 do: [ :code |
		UnicodeCodePoints at: code + 1 put: code ].
	
	"The following codes are different in ISO 8859-15 from those in ISO 8859-1,
	so the character code is not equal to the Unicode code point"
	UnicodeCodePoints at: 16rA4+1 put: 16r20AC.		"euro sign"
	UnicodeCodePoints at: 16rA6+1 put: 16r160.		"latin capital letter S with caron"
	UnicodeCodePoints at: 16rA8+1 put: 16r161.		"latin small letter s with caron"
	UnicodeCodePoints at: 16rB4+1 put: 16r17D.		"latin capital letter Z with caron"
	UnicodeCodePoints at: 16rB8+1 put: 16r17E.		"latin small letter z with caron"
	UnicodeCodePoints at: 16rBC+1 put: 16r152.		"latin capital ligature OE"
	UnicodeCodePoints at: 16rBD+1 put: 16r153.		"latin small ligature oe"
	UnicodeCodePoints at: 16rBE+1 put: 16r178.		"latin capital letter Y with diaeresis"

	UnicodeCodePoints at: 16r80+1 put: 16r2200.		"FOR ALL"
	UnicodeCodePoints at: 16r81+1 put: 16r2202.		"PARTIAL DIFFERENTIAL"
	UnicodeCodePoints at: 16r82+1 put: 16r2203.		"THERE EXISTS"
	UnicodeCodePoints at: 16r83+1 put: 16r2204.		"THERE DOES NOT EXIST"
	UnicodeCodePoints at: 16r84+1 put: 16r2205.		"EMPTY SET"
	UnicodeCodePoints at: 16r85+1 put: 16r221E.		"INFINITY"
	UnicodeCodePoints at: 16r86+1 put: 16r2102.		"DOUBLE-STRUCK CAPITAL C"
	UnicodeCodePoints at: 16r87+1 put: 16r210D.		"DOUBLE-STRUCK CAPITAL H"
	UnicodeCodePoints at: 16r88+1 put: 16r2115.		"DOUBLE-STRUCK CAPITAL N"
	UnicodeCodePoints at: 16r89+1 put: 16r2119.		"DOUBLE-STRUCK CAPITAL P"
	UnicodeCodePoints at: 16r8A+1 put: 16r211A.		"DOUBLE-STRUCK CAPITAL Q"
	UnicodeCodePoints at: 16r8B+1 put: 16r211D.		"DOUBLE-STRUCK CAPITAL R"
	UnicodeCodePoints at: 16r8C+1 put: 16r2124.		"DOUBLE-STRUCK CAPITAL Z"
	UnicodeCodePoints at: 16r8D+1 put: 16r2135.		"ALEF SYMBOL"
	UnicodeCodePoints at: 16r8E+1 put: 16r2A00.		"N-ARY CIRCLED DOT OPERATOR"
	UnicodeCodePoints at: 16r8F+1 put: 16r2A01.		"N-ARY CIRCLED PLUS OPERATOR"
	UnicodeCodePoints at: 16r90+1 put: 16r2A02.		"N-ARY CIRCLED TIMES OPERATOR"
	UnicodeCodePoints at: 16r91+1 put: 16r2211.		"N-ARY SUMMATION"
	UnicodeCodePoints at: 16r92+1 put: 16r222B.		"INTEGRAL"
	UnicodeCodePoints at: 16r93+1 put: 16r2A15.		"INTEGRAL AROUND A POINT OPERATOR"
	UnicodeCodePoints at: 16r94+1 put: 16r2260.		"NOT EQUAL TO"
	UnicodeCodePoints at: 16r95+1 put: 16r2261.		"IDENTICAL TO"
	UnicodeCodePoints at: 16r96+1 put: 16r2262.		"NOT IDENTICAL TO"
	UnicodeCodePoints at: 16r97+1 put: 16r2263.		"STRICTLY EQUIVALENT TO"
	UnicodeCodePoints at: 16r98+1 put: 16r2264.		"LESS-THAN OR EQUAL TO"
	UnicodeCodePoints at: 16r99+1 put: 16r2265.		"GREATER-THAN OR EQUAL TO"
	UnicodeCodePoints at: 16r9A+1 put: 16r2266.		"LESS-THAN OVER EQUAL TO"
	UnicodeCodePoints at: 16r9B+1 put: 16r2267.		"GREATER-THAN OVER EQUAL TO"
	UnicodeCodePoints at: 16r9C+1 put: 16r2268.		"LESS-THAN BUT NOT EQUAL TO"
	UnicodeCodePoints at: 16r9D+1 put: 16r2269.		"GREATER-THAN BUT NOT EQUAL TO"
	UnicodeCodePoints at: 16r9E+1 put: 16r2218.		"RING OPERATOR"
	UnicodeCodePoints at: 16r9F+1 put: 16r2219.		"BULLET OPERATOR"! !

!Character class methodsFor: 'instance creation' stamp: 'jmv 9/5/2016 16:49:54'!
allCharacters
	^ self characterTable! !

!Character class methodsFor: 'instance creation' stamp: 'jmv 9/2/2016 11:02:15'!
separators
	^ #(32 "space"
		13 "cr"
		9 "tab"
		10 "line feed"
		12 "form feed")
		collect: [:v | Character numericValue: v]

	
! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:16:33'!
arrowDown
	"
	Character arrowDown
	"
	^ self numericValue: 31! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:16:35'!
arrowLeft
	"
	Character arrowLeft
	"
	^ self numericValue: 28! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:16:36'!
arrowRight
	"
	Character arrowRight
	"
	^ self numericValue: 29! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:16:38'!
arrowUp
	"
	Character arrowUp
	"
	^ self numericValue: 30! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:16:40'!
backspace
	"Answer the Character representing a backspace."

	^self numericValue: 8! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:16:46'!
cr
	"Answer the Character representing a carriage return."

	^self numericValue: 13! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:16:48'!
delete
	^ self numericValue: 127! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:16:54'!
end
	^ self numericValue: 4! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:16:56'!
enter
	"Answer the Character representing enter."

	^self numericValue: 3! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:16:57'!
escape
	"Answer the ASCII ESC character"

	^self numericValue: 27! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:01:53'!
euro
	"The Euro currency sign, that E with two dashes.
	Cuis uses ISO Latin 9 (ISO 8859-15) character encoding.
	Each character is 1 byte"

	^ Character numericValue: 164! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:17:29'!
home
	^ self numericValue: 1! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:17:34'!
insert
	^ self numericValue: 5! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:17:42'!
lf
	"Answer the Character representing a linefeed."

	^self numericValue: 10! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:02:08'!
nbsp
	"non-breakable space."

	^ Character numericValue: 202! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:17:44'!
newPage
	"Answer the Character representing a form feed."

	^self numericValue: 12! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:17:48'!
null
	"Answer the null Character."

	^self numericValue: 0! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:17:55'!
pageDown
	^ self numericValue: 12! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:17:56'!
pageUp
	^ self numericValue: 11! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:17:59'!
space
	"Answer the Character representing a space."

	^self numericValue: 32! !

!Character class methodsFor: 'accessing untypeable characters' stamp: 'jmv 9/2/2016 11:18:05'!
tab
	"Answer the Character representing a tab."

	^self numericValue: 9! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:16:04'!
CC
	"
	Character CC
	"
	^ self numericValue: 16r86! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:16:07'!
HH
	"
	Character HH
	"
	^ self numericValue: 16r87! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:16:11'!
NN
	"
	Character NN
	"
	^ self numericValue: 16r88! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:16:14'!
PP
	"
	Character PP
	"
	^ self numericValue: 16r89! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:16:27'!
QQ
	"
	Character QQ
	"
	^ self numericValue: 16r8A! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:16:28'!
RR
	"
	Character RR
	"
	^ self numericValue: 16r8B! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:16:30'!
ZZ
	"
	Character ZZ
	"
	^ self numericValue: 16r8C! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:16:32'!
aleph
	"
	Character aleph
	"
	^ self numericValue: 16r8D! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:16:41'!
bullet
	"
	Character bullet
	"
	^ self numericValue: 16r9F! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:16:42'!
circle
	"
	Character circle
	"
	^ self numericValue: 16r9E! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:16:44'!
contourIntegral
	"
	Character contourIntegral
	"
	^ self numericValue: 16r93! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:16:49'!
doesNotExist
	"
	Character doesNotExist
	"
	^ self numericValue: 16r83! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:16:51'!
dot
	"
	Character dot
	"
	^ self numericValue: 16rB7! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:16:53'!
emptySet
	"
	Character emptySet
	"
	^ self numericValue: 16r84! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:17:02'!
exists
	"
	Character exists
	"
	^ self numericValue: 16r82! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:17:03'!
forAll
	"
	Character forAll
	"
	^ self numericValue: 16r80! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:17:06'!
greaterNotEqual
	"
	Character greaterNotEqual
	"
	^ self numericValue: 16r9D! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:17:08'!
greaterOrEqual
	"
	Character greaterOrEqual
	"
	^ self numericValue: 16r99! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:17:27'!
greaterOverEqual
	"
	Character greaterOverEqual
	"
	^ self numericValue: 16r9B! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:17:30'!
identical
	"
	Character identical
	"
	^ self numericValue: 16r95! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:17:32'!
infinity
	"
	Character infinity
	"
	^ self numericValue: 16r85! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:17:35'!
integral
	"
	Character integral
	"
	^ self numericValue: 16r92! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:17:37'!
lessNotEqual
	"
	Character lessNotEqual
	"
	^ self numericValue: 16r9C! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:17:38'!
lessOrEqual
	"
	Character lessOrEqual
	"
	^ self numericValue: 16r98! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:17:40'!
lessOverEqual
	"
	Character lessOverEqual
	"
	^ self numericValue: 16r9A! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:17:45'!
notEqual
	"
	Character notEqual
	"
	^ self numericValue: 16r94! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:17:47'!
notIdentical
	"
	Character notIdentical
	"
	^ self numericValue: 16r96! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:17:50'!
odot
	"
	Character odot
	"
	^ self numericValue: 16r8E! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:17:51'!
oplus
	"
	Character oplus
	"
	^ self numericValue: 16r8F! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:17:53'!
otimes
	"
	Character otimes
	"
	^ self numericValue: 16r90! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:17:58'!
partial
	"
	Character partial
	"
	^ self numericValue: 16r81! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:18:01'!
strictlyEquivalent
	"
	Character strictlyEquivalent
	"
	^ self numericValue: 16r97! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:18:03'!
summation
	"
	Character summation
	"
	^ self numericValue: 16r91! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'jmv 9/2/2016 11:18:07'!
times
	"
	Character times
	"
	^ self numericValue: 16rD7! !


!InputSensor methodsFor: 'keyboard' stamp: 'jmv 9/2/2016 11:08:09'!
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
				print: char numericValue;
				space.
			self isMouseButton3Pressed ifTrue: [ s nextPutAll: ' mouseButton3/blue/tertiary/halo ' ].
			self isMouseButton2Pressed ifTrue: [ s nextPutAll: ' mouseButton2/yellow/secondary/menu ' ].
			self isMouseButton1Pressed ifTrue: [ s nextPutAll: ' mouseButton1/red/primary ' ].
			self shiftPressed ifTrue: [ s nextPutAll: ' shift ' ].
			self controlKeyPressed ifTrue: [ s nextPutAll: ' control/ctrl ' ].
			(self primMouseButtons anyMask: 32) ifTrue: [ s nextPutAll: ' macOption ' ].
			self commandAltKeyPressed ifTrue: [ s nextPutAll: ' macCommand/winAlt ' ]])
		displayAt: 10 @ 10 ].! !

!InputSensor methodsFor: 'private' stamp: 'jmv 9/2/2016 11:02:50'!
characterForKeycode: keycode
	"Map the given keycode to a Smalltalk character object. Encoding:
		A keycode is 12 bits:   <4 modifer bits><8 bit ISO character>
		Modifier bits are:       <command><option><control><shift>"

	"NOTE: the command and option keys are specific to the Macintosh and may not have equivalents on other platforms."

	keycode ifNil: [ ^nil ].
	^ Character numericValue: (keycode bitAnd: 16rFF)! !


!EventSensor methodsFor: 'test' stamp: 'jmv 9/2/2016 11:02:36'!
printEventBuffer: evtBuf

	| type buttons macRomanCode modifiers position pressType stamp unicodeCodePoint |
	type _ evtBuf first.
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time localMillisecondClock ].
	type = EventSensor eventTypeMouse
		ifTrue: [
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
			Transcript
				newLine;
				show: type;
				show: ' macRomanCode:', macRomanCode printString, '-', 
					(Character numericValue: (Character macRomanToLatin1: macRomanCode)) asString, '-';
				show: ' unicodeCodePoint:', unicodeCodePoint printString.
			(Character iso8859s15CodeForUnicodeCodePoint: unicodeCodePoint) ifNotNil: [ :latin15 |
				Transcript show: '-', (Character numericValue: latin15) asString, '-' ].
			Transcript
				show: ' modifiers:', modifiers printString.
			(modifiers anyMask: 8) ifTrue: [ Transcript show: ' [commandWinAlt]' ].
			(modifiers anyMask: 4) ifTrue: [ Transcript show: ' [macOption]' ].
			(modifiers anyMask: 2) ifTrue: [ Transcript show: ' [control]' ].
			(modifiers anyMask: 1) ifTrue: [ Transcript show: ' [shift]' ].
			].! !

!EventSensor methodsFor: 'test' stamp: 'jmv 9/2/2016 11:02:42'!
test
	"
	Sensor test
	"
	| char lastStamp stamp |
	char _ nil.
	lastStamp _ 0.
	[ char = $x ] whileFalse: [
		Sensor nextEvent ifNotNil: [ :evt |
			stamp _ evt at: 2.
			stamp - lastStamp > 300 ifTrue: [ '' print ].
			self printEventBuffer: evt.
			Transcript show: evt.
			(evt first = 2 and: [ evt fourth = 0 ]) ifTrue: [
				char _ Character numericValue: evt third ].
			lastStamp _ stamp]]! !

!EventSensor methodsFor: 'initialization' stamp: 'jmv 9/2/2016 11:08:02'!
initialize
	"Run the I/O process"
	mouseButtons _ 0.
	mousePosition _ 0@0.
	self setInterruptKey: (interruptKey ifNil: [$. numericValue bitOr: 16r0800 ]). 	"cmd-."
	interruptSemaphore _ (Smalltalk specialObjectsArray at: 31) ifNil: [Semaphore new].
	self flushAllButDandDEvents.
	inputSemaphore _ Semaphore new.
	hasInputSemaphore _ false.

	self primSetInputSemaphore: (Smalltalk registerExternalObject: inputSemaphore).
	self installInterruptWatcher.
	self installEventTickler.
	self flushAllButDandDEvents.

	"Attempt to discover whether the input semaphore is actually being signaled."
	hasInputSemaphore _ false.
	inputSemaphore initSignals! !


!CharacterSet methodsFor: 'collection ops' stamp: 'jmv 9/2/2016 11:06:42'!
add: aCharacter
	map at: aCharacter numericValue+1  put: 1.! !

!CharacterSet methodsFor: 'collection ops' stamp: 'jmv 9/2/2016 11:06:47'!
includes: aCharacter
	^(map at: aCharacter numericValue + 1) > 0! !

!CharacterSet methodsFor: 'collection ops' stamp: 'jmv 9/2/2016 11:06:50'!
remove: aCharacter
	map at: aCharacter numericValue + 1  put: 0! !


!CharacterSet class methodsFor: 'instance creation' stamp: 'jmv 9/2/2016 11:02:28'!
allCharacters
	"return a set containing all characters"

	| set |
	set _ self new.
	0 to: 255 do: [ :ascii | set add: (Character numericValue: ascii) ].
	^set! !


!String methodsFor: 'accessing' stamp: 'jmv 9/2/2016 11:03:45'!
at: index 
	"Primitive. Answer the Character stored in the field of the receiver
	indexed by the argument. Fail if the index argument is not an Integer or
	is out of bounds. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 63>
	^Character numericValue: (super at: index)! !

!String methodsFor: 'converting' stamp: 'jmv 9/2/2016 11:03:36'!
asUnHtml
	"Strip out all Html stuff (commands in angle brackets <>) and convert
the characters &<> back to their real value.  Leave actual cr and tab as
they were in text."
	| in out char rest did inString |

	"Hack in some minimal workaround for Unicode stuff"
	inString _ self copyReplaceAll: '‚Äô' with: $' asString.
	"Check if we can handle this in #safeValue: in some way..."
	inString = self ifFalse: [ self halt ].
	
	in _ ReadStream on: inString.
	out _ WriteStream on: (String new: self size).
	[ in atEnd ] whileFalse: [
		in peek = $<
			ifTrue: [in unCommand] 	"Absorb <...><...>"
			ifFalse: [(char _ in next) = $&
						ifTrue: [rest _ in upTo: $;.
								did _ out position.
								rest = 'lt' ifTrue: [out nextPut: $<].
								rest = 'gt' ifTrue: [out nextPut: $>].
								rest = 'amp' ifTrue: [out nextPut: $&].
								rest = 'deg' ifTrue: [out nextPut: $°].
								rest = 'quot' ifTrue: [out nextPut: $"].
								rest first = $# ifTrue: [ out nextPut: (Character numericValue: rest asInteger) ].
								did = out position ifTrue: [
									out nextPut: $&; nextPutAll: rest.
									"self error: 'unknown encoded HTML char'."
									"Please add it to this method"]]
						ifFalse: [out nextPut: char]].
		].
	^ out contents! !


!SystemDictionary methodsFor: 'special objects' stamp: 'jmv 9/2/2016 11:04:26'!
recreateSpecialObjectsArray
	"Smalltalk recreateSpecialObjectsArray"
	
	"To external package developers:
	**** DO NOT OVERRIDE THIS METHOD.  *****
	If you are writing a plugin and need additional special object(s) for your own use, 
	use addGCRoot() function and use own, separate special objects registry "
	
	"The Special Objects Array is an array of objects used by the Squeak virtual machine.
	 Its contents are critical and accesses to it by the VM are unchecked, so don't even
	 think of playing here unless you know what you are doing."
	| newArray |
	newArray := Array new: 60.
	"Nil false and true get used throughout the interpreter"
	newArray at: 1 put: nil.
	newArray at: 2 put: false.
	newArray at: 3 put: true.
	"This association holds the active process (a ProcessScheduler)"
	newArray at: 4 put: (self associationAt: #Processor).
	"Numerous classes below used for type checking and instantiation"
	newArray at: 5 put: Bitmap.
	newArray at: 6 put: SmallInteger.
	newArray at: 7 put: String.
	newArray at: 8 put: Array.
	newArray at: 9 put: Smalltalk.
	newArray at: 10 put: BoxedFloat64.
	newArray at: 11 put: MethodContext.
	newArray at: 11 put: (self at: #MethodContext ifAbsent: [self at: #Context]).
	newArray at: 12 put: nil. "was BlockContext."
	newArray at: 13 put: Point.
	newArray at: 14 put: LargePositiveInteger.
	newArray at: 15 put: Display.
	newArray at: 16 put: Message.
	newArray at: 17 put: CompiledMethod.
	newArray at: 18 put: ((self specialObjectsArray at: 18) ifNil: [Semaphore new]). "low space Semaphore"
	newArray at: 19 put: Semaphore.
	newArray at: 20 put: Character.
	newArray at: 21 put: #doesNotUnderstand:.
	newArray at: 22 put: #cannotReturn:.
	newArray at: 23 put: nil. "This is the process signalling low space."
	"An array of the 32 selectors that are compiled as special bytecodes,
	 paired alternately with the number of arguments each takes."
	newArray at: 24 put: #(	#+ 1 #- 1 #< 1 #> 1 #<= 1 #>= 1 #= 1 #~= 1
							#* 1 #/ 1 #\\ 1 #@ 1 #bitShift: 1 #// 1 #bitAnd: 1 #bitOr: 1
							#at: 1 #at:put: 2 #size 0 #next 0 #nextPut: 1 #atEnd 0 #== 1 #class 0
							#blockCopy: 1 #value 0 #value: 1 #do: 1 #new 0 #new: 1 #x 0 #y 0 ).
	"An array of the 255 Characters in ascii order.
	 Cog inlines table into machine code at: prim so do not regenerate it."
"	newArray at: 25 put: ((0 to: 255) collect: [:ascii | Character numericValue: ascii])."
" 	 This is nil in Spur, which has immediate Characters."
	newArray at: 25 put: (self specialObjectsArray at: 25).
	newArray at: 26 put: #mustBeBoolean.
	newArray at: 27 put: ByteArray.
	newArray at: 28 put: Process.
	"An array of up to 31 classes whose instances will have compact headers; an empty array in Spur"
	newArray at: 29 put: self compactClassesArray.
	newArray at: 30 put: ((self specialObjectsArray at: 30) ifNil: [Semaphore new]). "delay Semaphore"
	newArray at: 31 put: ((self specialObjectsArray at: 31) ifNil: [Semaphore new]). "user interrupt Semaphore"

	"Prototype instances that can be copied for fast initialization"
	"(jmv) Maybe needed to share cod with V3 (Non-Spur?)"
"	newArray at: 32 put: Float new.
	newArray at: 33 put: (LargePositiveInteger new: 4).
	newArray at: 34 put: Point new."
	"Entries 32 - 34 unreferenced. Previously these contained prototype instances to be copied for fast initialization"
	newArray at: 32 put: nil. "was the prototype Float"
	newArray at: 33 put: nil. "was the prototype 4-byte LargePositiveInteger"
	newArray at: 34 put: nil. "was the prototype Point"

	newArray at: 35 put: #cannotInterpret:.

	"Note: This must be fixed once we start using context prototypes (yeah, right)"
	"(MethodContext new: CompiledMethod fullFrameSize)."
	"(jmv) Maybe needed to share cod with V3 (Non-Spur?)"
	newArray at: 36 put: (self specialObjectsArray at: 36). "Is the prototype MethodContext (unused by the VM)"
	newArray at: 36 put: nil. "was the prototype MethodContext"

	newArray at: 37 put: BlockClosure.

	"(BlockContext new: CompiledMethod fullFrameSize)."
	"(jmv) Maybe needed to share cod with V3 (Non-Spur?)"
	newArray at: 38 put: (self specialObjectsArray at: 38). "Is the prototype BlockContext (unused by the VM)"
	newArray at: 38 put: nil. "was the prototype BlockContext"

	"array of objects referred to by external code"
	newArray at: 39 put: (self specialObjectsArray at: 39).	"preserve external semaphores"
	newArray at: 40 put: nil. "Reserved for Mutex in Cog VMs"
	newArray at: 41 put: ((self specialObjectsArray at: 41) ifNil: [LinkedList new]). "Reserved for a LinkedList instance for overlapped calls in CogMT"
	newArray at: 42 put: ((self specialObjectsArray at: 42) ifNil: [Semaphore new]). "finalization Semaphore"
	newArray at: 43 put: LargeNegativeInteger.
	"External objects for callout.
	 Note: Written so that one can actually completely remove the FFI."
	newArray at: 44 put: (self at: #ExternalAddress ifAbsent: []).
	newArray at: 45 put: (self at: #ExternalStructure ifAbsent: []).
	newArray at: 46 put: (self at: #ExternalData ifAbsent: []).
	newArray at: 47 put: (self at: #ExternalFunction ifAbsent: []).
	newArray at: 48 put: (self at: #ExternalLibrary ifAbsent: []).
	newArray at: 49 put: #aboutToReturn:through:.
	newArray at: 50 put: #run:with:in:.
	"51 reserved for immutability message"
	newArray at: 51 put: #attemptToAssign:withIndex:.
	newArray at: 52 put: #(nil "nil => generic error" #'bad receiver'
							#'bad argument' #'bad index'
							#'bad number of arguments'
							#'inappropriate operation'  #'unsupported operation'
							#'no modification' #'insufficient object memory'
							#'insufficient C memory' #'not found' #'bad method'
							#'internal error in named primitive machinery'
							#'object may move' #'resource limit exceeded'
							#'object is pinned' #'primitive write beyond end of object').
	"53 to 55 are for Alien"
	newArray at: 53 put: (self at: #Alien ifAbsent: []).
	newArray at: 54 put: #invokeCallbackContext:. "use invokeCallback:stack:registers:jmpbuf: for old Alien callbacks."
	newArray at: 55 put: (self at: #UnsafeAlien ifAbsent: []).

	"Weak reference finalization"
	newArray at: 56 put: (self at: #WeakFinalizer ifAbsent: []).
	"(jmv) Maybe needed to share cod with V3 (Non-Spur?)"
	"Used to be WeakFinalizationList for WeakFinalizationList hasNewFinalization, obsoleted by ephemeron support."
	newArray at: 56 put: nil.

	"reserved for foreign callback process"
	newArray at: 57 put: (self specialObjectsArray at: 57 ifAbsent: []).

	newArray at: 58 put: #unusedBytecode.
	"59 reserved for Sista counter tripped message"
	newArray at: 59 put: #conditionalBranchCounterTrippedOn:.
	"60 reserved for Sista class trap message"
	newArray at: 60 put: #classTrapFor:.

	"Now replace the interpreter's reference in one atomic operation"
	self specialObjectsArray become: newArray
	! !


!RWBinaryOrTextStream methodsFor: 'as yet unclassified' stamp: 'jmv 9/2/2016 11:08:20'!
next

	| byte |
	^ isBinary 
			ifTrue: [byte _ super next.
				 byte ifNil: [nil] ifNotNil: [byte numericValue]]
			ifFalse: [super next].
! !


!Transcripter methodsFor: 'command line' stamp: 'jmv 9/2/2016 11:04:30'!
request: prompt
	| startPos char contents return |
	return _ Character numericValue: InputSensor returnKey.
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


!DataStream methodsFor: 'write and read' stamp: 'jmv 9/2/2016 11:07:29'!
writeStringOld: aString
	"PRIVATE -- Write the contents of a String."

	| length |
	aString size < 16384 
		ifTrue: [
			(length _ aString size) < 192
				ifTrue: [byteStream nextPut: length]
				ifFalse: 
					[byteStream nextPut: (length // 256 + 192).
					byteStream nextPut: (length \\ 256)].
			aString do: [:char | byteStream nextPut: char numericValue]]
		ifFalse: [self writeByteArray: aString].	"takes more space"! !


!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 9/2/2016 11:08:26'!
typeTableAt: aCharacter
	^typeTable at: aCharacter numericValue ifAbsent:[#xLetter]! !

!Scanner methodsFor: 'multi-character scans' stamp: 'jmv 9/2/2016 11:08:31'!
xIllegal
	"An illegal character was encountered"
	self notify: 'Illegal character (char code ' , hereChar numericValue printString, ' ', hereChar numericValue hex , ')' at: mark! !


!Scanner class methodsFor: 'cached class state' stamp: 'jmv 9/2/2016 11:02:59'!
initTypeTable
	| newTable |
	newTable := Array new: 256 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"

	1 to: 255
		do: [:index |
			(Character numericValue: index) isValidInIdentifiers
				ifTrue: [
					"Digits and $_ are overwritten later"
					newTable at: index put: #xLetter ]].

	newTable atAll: ($0 asciiValue to: $9 asciiValue) put: #xDigit.
	newTable atAll:  '!!%&+-*/\±◊˜¨≠Ø,<=>îïñóòôöõúù´ª?@~ÄÅÇÉéèêëíìûü∞∑' asByteArray put: #xBinary.

	newTable at: Scanner doItCharacterValue put: #doIt.
	newTable at: $" asciiValue put: #xDoubleQuote.
	newTable at: $# asciiValue put: #xLitQuote.
	newTable at: $$ asciiValue put: #xDollar.
	newTable at: $' asciiValue put: #xSingleQuote.
	newTable at: $: asciiValue put: #xColon.
	newTable at: $( asciiValue put: #leftParenthesis.
	newTable at: $) asciiValue put: #rightParenthesis.
	newTable at: $. asciiValue put: #period.
	newTable at: $; asciiValue put: #semicolon.
	newTable at: $[ asciiValue put: #leftBracket.
	newTable at: $] asciiValue put: #rightBracket.
	newTable at: ${ asciiValue put: #leftBrace.
	newTable at: $} asciiValue put: #rightBrace.
	newTable at: $^ asciiValue put: #upArrow.
	newTable at: $_ asciiValue put: #xUnderscore.
	newTable at: $| asciiValue put: #verticalBar.
	TypeTable := newTable "bon voyage!!"

	"
	Scanner initTypeTable
	"! !


!Editor class methodsFor: 'class initialization' stamp: 'jmv 9/2/2016 11:07:58'!
initializeCmdShortcuts
	"Initialize the (unshifted) command-key (or alt-key if not on Mac) shortcut table."
	"NOTE: if you don't know what your keyboard generates, use Sensor test"
	"
	Editor initialize
	"

	cmdShortcuts _ Array new: 256 withAll: #noop:.

	self basicCmdShortcutsSpec do: [ :ary |
		cmdShortcuts at: ary first numericValue + 1 put: ary second ].

	self cmdShortcutsSpec do: [ :ary |
		cmdShortcuts at: ary first numericValue + 1 put: ary second ]! !


!TrieNode class methodsFor: 'constants' stamp: 'jmv 9/2/2016 11:04:49'!
characterForLeaf
	"A special character to mean leaf node. Must have lower code than alphabetic chars,
	so in the trie 'car' comes before 'cars'"
	^Character numericValue: 0! !


!Clipboard methodsFor: 'private' stamp: 'jmv 9/2/2016 11:22:05'!
retrieveIdOrStringFromOS
	"Use a specific content type if ExtendedClipboard is active.
	Otherwise, use regular clipboard primitives"
	| primitiveFormat |

	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			"Answer nil if no id was stored"
			^ interface retrieveId ]].
		
	primitiveFormat _ self primitiveClipboardString asByteArray.

	"Clipboard primitives answer an empty string if there is no string in OS clipboard.
	We prefer nil"
	primitiveFormat isEmpty ifTrue: [ ^nil ].

	"The VM uses UTF-8 for clipboard"
	^(String fromUtf8: primitiveFormat hex: false trimLastNull: true) withCuisLineEndings! !


!BitBlt methodsFor: 'private' stamp: 'jmv 9/2/2016 11:06:18'!
primDisplayString: aString from: startIndex to: stopIndex map: glyphMap xTable: xTable kern: kernDelta
	| ascii glyph |
	<primitive:'primitiveDisplayString' module:'BitBltPlugin'>
	startIndex to: stopIndex do:[:charIndex|
		ascii _ (aString at: charIndex) numericValue.
		glyph _ glyphMap at: ascii + 1.
		sourceX _ xTable at: glyph + 1.
		width _ (xTable at: glyph + 2) - sourceX.
		self copyBits.
		destX _ destX + width + kernDelta.
	].! !


!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 9/2/2016 11:03:06'!
ensureCleanBold 
	"This ensures that all character glyphs have at least one pixel of white space on the right
	so as not to cause artifacts in neighboring characters in bold or italic."

	| wider glyph |
	emphasis = 0 ifFalse: [^ self].
	minAscii to: maxAscii do:
		[:i | glyph _ self glyphAt: (Character numericValue: i).
		(glyph copy: (glyph boundingBox topRight - (1@0)
					corner: glyph boundingBox bottomRight)) isAllWhite ifFalse: [
			wider _ Form extent: (glyph width + 1)@glyph height depth: glyph depth.
			glyph depth > 1 ifTrue: [wider fillWhite].
			glyph displayOn: wider.
			self glyphAt: (Character numericValue: i) put: wider]].
"
StrikeFont allInstancesDo: [:f | f ensureCleanBold].
(StrikeFont familyName: 'NewYork' size: 21) ensureCleanBold.
StrikeFont shutDown.  'Flush synthetic fonts'.
"
! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 9/2/2016 11:03:16'!
fillZeroWidthSlots
	| nullGlyph |
	"Note: this is slow because it copies the font once for every replacement."

	nullGlyph _ (Form extent: 1@glyphs height) fillGray.
	"Now fill the empty slots with narrow box characters."
	minAscii to: maxAscii do:
		[:i | (self widthOf: (Character numericValue: i)) = 0 ifTrue:
			[self glyphAt: (Character numericValue: i) put: nullGlyph]].
! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 9/2/2016 11:03:22'!
makeControlCharsVisible
	| glyph |
	self characterToGlyphMap.
	glyph _ self glyphAt: (Character space).
	glyph border: glyph boundingBox width: 1 fillColor: Color blue.
	self glyphAt: (Character numericValue: 134) put: glyph.
	
	"Keep tab(9), lf(10), cr(13) and space(32) transparent or whatever the user chose"
	#(0 1 2 3 4 5 6 7 8 11 12 14 15 16 17 18 19 20 21 22 23 24 25 26 27)
		do: [ :ascii |
			characterToGlyphMap at: ascii + 1 put: 134 ]! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 9/2/2016 11:03:27'!
makeCrVisible
	| glyph |
	self characterToGlyphMap.
	glyph _ self glyphAt: (Character numericValue: 182).
	glyph border: glyph boundingBox width: 1 fillColor: Color blue.
"	glyph _ glyph reverse."
	self glyphAt: (Character numericValue: 133) put: glyph.
	characterToGlyphMap at: 14 put: 133! !

!StrikeFont methodsFor: 'glyphs' stamp: 'jmv 9/2/2016 11:03:31'!
makeLfVisible
	| glyph |
	self characterToGlyphMap.
	glyph _ self glyphAt: (Character numericValue: 163).
	glyph border: glyph boundingBox width: 1 fillColor: Color blue.
"	glyph _ glyph reverse."
	self glyphAt: (Character numericValue: 132) put: glyph.
	characterToGlyphMap at: 11 put: 132! !

!StrikeFont methodsFor: 'building' stamp: 'jmv 9/2/2016 11:03:02'!
buildFromForm: allGlyphs data: data name: aString

	| x shortUnderscore |
	pointSize _ data first asNumber.
	ascent _ data second asNumber.
	descent _ data third asNumber.
	
	minAscii _ 28.
	maxAscii _ 255.
	name _ aString.
	type _ 0.  "ignored for now"
	superscript _ ascent - descent // 3.	
	subscript _ descent - ascent // 3.	
	emphasis _ 0.

	xTable _ (Array new: 258) atAllPut: 0.
	maxWidth _ 0.
	glyphs _ allGlyphs depth > 16 ifTrue: [ allGlyphs asFormOfDepth: 16 ] ifFalse: [ allGlyphs ].
	x _ 0.
	minAscii to: maxAscii+1 do: [ :i |
		x _ (data at: i-minAscii+4) asNumber.
		xTable at: i+1 put: x].
	xTable at: 258 put: x.
	self reset.
	derivativeFonts _ nil.
	
	"Replace glyph for 127 (a box) with a short version of the underscore (used to optionally mark subscript in code)"
	shortUnderscore _ self glyphAt: $_.
	shortUnderscore _ shortUnderscore copy: (0@0 extent: 1@shortUnderscore height).
	self glyphAt: (Character numericValue: 127) put: shortUnderscore! !


!CharacterScanner class methodsFor: 'class initialization' stamp: 'jmv 9/2/2016 11:06:36'!
initialize
	"
	CharacterScanner initialize
	"
	| stopConditions |
	stopConditions _ Array new: 258.
	stopConditions atAllPut: nil.
	stopConditions at: Character space numericValue + 1 put: nil.
	stopConditions at: Character tab numericValue + 1 put: #tab.

	"This line makes lf be shown as a newLine"
	stopConditions at: Character lf numericValue + 1 put: #doNewLine.

	"This line makes cr be shown as a newLine"
	stopConditions at: Character cr numericValue + 1 put: #doNewLine.

	stopConditions at: CharacterScanner endOfRunCode put: #endOfRun.
	stopConditions at: CharacterScanner crossedXCode put: #crossedX.
	DefaultStopConditions _ stopConditions.

	StopConditionsWithSpace _ DefaultStopConditions copy.
	StopConditionsWithSpace at: Character space numericValue + 1 put: #space.

	StopConditionsWithPaddedSpace _ DefaultStopConditions copy.
	StopConditionsWithPaddedSpace at: Character space numericValue + 1 put: #paddedSpace.
	
	! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 9/2/2016 11:02:56'!
displayBulletTextLeft: texLeft number: bulletNumber
	"texLeft is relative to the morph currently being drawn"

	| pattern i c j s bullet bulletPos bulletSize prefix |
	pattern _ paragraphStyle listBulletPattern.
	bullet _ pattern.
	(i _ pattern indexOf: $%) > 0
		ifTrue: [ bullet _ bulletNumber asString]
		ifFalse: [
			(i _ pattern indexOf: $z) > 0
				ifTrue: [ bullet _ (Character numericValue: 96 + bulletNumber) asString ]
				ifFalse: [
					(i _ pattern indexOf: $Z) > 0
						ifTrue: [ bullet _ (Character numericValue: 64 + bulletNumber) asString ]]].
	prefix _ 0.
	i > 0 ifTrue: [
		c _ pattern at: i.
		j _ i.
		s _ pattern size.
		[ j <= s and: [ (pattern at: j) = c ] ] whileTrue: [ j _ j + 1 ].
		j _ j - 1.
		bulletSize _ j-i+1.
		prefix _ bulletSize - bullet size max: 0.
		bullet size > bulletSize ifTrue: [
			bullet _ bullet copyFrom: bullet size - bulletSize + 1 to: bullet size ].
		bullet _ (pattern copyFrom: 1 to: i-1), bullet, (pattern copyFrom: j+1 to: pattern size) ].
	bulletPos _ paragraphStyle firstIndent + texLeft + ((font widthOf: $9) * prefix)@destY.
	canvas
		drawString: bullet
		from: 1
		to: bullet size
		at: bulletPos
		font: font
		color: foregroundColor! !


!HierarchicalListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 9/2/2016 11:08:06'!
arrowKey: aChar
	"Handle a keyboard navigation character. Answer true if handled, false if not."
	| keyEvent answer nextSelection oldSelection |
	answer _ false.
	keyEvent _ aChar numericValue.
	oldSelection _ self getSelectionIndex.
	nextSelection _ oldSelection.
	keyEvent = 31 ifTrue: [		"down"
		nextSelection _ oldSelection + 1 ].
	keyEvent = 30 ifTrue: [		"up"
		nextSelection _ (oldSelection - 1 max: 1) ].
	keyEvent = 1 ifTrue: [		"home"
		nextSelection _ 1 ].
	keyEvent = 4 ifTrue: [		"end"
		nextSelection _ scroller submorphs size ].
	keyEvent = 11 ifTrue: [		"page up"
		nextSelection _ (oldSelection - self numSelectionsInView max: 1) ].
	keyEvent = 12 ifTrue: [		"page down"
		nextSelection _ oldSelection + self numSelectionsInView ].
	keyEvent = 29 ifTrue: [		"right"
		selectedMorph ifNotNil: [
			(selectedMorph canExpand and: [ selectedMorph isExpanded not ])
				ifTrue: [
					self toggleExpandedState: selectedMorph.
					answer _ true ]
				ifFalse: [ nextSelection _ oldSelection + 1 ]] ].
	keyEvent = 28 ifTrue: [		"left"
		selectedMorph ifNotNil: [
			selectedMorph isExpanded
				ifTrue: [
					self toggleExpandedState: selectedMorph.
					answer _ true ]
				ifFalse: [ nextSelection _ (oldSelection - 1 max: 1) ]].
		^ true ].
	nextSelection = oldSelection ifFalse: [

		"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
		self highlightedMorph: (self listMorphAt: nextSelection).

		"Update the model in next world cycle, so user gets the immediate feedback."
		WorldState addDeferredUIMessage: [
			self setSelectionIndex: nextSelection ].
		answer _ true ].
	^ answer! !


!PluggableListMorph methodsFor: 'events' stamp: 'jmv 9/2/2016 11:08:17'!
keyStroke: aKeyboardEvent 
	"Process keys"
	
	| aCharacter |
	(Theme current keyStroke: aKeyboardEvent morph: self)
		ifTrue: [^ self].
	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self scrollByKeyboard: aKeyboardEvent) 
		ifTrue: [ ^self ].
	aCharacter _ aKeyboardEvent keyCharacter.
	(self arrowKey: aCharacter)
		ifTrue: [ ^self ].
	aCharacter numericValue = 27 ifTrue: [	" escape key"
		^ self mouseButton2Activity].
	aKeyboardEvent anyModifierKeyPressed
		ifTrue: [
			(self keystrokeAction: aCharacter)
				ifTrue: [ ^self ]].
	^ self keyboardSearch: aCharacter! !

!PluggableListMorph methodsFor: 'keyboard navigation' stamp: 'jmv 9/2/2016 11:08:12'!
arrowKey: aChar
	"Process the up and down arrows in a list pane.  Note that the listView tells us what index variable, how to get the list, and how to move the index."
	| keyEvent oldSelection nextSelection max min howMany answer w |
	answer _ false.
	keyEvent _ aChar numericValue.
	oldSelection _ self getCurrentSelectionIndex.
	nextSelection _ oldSelection.
	max _ self maximumSelection.
	min _ self minimumSelection.
	howMany _ self numSelectionsInView.	"get this exactly??"

	keyEvent = 31 ifTrue: [
		"down-arrow; move down one, wrapping to top if needed"
		nextSelection _ oldSelection + 1.
		nextSelection > max ifTrue: [ nextSelection _ 1 ]].

	keyEvent = 30 ifTrue: [
		"up arrow; move up one, wrapping to bottom if needed"
		nextSelection _ oldSelection - 1.
		nextSelection < 1 ifTrue: [ nextSelection _ max ]].

	keyEvent = 1 ifTrue: [ nextSelection _ 1 ].			"home"
	keyEvent = 4 ifTrue: [ nextSelection _ max ].		"end"
	keyEvent = 11 ifTrue: [ nextSelection _ min max: oldSelection - howMany ].		"page up"
	keyEvent = 12 ifTrue: [ nextSelection _ oldSelection + howMany min: max ].		"page down"
	nextSelection = oldSelection ifFalse: [
		w _ self owningWindow.
		(w isNil or: [ w okToChange ]) ifTrue: [ "No change if model is locked"

			"Highlight the row to be selected, for immediate user feedback in case the model takes a while to update the view."
			self listMorph highlightedRow: nextSelection.

			"Update the model in next world cycle, so user gets the immediate feedback."
			WorldState addDeferredUIMessage: [
				self changeModelSelection: nextSelection ].
			answer _ true ]].
	^answer! !


!HandMorph methodsFor: 'private events' stamp: 'jmv 9/2/2016 11:02:45'!
generateKeyboardEvent: evtBuf 
	"Generate the appropriate mouse event for the given raw event buffer"

	| buttons modifiers type keyValue pressType stamp |
	stamp _ evtBuf second.
	stamp = 0 ifTrue: [stamp _ Time localMillisecondClock].
	(evtBuf sixth <= 0 or: [
		(keyValue _ (Character iso8859s15CodeForUnicodeCodePoint: evtBuf sixth)) isNil ])
			ifTrue: [ keyValue _ Character macRomanToLatin1: evtBuf third ].

	(Sensor peekEvent) ifNotNil: [ :nxt |
		"start: Combining diacritical marks (i.e. accents in the Linux VM)"
		(nxt fourth = EventSensor eventKeyDown and:[ nxt third >255 ])
			ifTrue: [
				keyValue _ ((Character numericValue: keyValue) withDiacriticalMark: nxt third) iso8859s15Code.
				Sensor nextEvent; nextEvent; nextEvent
			].
		"end: Combining diacritical marks (i.e. accents in the Linux VM)"

		"start: Spurious LF after CR on Ctrl-Enter on Windows VM"
		((evtBuf fourth = EventSensor eventKeyChar and:[ evtBuf third = 13 ]) and: [
			(nxt fourth = EventSensor eventKeyChar and:[ nxt third = 10 ]) ] )
				ifTrue: [
					Sensor nextEvent "print "].
		"end: Spurious LF after CR on Ctrl-Enter on Windows VM"
	].

	modifiers _ evtBuf fifth.
	pressType _ evtBuf fourth.
	pressType = EventSensor eventKeyDown ifTrue: [
		type _ #keyDown.
		lastKeyDownValue _ keyValue].
	pressType = EventSensor eventKeyUp ifTrue: [type _ #keyUp].
	pressType = EventSensor eventKeyChar ifTrue: [
		type _ #keystroke.
		"If Control key pressed, and the VM answers a code below 27,
		 it means it did the translation, convert it back to regular character:
		We want to handle the meaning of ctrl ourselves."
		(modifiers anyMask: 2) ifTrue: [		"Control key pressed"
			keyValue < 27 ifTrue: [
				
				"But we don't want to do it for Home/End/PgUp/PgDn, just for alphabetic keys"
				lastKeyDownValue = keyValue ifFalse: [		"If equal, real Home/End/PgUp/PgDn in Windows => don't translate"
					(keyValue + 64 = lastKeyDownValue or: [ 	"If Equal, Ctrl-alphabetic in Windows => do translate"
							lastKeyDownValue < 47 ]) ifTrue: [		"Not on windows. If less (not sure about the bound, but do not translate 48: tab on Mac), alphabetic on Mac => do translate"
						keyValue _ (modifiers anyMask: 1)
							ifFalse: [ keyValue + 96 ]	"shift not pressed: conver to lowercase letter"
							ifTrue: [ keyValue + 64 ]].	"shift pressed: conver to uppercase letter"
					]
				].
			"Act as if command/alt was pressed for some usual Windows ctrl-key combinations"
			(self shouldControlEmulateAltFor: keyValue) ifTrue: [
				modifiers _ modifiers bitOr: 8 ]
			]].
	buttons _ modifiers bitShift: 3.
	^KeyboardEvent new 
		setType: type
		buttons: buttons
		position: self morphPosition
		keyValue: keyValue
		hand: self
		stamp: stamp! !


!SHParserST80 methodsFor: 'character testing' stamp: 'jmv 9/5/2016 17:01:27'!
isBinarySelectorCharacter: aCharacter

	aCharacter isValidInIdentifiers ifTrue: [^false].
	aCharacter isSeparator ifTrue: [^false].

	('"#$'':().;[]{}_'  includes: aCharacter) 
		ifTrue:[^false].
	aCharacter numericValue = Scanner doItCharacterValue ifTrue: [^false "the doIt char"].
	aCharacter numericValue = 0 ifTrue: [^false].
	"Any other char is ok as a binary selector char."
	^true! !

CharacterScanner initialize!
