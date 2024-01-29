'From Cuis6.3 [latest update: #6186] on 9 January 2024 at 12:26:21 pm'!

!Object methodsFor: 'testing' stamp: 'jmv 1/9/2024 12:18:01'!
isCharacter
	"Overridden to return true in Character."
	^ false! !


!CodeProvider class methodsFor: 'services' stamp: 'jmv 1/9/2024 12:18:14'!
normalizeCodeForCodeFiles: aTextOrString
	"Adapt edited code to be suitable or logging.
	Answer is a plain text.
	Additionally, if code was modified for better look and edition experience, normalize it.
	This may include reverting to ':=' for assignment, '^' for return, and any replacements of
	ASCII by Unicode symbols done by Shout.
	See also SHTextStylerST80 >> #privateConvert
	See also Clipboard >> #storeObject:"

	| answer |
	answer := aTextOrString asPlainString.
	answer := answer copyReplaceAll: `Character smalltalkLeftArrow asString, Character zeroWidthSpace asString` with: ':='.
	answer := answer copyReplaceAll: `Character smalltalkLeftArrow asString` with: ':='.
	answer := answer copyReplaceAll: `Character smalltalkUpArrow asString` with: '^'.
	^answer! !


!Character methodsFor: 'testing' stamp: 'jmv 1/9/2024 12:18:55'!
isSeparator
	"Answer whether the receiver is one of the separator characters"
	"Also see #separators"

	^ #(32 "space"
		9 "tab"
		10 "line feed"
		13 "cr"
		12 "form feed"
		8203 "Character zeroWidthSpace")
			statePointsTo: self codePoint! !


!Character class methodsFor: 'instance creation' stamp: 'jmv 1/9/2024 12:19:02'!
separators
	"Answer the separator characters"
	"Also see #isSeparator"

	^ #(32 "space"
		9 "tab"
		10 "line feed"
		13 "cr"
		12 "form feed"
		8203 "Character zeroWidthSpace")
			collect: [ :v | Character codePoint: v ]! !


!EventSensor methodsFor: 'private-I/O' stamp: 'jmv 1/9/2024 12:19:23'!
fetchMoreEventsDiscardingMouseEvents: discardMouseEvents
	"Fetch more events from the VM"
	| eventBuffer type |

	"Reset input semaphore so clients can wait for the next events after this one."
	inputSemaphore isSignaled
		ifTrue: [
			hasInputSemaphore := true.
			inputSemaphore initSignals ].

	"DebuggingAid""
	dd := Time localMillisecondClock - lastEventPoll.
	"

	eventBuffer := Array new: 8.
	[
		self primGetNextEvent: eventBuffer.
		"DebuggingAid""
		eventBuffer first = 2 ifTrue: [
			k1 := eventBuffer third.
			u1 := eventBuffer sixth.
			pressType := eventBuffer fourth.
			typeS := 'unknown'.
			pressType = EventSensor eventKeyDown ifTrue: [
				typeS := #keyDown].
			pressType = EventSensor eventKeyUp ifTrue: [
				typeS := #keyUp].
			pressType = EventSensor eventKeyChar ifTrue: [
				typeS := #keystroke].
			dd >100 ifTrue: ['-' print. dd := 0 ].
			{ k1 < 256 ifTrue: [Character codePoint: k1] ifFalse: [ '[]']. Character codePoint: k1. k1. k1 hex. '-----'. 
				Character codePoint: u1. u1. u1 hex. '----'. pressType. typeS } print.
			].
		"
		type := eventBuffer at: 1.
		type = EventSensor eventTypeNone ]
			whileFalse: [
				self
					processSensorEvent: eventBuffer
					discardingMouseEvents: discardMouseEvents ].

	"Remember the last time that I checked for events."
	lastEventPoll := Time localMillisecondClock.! !

!EventSensor methodsFor: 'test' stamp: 'jmv 1/9/2024 12:25:41'!
printEventBuffer: evtBuf
	"Print the event buffer, currently only used by the method `test`."

	| type buttons macRomanCode modifiers pressType stamp codePoint |
	type := evtBuf first.
	stamp := evtBuf second.
	stamp = 0 ifTrue: [ stamp := Time millisecondClockValue ]. "VMs report events using #millisecondClockValue"
	type = EventSensor eventTypeMouse
		ifTrue: [ | position |
			position := evtBuf third @ evtBuf fourth.
			buttons := evtBuf fifth.
			modifiers := evtBuf sixth.
			Transcript
				newLine;
				show: 'Mouse';
				show: ' position:', position printString;
				show: ' buttons:', buttons printString;
				show: ' modifiers:', modifiers printString.
			].
	type = EventSensor eventTypeMouseScroll
		ifTrue: [ | delta |
			delta := evtBuf third @ evtBuf fourth.
			buttons := evtBuf fifth.
			modifiers := evtBuf sixth.
			Transcript
				newLine;
				show: 'Scroll';
				show: ' delta:', delta printString;
				show: ' buttons:', buttons printString;
				show: ' modifiers:', modifiers printString.
			].
	type = EventSensor eventTypeKeyboard 
		ifTrue: [
			macRomanCode := evtBuf third.
			codePoint := evtBuf sixth.
			pressType := evtBuf fourth.
			modifiers := evtBuf fifth.
			pressType = EventSensor eventKeyDown ifTrue: [
				type := #keyDown].
			pressType = EventSensor eventKeyUp ifTrue: [
				type := #keyUp].
			pressType = EventSensor eventKeyChar ifTrue: [
				type := #keystroke].
			{type. 'macRoman:'. macRomanCode. macRomanCode hex.
				'Unicode CodePoint:'. codePoint. codePoint hex. Character codePoint: codePoint} print.
			Transcript nextPutAll: ' modifiers: ', modifiers printString.
			(modifiers anyMask: 8) ifTrue: [ Transcript nextPutAll: ' [commandWinAlt]' ].
			(modifiers anyMask: 4) ifTrue: [ Transcript nextPutAll: ' [macOption]' ].
			(modifiers anyMask: 2) ifTrue: [ Transcript nextPutAll: ' [control]' ].
			(modifiers anyMask: 1) ifTrue: [ Transcript nextPutAll: ' [shift]' ].
			''print.
			].! !


!String methodsFor: 'accessing' stamp: 'jmv 1/9/2024 12:12:34'!
at: index put: aCharacter 
	"Primitive. Store the Character in the field of the receiver indicated by
	the index. Fail if the index is not an Integer or is out of bounds, or if
	the argument is not a Character. Essential. See Object documentation
	whatIsAPrimitive."

	<primitive: 64>
	aCharacter isCharacter
		ifTrue: [	
			aCharacter codePoint > 255 ifTrue: [
				self error: 'String only store Latin-1 Characters'].
			index isInteger
				ifTrue: [self errorSubscriptBounds: index]
				ifFalse: [self errorNonIntegerIndex]]
		ifFalse: [self error: 'Strings only store Characters']! !


!UnicodeString methodsFor: 'accessing' stamp: 'jmv 1/9/2024 12:20:43'!
at: codePointIndex
	"Answer the Character with the CodePoint at code point index codePointIndex.
	(not a byte index!!)"

	| byteIndex |
	self boundsCheck: codePointIndex.
	self isAscii ifTrue: [ ^ Character codePoint: (bytes at: codePointIndex) ].
	byteIndex := self byteIndexAt: codePointIndex.
	Character utf8BytesAndCodePointAt: byteIndex in: bytes into: nil into: [ :codePoint |
		^Character codePoint: codePoint ].
	^nil "Invalid UTF-8"! !

!UnicodeString methodsFor: 'accessing' stamp: 'jmv 1/9/2024 12:21:30'!
at: codePointIndex put: aCharacter
	"Store argument at position codePointIndex "

	| byteIndex currentByteCount argumentBytes newBytes |

	self boundsCheck: codePointIndex.
	byteIndex := self byteIndexAt: codePointIndex.
	currentByteCount := self encodedByteCountAt: byteIndex.

	"Fastest path if both old and new are ASCII"
	currentByteCount = 1 ifTrue: [
		aCharacter asciiValue ifNotNil: [ :newAscii |
			bytes at: 	byteIndex put: newAscii.
			^ aCharacter ]].

	argumentBytes := Character utf8BytesOfUnicodeCodePoint: aCharacter codePoint.

	"Avoid creating new bytes object if both old and new require same number of bytes"
	argumentBytes size = currentByteCount ifTrue: [
		bytes replaceFrom: byteIndex to: byteIndex + currentByteCount - 1 with: argumentBytes startingAt: 1.
		strideIndex := nil.
		hash := nil.
		^ aCharacter ].

	"More expensive: needs a new bytes object of different size"
	newBytes := ByteArray new: bytes size - currentByteCount + argumentBytes size.
	newBytes
		replaceFrom: 1 to: byteIndex-1 with: bytes startingAt: 1;
		replaceFrom: byteIndex to: byteIndex + argumentBytes size - 1 with: argumentBytes startingAt: 1;
		replaceFrom: byteIndex + argumentBytes size to: newBytes size with: bytes startingAt: byteIndex + currentByteCount.

	self privateBytes: newBytes codePointCount: count.
	^ aCharacter.! !

!UnicodeString methodsFor: 'accessing' stamp: 'jmv 1/9/2024 12:22:32'!
indexOf: aCharacter startingAt: start ifAbsent: aBlock
	| codePointBytes startByteIndex foundByteIndex answer |
	aCharacter isCharacter ifFalse: [ ^ aBlock value ].
	codePointBytes := Character utf8BytesOfUnicodeCodePoint: aCharacter codePoint.
	startByteIndex := self byteIndexAt: start.
	foundByteIndex := String findString: codePointBytes in: bytes startingAt: startByteIndex.
	answer := self codePointIndexAt: foundByteIndex.
	^answer = 0
		ifTrue: [ aBlock value ]
		ifFalse: [ answer ]! !

!UnicodeString methodsFor: 'copying' stamp: 'jmv 1/9/2024 12:22:13'!
copyWith: aCharacter

	^UnicodeString
		fromUtf8Bytes: bytes, (Character utf8BytesOfUnicodeCodePoint: aCharacter codePoint)
		codePointCount: self size + 1.! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 1/9/2024 12:20:06'!
< aString
	"Answer whether the receiver sorts before aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) = 1].
		(aString isUnicodeString and: [aString isAscii]) ifTrue: [
			"aString is also an ascii UnicodeString. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) = 1]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) = 1! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 1/9/2024 12:20:12'!
<= aString
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) <= 2].
		(aString isUnicodeString and: [aString isAscii]) ifTrue: [
			"aString is also an ascii UnicodeString. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) <= 2]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) <= 2! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 1/9/2024 12:20:17'!
> aString
	"Answer whether the receiver sorts after aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ false ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) = 3].
		(aString isUnicodeString and: [aString isAscii]) ifTrue: [
			"aString is also an ascii UnicodeString. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) = 3]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) = 3! !

!UnicodeString methodsFor: 'comparing' stamp: 'jmv 1/9/2024 12:20:22'!
>= aString
	"Answer whether the receiver sorts after or equal to aString.
	The collation order is case sensitive."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If we are ascii, we have same bytes as equivalent String."
	self isAscii ifTrue: [
		aString isByteString ifTrue: [
			"Do comparison like String does it."
			^ (String compare: bytes with: aString) >= 2].
		(aString isUnicodeString and: [aString isAscii]) ifTrue: [
			"aString is also an ascii UnicodeString. Their bytes are also same as equiv String."
			^ (String compare: bytes with: aString bytes) >= 2]].

"This needs a reasonable implementation for Unicode (this behavior is INCORRECT).
It is also slow.
At least, it is consistent with #< in Character"
self flag: #unicodeNeedsImprovement.
self flag: #slowUnicode.
	^ (UnicodeString compare: self with: aString) >= 2! !

!UnicodeString methodsFor: 'Unicode normalization' stamp: 'jmv 1/9/2024 12:21:51'!
canonicalDecomposition
	"
	https://unicode.org/reports/tr15/#Norm_Forms
	Answer a String in the NFD form 'Normalization Form Decomposed'.

	(Character codePoint: 16r1EBF) asString
	(Character codePoint: 16r1EBF) asString asUtf8Bytes
	(Character codePoint: 16r1EBF) asString basicCanonicalDecomposition asUtf8Bytes
	(Character codePoint: 16r1EBF) asString asCodePoints asArray collect: [ :e | e hex ]
	(Character codePoint: 16r1EBF) asString basicCanonicalDecomposition asCodePoints asArray collect: [ :e | e hex ]

	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]).
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) asUtf8Bytes.
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) basicCanonicalDecomposition asUtf8Bytes.
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) asCodePoints asArray collect: [ :e | e hex ]
	(UnicodeString fromUtf8Bytes: #[195 160 204 155 204 149]) basicCanonicalDecomposition asCodePoints asArray collect: [ :e | e hex ]

	(Character codePoint: 119137) asString basicCanonicalDecomposition asCodePoints
	(Character codePoint: 119135) asString basicCanonicalDecomposition asCodePoints
	"
	^self basicCanonicalDecomposition basicCanonicallyOrdered! !


!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 1/9/2024 12:22:49'!
new: n
	"Answer will contain n null Characters"

	^self fromUtf8Bytes: (ByteArray new: n) codePointCount: n! !


!Encoder methodsFor: 'encoding' stamp: 'jmv 1/9/2024 12:23:23'!
asciiStringLiteral: literalObject
	"Convert UnicodeStrings to Strings if ASCII.
	Recursive for Arrays"
	
	literalObject class == Array ifTrue: [
		^literalObject collect: [ :each | self asciiStringLiteral: each ]].

	"Prefer String over UnicodeString for literals, if ASCII only."
	(literalObject class == UnicodeString and: [ literalObject isAscii ]) ifTrue: [
		^ literalObject asByteString ].

	^literalObject! !


!SmalltalkEditor methodsFor: 'clipboard' stamp: 'jmv 1/9/2024 12:23:46'!
clipboardStringOrText
	| clipContents |
	clipContents := super clipboardStringOrText.
	self replaceUnicodeArrowsWithSmalltalkArrows ifTrue: [
		clipContents := clipContents copyReplaceAll: `(Character codePoint: 16r2190) asString` with: `(Character smalltalkLeftArrow ) asString`.
		clipContents := clipContents copyReplaceAll: `(Character codePoint: 16r2191) asString` with: `(Character smalltalkUpArrow ) asString`.
		].
	^clipContents.! !


!UnicodeReadAdaptor methodsFor: 'reading' stamp: 'jmv 1/9/2024 12:24:18'!
next: n
	"Read n Characters (i.e. n Unicode CodePoints), or until the end of the file.
	Answer them, from current postion to end, seen from our perspective:
	Stream bytes as CodePoints (prefer UTF-8, but answer ISO-8859-15 if invalid UTF-8).
	Answer is an instance of UnicodeString."

	| utf8 codePointCount |
	codePointCount := 0.
	utf8 := ByteArray streamContents: [ :utf8Stream |
		[ codePointCount = n or: [stream atEnd] ] whileFalse: [
			stream
				nextUtf8BytesAndCodePointInto: [ :byte1 :byte2 :byte3 :byte4 |
					utf8Stream nextPut: byte1.
					byte2 ifNotNil: [
						utf8Stream nextPut: byte2.
						byte3 ifNotNil: [
							utf8Stream nextPut: byte3.
							byte4 ifNotNil: [
								utf8Stream nextPut: byte4 ]]].
					codePointCount := codePointCount + 1]
				into: nil ]].
	^UnicodeString fromUtf8Bytes: utf8 codePointCount: codePointCount.! !


!Clipboard methodsFor: 'accessing' stamp: 'jmv 1/9/2024 12:24:29'!
storeObject: anObject
	"Set new contents on the clipboard. Also export to OS.
	anObject can be a:
		String
		Text
		Form
		Morph
		Object.
	OS clipboard supports String. Other formats might be supported if ExtendedClipboardInterface is present and operative."

	| primitiveFormat id |

	"Store a copy of the object. This is appropriate in case the original object is modified after being copied to the clipboard.
	Another copy must be made again when pasting, as the same object could be pasted many times.
	Besides, store the original object, but weakly (so we don't prevent it GCed). The original object might be used in workspaces."
	(anObject isString or: [ anObject is: #Text])
		ifTrue: [
			contents := anObject copyForClipboard.
			contentsOriginalObjectWeakly at: 1 put: nil ]
		ifFalse: [
			contents := anObject copyForClipboard.
			contentsOriginalObjectWeakly at: 1 put: anObject ].
	
	self noteRecentClipping: contents.

	"Store on OS clipboard using ExtendedClipboardInterface if present.
	Warning: This has not yet been adapted to UTF-8 Unicode"
	self extendedClipboardInterface ifNotNil: [ :interface |
		interface canStore ifTrue: [
			id := self idFor: contents.
			contents isString
				ifTrue: [ ^interface storeString: contents id: id ].
			(contents is: #Text)
				ifTrue: [ ^interface storeText: contents id: id ].
			(contents is: #Form)
				ifTrue: [ ^interface storeForm: contents id: id ].
			(contents is: #Morph)
				ifTrue: [ ^interface storeForm: (contents imageForm: 32) id: id ].
			^interface storeString: contents asString id: id ]].

	"Otherwise use the clipboard primitives in the VM"
	"The VM uses UTF-8 for clipboard"
	primitiveFormat := self stringOrIdFor: contents.
	"See also SHTextStylerST80 >> #privateConvert
	See also CodeProvider class >> #normalizeCodeForCodeFiles:	"
	primitiveFormat := primitiveFormat copyReplaceAll: `Character smalltalkLeftArrow asString, Character zeroWidthSpace asString` with: ':='.
	primitiveFormat := primitiveFormat copyReplaceAll: Character smalltalkLeftArrow asString with: ':='.
	primitiveFormat := primitiveFormat copyReplaceAll: Character smalltalkUpArrow asString with: '^'.
	primitiveFormat := primitiveFormat asUtf8Bytes.
	self primitiveClipboardString: primitiveFormat! !


!SHTextStylerST80 methodsFor: 'private' stamp: 'jmv 1/9/2024 12:24:36'!
privateConvertAssignmentsOffset: delta
	"See also CodeProvider class >> #normalizeCodeForCodeFiles:	
	See also Clipboard >> #storeObject:"
	"Turn $_ into a left arrow"

	self
		replaceStringForRangesWithType: #assignment
		with: `Character smalltalkLeftArrow asString`
		offset: delta.
	(Preferences at: #showAssignmentAsLeftArrow)
		ifTrue: [
			"Turn := into a left arrow. Include a #zeroWidthSpace so as not to modify ranges, etc."
			self
				replaceStringForRangesWithType: #ansiAssignment
				with: `Character smalltalkLeftArrow asString, Character zeroWidthSpace asString`
				offset: delta.
			"Turn $^into up arrow"
			self
				replaceStringForRangesWithType: #return
				with: `Character smalltalkUpArrow asString`
				offset: delta ].! !

!methodRemoval: Character #= stamp: 'jmv 1/9/2024 12:12:17'!
Character removeSelector: #=!
!classRemoval: #UnicodeCodePoint stamp: 'jmv 1/9/2024 12:13:32'!
Smalltalk removeClassNamed: #UnicodeCodePoint!

