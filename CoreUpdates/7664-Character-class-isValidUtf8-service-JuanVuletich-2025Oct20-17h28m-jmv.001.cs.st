'From Cuis7.5 [latest update: #7663] on 20 October 2025 at 5:30:22 pm'!

!Character class methodsFor: 'converting' stamp: 'jmv 10/20/2025 16:50:09'!
isValidUtf8: aByteArray
	| s position usedBytes |
	s := aByteArray size.
	position := 1.
	[ position <= s ] whileTrue: [
		usedBytes := Character
			utf8BytesAndCodePointFor: (aByteArray at: position)
			byte2: (position+1 <= s ifTrue: [aByteArray at: position+1])
			byte3: (position+2 <= s ifTrue: [aByteArray at: position+2])
			byte4: (position+3 <= s ifTrue: [aByteArray at: position+3])
			into:nil
			into: nil
			assumeLatinIfInvalid: false.
		usedBytes ifNil: [ ^false ].
		position := position + usedBytes ].
	^true! !

!Character class methodsFor: 'converting' stamp: 'jmv 10/20/2025 16:23:22'!
utf8BytesAndCodePointFor: byte1 byte2: byte2 byte3: byte3 byte4: byte4 into: bytesBlock into: codePointBlock assumeLatinIfInvalid: aBoolean
	"Find next UTF-8 encoded code point in argument bytes.
	Evaluate bytesBlock (if not nil). Arguments are utf8 bytes for the codePoint found.
	Evaluate codePointBlock (if not nil) with codePoint found as argument.
	Answer the number of bytes that comprised this codePoint.
	If there is an invalid UTF-8 sequence:
		if aBoolean == true:
			'This will try to next bytes as UTF-8 if possible. Good if (for instance) a single invalid byte appears in the middle of valid UTF-8.'.
			consider the first byte an ISO-8859-1 Character, and answer 1.
		else
			'This is useful when validating if a byte sequence is valid UTF-8 or not'.
			answer nil
	Skip codePoint U+FEFF (BOM, unneededly added by Win clipboard). Still, answer the number of bytes of the BOM sequence, to allow senders to update an index in an iteration.
	See senders."

	| codePoint |
	byte1 < 128 ifTrue: [	"single byte"
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: nil value: nil value: nil ].
		codePointBlock ifNotNil: [
			codePointBlock value: byte1 ].
		^ 1 ].
	
	"At least 2 bytes"
	(byte2 notNil and: [ (byte2 bitAnd: 16rC0) = 16r80 ]) ifFalse: [
		"invalid UTF-8. Assume byte1 is ISO8859-1, Latin-1, or answer nil."
		^ aBoolean
			ifTrue: [ self utf8BytesAndCodePointForIso8859s1: byte1 into: bytesBlock into: codePointBlock ]].
	(byte1 bitAnd: 16rE0) = 16rC0  ifTrue: [ "two bytes"
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: byte2 value: nil value: nil ].
		codePointBlock ifNotNil: [
			codePoint := ((byte1 bitAnd: 31) bitShift: 6) + (byte2 bitAnd: 63).
			codePointBlock value: codePoint ].
		^2 ].
	
	"At least 3 bytes"
	(byte3 notNil and: [ (byte3 bitAnd: 16rC0) = 16r80 ]) ifFalse: [
		"invalid UTF-8. Assume byte1 is ISO8859-1, Latin-1, or answer nil."
		^ aBoolean
			ifTrue: [ self utf8BytesAndCodePointForIso8859s1: byte1 into: bytesBlock into: codePointBlock ]].
	(byte1 bitAnd: 16rF0) = 16rE0  ifTrue: [ "three bytes"
		codePoint :=  ((byte1 bitAnd: 15) bitShift: 12) + ((byte2 bitAnd: 63) bitShift: 6) + (byte3 bitAnd: 63).
		codePoint = 16rFEFF ifTrue: [ "BOM. Ignore it. Skip ti."
			^3 ].
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: byte2 value: byte3 value: nil ].
		codePointBlock ifNotNil: [
			codePointBlock value: codePoint ].
		^3 ].

	"4 bytes"
	(byte4 notNil and: [ (byte4 bitAnd: 16rC0) = 16r80 ]) ifFalse: [
		"invalid UTF-8. Assume byte1 is ISO8859-1, Latin-1, or answer nil."
		^ aBoolean
			ifTrue: [ self utf8BytesAndCodePointForIso8859s1: byte1 into: bytesBlock into: codePointBlock ]].
	(byte1 bitAnd: 16rF8) = 16rF0  ifTrue: [  "four bytes"
		bytesBlock ifNotNil: [
			bytesBlock value: byte1 value: byte2 value: byte3 value: byte4 ].
		codePointBlock ifNotNil: [
			codePoint := ((byte1 bitAnd: 16r7) bitShift: 18) + ((byte2 bitAnd: 63) bitShift: 12)
					+ ((byte3 bitAnd: 63) bitShift: 6) + (byte4 bitAnd: 63).
			codePointBlock value: codePoint ].
		^ 4].

	"invalid UTF-8. Assume byte1 is ISO8859-1, Latin-1, or answer nil."
	^ aBoolean
		ifTrue: [ self utf8BytesAndCodePointForIso8859s1: byte1 into: bytesBlock into: codePointBlock ]! !


!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 10/20/2025 16:23:39'!
nextUtf8BytesAndCodePointInto: bytesBlock into: codePointBlock
	"Find the next UTF-8 CodePoint from this file, assuming next byte is ISO 8859-1 if invalid UTF-8,
	or nil if at the end of the file."

	| bytesConsumed previousPosition |
	previousPosition := self position.
	bytesConsumed := 0.
	self nextByte ifNotNil: [ :byte1 |
		bytesConsumed := Character
			utf8BytesAndCodePointFor: byte1
			byte2: self nextByte
			byte3: self nextByte
			byte4: self nextByte
			into: bytesBlock
			into: codePointBlock
			assumeLatinIfInvalid: true ].
	self position: previousPosition + bytesConsumed.! !


!Character class methodsFor: 'converting' stamp: 'jmv 10/20/2025 16:23:47'!
utf8BytesAndCodePointAt: byteIndex in: aByteArray into: bytesBlock into: codePointBlock
	"Find next UTF-8 encoded code point in utf8ByteArray starting at byte byteIndex.
	Evaluate bytesBlock (if not nil). Arguments are utf8 bytes for the codePoint found.
	Evaluate codePointBlock (if not nil) with codePoint found as argument.
	Answer the number of bytes that comprised this codePoint.
	If there is an invalid UTF-8 sequence, consider the first byte an ISO-8859-1 Character (i.e. Latin-1), and answer 1.
	Skip codePoint U+FEFF (BOM, unneededly added by Win clipboard). Still, answer the number of bytes of the BOM sequence, to allow senders to update an index in an iteration.
	See senders."

	| i s byte1 byte2 byte3 byte4 |
	
	s := aByteArray size.
	i := byteIndex.
	byte1 := aByteArray at: i.
	i := byteIndex+1.
	byte2 := i <= s ifTrue: [aByteArray at: i].
	i := byteIndex+2.
	byte3 := i <= s ifTrue: [aByteArray at: i].
	i := byteIndex+3.
	byte4 := i <= s ifTrue: [aByteArray at: i].

	^self
		utf8BytesAndCodePointFor: byte1
		byte2: byte2
		byte3: byte3
		byte4: byte4
		into: bytesBlock
		into: codePointBlock
		assumeLatinIfInvalid: true! !

!methodRemoval: Character class #utf8BytesAndCodePointFor:byte2:byte3:byte4:into:into: stamp: 'jmv 10/20/2025 17:29:34'!
Character class removeSelector: #utf8BytesAndCodePointFor:byte2:byte3:byte4:into:into:!
