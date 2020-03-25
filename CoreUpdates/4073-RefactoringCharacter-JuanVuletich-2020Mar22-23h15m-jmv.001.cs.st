'From Cuis 5.0 [latest update: #4072] on 22 March 2020 at 11:37:21 pm'!

!Character class methodsFor: 'converting' stamp: 'jmv 3/22/2020 23:19:17'!
nextCodePointBytesFromUtf8: anUtf8Stream into: aBlock
	"anUtf8Stream can be over a ByteArray.
	See senders."

	| byte1 byte2 byte3 byte4 |
	byte1 _ anUtf8Stream next.
	byte1 < 128 ifTrue: [	"single byte"
		^ aBlock value: byte1 value: nil value: nil value: nil ].
	
	"At least 2 bytes"
	byte2 _ anUtf8Stream next.
	(byte2 bitAnd: 16rC0) = 16r80 ifFalse: [^nil]. "invalid UTF-8"
	(byte1 bitAnd: 16rE0) = 192 ifTrue: [ "two bytes"
		^ aBlock value: byte1 value: byte2 value: nil value: nil ].
	
	"At least 3 bytes"
	byte3 _ anUtf8Stream next.
	(byte3 bitAnd: 16rC0) = 16r80 ifFalse: [^nil]. "invalid UTF-8"
	(byte1 bitAnd: 16rF0) = 224 ifTrue: [ "three bytes"
		^ aBlock value: byte1 value: byte2 value: byte3 value: nil ].

	"4 bytes"
	byte4 _ anUtf8Stream next.
	(byte4 bitAnd: 16rC0) = 16r80 ifFalse: [^nil]. "invalid UTF-8"
	(byte1 bitAnd: 16rF8) = 240 ifTrue: [  "four bytes"
		^ aBlock value: byte1 value: byte2 value: byte3 value: byte4 ].

	^nil! !


!Character class methodsFor: 'converting' stamp: 'jmv 3/22/2020 23:33:20'!
nextUnicodeCodePointFromUtf8: anUtf8Stream
	"anUtf8Stream can be over a ByteArray
	Answer nil if conversion not possible, because of invalid UTF-8.
	Also answer nil for codePoint U+FEFF (BOM, unneededly added by Win clipboard)"

	^ self
		nextCodePointBytesFromUtf8: anUtf8Stream
		into: [ :byte1 :byte2 :byte3 :byte4 |
			byte4 notNil
				ifTrue: [ ((byte1 bitAnd: 16r7) bitShift: 18) + ((byte2 bitAnd: 63) bitShift: 12) + ((byte3 bitAnd: 63) bitShift: 6) + (byte4 bitAnd: 63) ]
				ifFalse: [
					byte3 notNil
						ifTrue: [ | codePoint |
							codePoint _  ((byte1 bitAnd: 15) bitShift: 12) + ((byte2 bitAnd: 63) bitShift: 6) + (byte3 bitAnd: 63).
							codePoint = 16rFEFF
								ifFalse: [ codePoint ]
								ifTrue: [ nil ]]
						ifFalse: [
							byte2 notNil
								ifTrue: [ ((byte1 bitAnd: 31) bitShift: 6) + (byte2 bitAnd: 63) ]
								ifFalse: [ byte1 ]]]]! !

!methodRemoval: Character class #nextCodePointBytesFromUtf8:ifOneByte:ifTwoBytes:ifThreeBytes:ifFourBytes: stamp: 'jmv 3/22/2020 23:35:02'!
Character class removeSelector: #nextCodePointBytesFromUtf8:ifOneByte:ifTwoBytes:ifThreeBytes:ifFourBytes:!
