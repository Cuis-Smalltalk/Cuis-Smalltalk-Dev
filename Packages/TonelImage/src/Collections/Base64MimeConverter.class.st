"
This class encodes and decodes data in Base64 format.  This is MIME encoding.  We translate a whole stream at once, taking a Stream as input and giving one as output.  Returns a whole stream for the caller to use.

           0 A        		17 R            	34 i            	51 z
           1 B          		18 S            	35 j            	52 0
           2 C            	19 T            	36 k            	53 1
           3 D            	20 U            	37 l            	54 2
           4 E            	21 V            	38 m            	55 3
           5 F            	22 W            	39 n            	56 4
           6 G            	23 X            	40 o            	57 5
           7 H            	24 Y            	41 p            	58 6
           8 I            	25 Z            	42 q            	59 7
           9 J            	26 a            	43 r            	60 8
          10 K            	27 b            	44 s            	61 9
          11 L            	28 c            	45 t            	62 +
          12 M            	29 d            	46 u            	63 /
          13 N            	30 e            	47 v
          14 O            	31 f            	48 w         	(pad) =
          15 P            	32 g            	49 x
          16 Q            	33 h            	50 y

Outbound: bytes are broken into 6 bit chunks, and the 0-63 value is converted to a character.  3 data bytes go into 4 characters.
Inbound: Characters are translated in to 0-63 values and shifted into 8 bit bytes.

(See: N. Borenstein, Bellcore, N. Freed, Innosoft, Network Working Group, Request for Comments: RFC 1521, September 1993, MIME (Multipurpose Internet Mail Extensions) Part One: Mechanisms for Specifying and Describing the Format of Internet Message Bodies. Sec 6.2)

By Ted Kaehler, based on Tim Olson's Base64Filter.

In Cuis, we only support bytes as the data stream (not Strings). See https://en.wikipedia.org/wiki/Base64
"
Class {
	#name : 'Base64MimeConverter',
	#superclass : 'Object',
	#instVars : [
		'dataStream',
		'mimeStream',
		'data',
		'multiLine'
	],
	#classVars : [
		'FromCharTable',
		'ToCharTable'
	],
	#category : 'Collections-Streams',
	#package : 'Collections',
	#tag : 'Streams'
}

{ #category : 'additional services' }
Base64MimeConverter class >> decodeInteger: mimeString [
	| bytes sum |
	"Decode the MIME string into an integer of any length"

	bytes := (Base64MimeConverter mimeDecodeToBytes: 
				mimeString readStream) contents.
	sum := 0.
	bytes reverseDo: [:by | sum := sum * 256 + by].
	^ sum
]

{ #category : 'additional services' }
Base64MimeConverter class >> encodeInteger: int [
	| strm |
	"Encode an integer of any length and return the MIME string"

	strm := ReadWriteStream on: (ByteArray new: int digitLength).
	1 to: int digitLength do: [:ii | strm nextPut: (int digitAt: ii)].
	strm reset.
	^ ((self mimeEncode: strm) contents) copyUpTo: $=	"remove padding"
]

{ #category : 'additional services' }
Base64MimeConverter class >> initializeTables [

	FromCharTable := Array new: 256.	"nils"
	ToCharTable := Array new: 64.
	($A asciiValue to: $Z asciiValue) withIndexDo: [ :ascii :ind | 
		FromCharTable at: ascii+1 put: ind-1.
		ToCharTable at: ind put: (Character codePoint: ascii) ].
	($a asciiValue to: $z asciiValue) withIndexDo: [ :ascii :ind | 
		FromCharTable at: ascii+1 put: ind+25.
		ToCharTable at: ind+26 put: (Character codePoint: ascii) ].
	($0 asciiValue to: $9 asciiValue) withIndexDo: [ :ascii :ind | 
		FromCharTable at: ascii+1 put: ind+25+26.
		ToCharTable at: ind+26+26 put: (Character codePoint: ascii) ].
	FromCharTable at: $+ asciiValue + 1 put: 62.
	ToCharTable at: 63 put: $+.
	FromCharTable at: $/ asciiValue + 1 put: 63.
	ToCharTable at: 64 put: $/.
	
]

{ #category : 'services' }
Base64MimeConverter class >> mimeDecode: aString [
	^ ByteArray streamContents: [ :out |
		self mimeDecode: aString to: out]
]

{ #category : 'services' }
Base64MimeConverter class >> mimeDecode: aString to: outStream [
	self new
		mimeStream: aString readStream;
		dataStream: outStream;
		mimeDecodeToByteArray
]

{ #category : 'additional services' }
Base64MimeConverter class >> mimeDecodeToBytes: aStream  [
	"Return a ReadWriteStream of the original ByteArray.  aStream has only 65 innocuous character values.  aStream is not binary.  (See class comment). 4 bytes in aStream goes to 3 bytes in output."

	| me |
	aStream position: 0.
	me := self new mimeStream: aStream.
	me dataStream: (ReadWriteStream on: (ByteArray new: aStream size * 3 // 4)).
	me mimeDecodeToByteArray.
	me dataStream position: 0.
	^ me dataStream
]

{ #category : 'services' }
Base64MimeConverter class >> mimeEncode: aStream [
	"Return a ReadWriteStream of characters.  The data of aStream is encoded as 65 innocuous characters.  (See class comment). 3 bytes in aStream goes to 4 bytes in output."
	^self mimeEncode: aStream multiLine: true atStart: true
]

{ #category : 'additional services' }
Base64MimeConverter class >> mimeEncode: aStream multiLine: aBool [
	"Return a ReadWriteStream of characters.  The data of aStream is encoded as 65 innocuous characters.  (See class comment). 3 bytes in aStream goes to 4 bytes in output."

	^self mimeEncode: aStream multiLine: aBool atStart: true
]

{ #category : 'additional services' }
Base64MimeConverter class >> mimeEncode: aStream multiLine: aBool atStart: resetInput [
	"Return a ReadWriteStream of characters.  The data of aStream is encoded as 65 innocuous characters.  (See class comment). 3 bytes in aStream goes to 4 bytes in output."

	| me |
	resetInput ifTrue:[aStream position: 0].
	me := self new dataStream: aStream.
	me multiLine: aBool.
	me mimeStream: (ReadWriteStream on: (String new: aStream size + 20 * 4 // 3)).
	me mimeEncode.
	me mimeStream position: 0.
	^ me mimeStream
]

{ #category : 'services' }
Base64MimeConverter class >> mimeEncode: aByteArray to: outStream [
	self new
		dataStream: aByteArray readStream;
		mimeStream: outStream;
		multiLine: true;
		mimeEncode
]

{ #category : 'additional services' }
Base64MimeConverter class >> mimeEncodeContinue: aStream [
	"Return a ReadWriteStream of characters.  The data of aStream is encoded as 65 innocuous characters.  (See class comment). 3 bytes in aStream goes to 4 bytes in output."
	^self mimeEncode: aStream multiLine: true atStart: false
]

{ #category : 'cached state access' }
Base64MimeConverter class >> releaseClassCachedState [
	FromCharTable := ToCharTable := nil
]

{ #category : 'accessing' }
Base64MimeConverter >> dataStream [
	^dataStream
]

{ #category : 'accessing' }
Base64MimeConverter >> dataStream: aByteStream [
	"aByteStream must hold bytes, i.e. integers in [0..255]"

	dataStream := aByteStream
]

{ #category : 'conversion' }
Base64MimeConverter >> mimeDecodeToByteArray [
	"Convert a stream in base 64 with only a-z,A-Z,0-9,+,/ to a full ByteArray of 0-255 values.  Reutrn a whole stream for the user to read."

	| nibA nibB nibC nibD |
	[mimeStream atEnd] whileFalse: [
		(nibA := self nextValue) ifNil: [^ dataStream].
		(nibB := self nextValue) ifNil: [^ dataStream].
		dataStream nextPut: ((nibA bitShift: 2) + (nibB bitShift: -4)).
		nibB := nibB bitAnd: 16rF.
		(nibC := self nextValue) ifNil: [^ dataStream].
		dataStream nextPut: ((nibB bitShift: 4) + (nibC bitShift: -2)).
		nibC := nibC bitAnd: 16r3.
		(nibD := self nextValue) ifNil: [^ dataStream].
		dataStream nextPut: ((nibC bitShift: 6) + nibD).
		].
	^ dataStream
]

{ #category : 'conversion' }
Base64MimeConverter >> mimeEncode [
	"Do conversion reading from dataStream writing to mimeStream"
	"Convert from data to 6 bit characters."

	| phase1 phase2 byte nib lineLength |
	ToCharTable ifNil: [ self class initializeTables ].
	phase1 := phase2 := false.
	lineLength := 0.
	[dataStream atEnd] whileFalse: [
		(multiLine and:[lineLength >= 70]) ifTrue: [ mimeStream newLine.  lineLength := 0. ].
		data := byte := dataStream next.
		nib := (data bitAnd: 16rFC) bitShift: -2.
		mimeStream nextPut: (ToCharTable at: nib+1).
		(byte := dataStream next) ifNil: [byte := 0. phase1 := true].
		data := ((data bitAnd: 3) bitShift: 8) + byte.
		nib := (data bitAnd: 16r3F0) bitShift: -4.
		mimeStream nextPut: (ToCharTable at: nib+1).
		(byte := dataStream next) ifNil: [byte := 0. phase2 := true].
		data := ((data bitAnd: 16rF) bitShift: 8) + (byte).
		nib := (data bitAnd: 16rFC0) bitShift: -6.
		mimeStream nextPut: (ToCharTable at: nib+1).
		nib := (data bitAnd: 16r3F).
		mimeStream nextPut: (ToCharTable at: nib+1).

		lineLength := lineLength + 4.].
	phase1 ifTrue: [
		mimeStream skip: -2; nextPut: $=; nextPut: $=.
		^ mimeStream].
	phase2 ifTrue: [
		mimeStream skipBack; nextPut: $=.
		^ mimeStream]
]

{ #category : 'accessing' }
Base64MimeConverter >> mimeStream [
	^mimeStream
]

{ #category : 'accessing' }
Base64MimeConverter >> mimeStream: aCharacterStream [
	"aCharacterStream is in base 64 with only a-z,A-Z,0-9,+,/ ."

	mimeStream := aCharacterStream
]

{ #category : 'accessing' }
Base64MimeConverter >> multiLine [
	"Determines whether we allow multi-line encodings (the default) or force everything into a single line (for use with URLs etc. where the continuation marker and the line break cause problems)"
	^multiLine
]

{ #category : 'accessing' }
Base64MimeConverter >> multiLine: aBool [
	"Determines whether we allow multi-line encodings (the default) or force everything into a single line (for use with URLs etc. where the continuation marker and the line break cause problems)"
	multiLine := aBool
]

{ #category : 'conversion' }
Base64MimeConverter >> nextValue [
	"The next six bits of data char from the mimeStream, or nil.  Skip all other chars"
	| raw num |
	FromCharTable ifNil: [ self class initializeTables ].
	[
		raw := mimeStream next.
		raw ifNil: [^ nil].	"end of stream"
		raw == $= ifTrue: [^ nil].
		num := FromCharTable at: raw asciiValue + 1.
		num ifNotNil: [^ num].
		"else ignore space, return, tab, ..."
	] repeat
]
