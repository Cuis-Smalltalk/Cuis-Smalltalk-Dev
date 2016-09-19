'From Cuis 4.2 of 25 July 2013 [latest update: #2940] on 19 September 2016 at 10:11:05 am'!
!classDefinition: #Base64MimeConverter category: #'Collections-Streams'!
Object subclass: #Base64MimeConverter
	instanceVariableNames: 'data multiLine mimeStream dataStream '
	classVariableNames: 'FromCharTable ToCharTable '
	poolDictionaries: ''
	category: 'Collections-Streams'!

!Base64MimeConverter commentStamp: 'jmv 9/19/2016 09:39:53' prior: 0!
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

In Cuis, we only support bytes as the data stream (not Strings). See https://en.wikipedia.org/wiki/Base64!


!Character class methodsFor: 'instance creation' stamp: 'jmv 9/19/2016 09:24:38'!
asciiValue: asciiValue
	"
	Answer the Character for the required asciiValue. Argument is a byte, i.e. in [0..255].
	values above 127, consider it an ISO 8859-15 code.
	
	self assert: (Character asciiValue: 16r41) = $A.
	self assert: (Character asciiValue: 16rA4) = $¤.
	"
	^ Character numericValue: asciiValue! !


!Base64MimeConverter methodsFor: 'accessing' stamp: 'jmv 9/19/2016 09:16:17'!
dataStream
	^dataStream! !

!Base64MimeConverter methodsFor: 'accessing' stamp: 'jmv 9/19/2016 09:27:56'!
dataStream: aByteStream
	"aByteStream must hold bytes, i.e. integers in [0..255]"

	dataStream _ aByteStream! !

!Base64MimeConverter methodsFor: 'accessing' stamp: 'jmv 9/19/2016 09:16:36'!
mimeStream
	^mimeStream! !

!Base64MimeConverter methodsFor: 'accessing' stamp: 'jmv 9/19/2016 09:26:53'!
mimeStream: aCharacterStream
	"aCharacterStream is in base 64 with only a-z,A-Z,0-9,+,/ ."

	mimeStream _ aCharacterStream! !


!Base64MimeConverter class methodsFor: 'services' stamp: 'jmv 9/19/2016 09:30:24'!
mimeDecode: aStringOrStream
	^ ByteArray streamContents: [ :out |
		self mimeDecode: aStringOrStream to: out]! !

!Base64MimeConverter class methodsFor: 'services' stamp: 'jmv 9/19/2016 09:31:46'!
mimeDecode: aStringOrStream to: outStream
	self new
		mimeStream: ((aStringOrStream is: #Stream)
			ifTrue: [aStringOrStream]
			ifFalse: [ReadStream on: aStringOrStream]);
		dataStream: outStream;
		mimeDecodeToByteArray! !

!Base64MimeConverter class methodsFor: 'services' stamp: 'jmv 9/19/2016 09:15:26'!
mimeEncode: aCollectionOrStream to: outStream
	self new
		dataStream: ((aCollectionOrStream is: #Stream)
			ifTrue: [aCollectionOrStream]
			ifFalse: [ReadStream on: aCollectionOrStream]);
		mimeStream: outStream;
		mimeEncode! !


!String methodsFor: 'converting' stamp: 'jmv 9/19/2016 09:33:15'!
base64Decoded
	"Decode the receiver from base 64"
	"
	'SGVsbG8gV29ybGQ=' base64Decoded
	'SGVsbG8gV29ybGQ=' base64Decoded asString
	"
	^(Base64MimeConverter mimeDecode: self)! !


!ContentPack class methodsFor: 'code pack' stamp: 'jmv 9/19/2016 09:47:46'!
encodeContentFrom: aDictionary

	| encodedTree |
	
	encodedTree _ Dictionary new.
	
	aDictionary associations
		do: [ :assoc |
			assoc value class == Dictionary
				ifTrue: [ 
					encodedTree 
						at: assoc key 
						put: (self encodeContentFrom: assoc value) 
				]
				ifFalse: [ 
					encodedTree 
						at: assoc key
						put: count.
					count _ count + 1.
					payload add: assoc value storeString asByteArray base64Encoded
				]
		].

	^ encodedTree associations collect: [ :assoc | 
		{ assoc key . assoc value }
	]! !


!Base64MimeConverter methodsFor: 'conversion' stamp: 'jmv 9/19/2016 10:07:49'!
mimeEncode
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
		mimeStream skip: -1; nextPut: $=.
		^ mimeStream]! !


!Base64MimeConverter class methodsFor: 'additional services' stamp: 'jmv 9/19/2016 09:25:21'!
initializeTables

	FromCharTable _ Array new: 256.	"nils"
	ToCharTable _ Array new: 64.
	($A asciiValue to: $Z asciiValue) withIndexDo: [ :ascii :ind | 
		FromCharTable at: ascii+1 put: ind-1.
		ToCharTable at: ind put: (Character asciiValue: ascii) ].
	($a asciiValue to: $z asciiValue) withIndexDo: [ :ascii :ind | 
		FromCharTable at: ascii+1 put: ind+25.
		ToCharTable at: ind+26 put: (Character asciiValue: ascii) ].
	($0 asciiValue to: $9 asciiValue) withIndexDo: [ :ascii :ind | 
		FromCharTable at: ascii+1 put: ind+25+26.
		ToCharTable at: ind+26+26 put: (Character asciiValue: ascii) ].
	FromCharTable at: $+ asciiValue + 1 put: 62.
	ToCharTable at: 63 put: $+.
	FromCharTable at: $/ asciiValue + 1 put: 63.
	ToCharTable at: 64 put: $/.
	! !

!methodRemoval: Base64MimeConverter class #mimeDecodeToChars:!
Base64MimeConverter class removeSelector: #mimeDecodeToChars:!
!methodRemoval: Base64MimeConverter #mimeDecode!
Base64MimeConverter removeSelector: #mimeDecode!
!methodRemoval: Base64MimeConverter #nextByte!
Base64MimeConverter removeSelector: #nextByte!
!methodRemoval: String #base64Encoded!
String removeSelector: #base64Encoded!
!classDefinition: #Base64MimeConverter category: #'Collections-Streams'!
Object subclass: #Base64MimeConverter
	instanceVariableNames: 'dataStream mimeStream data multiLine'
	classVariableNames: 'FromCharTable ToCharTable'
	poolDictionaries: ''
	category: 'Collections-Streams'!

!Base64MimeConverter class reorganize!
('cached state access' releaseClassCachedState)
('services' mimeDecode: mimeDecode:to: mimeEncode: mimeEncode:to:)
('additional services' decodeInteger: encodeInteger: initializeTables mimeDecodeToBytes: mimeEncode:multiLine: mimeEncode:multiLine:atStart: mimeEncodeContinue:)
!

!classRemoval: #MimeConverter!
Smalltalk removeClassNamed: #MimeConverter!
