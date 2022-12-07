'From Cuis 6.0 [latest update: #5572] on 6 December 2022 at 5:28:23 pm'!

!Object methodsFor: 'testing' stamp: 'jmv 12/6/2022 17:22:46'!
isString
	"Overridden to return true in String, Utf8String."
	^ false! !


!String methodsFor: 'comparing' stamp: 'jmv 12/6/2022 17:23:03'!
= aString
	"Answer whether the receiver sorts equally as aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, Utf8String, Utf8Symbol."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String or Symbol, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self size = aString size ifFalse: [
		^false ].

	self hash = aString hash ifFalse: [
		^false ].

	"Let them handle us"
	aString isByteString ifFalse: [
		^aString = self ].

	^String is: self equalTo: aString.! !

!String methodsFor: 'comparing' stamp: 'jmv 12/6/2022 17:23:17'!
hash
	"#hash is implemented, because #= is implemented.
	Note: #hash must be the same for String, Symbol, Utf8String, Utf8Symbol and Text
	if they hold same Characters."

	self size >  ByteArray minSizeForHashQuick ifTrue: [
		^ self hashQuick: String hash ].
	^ByteArray
		hashBytes: self
		startingWith: String hash! !

!String methodsFor: 'testing' stamp: 'jmv 12/6/2022 17:23:09'!
canHoldCodePoints
	"Nope. Needs something like Utf8String for that. Even an Array is better than us."
	^false! !


!String class methodsFor: 'instance creation' stamp: 'jmv 12/6/2022 17:23:23'!
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."
	| newArray index |
	newArray _ self new: aCollection size.
	index _ 0.
	aCollection do: [ :each | newArray at: (index _ index + 1) put: each].
	^ newArray

"
	{$c. $h. $r} as: String
	{$c. $h. $r} as: Utf8String
"! !


!Utf8String methodsFor: 'comparing' stamp: 'jmv 12/6/2022 17:23:36'!
= aString
	"Answer whether the receiver is equal to aString.
	This means same characters in same order.
	Argument could be instance of String, Symbol, Text, Utf8String, Utf8Symbol."

	"Any object is equal to itself"
	self == aString ifTrue: [ ^ true ].

	"If argument is not a kind of String, maybe it is a Text?"
	aString isString ifFalse: [
		(aString is: #Text) ifTrue: [ ^ self = aString string ].
		^ false].

	self hash = aString hash ifFalse: [
		^false ].

	(self isInCannonicalCompositionForm and: [ aString isInCannonicalCompositionForm ]) ifFalse: [
		^self cannonicalComposition = aString cannonicalComposition ].

	"Now we can assume we are both in NFC"
	self size = aString size ifFalse: [
		^false ].

	(self isAscii and: [	aString isByteString ]) ifTrue: [	
		"Fast lane."
		^String is: bytes equalTo: aString ].	

	aString isUtf8String ifFalse: [
		^ self beginsWith: aString ].

	self isAscii = aString isAscii ifFalse: [
		^false ].

	^String is: bytes equalTo: aString bytes.! !

!Utf8String methodsFor: 'comparing' stamp: 'jmv 12/6/2022 17:23:42'!
hash
	"#hash is implemented, because #= is implemented.
	Note: #hash must be the same for String, Symbol, Utf8String, Utf8Symbol and Text
	if they hold same Characters."

	"Lazy initialization."
	hash isNil ifTrue: [
		self findCannonicalCompositionAndHash ].

	"If 'hash' is not a number, it actually contains out #cannonicalComposition. Ask for its hash then."
	hash isNumber ifFalse: [
		^hash hash ].

	"If 'hash' is a number it means we are in cannonicalComposition form (NFC), and 'hash' is an good hash value."
	^hash! !


!Utf8String class methodsFor: 'instance creation' stamp: 'jmv 12/6/2022 17:23:47'!
newFrom: aSimilarObject
	^Utf8String streamContents: [ :strm |
		aSimilarObject do: [ :codePoint |
			strm nextPut: codePoint ]]

"
	{$c. $h. $r} as: String
	{$c. $h. $r} as: Utf8String
"! !

!Utf8String class methodsFor: 'stream creation' stamp: 'jmv 12/6/2022 17:23:59'!
streamContents: blockWithArg
	"Evaluate a block on a write stream that accepts bytes, characters.
	Arguments to #nextPutAll: can be sequences of bytes, Strings, Utf8Strings.
	Answer is an instance of us."

	| stream |	
	stream _ Utf8EncodedWriteStream on: (ByteArray new: 100).
	blockWithArg value: stream.
	^stream utf8Contents! !

!Utf8String class methodsFor: 'stream creation' stamp: 'jmv 12/6/2022 17:24:05'!
writeStream
	"Answer a write stream that accepts bytes, characters.
	Arguments to #nextPutAll: can be sequences of bytes, Strings, Utf8Strings."

	| stream |	
	stream _ Utf8EncodedWriteStream on: (ByteArray new: 100).
	^stream! !


!ByteArray class methodsFor: 'stream creation' stamp: 'jmv 12/6/2022 17:24:11'!
streamUtf8Contents: blockWithArg
	"Evaluate a block on a write stream that accepts bytes, characters.
	Arguments to #nextPutAll: can be sequences of bytes, Strings, Utf8Strings.
	Answer is an instance of us, holding UTF-8 encoded bytes."

	| stream |	
	stream _ Utf8EncodedWriteStream on: (ByteArray new: 100).
	blockWithArg value: stream.
	^stream byteContents! !


!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 12/6/2022 17:24:23'!
next
	"Answer the next entry from this file, or nil if at the end of the file.
	Answer will be
	- CodePoint (for #useUtf8String)
	- Character (for #useCharacters)
	- SmallInteger (for #useBytes)"

	^readAdaptor next! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 12/6/2022 17:24:29'!
next: n
	"Answer the next n charaters (or bytes) from this file, or nil if at the end of the file.
	Answer will be
	- an Utf8String or String or size n (or until the end of the file if shorter)
	- a ByteArray of size n (or until the end of the file if shorter)"

	^readAdaptor next: n! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 12/6/2022 17:24:50'!
next: anInteger putAll: aStringOrBytes startingAt: startIndex
	"Argument can be String, Utf8String or ByteArray.
	Write to this file. Both kinds of Strings are UTF-8 encoded.
	Optimized only for ByteArray, the common use."

	(aStringOrBytes is: #ByteArray) ifFalse: [
		(startIndex = 1 and: [anInteger = aStringOrBytes size])ifTrue: [
			^self nextPutAll: aStringOrBytes].
		^self nextPutAll: (aStringOrBytes copyFrom: startIndex to: startIndex+anInteger-1) ].

	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	self primWrite: fileID from: aStringOrBytes startingAt: startIndex count: anInteger.
	lastWrittenElement _ aStringOrBytes at: startIndex+anInteger-1.! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 12/6/2022 17:25:15'!
nextPutAll: aStringOrBytes
	"Argument can be String, Utf8String or ByteArray.
	Write to this file. Both kinds of Strings are UTF-8 encoded."

	| bytes |
	aStringOrBytes isEmpty ifTrue: [ ^self ].
	bytes _ aStringOrBytes.
	(aStringOrBytes is: #ByteArray) ifFalse: [
		bytes _ aStringOrBytes asUtf8Bytes].
	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	self primWrite: fileID from: bytes startingAt: 1 count: bytes basicSize.
	lastWrittenElement _ aStringOrBytes last.! !

!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 12/6/2022 17:25:51'!
useBytes
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String,  Utf8String,
	according to protocol."
	readAdaptor _ BytesReadAdaptor new stream: self.! !

!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 12/6/2022 17:26:10'!
useCharacters
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String,  Utf8String,
	according to protocol."
	readAdaptor _ ByteStringReadAdaptor new stream: self.! !

!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 12/6/2022 17:26:16'!
useUtf8String
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String,  Utf8String,
	according to protocol."
	readAdaptor _ Utf8ReadAdaptor new stream: self.! !


!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 12/6/2022 17:25:29'!
next: anInteger putAll: aStringOrBytes startingAt: startIndex
	"Argument can be String, Utf8String or ByteArray.
	Write to this file. Both kinds of Strings are UTF-8 encoded.
	Optimized only for ByteArray, the common use."

	(aStringOrBytes is: #ByteArray) ifFalse: [
		(startIndex = 1 and: [anInteger = aStringOrBytes size])ifTrue: [
			^self nextPutAll: aStringOrBytes].
		^self nextPutAll: (aStringOrBytes copyFrom: startIndex to: startIndex+anInteger-1) ].

	super next: anInteger putAll: aStringOrBytes startingAt: startIndex.
	lastWrittenElement _ aStringOrBytes at: startIndex+anInteger-1.
	numberOfCodePointsWritten _ nil.	"We can no longer know"! !

!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 12/6/2022 17:25:42'!
nextPutAll: aStringOrBytes
	"Argument can be String, Utf8String or ByteArray.
	Write to this stream. Both kinds of Strings are UTF-8 encoded."

	| bytes |
	aStringOrBytes isEmpty ifTrue: [ ^self ].
	bytes _ aStringOrBytes.
	(aStringOrBytes is: #ByteArray) ifFalse: [
		bytes _ aStringOrBytes asUtf8Bytes.
		numberOfCodePointsWritten ifNotNil: [
			"As aStringOrBytes is a kind of String, not utf-8 byteArray, we can use its size."
			numberOfCodePointsWritten _ numberOfCodePointsWritten + aStringOrBytes size ]].
	super nextPutAll: bytes.
	lastWrittenElement _ aStringOrBytes last.! !

!methodRemoval: UniFileStream #useUtf32String stamp: 'jmv 12/6/2022 17:21:38'!
UniFileStream removeSelector: #useUtf32String!
!methodRemoval: CharacterSequence #asUtf32String stamp: 'jmv 12/6/2022 17:21:06'!
CharacterSequence removeSelector: #asUtf32String!
!methodRemoval: Object #isUtf32String stamp: 'jmv 12/6/2022 17:22:53'!
Object removeSelector: #isUtf32String!
!classRemoval: #Utf32ReadAdaptor stamp: 'jmv 12/6/2022 17:22:00'!
Smalltalk removeClassNamed: #Utf32ReadAdaptor!

!classRemoval: #Utf32String stamp: 'jmv 12/6/2022 17:22:18'!
Smalltalk removeClassNamed: #Utf32String!

