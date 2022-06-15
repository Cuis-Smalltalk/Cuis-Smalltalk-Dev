'From Cuis 6.0 [latest update: #5212] on 31 May 2022 at 4:35:01 pm'!
!classDefinition: #EncodedStreamReadAdaptor category: #'Collections-Streams'!
Object subclass: #EncodedStreamReadAdaptor
	instanceVariableNames: 'stream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!
!classDefinition: #BytesEncodedStreamReadAdaptor category: #'Collections-Streams'!
EncodedStreamReadAdaptor subclass: #BytesEncodedStreamReadAdaptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!
!classDefinition: #StringEncodedStreamReadAdaptor category: #'Collections-Streams'!
EncodedStreamReadAdaptor subclass: #StringEncodedStreamReadAdaptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!
!classDefinition: #Utf32EncodedStreamReadAdaptor category: #'Collections-Streams'!
EncodedStreamReadAdaptor subclass: #Utf32EncodedStreamReadAdaptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!
!classDefinition: #Utf8EncodedStreamReadAdaptor category: #'Collections-Streams'!
EncodedStreamReadAdaptor subclass: #Utf8EncodedStreamReadAdaptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!
!classDefinition: #EncodedReadStream category: #'Collections-Streams'!
ReadStream subclass: #EncodedReadStream
	instanceVariableNames: 'readAdaptor'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!
!classDefinition: #EncodedWriteStream category: #'Collections-Streams'!
WriteStream subclass: #EncodedWriteStream
	instanceVariableNames: 'lastWrittenElement'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!
!classDefinition: #EncodedFileStream category: #'System-Files'!
ReadWriteStream subclass: #EncodedFileStream
	instanceVariableNames: 'rwmode name fileID buffer1 readAdaptor lastWrittenElement'
	classVariableNames: 'Registry'
	poolDictionaries: ''
	category: 'System-Files'!

!EncodedFileStream commentStamp: '<historical>' prior: 0!
A new FileStream for Cuis.

Writes bytes. Binary content is stored verbatim. String is stored in UTF-8, but invalid UTF-8 sequences are interpreted as Cuis Characters (i.e. ISO-8859-15).!

!classDefinition: #EncodedByteArrayStreamTest category: #'Tools-Testing'!
TestCase subclass: #EncodedByteArrayStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!
!classDefinition: #EncodedFileStreamTest category: #'Tools-Testing'!
TestCase subclass: #EncodedFileStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!
!classDefinition: #FileStreamTest category: #'Tools-Testing'!
TestCase subclass: #FileStreamTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Testing'!

!EncodedStreamReadAdaptor methodsFor: 'accessing' stamp: 'jmv 5/25/2022 17:09:05'!
byteStream: aStream
	stream _ aStream! !

!EncodedStreamReadAdaptor methodsFor: 'accessing' stamp: 'jmv 5/25/2022 17:05:14'!
collectionSpecies
	^self subclassResponsibility! !

!EncodedStreamReadAdaptor methodsFor: 'accessing' stamp: 'jmv 5/25/2022 17:09:12'!
stream: aStream
	stream _ aStream! !

!EncodedStreamReadAdaptor methodsFor: 'reading' stamp: 'jmv 5/25/2022 17:05:14'!
next
	self subclassResponsibility! !

!EncodedStreamReadAdaptor methodsFor: 'reading' stamp: 'jmv 5/31/2022 09:48:36'!
streamUpToEnd
	"Answer the contents of our stream, from current postion to end, seen from our perspective.
	Leave stream at the end position."

	^self subclassResponsibility! !


!BytesEncodedStreamReadAdaptor methodsFor: 'accessing' stamp: 'jmv 5/25/2022 17:05:30'!
collectionSpecies
	^ByteArray! !

!BytesEncodedStreamReadAdaptor methodsFor: 'reading' stamp: 'jmv 5/25/2022 17:08:42'!
next
	^stream nextByte! !

!BytesEncodedStreamReadAdaptor methodsFor: 'reading' stamp: 'jmv 5/31/2022 09:48:02'!
streamUpToEnd
	"Answer the contents of our stream, from current postion to end, seen from our perspective:
	Whatever bytes in the stream.
	Leave stream at the end position."

	^stream bytesUpToEnd! !


!StringEncodedStreamReadAdaptor methodsFor: 'accessing' stamp: 'jmv 5/25/2022 17:05:41'!
collectionSpecies
	^String! !

!StringEncodedStreamReadAdaptor methodsFor: 'reading' stamp: 'jmv 5/31/2022 09:58:03'!
next
	"Stream bytes as Cuis / ISO 8859-15 Characters.
	Disregard the fact that stream contents are most likely UTF-8.
	This is valid only if contents are ASCII, or Cuis / ISO-8859-15 byte Characters."

	stream nextByte ifNotNil: [ :byte |
		^Character numericValue: byte ].
	^nil! !

!StringEncodedStreamReadAdaptor methodsFor: 'reading' stamp: 'jmv 5/31/2022 09:58:10'!
streamUpToEnd
	"Answer the contents of our stream, from current postion to end, seen from our perspective:
	Stream bytes as Cuis / ISO 8859-15 Characters.
	Disregard the fact that stream contents are most likely UTF-8.
	This is valid only if contents are ASCII, or Cuis / ISO-8859-15 byte Characters.
	Leave stream at the end position."

	^stream bytesUpToEnd asString! !


!Utf32EncodedStreamReadAdaptor methodsFor: 'accessing' stamp: 'jmv 5/25/2022 17:05:51'!
collectionSpecies
	^Utf32String! !

!Utf32EncodedStreamReadAdaptor methodsFor: 'reading' stamp: 'jmv 5/25/2022 17:08:53'!
next
	^stream nextCodePoint! !

!Utf32EncodedStreamReadAdaptor methodsFor: 'reading' stamp: 'jmv 5/31/2022 09:48:20'!
streamUpToEnd
	"Answer the contents of our stream, from current postion to end, seen from our perspective:
	Stream bytes as CodePoints (prefer UTF-8, but answer ISO-8859-15 if invalid UTF-8).
	Leave stream at the end position."

	| utf32 |
	utf32 _ WordArray streamContents: [ :utf32Stream |
		[ stream atEnd ] whileFalse: [
			stream nextUtf8BytesAndCodePointInto: nil into: [ :codePoint |
				utf32Stream nextPut: codePoint]]].
	^Utf32String withWords: utf32! !


!Utf8EncodedStreamReadAdaptor methodsFor: 'accessing' stamp: 'jmv 5/25/2022 17:05:59'!
collectionSpecies
	^Utf8String! !

!Utf8EncodedStreamReadAdaptor methodsFor: 'reading' stamp: 'jmv 5/25/2022 17:08:58'!
next
	^stream nextCodePoint! !

!Utf8EncodedStreamReadAdaptor methodsFor: 'reading' stamp: 'jmv 5/31/2022 09:48:24'!
streamUpToEnd
	"Answer the contents of our stream, from current postion to end, seen from our perspective:
	Stream bytes as CodePoints (prefer UTF-8, but answer ISO-8859-15 if invalid UTF-8).
	Leave stream at the end position."

	| utf8 |
	utf8 _ ByteArray streamContents: [ :utf8Stream |
		[ stream atEnd ] whileFalse: [
			stream nextUtf8BytesAndCodePointInto: [ :byte1 :byte2 :byte3 :byte4 | 
				utf8Stream nextPut: byte1.
				byte2 ifNotNil: [
					utf8Stream nextPut: byte2.
					byte3 ifNotNil: [
						utf8Stream nextPut: byte3.
						byte4 ifNotNil: [
							utf8Stream nextPut: byte4]]]]
				into: nil ]].
	^Utf8String withBytes: utf8! !


!EncodedReadStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:11:15'!
next
	"Answer the next entry from this file, or nil if at the end of the file.
	Answer will be
	- CodePoint (for #useUtf8String and #useUtf32String)
	- Character (for #useCharacters)
	- SmallInteger (for #useBytes)"

	^readAdaptor next! !

!EncodedReadStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:11:41'!
next: n
	"Return a string with the next n characters of the filestream in it.  1/31/96 sw"
'--------' print.
thisContext printStack: 10.
self halt.
	^ self nextInto: (ByteArray new: n)! !

!EncodedReadStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:12:45'!
next: n into: aCollection startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| obj |
'--------' print.
thisContext printStack: 10.
self halt. "ojo"
	0 to: n-1 do: [ :i |
		(obj _ self nextByte) ifNil: [ ^aCollection copyFrom: 1 to: startIndex+i-1 ].
		aCollection at: startIndex+i put: obj].
	^aCollection! !

!EncodedReadStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:46:47'!
nextUtf8BytesAndCodePointInto: bytesBlock into: codePointBlock
	"Find the next UTF-8 CodePoint from this file, assuming next byte is ISO 8859-15 if invalid UTF-8,
	or nil if at the end of the file."

	| bytesConsumed previousPosition |
	previousPosition _ self position.
	bytesConsumed _ 0.
	self nextByte ifNotNil: [ :byte1 |
		bytesConsumed _ Character
			utf8BytesAndCodePointFor: byte1
			byte2: self nextByte
			byte3: self nextByte
			byte4: self nextByte
			into: bytesBlock
			into: codePointBlock ].
	self position: previousPosition + bytesConsumed.! !

!EncodedReadStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:18:16'!
nextWordsInto: aBitmap
	"Note: The file primitives automatically adjust for word based objects."

'--------' print.
thisContext printStack: 10.
self halt.
	self next: aBitmap basicSize into: aBitmap startingAt: 1.
	aBitmap restoreEndianness.
	^ aBitmap! !

!EncodedReadStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:46:52'!
peek
	"Answer what would be returned if the message next were sent to the receiver.
	If the receiver is at the end, answer nil.  "

	| next currentPosition |
	self atEnd ifTrue: [^ nil].
	currentPosition := self position.
	next _ self next.
	self position: currentPosition.
	^ next! !

!EncodedReadStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:32:26'!
peekFor: aCharacter 
	"Answer false and do not move over the next element if it is not equal to the argument, aCharacter 
	Answer true and increment the position for accessing elements, if the next element is equal to anObject."

	| nextChar |
	nextChar _ self peek.
	aCharacter = nextChar ifTrue: [
		self next.
		^ true].
	^ false! !

!EncodedReadStream methodsFor: 'read, write, position' stamp: 'jmv 5/30/2022 14:57:41'!
position: pos

	| m s |
	"Chequear que los senders sepan que estan mandando una posicion a byte!!"
	m _ thisContext sender method.
	thisContext sender receiver == self ifFalse: [
		thisContext sender receiver == readAdaptor ifFalse: [
			(EncodedFileStream positionValidatedSenders includes: m) ifFalse: [
				"SendersOfPositionColon _ Set new"
				s _ Smalltalk at: #SendersOfPositionColon ifAbsentPut: [ Set new ].
				(s includes: m) ifFalse: [
					s add: m.
					{ '-------- Sender de #position: '. m. pos} print.
					thisContext printStack: 10 ]]]].
	^super position: pos! !

!EncodedReadStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:20:26'!
readInto: byteArray startingAt: startIndex count: count
	"Read into the given array as specified, and return the count
	actually transferred.  index and count are in units of bytes or
	longs depending on whether the array is Bitmap, String or ByteArray"

'--------' print.
thisContext printStack: 10.
self halt.
	^(self next: count into: byteArray startingAt: startIndex) size - startIndex + 1
! !

!EncodedReadStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:22:34'!
skip
	"Skips one object in the receiver's future sequence values."

	self next! !

!EncodedReadStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:22:38'!
skip: n
	n < 0 ifTrue: [
		^n negated timesRepeat: [ self skipBack ]].
	n timesRepeat: [ self skip ].! !

!EncodedReadStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:47:16'!
skipBack
	| savePos try |
	savePos _ self position.

	"Try going back one byte. If ASCII, then OK, we are done.
	If not, don't assume ISO-8859-15 so lightly!!"
	try _ savePos-1.
	self position: try.
	self nextByte < 128 ifTrue: [
		^self position: try ].

	"Try going back two bytes. If doing #next leaves us at savePosition, we found a two byte CodePoint."
	try _ try-1.
	self position: try.
	(self next notNil and: [ self position = savePos ]) ifTrue: [
		^self position: try ].

	"Try going back three bytes. If doing #next leaves us at savePosition, we found a three byte CodePoint."
	try _ try-1.
	self position: try.
	(self next notNil and: [ self position = savePos ]) ifTrue: [
		^self position: try ].

	"Try going back four bytes. If doing #next leaves us at savePosition, we found a four byte CodePoint."
	try _ try-1.
	self position: try.
	(self next notNil and: [ self position = savePos ]) ifTrue: [
		^self position: try ].

	"Havent found a valid UTF-8 sequence that ends just before savePos.
	It means back byte is an ISO-8859-15 Character"
	self position: savePos-1.! !

!EncodedReadStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:24:32'!
upTo: delimiter delimiterIsTerminator: delimiterIsTerminator
'--------' print.
thisContext printStack: 10.
self halt! !

!EncodedReadStream methodsFor: 'private' stamp: 'jmv 5/31/2022 09:48:09'!
bytesUpToEnd
	"Leave stream at the end position."

	| start |
	start _ position+1.
	position _ collection size.
	^collection copyFrom: start to: position! !

!EncodedReadStream methodsFor: 'private' stamp: 'jmv 5/28/2022 17:30:06'!
nextByte
	"Return the next object in the Stream represented by the receiver."

	"treat me as a FIFO"
	^ position >= readLimit
		ifFalse: [collection at: (position _ position + 1)]! !

!EncodedReadStream methodsFor: 'private' stamp: 'jmv 5/28/2022 17:30:21'!
nextCodePoint
	| answer |
	answer _ nil.
	self nextUtf8BytesAndCodePointInto: nil into: [ :codePoint |
		answer _ UnicodeCodePoint codePoint: codePoint ].
	^answer! !

!EncodedReadStream methodsFor: 'private' stamp: 'jmv 5/28/2022 17:31:17'!
on: aCollection
	super on: aCollection.
	self useCharacters .
self useUtf8String.
"ok?"! !

!EncodedReadStream methodsFor: 'reading modes' stamp: 'jmv 5/28/2022 17:33:12'!
useBytes
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String,  Utf8String, Utf32String,
	according to protocol."
	readAdaptor _ BytesEncodedStreamReadAdaptor new stream: self.! !

!EncodedReadStream methodsFor: 'reading modes' stamp: 'jmv 5/28/2022 17:33:23'!
useCharacters
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String,  Utf8String, Utf32String,
	according to protocol."
	readAdaptor _ StringEncodedStreamReadAdaptor new stream: self.! !

!EncodedReadStream methodsFor: 'reading modes' stamp: 'jmv 5/28/2022 17:33:38'!
useUtf32String
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String,  Utf8String, Utf32String,
	according to protocol."
	readAdaptor _ Utf32EncodedStreamReadAdaptor new stream: self.! !

!EncodedReadStream methodsFor: 'reading modes' stamp: 'jmv 5/28/2022 17:33:45'!
useUtf8String
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String,  Utf8String, Utf32String,
	according to protocol."
	readAdaptor _ Utf8EncodedStreamReadAdaptor new stream: self.! !

!EncodedReadStream methodsFor: 'accessing' stamp: 'jmv 5/28/2022 17:34:08'!
collectionSpecies
	^readAdaptor collectionSpecies! !

!EncodedReadStream methodsFor: 'accessing' stamp: 'jmv 5/31/2022 09:52:53'!
contents
	"Return the full contents of the receiver. Do not close or otherwise touch the receiver.
	Return data in whatever mode the receiver is in (i.e. answer an instance of the appropriate class).
	Stream bytes as CodePoints (prefer UTF-8, but ISO-8859-15 if invalid UTF-8)."

	| savePos answer |
	savePos _ self position.
	self position: 0.
	answer _ self upToEnd.
	self position: savePos.
	^answer! !

!EncodedReadStream methodsFor: 'accessing' stamp: 'jmv 5/30/2022 17:16:50'!
upToEnd
	"Return the contents of the receiver. Do not close or otherwise touch the receiver.
	Return data in whatever mode the receiver is in (i.e. answer an instance of the appropriate class)."

	^readAdaptor streamUpToEnd! !


!EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:13:28'!
next: anInteger putAll: aString startingAt: startIndex
'--------' print.
thisContext printStack: 10.
self halt.
	lastWrittenElement _ aString at: anInteger.! !

!EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:15:29'!
nextPut: aCodePointOrCharacterOrByte
	"Insert the argument at the next position in the Stream represented by the receiver.
	Argument can be Character, CodePoint or Byte (SmallInteger).
	Write to this stream. Character and CodePoint are UTF-8 encoded."

	aCodePointOrCharacterOrByte isNumber
		ifTrue: [self nextPutByte: aCodePointOrCharacterOrByte]
		ifFalse: [self nextPutCodePoint: aCodePointOrCharacterOrByte codePoint].
	lastWrittenElement _ aCodePointOrCharacterOrByte.! !

!EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:16:04'!
nextPutAll: aStringOrBytes
	"Argument can be String, Utf8String, Utf32String or ByteArray.
	Write to this stream. All 3 kinds of Strings are UTF-8 encoded."

	| bytes |
	bytes _ aStringOrBytes.
	(aStringOrBytes is: #ByteArray) ifFalse: [
		bytes _ aStringOrBytes asUtf8Bytes].
	super nextPutAll: bytes.
	lastWrittenElement _ aStringOrBytes last.! !

!EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:17:07'!
nextPutCodePoint: aNumber
	"UTF-8 encoded"

	Character
		evaluate: [ :byte | self nextPutByte: byte ]
		withUtf8BytesOfUnicodeCodePoint: aNumber! !

!EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:19:19'!
pastEndPutByte: aByte
	"Grow the collection.
	Then we put <anObject> at the current write position."

	self growTo: collection size + 1.
	collection at: (position _ position + 1) put: aByte! !

!EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:20:09'!
peekLast

	^lastWrittenElement! !

!EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 5/30/2022 14:57:55'!
position: pos

	| m s |
	"Chequear que los senders sepan que estan mandando una posicion a byte!!"
	m _ thisContext sender method.
	thisContext sender receiver == self ifFalse: [
		(EncodedFileStream positionValidatedSenders includes: m) ifFalse: [
			"SendersOfPositionColon _ Set new"
			s _ Smalltalk at: #SendersOfPositionColon ifAbsentPut: [ Set new ].
			(s includes: m) ifFalse: [
				s add: m.
				{ '-------- Sender de #position: '. m. pos} print.
				thisContext printStack: 10 ]]].
	^super position: pos! !

!EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:22:26'!
skip
	"Skips one object in the receiver's future sequence values."
"
	self next
"
"Hago algo???"
self error: 'hacerlo?' print.
thisContext printStack: 10.! !

!EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:22:43'!
skip: n
	"Skips one object in the receiver's future sequence values."
"
	self next
"
"Hago algo???"
self error: 'hacerlo?' print.
thisContext printStack: 10.! !

!EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:23:12'!
skipBack
	"Skips one object in the receiver's future sequence values."
"
	self next
"
"Hago algo???"
self error: 'hacerlo?' print.
thisContext printStack: 10.! !

!EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 5/30/2022 10:24:26'!
withAttribute: aTextAttribute do: streamBlock
'--------' print.
thisContext printStack: 10.
self halt.! !

!EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 5/30/2022 10:24:42'!
withAttributes: attributes do: streamBlock 
'--------' print.
thisContext printStack: 10.
self halt.! !

!EncodedWriteStream methodsFor: 'private' stamp: 'jmv 5/31/2022 12:52:18'!
byteContents
	readLimit _ readLimit max: position.
	^collection copyFrom: 1 to: position.! !

!EncodedWriteStream methodsFor: 'private' stamp: 'jmv 5/30/2022 10:06:18'!
isCompatibleWithContents: aCollection

	aCollection isString
		ifTrue: [ ^true ].

	(aCollection is: #ByteArray)
		ifTrue: [ ^true ].

	^ false! !

!EncodedWriteStream methodsFor: 'private' stamp: 'jmv 5/28/2022 17:30:29'!
nextPutByte: aByte 
	"Insert the argument at the next position in the Stream represented by the receiver."

	position >= writeLimit
		ifTrue: [^ self pastEndPutByte: aByte]
		ifFalse: [
			position _ position + 1.
			^collection at: position put: aByte]! !

!EncodedWriteStream methodsFor: 'private' stamp: 'jmv 5/30/2022 10:23:37'!
pastEndPut: anObject
	self shouldNotImplement! !

!EncodedWriteStream methodsFor: 'accessing' stamp: 'jmv 5/30/2022 09:37:20'!
collectionSpecies

	self shouldNotImplement! !

!EncodedWriteStream methodsFor: 'accessing' stamp: 'jmv 5/30/2022 09:37:04'!
contents
	"We were built to create and answer an instance of Utf8String"

	^self utf8Contents! !

!EncodedWriteStream methodsFor: 'accessing' stamp: 'jmv 5/31/2022 12:52:16'!
utf8Contents
	"Answer the Utf8String built so far."

	^Utf8String withBytes: self byteContents.! !


!EncodedFileStream methodsFor: 'open/close' stamp: 'jmv 5/25/2022 16:58:41'!
close
	"Close this file."

	fileID ifNotNil: [
		collection ifNotNil: [
			readLimit := position := 0 ].
		self primClose: fileID.
		self unregister.
		fileID := nil].
! !

!EncodedFileStream methodsFor: 'open/close' stamp: 'jmv 5/25/2022 16:58:41'!
closed
	"Answer true if this file is closed."

	^ fileID == nil or: [ (self primSizeNoError: fileID) == nil ]
! !

!EncodedFileStream methodsFor: 'open/close' stamp: 'jmv 5/25/2022 16:58:41'!
ensureOpen
	"Make sure that this file really is open."

	self closed ifTrue: [^ self reopen].
	(self primSizeNoError: fileID) ifNotNil: [^ self].
	self reopen.
! !

!EncodedFileStream methodsFor: 'open/close' stamp: 'jmv 5/26/2022 11:30:05'!
open: fileName forWrite: writeMode
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."
	"Changed to do a GC and retry before failing ar 3/21/98 17:25"
	fileID _ EncodedFileStream retryWithGC: [ self primOpen: fileName asUtf8Bytes writable: writeMode ]
					until: [ :id | id notNil ]
					forFileNamed: fileName.
	fileID ifNil: [^ nil].  "allows sender to detect failure"
	name _ fileName.
	"jmv: Register after setting name. Name is assumed to be defined for registered objects."
	self register.
	rwmode _ writeMode.
	buffer1 _ ByteArray new: 1.
	self useCharacters.	  	"By default"
self useUtf8String.
	self enableReadBuffering.
! !

!EncodedFileStream methodsFor: 'open/close' stamp: 'jmv 5/25/2022 16:58:41'!
openReadOnly
	"Open the receiver as a read-only file.  1/31/96 sw"

	^ self open: name forWrite: false! !

!EncodedFileStream methodsFor: 'open/close' stamp: 'jmv 5/25/2022 16:58:41'!
reopen
	"Close and reopen this file. The file position is reset to zero."
	"Details: Files that were open when a snapshot occurs are no longer valid when the snapshot is resumed. This operation re-opens the file if that has happened."

	fileID ifNotNil: [
		collection ifNotNil: [
			position < readLimit ifTrue: [
				self flushReadBuffer ] ].
		self primCloseNoError: fileID ].
	self open: name forWrite: rwmode.! !

!EncodedFileStream methodsFor: 'properties-setting' stamp: 'jmv 5/25/2022 16:58:41'!
isReadOnly

	^ rwmode not
! !

!EncodedFileStream methodsFor: 'properties-setting' stamp: 'jmv 5/25/2022 16:58:41'!
readOnly
	"Make this file read-only."

	rwmode _ false.
! !

!EncodedFileStream methodsFor: 'properties-setting' stamp: 'jmv 5/25/2022 16:58:41'!
readWrite
	"Make this file writable."

	rwmode _ true.
! !

!EncodedFileStream methodsFor: 'access' stamp: 'jmv 5/25/2022 16:58:41'!
directory
	"Return the directory containing this file."

	^ self fileEntry parent! !

!EncodedFileStream methodsFor: 'access' stamp: 'jmv 5/25/2022 16:58:41'!
fileEntry
	^ self fullName asFileEntry! !

!EncodedFileStream methodsFor: 'access' stamp: 'jmv 5/25/2022 16:58:41'!
fullName
	"Answer this file's full path name."

	^ name
! !

!EncodedFileStream methodsFor: 'access' stamp: 'jmv 5/25/2022 16:58:41'!
localName
	^ self fileEntry name! !

!EncodedFileStream methodsFor: 'access' stamp: 'jmv 5/25/2022 16:58:41'!
name
	"Answer this file's full path name."

	^ name
! !

!EncodedFileStream methodsFor: 'access' stamp: 'jmv 5/25/2022 16:58:09'!
peekFor: aCharacter 
	"Answer false and do not move over the next element if it is not equal to the argument, aCharacter 
	Answer true and increment the position for accessing elements, if the next element is equal to anObject."

	| nextChar |
	nextChar _ self peek.
	aCharacter = nextChar ifTrue: [
		self next.
		^ true].
	^ false! !

!EncodedFileStream methodsFor: 'access' stamp: 'jmv 5/25/2022 16:58:41'!
printOn: aStream
	"Put a printed version of the receiver onto aStream.  1/31/96 sw"

	aStream nextPutAll: self class name; nextPutAll: ': '; print: name! !

!EncodedFileStream methodsFor: 'access' stamp: 'jmv 5/28/2022 17:47:46'!
reset
	self ensureOpen.
	self position: 0.! !

!EncodedFileStream methodsFor: 'access' stamp: 'jmv 5/25/2022 16:58:41'!
size
	"Answer the size of the file in characters.  2/12/96 sw"

	^ self primSize: fileID! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/25/2022 16:58:41'!
atEnd
	"Answer whether the receiver is at its end.  "
	
	collection ifNotNil: [
		position < readLimit ifTrue: [ ^false ] ].
	^self primAtEnd: fileID! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/25/2022 16:58:41'!
crc16
	"Copied from String>>crc16"
	
	| buffer crc |
	self reset.
	buffer _ ByteArray new: 2000.
	crc _ 0.
	[ self atEnd ] whileFalse: [
		buffer _ self nextInto: buffer.
		buffer do: [:c |
			crc _ (crc bitShift: -8) bitXor: (
			 #(	16r0000	16rC0C1	16rC181	16r0140	16rC301	16r03C0	16r0280	16rC241
				16rC601	16r06C0	16r0780	16rC741	16r0500	16rC5C1	16rC481	16r0440
				16rCC01	16r0CC0	16r0D80	16rCD41	16r0F00	16rCFC1	16rCE81	16r0E40
				16r0A00	16rCAC1	16rCB81	16r0B40	16rC901	16r09C0	16r0880	16rC841
				16rD801	16r18C0	16r1980	16rD941	16r1B00	16rDBC1	16rDA81	16r1A40
				16r1E00	16rDEC1	16rDF81	16r1F40	16rDD01	16r1DC0	16r1C80	16rDC41
				16r1400	16rD4C1	16rD581	16r1540	16rD701	16r17C0	16r1680	16rD641
				16rD201	16r12C0	16r1380	16rD341	16r1100	16rD1C1	16rD081	16r1040
				16rF001	16r30C0	16r3180	16rF141	16r3300	16rF3C1	16rF281	16r3240
				16r3600	16rF6C1	16rF781	16r3740	16rF501	16r35C0	16r3480	16rF441
				16r3C00	16rFCC1	16rFD81	16r3D40	16rFF01	16r3FC0	16r3E80	16rFE41
				16rFA01	16r3AC0	16r3B80	16rFB41	16r3900	16rF9C1	16rF881	16r3840
				16r2800	16rE8C1	16rE981	16r2940	16rEB01	16r2BC0	16r2A80	16rEA41
				16rEE01	16r2EC0	16r2F80	16rEF41	16r2D00	16rEDC1	16rEC81	16r2C40
				16rE401	16r24C0	16r2580	16rE541	16r2700	16rE7C1	16rE681	16r2640
				16r2200	16rE2C1	16rE381	16r2340	16rE101	16r21C0	16r2080	16rE041
				16rA001	16r60C0	16r6180	16rA141	16r6300	16rA3C1	16rA281	16r6240
				16r6600	16rA6C1	16rA781	16r6740	16rA501	16r65C0	16r6480	16rA441
				16r6C00	16rACC1	16rAD81	16r6D40	16rAF01	16r6FC0	16r6E80	16rAE41
				16rAA01	16r6AC0	16r6B80	16rAB41	16r6900	16rA9C1	16rA881	16r6840
				16r7800	16rB8C1	16rB981	16r7940	16rBB01	16r7BC0	16r7A80	16rBA41
				16rBE01	16r7EC0	16r7F80	16rBF41	16r7D00	16rBDC1	16rBC81	16r7C40
				16rB401	16r74C0	16r7580	16rB541	16r7700	16rB7C1	16rB681	16r7640
				16r7200	16rB2C1	16rB381	16r7340	16rB101	16r71C0	16r7080	16rB041
				16r5000	16r90C1	16r9181	16r5140	16r9301	16r53C0	16r5280	16r9241
				16r9601	16r56C0	16r5780	16r9741	16r5500	16r95C1	16r9481	16r5440
				16r9C01	16r5CC0	16r5D80	16r9D41	16r5F00	16r9FC1	16r9E81	16r5E40
				16r5A00	16r9AC1	16r9B81	16r5B40	16r9901	16r59C0	16r5880	16r9841
				16r8801	16r48C0	16r4980	16r8941	16r4B00	16r8BC1	16r8A81	16r4A40
				16r4E00	16r8EC1	16r8F81	16r4F40	16r8D01	16r4DC0	16r4C80	16r8C41
				16r4400	16r84C1	16r8581	16r4540	16r8701	16r47C0	16r4680	16r8641
				16r8201	16r42C0	16r4380	16r8341	16r4100	16r81C1	16r8081	16r4040)
			 at: ((crc bitXor: c) bitAnd: 16rFF) + 1) 
		].
	].
	^crc! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/25/2022 16:58:41'!
flush
	"Flush pending changes"
	^self primFlush: fileID! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/25/2022 16:58:09'!
next
	"Answer the next entry from this file, or nil if at the end of the file.
	Answer will be
	- CodePoint (for #useUtf8String and #useUtf32String)
	- Character (for #useCharacters)
	- SmallInteger (for #useBytes)"

	^readAdaptor next! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/27/2022 10:01:22'!
next: n
	"Return a string with the next n characters of the filestream in it.  1/31/96 sw"
'--------' print.
thisContext printStack: 10.
self halt.
	^ self nextInto: (ByteArray new: n)! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/27/2022 10:02:05'!
next: n into: aBytesOrWordsObject startingAt: startIndex
	"Read n bytes or words into the given aBytesOrWordsObject.
	Return aBytesOrWordsObject or a partial copy if less than
	n elements have been read."
	
	| count  newN newStartIndex |
true ifTrue: [
'--------' print.
thisContext printStack: 10.
self halt. "ojo"
"si se que son bytes quizas preferir
nextBytes: n into: aBytesOrWordsObject startingAt: startIndex
"
].
	collection 
		ifNil: [ 
			newN := n.
			newStartIndex := startIndex ]
		ifNotNil: [
			aBytesOrWordsObject class isBytes 
				ifFalse: [ 
					position < readLimit ifTrue: [ self flushReadBuffer ].
					newN := n.
					newStartIndex := startIndex ]
				ifTrue: [
					| available |
					(available := readLimit - position) > 0 
						ifFalse: [ available := 0 ]
						ifTrue: [
							| bufferedCount |
							bufferedCount := n min: available.
							aBytesOrWordsObject
								replaceFrom: startIndex
								to: startIndex + bufferedCount - 1
								with: collection
								startingAt: position + 1.
							position := position + bufferedCount.
							bufferedCount = n ifTrue: [ ^aBytesOrWordsObject ] ].
					newN := n - available.
					newStartIndex := startIndex + available ] ].
	count := self primRead: fileID into: aBytesOrWordsObject
				startingAt: newStartIndex count: newN.
	count = newN
		ifTrue:[ ^aBytesOrWordsObject ]
		ifFalse:[ ^aBytesOrWordsObject copyFrom: 1 to: newStartIndex + count - 1 ]! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/27/2022 10:23:30'!
next: anInteger putAll: aString startingAt: startIndex
	"Store the next anInteger elements from the given collection."
'--------' print.
thisContext printStack: 10.
self halt.
	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [
		position < readLimit ifTrue: [ self flushReadBuffer ] ].	
	self primWrite: fileID from: aString startingAt: startIndex count: anInteger.
	lastWrittenElement _ aString at: anInteger.
	^aString! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/25/2022 16:58:41'!
nextBytes: n into: aBytesOrWordsObject startingAt: startIndex
	"Read n bytes or words into the given aBytesOrWordsObject.
	Return aBytesOrWordsObject or a partial copy if less than
	n elements have been read."
	
	| count  newN newStartIndex |

	collection 
		ifNil: [ 
			newN := n.
			newStartIndex := startIndex ]
		ifNotNil: [
			aBytesOrWordsObject class isBytes 
				ifFalse: [ 
					position < readLimit ifTrue: [ self flushReadBuffer ].
					newN := n.
					newStartIndex := startIndex ]
				ifTrue: [
					| available |
					(available := readLimit - position) > 0 
						ifFalse: [ available := 0 ]
						ifTrue: [
							| bufferedCount |
							bufferedCount := n min: available.
							aBytesOrWordsObject
								replaceFrom: startIndex
								to: startIndex + bufferedCount - 1
								with: collection
								startingAt: position + 1.
							position := position + bufferedCount.
							bufferedCount = n ifTrue: [ ^aBytesOrWordsObject ] ].
					newN := n - available.
					newStartIndex := startIndex + available ] ].
	count := self primRead: fileID into: aBytesOrWordsObject
				startingAt: newStartIndex count: newN.
	count = newN
		ifTrue:[ ^aBytesOrWordsObject ]
		ifFalse:[ ^aBytesOrWordsObject copyFrom: 1 to: newStartIndex + count - 1 ]! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/27/2022 10:25:46'!
nextPut: aCodePointOrCharacterOrByte
	"Insert the argument at the next position in the Stream represented by the receiver.
	Argument can be Character, CodePoint or Byte (SmallInteger).
	Write to this stream. Character and CodePoint are UTF-8 encoded."

	aCodePointOrCharacterOrByte isNumber
		ifTrue: [self nextPutByte: aCodePointOrCharacterOrByte]
		ifFalse: [self nextPutCodePoint: aCodePointOrCharacterOrByte codePoint].
	lastWrittenElement _ aCodePointOrCharacterOrByte.! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/27/2022 10:24:25'!
nextPutAll: aStringOrBytes
	"Argument can be String, Utf8String, Utf32String or ByteArray.
	Write to this file. All 3 kinds of Strings are UTF-8 encoded."

	| bytes |
	bytes _ aStringOrBytes.
	(aStringOrBytes is: #ByteArray) ifFalse: [
		bytes _ aStringOrBytes asUtf8Bytes].
	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	self primWrite: fileID from: bytes startingAt: 1 count: bytes basicSize.
	lastWrittenElement _ aStringOrBytes last.! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/25/2022 16:58:09'!
nextPutCodePoint: aNumber
	"UTF-8 encoded"

	Character
		evaluate: [ :byte | self nextPutByte: byte ]
		withUtf8BytesOfUnicodeCodePoint: aNumber! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:44:57'!
nextUtf8BytesAndCodePointInto: bytesBlock into: codePointBlock
	"Find the next UTF-8 CodePoint from this file, assuming next byte is ISO 8859-15 if invalid UTF-8,
	or nil if at the end of the file."

	| bytesConsumed previousPosition |
	previousPosition _ self position.
	bytesConsumed _ 0.
	self nextByte ifNotNil: [ :byte1 |
		bytesConsumed _ Character
			utf8BytesAndCodePointFor: byte1
			byte2: self nextByte
			byte3: self nextByte
			byte4: self nextByte
			into: bytesBlock
			into: codePointBlock ].
	self position: previousPosition + bytesConsumed.! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/27/2022 10:01:29'!
nextWordsInto: aBitmap
	"Note: The file primitives automatically adjust for word based objects."

'--------' print.
thisContext printStack: 10.
self halt.
	self next: aBitmap basicSize into: aBitmap startingAt: 1.
	aBitmap restoreEndianness.
	^ aBitmap! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:45:02'!
padToEndIfCantTruncate
	"Only makes sense for file streams with existing content.
	On file systems that don't support truncating this is needed.
	If truncating is supported, try that first"

	"On the Mac, files do not truncate.  One can delete the old file and write a new one, but sometime deletion fails (file still open? file stale?).  This is a sad compromise.  Just let the file be the same length but pad it with a harmless character."

	| pad |
	self atEnd ifTrue: [^ self].
	self truncate.
	self atEnd ifTrue: [^ self].
'--------' print.
thisContext printStack: 10.
self halt.
	pad := Character space numericValue.
	self nextPutAll: (ByteArray new: ((self size - self position) min: 20000) 
							withAll: pad)! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:45:07'!
peek
	"Answer what would be returned if the message next were sent to the receiver.
	If the receiver is at the end, answer nil.  "

	| next currentPosition |
	self atEnd ifTrue: [^ nil].
	currentPosition := self position.
	next _ self next.
	self position: currentPosition.
	^ next! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/27/2022 10:28:13'!
peekLast

	^lastWrittenElement! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:43:46'!
position
	"Return the receiver's current file position."

	collection ifNotNil: [
		position < readLimit ifTrue: [
			^(self primGetPosition: fileID) - readLimit + position ] ].
	^self primGetPosition: fileID! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/30/2022 14:58:11'!
position: pos
	"Set the receiver's position as indicated."

	| m s |
	"Chequear que los senders sepan que estan mandando una posicion a byte!!"
	m _ thisContext sender method.
	thisContext sender receiver == self ifFalse: [
		thisContext sender receiver == readAdaptor ifFalse: [
			(EncodedFileStream positionValidatedSenders includes: m) ifFalse: [
				"SendersOfPositionColon _ Set new"
				s _ Smalltalk at: #SendersOfPositionColon ifAbsentPut: [ Set new ].
				(s includes: m) ifFalse: [
					s add: m.
					{ '-------- Sender de #position: '. m. pos} print.
					thisContext printStack: 10 ]]]].

	collection ifNotNil: [
		position < readLimit ifTrue: [
			| newPosition |
			newPosition := pos - (self primGetPosition: fileID) + readLimit.
			newPosition < 0 ifTrue: [
					| offsetPos |
					self primSetPosition: fileID to: (offsetPos := pos - (collection size // 4) max: 0).
					readLimit := self primRead: fileID into: collection startingAt: 1 count: collection size.
					position := pos - offsetPos.
					^self ].
			newPosition < readLimit 
				ifTrue: [
					position := newPosition.
					^self ]
				ifFalse: [
					readLimit := position := 0 ] ] ].
	^self primSetPosition: fileID to: pos! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/27/2022 10:01:38'!
readInto: byteArray startingAt: startIndex count: count
	"Read into the given array as specified, and return the count
	actually transferred.  index and count are in units of bytes or
	longs depending on whether the array is Bitmap, String or ByteArray"

'--------' print.
thisContext printStack: 10.
self halt.
	^(self next: count into: byteArray startingAt: startIndex) size - startIndex + 1
! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:47:55'!
setToEnd
	"Set the position of the receiver to the end of file.  1/31/96 sw"

	self position: self size! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/26/2022 14:38:48'!
skip
	"Skips one object in the receiver's future sequence values."

	self next! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/26/2022 14:36:56'!
skip: n
	n < 0 ifTrue: [
		^n negated timesRepeat: [ self skipBack ]].
	n timesRepeat: [ self skip ].! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:46:21'!
skipBack
	| savePos try |
	savePos _ self position.

	"Try going back one byte. If ASCII, then OK, we are done.
	If not, don't assume ISO-8859-15 so lightly!!"
	try _ savePos-1.
	self position: try.
	self nextByte < 128 ifTrue: [
		^self position: try ].

	"Try going back two bytes. If doing #next leaves us at savePosition, we found a two byte CodePoint."
	try _ try-1.
	self position: try.
	(self next notNil and: [ self position = savePos ]) ifTrue: [
		^self position: try ].

	"Try going back three bytes. If doing #next leaves us at savePosition, we found a three byte CodePoint."
	try _ try-1.
	self position: try.
	(self next notNil and: [ self position = savePos ]) ifTrue: [
		^self position: try ].

	"Try going back four bytes. If doing #next leaves us at savePosition, we found a four byte CodePoint."
	try _ try-1.
	self position: try.
	(self next notNil and: [ self position = savePos ]) ifTrue: [
		^self position: try ].

	"Havent found a valid UTF-8 sequence that ends just before savePos.
	It means back byte is an ISO-8859-15 Character"
	self position: savePos-1.
	! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/25/2022 16:58:41'!
truncate
	"Truncate to zero"

	^ self truncate: 0! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:47:58'!
truncate: bytePosition
	"Truncate to this position"

	self position: bytePosition.
	^self primTruncate: fileID to: bytePosition.! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/28/2022 17:45:59'!
upTo: delimiter delimiterIsTerminator: delimiterIsTerminator
	"See comment at PositionableStream.
	Fast version to speed up nextChunk"

	| pos buffer count skipSeparator tryAgain |

'--------' print.
thisContext printStack: 10.
self halt.
	skipSeparator _ delimiterIsTerminator ifTrue: [0] ifFalse: [1].
	collection ifNotNil: [
		(position < readLimit and: [
			(pos _ collection indexOf: delimiter startingAt: position + 1) <= readLimit and: [
				pos > 0 ] ]) ifTrue: [
					^ collection copyFrom: position + 1 to: (position _ pos) - skipSeparator ] ].

	pos _ self position.
	buffer _ self next: 2000.
	(count _ buffer indexOf: delimiter) > 0 ifTrue: [
		"Found the delimiter part way into buffer"
		self position: pos + count.
		^ buffer copyFrom: 1 to: count - skipSeparator].

	self atEnd ifTrue: [
		"Never found it, and hit end of file"
		^ delimiterIsTerminator ifTrue: [self position: pos. nil] ifFalse: [buffer]].

	"Never found it, but there's more..."
	tryAgain _ self upTo: delimiter delimiterIsTerminator: delimiterIsTerminator.
	tryAgain ifNil: [
		self position: pos.
		^ nil ].
	^ buffer, tryAgain.! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/30/2022 10:24:32'!
withAttribute: aTextAttribute do: streamBlock
'--------' print.
thisContext printStack: 10.
self halt.! !

!EncodedFileStream methodsFor: 'read, write, position' stamp: 'jmv 5/30/2022 10:24:44'!
withAttributes: attributes do: streamBlock 
'--------' print.
thisContext printStack: 10.
self halt.! !

!EncodedFileStream methodsFor: 'testing' stamp: 'jmv 5/25/2022 16:58:41'!
isFileStream
	^true! !

!EncodedFileStream methodsFor: 'primitives' stamp: 'jmv 5/25/2022 16:58:41'!
primAtEnd: id
	"Answer true if the file position is at the end of the file."

	<primitive: 'primitiveFileAtEnd' module: 'FilePlugin'>
	self primitiveFailed
! !

!EncodedFileStream methodsFor: 'primitives' stamp: 'jmv 5/25/2022 16:58:41'!
primClose: id
	"Close this file."

	<primitive: 'primitiveFileClose' module: 'FilePlugin'>
	self primitiveFailed
! !

!EncodedFileStream methodsFor: 'primitives' stamp: 'jmv 5/25/2022 16:58:41'!
primCloseNoError: id
	"Close this file. Don't raise an error if the primitive fails."

	<primitive: 'primitiveFileClose' module: 'FilePlugin'>
! !

!EncodedFileStream methodsFor: 'primitives' stamp: 'jmv 5/28/2022 17:45:16'!
primFlush: id
	"Flush pending changes to the disk"
	| p |
	<primitive: 'primitiveFileFlush' module: 'FilePlugin'>
	"In some OS's seeking to 0 and back will do a flush"
	p _ self position.
	self position: 0; position: p! !

!EncodedFileStream methodsFor: 'primitives' stamp: 'jmv 5/25/2022 16:58:41'!
primGetPosition: id
	"Get this files current position."

	<primitive: 'primitiveFileGetPosition' module: 'FilePlugin'>
	self primitiveFailed
! !

!EncodedFileStream methodsFor: 'primitives' stamp: 'jmv 5/25/2022 16:58:41'!
primOpen: fileNameAsUtf8Bytes writable: writableFlag
	"Open a file of the given name, and return the file ID obtained.
	If writableFlag is true, then
		if there is none with this name, then create one
		else prepare to overwrite the existing from the beginning
	otherwise
		if the file exists, open it read-only
		else return nil"

	<primitive: 'primitiveFileOpen' module: 'FilePlugin'>
	^ nil
! !

!EncodedFileStream methodsFor: 'primitives' stamp: 'jmv 5/25/2022 16:58:41'!
primRead: id into: byteArray startingAt: startIndex count: count
	"Read up to count bytes of data from this file into the given string or byte array starting at the given index. Answer the number of bytes actually read."

	<primitive: 'primitiveFileRead' module: 'FilePlugin'>
	self closed ifTrue: [^ self error: 'File is closed'].
	self error: 'File read failed'.
! !

!EncodedFileStream methodsFor: 'primitives' stamp: 'jmv 5/25/2022 16:58:41'!
primSetPosition: id to: anInteger
	"Set this file to the given position."

	<primitive: 'primitiveFileSetPosition' module: 'FilePlugin'>
	self primitiveFailed
! !

!EncodedFileStream methodsFor: 'primitives' stamp: 'jmv 5/25/2022 16:58:41'!
primSize: id
	"Answer the size of this file."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	self primitiveFailed
! !

!EncodedFileStream methodsFor: 'primitives' stamp: 'jmv 5/25/2022 16:58:41'!
primSizeNoError: id
	"Answer the size of this file. Answer nil if the primitive fails; this indicates that the file handle has become stale."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	^ nil
! !

!EncodedFileStream methodsFor: 'primitives' stamp: 'jmv 5/25/2022 16:58:41'!
primTruncate: id to: anInteger
	"Truncate this file to the given position."

	<primitive: 'primitiveFileTruncate' module: 'FilePlugin'>
	self primitiveFailed
! !

!EncodedFileStream methodsFor: 'primitives' stamp: 'jmv 5/25/2022 16:58:41'!
primWrite: id from: stringOrByteArray startingAt: startIndex count: count
	"Write count bytes onto this file from the given string or byte array starting at the given index. Answer the number of bytes written."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>
	(FileWriteError fileName: name)
		signal: (self closed
			ifTrue: [ 'File [', name, '] is closed' ]
			ifFalse: [ 'File [', name, '] write failed' ])! !

!EncodedFileStream methodsFor: 'registry' stamp: 'jmv 5/25/2022 16:58:41'!
register
	^self class register: self! !

!EncodedFileStream methodsFor: 'registry' stamp: 'jmv 5/25/2022 16:58:41'!
unregister
	^self class unregister: self! !

!EncodedFileStream methodsFor: 'finalization' stamp: 'jmv 5/25/2022 16:58:41'!
actAsExecutor
	super actAsExecutor.
	name := nil.! !

!EncodedFileStream methodsFor: 'finalization' stamp: 'jmv 5/25/2022 16:58:41'!
finalize
	self primCloseNoError: fileID.! !

!EncodedFileStream methodsFor: 'private' stamp: 'jmv 5/31/2022 09:48:05'!
bytesUpToEnd
	"Leave stream at the end position."

	| bytes s |
	s _ self size.
	bytes _ ByteArray new: s.
	^ self nextBytes: s into: bytes startingAt: 1.! !

!EncodedFileStream methodsFor: 'private' stamp: 'jmv 5/28/2022 17:44:44'!
disableReadBuffering

	collection ifNotNil: [
		position < readLimit
			ifTrue: [
				| currentPosition |
				currentPosition := self position.
				collection := readLimit := position := nil.
				self position: currentPosition ]
			ifFalse: [
				collection := readLimit := position := nil ] ]
		! !

!EncodedFileStream methodsFor: 'private' stamp: 'jmv 5/25/2022 16:58:41'!
enableReadBuffering

	collection ifNil: [
		buffer1 ifNotNil: [
			collection := ByteArray new: 2048 ] ].
	readLimit := position := 0! !

!EncodedFileStream methodsFor: 'private' stamp: 'jmv 5/28/2022 17:44:47'!
flushReadBuffer

	collection ifNotNil: [
		position < readLimit ifTrue: [
			| currentPosition |
			currentPosition := self position.
			position := readLimit := 0.
			self primSetPosition: fileID to: currentPosition ] ]! !

!EncodedFileStream methodsFor: 'private' stamp: 'jmv 5/25/2022 16:58:41'!
nextByte
	"Answer the next byte from this file, or nil if at the end of the file."
	
	| count |
	collection ifNotNil: [
		position < readLimit 
			ifFalse: [ 
				readLimit := self primRead: fileID into: collection startingAt: 1 count: collection size.
				position := 0.
				readLimit = 0 ifTrue: [ ^nil ] ].
		^collection at: (position := position + 1) ].	
	count _ self primRead: fileID into: buffer1 startingAt: 1 count: 1.
	^count = 1
		ifTrue: [ buffer1 at: 1 ]! !

!EncodedFileStream methodsFor: 'private' stamp: 'jmv 5/25/2022 16:58:09'!
nextCodePoint
	| answer |
	answer _ nil.
	self nextUtf8BytesAndCodePointInto: nil into: [ :codePoint |
		answer _ UnicodeCodePoint codePoint: codePoint ].
	^answer! !

!EncodedFileStream methodsFor: 'private' stamp: 'jmv 5/25/2022 16:58:41'!
nextPutByte: aByte
	"Write the given byte to this file."

	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	buffer1 at: 1 put: aByte.
	self primWrite: fileID from: buffer1 startingAt: 1 count: 1.! !

!EncodedFileStream methodsFor: 'private' stamp: 'jmv 5/30/2022 10:23:41'!
pastEndPut: anObject
	self shouldNotImplement! !

!EncodedFileStream methodsFor: 'printing' stamp: 'jmv 5/25/2022 16:58:41'!
longPrintOn: aStream
	"Do nothing, so it will print short.  Called to print the error file.  If the error was in a file operation, we can't read the contents of that file.  Just print its name instead."
! !

!EncodedFileStream methodsFor: 'printing' stamp: 'jmv 5/25/2022 16:58:41'!
longPrintOn: aStream limitedTo: sizeLimit indent: indent

	"Do nothing, so it will print short.  Called to print the error file.  If the error was in a file operation, we can't read the contents of that file.  Just print its name instead."

	aStream newLine! !

!EncodedFileStream methodsFor: 'printing' stamp: 'jmv 5/25/2022 16:58:41'!
storeOn: aStream
	"A file can't be serialized"

	^ self printOn: aStream! !

!EncodedFileStream methodsFor: 'accessing' stamp: 'jmv 5/25/2022 16:58:09'!
collectionSpecies
	^readAdaptor collectionSpecies! !

!EncodedFileStream methodsFor: 'accessing' stamp: 'jmv 5/31/2022 09:53:05'!
contents
	"Return the full contents of the receiver. Do not close or otherwise touch the receiver.
	Return data in whatever mode the receiver is in (i.e. answer an instance of the appropriate class).
	Stream bytes as CodePoints (prefer UTF-8, but ISO-8859-15 if invalid UTF-8)."

	| savePos answer |
	savePos _ self position.
	self position: 0.
	answer _ self upToEnd.
	self position: savePos.
	^answer! !

!EncodedFileStream methodsFor: 'accessing' stamp: 'jmv 5/25/2022 16:58:41'!
contentsOfEntireFile
	"Read all of the contents of the receiver."

	| answer |
	self readOnly.
	self reset.
	answer _ self contents.
	self close.
	^answer! !

!EncodedFileStream methodsFor: 'accessing' stamp: 'jmv 5/30/2022 17:17:41'!
upToEnd
	"Return the contents of the receiver. Do not close or otherwise touch the receiver.
	Return data in whatever mode the receiver is in (i.e. answer an instance of the appropriate class)."

	^readAdaptor streamUpToEnd! !

!EncodedFileStream methodsFor: 'fileIn/Out' stamp: 'jmv 5/25/2022 16:58:41'!
fileIn
	"Guarantee that the receiver is readOnly before fileIn for efficiency and
	to eliminate remote sharing conflicts."

	self readOnly.
	self fileInAnnouncing: 'Loading ', self localName.
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ].
! !

!EncodedFileStream methodsFor: 'reading modes' stamp: 'jmv 5/25/2022 17:09:17'!
useBytes
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String,  Utf8String, Utf32String,
	according to protocol."
	readAdaptor _ BytesEncodedStreamReadAdaptor new stream: self.! !

!EncodedFileStream methodsFor: 'reading modes' stamp: 'jmv 5/25/2022 17:09:23'!
useCharacters
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String,  Utf8String, Utf32String,
	according to protocol."
	readAdaptor _ StringEncodedStreamReadAdaptor new stream: self.! !

!EncodedFileStream methodsFor: 'reading modes' stamp: 'jmv 5/25/2022 17:09:27'!
useUtf32String
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String,  Utf8String, Utf32String,
	according to protocol."
	readAdaptor _ Utf32EncodedStreamReadAdaptor new stream: self.! !

!EncodedFileStream methodsFor: 'reading modes' stamp: 'jmv 5/25/2022 17:09:31'!
useUtf8String
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String,  Utf8String, Utf32String,
	according to protocol."
	readAdaptor _ Utf8EncodedStreamReadAdaptor new stream: self.! !


!EncodedFileStream class methodsFor: 'instance creation' stamp: 'jmv 5/25/2022 16:58:41'!
new
	^ self basicNew initialize! !

!EncodedFileStream class methodsFor: 'registry' stamp: 'jmv 5/25/2022 16:58:41'!
register: anObject
	
	^self registry add: anObject! !

!EncodedFileStream class methodsFor: 'registry' stamp: 'jmv 5/25/2022 16:58:41'!
registry
	
	^Registry ifNil: [ Registry := WeakRegistry new ]! !

!EncodedFileStream class methodsFor: 'registry' stamp: 'jmv 5/25/2022 16:58:41'!
retryWithGC: execBlock until: testBlock forFileNamed: fullName
	"Re-implemented to only force GC if a file with the given name exists"
	| blockValue foundIt |
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	"See if we have a file with the given name"
	foundIt _ self registry keys "hold on strongly for now" 
		anySatisfy:[:file| file name sameAs: fullName].
	foundIt ifFalse:[^blockValue].
	Smalltalk garbageCollectMost.
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	Smalltalk garbageCollect.
	^execBlock value.! !

!EncodedFileStream class methodsFor: 'registry' stamp: 'jmv 5/25/2022 16:58:41'!
unregister: anObject
	
	^self registry remove: anObject ifAbsent: nil! !

!EncodedFileStream class methodsFor: 'error handling' stamp: 'jmv 5/27/2022 10:01:55'!
fileDoesNotExistUserHandling: fullFileName

	| selection newName |
	selection _ (PopUpMenu labels:
'create a new file
choose another name
cancel')
			startUpWithCaption: fullFileName asFileEntry name, '
does not exist.'.
	selection = 1 ifTrue:
		[^ self new open: fullFileName forWrite: true].
	selection = 2 ifTrue:
		[ newName _ self request: 'Enter a new file name'
						initialAnswer:  fullFileName.
		^ FileIOAccessor default privateWriteableFile: newName asFileEntry ].
'--------' print.
thisContext printStack: 10.
	self halt! !

!EncodedFileStream class methodsFor: 'error handling' stamp: 'jmv 5/25/2022 16:58:41'!
fileExistsUserHandling: fullFileName
	| dir localName choice newName entry |
	entry _ fullFileName asFileEntry.
	dir _ entry parent.
	localName _ entry name.
	choice _ (PopUpMenu
		labels:
'overwrite that file\choose another name\cancel' withNewLines)
		startUpWithCaption: localName, '
already exists.'.

	choice = 1 ifTrue: [
		dir removeKey: localName
			ifAbsent: [self error: 'Could not delete the old version of that file'].
		^ self new open: fullFileName forWrite: true].

	choice = 2 ifTrue: [
		newName _ self request: 'Enter a new file name' initialAnswer: fullFileName.
		^ FileIOAccessor default privateNewFile: newName asFileEntry ].

	self error: 'Please close this to abort file opening'! !

!EncodedFileStream class methodsFor: 'error handling' stamp: 'jmv 5/25/2022 16:58:41'!
readOnlyFileDoesNotExistUserHandling: fullFileName

	| dir files choices selection newName fileName |
	dir _ fullFileName asFileEntry parent.
	files _ dir fileNames.
	fileName _ fullFileName asFileEntry name.
	choices _ fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	selection _ (PopUpMenu labelArray: choices lines: (Array with: 5) )
		startUpWithCaption: fullFileName asFileEntry name, '
does not exist.'.
	selection = choices size ifTrue:["cancel" ^ nil "should we raise another exception here?"].
	selection < (choices size - 1) ifTrue: [
		newName _ (dir pathName , '/', (choices at: selection))].
	selection = (choices size - 1) ifTrue: [
		newName _ self request: 'Enter a new file name' initialAnswer: fileName.
		"If Cancel was pressed, no file should be opened - Hernan"
		newName isEmpty ifTrue: [ ^nil ]].
	newName = '' ifFalse: [^ FileIOAccessor default privateReadOnlyFile: newName asFileEntry ].
	^ self error: 'Could not open a file'! !

!EncodedFileStream class methodsFor: 'cached state access' stamp: 'jmv 5/25/2022 16:58:41'!
releaseClassCachedState

	Registry ifNotNil: [
		"Make the best effort to empty the registry.
		Only keep files that are still open have references from some object."
		Registry isEmpty ifFalse: [
			Smalltalk garbageCollectMost.
			Registry isEmpty ifFalse: [
				Smalltalk garbageCollect.
				Registry isEmpty ifFalse: [
					Registry finalizeValues ]]].

		"No open files. Can nil the Registry."
		Registry isEmpty ifTrue: [
			Registry _ nil ]
	]! !

!EncodedFileStream class methodsFor: 'file creation' stamp: 'jmv 5/25/2022 17:02:07'!
crc16OfFileNamed: fileName
	"
	EncodedFileStream crc16OfFileNamed: 'cursor.jpeg'
	EncodedFileStream crc16OfFileNamed: 'deafultPID.txt'
	"

	^[fileName asFileEntry readStreamDo: [ :stream |
		stream crc16 ]]
			on: FileDoesNotExistException do: nil! !

!EncodedFileStream class methodsFor: 'as yet unclassified' stamp: 'jmv 5/31/2022 09:53:31'!
positionValidatedSenders
	^{
		ChangeList >> #scanDoIt.
		ChangeList >> #scanFile:from:to:.
		ChangeList >> #scanSpecificChangeRecordType.
		ChangeList >> #scanUpTo:informing:.
		ChangeList >> #scanCategory:class:meta:stamp:prior:overrides:.
		PositionableStream >> #skipSeparators.
		ChangeRecord >> #string.
		PositionableStream >> #back.
		PositionableStream >> #peekBack.
		PositionableStream >> #backChunk.
		CompiledMethod >> #getPreamble.
		EncodedReadStream >> #nextUtf8BytesAndCodePointInto:into:.
		Parser >> #parse:class:category:noPattern:doIt:context:notifying:ifFail:.
	}! !


!EncodedByteArrayStreamTest methodsFor: 'tests' stamp: 'jmv 5/31/2022 16:27:37'!
test01

	| utf8String utf32String stringAsIso8859s15 bytes1 bytes2 bytes3 |
	utf8String _ Utf8String withBytes: 'Hello World, ñandú, agüita. ' asUtf8Bytes, #[208 155 208 190 209 128 208 181 208 188].
	utf32String _ Utf32String withWords: utf8String asUtf32Words.
	stringAsIso8859s15 _ utf8String bytes asString.							"Includes random content, String can't handle full Unicode"

	bytes1 _ ByteArray streamUtf8Contents: [ :stream | stream nextPutAll: utf8String ].
	self assert: ((EncodedReadStream on: bytes1) useUtf8String; contents) equals: utf8String.
	self assert: ((EncodedReadStream on: bytes1) useBytes; contents) equals: bytes1.
	self assert: ((EncodedReadStream on: bytes1) useUtf32String; contents) equals: utf32String.
	self assert: ((EncodedReadStream on: bytes1) useCharacters; contents) equals: stringAsIso8859s15.

	bytes2 _ ByteArray streamUtf8Contents: [ :stream | stream nextPutAll: utf8String bytes ].
	self assert: ((EncodedReadStream on: bytes2) useUtf8String; contents) equals: utf8String.
	self assert: ((EncodedReadStream on: bytes2) useBytes; contents) equals: bytes2.
	self assert: ((EncodedReadStream on: bytes2) useUtf32String; contents) equals: utf32String.
	self assert: ((EncodedReadStream on: bytes2) useCharacters; contents) equals: stringAsIso8859s15.

	bytes3 _ ByteArray streamUtf8Contents: [ :stream | stream nextPutAll: utf32String ].
	self assert: ((EncodedReadStream on: bytes3) useUtf8String; contents) equals: utf8String.
	self assert: ((EncodedReadStream on: bytes3) useBytes; contents) equals: bytes3.
	self assert: ((EncodedReadStream on: bytes3) useUtf32String; contents) equals: utf32String.
	self assert: ((EncodedReadStream on: bytes3) useCharacters; contents) equals: stringAsIso8859s15.! !

!EncodedByteArrayStreamTest methodsFor: 'tests' stamp: 'jmv 5/31/2022 16:29:30'!
test02
	"Invalid UTF-8, but possible saved as reasonable ISO 8859-15, with use of full 8 bit range.
	To test invalid utf8 detection, and conversion from 8859-15 to utf."

	| invalidUtf8Bytes validUtf8Bytes utf8String utf32String bytes |

	"Second byte is invalid UTF-8. Created from a String (i.e. ISO 8859-15 bytes), and a valid UTF-8 ByteArray:
	'uña ' asByteArray, #[206 188]"
	invalidUtf8Bytes _ #[117 241 97 32 206 188].

	"A correct UTF-8 sequence, created by correct conversion of the String above, and the same valid UTF-8 coda.
	'uña ' asUtf8Bytes, #[206 188]"
	validUtf8Bytes _ #[117 195 177 97 32 206 188].
	
	utf8String _ Utf8String withBytes: validUtf8Bytes.
	utf32String _ Utf32String withWords: utf8String asUtf32Words.

	bytes _ ByteArray streamUtf8Contents: [ :stream | stream nextPutAll: invalidUtf8Bytes ].
	self assert: ((EncodedReadStream on: bytes) useUtf8String; contents) equals: utf8String.
	self assert: ((EncodedReadStream on: bytes) useBytes; contents) equals: bytes.
	self assert: ((EncodedReadStream on: bytes) useUtf32String; contents) equals: utf32String.! !


!EncodedFileStreamTest methodsFor: 'tests' stamp: 'jmv 5/31/2022 16:31:47'!
test01

	| bytes utf8String utf32String stringAsIso8859s15 fe1 fe2 fe3 |
	bytes _ 'Hello World, ñandú, agüita. ' asUtf8Bytes, #[208 155 208 190 209 128 208 181 208 188].
	utf8String _ Utf8String withBytes: bytes.
	utf32String _ Utf32String withWords: utf8String asUtf32Words.
	stringAsIso8859s15 _ utf8String bytes asString.							"Includes random content, String can't handle full Unicode"

	fe1 _ 'xxtes01tUtf8String.txt' asFileEntry.
	fe1 forceWriteStreamDo: [ :stream | stream nextPutAll: utf8String ].
	self assert: (fe1 readStreamDo: [ :strm | strm useUtf8String; contents ]) equals: utf8String.
	self assert: (fe1 readStreamDo: [ :strm | strm useBytes; contents ]) equals: bytes.
	self assert: (fe1 readStreamDo: [ :strm | strm useUtf32String; contents ]) equals: utf32String.
	self assert: (fe1 readStreamDo: [ :strm | strm useCharacters; contents ]) equals: stringAsIso8859s15.

	fe2 _ 'xxtest01Utf8Bytes.txt' asFileEntry.
	fe2 forceWriteStreamDo: [ :stream | stream nextPutAll: utf8String bytes ].
	self assert: (fe2 readStreamDo: [ :strm | strm useUtf8String; contents ]) equals: utf8String.
	self assert: (fe2 readStreamDo: [ :strm | strm useBytes; contents ]) equals: bytes.
	self assert: (fe2 readStreamDo: [ :strm | strm useUtf32String; contents ]) equals: utf32String.
	self assert: (fe2 readStreamDo: [ :strm | strm useCharacters; contents ]) equals: stringAsIso8859s15.

	fe3 _ 'xxtes01tUtf32String.txt' asFileEntry.
	fe3 forceWriteStreamDo: [ :stream | stream nextPutAll: utf32String ].
	self assert: (fe3 readStreamDo: [ :strm | strm useUtf8String; contents ]) equals: utf8String.
	self assert: (fe3 readStreamDo: [ :strm | strm useBytes; contents ]) equals: bytes.
	self assert: (fe3 readStreamDo: [ :strm | strm useUtf32String; contents ]) equals: utf32String.
	self assert: (fe3 readStreamDo: [ :strm | strm useCharacters; contents ]) equals: stringAsIso8859s15.

	fe1 delete.
	fe2 delete.
	fe3 delete.! !

!EncodedFileStreamTest methodsFor: 'tests' stamp: 'jmv 5/31/2022 16:32:25'!
test02
	"Invalid UTF-8, but possible saved as reasonable ISO 8859-15, with use of full 8 bit range.
	To test invalid utf8 detection, and conversion from 8859-15 to utf."

	| invalidUtf8Bytes validUtf8Bytes utf8String utf32String fe1 |

	"Second byte is invalid UTF-8. Created from a String (i.e. ISO 8859-15 bytes), and a valid UTF-8 ByteArray:
	'uña ' asByteArray, #[206 188]"
	invalidUtf8Bytes _ #[117 241 97 32 206 188].

	"A correct UTF-8 sequence, created by correct conversion of the String above, and the same valid UTF-8 coda.
	'uña ' asUtf8Bytes, #[206 188]"
	validUtf8Bytes _ #[117 195 177 97 32 206 188].
	
	utf8String _ Utf8String withBytes: validUtf8Bytes.
	utf32String _ Utf32String withWords: utf8String asUtf32Words.
	
	fe1 _ 'xxtest02InvalidUtf8Bytes.txt' asFileEntry.
	fe1 forceWriteStreamDo: [ :stream | stream nextPutAll: invalidUtf8Bytes ].
	self assert: (fe1 readStreamDo: [ :strm | strm useUtf8String; contents ]) equals: utf8String.
	self assert: (fe1 readStreamDo: [ :strm | strm useBytes; contents ]) equals: invalidUtf8Bytes.
	self assert: (fe1 readStreamDo: [ :strm | strm useUtf32String; contents ]) equals: utf32String.

	fe1 delete.! !


!FileStreamTest methodsFor: 'tests' stamp: 'jmv 5/17/2022 12:56:47'!
test01

	| ba str fe1 fe2 |
	str _ 'Hello World'.
	ba _ str asByteArray.	

	fe1 _ 'testStr.txt' asFileEntry.
	fe1 fileContents: str.
	self assert: (fe1 fileContents) equals: str.
	self assert: (fe1 readStreamDo: [ :strm | strm useBytes; contents ]) equals: ba.
	self assert: (fe1 readStreamDo: [ :strm | strm useCharacters; contents ]) equals: str.
	
	fe2 _ 'testStrBA.txt' asFileEntry.
	fe2 fileContents: ba.
	self assert: (fe2 fileContents) equals: str.
	self assert: (fe2 readStreamDo: [ :strm | strm useBytes; contents ]) equals: ba.
	self assert: (fe2 readStreamDo: [ :strm | strm useCharacters; contents ]) equals: str.

	fe1 delete.
	fe2 delete.! !

