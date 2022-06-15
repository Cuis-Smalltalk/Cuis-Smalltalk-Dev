'From Cuis 6.0 [latest update: #5286] on 15 June 2022 at 9:49:45 am'!
Smalltalk renameClassNamed: #EncodedFileStream as: #UniFileStream!
Smalltalk renameClassNamed: #EncodedWriteStream as: #Utf8EncodedWriteStream!
Smalltalk renameClassNamed: #EncodedStreamReadAdaptor as: #UniStreamReadAdaptor!
!classDefinition: #UniStreamReadAdaptor category: #'System-Files'!
Object subclass: #UniStreamReadAdaptor
	instanceVariableNames: 'stream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Files'!
Smalltalk renameClassNamed: #StringEncodedStreamReadAdaptor as: #ByteStringReadAdaptor!
!classDefinition: #ByteStringReadAdaptor category: #'System-Files'!
UniStreamReadAdaptor subclass: #ByteStringReadAdaptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Files'!
Smalltalk renameClassNamed: #BytesEncodedStreamReadAdaptor as: #BytesReadAdaptor!
!classDefinition: #BytesReadAdaptor category: #'System-Files'!
UniStreamReadAdaptor subclass: #BytesReadAdaptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Files'!
Smalltalk renameClassNamed: #Utf32EncodedStreamReadAdaptor as: #Utf32ReadAdaptor!
!classDefinition: #Utf32ReadAdaptor category: #'System-Files'!
UniStreamReadAdaptor subclass: #Utf32ReadAdaptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Files'!
Smalltalk renameClassNamed: #Utf8EncodedStreamReadAdaptor as: #Utf8ReadAdaptor!
!classDefinition: #Utf8ReadAdaptor category: #'System-Files'!
UniStreamReadAdaptor subclass: #Utf8ReadAdaptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Files'!

!Utf8String class methodsFor: 'stream creation' stamp: 'jmv 6/15/2022 09:44:15'!
streamContents: blockWithArg
	"Evaluate a block on a write stream that accepts bytes, characters.
	Arguments to #nextPutAll: can be sequences of bytes, Strings, Utf8Strings or Utf32Strings.
	Answer is an instance of us."

	| stream |	
	stream _ Utf8EncodedWriteStream on: (ByteArray new: 100).
	blockWithArg value: stream.
	^stream utf8Contents! !

!Utf8String class methodsFor: 'stream creation' stamp: 'jmv 6/15/2022 09:44:15'!
writeStream
	"Answer a write stream that accepts bytes, characters.
	Arguments to #nextPutAll: can be sequences of bytes, Strings, Utf8Strings or Utf32Strings."

	| stream |	
	stream _ Utf8EncodedWriteStream on: (ByteArray new: 100).
	^stream! !


!ByteArray class methodsFor: 'stream creation' stamp: 'jmv 6/15/2022 09:44:15'!
streamUtf8Contents: blockWithArg
	"Evaluate a block on a write stream that accepts bytes, characters.
	Arguments to #nextPutAll: can be sequences of bytes, Strings, Utf8Strings or Utf32Strings.
	Answer is an instance of us, holding UTF-8 encoded bytes."

	| stream |	
	stream _ Utf8EncodedWriteStream on: (ByteArray new: 100).
	blockWithArg value: stream.
	^stream byteContents! !


!UniFileStream methodsFor: 'open/close' stamp: 'jmv 6/15/2022 09:43:53'!
open: fileName forWrite: writeMode
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."
	"Changed to do a GC and retry before failing ar 3/21/98 17:25"
	fileID _ UniFileStream retryWithGC: [ self primOpen: fileName asUtf8Bytes writable: writeMode ]
					until: [ :id | id notNil ]
					forFileNamed: fileName.
	fileID ifNil: [^ nil].  "allows sender to detect failure"
	name _ fileName.
	"jmv: Register after setting name. Name is assumed to be defined for registered objects."
	self register.
	rwmode _ writeMode.
	buffer1 _ ByteArray new: 1.
	self useUtf8String.
	self enableReadBuffering.! !

!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 6/15/2022 09:46:47'!
useBytes
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String,  Utf8String, Utf32String,
	according to protocol."
	readAdaptor _ BytesReadAdaptor new stream: self.! !

!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 6/15/2022 09:47:04'!
useCharacters
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String,  Utf8String, Utf32String,
	according to protocol."
	readAdaptor _ ByteStringReadAdaptor new stream: self.! !

!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 6/15/2022 09:47:22'!
useUtf32String
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String,  Utf8String, Utf32String,
	according to protocol."
	readAdaptor _ Utf32ReadAdaptor new stream: self.! !

!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 6/15/2022 09:47:35'!
useUtf8String
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String,  Utf8String, Utf32String,
	according to protocol."
	readAdaptor _ Utf8ReadAdaptor new stream: self.! !


!Utf8String class reorganize!
('instance creation' empty fromUtf32Words: fromUtf8Bytes: fromUtf8Bytes:codePointCount: new:withAll: newFrom: with: with:with:)
('services' utf32FromUtf8: utf8FromUtf16: utf8FromUtf32:)
('system' symbolClassToUse)
('stream creation' streamContents: writeStream)
!


!ByteArray class reorganize!
('byte based hash' hashBytes:startingWith:)
('instance creation' newBits: newFrom: readHexFrom:)
('stream creation' streamUtf8Contents:)
!


!UniFileStream reorganize!
('open/close' close closed ensureOpen open:forWrite: openReadOnly reopen)
('properties-setting' isReadOnly readOnly readWrite)
('access' directory fileEntry fullName localName name peekFor: printOn: reset size)
('read, write, position' atEnd crc16 flush next next: next:into:startingAt: next:putAll:startingAt: nextBytes:into:startingAt: nextPut: nextPutAll: nextPutCodePoint: nextUtf8BytesAndCodePointInto:into: nextWordsInto: padToEndIfCantTruncate peek peekLast position position: readInto:startingAt:count: setToEnd skip skip: skipBack truncate truncate: upTo:delimiterIsTerminator: withAttribute:do: withAttributes:do:)
('testing' isFileStream)
('primitives' primAtEnd: primClose: primCloseNoError: primFlush: primGetPosition: primOpen:writable: primRead:into:startingAt:count: primSetPosition:to: primSize: primSizeNoError: primTruncate:to: primWrite:from:startingAt:count:)
('registry' register unregister)
('finalization' actAsExecutor finalize)
('private' bytesUpToEnd disableReadBuffering enableReadBuffering flushReadBuffer nextByte nextCodePoint nextPutByte: pastEndPut:)
('printing' longPrintOn: longPrintOn:limitedTo:indent: storeOn:)
('accessing' collectionSpecies contents contentsOfEntireFile upToEnd)
('fileIn/Out' fileIn)
('reading modes' useBytes useCharacters useUtf32String useUtf8String)
!

