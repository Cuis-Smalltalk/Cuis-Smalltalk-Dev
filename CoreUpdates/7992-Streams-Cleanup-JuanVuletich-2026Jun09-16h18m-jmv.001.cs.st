'From Cuis7.9 [latest update: #7989] on 9 June 2026 at 4:32:53 pm'!

!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 9/Jun/2026 14:56:22'!
useByteCharacters
	"Applies only to reading operations.
	For writing accepts bytes and Characters; or ByteArray, String and UnicodeString,
	according to protocol."
	readAdaptor := ByteStringReadAdaptor new stream: self.! !

!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 9/Jun/2026 14:56:36'!
useCodePoints
	"Applies only to reading operations.
	For writing accepts bytes and Characters; or ByteArray, String and UnicodeString,
	according to protocol."
	readAdaptor := UnicodeReadAdaptor new stream: self.! !

!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 9/Jun/2026 15:19:46'!
writeOnly
	"For writing only.
	Accept bytes, Characters, CodePoints or ByteArray, String, UnicodeString,
	according to protocol.
	Having no readAdaptor effectively makes any reading operation fail."
	readAdaptor := nil.! !


!Form methodsFor: 'fileIn/Out' stamp: 'jmv 9/Jun/2026 15:07:51'!
writeOnFileNamed: fileName
	"Saves the receiver on the file fileName in the format:
		fileCode, depth, extent, offset, bits."

	fileName asFileEntry forceWriteStreamDo: [ :file |
		file nextPut: 2.  "file code = 2"
		self writeOn: file ].

"
 | f |
[(f := Form fromUser) boundingBox area>25] whileTrue:
	[f writeOnFileNamed: 'test.form'.
	(Form fromFileNamed: 'test.form') display].
"! !


!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 9/Jun/2026 15:12:35'!
putForm: aForm onFile: fileEntry
	"Store the given form on a file of the given name."

	fileEntry forceWriteStreamDo: [ :stream |
		(self onBinaryStream: stream) nextPutImage: aForm ]! !


!JPEGReadWriter2 class methodsFor: 'image reading/writing' stamp: 'jmv 9/Jun/2026 15:17:53'!
putForm: aForm quality: quality progressiveJPEG: progressiveFlag onFileNamed: fileName
	"Store the given Form as a JPEG file of the given name, overwriting any existing file of that name. Quality goes from 0 (low) to 100 (high), where -1 means default. If progressiveFlag is true, encode as a progressive JPEG."

	fileName asFileEntry forceWriteStreamDo: [ :stream |
		(self onBinaryStream: stream)
			nextPutImage: aForm quality: quality progressiveJPEG: progressiveFlag ]! !


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 9/Jun/2026 15:02:05'!
logError: errMsg inContext: aContext to: baseFilename
	"Log the error message and a stack trace to the given file.
	Smalltalk logError: 'test error message' inContext: thisContext to: 'testErr'
	"

	| localFilename file |
	localFilename := (Preferences at: #debugLogTimestamp)
		ifTrue: [ baseFilename, '-',
				(String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ]),
				'.log' ]
		ifFalse: [ baseFilename, '.log' ].
	file := DirectoryEntry logsDirectory // localFilename.
	[
		file forceWriteStreamDo: [ :stream |
			stream nextPutAll: errMsg; newLine.
			aContext errorReportOn: stream ]
	] on: UnhandledError do: [ :ex | ex return]. "avoid recursive errors"
	[
 	 	StdIOWriteStream stdout newLine; nextPutAll: errMsg.
		StdIOWriteStream stdout newLine; nextPutAll: 'See '; nextPutAll: file pathName.
		StdIOWriteStream stdout newLine.
		aContext shortErrorReportOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout flush
	] on: UnhandledError do: [ :ex | ex return]. "avoid recursive errors"! !


!Stream methodsFor: 'accessing' library: #MinimalKernel01 stamp: 'jmv 9/Dec/2013 17:52:00'!
binary
	"Compatibility. If possible, retrieve bytes."
	self useBytes! !

!Stream methodsFor: 'nonhomogeneous read/write' library: #MinimalKernel01 stamp: 'jmv 9/Jun/2026 15:35:13'!
nextString
	"Read a string from the receiver. Only used by DataStream.
	The first byte is the length of the string, unless it is greater than 192, in which case the first four bytes encode the length."

	| bytes length |

	self binary.
	length := self next.		"first byte."
	length >= 192 ifTrue: [length := length - 192.
		1 to: 3 do: [:ii | length := length * 256 + self next]].
	bytes := ByteArray new: length.
	self nextInto: bytes.
	^bytes asString! !

!Stream methodsFor: 'nonhomogeneous read/write' stamp: 'jmv 9/Jun/2026 15:35:29'!
nextUnicodeString
	"Read a UnicodeString from the receiver.
	Only used by DataStream.
	Data is UTF-8 bytes.
	The first byte is the length of the data, unless it is greater than 192,
	in which case the first four bytes encode the length."

	| bytes length |

	"read the length in binary mode"
	self binary.
	length := self next.		"first byte."
	length >= 192 ifTrue: [length := length - 192.
		1 to: 3 do: [:ii | length := length * 256 + self next]].
	bytes := ByteArray new: length.
	self nextInto: bytes.
	^UnicodeString fromUtf8Bytes: bytes! !


!UniFileStream methodsFor: 'open/close' stamp: 'jmv 9/Jun/2026 14:43:00'!
open: fileName forWrite: writeMode
	"Open the file with the given name. If writeMode is true, allow writing, otherwise open the file in read-only mode."
	"Changed to do a GC and retry before failing ar 3/21/98 17:25"
	fileID := UniFileStream retryWithGC: [ self primOpen: fileName asUtf8Bytes writable: writeMode ]
					until: [ :id | id notNil ]
					forFileNamed: fileName.
	fileID ifNil: [^ nil].  "allows sender to detect failure"
	name := fileName.
	"jmv: Register after setting name. Name is assumed to be defined for registered objects."
	self register.
	rwmode := writeMode.
	buffer1 := ByteArray new: 1.
	self useCodePoints.
	self enableReadBuffering.! !

!UniFileStream methodsFor: 'read, write, position' stamp: 'jmv 9/Jun/2026 14:47:57'!
nextPut: aCharacterOrByte
	"Insert the argument at the next position in the Stream represented by the receiver.
	Argument can be Character or a Byte (SmallInteger).
	Write to this stream. Character and CodePoint are UTF-8 encoded."

	aCharacterOrByte isNumber
		ifTrue: [self nextPutByte: aCharacterOrByte]
		ifFalse: [self nextPutCodePoint: aCharacterOrByte codePoint].
	lastWrittenElement := aCharacterOrByte.! !

!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 9/Jun/2026 14:56:39'!
isBinary
	"Applies only to reading operations.
	For writing accepts bytes and Characters; or ByteArray, String and UnicodeString,
	according to protocol."
	^ readAdaptor class == BytesReadAdaptor! !

!UniFileStream methodsFor: 'reading modes' stamp: 'jmv 9/Jun/2026 14:56:32'!
useBytes
	"Applies only to reading operations.
	For writing accepts bytes and Characters; or ByteArray, String and UnicodeString,
	according to protocol."
	readAdaptor := BytesReadAdaptor new stream: self.! !


!Utf8EncodedWriteStream methodsFor: 'read, write, position' stamp: 'jmv 9/Jun/2026 14:47:43'!
nextPut: aCharacterOrByte
	"Insert the argument at the next position in the Stream represented by the receiver.
	Argument can be Character or a Byte (SmallInteger).
	Write to this stream. Character and CodePoint are UTF-8 encoded."

	aCharacterOrByte isNumber
		ifTrue: [self nextPutByte: aCharacterOrByte]
		ifFalse: [self nextPutCodePoint: aCharacterOrByte codePoint].
	lastWrittenElement := aCharacterOrByte.! !


!DataStream class methodsFor: 'examples' stamp: 'jmv 9/Jun/2026 15:27:06'!
exampleWithPictures
	"
	DataStream exampleWithPictures
	"
	| f result |
	f := Form fromUser.
	'Test-Picture' asFileEntry writeStreamDo: [ :file |
		(DataStream on: file) nextPut: f ].

	'Test-Picture' asFileEntry readStreamDo: [ :file |
		result := (DataStream on: file) next ].

	result display.! !

!DataStream class methodsFor: 'examples' stamp: 'jmv 9/Jun/2026 15:27:38'!
testWith: anObject
	"As a test of DataStream/ReferenceStream, write out anObject and read it back.
	11/19/92 jhm: Set the file type. More informative file name.
	DataStream testWith: 'hi'
	ReferenceStream testWith: 'hi'
	"
	| result |

	(self name, ' test') asFileEntry writeStreamDo: [ :file |
		(self on: file) nextPut: anObject ].

	(self name, ' test') asFileEntry readStreamDo: [ :file |
		result := (self on: file) next ].

	^ result! !


!SmartRefStream methodsFor: 'read write' stamp: 'jmv 9/Jun/2026 15:24:43'!
nextPut: anObject
	"Really write three objects: (version, class structure, object).  But only when called from the outside.  If any instance-specific classes are present, prepend their source code.  byteStream will be in fileOut format.
	You can see an analysis of which objects are written out by doing: 
	(SmartRefStream statsOfSubObjects: anObject)
	(SmartRefStream tallyOfSubObjects: anObject)
	(SmartRefStream subObjects: anObject ofClass: aClass)"

	| info |
	topCall
		ifNil: [
			topCall := anObject.
			'Please wait while objects are counted' 
				displayProgressAt: Sensor mousePoint
				from: 0 to: 10
				during: [ :barBlock | info := self instVarInfo: anObject].
			'Writing an object file' displayProgressAt: Sensor mousePoint
				from: 0 to: objCount*4	"estimate"
				during: [ :barBlock |
					objCount := 0.
					progressBar := barBlock.
					self setStream: byteStream reading: false.
						"set basePos, but keep any class renames"
					super nextPut: ReferenceStream versionCode.
					super nextPut: info.
					super nextPut: anObject.		"<- the real writing"
					].
						"Note: the terminator, $!!, is not doubled inside object data"
			"references is an IDict of every object that got written"
			byteStream nextPut: $!! codePoint; nextPut: 10; nextPut: 10.
			byteStream padToEndIfCantTruncate.
			topCall := progressBar := nil]	"reset it"
		ifNotNil: [
			super nextPut: anObject.
			progressBar ifNotNil: [progressBar value: (objCount := objCount + 1)]].! !


!FileIOAccessor methodsFor: 'actions' stamp: 'jmv 9/Jun/2026 15:05:45'!
copy: fromFileEntry to: toFileEntry
	| readStr writeStr |
	[
		readStr := (self privateReadOnlyFile: fromFileEntry) binary.
		writeStr := (self privateForceNewFile: toFileEntry) writeOnly.
		self copyFile: readStr toFile: writeStr]
			ensure: [
				readStr
					ifNotNil: [ :r | r close ].
				writeStr
					ifNotNil: [ :w | w close ]].
	toFileEntry parent invalidateChildren.
	toFileEntry invalidate.! !


!DirectoryEntry methodsFor: 'dictionary-like' stamp: 'jmv 9/Jun/2026 15:17:27'!
at: localFileName put: contents

	(self // localFileName) fileContents: contents.
	self invalidateChildren.
	^contents! !


!FileEntry methodsFor: 'actions-file' stamp: 'jmv 9/Jun/2026 15:26:24'!
appendContents: aStringOrBytes
	self appendStreamDo: [ :strm |
		strm nextPutAll: aStringOrBytes ].! !

!FileEntry methodsFor: 'actions-file' stamp: 'jmv 9/Jun/2026 15:16:23'!
fileContents: aStringOrBytes
	self forceWriteStreamDo: [ :stream |
		stream nextPutAll: aStringOrBytes ].! !

!methodRemoval: FileEntry #binaryContents: stamp: 'jmv 9/Jun/2026 16:25:00'!
FileEntry removeSelector: #binaryContents:!
!methodRemoval: FileEntry #textContents: stamp: 'jmv 9/Jun/2026 16:25:00'!
FileEntry removeSelector: #textContents:!
!methodRemoval: FileSystemEntry #setContentsOf:to: stamp: 'jmv 9/Jun/2026 16:25:49'!
FileSystemEntry removeSelector: #setContentsOf:to:!
!methodRemoval: UniFileStream #useCharacters stamp: 'jmv 9/Jun/2026 16:23:45'!
UniFileStream removeSelector: #useCharacters!
!methodRemoval: UniFileStream #useUnicodeString stamp: 'jmv 9/Jun/2026 16:24:30'!
UniFileStream removeSelector: #useUnicodeString!

!UniFileStream reorganize!
('open/close' close closed ensureOpen open:forWrite: openReadOnly reopen)
('properties-setting' isReadOnly readOnly readWrite)
('access' directory fileEntry fileID fullName localName name peekFor: printOn: reset size)
('read, write, position' atEnd crc16 flush next next: next:putAll:startingAt: nextBytes:into:startingAt: nextPut: nextPutAll: nextPutByte: nextPutCodePoint: nextUtf8BytesAndCodePointInto:into: nextWordsInto: padToEndIfCantTruncate peek peekLast position position: readInto:startingAt:count: resetToStart setToEnd skip skip: skipBack truncate truncate:)
('testing' isFileStream)
('primitives' primAtEnd: primClose: primCloseNoError: primFlush: primGetPosition: primOpen:writable: primRead:into:startingAt:count: primSetPosition:to: primSize: primSizeNoError: primTruncate:to: primWrite:from:startingAt:count:)
('registry' register unregister)
('finalization' finalize)
('private' bytesUpToEnd disableReadBuffering enableReadBuffering flushReadBuffer nextByte nextBytes: nextCodePoint pastEndPut:)
('printing' longPrintOn: longPrintOn:limitedTo:indent: storeOn:)
('accessing' collectionSpecies contents contentsOfEntireFile upToEnd)
('fileIn/Out' fileIn)
('reading modes' isBinary useByteCharacters useBytes useCodePoints writeOnly)
!


!Utf8EncodedWriteStream reorganize!
('read, write, position' next:putAll:startingAt: nextPut: nextPutAll: nextPutByte: nextPutCodePoint: pastEndPutByte: peekLast position: skip skip: skipBack)
('private' byteContents isCompatibleWithContents: on: pastEndPut:)
('accessing' collectionSpecies contents)
('positioning' reset resetToStart setToEnd)
!

