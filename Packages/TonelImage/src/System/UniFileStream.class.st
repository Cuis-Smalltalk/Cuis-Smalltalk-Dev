"
A new FileStream for Cuis.

Writes bytes. Binary content is stored verbatim. String is stored in UTF-8, but invalid UTF-8 sequences are interpreted as Latin-1 Characters (i.e. ISO-8859-1).
"
Class {
	#name : 'UniFileStream',
	#superclass : 'ReadWriteStream',
	#instVars : [
		'rwmode',
		'name',
		'fileID',
		'buffer1',
		'readAdaptor',
		'lastWrittenElement'
	],
	#classVars : [
		'Registry'
	],
	#category : 'System-Files',
	#package : 'System',
	#tag : 'Files'
}

{ #category : 'file creation' }
UniFileStream class >> crc16OfFileNamed: fileName [
	"
	UniFileStream crc16OfFileNamed: 'cursor.jpeg'
	UniFileStream crc16OfFileNamed: 'deafultPID.txt'
	"

	^[fileName asFileEntry readStreamDo: [ :stream |
		stream crc16 ]]
			on: FileDoesNotExistException do: nil
]

{ #category : 'error handling' }
UniFileStream class >> fileDoesNotExistUserHandling: fullFileName [

	| selection newName |
	selection := (PopUpMenu labels:
'create a new file
choose another name
cancel')
			startUpWithCaption: fullFileName asFullFileEntry name, '
does not exist.'.
	selection = 1 ifTrue: [
		^ self new open: fullFileName forWrite: true].
	selection = 2 ifTrue: [
		newName := self request: 'Enter a new file name'
						initialAnswer:  fullFileName.
		^ FileIOAccessor default privateWriteableFile: newName asFileEntry ].
	^ self error: 'Could not open a file'
]

{ #category : 'error handling' }
UniFileStream class >> fileExistsUserHandling: fullFileName [
	| dir localName choice newName entry |
	entry := fullFileName asFullFileEntry.
	dir := entry parent.
	localName := entry name.
	choice := (PopUpMenu
		labels:
'overwrite that file\choose another name\cancel' withNewLines)
		startUpWithCaption: localName, '
already exists.'.

	"overwrite that file"
	choice = 1 ifTrue: [
		dir removeKey: localName
			ifAbsent: [self error: 'Could not delete the old version of that file'].
		^ self new open: fullFileName forWrite: true].

	"choose another name"
	choice = 2 ifTrue: [
		newName := self request: 'Enter a new file name' initialAnswer: fullFileName.
		^ FileIOAccessor default privateNewFile: newName asFileEntry ].

	"choice = 3: cancel"
	"Answer nil. Let callers decide whether to continue or not"
	^nil.
]

{ #category : 'instance creation' }
UniFileStream class >> new [
	^ self basicNew initialize
]

{ #category : 'error handling' }
UniFileStream class >> readOnlyFileDoesNotExistUserHandling: fullFileName [

	| dir files choices selection newName fileName |
	dir := fullFileName asFullFileEntry parent.
	files := dir fileNames.
	fileName := fullFileName asFullFileEntry name.
	choices := fileName correctAgainst: files.
	choices add: 'Choose another name'.
	choices add: 'Cancel'.
	selection := (PopUpMenu labelArray: choices lines: (Array with: 5) )
		startUpWithCaption: fullFileName asFullFileEntry name, '
does not exist.'.
	selection = 0 ifTrue: ["esc, click outside" ^ nil ].
	selection = choices size ifTrue: ["cancel" ^ nil ].
	selection < (choices size - 1) ifTrue: [
		newName := (dir pathName , '/', (choices at: selection))].
	selection = (choices size - 1) ifTrue: [
		newName := self request: 'Enter a new file name' initialAnswer: fileName.
		"If Cancel was pressed, no file should be opened - Hernan"
		newName isEmpty ifTrue: [ ^nil ]].
	newName = '' ifFalse: [^ FileIOAccessor default privateReadOnlyFile: newName asFileEntry ].
	^ self error: 'Could not open a file'
]

{ #category : 'registry' }
UniFileStream class >> recreateRegistry [
	"
	UniFileStream recreateRegistry
	"
	| newOne |
	newOne := FinalizationRegistry new.
	Registry do: [ :fileStream |
		newOne add: fileStream ].
	Registry := newOne.
]

{ #category : 'registry' }
UniFileStream class >> register: anObject [
	
	^self registry add: anObject
]

{ #category : 'registry' }
UniFileStream class >> registry [
	
	^Registry ifNil: [ Registry := FinalizationRegistry new ]
]

{ #category : 'cached state access' }
UniFileStream class >> releaseClassCachedState [

	Registry ifNotNil: [
		"Make the best effort to empty the registry.
		Only keep files that are still open have references from some object."
		Registry isEmpty ifFalse: [
			Smalltalk garbageCollectMost.
			Registry isEmpty ifFalse: [
				Smalltalk garbageCollect]].

		"No open files. Can nil the Registry."
		Registry isEmpty ifTrue: [
			Registry := nil ]
	]
]

{ #category : 'registry' }
UniFileStream class >> retryWithGC: execBlock until: testBlock forFileNamed: fullName [
	"Re-implemented to only force GC if a file with the given name exists"
	| blockValue foundIt |
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	"See if we have a file with the given name"
	foundIt := self registry keys "hold on strongly for now" 
		anySatisfy:[:file| file name sameAs: fullName].
	foundIt ifFalse:[^blockValue].
	Smalltalk garbageCollectMost.
	blockValue := execBlock value.
	(testBlock value: blockValue) ifTrue:[^blockValue].
	Smalltalk garbageCollect.
	^execBlock value.
]

{ #category : 'registry' }
UniFileStream class >> unregister: anObject [
	
	^self registry remove: anObject ifAbsent: nil
]

{ #category : 'read, write, position' }
UniFileStream >> atEnd [
	"Answer whether the receiver is at its end."
	collection ifNotNil: [
		position < readLimit ifTrue: [ 
			^false ] ].

	^ (self primAtEnd: fileID) and: [ (self primGetPosition: fileID) = (self primSize: fileID) ]
]

{ #category : 'private' }
UniFileStream >> bytesUpToEnd [
	"Leave stream at the end position."

	^self nextBytes: self size - self position
]

{ #category : 'open/close' }
UniFileStream >> close [
	"Close this file."

	fileID ifNotNil: [
		collection ifNotNil: [
			readLimit := position := 0 ].
		self primClose: fileID.
		self unregister.
		fileID := nil].

]

{ #category : 'open/close' }
UniFileStream >> closed [
	"Answer true if this file is closed."

	^ fileID == nil or: [ (self primSizeNoError: fileID) == nil ]

]

{ #category : 'accessing' }
UniFileStream >> collectionSpecies [
	^readAdaptor collectionSpecies
]

{ #category : 'accessing' }
UniFileStream >> contents [
	"Return the full contents of the receiver. Do not close or otherwise touch the receiver.
	Return data in whatever mode the receiver is in (i.e. answer an instance of the appropriate class).
	Stream bytes as CodePoints (prefer UTF-8, but ISO-8859-1 if invalid UTF-8)."

	| savePos answer |
	savePos := self position.
	self position: 0.
	answer := self upToEnd.
	self position: savePos.
	^answer
]

{ #category : 'accessing' }
UniFileStream >> contentsOfEntireFile [
	"Read all of the contents of the receiver."

	| answer |
	self readOnly.
	self reset.
	answer := self contents.
	self close.
	^answer
]

{ #category : 'read, write, position' }
UniFileStream >> crc16 [
	"Copied from String>>crc16"
	
	| buffer crc |
	self reset.
	buffer := ByteArray new: 2000.
	crc := 0.
	[ self atEnd ] whileFalse: [
		buffer := self nextInto: buffer.
		buffer do: [:c |
			crc := (crc bitShift: -8) bitXor: (
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
	^crc
]

{ #category : 'access' }
UniFileStream >> directory [
	"Return the directory containing this file."

	^ self fileEntry parent
]

{ #category : 'private' }
UniFileStream >> disableReadBuffering [

	collection ifNotNil: [
		position < readLimit
			ifTrue: [
				| currentPosition |
				currentPosition := self position.
				collection := readLimit := position := nil.
				self position: currentPosition ]
			ifFalse: [
				collection := readLimit := position := nil ] ]
		
]

{ #category : 'private' }
UniFileStream >> enableReadBuffering [

	collection ifNil: [
		buffer1 ifNotNil: [
			collection := ByteArray new: 2048 ] ].
	readLimit := position := 0
]

{ #category : 'open/close' }
UniFileStream >> ensureOpen [
	"Make sure that this file really is open."

	self closed ifTrue: [^ self reopen].
	(self primSizeNoError: fileID) ifNotNil: [^ self].
	self reopen.

]

{ #category : 'access' }
UniFileStream >> fileEntry [
	^ self fullName asFullFileEntry
]

{ #category : 'access' }
UniFileStream >> fileID [

	^fileID
]

{ #category : 'fileIn/Out' }
UniFileStream >> fileIn [
	"Guarantee that the receiver is readOnly before fileIn for efficiency and
	to eliminate remote sharing conflicts."

	self readOnly.
	self fileInAnnouncing: 'Loading ', self localName.
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ].

]

{ #category : 'finalization' }
UniFileStream >> finalize [
	self primCloseNoError: fileID.
]

{ #category : 'read, write, position' }
UniFileStream >> flush [
	"Flush pending changes"
	^self primFlush: fileID
]

{ #category : 'private' }
UniFileStream >> flushReadBuffer [

	collection ifNotNil: [
		position < readLimit ifTrue: [
			| currentPosition |
			currentPosition := self position.
			position := readLimit := 0.
			self primSetPosition: fileID to: currentPosition ] ]
]

{ #category : 'access' }
UniFileStream >> fullName [
	"Answer this file's full path name."

	^ name

]

{ #category : 'reading modes' }
UniFileStream >> isBinary [
	^ readAdaptor class == BytesReadAdaptor
]

{ #category : 'testing' }
UniFileStream >> isFileStream [
	^true
]

{ #category : 'properties-setting' }
UniFileStream >> isReadOnly [

	^ rwmode not

]

{ #category : 'access' }
UniFileStream >> localName [
	^ self fileEntry name
]

{ #category : 'printing' }
UniFileStream >> longPrintOn: aStream [
	"Do nothing, so it will print short.  Called to print the error file.  If the error was in a file operation, we can't read the contents of that file.  Just print its name instead."

]

{ #category : 'printing' }
UniFileStream >> longPrintOn: aStream limitedTo: sizeLimit indent: indent [

	"Do nothing, so it will print short.  Called to print the error file.  If the error was in a file operation, we can't read the contents of that file.  Just print its name instead."

	aStream newLine
]

{ #category : 'access' }
UniFileStream >> name [
	"Answer this file's full path name."

	^ name

]

{ #category : 'read, write, position' }
UniFileStream >> next [
	"Answer the next entry from this file, or nil if at the end of the file.
	Answer will be
	- CodePoint (for #useUnicodeString)
	- Character (for #useCharacters)
	- SmallInteger (for #useBytes)"

	^readAdaptor next
]

{ #category : 'read, write, position' }
UniFileStream >> next: n [
	"Answer the next n charaters (or bytes) from this file, or nil if at the end of the file.
	Answer will be
	- an UnicodeString or String or size n (or until the end of the file if shorter)
	- a ByteArray of size n (or until the end of the file if shorter)"

	^readAdaptor next: n
]

{ #category : 'read, write, position' }
UniFileStream >> next: anInteger putAll: aStringOrBytes startingAt: startIndex [
	"Argument can be String, UnicodeString or ByteArray.
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
	lastWrittenElement := aStringOrBytes at: startIndex+anInteger-1.
]

{ #category : 'private' }
UniFileStream >> nextByte [
	"Answer the next byte from this file, or nil if at the end of the file."
	
	| count |
	collection ifNotNil: [
		position < readLimit 
			ifFalse: [ 
				readLimit := self primRead: fileID into: collection startingAt: 1 count: collection size.
				position := 0.
				readLimit = 0 ifTrue: [ ^nil ] ].
		^collection at: (position := position + 1) ].	
	count := self primRead: fileID into: buffer1 startingAt: 1 count: 1.
	^count = 1
		ifTrue: [ buffer1 at: 1 ]
]

{ #category : 'private' }
UniFileStream >> nextBytes: n [
	"Read n bytes, or until the end of the file."

	| bytes |
	bytes := ByteArray new: n.
	^ self nextBytes: n into: bytes startingAt: 1.
]

{ #category : 'read, write, position' }
UniFileStream >> nextBytes: n into: aBytesOrWordsObject startingAt: startIndex [
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
		ifFalse:[ ^aBytesOrWordsObject copyFrom: 1 to: newStartIndex + count - 1 ]
]

{ #category : 'private' }
UniFileStream >> nextCodePoint [
	| answer |
	answer := nil.
	self nextUtf8BytesAndCodePointInto: nil into: [ :codePoint |
		answer := Character codePoint: codePoint ].

	"If we'd skipped BOM, try again. See comment at
	utf8BytesAndCodePointFor:byte2:byte3:byte4:into:into:
	Feel free to blame Microsoft for this nonsense."
	answer ifNil: [
		(self position = 3 and: [self atEnd not]) ifTrue: [
			answer := self nextCodePoint ]].

	^answer
]

{ #category : 'read, write, position' }
UniFileStream >> nextPut: aCodePointOrCharacterOrByte [
	"Insert the argument at the next position in the Stream represented by the receiver.
	Argument can be Character, CodePoint or Byte (SmallInteger).
	Write to this stream. Character and CodePoint are UTF-8 encoded."

	aCodePointOrCharacterOrByte isNumber
		ifTrue: [self nextPutByte: aCodePointOrCharacterOrByte]
		ifFalse: [self nextPutCodePoint: aCodePointOrCharacterOrByte codePoint].
	lastWrittenElement := aCodePointOrCharacterOrByte.
]

{ #category : 'read, write, position' }
UniFileStream >> nextPutAll: aStringOrBytes [
	"Argument can be String, UnicodeString or ByteArray.
	Write to this file. Both kinds of Strings are UTF-8 encoded."

	| bytes |
	aStringOrBytes isEmpty ifTrue: [ ^self ].
	bytes := aStringOrBytes.
	(aStringOrBytes is: #ByteArray) ifFalse: [
		bytes := aStringOrBytes asUtf8Bytes].
	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	self primWrite: fileID from: bytes startingAt: 1 count: bytes basicSize.
	lastWrittenElement := aStringOrBytes last.
]

{ #category : 'private' }
UniFileStream >> nextPutByte: aByte [
	"Write the given byte to this file."

	rwmode ifFalse: [^ self error: 'Cannot write a read-only file'].
	collection ifNotNil: [ 
		position < readLimit ifTrue: [ self flushReadBuffer ] ].
	buffer1 at: 1 put: aByte.
	self primWrite: fileID from: buffer1 startingAt: 1 count: 1.
]

{ #category : 'read, write, position' }
UniFileStream >> nextPutCodePoint: aNumber [
	"UTF-8 encoded"

	Character
		evaluate: [ :byte | self nextPutByte: byte ]
		withUtf8BytesOf: aNumber
]

{ #category : 'read, write, position' }
UniFileStream >> nextUtf8BytesAndCodePointInto: bytesBlock into: codePointBlock [
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
	self position: previousPosition + bytesConsumed.
]

{ #category : 'read, write, position' }
UniFileStream >> nextWordsInto: aBitmap [
	"Note: The file primitives automatically adjust for word based objects."

	self nextBytes: aBitmap basicSize into: aBitmap startingAt: 1.
	aBitmap restoreEndianness.
	^ aBitmap
]

{ #category : 'open/close' }
UniFileStream >> open: fileName forWrite: writeMode [
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
	self useUnicodeString.
	self enableReadBuffering.
]

{ #category : 'open/close' }
UniFileStream >> openReadOnly [
	"Open the receiver as a read-only file.  1/31/96 sw"

	^ self open: name forWrite: false
]

{ #category : 'read, write, position' }
UniFileStream >> padToEndIfCantTruncate [
	"Only makes sense for file streams with existing content.
	On file systems that don't support truncating this is needed.
	If truncating is supported, try that first"

	"On the Mac, files do not truncate.  One can delete the old file and write a new one, but sometime deletion fails (file still open? file stale?).  This is a sad compromise.  Just let the file be the same length but pad it with a harmless character."

	| pad |
	self atEnd ifTrue: [^ self].
	self truncate.
	self atEnd ifTrue: [^ self].
	pad := Character space codePoint.
	self nextPutAll: (ByteArray new: ((self size - self position) min: 20000) 
							withAll: pad)
]

{ #category : 'private' }
UniFileStream >> pastEndPut: anObject [
	self shouldNotImplement
]

{ #category : 'read, write, position' }
UniFileStream >> peek [
	"Answer what would be returned if the message next were sent to the receiver.
	If the receiver is at the end, answer nil.  "

	| next currentPosition |
	self atEnd ifTrue: [^ nil].
	currentPosition := self position.
	next := self next.
	self position: currentPosition.
	^ next
]

{ #category : 'access' }
UniFileStream >> peekFor: aCharacter  [
	"Answer false and do not move over the next element if it is not equal to the argument, aCharacter 
	Answer true and increment the position for accessing elements, if the next element is equal to anObject."

	| nextChar |
	nextChar := self peek.
	aCharacter = nextChar ifTrue: [
		self next.
		^ true].
	^ false
]

{ #category : 'read, write, position' }
UniFileStream >> peekLast [

	^lastWrittenElement
]

{ #category : 'read, write, position' }
UniFileStream >> position [
	"Return the receiver's current file position."

	collection ifNotNil: [
		position < readLimit ifTrue: [
			^(self primGetPosition: fileID) - readLimit + position ] ].
	^self primGetPosition: fileID
]

{ #category : 'read, write, position' }
UniFileStream >> position: pos [
	"Set the receiver's position as indicated."

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
	self primSetPosition: fileID to: pos.
	lastWrittenElement := nil.
]

{ #category : 'primitives' }
UniFileStream >> primAtEnd: id [
	"Answer true if the file position is at the end of the file."

	<primitive: 'primitiveFileAtEnd' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : 'primitives' }
UniFileStream >> primClose: id [
	"Close this file."

	<primitive: 'primitiveFileClose' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : 'primitives' }
UniFileStream >> primCloseNoError: id [
	"Close this file. Don't raise an error if the primitive fails."

	<primitive: 'primitiveFileClose' module: 'FilePlugin'>

]

{ #category : 'primitives' }
UniFileStream >> primFlush: id [
	"Flush pending changes to the disk"
	| p |
	<primitive: 'primitiveFileFlush' module: 'FilePlugin'>
	"In some OS's seeking to 0 and back will do a flush"
	p := self position.
	self position: 0; position: p
]

{ #category : 'primitives' }
UniFileStream >> primGetPosition: id [
	"Get this files current position."

	<primitive: 'primitiveFileGetPosition' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : 'primitives' }
UniFileStream >> primOpen: fileNameAsUtf8Bytes writable: writableFlag [
	"Open a file of the given name, and return the file ID obtained.
	If writableFlag is true, then
		if there is none with this name, then create one
		else prepare to overwrite the existing from the beginning
	otherwise
		if the file exists, open it read-only
		else return nil"

	<primitive: 'primitiveFileOpen' module: 'FilePlugin'>
	^ nil

]

{ #category : 'primitives' }
UniFileStream >> primRead: id into: byteArray startingAt: startIndex count: count [
	"Read up to count bytes of data from this file into the given string or byte array starting at the given index. Answer the number of bytes actually read."

	<primitive: 'primitiveFileRead' module: 'FilePlugin'>
	self closed ifTrue: [^ self error: 'File is closed'].
	self error: 'File read failed'.

]

{ #category : 'primitives' }
UniFileStream >> primSetPosition: id to: anInteger [
	"Set this file to the given position."

	<primitive: 'primitiveFileSetPosition' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : 'primitives' }
UniFileStream >> primSize: id [
	"Answer the size of this file."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : 'primitives' }
UniFileStream >> primSizeNoError: id [
	"Answer the size of this file. Answer nil if the primitive fails; this indicates that the file handle has become stale."

	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	^ nil

]

{ #category : 'primitives' }
UniFileStream >> primTruncate: id to: anInteger [
	"Truncate this file to the given position."

	<primitive: 'primitiveFileTruncate' module: 'FilePlugin'>
	self primitiveFailed

]

{ #category : 'primitives' }
UniFileStream >> primWrite: id from: stringOrByteArray startingAt: startIndex count: count [
	"Write count bytes onto this file from the given string or byte array starting at the given index. Answer the number of bytes written."

	<primitive: 'primitiveFileWrite' module: 'FilePlugin'>
	(FileWriteError fileName: name)
		signal: (self closed
			ifTrue: [ 'File [', name, '] is closed' ]
			ifFalse: [ 'File [', name, '] write failed' ])
]

{ #category : 'access' }
UniFileStream >> printOn: aStream [
	"Put a printed version of the receiver onto aStream.  1/31/96 sw"

	aStream nextPutAll: self class name; nextPutAll: ': '; print: name
]

{ #category : 'read, write, position' }
UniFileStream >> readInto: byteArray startingAt: startIndex count: count [
	"Read into the given array as specified, and return the count
	actually transferred.  index and count are in units of bytes or
	longs depending on whether the array is Bitmap, String or ByteArray"
	
	^(self nextBytes: count into: byteArray startingAt: startIndex) size - startIndex + 1

]

{ #category : 'properties-setting' }
UniFileStream >> readOnly [
	"Make this file read-only."

	rwmode := false.

]

{ #category : 'properties-setting' }
UniFileStream >> readWrite [
	"Make this file writable."

	rwmode := true.

]

{ #category : 'registry' }
UniFileStream >> register [
	^self class register: self
]

{ #category : 'open/close' }
UniFileStream >> reopen [
	"Close and reopen this file. The file position is reset to zero."
	"Details: Files that were open when a snapshot occurs are no longer valid when the snapshot is resumed. This operation re-opens the file if that has happened."

	fileID ifNotNil: [
		collection ifNotNil: [
			position < readLimit ifTrue: [
				self flushReadBuffer ] ].
		self primCloseNoError: fileID ].
	self open: name forWrite: rwmode.
]

{ #category : 'access' }
UniFileStream >> reset [
	self ensureOpen.
	self position: 0.
]

{ #category : 'read, write, position' }
UniFileStream >> resetToStart [
	super resetToStart.
	lastWrittenElement := nil.
]

{ #category : 'read, write, position' }
UniFileStream >> setToEnd [
	"Set the position of the receiver to the end of file.  1/31/96 sw"

	self position: self size
]

{ #category : 'access' }
UniFileStream >> size [
	"Answer the size of the file in bytes."

	^ self primSize: fileID
]

{ #category : 'read, write, position' }
UniFileStream >> skip [
	"Skips one object in the receiver's future sequence values."

	self next
]

{ #category : 'read, write, position' }
UniFileStream >> skip: n [
	n < 0 ifTrue: [
		^n negated timesRepeat: [ self skipBack ]].
	n timesRepeat: [ self skip ].
]

{ #category : 'read, write, position' }
UniFileStream >> skipBack [
	| savePos try |
	savePos := self position.

	"Try going back one byte. If ASCII, then OK, we are done.
	If not, don't assume ISO-8859-1 so lightly!"
	try := savePos-1.
	self position: try.
	self nextByte < 128 ifTrue: [
		^self position: try ].

	"Try going back two bytes. If doing #next leaves us at savePosition, we found a two byte CodePoint."
	try := try-1.
	self position: try.
	(self next notNil and: [ self position = savePos ]) ifTrue: [
		^self position: try ].

	"Try going back three bytes. If doing #next leaves us at savePosition, we found a three byte CodePoint."
	try := try-1.
	self position: try.
	(self next notNil and: [ self position = savePos ]) ifTrue: [
		^self position: try ].

	"Try going back four bytes. If doing #next leaves us at savePosition, we found a four byte CodePoint."
	try := try-1.
	self position: try.
	(self next notNil and: [ self position = savePos ]) ifTrue: [
		^self position: try ].

	"Havent found a valid UTF-8 sequence that ends just before savePos.
	It means back byte is an ISO-8859-1 Character"
	self position: savePos-1.
	
]

{ #category : 'printing' }
UniFileStream >> storeOn: aStream [
	"A file can't be serialized"

	^ self printOn: aStream
]

{ #category : 'read, write, position' }
UniFileStream >> truncate [
	"Truncate to zero"

	^ self truncate: 0
]

{ #category : 'read, write, position' }
UniFileStream >> truncate: bytePosition [
	"Truncate to this position"

	self position: bytePosition.
	^self primTruncate: fileID to: bytePosition.
]

{ #category : 'registry' }
UniFileStream >> unregister [
	^self class unregister: self
]

{ #category : 'accessing' }
UniFileStream >> upToEnd [
	"Return the contents of the receiver. Do not close or otherwise touch the receiver.
	Return data in whatever mode the receiver is in (i.e. answer an instance of the appropriate class)."

	^readAdaptor streamUpToEnd
]

{ #category : 'reading modes' }
UniFileStream >> useBytes [
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String, UnicodeString,
	according to protocol."
	readAdaptor := BytesReadAdaptor new stream: self.
]

{ #category : 'reading modes' }
UniFileStream >> useCharacters [
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String, UnicodeString,
	according to protocol."
	readAdaptor := ByteStringReadAdaptor new stream: self.
]

{ #category : 'reading modes' }
UniFileStream >> useUnicodeString [
	"Only for reading.
	For writing accept bytes, Characters, CodePoints or ByteArray, String, UnicodeString,
	according to protocol."
	readAdaptor := UnicodeReadAdaptor new stream: self.
]
