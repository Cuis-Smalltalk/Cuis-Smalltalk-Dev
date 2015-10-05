'From Cuis 4.2 of 25 July 2013 [latest update: #2520] on 5 October 2015 at 12:24:40.318655 pm'!
'Description Please enter a description for this package'!
!provides: 'FileMan-StrictCompatibility' 1 5!
!requires: 'Tests' 1 33 nil!

!String methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:38'!
fileContents: aString by: encoderName
	self asFileEntry textContents: aString by: encoderName.
	^aString! !

!String methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:38'!
fileContentsBy: encoderName
	^self asFileEntry textContentsBy: encoderName! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:26'!
appendStream
	"Note: You need to eventually close the stream.
	Usually prefer #appendStreamContents: that closes the file for you."

	^ self privateAppendStream! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:09'!
appendStreamContents: blockWithArg 
	| stream |
	stream := self appendStream.
	[blockWithArg value: stream]
		ensure: [stream
				ifNotNil: [:s | s close]]! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:36'!
fileContentsBy: encoderName
	^self asFileEntry textContentsBy: encoderName.
! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:40'!
readStream

	^ [ self fileAccessor readOnlyFileNamed: self pathName ]
		on: FileDoesNotExistException
		do: [ :ex |
			self assureExistence.
			ex retry ]! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:11'!
readStreamContents: blockWithArg 
	| stream conts |
	stream := self readStream.
	[conts := blockWithArg value: stream]
		ensure: [stream
				ifNotNil: [:s | s close]].
	^ conts! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:12'!
readStreamIfError: errorHandlingBlock
	"Note: You need to eventually close the stream."

	^ [self fileAccessor readOnlyFileNamed: self pathName]
		on: FileDoesNotExistException
		do: [:ex | errorHandlingBlock value: ex]! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:14'!
tryReadStream
	"Raise FileDoesNotExistException if not found."
	"Note: You need to eventually close the stream."

	^ self fileAccessor readOnlyFileNamed: self pathName! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:14'!
tryWriteStream
	"If the file already exists, raise FileExistsException, to give the chance to pick another name or overwrite it."
	"Note: You need to eventually close the stream.
	Fail if the directory to create the file in doesn't exist."

	self refresh.
	^self fileAccessor newFileNamed: self pathName! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:30'!
writeStream
	"If the file already exists raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #writeStreamContents: that closes the file for you.
	Creates the directory if it doesn't exist."

	"hay que empezar por eliminar los senders..."

	^self fileAccessor forceNewFileNamed: self pathName! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:14'!
writeStreamConfirming
	"Note: You need to eventually close the stream."

	self refresh.
	^self fileAccessor newFileNamed: self pathName! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'MU 3/26/2014 12:41'!
writeStreamContents: blockWithArg 
	| stream |
	stream := self writeStream.
	[blockWithArg value: stream]
		ensure: [stream
				ifNotNil: [:s | s close]]! !

!FmFileEntry methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 9/27/2015 20:13'!
writeStreamIfError: errorHandlingBlock
	"Note: You need to eventually close the stream."

	self refresh.
	^[self fileAccessor newFileNamed: self pathName]
		on: FileExistsException
		do: [:ex | errorHandlingBlock value: ex]! !

!FmDirectoryEntry class methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/5/2015 12:24'!
default 
	"Compatibility with other implementations of FileMan.
	For Cuis specific code, it is advisable to call #currentDirectory, #smalltalkImageDirectory or #vmDirectory
	FmDirectoryEntry default
	"
	^self currentDirectory! !

!FmFileManTest methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/2/2015 17:09'!
testTryReadStream
	"FmFileManTest debug: #testTryReadStream" 
	| file1 readStream |
	file1 := self randomFileName asFileEntry.
	self deny: (file1 exists).

	self should: [readStream := file1 tryReadStream] raise: FileDoesNotExistException.
	self deny: (file1 exists).
	self assert: (readStream isNil).
	
	readStream := file1 readStreamIfError: [:ex | 'NotExist'].
	
	self assert: (readStream = 'NotExist').
	self deny: (file1 exists).

	file1 assureExistence.
	self assert: (file1 exists).
	
	readStream := file1 readStreamIfError: [:ex | 'NotExist'].
	self deny: (readStream = 'NotExist').
	
	self assert: (readStream contents isEmpty).
	readStream close.
	
	file1 delete.
	self deny: (file1 exists).! !

!FmFileManTest methodsFor: '*FileMan-StrictCompatibility' stamp: 'jmv 10/2/2015 17:09'!
testTryWriteStream
	"FmFileManTest debug: #testTryWriteStream" 
	| file1 writeStream |
	file1 := self randomFileName asFileEntry assureExistence.
	self assert: (file1 exists).

	self should: [writeStream := file1 tryWriteStream] raise: FileExistsException.
	self assert: (file1 exists).
	self assert: (writeStream isNil).
	
	writeStream := file1 writeStreamIfError: [:ex | 'Exists'].
	
	self assert: (writeStream = 'Exists').
	self assert: (file1 exists).
	
	file1 delete.
	self deny: (file1 exists).
	
	writeStream := file1 writeStreamIfError: [:ex | 'Exists'].
	self deny: (writeStream = 'Exists').
	writeStream nextPutAll: 'Test'; close.
	
	self assert: (file1 textContents = 'Test').
	
	file1 delete.
	self deny: (file1 exists).! !
