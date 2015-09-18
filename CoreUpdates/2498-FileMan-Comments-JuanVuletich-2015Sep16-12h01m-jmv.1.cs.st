'From Cuis 4.2 of 25 July 2013 [latest update: #2496] on 16 September 2015 at 12:07:11.570398 pm'!

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/16/2015 12:06'!
appendStream
	"Note: You need to eventually close the stream.
	Usually prefer #appendStreamContents: that closes the file for you."

	self exists
		ifFalse: [^ self writeStream].
	^ (self fileAccessor fileNamed: self pathName) setToEnd! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/16/2015 12:06'!
readStream
	"Raise #fileDoesNotExistException if not found.
	Note: You need to eventually close the stream.
	Usually prefer #readStreamContents: that closes the file for you."

	^ self fileAccessor readOnlyFileNamed: self pathName
	
	"What follows is the original FileMan code by Masashi Umezawa (mu),
	and the most likely behavior in other implementations of FileMan.
	But, if it is not there, there's no point in creating it (jmv)"
	"
	^ [self fileAccessor readOnlyFileNamed: self pathName]
		on: FmFileIOAccessor fileDoesNotExistException
		do: [:ex |
			self assureExistence.
			ex retry]
	"! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/16/2015 12:06'!
readStreamIfError: errorHandlingBlock
	"Note: You need to eventually close the stream."

	^ [self fileAccessor readOnlyFileNamed: self pathName]
		on: FileDoesNotExistException
		do: [:ex | errorHandlingBlock value: ex]! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/16/2015 12:06'!
tryReadStream
	"Raise #fileDoesNotExistException if not found."
	"Note: You need to eventually close the stream."

	^ self fileAccessor readOnlyFileNamed: self pathName! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/16/2015 12:06'!
tryWriteStream
	"If the file already exists, give the chance to pick another name or overwrite it."
	"Note: You need to eventually close the stream."

	self refresh.
	^self fileAccessor newFileNamed: self pathName! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/16/2015 12:06'!
writeStream
	"If the file already exists, delete it first without asking.
	Note: You need to eventually close the stream.
	Usually prefer #writeStreamContents: that closes the file for you."

	self refresh.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor forceNewFileNamed: self pathName! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/16/2015 12:06'!
writeStreamConfirming
	"Note: You need to eventually close the stream."

	self refresh.
	^self fileAccessor newFileNamed: self pathName! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/16/2015 12:06'!
writeStreamIfError: errorHandlingBlock
	"Note: You need to eventually close the stream."

	self refresh.
	^[self fileAccessor newFileNamed: self pathName]
		on: FileExistsException
		do: [:ex | errorHandlingBlock value: ex]! !

