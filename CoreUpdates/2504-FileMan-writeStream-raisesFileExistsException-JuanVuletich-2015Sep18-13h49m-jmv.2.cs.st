'From Cuis 4.2 of 25 July 2013 [latest update: #2502] on 18 September 2015 at 3:00:44.580986 pm'!

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/18/2015 14:58'!
forceWriteStream
	"If the file already exists, delete it first without asking. Do not raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #forceWriteStreamContents: that closes the file for you.
	Creates the directory if it doesn't exist."

	self refresh.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor forceNewFileNamed: self pathName! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/18/2015 14:58'!
forceWriteStreamContents: blockWithArg 
	"If the file already exists, delete it first without asking. Do not raise FileExistsException.
	Creates the directory if it doesn't exist."
	| stream |
	stream := self forceWriteStream.
	[blockWithArg value: stream]
		ensure: [stream
				ifNotNil: [:s | s close]]! !


!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/18/2015 14:52'!
tryWriteStream
	"If the file already exists, raise FileExistsException, to give the chance to pick another name or overwrite it."
	"Note: You need to eventually close the stream.
	Fail if the directory to create the file in doesn't exist."

	self refresh.
	^self fileAccessor newFileNamed: self pathName! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/18/2015 14:57'!
writeStream
	"If the file already exists raise FileExistsException.
	Note: You need to eventually close the stream.
	Usually prefer #writeStreamContents: that closes the file for you.
	Creates the directory if it doesn't exist."

	self refresh.
	self parent exists ifFalse: [self parent assureExistence].
	^self fileAccessor newFileNamed: self pathName
	"What follows is the original FileMan code by Masashi Umezawa (mu),
	and the most likely behavior in other implementations of FileMan.
	This behavior we call #forceWriteStream, to reduce annoyances to callers. (jmv)"
	"
	^self fileAccessor forceNewFileNamed: self pathName
	"! !

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/18/2015 14:58'!
writeStreamContents: blockWithArg 
	"If the file already exists raise FileExistsException.
	Creates the directory if it doesn't exist."
	| stream |
	stream := self writeStream.
	[blockWithArg value: stream]
		ensure: [stream
				ifNotNil: [:s | s close]]! !

