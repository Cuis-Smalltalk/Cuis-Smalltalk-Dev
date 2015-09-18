'From Cuis 4.2 of 25 July 2013 [latest update: #2501] on 17 September 2015 at 12:27:59.433862 pm'!

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/17/2015 12:18'!
readStream
	"Raise FileDoesNotExistException if not found.
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

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/17/2015 12:19'!
tryReadStream
	"Raise FileDoesNotExistException if not found."
	"Note: You need to eventually close the stream."

	^ self fileAccessor readOnlyFileNamed: self pathName! !

