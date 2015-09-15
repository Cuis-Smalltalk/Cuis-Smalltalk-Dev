'From Cuis 4.2 of 25 July 2013 [latest update: #2478] on 12 September 2015 at 9:20:23.192101 am'!

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/12/2015 09:19'!
readStream
	"Raise #fileDoesNotExistException if not found."
	^self fileAccessor readOnlyFileNamed: self pathName
	
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

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 9/12/2015 09:19'!
tryReadStream
	"Raise #fileDoesNotExistException if not found."
	
	^ self fileAccessor readOnlyFileNamed: self pathName! !


!Form class methodsFor: 'instance creation' stamp: 'jmv 7/25/2015 17:48'!
fromFileNamed: fileName
	"Read a Form or ColorForm from the given file."

	| file form |
	file _ fileName asFileEntry readStream binary.
	form _ self fromBinaryStream: file.
	file close.
	^ form
! !

