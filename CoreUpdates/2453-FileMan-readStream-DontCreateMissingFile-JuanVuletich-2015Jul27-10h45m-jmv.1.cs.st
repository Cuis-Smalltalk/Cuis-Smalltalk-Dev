'From Cuis 4.2 of 25 July 2013 [latest update: #2431] on 27 July 2015 at 10:46:46.489526 am'!

!FmFileEntry methodsFor: 'accessing-stream' stamp: 'jmv 7/25/2015 17:55'!
readStream
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

