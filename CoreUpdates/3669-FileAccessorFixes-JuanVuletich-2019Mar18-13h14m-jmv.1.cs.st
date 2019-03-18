'From Cuis 5.0 of 7 November 2016 [latest update: #3668] on 18 March 2019 at 1:14:24 pm'!

!WriteStream methodsFor: 'character writing' stamp: 'jmv 3/18/2019 12:14:37'!
cr
	"Append a cr character to the receiver.
	Use this method when you specifically need a cr character.
	In many cases, it is advisable to call #newLine"

	self nextPut: Character cr! !


!Transcript class methodsFor: 'old Transcript compatibility' stamp: 'jmv 3/18/2019 12:12:55'!
cr
	"Old Squeak Transcript protocol. Still used for some doIt examples.
	In the older TranscriptStream, it added a CR character.
	Now, finish the current incomplete entry."

	self finishEntry! !


!FileIOAccessor methodsFor: 'utilities' stamp: 'jmv 3/18/2019 12:52:32'!
fileSize: fileEntry

	| pathName f size |
	pathName _ fileEntry pathName.
	"At least on Linux 64 Cog, opening a directory as a stream and asking #size answers some absurd number: 9223372036854775807"
	(self basicDirectoryExists: pathName) ifTrue: [^ nil ].
	f _ self concreteStreamClass new open: pathName forWrite: false.
	f ifNil: [^ nil].
	size _ f size.
	f close.
	^ size! !

!FileIOAccessor methodsFor: 'file stream creation' stamp: 'jmv 3/18/2019 13:00:59'!
privateReadOnlyFile: fileEntry
	"Open the existing file with the given name in this directory for read-only access."

	| pathName |
	pathName _ fileEntry pathName.
	(self basicDirectoryExists: pathName) ifTrue: [
		"If it is a directory, the it is not a file, and the requested file does not exist."
		^ ((FileDoesNotExistException fileName: pathName) readOnly: true) signal ].
	^(self concreteStreamClass new open: pathName forWrite: false)
		ifNil: [
			"File does not exist..."
			((FileDoesNotExistException fileName: pathName) readOnly: true) signal ]! !

