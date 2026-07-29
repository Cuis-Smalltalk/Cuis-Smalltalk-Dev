'From Cuis7.9 [latest update: #8108] on 29 July 2026 at 4:38:33 pm'!

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 29/Jul/2026, 7:38:11 pm (UTC)'!
fileIn
	"FilIn content in Chunk Format.
	Guarantee that the receiver is readOnly before fileIn for efficiency and
	to eliminate remote sharing conflicts."

	self readOnly.
	self fileInAnnouncing: 'Loading ', self localName.
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ].
! !

!methodRemoval: UniFileStream #fileIn stamp: 'jmv 29/Jul/2026, 7:38:16 pm (UTC)'!
UniFileStream removeSelector: #fileIn!

!methodRemoval: PositionableStream #compileNextChunkWhenStartsWithExclamationMark stamp: 'jmv 29/Jul/2026, 7:50:52 pm (UTC)'!
PositionableStream removeSelector: #compileNextChunkWhenStartsWithExclamationMark!
!methodRemoval: PositionableStream #compileNextChunkHandlingExceptions stamp: 'jmv 29/Jul/2026, 7:50:52 pm (UTC)'!
PositionableStream removeSelector: #compileNextChunkHandlingExceptions!
!methodRemoval: PositionableStream #compileNextChunk stamp: 'jmv 29/Jul/2026, 7:50:52 pm (UTC)'!
PositionableStream removeSelector: #compileNextChunk!
!methodRemoval: PositionableStream #evaluate:printingErrorWith: stamp: 'jmv 29/Jul/2026, 7:50:52 pm (UTC)'!
PositionableStream removeSelector: #evaluate:printingErrorWith:!
!methodRemoval: PositionableStream #compileNextChunkWhenDoesNotStartWithExclamationMark stamp: 'jmv 29/Jul/2026, 7:50:52 pm (UTC)'!
PositionableStream removeSelector: #compileNextChunkWhenDoesNotStartWithExclamationMark!
