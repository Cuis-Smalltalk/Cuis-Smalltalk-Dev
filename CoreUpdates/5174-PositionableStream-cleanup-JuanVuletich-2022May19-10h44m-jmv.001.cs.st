'From Cuis 6.0 [latest update: #5173] on 19 May 2022 at 12:00:58 pm'!

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 5/19/2022 11:57:58'!
backChunk
	"Answer the contents of the receiver back to the previous terminator character.
	Doubled delimiter indicate an embedded delimiter character."
	
	| answer ch firstPosition delimiter prevCh count delimiterCount |
	delimiter _ $!!.
	"Go to end of chunk, not including delimiter."
	ch _ self back.
	ch = delimiter ifTrue: [ ch _ self back ].

	count _ 1.
	prevCh _ $$.
	"Go back until non-doubled delimiter, or beginning."
	[ch notNil and: [ ch ~= delimiter or: [self peekBack = delimiter or: [prevCh = delimiter]]]] whileTrue: [
		count _ count + 1.
		prevCh _ ch.
		ch _ self back. ].
	"Skip initial delimiter"
	ch = delimiter ifTrue: [
		self next.
		count _ count-1 ].

	"This is where chunk starts. Undouble doubled delimiters."
	firstPosition _ self position.
	answer _ String streamContents: [ :strm |
		prevCh _ $$.
		delimiterCount _ 0.
		count timesRepeat: [
			ch _ self next.
			delimiterCount \\ 2 = 0 ifTrue: [
				strm nextPut: ch ].
			delimiterCount _ ch = delimiter
				ifTrue: [ delimiterCount+1]
				ifFalse: [0]]].

	"Re position at start."
	self position: firstPosition.
	^ answer! !

!methodRemoval: PositionableStream #oldBack stamp: 'jmv 5/19/2022 12:00:24'!
PositionableStream removeSelector: #oldBack!
!methodRemoval: PositionableStream #oldPeekBack stamp: 'jmv 5/19/2022 12:00:18'!
PositionableStream removeSelector: #oldPeekBack!

!PositionableStream reorganize!
('accessing' back contents contentsOfEntireFile crLfNextLine last next: next:into: next:into:startingAt: next:putAll: next:putAll:startingAt: nextAvailable: nextDelimited: nextInto: nextInto:startingAt: nextKeyword nextLine nextWordsInto: originalContents peek peekBack peekFor: untilAnySatisfying: upTo: upTo:delimiterIsTerminator: upToAll: upToAny: upToEnd)
('testing' atEnd isBinary isCharacters isEmpty isFileStream isText notEmpty)
('positioning' backUpTo: match: padTo:put: padToNextLongPut: position position: reset resetContents setToEnd skip: skipTo:)
('fileIn/Out' backChunk checkForPreamble: compileNextChunk compileNextChunkHandlingExceptions compileNextChunkWhenDoesNotStartWithExclamationMark compileNextChunkWhenStartsWithExclamationMark copyMethodChunkFrom: evaluate:printingErrorWith: fileIn fileInAnnouncing: fileInInformingTo: nextChunk skipSeparators unCommand)
('private' collectionSpecies on: positionError setFrom:to:)
('filein/out' copyPreamble:from:at:)
('gui' untilEnd:displayingProgress:)
('que!!hincha!!!!pelot!!' teRompoTodo)
!

