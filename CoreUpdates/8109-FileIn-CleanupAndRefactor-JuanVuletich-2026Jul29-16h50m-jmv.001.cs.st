'From Cuis7.9 [latest update: #8108] on 29 July 2026 at 4:53:06 pm'!

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 29/Jul/2026, 7:51:34 pm (UTC)'!
createReaderAndFileIn: chunk

	"These are the ones that should do nothing, 
	because next line is a doit that does the stuff
	(or because it is handled elsewhere)"

	(chunk isEmpty or: [(chunk beginsWith: 'description:  ') 
		or: [ ChangeList knownFileInPreambles anySatisfy: [ :aPreamble | chunk beginsWith: aPreamble ]]])
			ifFalse: [
				[
					(Compiler evaluate: chunk logged: false)
						scanFrom: self
				]
					on: Error
					do: [ :ex |
						ex print.
						('while evaluating: ', chunk) print.
						ex pass ].
			].
! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 29/Jul/2026, 7:51:40 pm (UTC)'!
evaluateChunk: chunk

	self checkForPreamble: chunk.

	[ Compiler evaluate: chunk logged: true ]
		on: Error
		do: [ :ex |
			ex print.
			('while evaluating: ', chunk) print.
			ex pass ].
! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 29/Jul/2026, 7:27:14 pm (UTC)'!
fileInNextChunk
		
	(self peekFor: $!!) 
		ifTrue: [
			"This also reads the source code for a method following a
			ClassCategoryReader or ClassCommentReader chunk"
			self createReaderAndFileIn: self nextChunk
			]
		ifFalse: [
			"This reads doIts, preambles and postcripts."
			self evaluateChunk: self nextChunk
			]! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'jmv 29/Jul/2026, 7:29:55 pm (UTC)'!
fileInInformingTo: barBlock
	"fileIn / install of a source code file (.cs.st or .pck.st)"

	[ self atEnd ] whileFalse: [
		barBlock value: self position.
		self skipSeparators.
		[ self fileInNextChunk ]
			on: InMidstOfFileinNotification, UndeclaredVariableWarning, PoolDefinitionNotification 
			do: [ :ex | ex resume: true ] ].
! !


!ChangeSet class methodsFor: 'services' stamp: 'jmv 29/Jul/2026, 7:35:43 pm (UTC)'!
fileIn: aFileEntry
	"File in the entire contents of the file specified by the name provided.
	Used for the [install], [filein] and [install package] services in FileList."

	aFileEntry ifNil: [^ Smalltalk beep ].
	aFileEntry readStreamDo: [ :stream |
		stream ifNotNil: [ stream fileIn ]].
! !

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
