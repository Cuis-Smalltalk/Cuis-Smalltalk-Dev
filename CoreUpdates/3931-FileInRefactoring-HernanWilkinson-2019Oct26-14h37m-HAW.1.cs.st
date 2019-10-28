'From Cuis 5.0 [latest update: #3926] on 26 October 2019 at 3:08:06 pm'!

!ChangeList class methodsFor: 'scanning' stamp: 'HAW 10/26/2019 14:54:50'!
knownFileInPreambles

	^ `{
	'classDefinition:'. 
	'classRemoval:'. 
	'methodRemoval:'. 
	'methodMoveToSomePackage:'. 
	'classMoveToSomePackage:'. 
	'provides:'. 
	'requires:' }`! !

!ChangeList class methodsFor: 'scanning' stamp: 'HAW 10/26/2019 14:55:26'!
knownPreambles

	^ `{ 'commentStamp:'. 'methodsFor:'. }, ChangeList knownFileInPreambles`! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 15:01:19'!
compileNextChunk
		
	(self peekFor: $!!) 
		ifTrue: [ self compileNextChunkWhenStartsWithExclamationMark ]
		ifFalse: [ self compileNextChunkWhenDoesNotStartWithExclamationMark ]! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 14:43:50'!
compileNextChunkHandlingExceptions 

	[ self compileNextChunk ]
		on: InMidstOfFileinNotification, UndeclaredVariableWarning
		do: [ :ex | ex resume: true ]! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 15:03:58'!
compileNextChunkWhenDoesNotStartWithExclamationMark

	| chunk |
	
	chunk := self nextChunk.
	self checkForPreamble: chunk.
	self evaluate: [ Compiler evaluate: chunk logged: true ] printingErrorWith: chunk
		! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 15:04:31'!
compileNextChunkWhenStartsWithExclamationMark

	| chunk |
	
	chunk := self nextChunk.
	
	"These are the ones that should do nothing, 
	because next line is a doit that does the stuff
	(or because it is handled elsewhere)"
	
	((chunk beginsWith: 'description:  ') 
		or: [ ChangeList knownFileInPreambles anySatisfy: [ :aPreamble | chunk beginsWith: aPreamble ]])
		ifFalse: [ self evaluate: [ (Compiler evaluate: chunk logged: false) scanFrom: self ] printingErrorWith: chunk ]! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 15:03:41'!
evaluate: aBlock printingErrorWith: chunk

	aBlock
		on: Error
		do: [ :ex |
			ex print.
			('while evaluating: ', chunk) print.
			ex pass ]
				! !

!PositionableStream methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 14:44:04'!
fileInInformingTo: barBlock 
	
	[ self atEnd ] whileFalse: [
		barBlock value: self position.
		self skipSeparators.
		self compileNextChunkHandlingExceptions ]! !


!ChangeList methodsFor: 'scanning' stamp: 'HAW 10/26/2019 14:56:01'!
itemIsRecognized: item

	^ self class knownPreambles anySatisfy: [ :preamble | item includesSubString: preamble ] ! !


!PositionableStream methodsFor: 'fileIn/Out' stamp: 'HAW 10/26/2019 15:07:52'!
fileInAnnouncing: announcement
	"This is special for reading expressions from text that has been formatted 
	with exclamation delimitors. The expressions are read and passed to the 
	Compiler. Put up a progress report with the given announcement as the title."

	Utilities logsUserChanges: false.

	announcement 
		displayProgressAt: Sensor mousePoint
		from: 0
		to: self size
		during: [ :barBlock | self fileInInformingTo: barBlock ].

	"Note:  The main purpose of this banner is to flush the changes file."
	Utilities logsUserChanges: true.
	Smalltalk logChange: '----End fileIn of ' , self name , '----'.
	! !

!methodRemoval: PositionableStream #m1 stamp: 'HAW 10/26/2019 15:00:53'!
PositionableStream removeSelector: #m1!
!methodRemoval: ChangeList #knownPreambles stamp: 'HAW 10/26/2019 14:56:12'!
ChangeList removeSelector: #knownPreambles!
!methodRemoval: ChangeList #knownFileInPreambles stamp: 'HAW 10/26/2019 14:56:09'!
ChangeList removeSelector: #knownFileInPreambles!

!ChangeList class reorganize!
('public access' browseContents: browseFrom:on:labeled: browsePackageContents: browseRecent:on: browseRecentLog browseRecentLogOn: browseRecentLogOn:startingFrom:)
('fileIn/Out' fileReaderServicesForFile:suffix: serviceContents servicePackageContents serviceRecentChanges)
('scanning' knownFileInPreambles knownPreambles)
!


!PositionableStream reorganize!
('accessing' back contents contentsOfEntireFile crLfNextLine last next: next:into: next:into:startingAt: next:putAll: next:putAll:startingAt: nextAvailable: nextDelimited: nextInto: nextInto:startingAt: nextKeyword nextLine nextWordsInto: oldBack oldPeekBack originalContents peek peekBack peekFor: untilAnySatisfying: upTo: upToAll: upToAny: upToEnd)
('testing' atEnd isBinary isCharacters isEmpty isFileStream isText notEmpty)
('positioning' backUpTo: match: padTo:put: padToNextLongPut: position position: reset resetContents setToEnd skip: skipTo:)
('fileIn/Out' backChunk checkForPreamble: compileNextChunk compileNextChunkHandlingExceptions compileNextChunkWhenDoesNotStartWithExclamationMark compileNextChunkWhenStartsWithExclamationMark copyMethodChunkFrom: evaluate:printingErrorWith: fileIn fileInAnnouncing: fileInInformingTo: nextChunk skipSeparators unCommand)
('private' collectionSpecies on: positionError setFrom:to:)
('filein/out' copyPreamble:from:at:)
('gui' untilEnd:displayingProgress:)
!

