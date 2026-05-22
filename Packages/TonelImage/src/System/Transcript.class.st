"
A new implementation of Transcript.
- Thread safe.
- Very fast.
- Independent of Morphic or any other UI framework.
- Immediate feedback.
- Can log to file.
- Not an editor. Only used for output.
- All protocol is on the Class side
"
Class {
	#name : 'Transcript',
	#superclass : 'Object',
	#classInstVars : [
		'entries',
		'firstIndex',
		'lastIndex',
		'accessSemaphore',
		'unfinishedEntry',
		'logToFile',
		'showOnDisplay',
		'lastDisplayPosition',
		'bounds',
		'logToStdout',
		'displayEngine',
		'backgroundColor',
		'dirtyRectOnDisplay',
		'lastDisplayFlush'
	],
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'private' }
Transcript class >> addEntry: aString [
	"Add a new entrie to the entries circular list. If full, a new entry will replace the oldest one."
	| msg now |
	logToFile | logToStdout ifTrue: [
		now := DateAndTime now.
		msg := String streamContents: [ :strm |
			now printWithMsOn: strm.
			strm
				nextPutAll: ' process:';
				nextPutAll: Processor activeProcess priority printString;
				nextPut: $ ;
				nextPutAll: Processor activeProcess hash printString;
				nextPut: $ ;
				nextPutAll: aString;
				newLine ]].

	self addEntry: (aString copyReplaceAll: String newLineString with: ' ') log: msg
]

{ #category : 'private' }
Transcript class >> addEntry: aString log: otherString [
	"Add a new entrie to the entries circular list. If full, a new entry will replace the oldest one."

	accessSemaphore critical: [
		
		"Internal circular collection"
		lastIndex := lastIndex \\ self maxEntries + 1.
		firstIndex = lastIndex ifTrue: [ firstIndex := firstIndex \\ self maxEntries + 1 ].
		entries at: lastIndex put: aString.
		
		otherString ifNotNil: [
			self writeToFile: otherString.
			self writeToStdout: otherString
		]
	]
]

{ #category : 'accessing' }
Transcript class >> backgroundColor: aColor [
	backgroundColor := aColor
]

{ #category : 'preferred protocol' }
Transcript class >> bounds: aRectangle [

	bounds := aRectangle
]

{ #category : 'preferred protocol' }
Transcript class >> clear [

	accessSemaphore critical: [
		"Having at least one entry simplifies handling of the entries circular collection"
		firstIndex := 1.
		lastIndex := 0.
		entries atAllPut: nil.
		Smalltalk systemInformationString linesDo: [ :l |
			lastIndex := lastIndex + 1.
			entries at: lastIndex put: l ].
		unfinishedEntry reset.
		lastDisplayPosition := 0.
		
		logToFile ifTrue: [
			DirectoryEntry logsDirectory // self filename forceWriteStreamDo: [ :stream |
				stream nextPutAll: 'Transcript log started: '.
				DateAndTime now printOn: stream.
				stream
					newLine;
					nextPutAll: '------------------------------------------------------------------------';
					newLine ]]].
	self display
]

{ #category : 'preferred protocol' }
Transcript class >> clearAll [

	self clearInternal.
	logToFile ifTrue: [ self clearFile ].
	self clearStdout
]

{ #category : 'preferred protocol' }
Transcript class >> clearFile [
	accessSemaphore critical: [
		DirectoryEntry logsDirectory // self filename forceWriteStreamDo: [ :stream |
			stream nextPutAll: 'Transcript log started: '.
			DateAndTime now printOn: stream.
			stream
				newLine;
				nextPutAll: '------------------------------------------------------------------------';
				newLine ]]
]

{ #category : 'preferred protocol' }
Transcript class >> clearInternal [

	accessSemaphore critical: [
		"Having at least one entry simplifies handling of the entries circular collection"
		firstIndex := 1.
		lastIndex := 1.
		entries atAllPut: nil.
		entries at: 1 put: 'Transcript'.
		unfinishedEntry reset.
		lastDisplayPosition := 0 ].
	self display
]

{ #category : 'preferred protocol' }
Transcript class >> clearStdout [

	logToStdout ifTrue: [
		15 timesRepeat: [ self stdout newLine ].
		self stdout flush
	]
]

{ #category : 'private' }
Transcript class >> contents [
	^self contentsStartingAt: firstIndex
]

{ #category : 'private' }
Transcript class >> contentsStartingAt: index [
	^String streamContents: [ :strm |
		self entriesStartingAt: index do: [ :e |
			strm nextPutAll: e; newLine ].
		strm nextPutAll: unfinishedEntry contents ]
]

{ #category : 'old Transcript compatibility' }
Transcript class >> cr [
	"Old Squeak Transcript protocol. Still used for some doIt examples.
	In the older TranscriptStream, it added a CR character.
	Now, finish the current incomplete entry."

	self finishEntry
]

{ #category : 'private' }
Transcript class >> dirtyScreenRect: someDamage [
	"aRectangle needs updating on real OS Screen. This can be simply done by
	DisplayScreen forceDeferredToScreen: damage.
	The reason to do something more complicated is because #forceDeferredToScreen: may be expensive, especially on Mac Intel VMs as of July 2025, and especially if done at high frequency (i.e. with a time interval between calls below 25 milliseconds.
	"
	| now |
	now := Time localMillisecondClock.
	lastDisplayFlush ifNil: [ lastDisplayFlush := now ].

	"If enough time has elapsed since last push, just do it."
	now - lastDisplayFlush > 60 ifTrue: [
		dirtyRectOnDisplay := someDamage quickMerge: dirtyRectOnDisplay.
		DisplayScreen forceDeferredToScreen: dirtyRectOnDisplay.
		dirtyRectOnDisplay := nil.
		lastDisplayFlush := now.
		^self
		].

	dirtyRectOnDisplay
		ifNil: [
			"We don't have a previous dirty rect. If there has been some, it has already been pushed to Screen.
			Start a forked process to push it. Do it in a while. In the meantime, other calls to this process may
			add additional damage. It is also possible that some other call to this method ends up clearing the
			dirtyRectOnDisplay. Consider that too."
			dirtyRectOnDisplay := someDamage.
			[
				(Delay forMilliseconds: 100) wait.
				dirtyRectOnDisplay ifNotNil: [
					DisplayScreen forceDeferredToScreen: dirtyRectOnDisplay.
					dirtyRectOnDisplay := nil.
					lastDisplayFlush := Time localMillisecondClock ].
			] forkAt: Processor userInterruptPriority ]
		ifNotNil: [
			"We already have a forked process that will soon do the push. Wait for it. Merge damage."
			dirtyRectOnDisplay := someDamage quickMerge: dirtyRectOnDisplay.
			].
]

{ #category : 'displaying' }
Transcript class >> display [
	"See
		#display
		#displayUnfinishedEntry
		#displayEngine"
	showOnDisplay ifTrue: [
		self displayOnDisplay.
		self dirtyScreenRect: bounds ].
	"So any morph in front of us is repaired when Morphic cycles.
	This includes, for instance, the TranscriptWindow that shows our contents if showOnDisplay is false"
	UISupervisor whenUIinSafeState: [ self triggerEvent: #redrawTranscriptArea ].
]

{ #category : 'private' }
Transcript class >> displayEngine [
	"See
		#display
		#displayUnfinishedEntry
		#displayEngine"
	"
	Transcript releaseClassCachedState
	"
	| deferredDisplay |
	deferredDisplay := DisplayScreen deferredUpdatingDisplay.
	(displayEngine isNil or: [ displayEngine class ~= BitBltCanvasEngine])
		ifTrue: [
			^ displayEngine := BitBltCanvasEngine toForm: deferredDisplay ].
	displayEngine destForm == deferredDisplay ifFalse: [
		displayEngine setDestForm: deferredDisplay ].
	^ displayEngine
]

{ #category : 'displaying' }
Transcript class >> displayOnDisplay [
	"To be called directly, not from Morphic.
	See #displayOnCanvas:in:"
	"
	1 to: 2000 do: [ :i | i print ].
	1 to: 750 do: [ :i | i print ].
	Transcript displayOnDisplay.
	Display forceToScreen
	"
	self displayEngine.
	displayEngine clipRect: bounds.
	displayEngine
		fill: bounds fillColor: backgroundColor rule: Form over.
	self displayTextOn: self in: bounds.
]

{ #category : 'displaying' }
Transcript class >> displayTextOn: aCanvasOrSelf in: aRectangle [
	"See senders"
	| font count x y fh innerR index |
	font := FontFamily defaultFamilyAndPointSize.
	innerR := aRectangle insetBy: self padding.
	x := innerR left.
	y := innerR top.
	fh := font lineSpacing.
	count := lastIndex - firstIndex \\ self maxEntries + 1 min: innerR height // fh - 1.
	index := lastIndex - count \\ self maxEntries + 1.
	count timesRepeat: [
		(entries at: index) ifNotNil: [ :string |
			aCanvasOrSelf drawString: string at: x@y font: font color: Theme current text.
			y := y + fh ].
		index := index \\ self maxEntries + 1 ].
	aCanvasOrSelf drawString: unfinishedEntry contents at: x@y font: font color: Theme current text
]

{ #category : 'displaying' }
Transcript class >> displayUnfinishedEntry [
	"See
		#display
		#displayUnfinishedEntry
		#displayEngine"
	showOnDisplay ifTrue: [
		self displayUnfinishedEntryOnDisplay ifNotNil: [ :damage |
			self dirtyScreenRect: damage ]]
]

{ #category : 'displaying' }
Transcript class >> displayUnfinishedEntryOnDisplay [

	| font count string x y fh r innerR |
	self displayEngine.
	innerR := bounds insetBy: self padding.
	lastDisplayPosition < innerR right ifTrue: [
		font := FontFamily defaultFamilyAndPointSize.
		fh := font lineSpacing.
		count := innerR height // fh-1.
		x := innerR left.
		string := unfinishedEntry contents.
		y := ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font lineSpacing + innerR top.
		r := innerR left: lastDisplayPosition.
		displayEngine clipRect: r.
		(self drawString: string at: x@y font: font color: `Color darkGray`) ifNotNil: [ :lastPoint |
			lastDisplayPosition := lastPoint x.
			^r ]].
	^nil
]

{ #category : 'displaying' }
Transcript class >> drawString: s at: pt font: font color: aColor [
	"Answer position to place next glyph
	Answer nil if nothing was done"

	displayEngine colorMap: nil.
	^s asUnicodeString drawOnBitBltCanvasEngine: displayEngine
		from: 1
		to: s size
		at: pt roundedHAFZ
		font: font
		color: aColor
]

{ #category : 'old Transcript compatibility' }
Transcript class >> endEntry [
	"For compatibility with old TranscriptStream. nop here"
]

{ #category : 'iteration' }
Transcript class >> entriesStartingAt: startIndex do: aBlock [
	| f i |
	f := startIndex-1.
	firstIndex > lastIndex ifTrue: [ f := f - self maxEntries ].
	f < lastIndex ifTrue: [
		i := f \\ self maxEntries + 1.
		[
			aBlock value: (entries at: i).
			i = lastIndex
		] whileFalse: [ i := i \\ self maxEntries + 1 ] ]
]

{ #category : 'constants' }
Transcript class >> filename [
	^'transcript.txt'
]

{ #category : 'private' }
Transcript class >> finishEntry [
	| newEntry |
	newEntry := unfinishedEntry contents.
	unfinishedEntry reset.
	lastDisplayPosition := 0.
	self addEntry: newEntry.
	self display.
]

{ #category : 'old Transcript compatibility' }
Transcript class >> flush [
	self endEntry
]

{ #category : 'class initialization' }
Transcript class >> initialize [
	"
	self initialize
	"
	showOnDisplay := true.
	bounds := 20@20 extent: 300@500.
	logToFile := false.
	logToStdout := false.
	entries := Array new: self maxEntries.
	unfinishedEntry := String writeStream.
	lastDisplayPosition := 0.
	accessSemaphore := Semaphore forMutualExclusion.
	backgroundColor := `Color veryVeryLightGray`.
	self clear
]

{ #category : 'accessing' }
Transcript class >> lastIndex [
	^lastIndex
]

{ #category : 'preferred protocol' }
Transcript class >> log: aString [
	self addEntry: aString.
	self display
]

{ #category : 'preferred protocol' }
Transcript class >> logToFile: aBoolean [
	"
	self logToFile
	"
	logToFile := aBoolean
]

{ #category : 'preferred protocol' }
Transcript class >> logToStdout: aBoolean [

	logToStdout := aBoolean 
]

{ #category : 'preferred protocol' }
Transcript class >> logsToFile [
	^logToFile
]

{ #category : 'preferred protocol' }
Transcript class >> logsToStdout [

	^ logToStdout
]

{ #category : 'constants' }
Transcript class >> maxEntries [
	^1000
]

{ #category : 'old Transcript compatibility' }
Transcript class >> newLine [
	"WriteStream protocol.
	In the older TranscriptStream, it added a CR character.
	Now, finish the current incomplete entry."

	self finishEntry
]

{ #category : 'old Transcript compatibility' }
Transcript class >> newLineWhen: aCondition [

	aCondition ifTrue: [ self newLine ]
	
	
]

{ #category : 'old Transcript compatibility' }
Transcript class >> nextPut: aCharacter [
	"WriteStream protocol.
	Append aCharacter to the unfinishedEntry.
	cr characters sent with this message do NOT finish the current unfinishedEntry."

	unfinishedEntry nextPut: aCharacter.
	self displayUnfinishedEntry
]

{ #category : 'old Transcript compatibility' }
Transcript class >> nextPut: anObject when: aCondition  [

	aCondition ifTrue: [ self nextPut: anObject ].
]

{ #category : 'old Transcript compatibility' }
Transcript class >> nextPutAll: aString [
	"WriteStream protocol.
	Append aString to the unfinishedEntry.
	cr characters sent with this message do NOT finish the current unfinishedEntry."

	unfinishedEntry nextPutAll: aString.
	self displayUnfinishedEntry
]

{ #category : 'old Transcript compatibility' }
Transcript class >> nextPutAll: aCollection when: aCondition [

	aCondition ifTrue: [ self nextPutAll: aCollection ].

]

{ #category : 'ui' }
Transcript class >> open [
	"
	Transcript open
	"
	| win m |
	win := TranscriptWindow new.
	win
		setLabel: 'Transcript';
		model: Transcript.
	m := TranscriptMorph new.
	win layoutMorph
		"This is needed so the Layout correctly clips the TranscriptMorph"
		doAdoptWidgetsColor;
		doClipSubmorphs;
		addMorph: m proportionalHeight: 1.
	win model when: #redrawTranscriptArea send: #invalidateBounds to: win.
	^ win openInWorld
]

{ #category : 'displaying' }
Transcript class >> padding [
	^ 3
]

{ #category : 'old Transcript compatibility' }
Transcript class >> print: anObject [
	"Stream protocol"
	anObject printOn: self
]

{ #category : 'old Transcript compatibility' }
Transcript class >> print: anObject when: aCondition  [

	aCondition ifTrue: [self print: anObject].
]

{ #category : 'system startup' }
Transcript class >> releaseClassCachedState [
	displayEngine := nil
]

{ #category : 'old Transcript compatibility' }
Transcript class >> show: anObject [
	"Old TranscriptStream protocol."
	"
	Transcript
	show: 'Hello

		World';
	show: 'Hello
           
		World';
	newLine;
	show: 'stuff';
	newLine;
	newLine;
	show: 'done '; show: 'yeah'.
	"
	(anObject asString findTokens: `{Character newLineCharacter}` keep: `{Character newLineCharacter}`)
		do: [ :line |
			line = Character newLineCharacter asString
				ifFalse: [ self nextPutAll: line ]
				ifTrue: [ self newLine ]]
]

{ #category : 'preferred protocol' }
Transcript class >> showOnDisplay: aBoolean [
	"
	self logToFile
	"
	showOnDisplay := aBoolean
]

{ #category : 'displaying' }
Transcript class >> showsOnDisplay [
	^showOnDisplay
]

{ #category : 'old Transcript compatibility' }
Transcript class >> space [
	"WriteStream protocol.
	Append a space character to the receiver."

	self nextPut: Character space
]

{ #category : 'old Transcript compatibility' }
Transcript class >> spaceWhen: aCondition [
 
	aCondition ifTrue: [ self space ]
]

{ #category : 'private' }
Transcript class >> stdout [

	^ StdIOWriteStream stdout
]

{ #category : 'old Transcript compatibility' }
Transcript class >> store: stuff [
	self print: stuff
]

{ #category : 'old Transcript compatibility' }
Transcript class >> tab [
	"WriteStream protocol.
	Append a tab character to the receiver."

	self nextPut: Character tab
]

{ #category : 'old Transcript compatibility' }
Transcript class >> tabWhen: aCondition [

	aCondition ifTrue: [ self tab ]
]

{ #category : 'accessing' }
Transcript class >> unfinishedEntrySize [
	^unfinishedEntry position
]

{ #category : 'private' }
Transcript class >> writeToFile: anEntry [

	logToFile ifTrue: [
		DirectoryEntry logsDirectory // self filename appendStreamDo: [ :stream | stream nextPutAll: anEntry ]]
]

{ #category : 'private' }
Transcript class >> writeToStdout: anEntry [

	logToStdout ifTrue: [ self stdout nextPutAll: anEntry; flush ]
]
