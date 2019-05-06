'From Cuis 5.0 of 7 November 2016 [latest update: #3696] on 19 April 2019 at 3:03:00 pm'!
!classDefinition: 'Transcript class' category: #'System-Support'!
Transcript class
	instanceVariableNames: 'entries firstIndex lastIndex accessSemaphore unfinishedEntry logToFile showOnDisplay lastDisplayPosition bounds logToStdout '!

!Transcript class methodsFor: 'preferred protocol' stamp: 'GC 4/19/2019 14:55:44'!
clearStdout

	logToStdout ifTrue: [
		15 timesRepeat: [ self stdout newLine ]
	]! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'GC 4/19/2019 14:56:52'!
logToStdout: aBoolean

	logToStdout _ aBoolean ! !

!Transcript class methodsFor: 'preferred protocol' stamp: 'GC 4/19/2019 14:56:49'!
logsToStdout

	^ logToStdout! !

!Transcript class methodsFor: 'private' stamp: 'GC 4/19/2019 14:37:08'!
addEntry: aString logToFileAndStdout: fileEntryToLog
	"Add a new entrie to the entries circular list. If full, a new entry will replace the oldest one."

	accessSemaphore critical: [
		
		"Internal circular collection"
		lastIndex _ lastIndex \\ self maxEntries + 1.
		firstIndex = lastIndex ifTrue: [ firstIndex _ firstIndex \\ self maxEntries + 1 ].
		entries at: lastIndex put: aString.
		
		fileEntryToLog ifNotNil: [
			self writeToFile: fileEntryToLog.
			self writeToStdout: fileEntryToLog
		]
	]! !

!Transcript class methodsFor: 'private' stamp: 'GC 4/19/2019 14:16:07'!
stdout

	^ StdIOWriteStream stdout! !

!Transcript class methodsFor: 'private' stamp: 'GC 4/19/2019 14:37:53'!
writeToFile: anEntry

	self filename asFileEntry appendStreamDo: [ :stream | stream nextPutAll: anEntry ]! !

!Transcript class methodsFor: 'private' stamp: 'GC 4/19/2019 14:37:47'!
writeToStdout: anEntry

	logToStdout ifTrue: [ self stdout nextPutAll: anEntry ]! !


!TranscriptMorph methodsFor: 'menu commands' stamp: 'GC 4/19/2019 14:40:27'!
clearStdout

	Transcript clearStdout! !

!TranscriptMorph methodsFor: 'menu commands' stamp: 'GC 4/19/2019 15:02:25'!
dontLogToStdout

	Transcript logToStdout: false! !

!TranscriptMorph methodsFor: 'menu commands' stamp: 'GC 4/19/2019 15:02:22'!
logToStdout

	Transcript logToStdout: true! !


!Transcript class methodsFor: 'preferred protocol' stamp: 'GC 4/19/2019 14:41:02'!
clearAll

	self clearInternal.
	logToFile ifTrue: [ self clearFile ].
	self clearStdout! !

!Transcript class methodsFor: 'private' stamp: 'GC 4/19/2019 14:34:58'!
addEntry: aString
	"Add a new entrie to the entries circular list. If full, a new entry will replace the oldest one."
	| msg now entryToLog |
	entryToLog _ [
		now _ DateAndTime now.
		msg _ String streamContents: [ :strm |
			now printWithMsOn: strm.
			strm
				nextPutAll: ' process:';
				nextPutAll: Processor activeProcess priority printString;
				nextPut: $ ;
				nextPutAll: Processor activeProcess hash printString;
				nextPut: $ ;
				nextPutAll: aString;
				newLine ]].
	
	logToFile or: [ logToStdout ] :: ifTrue: entryToLog value.

	self addEntry: (aString copyReplaceAll: String newLineString with: ' ') logToFileAndStdout: msg! !

!Transcript class methodsFor: 'class initialization' stamp: 'GC 4/19/2019 14:01:37'!
initialize
	"
	self initialize
	"
	showOnDisplay _ true.
	bounds _ 20@20 extent: 300@500.
	logToFile _ false.
	logToStdout _ false.
	entries _ Array new: self maxEntries.
	unfinishedEntry _ String new writeStream.
	lastDisplayPosition _ 0.
	accessSemaphore _ Semaphore forMutualExclusion.
	self clear! !


!TranscriptMorph methodsFor: 'menus' stamp: 'GC 4/19/2019 14:40:07'!
getMenu
	"Set up the menu to apply to the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	doImmediateUpdates
		ifTrue: [ aMenu add: 'Only update in the regular Morphic cycle' 	action: #doRegularUpdates ]
		ifFalse: [ aMenu add: 'Immediately show each entry'	 				action: #doImmediateUpdates ].
	aMenu
		addLine;
		add: 'Workspace with Contents' 					action: #editContents;
		addLine;
		add: 'Clear Transcript' 							action: #clearInternal;
		add: 'Clear Transcript File' 						action: #clearFile;
		add: 'Clear Transcript Stdout' 					action: #clearStdout;
		add: 'Clear Both' 									action: #clearAll;
		addLine.
	Transcript logsToFile
		ifTrue: [ aMenu add: 'Stop logging to File'		action: #dontLogToFile ]
		ifFalse: [ aMenu add: 'Start logging to File'	action: #logToFile ].
	aMenu addLine.
	Transcript logsToStdout
		ifTrue:  [ aMenu add: 'Stop logging to Stdout'	action: #dontLogToStdout ]
		ifFalse: [ aMenu add: 'Start logging to Stdout'	action: #logToStdout  ].
	
	^ aMenu! !

Transcript initialize!
!classDefinition: 'Transcript class' category: #'System-Support'!
Transcript class
	instanceVariableNames: 'entries firstIndex lastIndex accessSemaphore unfinishedEntry logToFile showOnDisplay lastDisplayPosition bounds logToStdout '!
"Postscript:
To initialize Transcript logToStdout class variable"
Transcript initialize.
Display restore!

