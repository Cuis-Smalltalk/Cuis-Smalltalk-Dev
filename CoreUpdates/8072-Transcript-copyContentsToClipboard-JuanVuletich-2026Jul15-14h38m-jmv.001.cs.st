'From Cuis7.9 [latest update: #8064] on 15 July 2026 at 3:33:45 pm'!

!TranscriptMorph methodsFor: 'menu commands' stamp: 'jmv 15/Jul/2026, 6:32:35 pm (UTC)'!
copyContentsToCliboard
	Clipboard storeObject: Transcript contents.! !


!TranscriptMorph methodsFor: 'menu' stamp: 'jmv 15/Jul/2026, 6:32:17 pm (UTC)'!
getMenu
	"Set up the menu to apply to the receiver"

	| aMenu |
	aMenu := MenuMorph new defaultTarget: self.
	doImmediateUpdates
		ifTrue: [ aMenu add: 'Only update in the regular Morphic cycle' 	action: #doRegularUpdates ]
		ifFalse: [ aMenu add: 'Immediately show each entry'	 			action: #doImmediateUpdates ].
	aMenu
		addLine;
		add: 'Copy Contents to Clipboard' 					action: #copyContentsToCliboard;
		add: 'Workspace with Contents' 					action: #editContents;
		addLine;
		add: 'Clear Transcript (l)' 							action: #clearInternal;
		add: 'Clear Transcript File' 						action: #clearFile;
		add: 'Clear Transcript Stdout' 						action: #clearStdout;
		add: 'Clear Both' 									action: #clearAll;
		addLine.
	Transcript logsToFile
		ifTrue: [ aMenu add: 'Stop logging to File'			action: #dontLogToFile ]
		ifFalse: [ aMenu add: 'Start logging to File'			action: #logToFile ].
	aMenu addLine.
	Transcript logsToStdout
		ifTrue:  [ aMenu add: 'Stop logging to Stdout'		action: #dontLogToStdout ]
		ifFalse: [ aMenu add: 'Start logging to Stdout'		action: #logToStdout  ].
	
	^ aMenu! !

