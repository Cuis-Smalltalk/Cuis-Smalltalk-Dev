'From Cuis7.9 [latest update: #8015] on 22 June 2026 at 3:15:56 pm'!

!TranscriptMorph methodsFor: 'events' stamp: 'len 22/Jun/2026, 8:38:55 am (UTC)'!
keyStroke: anEvent
	(anEvent commandAltKeyPressed and: [anEvent keyCharacter = $l]) ifTrue: [^ self clearInternal].
	^ super keyStroke: anEvent! !


!TranscriptMorph methodsFor: 'menu' stamp: 'len 22/Jun/2026, 8:39:40 am (UTC)'!
getMenu
	"Set up the menu to apply to the receiver"

	| aMenu |
	aMenu := MenuMorph new defaultTarget: self.
	doImmediateUpdates
		ifTrue: [ aMenu add: 'Only update in the regular Morphic cycle' 	action: #doRegularUpdates ]
		ifFalse: [ aMenu add: 'Immediately show each entry'	 				action: #doImmediateUpdates ].
	aMenu
		addLine;
		add: 'Workspace with Contents' 					action: #editContents;
		addLine;
		add: 'Clear Transcript (l)' 							action: #clearInternal;
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

