"
Provide a view for integrating Transcript into the Morphic World.
"
Class {
	#name : 'TranscriptMorph',
	#superclass : 'ColoredBoxMorph',
	#instVars : [
		'workspace',
		'lastIncludedIndex',
		'workspaceWindow',
		'lastUnfinishedEntrySize',
		'doImmediateUpdates'
	],
	#category : 'Morphic-Composite Widgets',
	#package : 'Morphic',
	#tag : 'Composite Widgets'
}

{ #category : 'new-morph participation' }
TranscriptMorph class >> includeInNewMorphMenu [
	"Return true for all classes that can be instantiated from the menu"
	^ false
]

{ #category : 'drawing' }
TranscriptMorph >> adoptWidgetsColor: paneColor [
	color = self defaultColor ifTrue: [
		color := Theme current paneBackgroundFrom: paneColor.
		Transcript backgroundColor: color.
	].
	super adoptWidgetsColor: paneColor.
]

{ #category : 'menu commands' }
TranscriptMorph >> clearAll [
	Transcript clearAll
]

{ #category : 'menu commands' }
TranscriptMorph >> clearFile [
	Transcript clearFile
]

{ #category : 'menu commands' }
TranscriptMorph >> clearInternal [
	Transcript clearInternal
]

{ #category : 'menu commands' }
TranscriptMorph >> clearStdout [

	Transcript clearStdout
]

{ #category : 'accessing' }
TranscriptMorph >> color: aColor [
	"Set the receiver's color. "
	super color: aColor.
	Transcript backgroundColor: color.
]

{ #category : 'menu commands' }
TranscriptMorph >> doImmediateUpdates [
	doImmediateUpdates := true.
	Transcript 
		showOnDisplay: doImmediateUpdates
]

{ #category : 'menu commands' }
TranscriptMorph >> doRegularUpdates [
	doImmediateUpdates := false.
	Transcript 
		showOnDisplay: doImmediateUpdates
]

{ #category : 'menu commands' }
TranscriptMorph >> dontLogToFile [
	Transcript logToFile: false
]

{ #category : 'menu commands' }
TranscriptMorph >> dontLogToStdout [

	Transcript logToStdout: false
]

{ #category : 'drawing' }
TranscriptMorph >> drawOn: aCanvas [
	aCanvas
		fillRectangle: self localBounds
		color: color.
	Transcript displayTextOn: aCanvas in: self localBounds.
	Transcript bounds: (self externalizeBoundsToWorld: self localBounds)
				encompassingIntegerRectangle.
	self updateWorkspace
]

{ #category : 'menu commands' }
TranscriptMorph >> editContents [
	workspace := Workspace new.
	workspace isTextStylingEnabled ifTrue: [ workspace toggleStyling ].
	workspace contents: Transcript contents.
	lastUnfinishedEntrySize := Transcript unfinishedEntrySize.
	workspaceWindow := workspace openLabel: 'Workspace - Transcript contents'.
	lastIncludedIndex := Transcript lastIndex.
]

{ #category : 'menu' }
TranscriptMorph >> getMenu [
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
	
	^ aMenu
]

{ #category : 'event handling testing' }
TranscriptMorph >> handlesMouseDown: aMouseButtonEvent [
	^ true
]

{ #category : 'initialization' }
TranscriptMorph >> initialize [
	super initialize.
	doImmediateUpdates := true.
	Transcript backgroundColor: color.
	Transcript showOnDisplay: doImmediateUpdates.
]

{ #category : 'menu commands' }
TranscriptMorph >> logToFile [
	Transcript logToFile: true
]

{ #category : 'menu commands' }
TranscriptMorph >> logToStdout [

	Transcript logToStdout: true
]

{ #category : 'updating' }
TranscriptMorph >> updateWorkspace [
	| newContents newLastIndex newLastUnfinishedEntrySize |
	workspaceWindow ifNotNil: [
		workspaceWindow owner notNil
			ifTrue: [
				newLastIndex := Transcript lastIndex.
				newLastIndex = lastIncludedIndex ifFalse: [
					newContents := Transcript contentsStartingAt: lastIncludedIndex+1.
					newLastUnfinishedEntrySize := Transcript unfinishedEntrySize.
					 lastUnfinishedEntrySize  > 0 ifTrue: [
						newContents := newContents copyFrom: lastUnfinishedEntrySize+1 to: newContents size.].
					workspace actualContents: workspace actualContents, newContents.
					lastUnfinishedEntrySize := newLastUnfinishedEntrySize ].
					lastIncludedIndex := newLastIndex. ]
			ifFalse: [ workspace := lastIncludedIndex := nil ]]
]
