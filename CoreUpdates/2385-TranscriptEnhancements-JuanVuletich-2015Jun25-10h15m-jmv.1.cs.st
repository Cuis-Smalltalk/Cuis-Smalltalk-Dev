'From Cuis 4.2 of 25 July 2013 [latest update: #2384] on 25 June 2015 at 10:35:04.296776 am'!
!classDefinition: #TranscriptMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #TranscriptMorph
	instanceVariableNames: 'form workspace lastIncludedIndex workspaceWindow lastUnfinishedEntrySize doImmediateUpdates '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!TranscriptWindow methodsFor: 'drawing' stamp: 'jmv 6/24/2015 12:54'!
visible: aBoolean
	super visible: aBoolean.
	Transcript showOnDisplay: aBoolean! !


!TranscriptMorph methodsFor: 'initialization' stamp: 'jmv 6/25/2015 10:19'!
initialize
	super initialize.
	doImmediateUpdates _ false! !

!TranscriptMorph methodsFor: 'menu commands' stamp: 'jmv 6/25/2015 10:25'!
doImmediateUpdates
	doImmediateUpdates _ true.
	Transcript 
		showOnDisplay: doImmediateUpdates! !

!TranscriptMorph methodsFor: 'menu commands' stamp: 'jmv 6/25/2015 10:25'!
doRegularUpdates
	doImmediateUpdates _ false.
	Transcript 
		showOnDisplay: doImmediateUpdates! !


!SystemWindow methodsFor: 'drawing' stamp: 'jmv 6/24/2015 12:54'!
visible: aBoolean
	super visible: aBoolean.
	aBoolean ifTrue: [
		self activate ]! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 6/25/2015 10:29'!
drawOn: aCanvas
	"This breaks shadow when taking the TranscriptWindow with the hand"
	"
	Transcript
		showOnDisplay: false;
		bounds: self morphBoundsInWorld;
		displayOn: aCanvas form.
	"
	Transcript
		showOnDisplay: true;
		bounds: (0@0 extent: extent);
		displayOn: form;
		bounds: self morphBoundsInWorld;
		showOnDisplay: doImmediateUpdates.
	aCanvas image: form at: 0@0
	! !

!TranscriptMorph methodsFor: 'menus' stamp: 'jmv 6/25/2015 10:24'!
getMenu
	"Set up the menu to apply to the receiver"

	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	doImmediateUpdates
		ifTrue: [ aMenu addList: #(('Only update in the regular Morphic cycle'	doRegularUpdates)) ]
		ifFalse: [ aMenu addList: #(('Immediately show each entry'	doImmediateUpdates)) ].
	aMenu addList: #(
		-
		('Workspace with Contents'	editContents)
		-
		('Clear Transcript'				clearInternal)
		('Clear Transcript File'			clearFile)
		('Clear Both'					clearAll)
		-).
	Transcript logsToFile
		ifTrue: [ aMenu addList: #(('Stop logging to File'	dontLogToFile)) ]
		ifFalse: [ aMenu addList: #(('Start logging to File'	logToFile)) ].
	^ aMenu! !

!classDefinition: #TranscriptMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #TranscriptMorph
	instanceVariableNames: 'form workspace lastIncludedIndex workspaceWindow lastUnfinishedEntrySize doImmediateUpdates'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!TranscriptMorph reorganize!
('initialization' initialize)
('drawing' drawOn:)
('event handling testing' handlesMouseDown:)
('geometry' privateExtent:)
('menus' getMenu mouseButton2Activity)
('menu commands' clearAll clearFile clearInternal doImmediateUpdates doRegularUpdates dontLogToFile editContents logToFile)
('updating' redrawNeeded)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

	TranscriptMorph allInstancesDo: [ :tm | tm instVarNamed: 'doImmediateUpdates' put: false ]!


