'From Cuis 4.2 of 25 July 2013 [latest update: #2595] on 23 November 2015 at 12:12:11.704215 pm'!

!TranscriptMorph methodsFor: 'updating' stamp: 'jmv 11/23/2015 11:56'!
updateWorkspace
	| newContents newLastIndex newLastUnfinishedEntrySize |
	workspaceWindow ifNotNil: [
		workspaceWindow owner notNil
			ifTrue: [
				newLastIndex _ Transcript lastIndex.
				newLastIndex = lastIncludedIndex ifFalse: [
					newContents _ Transcript contentsStartingAt: lastIncludedIndex+1.
					newLastUnfinishedEntrySize _ Transcript unfinishedEntrySize.
					 lastUnfinishedEntrySize  > 0 ifTrue: [
						newContents _ newContents copyFrom: lastUnfinishedEntrySize+1 to: newContents size.].
					workspace actualContents: workspace actualContents, newContents.
					lastUnfinishedEntrySize _ newLastUnfinishedEntrySize ].
					lastIncludedIndex _ newLastIndex. ]
			ifFalse: [ workspace _ lastIncludedIndex _ nil ]]! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 11/23/2015 12:10'!
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
	aCanvas image: form at: 0@0.
	self updateWorkspace! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 11/23/2015 12:10'!
display
	showOnDisplay ifTrue: [
		self displayOn: Display.
		DisplayScreen screenUpdateRequired: innerRectangle ].
	"So any morph in front of us is repaired when Morphic cycles.
	This includes, for instance, the TranscriptWindow that shows our contents if showOnDisplay is false"
	self changed: #redraw! !

!methodRemoval: TranscriptMorph #redrawNeeded!
TranscriptMorph removeSelector: #redrawNeeded!
