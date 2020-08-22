'From Cuis 5.0 [latest update: #4359] on 21 August 2020 at 9:08:32 pm'!
!classDefinition: 'Transcript class' category: #'System-Support'!
Transcript class
	instanceVariableNames: 'entries firstIndex lastIndex accessSemaphore unfinishedEntry logToFile showOnDisplay lastDisplayPosition bounds logToStdout displayCanvas '!
!classDefinition: #TranscriptMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #TranscriptMorph
	instanceVariableNames: 'form workspace lastIncludedIndex workspaceWindow lastUnfinishedEntrySize doImmediateUpdates '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!Transcript class methodsFor: 'private' stamp: 'jmv 8/21/2020 21:08:10'!
canvas

	(displayCanvas isNil or: [
		displayCanvas class ~= MorphicCanvas activeSubclass]) ifTrue: [
			displayCanvas _ Display getCanvas ].
	^ displayCanvas! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 8/21/2020 19:51:49'!
displayOnCanvas: aCanvas
	"
	1 to: 2000 do: [ :i | i print ].
	1 to: 750 do: [ :i | i print ].
	Transcript displayOn: Display in: bounds
	"
	| font count string x y fh innerR index |
	"aCanvas form fill: aRectangle fillColor: `Color white`."
	font _ FontFamily defaultFamilyAndPointSize.

	"innerR _ aRectangle insetBy: self padding."
	innerR _ 0@0 extent: 100@100.
	aCanvas newClipRect: innerR.
	x _ innerR left.
	y _ innerR top.
	fh _ font lineSpacing.
	count _ lastIndex - firstIndex \\ self maxEntries + 1 min: innerR height // fh - 1.
	index _ lastIndex - count \\ self maxEntries + 1.
	count timesRepeat: [
		string _ entries at: index.	
		aCanvas drawString: string at: x@y font: font color: `Color veryDarkGray`.
		y _ y + fh.
		index _ index \\ self maxEntries + 1 ].
	string _ unfinishedEntry contents.
	aCanvas drawString: string at: x@y font: font color: `Color veryDarkGray`! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 8/21/2020 19:59:46'!
displayOnCanvas: aCanvas in: aRectangle
	"
	1 to: 2000 do: [ :i | i print ].
	1 to: 750 do: [ :i | i print ].
	Transcript displayOn: Display in: bounds
	"
	| font count string x y fh innerR index |
	aCanvas
		fillRectangle: aRectangle
		color: `Color white`.
	font _ FontFamily defaultFamilyAndPointSize.
	innerR _ aRectangle insetBy: self padding.
	x _ innerR left.
	y _ innerR top.
	fh _ font lineSpacing.
	count _ lastIndex - firstIndex \\ self maxEntries + 1 min: innerR height // fh - 1.
	index _ lastIndex - count \\ self maxEntries + 1.
	count timesRepeat: [
		string _ entries at: index.	
		aCanvas drawString: string at: x@y font: font color: `Color veryDarkGray`.
		y _ y + fh.
		index _ index \\ self maxEntries + 1 ].
	string _ unfinishedEntry contents.
	aCanvas drawString: string at: x@y font: font color: `Color veryDarkGray`! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 8/21/2020 19:48:07'!
displayUnfinishedEntryOnCanvas: aCanvas

	| font count string x y fh r innerR |
	innerR _ bounds insetBy: self padding.
	lastDisplayPosition < innerR right ifTrue: [
		font _ FontFamily defaultFamilyAndPointSize.
		fh _ font lineSpacing.
		count _ innerR height // fh-1.
		x _ innerR left.
		string _ unfinishedEntry contents.
		y _ ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font lineSpacing + innerR top.
		r _ innerR left: lastDisplayPosition.
		aCanvas newClipRect: r.
		(aCanvas drawString: string at: x@y font: font color: `Color veryDarkGray`) ifNotNil: [ :lastPoint |
			lastDisplayPosition _ lastPoint x.
			^r ]].
	^nil! !

!Transcript class methodsFor: 'system startup' stamp: 'jmv 8/21/2020 21:06:26'!
releaseClassCachedState
	displayCanvas _ nil! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 8/21/2020 20:13:23'!
display
	| c innerR |
	showOnDisplay ifTrue: [
		innerR _ bounds insetBy: self padding.
		c _ self canvas.
		c setClipRect: innerR.
		self displayOnCanvas: c in: bounds.
		DisplayScreen screenUpdateRequired: bounds ].
	"So any morph in front of us is repaired when Morphic cycles.
	This includes, for instance, the TranscriptWindow that shows our contents if showOnDisplay is false"
	self triggerEvent: #redraw! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 8/21/2020 20:13:32'!
displayUnfinishedEntry
	showOnDisplay ifTrue: [
		(self displayUnfinishedEntryOnCanvas: self canvas) ifNotNil: [ :damage |
			DisplayScreen screenUpdateRequired: damage ]]! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 8/21/2020 20:08:05'!
drawOn: aCanvas
	Transcript showOnDisplay: true.
	aCanvas clippingByCurrentMorphDo: [
		Transcript displayOnCanvas: aCanvas in: self morphLocalBounds ].
	self displayBounds ifNotNil: [ :r |
		Transcript bounds:r ].
	Transcript
		showOnDisplay: doImmediateUpdates.
	self updateWorkspace! !

!methodRemoval: TranscriptMorph #privateExtent: stamp: 'jmv 8/21/2020 21:00:19'!
TranscriptMorph removeSelector: #privateExtent:!
!methodRemoval: Transcript class #displayOn:in: stamp: 'jmv 8/21/2020 21:00:33'!
Transcript class removeSelector: #displayOn:in:!
!methodRemoval: Transcript class #displayUnfinishedEntryOn: stamp: 'jmv 8/21/2020 21:00:00'!
Transcript class removeSelector: #displayUnfinishedEntryOn:!
!classDefinition: 'Transcript class' category: #'System-Support'!
Transcript class
	instanceVariableNames: 'entries firstIndex lastIndex accessSemaphore unfinishedEntry logToFile showOnDisplay lastDisplayPosition bounds logToStdout displayCanvas'!

!Transcript class reorganize!
('preferred protocol' bounds: clear clearAll clearFile clearInternal clearStdout log: logToFile: logToStdout: logsToFile logsToStdout showOnDisplay:)
('old Transcript compatibility' cr endEntry flush newLine newLineWhen: nextPut: nextPut:when: nextPutAll: nextPutAll:when: print: print:when: show: space spaceWhen: store: tab tabWhen:)
('private' addEntry: addEntry:log: canvas contents contentsStartingAt: finishEntry stdout writeToFile: writeToStdout:)
('displaying' display displayOnCanvas: displayOnCanvas:in: displayUnfinishedEntry displayUnfinishedEntryOnCanvas: padding showsOnDisplay)
('class initialization' initialize)
('constants' filename maxEntries)
('iteration' entriesStartingAt:do:)
('accessing' lastIndex unfinishedEntrySize)
('system startup' releaseClassCachedState releaseClassState)
!

!classDefinition: #TranscriptMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #TranscriptMorph
	instanceVariableNames: 'workspace lastIncludedIndex workspaceWindow lastUnfinishedEntrySize doImmediateUpdates'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
