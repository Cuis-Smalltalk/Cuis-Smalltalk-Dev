'From Cuis 5.0 [latest update: #4906] on 8 October 2021 at 8:12:40 pm'!
!classDefinition: 'Transcript class' category: #'System-Support'!
Transcript class
	instanceVariableNames: 'entries firstIndex lastIndex accessSemaphore unfinishedEntry logToFile showOnDisplay lastDisplayPosition bounds logToStdout displayCanvas displayEngine '!

!Transcript class methodsFor: 'private' stamp: 'jmv 10/8/2021 19:49:14'!
displayEngine

	(displayEngine isNil or: [
		displayEngine class ~= BitBltCanvasEngine]) ifTrue: [
			displayEngine _ BitBltCanvasEngine toForm: Display ].
	^ displayEngine! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 10/8/2021 20:10:59'!
displayOnDisplay
	"To be called directly, not from Morphic.
	See #displayOnCanvas:in:"
	"
	1 to: 2000 do: [ :i | i print ].
	1 to: 750 do: [ :i | i print ].
	Transcript displayOnDisplay.
	Display forceToScreen
	"
	| innerR |
	self displayEngine.
	innerR _ bounds insetBy: self padding.
	displayEngine clipRect: innerR.
	displayEngine
		copy: innerR
		from: `0@0` in: nil
		fillColor: `Color white` rule: Form over.
	self displayTextOn: self in: innerR.! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 10/8/2021 20:07:43'!
displayTextOn: aCanvasOrSelf in: aRectangle
	"See senders"
	| font count string x y fh innerR index |
	font _ FontFamily defaultFamilyAndPointSize.
	innerR _ aRectangle insetBy: self padding.
	x _ innerR left.
	y _ innerR top.
	fh _ font lineSpacing.
	count _ lastIndex - firstIndex \\ self maxEntries + 1 min: innerR height // fh - 1.
	index _ lastIndex - count \\ self maxEntries + 1.
	count timesRepeat: [
		string _ entries at: index.	
		aCanvasOrSelf drawString: string at: x@y font: font color: `Color veryDarkGray`.
		y _ y + fh.
		index _ index \\ self maxEntries + 1 ].
	string _ unfinishedEntry contents.
	aCanvasOrSelf drawString: string at: x@y font: font color: `Color veryDarkGray`! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 10/8/2021 20:00:14'!
displayUnfinishedEntryOnDisplay

	| font count string x y fh r innerR |
	self displayEngine.
	innerR _ bounds insetBy: self padding.
	lastDisplayPosition < innerR right ifTrue: [
		font _ FontFamily defaultFamilyAndPointSize.
		fh _ font lineSpacing.
		count _ innerR height // fh-1.
		x _ innerR left.
		string _ unfinishedEntry contents.
		y _ ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font lineSpacing + innerR top.
		r _ innerR left: lastDisplayPosition.
		displayEngine clipRect: r.
		(self drawString: string at: x@y font: font color: `Color veryDarkGray`) ifNotNil: [ :lastPoint |
			lastDisplayPosition _ lastPoint x.
			^r ]].
	^nil! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 10/8/2021 19:59:46'!
drawString: s at: pt font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| p1 |
	p1 _ pt rounded.
	p1 _ p1 + (0@(0 - (font ascent + font lineGap-1))).
	p1 _ p1 rounded.
	displayEngine colorMap: nil.
	^font
		onBitBltCanvasEngine: displayEngine
		displayString: s
		from: 1
		to: s size
		at: p1
		color: aColor! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 10/8/2021 19:53:38'!
display
	showOnDisplay ifTrue: [
		self displayOnDisplay.
		DisplayScreen screenUpdateRequired: bounds ].
	"So any morph in front of us is repaired when Morphic cycles.
	This includes, for instance, the TranscriptWindow that shows our contents if showOnDisplay is false"
	self triggerEvent: #redraw! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 10/8/2021 19:57:35'!
displayUnfinishedEntry
	showOnDisplay ifTrue: [
		self displayUnfinishedEntryOnDisplay ifNotNil: [ :damage |
			DisplayScreen screenUpdateRequired: damage ]]! !

!Transcript class methodsFor: 'system startup' stamp: 'jmv 10/8/2021 19:48:11'!
releaseClassCachedState
	displayEngine _ nil! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 10/8/2021 20:10:01'!
drawOn: aCanvas
	"If we don't call super, clipping fails if zoomed / rotated, and nothing is shown."
	super drawOn: aCanvas.
	aCanvas clippingByCurrentMorphDo: [
		aCanvas
			fillRectangle: self morphLocalBounds
			color: `Color white`.
		Transcript displayTextOn: aCanvas in: self morphLocalBounds ].
	self displayBounds ifNotNil: [ :r |
		Transcript bounds:r ].
	self updateWorkspace! !

!methodRemoval: Transcript class #canvas stamp: 'jmv 10/8/2021 20:05:31'!
Transcript class removeSelector: #canvas!
!methodRemoval: Transcript class #displayUnfinishedEntryOnCanvas: stamp: 'jmv 10/8/2021 20:03:38'!
Transcript class removeSelector: #displayUnfinishedEntryOnCanvas:!
!methodRemoval: Transcript class #displayOnCanvas:in: stamp: 'jmv 10/8/2021 20:10:14'!
Transcript class removeSelector: #displayOnCanvas:in:!
!classDefinition: 'Transcript class' category: #'System-Support'!
Transcript class
	instanceVariableNames: 'entries firstIndex lastIndex accessSemaphore unfinishedEntry logToFile showOnDisplay lastDisplayPosition bounds logToStdout displayEngine'!
