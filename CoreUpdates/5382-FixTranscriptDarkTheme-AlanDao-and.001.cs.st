'From Cuis 6.0 [latest update: #5305] on 7 July 2022 at 10:45:13 pm'!

!TranscriptMorph methodsFor: 'drawing' stamp: 'and 7/5/2022 00:19:48'!
adoptWidgetsColor: paneColor
	color = self defaultColor ifTrue: [
		color _ Theme current paneBackgroundFrom: paneColor.
	].
	super adoptWidgetsColor: paneColor.! !


!Transcript class methodsFor: 'displaying' stamp: 'and 7/5/2022 00:20:31'!
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
		aCanvasOrSelf drawString: string at: x@y font: font color: Theme current text.
		y _ y + fh.
		index _ index \\ self maxEntries + 1 ].
	string _ unfinishedEntry contents.
	aCanvasOrSelf drawString: string at: x@y font: font color: Theme current text! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'and 7/5/2022 00:18:37'!
drawOn: aCanvas
	"If we don't call super, clipping fails if zoomed / rotated, and nothing is shown."
	super drawOn: aCanvas.
	aCanvas clippingByCurrentMorphDo: [
		aCanvas
			fillRectangle: self morphLocalBounds
			color: color.
		Transcript displayTextOn: aCanvas in: self morphLocalBounds ].
	self displayBounds ifNotNil: [ :r |
		Transcript bounds:r ].
	self updateWorkspace! !


!TranscriptMorph reorganize!
('initialization' defaultBorderWidth initialize)
('drawing' adoptWidgetsColor: drawOn:)
('event handling testing' handlesMouseDown:)
('menus' getMenu mouseButton2Activity)
('menu commands' clearAll clearFile clearInternal clearStdout doImmediateUpdates doRegularUpdates dontLogToFile dontLogToStdout editContents logToFile logToStdout)
('updating' updateWorkspace)
!

