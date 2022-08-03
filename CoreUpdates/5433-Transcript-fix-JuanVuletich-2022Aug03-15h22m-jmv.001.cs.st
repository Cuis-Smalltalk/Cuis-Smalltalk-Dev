'From Cuis 6.0 [latest update: #5433] on 3 August 2022 at 3:27:52 pm'!

!Transcript class methodsFor: 'displaying' stamp: 'jmv 8/3/2022 15:27:21'!
displayOnDisplay
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
		fill: bounds fillColor: `Color veryVeryLightGray` rule: Form over.
	self displayTextOn: self in: bounds.! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 8/3/2022 15:20:20'!
drawString: s at: pt font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	displayEngine colorMap: nil.
	^font
		onBitBltCanvasEngine: displayEngine
		displayString: s
		from: 1
		to: s size
		at: pt roundedHAFZ
		color: aColor! !


!TranscriptMorph methodsFor: 'drawing' stamp: 'jmv 8/3/2022 15:26:50'!
drawOn: aCanvas
	"If we don't call super, clipping fails if zoomed / rotated, and nothing is shown."
	super drawOn: aCanvas.
	aCanvas clippingByCurrentMorphDo: [
		aCanvas
			fillRectangle: self morphLocalBounds
			color: color.
		Transcript displayTextOn: aCanvas in: self morphLocalBounds ].
	Transcript bounds: (self externalizeBoundsToWorld: self morphLocalBounds)
				encompassingIntegerRectangle.
	self updateWorkspace! !

