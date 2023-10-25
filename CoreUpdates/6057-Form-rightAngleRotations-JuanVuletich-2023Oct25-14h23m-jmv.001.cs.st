'From Cuis 6.0 [latest update: #6056] on 25 October 2023 at 2:31:52 pm'!

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 10/25/2023 14:30:52'!
rotated180Degrees
	"Return a copy of the receiver rotated by 180 degrees.
	ImageMorph defaultForm display.
	ImageMorph defaultForm rotated90Degrees display.
	ImageMorph defaultForm rotated180Degrees display.
	ImageMorph defaultForm rotated270Degrees display.
	"
	| newForm quad |
	newForm := self class extent: self extent depth: depth.
	quad := self boundingBox innerCorners.
	quad := #(3 4 1 2)		collect: [:i | quad at: i].
	(WarpBlt toForm: newForm)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: newForm);
		combinationRule: 3;
		copyQuad: quad toRect: newForm boundingBox.
	^ newForm! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 10/25/2023 14:30:39'!
rotated270Degrees
	"Return a copy of the receiver rotated by 270 degrees.
	ImageMorph defaultForm display.
	ImageMorph defaultForm rotated90Degrees display.
	ImageMorph defaultForm rotated180Degrees display.
	ImageMorph defaultForm rotated270Degrees display.
	"
	| newForm quad |
	newForm := self class extent: self extent transposed depth: depth.
	quad := self boundingBox innerCorners.
	quad := #(4 1 2 3)		collect: [:i | quad at: i].
	(WarpBlt toForm: newForm)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: newForm);
		combinationRule: 3;
		copyQuad: quad toRect: newForm boundingBox.
	^ newForm! !

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 10/25/2023 14:30:47'!
rotated90Degrees
	"Return a copy of the receiver rotated by 90 degrees.
	ImageMorph defaultForm display.
	ImageMorph defaultForm rotated90Degrees display.
	ImageMorph defaultForm rotated180Degrees display.
	ImageMorph defaultForm rotated270Degrees display.
	"
	| newForm quad |
	newForm := self class extent: self extent transposed depth: depth.
	quad := self boundingBox innerCorners.
	quad := #(2 3 4 1)		collect: [:i | quad at: i].
	(WarpBlt toForm: newForm)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: newForm);
		combinationRule: 3;
		copyQuad: quad toRect: newForm boundingBox.
	^ newForm! !


!Form methodsFor: 'scaling, rotation' stamp: 'jmv 10/25/2023 14:23:56'!
flippedBy: direction
	"Return a copy of the receiver flipped either #vertical, #horizontal or #both. (#both is a 180 degrees rotation)
	ImageMorph defaultForm display.
	(ImageMorph defaultForm flippedBy: #vertical) display.
	(ImageMorph defaultForm flippedBy: #horizontal) display.
	(ImageMorph defaultForm flippedBy: #both) display.
	"
	| newForm quad |
	newForm := self class extent: self extent depth: depth.
	quad := self boundingBox innerCorners.
	quad := (
		direction caseOf: {
			[ #vertical ] 		-> [#(2 1 4 3)].
			[ #horizontal ] 	-> [#(4 3 2 1)].
			[ #both ] 			-> [#(3 4 1 2)]})
		collect: [:i | quad at: i].
	(WarpBlt toForm: newForm)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: newForm);
		combinationRule: 3;
		copyQuad: quad toRect: newForm boundingBox.
"	newForm offset: (self offset flippedBy: direction centerAt: aPoint)."
	^ newForm
"
[Sensor isAnyButtonPressed] whileFalse:
	[((Form fromDisplay: (Sensor mousePoint extent: 130@66))
			flippedBy: #vertical centerAt: 0@0) display]
"
"Consistency test...
 | f f2 p |
[ Sensor isAnyButtonPressed ] whileFalse: [
	f _ Form fromDisplay: ((p _ Sensor mousePoint) extent: 31@41).
	Display fillBlack: (p extent: 31@41).
	f2 _ f flippedBy: #vertical centerAt: 0@0.
	(f2 flippedBy: #vertical centerAt: 0@0) displayAt: p ]
"! !

