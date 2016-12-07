'From Cuis 5.0 of 7 November 2016 [latest update: #2986] on 2 December 2016 at 4:40:51 pm'!

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 12/2/2016 15:47:08'!
flippedBy: direction
	"Return a copy of the receiver flipped either #vertical, #horizontal or #both. (#both is a 180 degrees rotation)
	Form lena display.
	(Form lena flippedBy: #vertical) display.
	(Form lena flippedBy: #horizontal) display.
	(Form lena flippedBy: #both) display.
	"
	| newForm quad |
	newForm _ self class extent: self extent depth: depth.
	quad _ self boundingBox innerCorners.
	quad _ (
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

