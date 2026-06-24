'From Cuis7.9 [latest update: #8016] on 24 June 2026 at 2:01:56 pm'!

!Form methodsFor: 'scaling, rotation' stamp: 'jmv 24/Jun/2026, 4:59:14 pm (UTC)'!
flipBy: direction on: newForm
	"Similar to #flippedBy:, but work on an existing newForm, assumed to be of the same extent as us."
	| quad |
	quad := self boundingBox innerCorners.
	quad := (
		direction caseOf: {
			[ #vertical ] 		-> [#(2 1 4 3)].
			[ #horizontal ] 		-> [#(4 3 2 1)].
			[ #both ] 			-> [#(3 4 1 2)]})
		collect: [:i | quad at: i].
	(WarpBlt toForm: newForm)
		sourceForm: self;
		colorMap: (self colormapIfNeededFor: newForm);
		combinationRule: 3;
		copyQuad: quad toRect: newForm boundingBox.! !


!ColorForm methodsFor: 'scaling, rotation' stamp: 'jmv 24/Jun/2026, 4:59:10 pm (UTC)'!
flipBy: direction on: newForm
	| oldColors |
	oldColors := colors.
	self colors: nil.
	super flippedBy: direction.
	self colors: oldColors.
	newForm colors: oldColors.! !

