'From Cuis 4.1 of 12 December 2012 [latest update: #1634] on 20 March 2013 at 10:56:44 am'!

!Pen methodsFor: 'operations' stamp: 'jmv 3/20/2013 10:50'!
fill: drawBlock color: color

	"Draw whatever the drawBlocks does, but also paint fill with color.
	Do it properly in Morphic 3"

	"
	| region tileForm tilePen shape saveColor recorder |
	drawBlock value: (recorder _ self as: PenPointRecorder).
	region _ Rectangle encompassing: recorder points.
	tileForm _ Form extent: region extent+6.
	tilePen _ Pen newOnForm: tileForm.
	tilePen location: location-(region origin-3)
		direction: direction
		penDown: penDown.
	drawBlock value: tilePen.  'Draw the shape in B/W'.
	saveColor _ halftoneForm.
	drawBlock value: self.
	halftoneForm _ saveColor.
	shape _ (tileForm findShapeAroundSeedBlock: [:f | f borderWidth: 1]) reverse.
	shape copy: shape boundingBox from: tileForm to: 0@0 rule: Form erase.
	destForm fillShape: shape fillColor: color at: region origin-3
	"

	self color: color.
	drawBlock value: self! !

