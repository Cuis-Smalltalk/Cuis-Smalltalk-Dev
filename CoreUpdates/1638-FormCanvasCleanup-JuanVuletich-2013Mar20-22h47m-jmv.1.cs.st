'From Cuis 4.1 of 12 December 2012 [latest update: #1636] on 20 March 2013 at 10:56:32 pm'!

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 3/20/2013 22:54'!
fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor
	"Fill aRectangle with the equivalent of aForm multiplied by aColor
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel
	Similar to #image:multipliedBy:at:


	Display getCanvas fillRectangle: (10@10 extent: 100@100) tilingWith: (FormCanvas verticalGrayGradient: 30 gradientTop: 0.8 gradientBottom: 0.5) multipliedBy: Color red.
	"

	| displayRectangle |
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.
	AccessProtect critical: [
		self buildAuxWith: aForm multipliedWith: aColor.
		self fillRectangle: displayRectangle tilingWith: AuxForm sourceRect: aForm boundingBox rule: Form paint ]! !


!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 3/20/2013 22:56'!
frameRectangle: r color: aColor borderWidth: borderWidth borderStyleSymbol: aSymbol
	"
	Display getCanvas fillRectangle: (10@10 extent: 300@200) color: Color white.
	Display getCanvas
		frameRectangle: (10@10 extent: 300@200)
		color: Color green
		borderWidth: 2
		borderStyleSymbol: #raised.
	"

	| displayRectangle bw |
		bw _ (currentTransformation externalizeScalar: borderWidth) rounded.
	aSymbol == #raised ifTrue: [
		displayRectangle _ currentTransformation displayBoundsOfTransformOf: r.
		^ self
			frameRectangle: displayRectangle
			borderWidth: bw
			topLeftColor: aColor quiteWhiter
			bottomRightColor: aColor quiteBlacker ].

	aSymbol == #inset ifTrue: [
		displayRectangle _ currentTransformation displayBoundsOfTransformOf: r.
		^ self
			frameRectangle: displayRectangle
			borderWidth: bw
			topLeftColor: aColor quiteBlacker
			bottomRightColor: aColor quiteWhiter ].
	
	"Unrecognized border style. Draw some border..."
	self frameRectangle: r borderWidth: bw color: aColor! !

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 3/20/2013 22:52'!
roundRect: displayRectangle color: aColor radius: r gradientTop: topFactor gradientBottom: bottomFactor gradientHeight: h
	"
	Display restore.
	FormCanvas clearFormsCache. 
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10 gradientTop: 1.0 gradientBottom: 0.5 gradientHeight: 35
	"
	| bottomColor |
	"top stripe"
	self
		image: (FormCanvas topLeftCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topLeft.
	self
		image: (FormCanvas topRightCorner: r height: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: displayRectangle topRight - (r@0).
	self
		fillRectangle: ((displayRectangle withHeight: h) insetBy: r@0)
		tilingWith: (FormCanvas verticalGrayGradient: h gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.

	bottomColor _ aColor * bottomFactor.
	"center stripe"
	self fillRectangle: (displayRectangle insetBy: (0 @ h corner: 0 @ r)) color: bottomColor.
	
	"bottom stripe"
	self
		image: (FormCanvas bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomLeft - (0@r).
	self
		image: (FormCanvas bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: displayRectangle bottomRight - (r@r) .
	self fillRectangle: ((displayRectangle bottomLeft + (r@r negated)) extent: (displayRectangle width - r - r@r)) color: bottomColor! !

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 3/20/2013 22:52'!
roundRect: aRectangle color: aColor radius: r gradientTop: topFactor gradientCenter: centerFactor gradientBottom: bottomFactor gradient1Height: h1
	"
	Display restore.
	FormCanvas clearFormsCache. 
	Display getCanvas roundRect: (10@10 extent: 200@100) color: Color red radius: 10 gradientTop: 1.0 gradientCenter: 0.0 gradientBottom: 1.0 gradient1Height: 35
	"
	| h2 |
	"top stripe"
	self
		image: (FormCanvas topLeftCorner: r height: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (FormCanvas topRightCorner: r height: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self
		fillRectangle: ((aRectangle withHeight: h1) insetBy: r@0)
		tilingWith: (FormCanvas verticalGrayGradient: h1 gradientTop: topFactor gradientBottom: centerFactor)
		multipliedBy: aColor.
	
	"bottom stripe"
	h2 _ aRectangle height - h1.
	self
		image: (FormCanvas bottomLeftCorner: r height: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topLeft + (0@h1).
	self
		image: (FormCanvas bottomRightCorner: r height: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topRight + (r negated@h1).
	self
		fillRectangle: ((aRectangle topLeft + (r@h1)) extent: (aRectangle width-r-r@h2))
		tilingWith: (FormCanvas verticalGrayGradient: h2 gradientTop: centerFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.! !

!FormCanvas methodsFor: 'drawing-roundedRectangles' stamp: 'jmv 3/20/2013 22:52'!
windowFrame: aRectangle color: aColor radius: r border: bw labelHeight: lh gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor
	"
	Display getCanvas windowFrame: (10@10 extent: 200@100) color: Color red radius: 10  border: 5 labelHeight: 25 gradientTop: 1.0 gradientBottom: 0.5 insideColor: Color green
	"
	"top stripe"
	| bottomColor he tl tr |
	self
		image: (FormCanvas topLeftCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (FormCanvas topRightCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self
		fillRectangle: ((aRectangle withHeight: lh) insetBy: r@0)
		tilingWith: (FormCanvas verticalGrayGradient: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.

	bottomColor _ aColor * bottomFactor.

	"left and right borders"
	tl _ aRectangle topLeft + (0@lh).
	tr _ aRectangle topRight + (bw negated@lh).
	he _ bw@(aRectangle height - lh - r).
	self fillRectangle: (tl extent: he) color: bottomColor.
	self fillRectangle: (tr extent: he) color: bottomColor.
	
	"bottom stripe"
	self
		image: (FormCanvas bottomLeftCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: aRectangle bottomLeft - (0@r).
	self
		image: (FormCanvas bottomRightCorner: r height: r gradientTop: 1 gradientBottom: 1)
		multipliedBy: bottomColor
		at: aRectangle bottomRight - (r@r) .
	self fillRectangle: ((aRectangle bottomLeft + (r@bw negated)) extent: (aRectangle width - r - r@bw)) color: bottomColor.

	"inside"
	self fillRectangle: (aRectangle insetBy: (bw@lh corner: bw@bw)) color: insideColor! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 3/20/2013 22:55'!
fillRectangle: rInPortTerms tilingWith: aForm sourceRect: patternBox rule: aCombinationRule
	"aRectangle is in form coordinates, no transformation is done."
	| additionalOffset clippedPort targetTopLeft clipOffset ex 
	targetBox savedMap top left |

	"this is a bit of a kludge to get the form to be aligned where I *think* it should be.
	something better is needed, but not now"

	ex _ patternBox extent.
	additionalOffset _ 0@0.
	clippedPort _ port clippedBy: rInPortTerms.
	targetTopLeft _ clippedPort clipRect topLeft truncateTo: ex.
	clipOffset _ rInPortTerms topLeft - targetTopLeft.
	additionalOffset _ (clipOffset \\ ex) - ex.

	"do it iteratively"
	targetBox _ clippedPort clipRect.
	savedMap _ clippedPort colorMap.
	clippedPort sourceForm: aForm;
		fillColor: nil;
		combinationRule: aCombinationRule;
		sourceRect: patternBox;
		colorMap: (aForm colormapIfNeededFor: clippedPort destForm).
	top _ (targetBox top truncateTo: patternBox height) + additionalOffset y.
	left _  (targetBox left truncateTo: patternBox width) + additionalOffset x.

	left to: (targetBox right - 1) by: patternBox width do: [:x |
		top to: (targetBox bottom - 1) by: patternBox height do: [:y |
			clippedPort destOrigin: x@y; copyBits]].
	clippedPort colorMap: savedMap! !

!methodRemoval: FormCanvas #fillRectangle:infiniteForm:multipliedBy:!
FormCanvas removeSelector: #fillRectangle:infiniteForm:multipliedBy:!
