'From Cuis 4.1 of 12 December 2012 [latest update: #1673] on 14 April 2013 at 9:54:33 pm'!

!FormCanvas methodsFor: 'drawing-rectangles' stamp: 'jmv 4/12/2013 20:21'!
fillRectangle: aRectangle tilingWith: aForm multipliedBy: aColor
	"Fill aRectangle with the equivalent of aForm multiplied by aColor
	aForm is a kind of advanced stencil, supplying brightness and opacity at each pixel
	Similar to #image:multipliedBy:at:


	Display getCanvas fillRectangle: (10@10 extent: 100@100) tilingWith: (FormCanvas verticalGrayGradient: 30 gradientTop: 0.8 gradientBottom: 0.5) multipliedBy: Color red.
	"

	AccessProtect critical: [
		self buildAuxWith: aForm multipliedWith: aColor.
		"Warning: aForm boundingBox is most likely different from AuxForm boundingBox!!"
		self fillRectangle: aRectangle tilingWith: AuxForm sourceRect: aForm boundingBox rule: Form paint ]! !

!FormCanvas methodsFor: 'private' stamp: 'jmv 4/12/2013 20:22'!
fillRectangle: aRectangle tilingWith: aForm sourceRect: patternBox rule: aCombinationRule
	"aRectangle is in form coordinates, no transformation is done."
	| displayRectangle  additionalOffset clippedPort targetTopLeft clipOffset ex 
	targetBox savedMap top left |
	
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.

	"this is a bit of a kludge to get the form to be aligned where I *think* it should be.
	something better is needed, but not now"

	ex _ patternBox extent.
	additionalOffset _ 0@0.
	clippedPort _ port clippedBy: displayRectangle.
	targetTopLeft _ clippedPort clipRect topLeft truncateTo: ex.
	clipOffset _ displayRectangle topLeft - targetTopLeft.
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


!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 4/12/2013 20:35'!
drawSelectionOn: aCanvas
	| rightX leftX bottom |

	bottom _ self baseFont height.
	leftX _ self fontToUse widthOfString: contents from: 1 to: editor startIndex-1.
	leftX _ leftX min: extent x.
	rightX _ self fontToUse widthOfString: contents from: 1 to: editor stopIndex-1.
	rightX _ rightX min: extent x.

	aCanvas
		fillRectangle: (leftX @ 0 corner: rightX @ bottom)
		color: (Theme current textHighlightFocused: self hasKeyboardFocus)! !

