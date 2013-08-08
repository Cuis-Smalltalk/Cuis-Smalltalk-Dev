'From Cuis 4.2 of 25 July 2013 [latest update: #1782] on 3 August 2013 at 3:23:51.989483 pm'!

!FormCanvas methodsFor: 'private' stamp: 'jmv 8/3/2013 15:23'!
fillRectangle: aRectangle tilingWith: aForm sourceRect: patternBox rule: aCombinationRule
	"aRectangle is in form coordinates, no transformation is done."
	| displayRectangle  additionalOffset clippedPort targetTopLeft clipOffset ex 
	targetBox savedMap top left |
	
	ex _ patternBox extent.
	ex x = 0 ifTrue: [ ^self ].
	ex y = 0 ifTrue: [ ^self ].
	displayRectangle _ currentTransformation displayBoundsOfTransformOf: aRectangle.

	"this is a bit of a kludge to get the form to be aligned where I *think* it should be.
	something better is needed, but not now"

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

