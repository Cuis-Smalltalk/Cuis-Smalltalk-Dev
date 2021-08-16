'From Cuis 5.0 [latest update: #4754] on 13 August 2021 at 5:35:54 pm'!

!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/13/2021 17:13:59'!
updateCurrentBounds
	"Update display bounds, for currentMorph only."

	currentMorph displayBoundsSetFrom: self.! !


!Morph methodsFor: 'updating' stamp: 'jmv 8/13/2021 16:54:28'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn.
	Usually because we changed in some way."

	self needsRedraw: true.
	self allOwnersDo: [ :m | m submorphNeedsRedraw: true ].
	self invalidateDisplayRect: (self displayBoundsOrBogus outsetBy: 1) for: self.	  		"Due to anti aliasing in VectorCanvas"
	(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifTrue: [
		self submorphsReverseDo: [ :m | m redrawNeeded ]].! !


!HaloMorph methodsFor: 'drawing' stamp: 'jmv 8/13/2021 17:29:23'!
drawCoordinateSystemOn: aCanvas

	| x0 x1 y0 y1 c stepX stepY haloTargetTx prevTx |
	haloTargetTx _ MorphicTranslation identity.
	target allOwnersReverseDo: [ :o | haloTargetTx _ haloTargetTx composedWith: o location ].
	haloTargetTx _ haloTargetTx composedWith: target location.

	target knowsOwnLocalBounds
		ifTrue: [ | r |
			r _ target morphLocalBounds.
			x0 _ r left.
			x1 _ r right.
			y0 _ r top.
			y1 _ r bottom ]
		ifFalse: [
			x0 _ x1 _ y0 _ y1 _ 0.
			target displayFullBounds corners collect: [ :pt | | p |
				p _ haloTargetTx inverseTransform: pt.
				x0 _ x0 min: p x.
				x1 _ x1 max: p x.
				y0 _ y0 min: p y.
				y1 _ y1 max: p y.]].
	x1-x0 < 1000 ifTrue: [
		stepX _ 50.
		stepY _ 20 ]
	ifFalse: [
		stepX _ 100.
		stepY _ 50 ].

	prevTx _ aCanvas currentTransformation.
	aCanvas geometryTransformation: haloTargetTx.

	c _ `Color black alpha: 0.4`.
	aCanvas line: x0@0 to: x1@0 width: 2 color: c.
	aCanvas line: 0@y0 to: 0@y1 width: 2 color: c.

	(x0 truncateTo: stepX) to: (x1 - stepX truncateTo: stepX) by: stepX do: [ :x |
		aCanvas line: x @ -5 to: x @ 5 width: 2 color: c.
	   	aCanvas drawString: x printString atCenterXBaselineY: x @ -10 font: nil color: c ].
	  aCanvas drawString: 'x' atCenterX: x1 - 15 @ 0 font: nil color: c.

	(y0 truncateTo: stepY) to: (y1 - stepY truncateTo: stepY) by: stepY do: [ :y |
		aCanvas line: -5 @ y to: 5 @ y width: 2 color: c.
	   	aCanvas drawString: y printString, ' ' atWaistRight: -5 @ y font: nil color: c ].
	  aCanvas drawString: 'y' atWaistRight: -5 @ (y1 - 20) font: nil color: c.

	aCanvas geometryTransformation: prevTx.! !

!HaloMorph methodsFor: 'drawing' stamp: 'jmv 8/13/2021 17:29:49'!
drawOn: aCanvas 

	| b |
	target ifNil: [^super drawOn: aCanvas].
	"Debugging aid"
	"aCanvas
		fillRectangle: haloBox
		color: (Color red alpha: 0.2).
	aCanvas
		fillRectangle: self morphLocalBounds
		color: (Color gray alpha: 0.4)."
	Preferences halosShowCoordinateSystem ifTrue: [
		target displayFullBounds ifNotNil: [ :fb |
			aCanvas frameGlobalRect: fb borderWidth: 3 color: `Color black alpha: 0.1`.
			b _ target displayBounds.
			b = fb ifFalse: [
				aCanvas frameGlobalRect: b borderWidth: 3 color: `Color black alpha: 0.1` ].
			self drawCoordinateSystemOn: aCanvas ]].! !


!BitBltBoundsFinderCanvas methodsFor: 'morphic' stamp: 'jmv 8/13/2021 16:35:14'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	BitBltCanvas doesn't keep track of Morph bounds as it draws them. But it is limited to drawing morphs where
	#morphLocalBounds is known. Use it, then."

	^self boundingRectOfCurrentMorphFromLocalBounds! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 8/13/2021 16:35:23'!
boundingRectOfCurrentMorphAfterDraw
	"In targetForm coordinates.
	Clipped to owner if appropriate.
	BitBltCanvas doesn't keep track of Morph bounds as it draws them. But it is limited to drawing morphs where
	#morphLocalBounds is known. Use it, then."

	^self boundingRectOfCurrentMorphFromLocalBounds! !

!methodRemoval: MorphicCanvas #updateCurrentBounds stamp: 'jmv 8/13/2021 17:11:25'!
MorphicCanvas removeSelector: #updateCurrentBounds!
