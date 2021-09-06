'From Cuis 5.0 [latest update: #4839] on 6 September 2021 at 10:24:41 am'!

!HaloMorph methodsFor: 'drawing' stamp: 'jmv 9/6/2021 10:21:10'!
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
	stepX _ FontFamily defaultPointSize * 4 //10 * 10.
	stepY _ FontFamily defaultPointSize * 2 //10 * 10.

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

