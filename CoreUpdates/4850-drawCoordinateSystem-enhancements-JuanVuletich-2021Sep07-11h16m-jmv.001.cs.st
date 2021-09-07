'From Cuis 5.0 [latest update: #4849] on 7 September 2021 at 11:17:08 am'!

!Number methodsFor: 'truncation and round off' stamp: 'jmv 9/7/2021 10:54:01'!
round4perMagnitudeOrder
	"Round receiver to 1 or two significant digits.
	Answer is 1, 2, 2.5, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, etc.
	better name?"

	| excess firstDigitPosition |
	firstDigitPosition _ self log floor.
	excess _ self log - firstDigitPosition.
	excess < 2 log ifTrue: [ ^10 raisedTo: firstDigitPosition ].
	excess < 2.5 log ifTrue: [ ^(10 raisedTo: firstDigitPosition) * 2 ].
	excess < 5 log ifTrue: [ ^(10 raisedTo: firstDigitPosition-1) * 25 ].
	^(10 raisedTo: firstDigitPosition) * 5! !


!HaloMorph methodsFor: 'drawing' stamp: 'jmv 9/7/2021 11:13:25'!
drawCoordinateSystemOn: aCanvas

	| x0 x1 y0 y1 c stepX stepY haloTargetTx prevTx font strokeWidth tickLength stepXDecimals stepYDecimals |
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

	font _ FontFamily defaultFamilyPointSize: FontFamily defaultPointSize * 1.5 / haloTargetTx scale.
	stepX _ (font pointSize * 10) round4perMagnitudeOrder asFloat.
	stepXDecimals _ stepX log rounded negated + 1.
	stepY _ (font pointSize * 5) round4perMagnitudeOrder asFloat.
	stepYDecimals _ stepY log rounded negated + 1.
	strokeWidth _ 3/ haloTargetTx scale.
	tickLength _ 5 / haloTargetTx scale.

	prevTx _ aCanvas currentTransformation.
	aCanvas geometryTransformation: haloTargetTx.

	c _ `Color black alpha: 0.4`.
	aCanvas line: x0@0 to: x1@0 width: strokeWidth color: c.
	aCanvas line: 0@y0 to: 0@y1 width: strokeWidth color: c.

	(x0 truncateTo: stepX) to: x1-(stepX*0.2) by: stepX do: [ :x |
		aCanvas line: x @ tickLength negated to: x @ tickLength width: strokeWidth color: c.
	   	aCanvas drawString: (x printStringFractionDigits: stepXDecimals) atCenterXBaselineY: x @ (tickLength*2) negated font: font color: c ].
	  aCanvas drawString: 'x' atCenterX: x1 - (tickLength*3) @ 0 font: font color: c.

	(y0 truncateTo: stepY) to: y1-(stepY*0.5) by: stepY do: [ :y |
		aCanvas line: tickLength negated @ y to: tickLength @ y width: strokeWidth color: c.
	   	aCanvas drawString: (y printStringFractionDigits: stepYDecimals), ' ' atWaistRight: tickLength negated @ y font: font color: c ].
	  aCanvas drawString: 'y' atWaistRight: tickLength negated @ (y1 - (tickLength*4)) font: font color: c.

	aCanvas geometryTransformation: prevTx.! !


!WidgetMorph methodsFor: 'halos and balloon help' stamp: 'jmv 9/7/2021 09:26:23'!
haloShowsCoordinateSystem
	"We are usually not concerned with this level of detail for Widgets, as they prefer using automatic Layout."

	^self requiresVectorCanvas! !

