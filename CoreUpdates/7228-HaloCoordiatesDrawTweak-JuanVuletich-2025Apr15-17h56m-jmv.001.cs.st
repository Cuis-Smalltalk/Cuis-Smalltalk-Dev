'From Cuis7.3 [latest update: #7137] on 15 April 2025 at 5:57:32 pm'!

!HaloMorph methodsFor: 'drawing' stamp: 'jmv 4/15/2025 17:57:12'!
drawCoordinateSystemOn: aCanvas

	| x0 x1 y0 y1 c stepX stepY haloTargetTx prevTx font strokeWidth tickLength stepXDecimals stepYDecimals |
	haloTargetTx := MorphicTranslation identity.
	target allOwnersReverseDo: [ :o | haloTargetTx := haloTargetTx composedWith: o location ].
	haloTargetTx := haloTargetTx composedWith: target location.

	target knowsOwnLocalBounds
		ifTrue: [ | r |
			r := target localBounds.
			x0 := r left.
			x1 := r right.
			y0 := r top.
			y1 := r bottom ]
		ifFalse: [
			x0 := x1 := y0 := y1 := 0.
			target displayFullBounds corners collect: [ :pt | | p |
				p := haloTargetTx inverseTransform: pt.
				x0 := x0 min: p x.
				x1 := x1 max: p x.
				y0 := y0 min: p y.
				y1 := y1 max: p y.]].

	font := FontFamily defaultFamilyPointSize: (FontFamily defaultPointSize * 1.5 / haloTargetTx scale) rounded.
	stepX := (font pointSize * 9) round4perMagnitudeOrder asFloat.
	stepXDecimals := stepX log rounded negated + 1.
	stepY := (font pointSize * 5) round4perMagnitudeOrder asFloat.
	stepYDecimals := stepY log rounded negated + 1.
	strokeWidth := 3/ haloTargetTx scale.
	tickLength := 5 / haloTargetTx scale.

	prevTx := aCanvas currentTransformation.
	aCanvas geometryTransformation: haloTargetTx.

	c := `Color black alpha: 0.4`.
	aCanvas line: x0@0 to: x1@0 width: strokeWidth color: c.
	aCanvas line: 0@y0 to: 0@y1 width: strokeWidth color: c.

	(x0 truncateTo: stepX) to: x1 by: stepX do: [ :x |
		aCanvas line: x @ tickLength negated to: x @ tickLength width: strokeWidth color: c.
		aCanvas drawString: (x printStringFractionDigits: stepXDecimals) atWaistCenter: x @ (tickLength*4) negated font: font color: c ].
	  aCanvas drawString: 'x' atCenterX: x1 - (tickLength*3) @ 0 font: font color: c.

	(y0 truncateTo: stepY) to: y1 by: stepY do: [ :y |
		aCanvas line: tickLength negated @ y to: tickLength @ y width: strokeWidth color: c.
		aCanvas drawString: (y printStringFractionDigits: stepYDecimals), ' ' atWaistRight: tickLength negated @ y font: font color: c ].
	  aCanvas drawString: 'y' atWaist: tickLength @ (y1 - (tickLength*4)) font: font color: c.

	aCanvas geometryTransformation: prevTx.! !

