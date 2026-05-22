Class {
	#name : 'VectorEngineSmalltalk',
	#superclass : 'VectorEngineDrawer',
	#instVars : [
		'spanLeft',
		'spanTop',
		'spanRight',
		'spanBottom',
		'prevYTruncated',
		'prevYRounded',
		'leftAtThisY',
		'rightAtThisY',
		'aawScaledInverse',
		'swDilatedHalf',
		'swDilatedHalfSquared',
		'swErodedHalfSquared',
		'trajectoryLength',
		'needsFullAlphaCircle'
	],
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'accessing' }
VectorEngineSmalltalk >> antiAliasingWidth: aNumber [

	super antiAliasingWidth: aNumber.
	aawScaledInverse := 127.0 / antiAliasingWidth.
]

{ #category : 'primitive algorithms' }
VectorEngineSmalltalk >> arcCenterX: cx centerY: cy radiusX: rx radiusY: ry start: startAngle sweep: sweepAngle rotationCos: thetaCos rotationSin: thetaSin [

	| dx dy trx try tcx tcy center angle xp yp x y d hops |
	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	
	dx := geometryTransformation externalizeDelta: rx@0.
	dy := geometryTransformation externalizeDelta: 0@ry.
	trx := dx r.
	try := dy r.
	hops := ((trx max: try) * sweepAngle abs / hop) truncated + 2.
	d := hops asFloat.
	center := geometryTransformation externalizePosition: cx@cy.
	tcx := center x.
	tcy := center y.
	0 to: hops do: [ :h |
		angle := h / d * sweepAngle + startAngle.
		xp := angle cos * trx.
		yp := angle sin * try.
		x := thetaCos * xp - (thetaSin * yp) + tcx.
		y := thetaSin * xp + (thetaCos * yp) + tcy.
		spanLeft := spanLeft min: x.
		spanTop := spanTop min: y.
		spanRight := spanRight max: x.
		spanBottom := spanBottom max: y.
		self updateAlphasForX: x y: y.
		fillColor notNil ifTrue: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y. ].
]

{ #category : 'private - blend with mask' }
VectorEngineSmalltalk >> blendFillOnly [

	self subclassResponsibility.
]

{ #category : 'private - blend with mask' }
VectorEngineSmalltalk >> blendStrokeAndFill [

	self subclassResponsibility.
]

{ #category : 'private - blend with mask' }
VectorEngineSmalltalk >> blendStrokeOnly [

	self subclassResponsibility.
]

{ #category : 'primitive algorithms' }
VectorEngineSmalltalk >> cubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2 [

	| p txFrom tyFrom txTo tyTo txControl1 tyControl1 txControl2 tyControl2 dx dy xMinEnd xMaxEnd yMinEnd yMaxEnd t oneLessT increment f2 f3 f1 f23 f4 x y t0 x0 y0 length correction |
	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	
	p := geometryTransformation externalizePosition: xFrom@yFrom. txFrom := p x. tyFrom := p y.
	p := geometryTransformation externalizePosition: xTo@yTo. txTo := p x. tyTo := p y.
	p := geometryTransformation externalizePosition: xControl1@yControl1. txControl1 := p x. tyControl1 := p y.
	p := geometryTransformation externalizePosition: xControl2@yControl2. txControl2 := p x. tyControl2 := p y.

	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
	xMinEnd := txFrom min: txTo.
	xMaxEnd := txFrom max: txTo.
	yMinEnd := tyFrom min: tyTo.
	yMaxEnd := tyFrom max: tyTo.
	spanLeft := spanLeft min: (xMinEnd min: xMinEnd*0.25 + ((txControl1 min: txControl2)*0.75)).
	spanRight := spanRight max: (xMaxEnd max: xMaxEnd*0.25 + ((txControl1 max: txControl2)*0.75)).
	spanTop := spanTop min: (yMinEnd min: yMinEnd*0.25 + ((tyControl1 min: tyControl2)*0.75)).
	spanBottom := spanBottom max: (yMaxEnd max: yMaxEnd*0.25 + ((tyControl1 max: tyControl2)*0.75)).

	"Case t = 0.0"
	x := txFrom.
	y := tyFrom.
	self updateAlphasForX: x y: y.
	fillColor notNil ifTrue: [
		self updateEdgeCountAtX: x y: y ].
	self updateContourForX: x y: y.

	dx := (txTo-txFrom) abs.
	dy := (tyTo-tyFrom) abs.
	
	"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
	increment := 0.5 / (dx max: dy) min: 0.5.
	t := 0.0.
	[
		t0 := t. x0 := x. y0 := y.
		"Compute next point"
		t := t0 + increment. oneLessT := 1.0 - t.
		f1 := oneLessT * oneLessT * oneLessT. f23 := 3.0 * oneLessT * t. f2 := f23 * oneLessT. f3 := f23 * t. f4 := t * t * t.
		x := (f1 * txFrom) + (f2 * txControl1) + (f3 * txControl2) + (f4 * txTo).
		y := (f1 * tyFrom) + (f2 * tyControl1) + (f3 * tyControl2) + (f4 * tyTo).
		"Now adjust the increment to aim at the required hop length, and recompute next point."
		length := ((x-x0)@(y-y0)) r.
		correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
		[
			increment := increment * correction.
			t := t0 + increment. oneLessT := 1.0 - t.
			f1 := oneLessT * oneLessT * oneLessT. f23 := 3.0 * oneLessT * t. f2 := f23 * oneLessT. f3 := f23 * t. f4 := t * t * t.
			x := (f1 * txFrom) + (f2 * txControl1) + (f3 * txControl2) + (f4 * txTo).
			y := (f1 * tyFrom) + (f2 * tyControl1) + (f3 * tyControl2) + (f4 * tyTo).
			length := ((x-x0)@(y-y0)) r.
			correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
			correction < 0.99]  whileTrue.		   "Keep adjusting if actual hop would be too big"
		t < 1.0 ]
	whileTrue: [
		self updateAlphasForX: x y: y.
		fillColor notNil ifTrue: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y ].

	"Case t= 1.0"
	self updateAlphasForX: txTo y: tyTo.
	fillColor notNil ifTrue: [
		self updateEdgeCountAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.
]

{ #category : 'private' }
VectorEngineSmalltalk >> finishPath: ensureClosePath [
	"Blend stroke and/or fill over background, according to the just computed pen alphas (distance from pixel to center of stroke)"
	
	"If we are doing a fill, and caller forgot to close the trajectory, then do it anyway."
	ensureClosePath ifTrue: [
		firstX isNaN ifFalse: [
			(firstX = currentX and: [ firstY = currentY ]) ifFalse: [
				"This is actually an error condition. If we are asked to fill, then the path should be closed.
				This is a workaround. If width > 0.0, we'll be adding a visible edge to the path."
				self lineToFirstPoint ]]].
	self updateContourLastLine.

	spanLeft := (spanLeft - swDilatedHalf - self subPixelDelta + 1) truncated.																	"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	spanTop := (spanTop - swDilatedHalf + 1) truncated.																	"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	spanRight := (spanRight + swDilatedHalf + self subPixelDelta) truncated + 1. 								"Make room not just for updated mask, but also edges (hence, +1)"
	spanBottom := (spanBottom + swDilatedHalf) truncated.

	"morphBounds is not affected by clipRect"
	morphBoundsLeft := morphBoundsLeft min: spanLeft.
	morphBoundsTop := morphBoundsTop min: spanTop.
	morphBoundsRight := morphBoundsRight max: spanRight.
	morphBoundsBottom := morphBoundsBottom max: spanBottom.

	spanLeft< clipLeft ifTrue: [ spanLeft := clipLeft ].
	spanTop < clipTop ifTrue: [ spanTop := clipTop ].
	spanRight > clipRight ifTrue: [ spanRight := clipRight ].
	spanBottom > clipBottom ifTrue: [ spanBottom := clipBottom ].

	(spanRight >= spanLeft and: [ spanBottom >= spanTop ]) ifTrue: [
		fillColor notNil ifTrue: [
			strokeColor notNil
				ifTrue: [ self blendStrokeAndFill ]
				ifFalse: [ self blendFillOnly ]]
			ifFalse: [
				self blendStrokeOnly ]].
]

{ #category : 'private' }
VectorEngineSmalltalk >> initializePath [

	super initializePath.

	spanTop := targetHeight.				"drawable bottom. Will later be refined."
	spanBottom := 0.					"drawable top. Will later be refined."
	spanLeft := targetWidth.				"drawable right. Will later be refined."
	spanRight := 0.						"drawable left. Will later be refined."
	prevYRounded := 16r3FFFFFFF.
]

{ #category : 'primitive algorithms' }
VectorEngineSmalltalk >> lineFromX: xFrom y: yFrom toX: xTo y: yTo [

	| p txFrom tyFrom txTo tyTo dx dy hops t oneLessT increment x y |
	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	
	p := geometryTransformation externalizePosition: xFrom@yFrom. txFrom := p x. tyFrom := p y.
	p := geometryTransformation externalizePosition: xTo@yTo. txTo := p x. tyTo := p y.
	dx := (txTo - txFrom) abs.
	dy := (tyTo - tyFrom) abs.
	hops := ((dx max: dy) / hop) truncated + 1.

	spanLeft := spanLeft min: (txFrom min: txTo).
	spanRight := spanRight max: (txFrom max: txTo).
	spanTop := spanTop min: (tyFrom min: tyTo).
	spanBottom := spanBottom max: (tyFrom max: tyTo).

	t := 0.0.
	increment := 1.0 / hops.
	[ t < 1.0 ] whileTrue: [
		oneLessT := 1.0 - t.
		x := (oneLessT * txFrom) + (t * txTo).
		y := (oneLessT * tyFrom) + (t * tyTo).
		self updateAlphasForX: x y: y.
		fillColor notNil ifTrue: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y.
		t := t + increment. ].
	self updateAlphasForX: txTo y: tyTo.
	fillColor notNil ifTrue: [
		self updateEdgeCountAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.
]

{ #category : 'private' }
VectorEngineSmalltalk >> newTrajectoryFragment [

	prevYTruncated := 16r3FFFFFFF.
]

{ #category : 'private' }
VectorEngineSmalltalk >> pvtFillColor: aFillColorOrNil [
	fillColor := aFillColorOrNil
]

{ #category : 'private' }
VectorEngineSmalltalk >> pvtStrokeColor: aStrokeColorOrNil [
	strokeColor := aStrokeColorOrNil
]

{ #category : 'private' }
VectorEngineSmalltalk >> pvtStrokeWidth: aNumber hop: aFloat [

	| swErodedHalf |
	strokeWidth := aNumber.
	hop := aFloat.
	swDilatedHalf := strokeWidth + antiAliasingWidth * 0.5.
	swDilatedHalfSquared := swDilatedHalf * swDilatedHalf.
	"swErodedHalf is the inner radious of the alphas ring.
	See #updateAlphasForX:y:"
	swErodedHalf := strokeWidth - antiAliasingWidth * 0.5 -hop - 2.5.
	swErodedHalfSquared := swErodedHalf * swErodedHalf abs.
]

{ #category : 'primitive algorithms' }
VectorEngineSmalltalk >> quadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl [

	| p txFrom tyFrom txTo tyTo txControl tyControl dx dy dx2 dy2 xMinEnd xMaxEnd yMinEnd yMaxEnd t oneLessT increment f2 f3 f1 x y t0 x0 y0 length correction |

	"If control point is bogus, just draw a line"
	(xControl = xTo and: [ yControl = yTo ]) ifTrue: [
		^self lineFromX: xFrom y: yFrom toX: xTo y: yTo ].
	(xControl = xFrom and: [ yControl = yFrom ]) ifTrue: [
		^self lineFromX: xFrom y: yFrom toX: xTo y: yTo ].
	
	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.

	p := geometryTransformation externalizePosition: xFrom@yFrom. txFrom := p x. tyFrom := p y.
	p := geometryTransformation externalizePosition: xTo@yTo. txTo := p x. tyTo := p y.
	p := geometryTransformation externalizePosition: xControl@yControl. txControl := p x. tyControl := p y.

	dx := (txTo-txFrom) abs.
	dx2 := (txControl-txFrom) abs.
	dy := (tyTo-tyFrom) abs.
	dy2 := (tyControl-tyFrom) abs.

	"If almost a vertical line, just draw a line. (Ignoring control point)"
	(dx < 1.0 and: [dx2 < 1.0]) ifTrue: [
		^self lineFromX: xFrom y: yFrom toX: xTo y: yTo ].

	"If almost an horizontal line, just draw a line. (Ignoring control point)"
	(dy < 1.0 and: [dy2 < 1.0]) ifTrue: [
		^self lineFromX: xFrom y: yFrom toX: xTo y: yTo ].

	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
	xMinEnd := txFrom min: txTo.
	xMaxEnd := txFrom max: txTo.
	yMinEnd := tyFrom min: tyTo.
	yMaxEnd := tyFrom max: tyTo.
	spanLeft := spanLeft min: (xMinEnd min: (xMinEnd+txControl) / 2.0).
	spanRight := spanRight max: (xMaxEnd max: (xMaxEnd+txControl) / 2.0).
	spanTop := spanTop min: (yMinEnd min: (yMinEnd+tyControl) / 2.0).
	spanBottom := spanBottom max: (yMaxEnd max: (yMaxEnd+tyControl) / 2.0).

	"Case t = 0.0"
	x := txFrom.
	y := tyFrom.
	self updateAlphasForX: x y: y.
	fillColor notNil ifTrue: [
		self updateEdgeCountAtX: x y: y ].
	self updateContourForX: x y: y.

	"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
	increment := 0.5 / (dx max: dy) min: 0.5.
	t := 0.0.
	[
		t0 := t. x0 := x. y0 := y.
		"Compute next point"
		t := t0 + increment. oneLessT := 1.0 - t.
		f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
		x := (f1 * txFrom) + (f2 * txControl) + (f3 * txTo).
		y := (f1 * tyFrom) + (f2 * tyControl) + (f3 * tyTo).
		"Now adjust the increment to aim at the required hop length, and recompute next point."
		length := ((x-x0)@(y-y0)) r.
		correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
		[
			increment := increment * correction.
			t := t0 + increment. oneLessT := 1.0 - t.
			f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
			x := (f1 * txFrom) + (f2 * txControl) + (f3 * txTo).
			y := (f1 * tyFrom) + (f2 * tyControl) + (f3 * tyTo).
			length := ((x-x0)@(y-y0)) r.
			correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
			correction < 0.99]  whileTrue.		   "Keep adjusting if actual hop would be too big"
		t < 1.0 ]
	whileTrue: [
		self updateAlphasForX: x y: y.
		fillColor notNil ifTrue: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y ].

	"Case t= 1.0"
	self updateAlphasForX: txTo y: tyTo.
	fillColor notNil ifTrue: [
		self updateEdgeCountAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.
]

{ #category : 'private' }
VectorEngineSmalltalk >> resetMorphBounds [

	leftAtThisY := targetWidth.
	rightAtThisY := 0.
	"Clear any possible contour."
	morphBoundsTop ifNotNil: [
		(morphBoundsTop max: 0) to: (morphBoundsBottom min: targetHeight -1) do: [ :y |
			contour at: y * 2 + 1 put: targetWidth.
			contour at: y * 2 + 2 put: 0 ]].
	super resetMorphBounds.
]

{ #category : 'primitive algorithms - aux' }
VectorEngineSmalltalk >> updateContourForX: x y: y [
	"The Contour of the current morph is the pixel area affected, but without holes.
	For each y line where a pixel is affected, record the leftmost and rightmost affected pixels.
	Note: Only includes pen trajectory, but not stroke width. Ignores any clipping."

	| thisYRounded |
	thisYRounded := y rounded.
	(thisYRounded between: 0 and: targetHeight-1) ifTrue: [
		thisYRounded = prevYRounded ifFalse: [
			prevYRounded = 16r3FFFFFFF ifFalse: [
				contour at: prevYRounded * 2 + 1 put: leftAtThisY.
				contour at: prevYRounded * 2 + 2 put: rightAtThisY ].
			leftAtThisY := contour at: thisYRounded * 2 + 1.
			rightAtThisY := contour at: thisYRounded * 2 + 2.
			prevYRounded := thisYRounded ].
		leftAtThisY := leftAtThisY min: x.
		rightAtThisY := rightAtThisY max: x ].
]

{ #category : 'private' }
VectorEngineSmalltalk >> updateContourLastLine [

	prevYRounded = 16r3FFFFFFF ifFalse: [
		contour at: prevYRounded * 2 + 1 put: leftAtThisY.
		contour at: prevYRounded * 2 + 2 put: rightAtThisY ].
]
