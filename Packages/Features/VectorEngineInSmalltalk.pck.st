'From Cuis 6.0 [latest update: #5043] on 17 January 2022 at 1:11:07 pm'!
'Description '!
!provides: 'VectorEngineInSmalltalk' 1 1!
!requires: 'VectorGraphics' 1 333!
SystemOrganization addCategory: 'VectorEngineInSmalltalk'!


!classDefinition: #VectorEngineSmalltalk category: 'VectorEngineInSmalltalk'!
VectorEngineDrawer subclass: #VectorEngineSmalltalk
	instanceVariableNames: 'spanLeft spanTop spanRight spanBottom prevYTruncated prevYRounded leftAtThisY rightAtThisY aawScaledInverse swDilatedHalf swDilatedHalfSquared swErodedHalfSquared'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorEngineInSmalltalk'!
!classDefinition: 'VectorEngineSmalltalk class' category: 'VectorEngineInSmalltalk'!
VectorEngineSmalltalk class
	instanceVariableNames: ''!

!classDefinition: #VectorEngineSubPixel category: 'VectorEngineInSmalltalk'!
VectorEngineSmalltalk subclass: #VectorEngineSubPixel
	instanceVariableNames: 'subPixelDelta'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorEngineInSmalltalk'!
!classDefinition: 'VectorEngineSubPixel class' category: 'VectorEngineInSmalltalk'!
VectorEngineSubPixel class
	instanceVariableNames: ''!

!classDefinition: #VectorEngineWholePixel category: 'VectorEngineInSmalltalk'!
VectorEngineSmalltalk subclass: #VectorEngineWholePixel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorEngineInSmalltalk'!
!classDefinition: 'VectorEngineWholePixel class' category: 'VectorEngineInSmalltalk'!
VectorEngineWholePixel class
	instanceVariableNames: ''!


!VectorEngineSubPixel commentStamp: '<historical>' prior: 0!
Does sub pixel antialiasing, both for text and graphics. Appropriate for low to medium resolution displays, where subpixel antialiasing improves quality. Consumes more CPU than VectorEngineWholePixel.
Variables edgeCounts and alphaMask are 4 bytes per pixel (3 actually used).!

!VectorEngineWholePixel commentStamp: '<historical>' prior: 0!
Does only whole pixel antialiasing. Appropriate for very high resolution displays, or slower systems where the extra computational load of VectorEngineSubPixel is not affordable.
Variables edgeCounts and alphaMask are single byte per pixel.!

!VectorEngineSmalltalk methodsFor: 'accessing' stamp: 'jmv 8/10/2021 12:04:36'!
antiAliasingWidth: aNumber

	super antiAliasingWidth: aNumber.
	aawScaledInverse _ 127.0 / antiAliasingWidth.! !

!VectorEngineSmalltalk methodsFor: 'accessing' stamp: 'jmv 8/10/2021 12:07:18'!
clipCurrentMorph: aBoolean
	(clipCurrentMorph and: [ aBoolean not ]) ifTrue: [
		| pixelIndex |
		clipTop to: clipBottom do: [ :displayY |
			pixelIndex _ displayY * targetWidth + clipLeft.
			clipLeft to: clipRight do: [ :displayX |
				pixelIndex _ pixelIndex + 1.
				morphIds at: pixelIndex put: ((morphIds at: pixelIndex) >>8 << 8) ]]].
	clipCurrentMorph _ aBoolean! !

!VectorEngineSmalltalk methodsFor: 'primitive algorithms' stamp: 'jmv 9/15/2021 14:47:40'!
arcCenterX: cx centerY: cy radiusX: rx radiusY: ry start: startAngle sweep: sweepAngle rotationCos: thetaCos rotationSin: thetaSin

	| dx dy trx try tcx tcy center angle xp yp x y d hops |
	dx _ geometryTransformation externalizeDelta: rx@0.
	dy _ geometryTransformation externalizeDelta: 0@ry.
	trx _ dx r.
	try _ dy r.
	hops _ ((trx max: try) * sweepAngle abs / hop) truncated + 2.
	d _ hops asFloat.
	center _ geometryTransformation externalizePosition: cx@cy.
	tcx _ center x.
	tcy _ center y.
	0 to: hops do: [ :h |
		angle _ h / d * sweepAngle + startAngle.
		xp _ angle cos * trx.
		yp _ angle sin * try.
		x _ thetaCos * xp - (thetaSin * yp) + tcx.
		y _ thetaSin * xp + (thetaCos * yp) + tcy.
		spanLeft _ spanLeft min: x.
		spanTop _ spanTop min: y.
		spanRight _ spanRight max: x.
		spanBottom _ spanBottom max: y.
		self updateAlphasForX: x y: y.
		fillColor notNil ifTrue: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y. ].! !

!VectorEngineSmalltalk methodsFor: 'primitive algorithms' stamp: 'jmv 10/12/2021 09:59:10'!
cubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2

	| p txFrom tyFrom txTo tyTo txControl1 tyControl1 txControl2 tyControl2 dx dy hops xMinEnd xMaxEnd yMinEnd yMaxEnd t oneLessT increment f2 f3 f1 f23 f4 x y |
	p _ geometryTransformation externalizePosition: xFrom@yFrom. txFrom _ p x. tyFrom _ p y.
	p _ geometryTransformation externalizePosition: xTo@yTo. txTo _ p x. tyTo _ p y.
	p _ geometryTransformation externalizePosition: xControl1@yControl1. txControl1 _ p x. tyControl1 _ p y.
	p _ geometryTransformation externalizePosition: xControl2@yControl2. txControl2 _ p x. tyControl2 _ p y.
	"Factors found by trial and error. Seems to honor hopsPerPixel in the worst case."
	dx _ ((txControl1-txFrom) abs max: (txTo-txControl2) abs)*3 max: (txControl2-txControl1) abs*1.5.
	dy _ ((tyControl1-tyFrom) abs max: (tyTo-tyControl2) abs)*3 max: (tyControl2-tyControl1) abs*1.5.
	hops _ ((dx max: dy) / hop) truncated + 1.

	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
	xMinEnd _ txFrom min: txTo.
	xMaxEnd _ txFrom max: txTo.
	yMinEnd _ tyFrom min: tyTo.
	yMaxEnd _ tyFrom max: tyTo.
	spanLeft _ spanLeft min: (xMinEnd min: xMinEnd*0.25 + ((txControl1 min: txControl2)*0.75)).
	spanRight _ spanRight max: (xMaxEnd max: xMaxEnd*0.25 + ((txControl1 max: txControl2)*0.75)).
	spanTop _ spanTop min: (yMinEnd min: yMinEnd*0.25 + ((tyControl1 min: tyControl2)*0.75)).
	spanBottom _ spanBottom max: (yMaxEnd max: yMaxEnd*0.25 + ((tyControl1 max: tyControl2)*0.75)).

	t _ 0.0.
	increment _ 1.0 / hops.
	[ t < 1.0 ] whileTrue: [
		oneLessT _ 1.0 - t.
		f1 _ oneLessT * oneLessT * oneLessT.
		f23 _ 3.0 * oneLessT * t.
		f2 _ f23 * oneLessT.
		f3 _ f23 * t.
		f4 _ t * t * t.
		x _ (f1 * txFrom) + (f2 * txControl1) + (f3 * txControl2) + (f4 * txTo).
		y _ (f1 * tyFrom) + (f2 * tyControl1) + (f3 * tyControl2) + (f4 * tyTo).
		self updateAlphasForX: x y: y.
		fillColor notNil ifTrue: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y.
		t _ t + increment. ].
	self updateAlphasForX: txTo y: tyTo.
	fillColor notNil ifTrue: [
		self updateEdgeCountAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEngineSmalltalk methodsFor: 'primitive algorithms' stamp: 'jmv 10/12/2021 10:16:19'!
lineFromX: xFrom y: yFrom toX: xTo y: yTo

	| p txFrom tyFrom txTo tyTo dx dy hops t oneLessT increment x y |
	p _ geometryTransformation externalizePosition: xFrom@yFrom. txFrom _ p x. tyFrom _ p y.
	p _ geometryTransformation externalizePosition: xTo@yTo. txTo _ p x. tyTo _ p y.
	dx _ (txTo - txFrom) abs.
	dy _ (tyTo - tyFrom) abs.
	hops _ ((dx max: dy) / hop) truncated + 1.

	spanLeft _ spanLeft min: (txFrom min: txTo).
	spanRight _ spanRight max: (txFrom max: txTo).
	spanTop _ spanTop min: (tyFrom min: tyTo).
	spanBottom _ spanBottom max: (tyFrom max: tyTo).

	t _ 0.0.
	increment _ 1.0 / hops.
	[ t < 1.0 ] whileTrue: [
		oneLessT _ 1.0 - t.
		x _ (oneLessT * txFrom) + (t * txTo).
		y _ (oneLessT * tyFrom) + (t * tyTo).
		self updateAlphasForX: x y: y.
		fillColor notNil ifTrue: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y.
		t _ t + increment. ].
	self updateAlphasForX: txTo y: tyTo.
	fillColor notNil ifTrue: [
		self updateEdgeCountAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEngineSmalltalk methodsFor: 'primitive algorithms' stamp: 'jmv 8/10/2021 12:06:37'!
quadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl

	| p txFrom tyFrom txTo tyTo txControl tyControl dx dy dx2 dy2 xMinEnd xMaxEnd yMinEnd yMaxEnd t oneLessT increment f2 f3 f1 x y t0 x0 y0 length correction |

	"If control point is bogus, just draw a line"
	(xControl = xTo and: [ yControl = yTo ]) ifTrue: [
		^self lineFromX: xFrom y: yFrom toX: xTo y: yTo ].
	(xControl = xFrom and: [ yControl = yFrom ]) ifTrue: [
		^self lineFromX: xFrom y: yFrom toX: xTo y: yTo ].

	p _ geometryTransformation externalizePosition: xFrom@yFrom. txFrom _ p x. tyFrom _ p y.
	p _ geometryTransformation externalizePosition: xTo@yTo. txTo _ p x. tyTo _ p y.
	p _ geometryTransformation externalizePosition: xControl@yControl. txControl _ p x. tyControl _ p y.

	dx _ (txTo-txFrom) abs.
	dx2 _ (txControl-txFrom) abs.
	dy _ (tyTo-tyFrom) abs.
	dy2 _ (tyControl-tyFrom) abs.

	"If almost a vertical line, just draw a line. (Ignoring control point)"
	(dx < 1.0 and: [dx2 < 1.0]) ifTrue: [
		^self lineFromX: xFrom y: yFrom toX: xTo y: yTo ].

	"If almost an horizontal line, just draw a line. (Ignoring control point)"
	(dy < 1.0 and: [dy2 < 1.0]) ifTrue: [
		^self lineFromX: xFrom y: yFrom toX: xTo y: yTo ].

	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
	xMinEnd _ txFrom min: txTo.
	xMaxEnd _ txFrom max: txTo.
	yMinEnd _ tyFrom min: tyTo.
	yMaxEnd _ tyFrom max: tyTo.
	spanLeft _ spanLeft min: (xMinEnd min: (xMinEnd+txControl) / 2.0).
	spanRight _ spanRight max: (xMaxEnd max: (xMaxEnd+txControl) / 2.0).
	spanTop _ spanTop min: (yMinEnd min: (yMinEnd+tyControl) / 2.0).
	spanBottom _ spanBottom max: (yMaxEnd max: (yMaxEnd+tyControl) / 2.0).

	"Case t = 0.0"
	x _ txFrom.
	y _ tyFrom.
	self updateAlphasForX: x y: y.
	fillColor notNil ifTrue: [
		self updateEdgeCountAtX: x y: y ].
	self updateContourForX: x y: y.

	"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
	increment _ 0.5 / (dx max: dy) min: 0.5.
	t _ 0.0.
	[
		t0 _ t. x0 _ x. y0 _ y.
		"Compute next point"
		t _ t0 + increment. oneLessT _ 1.0 - t.
		f1 _ oneLessT * oneLessT. f2 _ 2.0 * oneLessT * t. f3 _ t * t.
		x _ (f1 * txFrom) + (f2 * txControl) + (f3 * txTo).
		y _ (f1 * tyFrom) + (f2 * tyControl) + (f3 * tyTo).
		"Now adjust the increment to aim at the required hop length, and recompute next point."
		length _ ((x-x0)@(y-y0)) r.
		correction _ hop / length.
		[
			increment _ increment * correction.
			t _ t0 + increment. oneLessT _ 1.0 - t.
			f1 _ oneLessT * oneLessT. f2 _ 2.0 * oneLessT * t. f3 _ t * t.
			x _ (f1 * txFrom) + (f2 * txControl) + (f3 * txTo).
			y _ (f1 * tyFrom) + (f2 * tyControl) + (f3 * tyTo).
			length _ ((x-x0)@(y-y0)) r.
			correction _ hop / length.
			correction < 1.0]  whileTrue.		   "Keep adjusting if hop would be too big"
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
	self updateContourForX: txTo y: tyTo.! !

!VectorEngineSmalltalk methodsFor: 'primitive algorithms - aux' stamp: 'jmv 8/30/2021 16:35:46'!
updateContourForX: x y: y
	"The Contour of the current morph is the pixel area affected, 	but without holes.
	For each y line where a pixel is affected, record the leftmost and rightmost affected pixels.
	Note: Only includes pen trajectory, but not stroke width."

	| thisYRounded |
	thisYRounded _ y rounded.
	(thisYRounded between: 0 and: targetHeight-1) ifTrue: [
		thisYRounded = prevYRounded ifFalse: [
			prevYRounded = 16r3FFFFFFF ifFalse: [
				contour at: prevYRounded * 2 + 1 put: leftAtThisY.
				contour at: prevYRounded * 2 + 2 put: rightAtThisY ].
			leftAtThisY _ contour at: thisYRounded * 2 + 1.
			rightAtThisY _ contour at: thisYRounded * 2 + 2.
			prevYRounded _ thisYRounded ].
		leftAtThisY _ leftAtThisY min: x.
		rightAtThisY _ rightAtThisY max: x ].! !

!VectorEngineSmalltalk methodsFor: 'private' stamp: 'jmv 8/10/2021 12:07:01'!
finishPath: ensureClosePath
	"Blend stroke and/or fill over background, according to the just computed pen alphas (distance from pixel to center of stroke)"
	
	"If we are doing a fill, and caller forgot to close the trajectory, then do it anyway."
	ensureClosePath ifTrue: [
		firstX isNaN ifFalse: [
			(firstX = currentX and: [ firstY = currentY ]) ifFalse: [
				"This is actually an error condition. If we are asked to fill, then the path should be closed.
				This is a workaround. If width > 0.0, we'll be adding a visible edge to the path."
				self lineToFirstPoint ]]].
	self updateContourLastLine.

	spanLeft _ (spanLeft - swDilatedHalf - self subPixelDelta + 1) truncated.																	"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	spanTop _ (spanTop - swDilatedHalf + 1) truncated.																	"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	spanRight _ (spanRight + swDilatedHalf + self subPixelDelta) truncated + 1. 								"Make room not just for updated mask, but also edges (hence, +1)"
	spanBottom _ (spanBottom + swDilatedHalf) truncated.

	"morphBounds is not affected by clipRect"
	morphBoundsLeft _ morphBoundsLeft min: spanLeft.
	morphBoundsTop _ morphBoundsTop min: spanTop.
	morphBoundsRight _ morphBoundsRight max: spanRight.
	morphBoundsBottom _ morphBoundsBottom max: spanBottom.

	spanLeft< clipLeft ifTrue: [ spanLeft _ clipLeft ].
	spanTop < clipTop ifTrue: [ spanTop _ clipTop ].
	spanRight > clipRight ifTrue: [ spanRight _ clipRight ].
	spanBottom > clipBottom ifTrue: [ spanBottom _ clipBottom ].

	(spanRight >= spanLeft and: [ spanBottom >= spanTop ]) ifTrue: [
		fillColor notNil ifTrue: [
			strokeColor notNil
				ifTrue: [ self blendStrokeAndFill ]
				ifFalse: [ self blendFillOnly ]]
			ifFalse: [
				self blendStrokeOnly ]].! !

!VectorEngineSmalltalk methodsFor: 'private' stamp: 'jmv 8/30/2021 16:35:40'!
initializePath

	super initializePath.

	spanTop _ targetHeight.						"drawable bottom. Will later be refined."
	spanBottom _ 0.						"drawable top. Will later be refined."
	spanLeft _ targetWidth.						"drawable right. Will later be refined."
	spanRight _ 0.						"drawable left. Will later be refined."
	prevYRounded _ 16r3FFFFFFF.! !

!VectorEngineSmalltalk methodsFor: 'private' stamp: 'jmv 8/30/2021 16:35:43'!
newTrajectoryFragment

	prevYTruncated _ 16r3FFFFFFF.! !

!VectorEngineSmalltalk methodsFor: 'private' stamp: 'jmv 8/10/2021 12:06:37'!
pvtFillColor: aFillColorOrNil
	fillColor _ aFillColorOrNil! !

!VectorEngineSmalltalk methodsFor: 'private' stamp: 'jmv 8/10/2021 12:06:31'!
pvtStrokeColor: aStrokeColorOrNil
	strokeColor _ aStrokeColorOrNil! !

!VectorEngineSmalltalk methodsFor: 'private' stamp: 'jmv 8/10/2021 12:05:07'!
pvtStrokeWidth: aNumber

	| swErodedHalf |
	super pvtStrokeWidth: aNumber.
	swDilatedHalf _ strokeWidth + antiAliasingWidth * 0.5.
	swDilatedHalfSquared _ swDilatedHalf * swDilatedHalf.
	swErodedHalf _ strokeWidth - antiAliasingWidth * 0.5.
	swErodedHalfSquared _ swErodedHalf * swErodedHalf abs.! !

!VectorEngineSmalltalk methodsFor: 'private' stamp: 'jmv 8/10/2021 12:07:46'!
resetMorphBounds

	leftAtThisY _ targetWidth.
	rightAtThisY _ 0.
	"Clear any possible contour."
	morphBoundsTop ifNotNil: [
		(morphBoundsTop max: 0) to: (morphBoundsBottom min: targetHeight -1) do: [ :y |
			contour at: y * 2 + 1 put: targetWidth.
			contour at: y * 2 + 2 put: 0 ]].
	super resetMorphBounds.! !

!VectorEngineSmalltalk methodsFor: 'private' stamp: 'jmv 8/30/2021 16:35:49'!
updateContourLastLine

	prevYRounded = 16r3FFFFFFF ifFalse: [
		contour at: prevYRounded * 2 + 1 put: leftAtThisY.
		contour at: prevYRounded * 2 + 2 put: rightAtThisY ].! !

!VectorEngineSmalltalk methodsFor: 'private - blend with mask' stamp: 'jmv 8/8/2021 23:12:07'!
blendFillOnly

	self subclassResponsibility.! !

!VectorEngineSmalltalk methodsFor: 'private - blend with mask' stamp: 'jmv 8/8/2021 23:12:02'!
blendStrokeAndFill

	self subclassResponsibility.! !

!VectorEngineSmalltalk methodsFor: 'private - blend with mask' stamp: 'jmv 8/8/2021 23:12:12'!
blendStrokeOnly

	self subclassResponsibility.! !

!VectorEngineSubPixel methodsFor: 'accessing' stamp: 'jmv 6/18/2021 15:28:29'!
disableSubPixelSampling
	subPixelDelta _ 0.0! !

!VectorEngineSubPixel methodsFor: 'accessing' stamp: 'jmv 3/22/2021 14:30:40'!
subPixelDelta
	^subPixelDelta! !

!VectorEngineSubPixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 8/10/2021 12:07:01'!
updateAlphasForX: x y: y
	| distanceToAxisSquared alpha t b l r alphaWord doUpdate dySquared dx dxSquared dy pixelIndex dxp |

	"Compute affected rect. Honor clipRect"
	t _ (y - swDilatedHalf + 1) truncated.													"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	t < clipTop ifTrue: [ t _ clipTop ].
	b _ (y + swDilatedHalf) truncated.
	b > clipBottom ifTrue: [ b _ clipBottom ].
	l _ (x - swDilatedHalf - subPixelDelta + 1) truncated.												"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	l < clipLeft ifTrue: [ l _ clipLeft ].
	r _ (x + swDilatedHalf + subPixelDelta) truncated.
	r > clipRight ifTrue: [ r _ clipRight ].

	t to: b do: [ :displayY |
		pixelIndex _ displayY * targetWidth + l.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			alphaWord _ alphaMask bytesAt: pixelIndex.
			alphaWord = "16r007F7F7F" #[16r7F 16r7F 16r7F 16r00] ifFalse: [
				doUpdate _ false.
				dy _ displayY - y.
				dySquared _ dy * dy.
				dx _ displayX - x.
				"Red"
				dxp _ dx - subPixelDelta.
				dxSquared _ dxp * dxp.
				distanceToAxisSquared _ dxSquared + dySquared.
				distanceToAxisSquared < swDilatedHalfSquared
					ifTrue: [
						alpha _ distanceToAxisSquared <= swErodedHalfSquared
							ifTrue: [ 16r7F ]
							ifFalse: [ (swDilatedHalf - distanceToAxisSquared sqrt * aawScaledInverse) truncated ].
						alpha > (alphaWord at: 3) ifTrue: [
							alphaWord at: 3 put: alpha.
							doUpdate _ true ]].
				"Green"
				dxSquared _ dx * dx.
				distanceToAxisSquared _ dxSquared + dySquared.
				distanceToAxisSquared < swDilatedHalfSquared
					ifTrue: [
						alpha _ distanceToAxisSquared <= swErodedHalfSquared
							ifTrue: [ 16r7F ]
							ifFalse: [ (swDilatedHalf - distanceToAxisSquared sqrt * aawScaledInverse) truncated ].
						alpha > (alphaWord at: 2) ifTrue: [
							alphaWord at: 2 put: alpha.
							doUpdate _ true ]].
				"Blue"
				dxp _ dx + subPixelDelta.
				dxSquared _ dxp * dxp.
				distanceToAxisSquared _ dxSquared + dySquared.
				distanceToAxisSquared < swDilatedHalfSquared
					ifTrue: [
						alpha _ distanceToAxisSquared <= swErodedHalfSquared
							ifTrue: [ 16r7F ]
							ifFalse: [ (swDilatedHalf - distanceToAxisSquared sqrt * aawScaledInverse) truncated ].
						alpha > (alphaWord at: 1) ifTrue: [
							alphaWord at: 1 put: alpha.
							doUpdate _ true ]].
				doUpdate ifTrue: [
					alphaMask bytesAt: pixelIndex put: alphaWord ]]]]! !

!VectorEngineSubPixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 8/30/2021 16:35:51'!
updateEdgeCountAtX: x y: y
	"Compute edges intersecting with this horizontal line, for fills.
		
	See https://en.wikipedia.org/wiki/Nonzero-rule .

Where it says 'pick a ray in any direction', we pick an horizontal ray pointing to the left. This means that for any point (actually for every pixel) p1=(x1, y1), we record any crossings of y1, i.e. any sequence of path points (... pp1, pp2, .....) where the jump from pp1 to pp2 crosses y1, and pp1.x and pp2.x are at the left of x1. If the path is going up, it is clockwwise. If it is going down, it is counter clockwise.

As an additional optimization, as we need to store for every pixel all the crossings to the left, what we actually store are the crossings at each pixel (i.e. within 1 pixel distance to the left). Then, when we use them, the algorithms iterates from left to right, and accumulates the counters from previous pixels.
	"

	| thisYTruncated pixelY pixelIndexBase redPixelIndex greenPixelIndex bluePixelIndex increment countWord redCount greenCount blueCount redOffset greenOffset blueOffset |

	thisYTruncated _ y truncated.
	thisYTruncated = prevYTruncated ifTrue: [
		^ self].
	(thisYTruncated between: clipTop-1 and: clipBottom) ifFalse: [
		^ self ].
	prevYTruncated = 16r3FFFFFFF ifTrue: [ 
		^ prevYTruncated _ thisYTruncated ].

	thisYTruncated > prevYTruncated
		ifTrue: [
			pixelY _ thisYTruncated.
			increment _ 1 ]
		ifFalse: [
			pixelY _ prevYTruncated.
			increment _ -1 ].
	prevYTruncated _ thisYTruncated.

	"All edge count at the left of the clipRect are added there (at the left of the clipRect).
	The effect is the same, and we need to clean up less stuff afterwards.
	More important, it avoids trying to acess pixels outside our form, i.e. invalid array acesses."
	pixelIndexBase _ pixelY * targetWidth.
	redOffset _ (x+subPixelDelta+1) truncated max: clipLeft.			"take the next red subpixel center to the right of x"
	greenOffset _ (x+1) truncated max: clipLeft.											"take the next green subpixel center to the right of x"
	blueOffset _ (x-subPixelDelta+1) truncated max: clipLeft.		"take the next blue subpixel center to the right of x"
	redPixelIndex _ pixelIndexBase + redOffset +1.
	greenPixelIndex _ pixelIndexBase + greenOffset +1.
	bluePixelIndex _ pixelIndexBase + blueOffset +1.

	"Three possible cases here: RGB in one word (pixel); RG in one, and G in another; R in one, GB in another"
	redPixelIndex = bluePixelIndex
		ifTrue: [						"First case: RGB in the same word"
			redOffset <= clipRight ifTrue: [
				countWord _ edgeCounts bytesAt: redPixelIndex.
				redCount _ (countWord at: 3) + increment bitAnd: 16rFF.
				countWord at: 3 put: redCount.
				greenCount _ (countWord at: 2) + increment bitAnd: 16rFF.
				countWord at: 2 put: greenCount.
				blueCount _ (countWord at: 1) + increment bitAnd: 16rFF.
				countWord at: 1 put: blueCount.
				edgeCounts bytesAt: redPixelIndex put: countWord ].
		]
		ifFalse: [
			redPixelIndex = greenPixelIndex
				ifTrue: [				"Second case: RG in one word, B in previous"
					redOffset <= clipRight ifTrue: [
						countWord _ edgeCounts bytesAt: redPixelIndex.
						redCount _ (countWord at: 3) + increment bitAnd: 16rFF.
						countWord at: 3 put: redCount.
						greenCount _ (countWord at: 2) + increment bitAnd: 16rFF.
						countWord at: 2 put: greenCount.
						edgeCounts bytesAt: redPixelIndex put: countWord ].

					blueOffset <= clipRight ifTrue: [
						countWord _ edgeCounts bytesAt: bluePixelIndex.
						blueCount _ (countWord at: 1) + increment bitAnd: 16rFF.
						countWord at: 1 put: blueCount.
						edgeCounts bytesAt: bluePixelIndex put: countWord ].
				]
				ifFalse: [			"Third case: R in one word, GB in the previous"
					redOffset <= clipRight ifTrue: [
						countWord _ edgeCounts bytesAt: redPixelIndex.
						redCount _ (countWord at: 3) + increment bitAnd: 16rFF.
						countWord at: 3 put: redCount.
						edgeCounts bytesAt: redPixelIndex put: countWord ].

					blueOffset <= clipRight ifTrue: [
						countWord _ edgeCounts bytesAt: bluePixelIndex.
						greenCount _ (countWord at: 2) + increment bitAnd: 16rFF.
						countWord at: 2 put: greenCount.
						blueCount _ (countWord at: 1) + increment bitAnd: 16rFF.
						countWord at: 1 put: blueCount.
						edgeCounts bytesAt: bluePixelIndex put: countWord ].
				]
		].! !

!VectorEngineSubPixel methodsFor: 'private' stamp: 'jmv 9/3/2021 14:23:38'!
auxPixelDataClass
	"Because we hold data for subpixels"

	^WordArray! !

!VectorEngineSubPixel methodsFor: 'private' stamp: 'jmv 9/3/2021 14:27:10'!
pvtSetForm: aForm

	super pvtSetForm: aForm.
	subPixelDelta _ self class defaultSubPixelDelta.! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 8/10/2021 12:06:05'!
blendFillOnly
	"Blends fill color over background. Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixel edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside pixelIndex antiAliasAlphasWord |
	spanTop to: spanBottom do: [ :displayY |
		edgesUpToThisPixelR _ 0.
		edgesUpToThisPixelG _ 0.
		edgesUpToThisPixelB _ 0.
		pixelIndex _ displayY * targetWidth + spanLeft.
		spanLeft to: spanRight do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			edgesThisPixel _ edgeCounts bytesAt: pixelIndex.
			edgeCounts at: pixelIndex put: 0.
			edgesThisPixelR _ edgesThisPixel at: 3.
			edgesThisPixelG _ edgesThisPixel at: 2.
			edgesThisPixelB _ edgesThisPixel at: 1.

			"Not needed in C or OpenCL, use signed char, i.e. a type where 16rFF means -1 and not 255"
			edgesThisPixelR > 127 ifTrue: [ edgesThisPixelR _ edgesThisPixelR - 256 ].
			edgesThisPixelG > 127 ifTrue: [ edgesThisPixelG _ edgesThisPixelG - 256 ].
			edgesThisPixelB > 127 ifTrue: [ edgesThisPixelB _ edgesThisPixelB - 256 ].

			edgesUpToThisPixelR _ edgesUpToThisPixelR + edgesThisPixelR.
			edgesUpToThisPixelG _ edgesUpToThisPixelG + edgesThisPixelG.
			edgesUpToThisPixelB _ edgesUpToThisPixelB + edgesThisPixelB.
			isRedInside _ edgesUpToThisPixelR ~= 0.
			isGreenInside _ edgesUpToThisPixelG ~= 0.
			isBlueInside _ edgesUpToThisPixelB ~= 0.

			antiAliasAlphasWord _ alphaMask bytesAt: pixelIndex.
			antiAliasAlphasWord ~= "16r00000000" #[16r00 16r00 16r00 16r00] ifTrue: [
				alphaMask at: pixelIndex put: 0 ].
			(antiAliasAlphasWord ~= "16r00000000" #[16r00 16r00 16r00 16r00] or: [
				isRedInside or: [ isGreenInside or: [ isBlueInside ]]]) ifTrue: [
					"At least one subpixel inside the shape, or in the anti aliasing area"
					self blendFillOnlyAt: pixelIndex
						redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
						antiAliasAlphasWord: antiAliasAlphasWord ]]].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 9/30/2021 12:09:20'!
blendFillOnlyAt: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside antiAliasAlphasWord: antiAliasAlphasWord
	"Blends fill color over background. Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB antiAliasRedAlphaBits antiAliasRedAlpha antiAliasGreenAlphaBits antiAliasGreenAlpha antiAliasBlueAlphaBits antiAliasBlueAlpha alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlpha targetAlpha clippingAntiAlias |

	"In this method, antiAliasAlphas are not used to blend stroke, but fill. This means that in the inside of the shape, and away from the stroke, they must be 1.0 (not 0.0)."
	antiAliasRedAlphaBits _ antiAliasAlphasWord at: 3.
	antiAliasGreenAlphaBits _ antiAliasAlphasWord at: 2.
	antiAliasBlueAlphaBits _ antiAliasAlphasWord at: 1.
	isRedInside ifTrue: [ antiAliasRedAlphaBits _ 127 - antiAliasRedAlphaBits ].
	isGreenInside ifTrue: [ antiAliasGreenAlphaBits _ 127 - antiAliasGreenAlphaBits ].
	isBlueInside ifTrue: [ antiAliasBlueAlphaBits _ 127 - antiAliasBlueAlphaBits ].
	antiAliasRedAlpha _ antiAliasRedAlphaBits / 127.0.
	antiAliasGreenAlpha _ antiAliasGreenAlphaBits / 127.0.
	antiAliasBlueAlpha _ antiAliasBlueAlphaBits / 127.0.
	alphaR _ antiAliasRedAlpha * fillColor alpha.
	alphaG _ antiAliasGreenAlpha * fillColor alpha.
	alphaB _ antiAliasBlueAlpha * fillColor alpha.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
			clippingAntiAlias _ clippingAntiAlias max: antiAliasGreenAlphaBits]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
					alphaR _ alphaR * clippingAntiAlias / 127.0.
					alphaG _ alphaG * clippingAntiAlias / 127.0.
					alphaB _ alphaB * clippingAntiAlias / 127.0 ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAlias _ 0]].

	alphaR + alphaG + alphaB = 0.0 ifFalse: [
		targetWord _ targetForm bits bytesAt: pixelIndex.
		targetAlpha _ (targetWord at: 4) / 255.0.

		"These if are not really needed. just ignore them if we use simd instructions."
		alphaR = 0.0 ifFalse: [
			unAlphaR _ 1.0 - alphaR.
			resultAlpha _ alphaR + (unAlphaR * targetAlpha).
			resultR _ alphaR * fillColor red * 255 + (unAlphaR * (targetWord at: 3) * targetAlpha).
			resultR _ resultR/resultAlpha.
			targetWord at: 3 put: resultR rounded ].

		alphaG = 0.0 ifFalse: [
			unAlphaG _ 1.0 - alphaG.
			resultAlpha _ alphaG + (unAlphaG * targetAlpha).
			resultG _ alphaG * fillColor green * 255 + (unAlphaG * (targetWord at: 2) * targetAlpha).
			resultG _ resultG/resultAlpha.
			targetWord at: 2 put: resultG rounded.
			targetWord at: 4 put: (resultAlpha*255) rounded ].

		alphaB = 0.0 ifFalse: [
			unAlphaB _ 1.0 - alphaB.
			resultAlpha _ alphaB + (unAlphaB * targetAlpha).
			resultB _ alphaB * fillColor blue * 255 + (unAlphaB * (targetWord at: 1) * targetAlpha).
			resultB _ resultB/resultAlpha.
			targetWord at: 1 put: resultB rounded ].

		targetForm bits bytesAt: pixelIndex put: targetWord.
		morphIds at: pixelIndex put: currentMorphId<<8 + clippingAntiAlias ].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 8/10/2021 12:06:05'!
blendStrokeAndFill
	"Do an appropriate (anti aliased) mix between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixel edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside pixelIndex antiAliasAlphasWord |
	spanTop to: spanBottom do: [ :displayY |
		edgesUpToThisPixelR _ 0.
		edgesUpToThisPixelG _ 0.
		edgesUpToThisPixelB _ 0.
		pixelIndex _ displayY * targetWidth + spanLeft.
		spanLeft to: spanRight do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			edgesThisPixel _ edgeCounts bytesAt: pixelIndex.
			edgeCounts at: pixelIndex put: 0.
			edgesThisPixelR _ edgesThisPixel at: 3.
			edgesThisPixelG _ edgesThisPixel at: 2.
			edgesThisPixelB _ edgesThisPixel at: 1.

			"Not needed in C or OpenCL, use signed char, i.e. a type where 16rFF means -1 and not 255"
			edgesThisPixelR > 127 ifTrue: [ edgesThisPixelR _ edgesThisPixelR - 256 ].
			edgesThisPixelG > 127 ifTrue: [ edgesThisPixelG _ edgesThisPixelG - 256 ].
			edgesThisPixelB > 127 ifTrue: [ edgesThisPixelB _ edgesThisPixelB - 256 ].

			edgesUpToThisPixelR _ edgesUpToThisPixelR + edgesThisPixelR.
			edgesUpToThisPixelG _ edgesUpToThisPixelG + edgesThisPixelG.
			edgesUpToThisPixelB _ edgesUpToThisPixelB + edgesThisPixelB.
			isRedInside _ edgesUpToThisPixelR ~= 0.
			isGreenInside _ edgesUpToThisPixelG ~= 0.
			isBlueInside _ edgesUpToThisPixelB ~= 0.

			antiAliasAlphasWord _ alphaMask bytesAt: pixelIndex.
			antiAliasAlphasWord ~= "16r00000000" #[16r00 16r00 16r00 16r00] ifTrue: [
				alphaMask at: pixelIndex put: 0 ].
			(antiAliasAlphasWord ~= "16r00000000" #[16r00 16r00 16r00 16r00] or: [
				isRedInside or: [ isGreenInside or: [ isBlueInside ]]]) ifTrue: [
					"At least one subpixel in the stroke or inside the shape"
					self blendStrokeAndFillAt: pixelIndex
						redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
						antiAliasAlphasWord: antiAliasAlphasWord ]]].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 9/30/2021 12:10:44'!
blendStrokeAndFillAt: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside antiAliasAlphasWord: antiAliasAlphasWord
	"Do an appropriate (anti aliased) mix between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB antiAliasRedAlphaBits antiAliasRedAlpha antiAliasGreenAlphaBits antiAliasGreenAlpha antiAliasBlueAlphaBits antiAliasBlueAlpha alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB foreR foreG foreB targetAlpha resultAlpha clippingAntiAlias |

	antiAliasRedAlphaBits _ antiAliasAlphasWord at: 3.
	antiAliasGreenAlphaBits _ antiAliasAlphasWord at: 2.
	antiAliasBlueAlphaBits _ antiAliasAlphasWord at: 1.
	antiAliasRedAlpha _ antiAliasRedAlphaBits / 127.0.
	antiAliasGreenAlpha _ antiAliasGreenAlphaBits / 127.0.
	antiAliasBlueAlpha _ antiAliasBlueAlphaBits / 127.0.

	isRedInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaR _ antiAliasRedAlpha * strokeColor alpha + ((1.0-antiAliasRedAlpha) * fillColor alpha).
			foreR _ antiAliasRedAlpha * strokeColor red * 255 + ((1.0-antiAliasRedAlpha) * fillColor red * 255) ]
		ifFalse: [							"Blend stroke over background"
			alphaR _ antiAliasRedAlpha * strokeColor alpha.
			foreR _ strokeColor red * 255 ].

	isGreenInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaG _ antiAliasGreenAlpha * strokeColor alpha + ((1.0-antiAliasGreenAlpha) * fillColor alpha).
			foreG _ antiAliasGreenAlpha * strokeColor green * 255 + ((1.0-antiAliasGreenAlpha) * fillColor green * 255) ]
		ifFalse: [							"Blend stroke over background"
			alphaG _ antiAliasGreenAlpha * strokeColor alpha.
			foreG _ strokeColor green * 255 ].

	isBlueInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaB _ antiAliasBlueAlpha * strokeColor alpha + ((1.0-antiAliasBlueAlpha) * fillColor alpha).
			foreB _ antiAliasBlueAlpha * strokeColor blue * 255 + ((1.0-antiAliasBlueAlpha) * fillColor blue * 255) ]
		ifFalse: [							"Blend stroke over background"
			alphaB _ antiAliasBlueAlpha * strokeColor alpha.
			foreB _ strokeColor blue * 255 ].

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			isGreenInside
				ifTrue: [clippingAntiAlias _ 127]
				ifFalse: [
					clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
					clippingAntiAlias _ clippingAntiAlias max: antiAliasGreenAlphaBits]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
					alphaR _ alphaR * clippingAntiAlias / 127.0.
					alphaG _ alphaG * clippingAntiAlias / 127.0.
					alphaB _ alphaB * clippingAntiAlias / 127.0 ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAlias _ 0]].

	alphaR + alphaG + alphaB = 0.0 ifFalse: [
		targetWord _ targetForm bits bytesAt: pixelIndex.
		targetAlpha _ (targetWord at: 4) / 255.0.

		"These if are not really needed. just ignore them if we use simd instructions."
		alphaR = 0.0 ifFalse: [
			unAlphaR _ 1.0 - alphaR.
			resultAlpha _ alphaR + (unAlphaR * targetAlpha).
			resultR _ alphaR * foreR + (unAlphaR * (targetWord at: 3) * targetAlpha).
			resultR _ resultR/resultAlpha.
			targetWord at: 3 put: resultR rounded ].
		alphaG = 0.0 ifFalse: [
			unAlphaG _ 1.0 - alphaG.
			resultAlpha _ alphaG + (unAlphaG * targetAlpha).
			resultG _ alphaG * foreG + (unAlphaG * (targetWord at: 2) * targetAlpha).
			resultG _ resultG/resultAlpha.
			targetWord at: 2 put: resultG rounded.
			targetWord at: 4 put: (resultAlpha*255) rounded ].
		alphaB = 0.0 ifFalse: [
			unAlphaB _ 1.0 - alphaB.
			resultAlpha _ alphaB + (unAlphaB * targetAlpha).
			resultB _ alphaB * foreB + (unAlphaB * (targetWord at: 1) * targetAlpha).
			resultB _ resultB/resultAlpha.
			targetWord at: 1 put: resultB rounded ].

		targetForm bits bytesAt: pixelIndex put: targetWord.
		morphIds at: pixelIndex put: currentMorphId<<8 + clippingAntiAlias ].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 8/10/2021 12:06:05'!
blendStrokeOnly
	"Blends stroke color over background. Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| pixelIndex antiAliasAlphasWord |
	spanTop to: spanBottom do: [ :displayY |
		pixelIndex _ displayY * targetWidth + spanLeft.
		spanLeft to: spanRight do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			antiAliasAlphasWord _ alphaMask bytesAt: pixelIndex.
			"At least one subpixel in the stroke."
			antiAliasAlphasWord = "16r00000000" #[16r00 16r00 16r00 16r00] ifFalse: [
				alphaMask at: pixelIndex put: 0.
				self blendStrokeOnlyAt: pixelIndex antiAliasAlphasWord: antiAliasAlphasWord ]]].! !

!VectorEngineSubPixel methodsFor: 'private - blend with mask' stamp: 'jmv 9/30/2021 12:11:29'!
blendStrokeOnlyAt: pixelIndex antiAliasAlphasWord: antiAliasAlphasWord
	"Blends stroke color over background. Target translucency computed correctly.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB antiAliasRedAlphaBits antiAliasRedAlpha antiAliasGreenAlphaBits antiAliasGreenAlpha antiAliasBlueAlphaBits antiAliasBlueAlpha alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlpha targetAlpha clippingAntiAlias |

	antiAliasRedAlphaBits _ antiAliasAlphasWord at: 3.
	antiAliasGreenAlphaBits _ antiAliasAlphasWord at: 2.
	antiAliasBlueAlphaBits _ antiAliasAlphasWord at: 1.
	antiAliasRedAlpha _ antiAliasRedAlphaBits / 127.0.
	antiAliasGreenAlpha _ antiAliasGreenAlphaBits / 127.0.
	antiAliasBlueAlpha _ antiAliasBlueAlphaBits / 127.0.
	alphaR _ antiAliasRedAlpha * strokeColor alpha.
	alphaG _ antiAliasGreenAlpha * strokeColor alpha.
	alphaB _ antiAliasBlueAlpha * strokeColor alpha.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
			clippingAntiAlias _ clippingAntiAlias max: antiAliasGreenAlphaBits]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
					alphaR _ alphaR * clippingAntiAlias / 127.0.
					alphaG _ alphaG * clippingAntiAlias / 127.0.
					alphaB _ alphaB * clippingAntiAlias / 127.0 ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAlias _ 0]].

	alphaR + alphaG + alphaB = 0.0 ifFalse: [
		targetWord _ targetForm bits bytesAt: pixelIndex.
		targetAlpha _ (targetWord at: 4) / 255.0.

		"These if are not really needed. just ignore them if we use simd instructions."
		alphaR = 0.0 ifFalse: [
			unAlphaR _ 1.0 - alphaR.
			resultAlpha _ alphaR + (unAlphaR * targetAlpha).
			resultR _ alphaR * strokeColor red * 255 + (unAlphaR * (targetWord at: 3) * targetAlpha).
			resultR _ resultR/resultAlpha.
			targetWord at: 3 put: resultR rounded ].

		alphaG = 0.0 ifFalse: [
			unAlphaG _ 1.0 - alphaG.
			resultAlpha _ alphaG + (unAlphaG * targetAlpha).
			resultG _ alphaG * strokeColor green * 255 + (unAlphaG * (targetWord at: 2) * targetAlpha).
			resultG _ resultG/resultAlpha.
			targetWord at: 2 put: resultG rounded.
			targetWord at: 4 put: (resultAlpha*255) rounded ].

		alphaB = 0.0 ifFalse: [
			unAlphaB _ 1.0 - alphaB.
			resultAlpha _ alphaB + (unAlphaB * targetAlpha).
			resultB _ alphaB * strokeColor blue * 255 + (unAlphaB * (targetWord at: 1) * targetAlpha).
			resultB _ resultB/resultAlpha.
			targetWord at: 1 put: resultB rounded ].

		targetForm bits bytesAt: pixelIndex put: targetWord.
		morphIds at: pixelIndex put: currentMorphId<<8 + clippingAntiAlias ].! !

!VectorEngineSubPixel class methodsFor: 'instance creation' stamp: 'jmv 6/17/2020 14:34:51'!
defaultAntiAliasingWidth
	^1.4! !

!VectorEngineSubPixel class methodsFor: 'instance creation' stamp: 'jmv 5/18/2021 10:06:34'!
defaultSubPixelDelta

	^0.25			"Values closer to 0.333 might produce a bit of coloring on some displays. Used to be 0.28"! !

!VectorEngineWholePixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 8/10/2021 12:07:01'!
updateAlphasForX: x y: y
	| distanceToAxisSquared alpha t b l r prevAlpha dx dy pixelIndex |

	"Compute affected rect. Honor clipRect."
	t _ (y - swDilatedHalf + 1) truncated.												"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	t < clipTop ifTrue: [ t _ clipTop ].
	b _ (y + swDilatedHalf) truncated.
	b > clipBottom ifTrue: [ b _ clipBottom ].
	l _ (x - swDilatedHalf + 1) truncated.												"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	l < clipLeft ifTrue: [ l _ clipLeft ].
	r _ (x + swDilatedHalf) truncated.
	r > clipRight ifTrue: [ r _ clipRight ].

	t to: b do: [ :displayY |
		pixelIndex _ displayY * targetWidth + l.
		dy _ displayY - y.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			prevAlpha _ alphaMask at: pixelIndex.
			prevAlpha = 16r7F ifFalse: [
				dx _ displayX - x.
				distanceToAxisSquared _ (dx*dx) + (dy*dy).
				distanceToAxisSquared < swDilatedHalfSquared
					ifTrue: [
						alpha _ distanceToAxisSquared <= swErodedHalfSquared
							ifTrue: [ 16r7F ]
							ifFalse: [ (swDilatedHalf - distanceToAxisSquared sqrt * aawScaledInverse) truncated ].
						alpha > prevAlpha ifTrue: [
							alphaMask at: pixelIndex put: alpha ]]]]]! !

!VectorEngineWholePixel methodsFor: 'primitive algorithms - aux' stamp: 'jmv 8/30/2021 16:35:55'!
updateEdgeCountAtX: x y: y
	"Compute edges intersecting with this horizontal line, for fills.
		
	See https://en.wikipedia.org/wiki/Nonzero-rule .

Where it says 'pick a ray in any direction', we pick an horizontal ray pointing to the left. This means that for any point (actually for every pixel) p1=(x1, y1), we record any crossings of y1, i.e. any sequence of path points (... pp1, pp2, .....) where the jump from pp1 to pp2 crosses y1, and pp1.x and pp2.x are at the left of x1. If the path is going up, it is clockwwise. If it is going down, it is counter clockwise.

As an additional optimization, as we need to store for every pixel all the crossings to the left, what we actually store are the crossings at each pixel (i.e. within 1 pixel distance to the left). Then, when we use them, the algorithms iterates from left to right, and accumulates the counters from previous pixels.
	"

	| thisYTruncated pixelY pixelIndex increment count pixelX |

	thisYTruncated _ y truncated.
	thisYTruncated = prevYTruncated ifTrue: [
		^ self].
	(thisYTruncated between: clipTop-1 and: clipBottom) ifFalse: [
		^ self ].
	prevYTruncated = 16r3FFFFFFF ifTrue: [ 
		^ prevYTruncated _ thisYTruncated ].

	"All edge count at the left of the clipRect are added there (at the left of the clipRect).
	The effect is the same, and we need to clean up less stuff afterwards.
	More important, it avoids trying to acess pixels outside our form, i.e. invalid array acesses."
	pixelX _ (x+1) truncated max: clipLeft.										"take the next pixel center to the right of x"
	pixelX <= clipRight ifTrue: [
		pixelY _ thisYTruncated max: prevYTruncated.
		increment _ (thisYTruncated - prevYTruncated) sign.
		pixelIndex _ pixelY * targetWidth + pixelX +1.
		count _ edgeCounts at: pixelIndex.
		count _ count + increment bitAnd: 16rFF.
		edgeCounts at: pixelIndex put: count ].
	prevYTruncated _ thisYTruncated.! !

!VectorEngineWholePixel methodsFor: 'private' stamp: 'jmv 9/3/2021 14:24:15'!
auxPixelDataClass
	"Because we hold data for whole pixels"

	^ByteArray! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 8/10/2021 12:06:05'!
blendFillOnly
	"Blends fill color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| edgesUpToThisPixel edgesThisPixel pixelIndex antiAliasAlphaBits |
	spanTop to: spanBottom do: [ :displayY |
		edgesUpToThisPixel _ 0.
		pixelIndex _ displayY * targetWidth + spanLeft.
		spanLeft to: spanRight do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			edgesThisPixel _ edgeCounts at: pixelIndex.
			edgesThisPixel = 0 ifFalse: [
				edgeCounts at: pixelIndex put: 0.
				"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
				edgesThisPixel > 127 ifTrue: [ edgesThisPixel _ edgesThisPixel - 256 ].
				edgesUpToThisPixel _ edgesUpToThisPixel + edgesThisPixel ].
			antiAliasAlphaBits _ alphaMask at: pixelIndex.
			edgesUpToThisPixel = 0
				ifFalse: [				"Inside the shape"
					antiAliasAlphaBits = 0 ifFalse: [ alphaMask at: pixelIndex put: 0 ].
					antiAliasAlphaBits _ 127 - antiAliasAlphaBits.
					self blendFillOnlyAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits
					]
				ifTrue: [				"Still in the anti aliasing area, but outside the shape, strictly speaking."
					antiAliasAlphaBits = 0 ifFalse: [
						alphaMask at: pixelIndex put: 0.
						self blendFillOnlyAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits ]]]].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 9/30/2021 12:11:38'!
blendFillOnlyAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits
	"Blends fill color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| resultR resultG resultB antiAliasAlpha alpha unAlpha resultAlpha targetWord targetAlpha clippingAntiAlias |

	antiAliasAlpha _ antiAliasAlphaBits / 127.0.
	alpha _ antiAliasAlpha * fillColor alpha.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
			clippingAntiAlias _ clippingAntiAlias max: antiAliasAlphaBits ]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
					alpha _ alpha * clippingAntiAlias / 127.0 ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAlias _ 0]].

	alpha = 0.0 ifFalse: [
		unAlpha _ 1.0 - alpha.
		targetWord _ targetForm bits bytesAt: pixelIndex.
		targetAlpha _ (targetWord at: 4) / 255.0.
		resultAlpha _ alpha + (unAlpha * targetAlpha).
		targetWord at: 4 put: (resultAlpha*255) rounded.
		
		resultR _ alpha * fillColor red * 255 + (unAlpha * (targetWord at: 3) * targetAlpha).
		resultR _ resultR/resultAlpha.
		targetWord at: 3 put: resultR rounded.

		resultG _ alpha * fillColor green * 255 + (unAlpha * (targetWord at: 2) * targetAlpha).
		resultG _ resultG/resultAlpha.
		targetWord at: 2 put: resultG rounded.

		resultB _ alpha * fillColor blue * 255 + (unAlpha * (targetWord at: 1) * targetAlpha).
		resultB _ resultB/resultAlpha.
		targetWord at: 1 put: resultB rounded.

		targetForm bits bytesAt: pixelIndex put: targetWord.
		morphIds at: pixelIndex put: currentMorphId<<8 + clippingAntiAlias ].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 8/10/2021 12:06:05'!
blendStrokeAndFill
	"Do an appropriate (anti aliased) mix between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| edgesUpToThisPixel edgesThisPixel pixelIndex antiAliasAlphaBits |
	spanTop to: spanBottom do: [ :displayY |
		edgesUpToThisPixel _ 0.
		pixelIndex _ displayY * targetWidth + spanLeft.
		spanLeft to: spanRight do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			edgesThisPixel _ edgeCounts at: pixelIndex.
			edgesThisPixel = 0 ifFalse: [
				edgeCounts at: pixelIndex put: 0.
				"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
				edgesThisPixel > 127 ifTrue: [ edgesThisPixel _ edgesThisPixel - 256 ].
				edgesUpToThisPixel _ edgesUpToThisPixel + edgesThisPixel ].
			antiAliasAlphaBits _ alphaMask at: pixelIndex.
			edgesUpToThisPixel = 0
				ifFalse: [				"Inside the shape"
					antiAliasAlphaBits = 0 ifFalse: [ alphaMask at: pixelIndex put: 0 ].
					self blendStrokeAndFillAt: pixelIndex isInside: true antiAliasAlphaByte: antiAliasAlphaBits ]
				ifTrue: [				"Still in the stroke, but outside the shape, strictly speaking."
					antiAliasAlphaBits = 0 ifFalse: [
						alphaMask at: pixelIndex put: 0.
						antiAliasAlphaBits = 0 ifFalse: [
							self blendStrokeAndFillAt: pixelIndex isInside: false antiAliasAlphaByte: antiAliasAlphaBits ]]]]].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 9/30/2021 12:11:47'!
blendStrokeAndFillAt: pixelIndex isInside: isInside antiAliasAlphaByte: antiAliasAlphaBits
	"Do an appropriate (anti aliased) mix between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB antiAliasAlpha alpha unAlpha foreR foreG foreB targetAlpha resultAlpha clippingAntiAlias |

	antiAliasAlpha _ antiAliasAlphaBits / 127.0.

	isInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alpha _ antiAliasAlpha * strokeColor alpha + ((1-antiAliasAlpha) * fillColor alpha).
			foreR _ antiAliasAlpha * strokeColor red * 255 + ((1-antiAliasAlpha) * fillColor red * 255).
			foreG _ antiAliasAlpha * strokeColor green * 255 + ((1-antiAliasAlpha) * fillColor green * 255).
			foreB _ antiAliasAlpha * strokeColor blue * 255 + ((1-antiAliasAlpha) * fillColor blue * 255) ]
		ifFalse: [							"Blend stroke over background"
			alpha _ antiAliasAlpha * strokeColor alpha.
			foreR _ strokeColor red * 255.
			foreG _ strokeColor green * 255.
			foreB _ strokeColor blue * 255 ].

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			isInside
				ifTrue: [clippingAntiAlias _ 127]
				ifFalse: [
					clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
					clippingAntiAlias _ clippingAntiAlias max: antiAliasAlphaBits]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
					alpha _ alpha * clippingAntiAlias / 127.0 ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAlias _ 0]].

	alpha = 0.0 ifFalse: [
		unAlpha _ 1.0 - alpha.
		targetWord _ targetForm bits bytesAt: pixelIndex.
		targetAlpha _ (targetWord at: 4) / 255.0.
		resultAlpha _ alpha + (unAlpha * targetAlpha).
		targetWord at: 4 put: (resultAlpha*255) rounded.

		resultR _ alpha * foreR + (unAlpha * (targetWord at: 3) * targetAlpha).
		resultR _ resultR/resultAlpha.
		targetWord at: 3 put: resultR rounded.

		resultG _ alpha * foreG + (unAlpha * (targetWord at: 2) * targetAlpha).
		resultG _ resultG/resultAlpha.
		targetWord at: 2 put: resultG rounded.

		resultB _ alpha * foreB + (unAlpha * (targetWord at: 1) * targetAlpha).
		resultB _ resultB/resultAlpha.
		targetWord at: 1 put: resultB rounded.

		targetForm bits bytesAt: pixelIndex put: targetWord.
		morphIds at: pixelIndex put: currentMorphId<<8 + clippingAntiAlias ].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 8/10/2021 12:06:05'!
blendStrokeOnly
	"Blends stroke color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| pixelIndex antiAliasAlphaBits |
	spanTop to: spanBottom do: [ :displayY |
		pixelIndex _ displayY * targetWidth + spanLeft.
		spanLeft to: spanRight do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			antiAliasAlphaBits _ alphaMask at: pixelIndex.
			antiAliasAlphaBits = 16r00 ifFalse: [										"In the stroke"
				alphaMask at: pixelIndex put: 0.
				self blendStrokeOnlyAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits ]]].! !

!VectorEngineWholePixel methodsFor: 'private - blend with mask' stamp: 'jmv 9/30/2021 12:11:56'!
blendStrokeOnlyAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits
	"Blends stroke color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB antiAliasAlpha alpha unAlpha resultAlpha targetAlpha clippingAntiAlias |
	antiAliasAlpha _ antiAliasAlphaBits / 127.0.
	alpha _ antiAliasAlpha * strokeColor alpha.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
			clippingAntiAlias _ clippingAntiAlias max: antiAliasAlphaBits ]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					clippingAntiAlias _ (morphIds at: pixelIndex) bitAnd: 127.
					alpha _ alpha * clippingAntiAlias / 127.0 ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAlias _ 0]].

	alpha = 0.0 ifFalse: [
		unAlpha _ 1.0 - alpha.
		targetWord _ targetForm bits bytesAt: pixelIndex.

		targetAlpha _ (targetWord at: 4) / 255.0.
		resultAlpha _ alpha + (unAlpha * targetAlpha).
		targetWord at: 4 put: (resultAlpha*255) rounded.

		resultR _ alpha * strokeColor red * 255 + (unAlpha * (targetWord at: 3) * targetAlpha).
		resultR _ resultR/resultAlpha.
		targetWord at: 3 put: resultR rounded.

		resultG _ alpha * strokeColor green * 255 + (unAlpha * (targetWord at: 2) * targetAlpha).
		resultG _ resultG/resultAlpha.
		targetWord at: 2 put: resultG rounded.

		resultB _ alpha * strokeColor blue * 255 + (unAlpha * (targetWord at: 1) * targetAlpha).
		resultB _ resultB/resultAlpha.
		targetWord at: 1 put: resultB rounded.

		targetForm bits bytesAt: pixelIndex put: targetWord.
		morphIds at: pixelIndex put: currentMorphId<<8 + clippingAntiAlias ].! !

!VectorEngineWholePixel class methodsFor: 'instance creation' stamp: 'jmv 5/18/2021 12:40:20'!
defaultAntiAliasingWidth
	^1.6! !
