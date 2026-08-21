'From Cuis7.9 [latest update: #8144] on 19 August 2026 at 10:16:39 am'!

!VectorEngineWholePixel methodsFor: 'primitive algorithms' stamp: 'jmv 19/Aug/2026, 12:59:19 pm (UTC)'!
basicHorizontalLineAtY: y fromX: x0 toX: x1
	"Optimization for simple horizontal lines (in target space).
	Assumptions:
		- Stroke Only (no fill)
		- No dashes (dashBitLength = 0.0)
		- x1 > x0"

	| t b l r pixelIndex signedDistanceToAxis distanceToOuterEdgeOfAA candidateAlpha prevAlpha affectedBitsIndex nextPixelIndexToUpdateAffectedBits |
	needsFullAlphaCircle := true.
	self updateAlphasForX: x0 y: y.
	self updateContourForX: x0 y: y.

	"Compute affected rect. Honor clipRect."
	t := (y - swDilatedHalf + 1) truncated.	"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	t < clipTop ifTrue: [ t := clipTop ].
	b := (y + swDilatedHalf) truncated.
	b > clipBottom ifTrue: [ b := clipBottom ].
	l := x0 truncated + 1.					"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	l < clipLeft ifTrue: [ l := clipLeft ].
	r := x1 truncated.
	r > clipRight ifTrue: [ r := clipRight ].

	signedDistanceToAxis := t - y - 1.0.
	t to: b do: [ :displayY |
		pixelIndex := displayY * targetWidth + l.
		nextPixelIndexToUpdateAffectedBits := pixelIndex+1.
		candidateAlpha := 16r7F.
		signedDistanceToAxis := signedDistanceToAxis + 1.0.
		distanceToOuterEdgeOfAA := swDilatedHalf - signedDistanceToAxis abs.
		distanceToOuterEdgeOfAA < antiAliasingWidth
			ifTrue: [ candidateAlpha := (distanceToOuterEdgeOfAA * aawScaledInverse) truncated].
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			prevAlpha := alphaMask at: pixelIndex.
			candidateAlpha > prevAlpha ifTrue: [
				alphaMask at: pixelIndex put: candidateAlpha.
				pixelIndex >= nextPixelIndexToUpdateAffectedBits ifTrue: [
					affectedBitsIndex := pixelIndex-1>>4+1.
					affectedBits at: affectedBitsIndex put: 1.
					nextPixelIndexToUpdateAffectedBits := (pixelIndex-1 bitAnd: 16rFFFFFFF0) + 16r10 + 1 ]]]].

	needsFullAlphaCircle := true.
	self updateAlphasForX: x1 y: y.
	self updateContourForX: x1 y: y.
! !

!VectorEngineWholePixel methodsFor: 'primitive algorithms' stamp: 'jmv 19/Aug/2026, 1:01:26 pm (UTC)'!
basicVerticalLineAtX: x fromY: y0 toY: y1
	"Optimization for simple vertical lines (in target space).
	Assumptions:
		- Stroke Only (no fill)
		- No dashes (dashBitLength = 0.0)
		- y1 > y0"

	| t b l r pixelIndex signedDistanceToAxis distanceToOuterEdgeOfAA candidateAlpha prevAlpha affectedBitsIndex nextPixelIndexToUpdateAffectedBits |
	needsFullAlphaCircle := true.
	self updateAlphasForX: x y: y0.
	self updateContourForX: x y: y0.

	"Compute affected rect. Honor clipRect."
	t := y0 truncated + 1.					"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	t < clipTop ifTrue: [ t := clipTop ].
	b := y1 truncated.
	b > clipBottom ifTrue: [ b := clipBottom ].
	l := (x - swDilatedHalf + 1) truncated.	"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	l < clipLeft ifTrue: [ l := clipLeft ].
	r := (x + swDilatedHalf) truncated.
	r > clipRight ifTrue: [ r := clipRight ].

	t to: b do: [ :displayY |
		pixelIndex := displayY * targetWidth + l.
		nextPixelIndexToUpdateAffectedBits := pixelIndex+1.
		signedDistanceToAxis := l - x - 1.0.
		l to: r do: [ :displayX |
			candidateAlpha := 16r7F.
			signedDistanceToAxis := signedDistanceToAxis + 1.0.
			distanceToOuterEdgeOfAA := swDilatedHalf - signedDistanceToAxis abs.
			distanceToOuterEdgeOfAA < antiAliasingWidth
				ifTrue: [ candidateAlpha := (distanceToOuterEdgeOfAA * aawScaledInverse) truncated].
			pixelIndex := pixelIndex + 1.
			prevAlpha := alphaMask at: pixelIndex.
			candidateAlpha > prevAlpha ifTrue: [
				alphaMask at: pixelIndex put: candidateAlpha.
				pixelIndex >= nextPixelIndexToUpdateAffectedBits ifTrue: [
					affectedBitsIndex := pixelIndex-1>>4+1.
					affectedBits at: affectedBitsIndex put: 1.
					nextPixelIndexToUpdateAffectedBits := (pixelIndex-1 bitAnd: 16rFFFFFFF0) + 16r10 + 1 ]]]].

	needsFullAlphaCircle := true.
	self updateAlphasForX: x y: y1.
	self updateContourForX: x y: y1.
! !

!VectorEngineWholePixel methodsFor: 'primitive algorithms' stamp: 'jmv 18/Aug/2026, 5:53:00 pm (UTC)'!
lineFromX: xFrom y: yFrom toX: xTo y: yTo

	| p txFrom tyFrom txTo tyTo x0 x1 y0 y1 dx dy hops t oneLessT increment x y |
	
	p := geometryTransformation externalizePosition: xFrom@yFrom. txFrom := p x. tyFrom := p y.
	p := geometryTransformation externalizePosition: xTo@yTo. txTo := p x. tyTo := p y.

	x0 := txFrom min: txTo.
	x1 := txFrom max: txTo.
	y0 := tyFrom min: tyTo.
	y1 := tyFrom max: tyTo.
	spanLeft := spanLeft min: x0.
	spanRight := spanRight max: x1.
	spanTop := spanTop min: y0.
	spanBottom := spanBottom max: y1.

	(fillColor isNil and: [dashBitLength = 0.0]) ifTrue: [
		y0 = y1 ifTrue: [
			^self basicHorizontalLineAtY: y0 fromX: x0 toX: x1 ].
		x0 = x1 ifTrue: [
			^self basicVerticalLineAtX: x0 fromY: y0 toY: y1 ]].

	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	dx := (txTo - txFrom) abs.
	dy := (tyTo - tyFrom) abs.
	hops := ((dx max: dy) / hop) truncated + 1.
	increment := 1.0 / hops.
	t := 0.0.
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
! !


!VectorEngineWholePixel reorganize!
('primitive algorithms' basicHorizontalLineAtY:fromX:toX: basicVerticalLineAtX:fromY:toY: lineFromX:y:toX:y:)
('primitive algorithms - aux' updateAlphasForX:y: updateAlphasZeroStrokeForX:y: updateEdgeCountAtX:y:)
('private' auxPixelDataClass)
('private - blend with mask' blendFillOnly blendFillOnlyAt:antiAliasAlphaByte: blendStrokeAndFill blendStrokeAndFillAt:antiAliasAlphaByte: blendStrokeOnly blendStrokeOnlyAt:antiAliasAlphaByte:)
!

