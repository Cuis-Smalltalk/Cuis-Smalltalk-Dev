'From Cuis7.9 [latest update: #9996] on 21 August 2026 at 5:21:33 pm'!
'Description Version 1.36 of VectorEnginePlugin.pck.st is equivalent to VectorEnginePlugin-jmv.26.mcz at squeaksource.com/VectorEnginePlugin.html
'!
!provides: 'VectorEnginePlugin' 1 37!
!requires: 'VMMaker' 1 7 nil!
SystemOrganization addCategory: #VectorEnginePlugin!


!classDefinition: #VectorEnginePlugin category: #VectorEnginePlugin!
SmartSyntaxInterpreterPlugin subclass: #VectorEnginePlugin
	instanceVariableNames: 'targetBits morphIds edgeCountsSP edgeCountsWP alphaMaskSP alphaMaskWP affectedBits contour targetWidth targetHeight targetAssumedOpaque antiAliasingWidth subPixelDelta hop strokeWidth strokeR strokeG strokeB strokeA strokeColorWord dashedStrokeBits dashBitCount dashBitLength dashBitOffset fillR fillG fillB fillA fillColorWord txA11 txA12 txA13 txA21 txA22 txA23 currentMorphId clipLeft clipTop clipRight clipBottom clippingSpec auxAntiAliasingWidthScaledInverse auxStrokeWidthDilatedHalf auxStrokeWidthDilatedHalfSquared auxStrokeWidthErodedHalfSquared spanLeft spanTop spanRight spanBottom prevYTruncated prevYRounded leftAtThisY rightAtThisY trajectoryLength needsFullAlphaCircle'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorEnginePlugin'!
!classDefinition: 'VectorEnginePlugin class' category: #VectorEnginePlugin!
VectorEnginePlugin class
	instanceVariableNames: ''!

!classDefinition: #VectorEnginePluginSimulator category: #VectorEnginePlugin!
VectorEnginePlugin subclass: #VectorEnginePluginSimulator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorEnginePlugin'!
!classDefinition: 'VectorEnginePluginSimulator class' category: #VectorEnginePlugin!
VectorEnginePluginSimulator class
	instanceVariableNames: ''!


!VectorEnginePlugin commentStamp: 'jmv 3/25/2025 09:03' prior: 0!
Slang implementation of VectorGraphicsEngine. Based on https://www.researchgate.net/publication/267152327_Prefiltering_Antialiasing_for_General_Vector_Graphics
!

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 8/Apr/2025, 12:49:00 pm (UTC)'!
antiAliasingWidth: aFloat subPixelDelta: otherFloat
	self
		primitive: 'primAntiAliasingWidthsubPixelDelta'
		parameters: #(Float Float).

	antiAliasingWidth := aFloat.
	subPixelDelta := otherFloat.
	auxAntiAliasingWidthScaledInverse := 127.0 / antiAliasingWidth.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'eem 14/Nov/2025, 10:35:00 pm (UTC)'!
clearClippingSpec

	self
		primitive: 'primClearClippingSpec'
		parameters: #().

	clippingSpec := nil.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 11/Mar/2021, 2:00:23 pm (UTC)'!
clipLeft: l clipTop: t clipRight: r clipBottom: b
	self
		primitive: 'primClipLeftclipTopclipRightclipBottom'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger).

	clipLeft := l.
	clipTop := t.
	clipRight := r.
	clipBottom := b! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 25/Mar/2025, 1:24:00 pm (UTC)'!
clippingSpec: anIntegerArray

	self
		primitive: 'primSetClippingSpec'
		parameters: #(IntegerArray).

	clippingSpec := anIntegerArray.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 27/Mar/2025, 1:43:00 pm (UTC)'!
currentMorphId: aNumber
	self
		primitive: 'primCurrentMorphId'
		parameters: #(SmallInteger).

	currentMorphId := aNumber.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 8/Apr/2025, 5:54:00 pm (UTC)'!
dashedStrokeBits: onOffBitSequence dashBitCount: numberOfBitsInSequence dashBitLength: lengthOfEachBit dashBitOffset: offset
	self
		primitive: 'dashedStrokeBitsSet'
		parameters: #(SmallInteger SmallInteger Float SmallInteger).

	dashedStrokeBits := onOffBitSequence.
	dashBitCount := numberOfBitsInSequence.
	dashBitLength := lengthOfEachBit.
	dashBitOffset := offset.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 20/Aug/2026, 6:18:04 pm (UTC)'!
fillR: r g: g b: b a: a
	| auxA auxR auxG auxB |
	<var: 'auxA' type: #'uint32_t'>
	<var: 'auxR' type: #'uint32_t'>
	<var: 'auxG' type: #'uint32_t'>
	<var: 'auxB' type: #'uint32_t'>
	self
		primitive: 'primFillRGBA'
		parameters: #(Float Float Float Float).

	fillR := r * 255.0.
	fillG := g * 255.0.
	fillB := b * 255.0.
	fillA := a.
	auxA := (fillA * 255 + 0.5) asInteger bitShift: 24.
	auxR := (fillR + 0.5) asInteger bitShift: 16.
	auxG := (fillG + 0.5) asInteger bitShift: 8.
	auxB := (fillB + 0.5) asInteger.
	fillColorWord := ((auxA bitOr: auxR) bitOr: auxG) bitOr: auxB.
! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 13/May/2021, 2:46:17 pm (UTC)'!
geometryTxA11: a11 a12: a12 a13: a13 a21: a21 a22: a22 a23: a23
	self
		primitive: 'primGeometryTxSet'
		parameters: #(Float Float Float Float Float Float).

	txA11 := a11.
	txA12 := a12.
	txA13 := a13.
	txA21 := a21.
	txA22 := a22.
	txA23 := a23.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 5/Jul/2021, 7:11:13 pm (UTC)'!
initializePath
	self
		primitive: 'primInitializePath'
		parameters: #().

	spanLeft := targetWidth.						"drawable right. Will later be refined."
	spanTop := targetHeight.						"drawable bottom. Will later be refined."
	spanRight := 0.						"drawable left. Will later be refined."
	spanBottom := 0.						"drawable top. Will later be refined."
	prevYRounded := 16r7FFFFFFF.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 8/Apr/2025, 3:54:00 pm (UTC)'!
initializeTrajectoryFragment

	prevYTruncated := 16r7FFFFFFF.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'dtl 27/Jul/2021, 7:41:01 pm (UTC)'!
newTrajectoryFragment
	self
		primitive: 'primNewTrajectoryFragment'
		parameters: #().

	^ self initializeTrajectoryFragment.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 27/Mar/2025, 1:59:00 pm (UTC)'!
pluginApiVersion
	"
	API version is a SmallInteger.
	Note: this is Api version, not package version. If no Api change, Api version doesn't change, regardless of changes in the Plugin or Smalltalk code.
	See senders and implementors.
	"
	self
		primitive: 'pluginApiVersion'
		parameters: #().

	^ 7 asOop: SmallInteger.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 15/Nov/2025, 3:29:00 pm (UTC)'!
resetContourTop: t bottom: b
	self
		primitive: 'primResetContour'
		parameters: #(SmallInteger SmallInteger).

	leftAtThisY := 1000000.0.
	rightAtThisY := 0.0.
	t to: b do: [ :y |
		contour at: y * 2 put: 1000000.0.
		contour at: y * 2 + 1 put: 0.0 ].! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 10/Mar/2021, 1:53:54 pm (UTC)'!
spanBottom
	self
		primitive: 'primSpanBottom'
		parameters: #().

	^(spanBottom + auxStrokeWidthDilatedHalf) asInteger asOop: SmallInteger.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 6/Apr/2021, 7:13:04 pm (UTC)'!
spanLeft
	self
		primitive: 'primSpanLeft'
		parameters: #().

	"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	^(spanLeft - auxStrokeWidthDilatedHalf - subPixelDelta + 1) asInteger asOop: SmallInteger! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 6/Apr/2021, 7:13:38 pm (UTC)'!
spanRight
	self
		primitive: 'primSpanRight'
		parameters: #().

	"Make room not just for updated mask, but also edges (hence, +1)"
	^(spanRight + auxStrokeWidthDilatedHalf + subPixelDelta) asInteger +1 asOop: SmallInteger.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 6/Apr/2021, 7:13:45 pm (UTC)'!
spanTop
	self
		primitive: 'primSpanTop'
		parameters: #().

	"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	^(spanTop - auxStrokeWidthDilatedHalf + 1) asInteger asOop: SmallInteger! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 20/Aug/2026, 6:18:13 pm (UTC)'!
strokeR: r g: g b: b a: a
	| auxA auxR auxG auxB |
	<var: 'auxA' type: #'uint32_t'>
	<var: 'auxR' type: #'uint32_t'>
	<var: 'auxG' type: #'uint32_t'>
	<var: 'auxB' type: #'uint32_t'>
	self
		primitive: 'primStrokeRGBA'
		parameters: #(Float Float Float Float).

	strokeR := r * 255.0.
	strokeG := g * 255.0.
	strokeB := b * 255.0.
	strokeA := a.
	auxA := (strokeA * 255 + 0.5) asInteger bitShift: 24.
	auxR := (strokeR + 0.5) asInteger bitShift: 16.
	auxG := (strokeG + 0.5) asInteger bitShift: 8.
	auxB := (strokeB + 0.5) asInteger.
	strokeColorWord := ((auxA bitOr: auxR) bitOr: auxG) bitOr: auxB.
! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 8/Apr/2025, 1:36:00 pm (UTC)'!
strokeWidth: aNumber hop: aFloat
	| swErodedHalf |
	<var: 'swErodedHalf' type: #float>
	self
		primitive: 'primStrokeWidthHop'
		parameters: #(Float Float).

	strokeWidth := aNumber.
	hop := aFloat.
	auxStrokeWidthDilatedHalf := strokeWidth + antiAliasingWidth * 0.5.
	auxStrokeWidthDilatedHalfSquared := auxStrokeWidthDilatedHalf * auxStrokeWidthDilatedHalf.
	"swErodedHalf is the inner radious of the alphas ring."
	swErodedHalf := (strokeWidth - antiAliasingWidth) * 0.5 - hop - 2.0.
	auxStrokeWidthErodedHalfSquared := swErodedHalf * swErodedHalf abs.
	
	"Set them later with a call to dashedStrokeBits if desired."
	dashedStrokeBits := 0.
	dashBitCount := 0.
	dashBitLength := 0.0.
	dashBitOffset := 0.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 20/Aug/2026, 9:07:44 pm (UTC)'!
targetAssumedOpaque: aBoolean
	"Set to true for no translucency in target. Slightly better performance.
	Set to false for true translucency on target.
	Ignored for the Whole Pixel primitives. Accounted for in the SubPixel primitives."

	self
		primitive: 'primTargetAssumedOpaque'
		parameters: #(Boolean).

	targetAssumedOpaque := aBoolean.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 20/Aug/2026, 5:49:43 pm (UTC)'!
targetBits: aBitmap morphIds: aWordArray edgeCountsSP: otherWordArray alphaMaskSP: anotherWordArray affectedBits: affectedBitsBA contour: aFloat32Array targetWidth: aNumber targetHeight: otherNumber

	self
		primitive: 'primSetTarget'
		parameters: #(WordArray WordArray WordArray WordArray ByteArray Float32Array SmallInteger SmallInteger).

	targetBits := aBitmap.
	morphIds := aWordArray.
	edgeCountsSP := otherWordArray.
	alphaMaskSP := anotherWordArray.
	affectedBits := affectedBitsBA.
	contour := aFloat32Array.
	targetWidth := aNumber.
	targetHeight := otherNumber.
	clippingSpec := nil.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'eem 14/Nov/2025, 10:34:00 pm (UTC)'!
targetBits: aBitmap morphIds: aWordArray edgeCountsWP: otherByteArray alphaMaskWP: anotherByteArray affectedBits: affectedBitsBA contour: aFloat32Array targetWidth: aNumber targetHeight: otherNumber

	self
		primitive: 'primSetTargetWP'
		parameters: #(WordArray WordArray ByteArray ByteArray ByteArray Float32Array SmallInteger SmallInteger).

	targetBits := aBitmap.
	morphIds := aWordArray.
	edgeCountsWP := otherByteArray.
	alphaMaskWP := anotherByteArray.
	affectedBits := affectedBitsBA.
	contour := aFloat32Array.
	targetWidth := aNumber.
	targetHeight := otherNumber.
	clippingSpec := nil.! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - paths algorithms' stamp: 'jmv 20/Aug/2026, 6:01:48 pm (UTC)'!
arcSPCenterX: centerX centerY: centerY radiusX: radiusPointX radiusY: radiusPointY start: startAngle sweep: sweepAngle rotationCos: tthetaCos rotationSin: tthetaSin

	| tcx tcy trx try scale hops d angle xp yp x y |
	<var: 'tcx' type: #float>
	<var: 'tcy' type: #float>
	<var: 'trx' type: #float>
	<var: 'try' type: #float>
	<var: 'scale' type: #float>
	<var: 'angle' type: #float>
	<var: 'xp' type: #float>
	<var: 'yp' type: #float>
	<var: 'x' type: #float>
	<var: 'y' type: #float>
	<var: 'hops' type: #int>
	<var: 'd' type: #float>
	self
		primitive: 'primArc'
		parameters: #(Float Float Float Float Float Float Float Float).

	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	tcx := (centerX * txA11) + (centerY * txA12) + txA13.
	tcy := (centerX * txA21) + (centerY * txA22) + txA23.
	scale := (txA11 squared + txA21 squared) sqrt.
	trx := radiusPointX * scale.
	try := radiusPointY * scale.
	hops := ((trx max: try) * (sweepAngle abs) / hop) asInteger + 2.
	d := hops.

	0 to: hops do: [ :h |
		angle := h / d * sweepAngle + startAngle.
		xp := angle cos * trx.
		yp := angle sin * try.
		x := tthetaCos * xp - (tthetaSin * yp) + tcx.
		y := tthetaSin * xp + (tthetaCos * yp) + tcy.
		spanLeft := spanLeft min: x.
		spanTop := spanTop min: y.
		spanRight := spanRight max: x.
		spanBottom := spanBottom max: y.
		self updateAlphasSPForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountSPAtX: x y: y ].
		self updateContourForX: x y: y. ].! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - paths algorithms' stamp: 'jmv 20/Aug/2026, 5:54:37 pm (UTC)'!
cubicBezierSPFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2

	self
		primitive: 'primCubicBezier'
		parameters: #(Float Float Float Float Float Float Float Float).

	self pvt_cubicBezierSPFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - paths algorithms' stamp: 'jmv 20/Aug/2026, 5:55:44 pm (UTC)'!
lineFromSPX: xFrom y: yFrom toX: xTo y: yTo

	self
		primitive: 'primLine'
		parameters: #(Float Float Float Float).

	self pvt_lineSPFromX: xFrom y: yFrom toX: xTo y: yTo.! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - paths algorithms' stamp: 'jmv 20/Aug/2026, 6:31:01 pm (UTC)'!
pvt_cubicBezierSPFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2

	<var: 'xFrom' type: #float>
	<var: 'yFrom' type: #float>
	<var: 'xTo' type: #float>
	<var: 'yTo' type: #float>
	<var: 'xControl1' type: #float>
	<var: 'yControl1' type: #float>
	<var: 'xControl2' type: #float>
	<var: 'yControl2' type: #float>
	| txFrom tyFrom txTo tyTo txControl1 tyControl1 txControl2 tyControl2 dx dy xMinEnd xMaxEnd yMinEnd yMaxEnd t oneLessT increment f1 f23 f2 f3 f4 x y t0 x0 y0 length correction |
	<var: 'txFrom' type: #float>
	<var: 'tyFrom' type: #float>
	<var: 'txTo' type: #float>
	<var: 'tyTo' type: #float>
	<var: 'txControl1' type: #float>
	<var: 'tyControl1' type: #float>
	<var: 'txControl2' type: #float>
	<var: 'tyControl2' type: #float>
	<var: 'dx' type: #float>
	<var: 'dy' type: #float>
	<var: 't' type: #float>
	<var: 'oneLessT' type: #float>
	<var: 'increment' type: #float>
	<var: 'f1' type: #float>
	<var: 'f23' type: #float>
	<var: 'f2' type: #float>
	<var: 'f3' type: #float>
	<var: 'f4' type: #float>
	<var: 'x' type: #float>
	<var: 'y' type: #float>
	<var: 'xMinEnd' type: #float>
	<var: 'xMaxEnd' type: #float>
	<var: 'yMinEnd' type: #float>
	<var: 'yMaxEnd' type: #float>
	<var: 'length' type: #float>
	<var: 'correction' type: #float>
	<var: 't0' type: #float>
	<var: 'x0' type: #float>
	<var: 'y0' type: #float>
	
	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	txFrom := (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom := (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo := (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo := (xTo * txA21) + (yTo * txA22) + txA23.
	txControl1 := (xControl1 * txA11) + (yControl1 * txA12) + txA13.
	tyControl1 := (xControl1 * txA21) + (yControl1 * txA22) + txA23.
	txControl2 := (xControl2 * txA11) + (yControl2 * txA12) + txA13.
	tyControl2 := (xControl2 * txA21) + (yControl2 * txA22) + txA23.

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
	self updateAlphasSPForX: x y: y.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountSPAtX: x y: y ].
	self updateContourForX: x y: y.

	dx := (txTo-txFrom) abs.
	dy := (tyTo-tyFrom) abs.
	
	"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
	increment := 0.5 / (dx+dy max: 1.0).
	t := 0.0.
	[
		t0 := t. x0 := x. y0 := y.
		"Compute next point"
		t := t0 + increment. oneLessT := 1.0 - t.
		f1 := oneLessT * oneLessT * oneLessT. f23 := 3.0 * oneLessT * t. f2 := f23 * oneLessT. f3 := f23 * t. f4 := t * t * t.
		x := (f1 * txFrom) + (f2 * txControl1) + (f3 * txControl2) + (f4 * txTo).
		y := (f1 * tyFrom) + (f2 * tyControl1) + (f3 * tyControl2) + (f4 * tyTo).
		"Now adjust the increment to aim at the required hop length, and recompute next point."
		dx := x-x0. dy := y-y0.
		length := ((dx*dx) + (dy*dy)) sqrt.
		correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
		[
			increment := increment * correction.
			t := t0 + increment. oneLessT := 1.0 - t.
			f1 := oneLessT * oneLessT * oneLessT. f23 := 3.0 * oneLessT * t. f2 := f23 * oneLessT. f3 := f23 * t. f4 := t * t * t.
			x := (f1 * txFrom) + (f2 * txControl1) + (f3 * txControl2) + (f4 * txTo).
			y := (f1 * tyFrom) + (f2 * tyControl1) + (f3 * tyControl2) + (f4 * tyTo).
			dx := x-x0. dy := y-y0.
			length := ((dx*dx) + (dy*dy)) sqrt.
			correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
			correction < 0.99] whileTrue.		   "Keep adjusting if actual hop would be too big"
		t < 1.0 ]
	whileTrue: [
		self updateAlphasSPForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountSPAtX: x y: y ].
		self updateContourForX: x y: y ].

	"Case t= 1.0"
	self updateAlphasSPForX: txTo y: tyTo.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountSPAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - paths algorithms' stamp: 'jmv 20/Aug/2026, 6:00:18 pm (UTC)'!
pvt_lineSPFromX: xFrom y: yFrom toX: xTo y: yTo

	<var: 'xFrom' type: #float>
	<var: 'yFrom' type: #float>
	<var: 'xTo' type: #float>
	<var: 'yTo' type: #float>
	| txFrom tyFrom txTo tyTo dx dy hops t oneLessT increment x y |
	<var: 'txFrom' type: #float>
	<var: 'tyFrom' type: #float>
	<var: 'txTo' type: #float>
	<var: 'tyTo' type: #float>
	<var: 'dx' type: #float>
	<var: 'dy' type: #float>
	<var: 'hops' type: #int>
	<var: 't' type: #float>
	<var: 'oneLessT' type: #float>
	<var: 'increment' type: #float>
	<var: 'x' type: #float>
	<var: 'y' type: #float>
	
	txFrom := (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom := (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo := (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo := (xTo * txA21) + (yTo * txA22) + txA23.

	spanLeft := spanLeft min: (txFrom min: txTo).
	spanRight := spanRight max: (txFrom max: txTo).
	spanTop := spanTop min: (tyFrom min: tyTo).
	spanBottom := spanBottom max: (tyFrom max: tyTo).

	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	dx := txTo-txFrom.
	dy := tyTo-tyFrom.
	dx := dx abs.
	dy := dy abs.
	hops := ((dx max: dy) / hop) asInteger + 1.
	increment := 1.0 / hops.
	t := 0.0.
	[ t < 1.0 ] whileTrue: [
		oneLessT := 1.0 - t.
		x := (oneLessT * txFrom) + (t * txTo).
		y := (oneLessT * tyFrom) + (t * tyTo).
		self updateAlphasSPForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountSPAtX: x y: y ].
		self updateContourForX: x y: y.
		t := t + increment. ].
	self updateAlphasSPForX: txTo y: tyTo.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountSPAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - paths algorithms' stamp: 'jmv 20/Aug/2026, 6:32:07 pm (UTC)'!
pvt_quadraticBezierSPFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl

	<var: 'xFrom' type: #float>
	<var: 'yFrom' type: #float>
	<var: 'xTo' type: #float>
	<var: 'yTo' type: #float>
	<var: 'xControl' type: #float>
	<var: 'yControl' type: #float>
	| txFrom tyFrom txTo tyTo txControl tyControl dx dy dx2 dy2 length xMinEnd xMaxEnd yMinEnd yMaxEnd t0 x0 y0 t oneLessT increment f1 f2 f3 x y correction |
	<var: 'txFrom' type: #float>
	<var: 'tyFrom' type: #float>
	<var: 'txTo' type: #float>
	<var: 'tyTo' type: #float>
	<var: 'txControl' type: #float>
	<var: 'tyControl' type: #float>
	<var: 'dx' type: #float>
	<var: 'dy' type: #float>
	<var: 'dx2' type: #float>
	<var: 'dy2' type: #float>
	<var: 'length' type: #float>
	<var: 'correction' type: #float>
	<var: 't0' type: #float>
	<var: 'x0' type: #float>
	<var: 'y0' type: #float>
	<var: 't' type: #float>
	<var: 'oneLessT' type: #float>
	<var: 'increment' type: #float>
	<var: 'f1' type: #float>
	<var: 'f2' type: #float>
	<var: 'f3' type: #float>
	<var: 'x' type: #float>
	<var: 'y' type: #float>
	<var: 'xMinEnd' type: #float>
	<var: 'xMaxEnd' type: #float>
	<var: 'yMinEnd' type: #float>
	<var: 'yMaxEnd' type: #float>
	

	"If control point is bogus, just draw a line"
	(xControl = xTo and: [ yControl = yTo ]) ifTrue: [
		^self pvt_lineSPFromX: xFrom y: yFrom toX: xTo y: yTo ].
	(xControl = xFrom and: [ yControl = yFrom ]) ifTrue: [
		^self pvt_lineSPFromX: xFrom y: yFrom toX: xTo y: yTo ].

	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	txFrom := (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom := (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo := (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo := (xTo * txA21) + (yTo * txA22) + txA23.
	txControl := (xControl * txA11) + (yControl * txA12) + txA13.
	tyControl := (xControl * txA21) + (yControl * txA22) + txA23.
	dx := (txTo-txFrom) abs.
	dx2 := (txControl-txFrom) abs.
	dy := (tyTo-tyFrom) abs.
	dy2 := (tyControl-tyFrom) abs.

	"If almost a vertical line, just draw a line. (Ignoring control point)"
	(dx < 1.0 and: [dx2 < 1.0]) ifTrue: [
		^self pvt_lineSPFromX: xFrom y: yFrom toX: xTo y: yTo ].

	"If almost an horizontal line, just draw a line. (Ignoring control point)"
	(dy < 1.0 and: [dy2 < 1.0]) ifTrue: [
		^self pvt_lineSPFromX: xFrom y: yFrom toX: xTo y: yTo ].

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
	self updateAlphasSPForX: x y: y.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountSPAtX: x y: y ].
	self updateContourForX: x y: y.

	"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
	increment := 0.5 / (dx+dy max: 1.0).
	t := 0.0.
	[
		t0 := t. x0 := x. y0 := y.
		"Compute next point"
		t := t0 + increment. oneLessT := 1.0 - t.
		f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
		x := (f1 * txFrom) + (f2 * txControl) + (f3 * txTo).
		y := (f1 * tyFrom) + (f2 * tyControl) + (f3 * tyTo).
		"Now adjust the increment to aim at the required hop length, and recompute next point."
		dx := x-x0. dy := y-y0.
		length := ((dx*dx) + (dy*dy)) sqrt.
		correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
		[
			increment := increment * correction.
			t := t0 + increment. oneLessT := 1.0 - t.
			f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
			x := (f1 * txFrom) + (f2 * txControl) + (f3 * txTo).
			y := (f1 * tyFrom) + (f2 * tyControl) + (f3 * tyTo).
			dx := x-x0. dy := y-y0.
			length := ((dx*dx) + (dy*dy)) sqrt.
			correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
			correction < 0.99] whileTrue.		   "Keep adjusting if actual hop would be too big"
		t < 1.0 ]
	whileTrue: [
		self updateAlphasSPForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountSPAtX: x y: y ].
		self updateContourForX: x y: y ].

	"Case t= 1.0"
	self updateAlphasSPForX: txTo y: tyTo.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountSPAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - paths algorithms' stamp: 'jmv 20/Aug/2026, 5:57:15 pm (UTC)'!
quadraticBezierSPFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl

	self
		primitive: 'primQuadraticBezier'
		parameters: #(Float Float Float Float Float Float).

	self pvt_quadraticBezierSPFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl.! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - paths algorithms' stamp: 'jmv 20/Aug/2026, 5:51:27 pm (UTC)'!
updateAlphasSPForX: x y: y
	
	"In a basic implementation, this method computes anti aliasing translucency for all possibly affected pixels as determined by
	the outer radius of the pen nib: auxStrokeWidthDilatedHalf.
	But usually the next point visited is very close to the last, and there's no real need to fill the inner part of the pen nib as opaque,
	as it was already set. So, unless needsFullAlphaCircle, only the outer ring is computed and stored.
	The inner radius of this ring is auxStrokeWidthErodedHalfSquared"

	<var: 'x' type: #float>
	<var: 'y' type: #float>
	| t b l r pixelIndex candidateAlpha alphaWord redAlpha greenAlpha blueAlpha doUpdate distanceToAxisSquared dy dySquared dx dxp bit lastUpdated affectedBitsIndex |
	<var: 'candidateAlpha' type: #'uint32_t'>
	<var: 'alphaWord' type: #'uint32_t'>
	<var: 'redAlpha' type: #'uint32_t'>
	<var: 'greenAlpha' type: #'uint32_t'>
	<var: 'blueAlpha' type: #'uint32_t'>
	<var: 'distanceToAxisSquared' type: #float>
	<var: 'dy' type: #float>
	<var: 'dySquared' type: #float>
	<var: 'dx' type: #float>
	<var: 'dxp' type: #float>
	
	"If dashed strokes, only draw if in a dash, not in a gap."
	dashBitLength = 0.0 ifFalse: [
		"Compute trajectory length. This is not precise. In many cases the actual hop used is smaller than this."
		trajectoryLength := trajectoryLength + hop.
		"Note: dashBitOffset must be positive."
		bit := ((trajectoryLength / dashBitLength) asInteger + dashBitOffset) \\ dashBitCount.
		(dashedStrokeBits bitAnd: 1 << (dashBitCount-bit-1)) = 0 ifTrue: [
			needsFullAlphaCircle := true.
			^self ]].

	"Compute affected rect. Honor clipRect"
	t := (y - auxStrokeWidthDilatedHalf + 1) asInteger.					"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	t < clipTop ifTrue: [ t := clipTop ].
	b := (y + auxStrokeWidthDilatedHalf) asInteger.
	b > clipBottom ifTrue: [ b := clipBottom ].
	l := (x - auxStrokeWidthDilatedHalf - subPixelDelta + 1) asInteger.	"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	l < clipLeft ifTrue: [ l := clipLeft ].
	r := (x + auxStrokeWidthDilatedHalf + subPixelDelta) asInteger.
	r > clipRight ifTrue: [ r := clipRight ].

	lastUpdated := -1.
	t to: b do: [ :displayY |
		pixelIndex := displayY * targetWidth + l -1.
		dy := displayY - y.
		dySquared := dy * dy.
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			dx := displayX - x.
			distanceToAxisSquared := dx * dx + dySquared. "Use Green subpixel for this."
			(needsFullAlphaCircle or: [distanceToAxisSquared > auxStrokeWidthErodedHalfSquared]) ifTrue: [
				alphaWord := alphaMaskSP at: pixelIndex.
				alphaWord = 16r007F7F7F ifFalse: [
					redAlpha := alphaWord bitAnd: 16r7F0000.
					greenAlpha := alphaWord bitAnd: 16r7F00.
					blueAlpha := alphaWord bitAnd: 16r7F.
					doUpdate := false.
					"Red"
					dxp := dx - subPixelDelta.
					distanceToAxisSquared := dxp * dxp + dySquared.
					distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
						ifTrue: [
							candidateAlpha := (auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt * auxAntiAliasingWidthScaledInverse) asInteger.
							candidateAlpha := candidateAlpha bitShift: 16.
							candidateAlpha > redAlpha ifTrue: [
								doUpdate := true.
								redAlpha := (candidateAlpha min: 16r7F0000) ]].
					"Green"
					distanceToAxisSquared := dx * dx + dySquared.
					distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
						ifTrue: [
							candidateAlpha := (auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt * auxAntiAliasingWidthScaledInverse) asInteger.
							candidateAlpha := candidateAlpha bitShift: 8.
							candidateAlpha > greenAlpha ifTrue: [
								doUpdate := true.
								greenAlpha := (candidateAlpha min: 16r7F00) ]].
					"Blue"
					dxp := dx + subPixelDelta.
					distanceToAxisSquared := dxp * dxp + dySquared.
					distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
						ifTrue: [
							candidateAlpha := (auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt * auxAntiAliasingWidthScaledInverse) asInteger.
							candidateAlpha > blueAlpha ifTrue: [
								doUpdate := true.
								blueAlpha := (candidateAlpha min: 16r7F) ]].
					doUpdate ifTrue: [
						alphaWord := (redAlpha bitOr: greenAlpha) bitOr: blueAlpha.
						alphaMaskSP at: pixelIndex put: alphaWord.
						affectedBitsIndex := pixelIndex>>4.
						affectedBitsIndex = lastUpdated ifFalse: [
							affectedBits at: affectedBitsIndex put: 1.
							lastUpdated := affectedBitsIndex ]]]]]].
	needsFullAlphaCircle := false.! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - paths algorithms' stamp: 'jmv 20/Aug/2026, 5:49:21 pm (UTC)'!
updateEdgeCountSPAtX: x y: y
	"Compute edges intersecting with this horizontal line, for fills."

	<var: 'x' type: #float>
	<var: 'y' type: #float>
	| thisYTruncated pixelY pixelIndexBase redPixelIndex greenPixelIndex bluePixelIndex redIncrement greenIncrement blueIncrement countWord redCount greenCount blueCount rest redOffset greenOffset blueOffset affectedBitsIndex |

	<var: 'countWord' type: #'uint32_t'>
	<var: 'redCount' type: #'uint32_t'>
	<var: 'greenCount' type: #'uint32_t'>
	<var: 'blueCount' type: #'uint32_t'>
	<var: 'redIncrement' type: #'uint32_t'>
	<var: 'greenIncrement' type: #'uint32_t'>
	<var: 'blueIncrement' type: #'uint32_t'>
	

	thisYTruncated := y asInteger.	"truncated, both in C and Smalltalk"
	thisYTruncated = prevYTruncated ifTrue: [
		^ 0].
	(thisYTruncated between: clipTop-1 and: clipBottom) ifFalse: [
		^ 0 ].
	prevYTruncated = 16r7FFFFFFF ifTrue: [ 
		prevYTruncated := thisYTruncated.
		^ 0 ].

	thisYTruncated > prevYTruncated
		ifTrue: [
			pixelY := thisYTruncated.
			redIncrement := 16r010000.
			greenIncrement := 16r0100.
			blueIncrement := 16r01 ]
		ifFalse: [
			pixelY := prevYTruncated.
			redIncrement := 16rFF0000.
			greenIncrement := 16rFF00.
			blueIncrement := 16rFF ].
	prevYTruncated := thisYTruncated.

	"All edge count at the left of the clipRect are added there (at the left of the clipRect).
	The effect is the same, and we need to clean up less stuff afterwards.
	More important, it avoids trying to acess pixels outside our form, i.e. invalid array acesses."
	pixelIndexBase := pixelY * targetWidth.
	redOffset := (x+subPixelDelta+1) asInteger max: clipLeft.		"take the next red subpixel center to the right of x"
	greenOffset := (x+1) asInteger max: clipLeft.					"take the next green subpixel center to the right of x"
	blueOffset := (x-subPixelDelta+1) asInteger max: clipLeft.		"take the next blue subpixel center to the right of x"
	redPixelIndex := pixelIndexBase + redOffset.
	greenPixelIndex := pixelIndexBase + greenOffset.
	bluePixelIndex := pixelIndexBase + blueOffset.

	"Three possible cases here: RGB in one word (pixel); RG in one, and G in another; R in one, GB in another"
	redPixelIndex = bluePixelIndex
		ifTrue: [						"First case: RGB in the same word"
			redOffset <= clipRight ifTrue: [
				countWord := edgeCountsSP at: redPixelIndex.
				redCount := (countWord + redIncrement) bitAnd: 16rFF0000.
				greenCount := (countWord + greenIncrement) bitAnd: 16rFF00.
				blueCount := (countWord + blueIncrement) bitAnd: 16rFF.
				countWord := (redCount bitOr: greenCount) bitOr: blueCount.
				edgeCountsSP at: redPixelIndex put: countWord.
				affectedBitsIndex := redPixelIndex>>4.
				affectedBits at: affectedBitsIndex put: 1 ].
		]
		ifFalse: [
			redPixelIndex = greenPixelIndex
				ifTrue: [				"Second case: RG in one word, B in previous"
					redOffset <= clipRight ifTrue: [
						countWord := edgeCountsSP at: redPixelIndex.
						redCount := (countWord + redIncrement) bitAnd: 16rFF0000.
						greenCount := (countWord + greenIncrement) bitAnd: 16rFF00.
						rest := countWord bitAnd: 16rFF.
						countWord := (redCount bitOr: greenCount) bitOr: rest.
						edgeCountsSP at: redPixelIndex put: countWord ].

					blueOffset <= clipRight ifTrue: [
						countWord := edgeCountsSP at: bluePixelIndex.
						rest := countWord bitAnd: 16rFFFF00.
						blueCount := (countWord + blueIncrement) bitAnd: 16rFF.
						countWord := rest bitOr: blueCount.
						edgeCountsSP at: bluePixelIndex put: countWord ].
				]
				ifFalse: [			"Third case: R in one word, GB in the previous"
					redOffset <= clipRight ifTrue: [
						countWord := edgeCountsSP at: redPixelIndex.
						redCount := (countWord + redIncrement) bitAnd: 16rFF0000.
						rest := countWord bitAnd: 16rFFFF.
						countWord := redCount bitOr: rest.
						edgeCountsSP at: redPixelIndex put: countWord ].

					blueOffset <= clipRight ifTrue: [
						countWord := edgeCountsSP at: bluePixelIndex.
						rest := countWord bitAnd: 16rFF0000.
						greenCount := (countWord + greenIncrement) bitAnd: 16rFF00.
						blueCount := (countWord + blueIncrement) bitAnd: 16rFF.
						countWord := (rest bitOr: greenCount) bitOr: blueCount.
						edgeCountsSP at: bluePixelIndex put: countWord ].
				].
				affectedBitsIndex := redPixelIndex>>4.
				affectedBits at: affectedBitsIndex put: 1.
				affectedBitsIndex := bluePixelIndex>>4.
				affectedBits at: affectedBitsIndex put: 1.
		].! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - path algorithms' stamp: 'jmv 8/Apr/2025, 2:49:00 pm (UTC)'!
arcWPCenterX: centerX centerY: centerY radiusX: radiusPointX radiusY: radiusPointY start: startAngle sweep: sweepAngle rotationCos: tthetaCos rotationSin: tthetaSin

	| tcx tcy trx try scale hops d angle xp yp x y |
	<var: 'tcx' type: #float>
	<var: 'tcy' type: #float>
	<var: 'trx' type: #float>
	<var: 'try' type: #float>
	<var: 'scale' type: #float>
	<var: 'angle' type: #float>
	<var: 'xp' type: #float>
	<var: 'yp' type: #float>
	<var: 'x' type: #float>
	<var: 'y' type: #float>
	<var: 'hops' type: #int>
	<var: 'd' type: #float>
	self
		primitive: 'primArcWP'
		parameters: #(Float Float Float Float Float Float Float Float).

	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	tcx := (centerX * txA11) + (centerY * txA12) + txA13.
	tcy := (centerX * txA21) + (centerY * txA22) + txA23.
	scale := (txA11 squared + txA21 squared) sqrt.
	trx := radiusPointX * scale.
	try := radiusPointY * scale.
	hops := ((trx max: try) * (sweepAngle abs) / hop) asInteger + 2.
	d := hops.

	0 to: hops do: [ :h |
		angle := h / d * sweepAngle + startAngle.
		xp := angle cos * trx.
		yp := angle sin * try.
		x := tthetaCos * xp - (tthetaSin * yp) + tcx.
		y := tthetaSin * xp + (tthetaCos * yp) + tcy.
		spanLeft := spanLeft min: x.
		spanTop := spanTop min: y.
		spanRight := spanRight max: x.
		spanBottom := spanBottom max: y.
		self updateAlphasWPForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountWPAtX: x y: y ].
		self updateContourForX: x y: y. ].! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - path algorithms' stamp: 'jmv 8/Apr/2025, 2:52:00 pm (UTC)'!
cubicBezierWPFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2

	self
		primitive: 'primCubicBezierWP'
		parameters: #(Float Float Float Float Float Float Float Float).

	self pvt_cubicBezierWPFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - path algorithms' stamp: 'jmv 8/Apr/2025, 2:53:00 pm (UTC)'!
lineWPFromX: xFrom y: yFrom toX: xTo y: yTo

	self
		primitive: 'primLineWP'
		parameters: #(Float Float Float Float).

	self pvt_lineWPFromX: xFrom y: yFrom toX: xTo y: yTo.! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - path algorithms' stamp: 'jmv 19/Aug/2026, 12:49:45 pm (UTC)'!
pvt_basicHorizontalLineWPAtY: y fromX: x0 toX: x1
	"Optimization for simple horizontal lines (in target space).
	Assumptions:
		- Stroke Only (no fill)
		- No dashes (dashBitLength = 0.0)
		- x1 > x0"
	<var: 'y' type: #float>
	<var: 'x0' type: #float>
	<var: 'x1' type: #float>
	| t b l r pixelIndex candidateAlpha prevAlpha signedDistanceToAxis distanceToOuterEdgeOfAA affectedBitsIndex nextPixelIndexToUpdateAffectedBits |
	<var: 'candidateAlpha' type: #'uint8_t'>
	<var: 'prevAlpha' type: #'uint8_t'>
	<var: 'signedDistanceToAxis' type: #float>
	<var: 'distanceToOuterEdgeOfAA' type: #float>

	needsFullAlphaCircle := true.
	self updateAlphasWPForX: x0 y: y.
	self updateContourForX: x0 y: y.

	"Compute affected rect. Honor clipRect."
	t := (y - auxStrokeWidthDilatedHalf + 1) asInteger.
	t < clipTop ifTrue: [ t := clipTop ].
	b := (y + auxStrokeWidthDilatedHalf) asInteger.
	b > clipBottom ifTrue: [ b := clipBottom ].
	l := x0 asInteger + 1.
	l < clipLeft ifTrue: [ l := clipLeft ].
	r := x1 asInteger.
	r > clipRight ifTrue: [ r := clipRight ].

	signedDistanceToAxis := t - y - 1.0.
	t to: b do: [ :displayY |
		pixelIndex := displayY * targetWidth + l-1.
		nextPixelIndexToUpdateAffectedBits := pixelIndex+1.
		candidateAlpha := 16r7F.
		signedDistanceToAxis := signedDistanceToAxis + 1.0.
		distanceToOuterEdgeOfAA := auxStrokeWidthDilatedHalf - signedDistanceToAxis abs.
		distanceToOuterEdgeOfAA < antiAliasingWidth
			ifTrue: [ candidateAlpha := (distanceToOuterEdgeOfAA * auxAntiAliasingWidthScaledInverse) asInteger].
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			prevAlpha := alphaMaskWP at: pixelIndex.
			candidateAlpha > prevAlpha ifTrue: [
				alphaMaskWP at: pixelIndex put: candidateAlpha.
				pixelIndex >= nextPixelIndexToUpdateAffectedBits ifTrue: [
					affectedBitsIndex := pixelIndex>>4.
					affectedBits at: affectedBitsIndex put: 1.
					nextPixelIndexToUpdateAffectedBits := (pixelIndex bitAnd: 16rFFFFFFF0) + 16r10 ]]]].


	needsFullAlphaCircle := true.
	self updateAlphasWPForX: x1 y: y.
	self updateContourForX: x1 y: y.! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - path algorithms' stamp: 'jmv 19/Aug/2026, 12:49:48 pm (UTC)'!
pvt_basicVerticalLineWPAtX: x fromY: y0 toY: y1
	"Optimization for simple vertical lines (in target space).
	Assumptions:
		- Stroke Only (no fill)
		- No dashes (dashBitLength = 0.0)
		- y1 > y0"
	<var: 'x' type: #float>
	<var: 'y0' type: #float>
	<var: 'y1' type: #float>
	| t b l r pixelIndex candidateAlpha prevAlpha signedDistanceToAxis distanceToOuterEdgeOfAA affectedBitsIndex nextPixelIndexToUpdateAffectedBits |
	<var: 'candidateAlpha' type: #'uint8_t'>
	<var: 'prevAlpha' type: #'uint8_t'>
	<var: 'signedDistanceToAxis' type: #float>
	<var: 'distanceToOuterEdgeOfAA' type: #float>
	
	needsFullAlphaCircle := true.
	self updateAlphasWPForX: x y: y0.
	self updateContourForX: x y: y0.

	"Compute affected rect. Honor clipRect."
	t := y0 asInteger + 1.
	t < clipTop ifTrue: [ t := clipTop ].
	b := y1 asInteger.
	b > clipBottom ifTrue: [ b := clipBottom ].
	l := (x - auxStrokeWidthDilatedHalf + 1) asInteger.
	l < clipLeft ifTrue: [ l := clipLeft ].
	r := (x + auxStrokeWidthDilatedHalf) asInteger.
	r > clipRight ifTrue: [ r := clipRight ].
	
	t to: b do: [ :displayY |
		pixelIndex := displayY * targetWidth + l - 1.
		nextPixelIndexToUpdateAffectedBits := pixelIndex+1.
		signedDistanceToAxis := l - x - 1.0.
		l to: r do: [ :displayX |
			candidateAlpha := 16r7F.
			signedDistanceToAxis := signedDistanceToAxis + 1.0.
			distanceToOuterEdgeOfAA := auxStrokeWidthDilatedHalf - signedDistanceToAxis abs.
			distanceToOuterEdgeOfAA < antiAliasingWidth
				ifTrue: [ candidateAlpha := (distanceToOuterEdgeOfAA * auxAntiAliasingWidthScaledInverse) asInteger].
			pixelIndex := pixelIndex + 1.
			prevAlpha := alphaMaskWP at: pixelIndex.
			candidateAlpha > prevAlpha ifTrue: [
				alphaMaskWP at: pixelIndex put: candidateAlpha.
				pixelIndex >= nextPixelIndexToUpdateAffectedBits ifTrue: [
					affectedBitsIndex := pixelIndex>>4.
					affectedBits at: affectedBitsIndex put: 1.
					nextPixelIndexToUpdateAffectedBits := (pixelIndex bitAnd: 16rFFFFFFF0) + 16r10 ]]]].


	needsFullAlphaCircle := true.
	self updateAlphasWPForX: x y: y1.
	self updateContourForX: x y: y1.! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - path algorithms' stamp: 'jmv 20/Aug/2026, 6:31:11 pm (UTC)'!
pvt_cubicBezierWPFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2

	<var: 'xFrom' type: #float>
	<var: 'yFrom' type: #float>
	<var: 'xTo' type: #float>
	<var: 'yTo' type: #float>
	<var: 'xControl1' type: #float>
	<var: 'yControl1' type: #float>
	<var: 'xControl2' type: #float>
	<var: 'yControl2' type: #float>
	| txFrom tyFrom txTo tyTo txControl1 tyControl1 txControl2 tyControl2 dx dy xMinEnd xMaxEnd yMinEnd yMaxEnd t oneLessT increment f1 f23 f2 f3 f4 x y t0 x0 y0 length correction |
	<var: 'txFrom' type: #float>
	<var: 'tyFrom' type: #float>
	<var: 'txTo' type: #float>
	<var: 'tyTo' type: #float>
	<var: 'txControl1' type: #float>
	<var: 'tyControl1' type: #float>
	<var: 'txControl2' type: #float>
	<var: 'tyControl2' type: #float>
	<var: 'dx' type: #float>
	<var: 'dy' type: #float>
	<var: 't' type: #float>
	<var: 'oneLessT' type: #float>
	<var: 'increment' type: #float>
	<var: 'f1' type: #float>
	<var: 'f23' type: #float>
	<var: 'f2' type: #float>
	<var: 'f3' type: #float>
	<var: 'f4' type: #float>
	<var: 'x' type: #float>
	<var: 'y' type: #float>
	<var: 'xMinEnd' type: #float>
	<var: 'xMaxEnd' type: #float>
	<var: 'yMinEnd' type: #float>
	<var: 'yMaxEnd' type: #float>
	<var: 'length' type: #float>
	<var: 'correction' type: #float>
	<var: 't0' type: #float>
	<var: 'x0' type: #float>
	<var: 'y0' type: #float>
	
	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	txFrom := (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom := (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo := (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo := (xTo * txA21) + (yTo * txA22) + txA23.
	txControl1 := (xControl1 * txA11) + (yControl1 * txA12) + txA13.
	tyControl1 := (xControl1 * txA21) + (yControl1 * txA22) + txA23.
	txControl2 := (xControl2 * txA11) + (yControl2 * txA12) + txA13.
	tyControl2 := (xControl2 * txA21) + (yControl2 * txA22) + txA23.

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
	self updateAlphasWPForX: x y: y.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountWPAtX: x y: y ].
	self updateContourForX: x y: y.

	dx := (txTo-txFrom) abs.
	dy := (tyTo-tyFrom) abs.
	
	"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
	increment := 0.5 / (dx+dy max: 1.0).
	t := 0.0.
	[
		t0 := t. x0 := x. y0 := y.
		"Compute next point"
		t := t0 + increment. oneLessT := 1.0 - t.
		f1 := oneLessT * oneLessT * oneLessT. f23 := 3.0 * oneLessT * t. f2 := f23 * oneLessT. f3 := f23 * t. f4 := t * t * t.
		x := (f1 * txFrom) + (f2 * txControl1) + (f3 * txControl2) + (f4 * txTo).
		y := (f1 * tyFrom) + (f2 * tyControl1) + (f3 * tyControl2) + (f4 * tyTo).
		"Now adjust the increment to aim at the required hop length, and recompute next point."
		dx := x-x0. dy := y-y0.
		length := ((dx*dx) + (dy*dy)) sqrt.
		correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
		[
			increment := increment * correction.
			t := t0 + increment. oneLessT := 1.0 - t.
			f1 := oneLessT * oneLessT * oneLessT. f23 := 3.0 * oneLessT * t. f2 := f23 * oneLessT. f3 := f23 * t. f4 := t * t * t.
			x := (f1 * txFrom) + (f2 * txControl1) + (f3 * txControl2) + (f4 * txTo).
			y := (f1 * tyFrom) + (f2 * tyControl1) + (f3 * tyControl2) + (f4 * tyTo).
			dx := x-x0. dy := y-y0.
			length := ((dx*dx) + (dy*dy)) sqrt.
			correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
			correction < 0.99] whileTrue.		   "Keep adjusting if actual hop would be too big"
		t < 1.0 ]
	whileTrue: [
		self updateAlphasWPForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountWPAtX: x y: y ].
		self updateContourForX: x y: y ].

	"Case t= 1.0"
	self updateAlphasWPForX: txTo y: tyTo.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountWPAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - path algorithms' stamp: 'jmv 18/Aug/2026, 5:55:22 pm (UTC)'!
pvt_lineWPFromX: xFrom y: yFrom toX: xTo y: yTo

	<var: 'xFrom' type: #float>
	<var: 'yFrom' type: #float>
	<var: 'xTo' type: #float>
	<var: 'yTo' type: #float>
	| txFrom tyFrom txTo tyTo x0 x1 y0 y1 dx dy hops t oneLessT increment x y |
	<var: 'txFrom' type: #float>
	<var: 'tyFrom' type: #float>
	<var: 'txTo' type: #float>
	<var: 'tyTo' type: #float>
	<var: 'x0' type: #float>
	<var: 'x1' type: #float>
	<var: 'y0' type: #float>
	<var: 'y1' type: #float>
	<var: 'dx' type: #float>
	<var: 'dy' type: #float>
	<var: 'hops' type: #int>
	<var: 't' type: #float>
	<var: 'oneLessT' type: #float>
	<var: 'increment' type: #float>
	<var: 'x' type: #float>
	<var: 'y' type: #float>
	
	txFrom := (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom := (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo := (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo := (xTo * txA21) + (yTo * txA22) + txA23.

	x0 := txFrom min: txTo.
	x1 := txFrom max: txTo.
	y0 := tyFrom min: tyTo.
	y1 := tyFrom max: tyTo.
	spanLeft := spanLeft min: x0.
	spanRight := spanRight max: x1.
	spanTop := spanTop min: y0.
	spanBottom := spanBottom max: y1.

	"Stroke only, and no dashes. Maybe a simple horizontal or vertical line."
	fillA+dashBitLength = 0.0 ifTrue: [ 	"If both are zero"
		y0 = y1 ifTrue: [
			self pvt_basicHorizontalLineWPAtY: y0 fromX: x0 toX: x1.
			^self ].
		x0 = x1 ifTrue: [
			self pvt_basicVerticalLineWPAtX: x0 fromY: y0 toY: y1.
			^self ]].

	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	dx := txTo-txFrom.
	dy := tyTo-tyFrom.
	dx := dx abs.
	dy := dy abs.
	hops := ((dx max: dy) / hop) asInteger + 1.
	increment := 1.0 / hops.
	t := 0.0.
	[ t < 1.0 ] whileTrue: [
		oneLessT := 1.0 - t.
		x := (oneLessT * txFrom) + (t * txTo).
		y := (oneLessT * tyFrom) + (t * tyTo).
		self updateAlphasWPForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountWPAtX: x y: y ].
		self updateContourForX: x y: y.
		t := t + increment. ].
	self updateAlphasWPForX: txTo y: tyTo.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountWPAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - path algorithms' stamp: 'jmv 20/Aug/2026, 6:32:17 pm (UTC)'!
pvt_quadraticBezierWPFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl

	<var: 'xFrom' type: #float>
	<var: 'yFrom' type: #float>
	<var: 'xTo' type: #float>
	<var: 'yTo' type: #float>
	<var: 'xControl' type: #float>
	<var: 'yControl' type: #float>
	| txFrom tyFrom txTo tyTo txControl tyControl dx dy dx2 dy2 length xMinEnd xMaxEnd yMinEnd yMaxEnd t0 x0 y0 t oneLessT increment f1 f2 f3 x y correction |
	<var: 'txFrom' type: #float>
	<var: 'tyFrom' type: #float>
	<var: 'txTo' type: #float>
	<var: 'tyTo' type: #float>
	<var: 'txControl' type: #float>
	<var: 'tyControl' type: #float>
	<var: 'dx' type: #float>
	<var: 'dy' type: #float>
	<var: 'dx2' type: #float>
	<var: 'dy2' type: #float>
	<var: 'length' type: #float>
	<var: 'correction' type: #float>
	<var: 't0' type: #float>
	<var: 'x0' type: #float>
	<var: 'y0' type: #float>
	<var: 't' type: #float>
	<var: 'oneLessT' type: #float>
	<var: 'increment' type: #float>
	<var: 'f1' type: #float>
	<var: 'f2' type: #float>
	<var: 'f3' type: #float>
	<var: 'x' type: #float>
	<var: 'y' type: #float>
	<var: 'xMinEnd' type: #float>
	<var: 'xMaxEnd' type: #float>
	<var: 'yMinEnd' type: #float>
	<var: 'yMaxEnd' type: #float>
	

	"If control point is bogus, just draw a line"
	(xControl = xTo and: [ yControl = yTo ]) ifTrue: [
		^self pvt_lineWPFromX: xFrom y: yFrom toX: xTo y: yTo ].
	(xControl = xFrom and: [ yControl = yFrom ]) ifTrue: [
		^self pvt_lineWPFromX: xFrom y: yFrom toX: xTo y: yTo ].

	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	txFrom := (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom := (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo := (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo := (xTo * txA21) + (yTo * txA22) + txA23.
	txControl := (xControl * txA11) + (yControl * txA12) + txA13.
	tyControl := (xControl * txA21) + (yControl * txA22) + txA23.
	dx := (txTo-txFrom) abs.
	dx2 := (txControl-txFrom) abs.
	dy := (tyTo-tyFrom) abs.
	dy2 := (tyControl-tyFrom) abs.

	"If almost a vertical line, just draw a line. (Ignoring control point)"
	(dx < 1.0 and: [dx2 < 1.0]) ifTrue: [
		^self pvt_lineWPFromX: xFrom y: yFrom toX: xTo y: yTo ].

	"If almost an horizontal line, just draw a line. (Ignoring control point)"
	(dy < 1.0 and: [dy2 < 1.0]) ifTrue: [
		^self pvt_lineWPFromX: xFrom y: yFrom toX: xTo y: yTo ].

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
	self updateAlphasWPForX: x y: y.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountWPAtX: x y: y ].
	self updateContourForX: x y: y.

	"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
	increment := 0.5 / (dx+dy max: 1.0).
	t := 0.0.
	[
		t0 := t. x0 := x. y0 := y.
		"Compute next point"
		t := t0 + increment. oneLessT := 1.0 - t.
		f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
		x := (f1 * txFrom) + (f2 * txControl) + (f3 * txTo).
		y := (f1 * tyFrom) + (f2 * tyControl) + (f3 * tyTo).
		"Now adjust the increment to aim at the required hop length, and recompute next point."
		dx := x-x0. dy := y-y0.
		length := ((dx*dx) + (dy*dy)) sqrt.
		correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
		[
			increment := increment * correction.
			t := t0 + increment. oneLessT := 1.0 - t.
			f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
			x := (f1 * txFrom) + (f2 * txControl) + (f3 * txTo).
			y := (f1 * tyFrom) + (f2 * tyControl) + (f3 * tyTo).
			dx := x-x0. dy := y-y0.
			length := ((dx*dx) + (dy*dy)) sqrt.
			correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
			correction < 0.99] whileTrue.		   "Keep adjusting if actual hop would be too big"
		t < 1.0 ]
	whileTrue: [
		self updateAlphasWPForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountWPAtX: x y: y ].
		self updateContourForX: x y: y ].

	"Case t= 1.0"
	self updateAlphasWPForX: txTo y: tyTo.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountWPAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - path algorithms' stamp: 'jmv 8/Apr/2025, 2:54:00 pm (UTC)'!
quadraticBezierWPFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl

	self
		primitive: 'primQuadraticBezierWP'
		parameters: #(Float Float Float Float Float Float).

	self pvt_quadraticBezierWPFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl.! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - path algorithms' stamp: 'jmv 18/Aug/2026, 4:00:58 pm (UTC)'!
updateAlphasWPForX: x y: y
	
	"In a basic implementation, this method computes anti aliasing translucency for all possibly affected pixels as determined by
	the outer radius of the pen nib: auxStrokeWidthDilatedHalf.
	But usually the next point visited is very close to the last, and there's no real need to fill the inner part of the pen nib as opaque,
	as it was already set. So, unless needsFullAlphaCircle, only the outer ring is computed and stored.
	The inner radius of this ring is auxStrokeWidthErodedHalfSquared"

	<var: 'x' type: #float>
	<var: 'y' type: #float>
	| t b l r pixelIndex candidateAlpha alphaByte distanceToAxisSquared dy dySquared dx bit distanceToOuterEdgeOfAA affectedBitsIndex nextPixelIndexToUpdateAffectedBits |
	<var: 'candidateAlpha' type: #'uint8_t'>
	<var: 'alphaByte' type: #'uint8_t'>
	<var: 'distanceToAxisSquared' type: #float>
	<var: 'dy' type: #float>
	<var: 'dySquared' type: #float>
	<var: 'dx' type: #float>
	<var: 'distanceToOuterEdgeOfAA' type: #float>
	
	"Use this optimized version if possible. It is always used, for instance, for TrueType."
	(strokeWidth = 0.0 and: [(antiAliasingWidth - 1.6) abs < 1e-6]) ifTrue: [
		^self updateAlphasWPZeroStrokeForX: x y: y ].

	"If dashed strokes, only draw if in a dash, not in a gap."
	dashBitLength = 0.0 ifFalse: [
		"Compute trajectory length. This is not precise. In many cases the actual hop used is smaller than this."
		trajectoryLength := trajectoryLength + hop.
		"Note: dashBitOffset must be positive."
		bit := ((trajectoryLength / dashBitLength) asInteger + dashBitOffset) \\ dashBitCount.
		(dashedStrokeBits bitAnd: 1 << (dashBitCount-bit-1)) = 0 ifTrue: [
			needsFullAlphaCircle := true.
			^self ]].
	
	"Compute affected rect. Honor clipRect"
	t := (y - auxStrokeWidthDilatedHalf + 1) asInteger.	"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	t < clipTop ifTrue: [ t := clipTop ].
	b := (y + auxStrokeWidthDilatedHalf) asInteger.
	b > clipBottom ifTrue: [ b := clipBottom ].
	l := (x - auxStrokeWidthDilatedHalf + 1) asInteger.	"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	l < clipLeft ifTrue: [ l := clipLeft ].
	r := (x + auxStrokeWidthDilatedHalf) asInteger.
	r > clipRight ifTrue: [ r := clipRight ].

	t to: b do: [ :displayY |
		pixelIndex := displayY * targetWidth + l -1.
		nextPixelIndexToUpdateAffectedBits := pixelIndex + 1.
		dy := displayY - y.
		dySquared := dy * dy.
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			dx := displayX - x.
			distanceToAxisSquared := dx*dx + dySquared.
			distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
				ifTrue: [
					(needsFullAlphaCircle or: [distanceToAxisSquared > auxStrokeWidthErodedHalfSquared]) ifTrue: [
						alphaByte := alphaMaskWP at: pixelIndex.
						alphaByte = 16r7F ifFalse: [
							candidateAlpha := 16r7F.
							distanceToOuterEdgeOfAA := auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt.
							distanceToOuterEdgeOfAA < antiAliasingWidth ifTrue: [
								candidateAlpha := (distanceToOuterEdgeOfAA * auxAntiAliasingWidthScaledInverse) asInteger ].
							candidateAlpha > alphaByte ifTrue: [
								alphaMaskWP at: pixelIndex put: candidateAlpha.
								pixelIndex >= nextPixelIndexToUpdateAffectedBits ifTrue: [
									affectedBitsIndex := pixelIndex>>4.
									affectedBits at: affectedBitsIndex put: 1.
									"nextPixelIndexToUpdateAffectedBits := affectedBitsIndex+1 << 4."
									"Both have same effect."
									nextPixelIndexToUpdateAffectedBits := (pixelIndex bitAnd: 16rFFFFFFF0) + 16r10 ]]]]]]].
	needsFullAlphaCircle := false.! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - path algorithms' stamp: 'jmv 18/Aug/2026, 3:11:34 pm (UTC)'!
updateAlphasWPZeroStrokeForX: x y: y
	"Optimization.
	Assumptions:
		- strokeWidth = 0.0
		- antiAliasingWidth = 1.6 (Default for whole pixel anti aliasing)
		- No support for dashedStrokes
	Therefore:
		- swDilatedHalf = 0.8
		- swDilatedHalfSquared = 0.64
		- aawScaledInverse = 79.375
		- No use for needsFullAlphaCircle, swErodedHalfSquared
		- 4 pixels are affected (maybe less, if clipping)
		"

	<var: 'x' type: #float>
	<var: 'y' type: #float>
	| t b l r pixelIndex candidateAlpha alphaByte distanceToAxisSquared dy dySquared dy2 dy2Squared dx dxSquared dx2 dx2Squared affectedBitsIndex |
	<var: 'candidateAlpha' type: #'uint8_t'>
	<var: 'alphaByte' type: #'uint8_t'>
	<var: 'distanceToAxisSquared' type: #float>
	<var: 'dy' type: #float>
	<var: 'dySquared' type: #float>
	<var: 'dy2' type: #float>
	<var: 'dy2Squared' type: #float>
	<var: 'dx' type: #float>
	<var: 'dxSquared' type: #float>
	<var: 'dx2' type: #float>
	<var: 'dx2Squared' type: #float>
	
	"Compute affected rect. Honor clipRect"
	t := y asInteger.
	b := t + 1.
	l := x asInteger.
	r := l + 1.
	t < clipTop ifTrue: [ t := clipTop ].
	b > clipBottom ifTrue: [ b := clipBottom ].
	l < clipLeft ifTrue: [ l := clipLeft ].
	r > clipRight ifTrue: [ r := clipRight ].
	t> b ifTrue: [ ^self ].
	l > r ifTrue: [ ^self ].

	pixelIndex := t * targetWidth + l.
	dy := t - y.
	dySquared := dy * dy.
	dx := l - x.
	dxSquared := dx * dx.
	distanceToAxisSquared := dxSquared + dySquared.
	distanceToAxisSquared < 0.64 ifTrue: [
		alphaByte := alphaMaskWP at: pixelIndex.
		alphaByte = 16r7F ifFalse: [
			candidateAlpha := ((0.8 - distanceToAxisSquared sqrt) * 79.375) asInteger.
			candidateAlpha > alphaByte ifTrue: [
				alphaMaskWP at: pixelIndex put: candidateAlpha .
				affectedBitsIndex := pixelIndex>>4.
				affectedBits at: affectedBitsIndex put: 1 ]].
		distanceToAxisSquared < 0.36 ifTrue: [ ^self ]].

	r = l ifFalse: [
		dx2 := dx + 1.
		dx2Squared := dx2 * dx2.
		distanceToAxisSquared := dx2Squared + dySquared.
		distanceToAxisSquared < 0.64 ifTrue: [
			alphaByte := alphaMaskWP at: pixelIndex+1.
			alphaByte = 16r7F ifFalse: [
				candidateAlpha := ((0.8 - distanceToAxisSquared sqrt) * 79.375) asInteger.
				candidateAlpha > alphaByte ifTrue: [
					alphaMaskWP at: pixelIndex+1 put: candidateAlpha.
					affectedBitsIndex := (pixelIndex+1)>>4.
					affectedBits at: affectedBitsIndex put: 1 ]].
			distanceToAxisSquared < 0.36 ifTrue: [ ^self ]]].

	t = b ifTrue: [ ^self ].

	pixelIndex := b * targetWidth + l.
	dy2 := dy + 1.
	dy2Squared := dy2 * dy2.
	distanceToAxisSquared := dxSquared + dy2Squared.
	distanceToAxisSquared < 0.64 ifTrue: [
		alphaByte := alphaMaskWP at: pixelIndex.
		alphaByte = 16r7F ifFalse: [
			candidateAlpha := ((0.8 - distanceToAxisSquared sqrt) * 79.375) asInteger.
			candidateAlpha > alphaByte ifTrue: [
				alphaMaskWP at: pixelIndex put: candidateAlpha.
				affectedBitsIndex := pixelIndex>>4.
				affectedBits at: affectedBitsIndex put: 1 ]].
		distanceToAxisSquared < 0.36 ifTrue: [ ^self ]].

	r = l ifFalse: [
		distanceToAxisSquared := dx2Squared + dy2Squared.
		distanceToAxisSquared < 0.64 ifTrue: [
			alphaByte := alphaMaskWP at: pixelIndex+1.
			alphaByte = 16r7F ifFalse: [
				candidateAlpha := ((0.8 - distanceToAxisSquared sqrt) * 79.375) asInteger.
				candidateAlpha > alphaByte ifTrue: [
					alphaMaskWP at: pixelIndex+1 put: candidateAlpha.
					affectedBitsIndex := (pixelIndex+1)>>4.
					affectedBits at: affectedBitsIndex put: 1 ]]]].! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - path algorithms' stamp: 'jmv 18/Aug/2026, 3:25:03 pm (UTC)'!
updateEdgeCountWPAtX: x y: y
	"Compute edges intersecting with this horizontal line, for fills."

	<var: 'x' type: #float>
	<var: 'y' type: #float>
	| thisYTruncated pixelY pixelIndex increment count pixelOffset affectedBitsIndex |

	<var: 'count' type: #'uint8_t'>
	<var: 'increment' type: #'uint8_t'>
	

	thisYTruncated := y asInteger.	"truncated, both in C and Smalltalk"
	thisYTruncated = prevYTruncated ifTrue: [
		^ 0].
	(thisYTruncated between: clipTop-1 and: clipBottom) ifFalse: [
		^ 0 ].
	prevYTruncated = 16r7FFFFFFF ifTrue: [ 
		prevYTruncated := thisYTruncated.
		^ 0 ].

	thisYTruncated > prevYTruncated
		ifTrue: [
			pixelY := thisYTruncated.
			increment := 16r01 ]
		ifFalse: [
			pixelY := prevYTruncated.
			increment := 16rFF ].
	prevYTruncated := thisYTruncated.

	"All edge count at the left of the clipRect are added there (at the left of the clipRect).
	The effect is the same, and we need to clean up less stuff afterwards.
	More important, it avoids trying to acess pixels outside our form, i.e. invalid array acesses."
	pixelOffset := (x+1) asInteger max: clipLeft.		"take the next pixel center to the right of x"

	pixelOffset <= clipRight ifTrue: [
		pixelIndex := pixelY * targetWidth + pixelOffset.
		count := edgeCountsWP at: pixelIndex.
		count := count + increment.
		edgeCountsWP at: pixelIndex put: count.
		affectedBitsIndex := pixelIndex>>4.
		affectedBits at: affectedBitsIndex put: 1 ].! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - blend stroke and fill' stamp: 'jmv 20/Aug/2026, 6:02:44 pm (UTC)'!
blendFillOnlySPAt: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside antiAliasAlphasWord: strokeAntiAliasAlphasWord
	"Blends fill color over background.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	Target translucency is computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	<var: 'strokeAntiAliasAlphasWord' type: #'uint32_t'>
	| targetWord resultR resultG resultB resultRBits resultGBits resultBBits resultAlphaBits alphaRBits alphaGBits alphaBBits alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlphaR resultAlphaG resultAlphaB targetAlphaBits targetAlpha |

	<var: 'alphaRBits' type: #'uint32_t'>
	<var: 'alphaGBits' type: #'uint32_t'>
	<var: 'alphaBBits' type: #'uint32_t'>
	<var: 'targetWord' type: #'uint32_t'>
	<var: 'targetAlphaBits' type: #'uint32_t'>
	<var: 'targetAlpha' type: #float>
	<var: 'resultRBits' type: #'uint32_t'>
	<var: 'resultGBits' type: #'uint32_t'>
	<var: 'resultBBits' type: #'uint32_t'>
	<var: 'resultR' type: #float>
	<var: 'resultG' type: #float>
	<var: 'resultB' type: #float>
	<var: 'alphaR' type: #float>
	<var: 'alphaG' type: #float>
	<var: 'alphaB' type: #float>
	<var: 'unAlphaR' type: #float>
	<var: 'unAlphaG' type: #float>
	<var: 'unAlphaB' type: #float>
	<var: 'resultAlphaR' type: #float>
	<var: 'resultAlphaG' type: #float>
	<var: 'resultAlphaB' type: #float>
	<var: 'resultAlphaBits' type: #'uint32_t'>
	

	"In this method, antiAliasAlphas are not used to blend stroke, but fill. This means that in the inside of the shape, and away from the stroke, they must be 1.0 (not 0.0)."
	alphaRBits := strokeAntiAliasAlphasWord bitAnd: 16r7F0000.
	alphaGBits := strokeAntiAliasAlphasWord bitAnd: 16r7F00.
	alphaBBits := strokeAntiAliasAlphasWord bitAnd: 16r7F.
	isRedInside ifTrue: [ alphaRBits := 16r7F0000 - alphaRBits ].
	isGreenInside ifTrue: [ alphaGBits := 16r7F00 - alphaGBits ].
	isBlueInside ifTrue: [ alphaBBits := 16r7F - alphaBBits ].
	alphaR := alphaRBits * (1.0 / (127.0 * 256 * 256)).
	alphaG := alphaGBits * (1.0 / (127.0 * 256)).
	alphaB := alphaBBits * (1.0 / 127.0).
	alphaR := alphaR * fillA.
	alphaG := alphaG * fillA.
	alphaB := alphaB * fillA.

	targetWord := targetBits at: pixelIndex.
	targetAlphaBits := targetWord bitAnd: 16rFF000000.
	targetAlpha := targetAlphaBits * ( 1.0 / (255.0 * 256 * 256 * 256)).
	resultAlphaBits := targetAlphaBits.
	resultRBits := targetWord bitAnd: 16rFF0000.
	resultGBits := targetWord bitAnd: 16rFF00.
	resultBBits := targetWord bitAnd: 16rFF.

	"These if are not really needed. just ignore them if we use simd instructions."
	alphaR = 0.0 ifFalse: [
		unAlphaR := 1.0 - alphaR.
		resultAlphaR := alphaR + (unAlphaR * targetAlpha).
		resultR := alphaR * fillR + (unAlphaR * (resultRBits bitShift: -16) * targetAlpha).
		resultRBits := (resultR / resultAlphaR + 0.5) asInteger bitShift: 16].

	alphaG = 0.0 ifFalse: [
		unAlphaG := 1.0 - alphaG.
		resultAlphaG := alphaG + (unAlphaG * targetAlpha).
		resultG := alphaG * fillG + (unAlphaG * (resultGBits bitShift: -8) * targetAlpha).
		resultGBits := (resultG / resultAlphaG + 0.5) asInteger bitShift: 8.
		resultAlphaBits := (resultAlphaG * 255.0 + 0.5) asInteger bitShift: 24].

	alphaB = 0.0 ifFalse: [
		unAlphaB := 1.0 - alphaB.
		resultAlphaB := alphaB + (unAlphaB * targetAlpha).
		resultB := alphaB * fillB + (unAlphaB * resultBBits * targetAlpha).
		resultBBits := (resultB / resultAlphaB + 0.5) asInteger].

	targetWord := ((resultAlphaBits bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
	targetBits at: pixelIndex put: targetWord.
	morphIds at: pixelIndex put: currentMorphId.! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - blend stroke and fill' stamp: 'jmv 20/Aug/2026, 7:00:45 pm (UTC)'!
blendFillOnlySPLeft: l top: t right: r bottom: b
	"Blends fill color over background.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	If targetAssumedOpaque is false, target translucency is computed correctly.
	If targetAssumedOpaque is true (and target translucency is actually 255 on input), an optimization is made and target translucency is set to 255.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"
	| pixelIndex edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixelWord edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside strokeAntiAliasAlphasWord clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetColor realFillAlpha opaqueFillColorWord realOpaqueFillColorWord auxR auxG auxB displayX affectedBitsIndex segmentLength alphasOrEdgeCountsInThisSegment lastSegmentIndex aux1 |
	<var: 'edgesThisPixelWord' type: #'uint32_t'>
	<var: 'edgesThisPixelR' type: #'uint8_t'>
	<var: 'edgesThisPixelG' type: #'uint8_t'>
	<var: 'edgesThisPixelB' type: #'uint8_t'>
	<var: 'edgesUpToThisPixelR' type: #'uint8_t'>
	<var: 'edgesUpToThisPixelG' type: #'uint8_t'>
	<var: 'edgesUpToThisPixelB' type: #'uint8_t'>
	<var: 'strokeAntiAliasAlphasWord' type: #'uint32_t'>
	<var: 'opaqueFillColorWord' type: #'uint32_t'>
	<var: 'realOpaqueFillColorWord' type: #'uint32_t'>
	<var: 'auxR' type: #'uint32_t'>
	<var: 'auxG' type: #'uint32_t'>
	<var: 'auxB' type: #'uint32_t'>
	<var: 'realFillAlpha' type: #'float'>
	self
		primitive: 'primBlendFillOnly'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger).

	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedClippedLeftPixel := targetWidth."targetWidth means effectively no AA for clipping"
	antiAliasedClippedRightPixel := targetWidth."targetWidth means effectively no AA for clipping"
	clippingSpecIndex := t * 2 + 1 - 1.
	mustResetColor := false.
	opaqueFillColorWord := 0.
	targetAssumedOpaque ifTrue: [
		fillA = 1.0 ifTrue: [
			auxR := (fillR + 0.5) asInteger bitShift: 16.
			auxG := (fillG + 0.5) asInteger bitShift: 8.
			auxB := (fillB + 0.5) asInteger.
			opaqueFillColorWord := ((16rFF000000 bitOr: auxR) bitOr: auxG) bitOr: auxB ]].
	lastSegmentIndex := -1.
	t to: b do: [ :displayY |
		clippingSpec ifNotNil: [
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedClippedLeftPixel := clippingSpecL >= l ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedClippedRightPixel := clippingSpecR <= r ifTrue: [clippingSpecR] ifFalse: [targetWidth]].
		edgesUpToThisPixelR := 0.
		edgesUpToThisPixelG := 0.
		edgesUpToThisPixelB := 0.
		isRedInside := isGreenInside := isBlueInside := false.
		pixelIndex := displayY * targetWidth + l.
		displayX := l.
		[ displayX <= r ] whileTrue: [
			
			affectedBitsIndex := pixelIndex>>4.
			lastSegmentIndex = affectedBitsIndex ifFalse: [
				alphasOrEdgeCountsInThisSegment := (affectedBits at: affectedBitsIndex) = 1.
				lastSegmentIndex := affectedBitsIndex.
				affectedBits at: affectedBitsIndex put: 0 ].
			segmentLength := affectedBitsIndex+1 << 4 - pixelIndex.
			alphasOrEdgeCountsInThisSegment | isGreenInside ifTrue: [
				aux1 := r-displayX+1.
				1 to: (segmentLength min: aux1) do: [ :idx |
					
					strokeAntiAliasAlphasWord := 16r00000000.
					alphasOrEdgeCountsInThisSegment ifTrue: [
						edgesThisPixelWord := edgeCountsSP at: pixelIndex.
						edgesThisPixelWord = 0 ifFalse: [
							edgeCountsSP at: pixelIndex put: 0.
							edgesThisPixelR := (edgesThisPixelWord bitAnd: 16rFF0000) bitShift: -16.
							edgesThisPixelG := (edgesThisPixelWord bitAnd: 16rFF00)bitShift: -8.
							edgesThisPixelB := edgesThisPixelWord bitAnd: 16rFF.
							self cCode: nil inSmalltalk: [
								"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
								edgesThisPixelR > 127 ifTrue: [ edgesThisPixelR := edgesThisPixelR - 256 ].
								edgesThisPixelG > 127 ifTrue: [ edgesThisPixelG := edgesThisPixelG - 256 ].
								edgesThisPixelB > 127 ifTrue: [ edgesThisPixelB := edgesThisPixelB - 256 ]].
							edgesUpToThisPixelR := edgesUpToThisPixelR + edgesThisPixelR.
							edgesUpToThisPixelG := edgesUpToThisPixelG + edgesThisPixelG.
							edgesUpToThisPixelB := edgesUpToThisPixelB + edgesThisPixelB.
							"In C, integers already behave like booleans"
							self cCode: '
									isRedInside = edgesUpToThisPixelR;
									isGreenInside = edgesUpToThisPixelG;
									isBlueInside = edgesUpToThisPixelB;'
								inSmalltalk: [
									isRedInside := edgesUpToThisPixelR ~= 0.
									isGreenInside := edgesUpToThisPixelG ~= 0.
									isBlueInside := edgesUpToThisPixelB ~= 0 ]].
						strokeAntiAliasAlphasWord := alphaMaskSP at: pixelIndex.
						strokeAntiAliasAlphasWord ~= 16r00000000 ifTrue: [
							alphaMaskSP at: pixelIndex put: 0 ]].
					
					(displayX >= clippingSpecL and: [ displayX <= clippingSpecR ]) ifTrue: [
						(displayX = antiAliasedClippedLeftPixel or: [ displayX = antiAliasedClippedRightPixel])
							ifTrue: [
								realFillAlpha := fillA. fillA := fillA * 0.25.
								realOpaqueFillColorWord := opaqueFillColorWord. opaqueFillColorWord := 0. mustResetColor := true ]
							ifFalse: [
								(displayX-1 = antiAliasedClippedLeftPixel or: [ displayX+1 = antiAliasedClippedRightPixel]) ifTrue: [
									realFillAlpha := fillA. fillA := fillA * 0.75.
									realOpaqueFillColorWord := opaqueFillColorWord. opaqueFillColorWord := 0. mustResetColor := true ]].
						
						(opaqueFillColorWord ~= 0 and: [ strokeAntiAliasAlphasWord = 16r00000000 and: [ isGreenInside ]])
							ifTrue: [ "Overwrite with fill color is ok and we are in the fill, far from anti aliasing"
								"If no alpha, and isGreenInside is true, isRedInside and isBlueInside are also true"
								targetBits at: pixelIndex put: opaqueFillColorWord.
								morphIds at: pixelIndex put: currentMorphId]
							ifFalse: [ "General case. (strokeAntiAliasAlphasWord = 0 and outside the shape means NOP)"
								(strokeAntiAliasAlphasWord ~= 16r00000000 or: [ isGreenInside ]) ifTrue: [
									"If no alpha, and isGreenInside is true, isRedInside and isBlueInside are also true
									If there is any alpha, isRedInside, isGreenInside, isBlueInside may be different."
									self blendFillOnlySPAt: pixelIndex
										redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
										antiAliasAlphasWord: strokeAntiAliasAlphasWord ]].
						
						mustResetColor ifTrue: [
							fillA := realFillAlpha. opaqueFillColorWord := realOpaqueFillColorWord. mustResetColor := false ]].
					displayX := displayX + 1.
					pixelIndex := pixelIndex + 1 ]]
			ifFalse: [				"All alphas and edgeCounts are zero in this segment of length delta"
				displayX := displayX + segmentLength.
				pixelIndex := pixelIndex + segmentLength ]].
		
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - blend stroke and fill' stamp: 'jmv 20/Aug/2026, 6:03:21 pm (UTC)'!
blendStrokeAndFillSPAt: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside antiAliasAlphasWord: strokeAntiAliasAlphasWord
	"Blends stroke color and fill color over background.
	Do an appropriate (anti aliased) gradient between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	Target translucency is computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	<var: 'strokeAntiAliasAlphasWord' type: #'uint32_t'>
	| targetWord resultR resultG resultB resultRBits resultGBits resultBBits resultAlphaBits strokeAARedAlphaBits strokeAARedAlpha strokeAAGreenAlphaBits strokeAAGreenAlpha strokeAABlueAlphaBits strokeAABlueAlpha foreR foreG foreB alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlphaR resultAlphaG resultAlphaB targetAlphaBits targetAlpha |

	<var: 'strokeAARedAlphaBits' type: #'uint32_t'>
	<var: 'strokeAAGreenAlphaBits' type: #'uint32_t'>
	<var: 'strokeAABlueAlphaBits' type: #'uint32_t'>
	<var: 'targetWord' type: #'uint32_t'>
	<var: 'targetAlphaBits' type: #'uint32_t'>
	<var: 'targetAlpha' type: #float>
	<var: 'resultRBits' type: #'uint32_t'>
	<var: 'resultGBits' type: #'uint32_t'>
	<var: 'resultBBits' type: #'uint32_t'>
	<var: 'resultR' type: #float>
	<var: 'resultG' type: #float>
	<var: 'resultB' type: #float>
	<var: 'strokeAARedAlpha' type: #float>
	<var: 'strokeAAGreenAlpha' type: #float>
	<var: 'strokeAABlueAlpha' type: #float>
	<var: 'foreR' type: #float>
	<var: 'foreG' type: #float>
	<var: 'foreB' type: #float>
	<var: 'alphaR' type: #float>
	<var: 'alphaG' type: #float>
	<var: 'alphaB' type: #float>
	<var: 'unAlphaR' type: #float>
	<var: 'unAlphaG' type: #float>
	<var: 'unAlphaB' type: #float>
	<var: 'resultAlphaR' type: #float>
	<var: 'resultAlphaG' type: #float>
	<var: 'resultAlphaB' type: #float>
	<var: 'resultAlphaBits' type: #'uint32_t'>
	

	strokeAARedAlphaBits := strokeAntiAliasAlphasWord bitAnd: 16r7F0000.
	strokeAAGreenAlphaBits := strokeAntiAliasAlphasWord bitAnd: 16r7F00.
	strokeAABlueAlphaBits := strokeAntiAliasAlphasWord bitAnd: 16r7F.
	strokeAARedAlpha := strokeAARedAlphaBits * (1.0 / (127.0 * 256 * 256)).
	strokeAAGreenAlpha := strokeAAGreenAlphaBits * (1.0 / (127.0 * 256)).
	strokeAABlueAlpha := strokeAABlueAlphaBits * (1.0 / 127.0).

	isRedInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaR := strokeAARedAlpha * strokeA + ((1.0-strokeAARedAlpha) * fillA).
			foreR := strokeAARedAlpha * strokeR + ((1.0-strokeAARedAlpha) * fillR) ]
		ifFalse: [							"Blend stroke over background"
			alphaR := strokeAARedAlpha * strokeA.
			foreR := strokeR ].

	isGreenInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaG := strokeAAGreenAlpha * strokeA + ((1.0-strokeAAGreenAlpha) * fillA).
			foreG := strokeAAGreenAlpha * strokeG + ((1.0-strokeAAGreenAlpha) * fillG) ]
		ifFalse: [							"Blend stroke over background"
			alphaG := strokeAAGreenAlpha * strokeA.
			foreG := strokeG ].

	isBlueInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaB := strokeAABlueAlpha * strokeA + ((1.0-strokeAABlueAlpha) * fillA).
			foreB := strokeAABlueAlpha * strokeB + ((1.0-strokeAABlueAlpha) * fillB) ]
		ifFalse: [							"Blend stroke over background"
			alphaB := strokeAABlueAlpha * strokeA.
			foreB := strokeB ].

	targetWord := targetBits at: pixelIndex.
	targetAlphaBits := targetWord bitAnd: 16rFF000000.
	targetAlpha := targetAlphaBits * ( 1.0 / (255.0 * 256 * 256 * 256)).
	resultAlphaBits := targetAlphaBits.
	resultRBits := targetWord bitAnd: 16rFF0000.
	resultGBits := targetWord bitAnd: 16rFF00.
	resultBBits := targetWord bitAnd: 16rFF.

	"These if are not really needed. just ignore them if we use simd instructions."
	alphaR = 0.0 ifFalse: [
		unAlphaR := 1.0 - alphaR.
		resultAlphaR := alphaR + (unAlphaR * targetAlpha).
		resultR := alphaR * foreR + (unAlphaR * (resultRBits bitShift: -16) * targetAlpha).
		resultRBits := (resultR / resultAlphaR + 0.5) asInteger bitShift: 16].

	alphaG = 0.0 ifFalse: [
		unAlphaG := 1.0 - alphaG.
		resultAlphaG := alphaG + (unAlphaG * targetAlpha).
		resultG := alphaG * foreG + (unAlphaG * (resultGBits bitShift: -8) * targetAlpha).
		resultGBits := (resultG / resultAlphaG + 0.5) asInteger bitShift: 8.
		resultAlphaBits := (resultAlphaG * 255.0 + 0.5) asInteger bitShift: 24].

	alphaB = 0.0 ifFalse: [
		unAlphaB := 1.0 - alphaB.
		resultAlphaB := alphaB + (unAlphaB * targetAlpha).
		resultB := alphaB * foreB + (unAlphaB * resultBBits * targetAlpha).
		resultBBits := (resultB / resultAlphaB + 0.5) asInteger].

	targetWord := ((resultAlphaBits bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
	targetBits at: pixelIndex put: targetWord.
	morphIds at: pixelIndex put: currentMorphId.! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - blend stroke and fill' stamp: 'jmv 20/Aug/2026, 9:12:11 pm (UTC)'!
blendStrokeAndFillSPLeft: l top: t right: r bottom: b
	"Blends stroke color and fill color over background.
	Do an appropriate (anti aliased) gradient between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	If targetAssumedOpaque is false, target translucency is computed correctly.
	If targetAssumedOpaque is true (and target translucency is actually 255 on input), an optimization is made and target translucency is set to 255.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"
	| pixelIndex edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixelWord edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside strokeAntiAliasAlphasWord clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetColor realStrokeAlpha realFillAlpha opaqueStrokeColorWord opaqueFillColorWord realOpaqueStrokeColorWord realOpaqueFillColorWord auxR auxG auxB displayX affectedBitsIndex segmentLength alphasOrEdgeCountsInThisSegment lastSegmentIndex aux1 |
	<var: 'edgesThisPixelWord' type: #'uint32_t'>
	<var: 'edgesThisPixelR' type: #'uint8_t'>
	<var: 'edgesThisPixelG' type: #'uint8_t'>
	<var: 'edgesThisPixelB' type: #'uint8_t'>
	<var: 'edgesUpToThisPixelR' type: #'uint8_t'>
	<var: 'edgesUpToThisPixelG' type: #'uint8_t'>
	<var: 'edgesUpToThisPixelB' type: #'uint8_t'>
	<var: 'strokeAntiAliasAlphasWord' type: #'uint32_t'>
	<var: 'opaqueStrokeColorWord' type: #'uint32_t'>
	<var: 'realOpaqueStrokeColorWord' type: #'uint32_t'>
	<var: 'opaqueFillColorWord' type: #'uint32_t'>
	<var: 'realOpaqueFillColorWord' type: #'uint32_t'>
	<var: 'auxR' type: #'uint32_t'>
	<var: 'auxG' type: #'uint32_t'>
	<var: 'auxB' type: #'uint32_t'>
	<var: 'realStrokeAlpha' type: #float>
	<var: 'realFillAlpha' type: #'float'>
	self
		primitive: 'primBlendStrokeAndFill'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger).

	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedClippedLeftPixel := targetWidth."targetWidth means effectively no AA for clipping"
	antiAliasedClippedRightPixel := targetWidth."targetWidth means effectively no AA for clipping"
	clippingSpecIndex := t * 2 + 1 - 1.
	mustResetColor := false.
	opaqueStrokeColorWord := 0.
	opaqueFillColorWord := 0.
	targetAssumedOpaque ifTrue: [
		strokeA * fillA = 1.0 ifTrue: [
			auxR := (strokeR + 0.5) asInteger bitShift: 16.
			auxG := (strokeG + 0.5) asInteger bitShift: 8.
			auxB := (strokeB + 0.5) asInteger.
			opaqueStrokeColorWord := ((16rFF000000 bitOr: auxR) bitOr: auxG) bitOr: auxB.
			auxR := (fillR + 0.5) asInteger bitShift: 16.
			auxG := (fillG + 0.5) asInteger bitShift: 8.
			auxB := (fillB + 0.5) asInteger.
			opaqueFillColorWord := ((16rFF000000 bitOr: auxR) bitOr: auxG) bitOr: auxB ]].
	lastSegmentIndex := -1.
	t to: b do: [ :displayY |
		clippingSpec ifNotNil: [
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedClippedLeftPixel := clippingSpecL >= l ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedClippedRightPixel := clippingSpecR <= r ifTrue: [clippingSpecR] ifFalse: [targetWidth]].
		edgesUpToThisPixelR := 0.
		edgesUpToThisPixelG := 0.
		edgesUpToThisPixelB := 0.
		isRedInside := isGreenInside := isBlueInside := false.
		pixelIndex := displayY * targetWidth + l.
		displayX := l.
		[ displayX <= r ] whileTrue: [
			
			affectedBitsIndex := pixelIndex>>4.
			lastSegmentIndex = affectedBitsIndex ifFalse: [
				alphasOrEdgeCountsInThisSegment := (affectedBits at: affectedBitsIndex) = 1.
				lastSegmentIndex := affectedBitsIndex.
				affectedBits at: affectedBitsIndex put: 0 ].
			segmentLength := affectedBitsIndex+1 << 4 - pixelIndex.
			alphasOrEdgeCountsInThisSegment | isGreenInside ifTrue: [
				aux1 := r-displayX+1.
				1 to: (segmentLength min: aux1) do: [ :idx |
					
					strokeAntiAliasAlphasWord := 16r00000000.
					alphasOrEdgeCountsInThisSegment ifTrue: [
						edgesThisPixelWord := edgeCountsSP at: pixelIndex.
						edgesThisPixelWord = 0 ifFalse: [
							edgeCountsSP at: pixelIndex put: 0.
							edgesThisPixelR := (edgesThisPixelWord bitAnd: 16rFF0000) bitShift: -16.
							edgesThisPixelG := (edgesThisPixelWord bitAnd: 16rFF00)bitShift: -8.
							edgesThisPixelB := edgesThisPixelWord bitAnd: 16rFF.
							self cCode: nil inSmalltalk: [
								"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
								edgesThisPixelR > 127 ifTrue: [ edgesThisPixelR := edgesThisPixelR - 256 ].
								edgesThisPixelG > 127 ifTrue: [ edgesThisPixelG := edgesThisPixelG - 256 ].
								edgesThisPixelB > 127 ifTrue: [ edgesThisPixelB := edgesThisPixelB - 256 ]].
							edgesUpToThisPixelR := edgesUpToThisPixelR + edgesThisPixelR.
							edgesUpToThisPixelG := edgesUpToThisPixelG + edgesThisPixelG.
							edgesUpToThisPixelB := edgesUpToThisPixelB + edgesThisPixelB.
							"In C, integers already behave like booleans"
							self cCode: '
									isRedInside = edgesUpToThisPixelR;
									isGreenInside = edgesUpToThisPixelG;
									isBlueInside = edgesUpToThisPixelB;'
								inSmalltalk: [
									isRedInside := edgesUpToThisPixelR ~= 0.
									isGreenInside := edgesUpToThisPixelG ~= 0.
									isBlueInside := edgesUpToThisPixelB ~= 0 ]].
						strokeAntiAliasAlphasWord := alphaMaskSP at: pixelIndex.
						strokeAntiAliasAlphasWord ~= 16r00000000 ifTrue: [
							alphaMaskSP at: pixelIndex put: 0 ]].
					
					(displayX >= clippingSpecL and: [ displayX <= clippingSpecR ]) ifTrue: [
						(displayX = antiAliasedClippedLeftPixel or: [ displayX = antiAliasedClippedRightPixel])
							ifTrue: [
								realStrokeAlpha := strokeA. strokeA := strokeA * 0.25. realFillAlpha := fillA. fillA := fillA * 0.25.
								realOpaqueStrokeColorWord := opaqueStrokeColorWord. opaqueStrokeColorWord := 0. 
								realOpaqueFillColorWord := opaqueFillColorWord. opaqueFillColorWord := 0. mustResetColor := true ]
							ifFalse: [
								(displayX-1 = antiAliasedClippedLeftPixel or: [ displayX+1 = antiAliasedClippedRightPixel]) ifTrue: [
									realStrokeAlpha := strokeA. strokeA := strokeA * 0.75. realFillAlpha := fillA. fillA := fillA * 0.75.
									realOpaqueStrokeColorWord := opaqueStrokeColorWord. opaqueStrokeColorWord := 0. 
									realOpaqueFillColorWord := opaqueFillColorWord. opaqueFillColorWord := 0. mustResetColor := true ]].
						
						strokeAntiAliasAlphasWord = 16r00000000
							ifTrue: [ 		"Not in the stroke at all. Either fully in the fill, or outside the shape (pixel is unaffected)."
								isGreenInside
									ifTrue: [
										"Fully inside the fill, far from anti aliasing. (Here isGreenInside also implies isRedInside and isBlueInside)"
										opaqueFillColorWord ~= 0
											ifTrue: [
												"Fill color is opaque. Target is too. Just overwrite with fill color."
												targetBits at: pixelIndex put: opaqueFillColorWord.
												morphIds at: pixelIndex put: currentMorphId ]
											ifFalse: [
												"Translucent color or target. Do proper blend of fill over target."
												self blendFillOnlySPAt: pixelIndex
													redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
													antiAliasAlphasWord: strokeAntiAliasAlphasWord ]]]
							ifFalse: [		"At least one subpixel in the stroke."
								strokeAntiAliasAlphasWord = 16r007F7F7F
									ifTrue: [
										"Fully inside the stroke, far from anti aliasing."
										opaqueStrokeColorWord ~= 0
											ifTrue: [
												"Stroke color is opaque. Target is too. Just overwrite with stroke color."
												targetBits at: pixelIndex put: opaqueStrokeColorWord.
												morphIds at: pixelIndex put: currentMorphId ]
											ifFalse: [
												"Translucent color or target. Do proper blend of stroke over target."
												self blendStrokeOnlySPAt: pixelIndex antiAliasAlphasWord: 16r007F7F7F ]]
									ifFalse: [
										"In an anti aliased part of the stroke. Either blend stroke over background, or pre-mix stroke and fill."
										self blendStrokeAndFillSPAt: pixelIndex
											redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
											antiAliasAlphasWord: strokeAntiAliasAlphasWord ]].
						
						mustResetColor ifTrue: [
							strokeA := realStrokeAlpha. fillA := realFillAlpha.
							opaqueStrokeColorWord := realOpaqueStrokeColorWord. opaqueFillColorWord := realOpaqueFillColorWord.
							mustResetColor := false ]].
					displayX := displayX + 1.
					pixelIndex := pixelIndex + 1 ]]
			ifFalse: [				"All alphas and edgeCounts are zero in this segment of length delta"
				displayX := displayX + segmentLength.
				pixelIndex := pixelIndex + segmentLength ]].
		
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - blend stroke and fill' stamp: 'jmv 20/Aug/2026, 6:04:06 pm (UTC)'!
blendStrokeOnlySPAt: pixelIndex antiAliasAlphasWord: strokeAntiAliasAlphasWord
	"Blends stroke color over background.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	Target translucency is computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	<var: 'strokeAntiAliasAlphasWord' type: #'uint32_t'>
	| targetWord resultR resultG resultB resultRBits resultGBits resultBBits resultAlphaBits alphaRBits alphaGBits alphaBBits alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlphaR resultAlphaG resultAlphaB targetAlphaBits targetAlpha |

	<var: 'alphaRBits' type: #'uint32_t'>
	<var: 'alphaGBits' type: #'uint32_t'>
	<var: 'alphaBBits' type: #'uint32_t'>
	<var: 'targetWord' type: #'uint32_t'>
	<var: 'targetAlphaBits' type: #'uint32_t'>
	<var: 'targetAlpha' type: #float>
	<var: 'resultRBits' type: #'uint32_t'>
	<var: 'resultGBits' type: #'uint32_t'>
	<var: 'resultBBits' type: #'uint32_t'>
	<var: 'resultR' type: #float>
	<var: 'resultG' type: #float>
	<var: 'resultB' type: #float>
	<var: 'alphaR' type: #float>
	<var: 'alphaG' type: #float>
	<var: 'alphaB' type: #float>
	<var: 'unAlphaR' type: #float>
	<var: 'unAlphaG' type: #float>
	<var: 'unAlphaB' type: #float>
	<var: 'resultAlphaR' type: #float>
	<var: 'resultAlphaG' type: #float>
	<var: 'resultAlphaB' type: #float>
	<var: 'resultAlphaBits' type: #'uint32_t'>
	

	alphaRBits := strokeAntiAliasAlphasWord bitAnd: 16r7F0000.
	alphaGBits := strokeAntiAliasAlphasWord bitAnd: 16r7F00.
	alphaBBits := strokeAntiAliasAlphasWord bitAnd: 16r7F.
	alphaR := alphaRBits * (1.0 / (127.0 * 256 * 256)).
	alphaG := alphaGBits * (1.0 / (127.0 * 256)).
	alphaB := alphaBBits * (1.0 / 127.0).
	alphaR := alphaR * strokeA.
	alphaG := alphaG * strokeA.
	alphaB := alphaB * strokeA.

	targetWord := targetBits at: pixelIndex.
	targetAlphaBits := targetWord bitAnd: 16rFF000000.
	targetAlpha := targetAlphaBits * ( 1.0 / (255.0 * 256 * 256 * 256)).
	resultAlphaBits := targetAlphaBits.
	resultRBits := targetWord bitAnd: 16rFF0000.
	resultGBits := targetWord bitAnd: 16rFF00.
	resultBBits := targetWord bitAnd: 16rFF.

	"These if are not really needed. just ignore them if we use simd instructions."
	alphaR = 0.0 ifFalse: [
		unAlphaR := 1.0 - alphaR.
		resultAlphaR := alphaR + (unAlphaR * targetAlpha).
		resultR := alphaR * strokeR + (unAlphaR * (resultRBits bitShift: -16) * targetAlpha).
		resultRBits := (resultR / resultAlphaR + 0.5) asInteger bitShift: 16].

	alphaG = 0.0 ifFalse: [
		unAlphaG := 1.0 - alphaG.
		resultAlphaG := alphaG + (unAlphaG * targetAlpha).
		resultG := alphaG * strokeG + (unAlphaG * (resultGBits bitShift: -8) * targetAlpha).
		resultGBits := (resultG / resultAlphaG + 0.5) asInteger bitShift: 8.
		resultAlphaBits := (resultAlphaG * 255.0 + 0.5) asInteger bitShift: 24].

	alphaB = 0.0 ifFalse: [
		unAlphaB := 1.0 - alphaB.
		resultAlphaB := alphaB + (unAlphaB * targetAlpha).
		resultB := alphaB * strokeB + (unAlphaB * resultBBits * targetAlpha).
		resultBBits := (resultB / resultAlphaB + 0.5) asInteger].

	targetWord := ((resultAlphaBits bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
	targetBits at: pixelIndex put: targetWord.
	morphIds at: pixelIndex put: currentMorphId.! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - blend stroke and fill' stamp: 'jmv 20/Aug/2026, 9:13:11 pm (UTC)'!
blendStrokeOnlySPLeft: l top: t right: r bottom: b
	"Blends stroke color over background.
	Sub pixel anti aliasing means each R, G, B subpixel might use a different anti aliasing alpha.
	If targetAssumedOpaque is false, target translucency is computed correctly.
	If targetAssumedOpaque is true (and target translucency is actually 255 on input), an optimization is made and target translucency is set to 255.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"
	| pixelIndex strokeAntiAliasAlphasWord clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetColor realStrokeAlpha opaqueStrokeColorWord realOpaqueStrokeColorWord auxR auxG auxB displayX affectedBitsIndex segmentLength alphasOrEdgeCountsInThisSegment lastSegmentIndex aux1 |
	<var: 'strokeAntiAliasAlphasWord' type: #'uint32_t'>
	<var: 'opaqueStrokeColorWord' type: #'uint32_t'>
	<var: 'realOpaqueStrokeColorWord' type: #'uint32_t'>
	<var: 'auxR' type: #'uint32_t'>
	<var: 'auxG' type: #'uint32_t'>
	<var: 'auxB' type: #'uint32_t'>
	<var: 'realStrokeAlpha' type: #float>
	self
		primitive: 'primBlendStrokeOnly'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger).

	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedClippedLeftPixel := targetWidth."targetWidth means effectively no AA for clipping"
	antiAliasedClippedRightPixel := targetWidth."targetWidth means effectively no AA for clipping"
	clippingSpecIndex := t * 2 + 1 - 1.
	mustResetColor := false.
	opaqueStrokeColorWord := 0.
	targetAssumedOpaque ifTrue: [
		strokeA = 1.0 ifTrue: [
			auxR := (strokeR + 0.5) asInteger bitShift: 16.
			auxG := (strokeG + 0.5) asInteger bitShift: 8.
			auxB := (strokeB + 0.5) asInteger.
			opaqueStrokeColorWord := ((16rFF000000 bitOr: auxR) bitOr: auxG) bitOr: auxB ]].
	lastSegmentIndex := -1.
	t to: b do: [ :displayY |
		clippingSpec ifNotNil: [
			clippingSpecL := clippingSpec at: clippingSpecIndex.
			clippingSpecR := clippingSpec at: clippingSpecIndex+1.
			antiAliasedClippedLeftPixel := clippingSpecL >= l ifTrue: [clippingSpecL] ifFalse: [targetWidth].
			antiAliasedClippedRightPixel := clippingSpecR <= r ifTrue: [clippingSpecR] ifFalse: [targetWidth]].
		pixelIndex := displayY * targetWidth + l.
		displayX := l.
		[ displayX <= r ] whileTrue: [
			
			affectedBitsIndex := pixelIndex>>4.
			lastSegmentIndex = affectedBitsIndex ifFalse: [
				alphasOrEdgeCountsInThisSegment := (affectedBits at: affectedBitsIndex) = 1.
				lastSegmentIndex := affectedBitsIndex.
				affectedBits at: affectedBitsIndex put: 0 ].
			segmentLength := affectedBitsIndex+1 << 4 - pixelIndex.
			alphasOrEdgeCountsInThisSegment ifTrue: [
				aux1 := r-displayX+1.
				1 to: (segmentLength min: aux1) do: [ :idx |
					
					strokeAntiAliasAlphasWord := alphaMaskSP at: pixelIndex.
					strokeAntiAliasAlphasWord = 16r00000000 ifFalse: [
						alphaMaskSP at: pixelIndex put: 0.
						
						(displayX >= clippingSpecL and: [ displayX <= clippingSpecR ]) ifTrue: [
							(displayX = antiAliasedClippedLeftPixel or: [ displayX = antiAliasedClippedRightPixel])
								ifTrue: [
									realStrokeAlpha := strokeA. strokeA := strokeA * 0.25.
									realOpaqueStrokeColorWord := opaqueStrokeColorWord. opaqueStrokeColorWord := 0. mustResetColor := true ]
								ifFalse: [
									(displayX-1 = antiAliasedClippedLeftPixel or: [ displayX+1 = antiAliasedClippedRightPixel]) ifTrue: [
										realStrokeAlpha := strokeA. strokeA := strokeA * 0.75.
										realOpaqueStrokeColorWord := opaqueStrokeColorWord. opaqueStrokeColorWord := 0. mustResetColor := true ]].
							
							(opaqueStrokeColorWord ~= 0 and: [strokeAntiAliasAlphasWord = 16r007F7F7F])
								ifTrue: [
									"Fully inside the stroke, far from anti aliasing. Color is opaque. Target is too. Just overwrite with stroke color."
									targetBits at: pixelIndex put: opaqueStrokeColorWord.
									morphIds at: pixelIndex put: currentMorphId ]
								ifFalse: [
									"At least one subpixel in the anti aliasing area of the stroke, or color is translucent, or target translucency is desired."
									self blendStrokeOnlySPAt: pixelIndex antiAliasAlphasWord: strokeAntiAliasAlphasWord ]].
						
						mustResetColor ifTrue: [
							strokeA := realStrokeAlpha. opaqueStrokeColorWord := realOpaqueStrokeColorWord. mustResetColor := false ]].
					displayX := displayX + 1.
					pixelIndex := pixelIndex + 1 ]]
			ifFalse: [				"All alphas and edgeCounts are zero in this segment of length delta"
				displayX := displayX + segmentLength.
				pixelIndex := pixelIndex + segmentLength ]].
		
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - blend stroke and fill' stamp: 'jmv 21/Aug/2026, 2:08:12 pm (UTC)'!
blendFillOnlyWPAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits fillAlpha: fillAlpha fillRedBlue: fillRedBlue fillGreen: fillGreen
	"Blends fill color over background.
	WP: Whole pixel anti aliasing.
	Target translucency is ignored and set to opaque.
	Optimized implementation using integer arithmetic and no division."

	<var: 'strokeAntiAliasAlphaBits' type: #'uint32_t'>
	<var: 'fillAlpha' type: #uint32_t>
	<var: 'fillRedBlue' type: #uint32_t>
	<var: 'fillGreen' type: #uint32_t>
	| alpha unAlpha targetRedBlue targetGreen targetWord resultRedBlue resultGreen |

	<var: 'alpha' type: #uint32_t>
	<var: 'unAlpha' type: #uint32_t>
	<var: 'targetWord' type: #'uint32_t'>
	<var: 'targetRedBlue' type: #uint32_t>
	<var: 'targetGreen' type: #uint32_t>
	<var: 'resultRedBlue' type: #uint32_t>
	<var: 'resultGreen' type: #uint32_t>

	alpha := (strokeAntiAliasAlphaBits * fillAlpha) >> 7.
	unAlpha := 255 - alpha.
	targetWord := targetBits at: pixelIndex.
	targetRedBlue  := targetWord  bitAnd: 16r00FF00FF.
	targetGreen   := targetWord  bitAnd: 16r0000FF00.

	"Blend Red and Blue channels simultaneously"
	resultRedBlue := ((((fillRedBlue * alpha) + (targetRedBlue * unAlpha)) + 16r00FF00FF) >> 8) bitAnd: 16r00FF00FF.
	"Blend Green channel"
	resultGreen := ((((fillGreen * alpha) + (targetGreen * unAlpha)) + 16r0000FF00) >> 8) bitAnd: 16r0000FF00.

	"Combine channels back together. Resuilt alpha set to opaque."
	targetWord := 16rFF000000 bitOr: (resultRedBlue bitOr: resultGreen).
	
	targetBits at: pixelIndex put: targetWord.
	morphIds at: pixelIndex put: currentMorphId.! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - blend stroke and fill' stamp: 'jmv 21/Aug/2026, 6:13:40 pm (UTC)'!
blendFillOnlyWPLeft: l top: t right: r bottom: b
	"Blends fill color over background.
	WP: Whole pixel anti aliasing.
	Ignore targetAssumedOpaque, take it as is true, target translucency is ignored ande set to 255,
	to allow integer only arithnetic in blend, and no division. This is a performance optimization.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]
	The clippingSpec notNil case is separated in another method. Assume it is nil after the check and call.
	This method is essentially a simplification of the clippingSpec variant. Keep it like that when modified."
	| pixelIndex edgesUpToThisPixel edgesThisPixel strokeAntiAliasAlphaBits displayX affectedBitsIndex segmentLength alphasOrEdgeCountsInThisSegment lastSegmentIndex aux1 intFillAlpha intFillRedBlue intFillGreen |
	<var: 'intFillAlpha' type: #uint32_t>
	<var: 'intFillRedBlue' type: #uint32_t>
	<var: 'intFillGreen' type: #uint32_t>
	<var: 'strokeAntiAliasAlphaBits' type: #'uint32_t'>
	<var: 'edgesThisPixel' type: #'uint8_t'>
	<var: 'edgesUpToThisPixel' type: #'uint8_t'>
	self
		primitive: 'primBlendFillOnlyWP'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger).
	
	clippingSpec ifNotNil: [
		^self  blendFillOnlyWPclipSpecLeft: l top: t right: r bottom: b ].

	intFillAlpha := fillColorWord >>24.
	intFillRedBlue := fillColorWord bitAnd: 16r00FF00FF.
	intFillGreen  := fillColorWord bitAnd: 16r0000FF00.
	lastSegmentIndex := -1.
	t to: b do: [ :displayY |
		edgesUpToThisPixel := 0.
		pixelIndex := displayY * targetWidth + l.
		displayX := l.
		[ displayX <= r ] whileTrue: [
			
			affectedBitsIndex := pixelIndex>>4.
			lastSegmentIndex = affectedBitsIndex ifFalse: [
				alphasOrEdgeCountsInThisSegment := (affectedBits at: affectedBitsIndex) = 1.
				lastSegmentIndex := affectedBitsIndex.
				affectedBits at: affectedBitsIndex put: 0 ].
			segmentLength := affectedBitsIndex+1 << 4 - pixelIndex.
			alphasOrEdgeCountsInThisSegment | (edgesUpToThisPixel ~= 0) ifTrue: [
				aux1 := r-displayX+1.
				1 to: (segmentLength min: aux1) do: [ :idx |
					
					strokeAntiAliasAlphaBits := 0.
					alphasOrEdgeCountsInThisSegment ifTrue: [
						edgesThisPixel := edgeCountsWP at: pixelIndex.
						edgesThisPixel = 0 ifFalse: [
							edgeCountsWP at: pixelIndex put: 0.
							self cCode: nil inSmalltalk: [
								"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
								edgesThisPixel > 127 ifTrue: [ edgesThisPixel := edgesThisPixel - 256 ]].
							edgesUpToThisPixel := edgesUpToThisPixel + edgesThisPixel ].
							strokeAntiAliasAlphaBits := alphaMaskWP at: pixelIndex.
							alphaMaskWP at: pixelIndex put: 0 ].
						
					edgesUpToThisPixel = 0
						ifFalse: [			"Inside the shape"
							(intFillAlpha = 16rFF and: [strokeAntiAliasAlphaBits = 16r00])
								ifTrue: [	"Optimize common case: opaque fill, inside fill area, no anti aliasing, no clipping at this point."
									targetBits at: pixelIndex put: fillColorWord.
									morphIds at: pixelIndex put: currentMorphId ]
								ifFalse: [	"Inside the shape. Turn stroke anti aliasing into fill anti aliasing."
									self blendFillOnlyWPAt: pixelIndex antiAliasAlphaByte: 16r7F - strokeAntiAliasAlphaBits
													"Still in the anti aliasing area, but outside the shape, strictly speaking."
										fillAlpha: intFillAlpha fillRedBlue: intFillRedBlue fillGreen: intFillGreen ]]
						ifTrue: [
							strokeAntiAliasAlphaBits = 16r00 ifFalse: [
								self blendFillOnlyWPAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits
									fillAlpha: intFillAlpha fillRedBlue: intFillRedBlue fillGreen: intFillGreen ]].
					
					displayX := displayX + 1.
					pixelIndex := pixelIndex + 1 ]]
			ifFalse: [				"All alphas and edgeCounts are zero in this segment of length delta"
				displayX := displayX + segmentLength.
				pixelIndex := pixelIndex + segmentLength ]]].! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - blend stroke and fill' stamp: 'jmv 21/Aug/2026, 6:14:22 pm (UTC)'!
blendFillOnlyWPclipSpecLeft: l top: t right: r bottom: b
	"Blends fill color over background.
	WP: Whole pixel anti aliasing.
	Ignore targetAssumedOpaque, take it as is true, target translucency is ignored ande set to 255,
	to allow integer only arithnetic in blend, and no division. This is a performance optimization.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]
	clippingSpec is not nil. Apply it.
	Sender is essentially a simplification of this method. Keep them in sync when modifying."
	| pixelIndex edgesUpToThisPixel edgesThisPixel strokeAntiAliasAlphaBits clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetAlpha displayX affectedBitsIndex segmentLength alphasOrEdgeCountsInThisSegment lastSegmentIndex aux1 intFillAlpha intFillRedBlue intFillGreen |
	<var: 'intFillAlpha' type: #uint32_t>
	<var: 'intFillRedBlue' type: #uint32_t>
	<var: 'intFillGreen' type: #uint32_t>
	<var: 'strokeAntiAliasAlphaBits' type: #'uint32_t'>
	<var: 'edgesThisPixel' type: #'uint8_t'>
	<var: 'edgesUpToThisPixel' type: #'uint8_t'>
	
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedClippedLeftPixel := targetWidth."targetWidth means effectively no AA for clipping"
	antiAliasedClippedRightPixel := targetWidth."targetWidth means effectively no AA for clipping"
	clippingSpecIndex := t * 2 + 1 - 1.
	intFillAlpha := fillColorWord >>24. mustResetAlpha := false.
	intFillRedBlue := fillColorWord bitAnd: 16r00FF00FF.
	intFillGreen  := fillColorWord bitAnd: 16r0000FF00.
	lastSegmentIndex := -1.
	t to: b do: [ :displayY |
		clippingSpecL := clippingSpec at: clippingSpecIndex.
		clippingSpecR := clippingSpec at: clippingSpecIndex+1.
		antiAliasedClippedLeftPixel := clippingSpecL >= l ifTrue: [clippingSpecL] ifFalse: [targetWidth].
		antiAliasedClippedRightPixel := clippingSpecR <= r ifTrue: [clippingSpecR] ifFalse: [targetWidth].
		edgesUpToThisPixel := 0.
		pixelIndex := displayY * targetWidth + l.
		displayX := l.
		[ displayX <= r ] whileTrue: [
			
			affectedBitsIndex := pixelIndex>>4.
			lastSegmentIndex = affectedBitsIndex ifFalse: [
				alphasOrEdgeCountsInThisSegment := (affectedBits at: affectedBitsIndex) = 1.
				lastSegmentIndex := affectedBitsIndex.
				affectedBits at: affectedBitsIndex put: 0 ].
			segmentLength := affectedBitsIndex+1 << 4 - pixelIndex.
			alphasOrEdgeCountsInThisSegment | (edgesUpToThisPixel ~= 0) ifTrue: [
				aux1 := r-displayX+1.
				1 to: (segmentLength min: aux1) do: [ :idx |
					
					strokeAntiAliasAlphaBits := 0.
					alphasOrEdgeCountsInThisSegment ifTrue: [
						edgesThisPixel := edgeCountsWP at: pixelIndex.
						edgesThisPixel = 0 ifFalse: [
							edgeCountsWP at: pixelIndex put: 0.
							self cCode: nil inSmalltalk: [
								"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
								edgesThisPixel > 127 ifTrue: [ edgesThisPixel := edgesThisPixel - 256 ]].
							edgesUpToThisPixel := edgesUpToThisPixel + edgesThisPixel ].
							strokeAntiAliasAlphaBits := alphaMaskWP at: pixelIndex.
							alphaMaskWP at: pixelIndex put: 0 ].
					
					(displayX >= clippingSpecL and: [ displayX <= clippingSpecR ]) ifTrue: [
						(displayX = antiAliasedClippedLeftPixel or: [ displayX = antiAliasedClippedRightPixel])
							ifTrue: [
								intFillAlpha := fillColorWord >>26. mustResetAlpha := true ]
							ifFalse: [
								(displayX-1 = antiAliasedClippedLeftPixel or: [ displayX+1 = antiAliasedClippedRightPixel]) ifTrue: [
									intFillAlpha := (fillColorWord >>25) + fillColorWord >>26. mustResetAlpha := true ]].
						
						edgesUpToThisPixel = 0
							ifFalse: [			"Inside the shape"
								(intFillAlpha = 16rFF and: [strokeAntiAliasAlphaBits = 16r00])
									ifTrue: [	"Optimize common case: opaque fill, inside fill area, no anti aliasing, no clipping at this point."
										targetBits at: pixelIndex put: fillColorWord.
										morphIds at: pixelIndex put: currentMorphId ]
									ifFalse: [	"Inside the shape. Turn stroke anti aliasing into fill anti aliasing."
										self blendFillOnlyWPAt: pixelIndex antiAliasAlphaByte: 16r7F - strokeAntiAliasAlphaBits
														"Still in the anti aliasing area, but outside the shape, strictly speaking."
											fillAlpha: intFillAlpha fillRedBlue: intFillRedBlue fillGreen: intFillGreen ]]
							ifTrue: [
								strokeAntiAliasAlphaBits = 16r00 ifFalse: [
									self blendFillOnlyWPAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits
										fillAlpha: intFillAlpha fillRedBlue: intFillRedBlue fillGreen: intFillGreen ]].
						
							mustResetAlpha ifTrue: [
								intFillAlpha := fillColorWord >>24. mustResetAlpha := false ]].
					displayX := displayX + 1.
					pixelIndex := pixelIndex + 1 ]]
			ifFalse: [				"All alphas and edgeCounts are zero in this segment of length delta"
				displayX := displayX + segmentLength.
				pixelIndex := pixelIndex + segmentLength ]].
		
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - blend stroke and fill' stamp: 'jmv 21/Aug/2026, 2:07:16 pm (UTC)'!
blendStrokeAndFillWPAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits strokeAlpha: strokeAlpha strokeRedBlue: strokeRedBlue strokeGreen: strokeGreen fillAlpha: fillAlpha fillRedBlue: fillRedBlue fillGreen: fillGreen
	"Inside the shape: Do an appropriate (anti aliased) gradient between stoke color and fill color.
	Blend this over background.
	WP: Whole pixel anti aliasing.
	Target translucency is ignored and set to opaque.
	Optimized implementation using integer arithmetic and no division."

	<var: 'strokeAntiAliasAlphaBits' type: #'uint32_t'>
	<var: 'strokeAlpha' type: #uint32_t>
	<var: 'strokeRedBlue' type: #uint32_t>
	<var: 'strokeGreen' type: #uint32_t>
	<var: 'fillAlpha' type: #uint32_t>
	<var: 'fillRedBlue' type: #uint32_t>
	<var: 'fillGreen' type: #uint32_t>
	| strokeAAAlpha strokeAAUnAlpha foreRedBlue foreGreen alpha unAlpha targetWord targetRedBlue targetGreen resultRedBlue resultGreen |

	<var: 'strokeAAAlpha' type: #uint32_t>
	<var: 'strokeAAUnAlpha' type: #uint32_t>
	<var: 'foreRedBlue' type: #uint32_t>
	<var: 'foreGreen' type: #uint32_t>
	<var: 'alpha' type: #uint32_t>
	<var: 'unAlpha' type: #uint32_t>
	<var: 'targetWord' type: #'uint32_t'>
	<var: 'targetRedBlue' type: #uint32_t>
	<var: 'targetGreen' type: #uint32_t>
	<var: 'resultRedBlue' type: #uint32_t>
	<var: 'resultGreen' type: #uint32_t>

	"Start by blending Stroke color and Fill color according to strokeAntiAliasAlphaBits at this point. Same for their Alpha channel."
	strokeAAAlpha := strokeAntiAliasAlphaBits << 1.
	strokeAAUnAlpha := 255- strokeAAAlpha.

	"Blend Red and Blue channels simultaneously. Blend Green channel. Blend Alpha Channel."
	foreRedBlue := ((((strokeRedBlue * strokeAAAlpha) + (fillRedBlue * strokeAAUnAlpha)) + 16r00FF00FF) >> 8) bitAnd: 16r00FF00FF.
	foreGreen := ((((strokeGreen * strokeAAAlpha) + (fillGreen * strokeAAUnAlpha)) + 16r0000FF00) >> 8) bitAnd: 16r0000FF00.
	alpha := (((strokeAlpha * strokeAAAlpha) + (fillAlpha * strokeAAUnAlpha)) + 16r000000FF) >> 8.
	unAlpha := 255 - alpha.

	"Blend the computed Fore Color (using its Alpha channel) over Target."	
	targetWord := targetBits at: pixelIndex.
	targetRedBlue  := targetWord  bitAnd: 16r00FF00FF.
	targetGreen   := targetWord  bitAnd: 16r0000FF00.

	"Blend Red and Blue channels simultaneously"
	resultRedBlue := ((((foreRedBlue * alpha) + (targetRedBlue * unAlpha)) + 16r00FF00FF) >> 8) bitAnd: 16r00FF00FF.
	"Blend Green channel"
	resultGreen := ((((foreGreen * alpha) + (targetGreen * unAlpha)) + 16r0000FF00) >> 8) bitAnd: 16r0000FF00.

	"Combine channels back together. Resuilt alpha set to opaque."
	targetWord := 16rFF000000 bitOr: (resultRedBlue bitOr: resultGreen).
	
	targetBits at: pixelIndex put: targetWord.
	morphIds at: pixelIndex put: currentMorphId.
! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - blend stroke and fill' stamp: 'jmv 21/Aug/2026, 6:13:14 pm (UTC)'!
blendStrokeAndFillWPLeft: l top: t right: r bottom: b
	"Blends stroke color and fill color over background.
	Do an appropriate (anti aliased) gradient between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	WP: Whole pixel anti aliasing.
	Ignore targetAssumedOpaque, take it as is true, target translucency is ignored ande set to 255,
	to allow integer only arithnetic in blend, and no division. This is a performance optimization.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]
	The clippingSpec notNil case is separated in another method. Assume it is nil after the check and call.
	This method is essentially a simplification of the clippingSpec variant. Keep it like that when modified."
	| pixelIndex edgesUpToThisPixel edgesThisPixel strokeAntiAliasAlphaBits displayX affectedBitsIndex segmentLength alphasOrEdgeCountsInThisSegment lastSegmentIndex 
	aux1 intStrokeAlpha intStrokeRedBlue intStrokeGreen intFillAlpha intFillRedBlue intFillGreen |
	<var: 'intStrokeAlpha' type: #uint32_t>
	<var: 'intStrokeRedBlue' type: #uint32_t>
	<var: 'intStrokeGreen' type: #uint32_t>
	<var: 'intFillAlpha' type: #uint32_t>
	<var: 'intFillRedBlue' type: #uint32_t>
	<var: 'intFillGreen' type: #uint32_t>
	<var: 'strokeAntiAliasAlphaBits' type: #'uint32_t'>
	<var: 'edgesThisPixel' type: #'uint8_t'>
	<var: 'edgesUpToThisPixel' type: #'uint8_t'>
	self
		primitive: 'primBlendStrokeAndFillWP'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger).
	
	clippingSpec ifNotNil: [
		^self blendStrokeAndFillWPclipSpecLeft: l top: t right: r bottom: b ].

	intStrokeAlpha := strokeColorWord >>24.
	intStrokeRedBlue := strokeColorWord bitAnd: 16r00FF00FF.
	intStrokeGreen  := strokeColorWord bitAnd: 16r0000FF00.
	intFillAlpha := fillColorWord >>24.
	intFillRedBlue := fillColorWord bitAnd: 16r00FF00FF.
	intFillGreen  := fillColorWord bitAnd: 16r0000FF00.
	lastSegmentIndex := -1.
	t to: b do: [ :displayY |
		edgesUpToThisPixel := 0.
		pixelIndex := displayY * targetWidth + l.
		displayX := l.
		[ displayX <= r ] whileTrue: [
			
			affectedBitsIndex := pixelIndex>>4.
			lastSegmentIndex = affectedBitsIndex ifFalse: [
				alphasOrEdgeCountsInThisSegment := (affectedBits at: affectedBitsIndex) = 1.
				lastSegmentIndex := affectedBitsIndex.
				affectedBits at: affectedBitsIndex put: 0 ].
			segmentLength := affectedBitsIndex+1 << 4 - pixelIndex.
			alphasOrEdgeCountsInThisSegment | (edgesUpToThisPixel ~= 0) ifTrue: [
				aux1 := r-displayX+1.
				1 to: (segmentLength min: aux1) do: [ :idx |
					
					strokeAntiAliasAlphaBits := 0.
					alphasOrEdgeCountsInThisSegment ifTrue: [
						edgesThisPixel := edgeCountsWP at: pixelIndex.
						edgesThisPixel = 0 ifFalse: [
							edgeCountsWP at: pixelIndex put: 0.
							self cCode: nil inSmalltalk: [
								"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
								edgesThisPixel > 127 ifTrue: [ edgesThisPixel := edgesThisPixel - 256 ]].
							edgesUpToThisPixel := edgesUpToThisPixel + edgesThisPixel ].
							strokeAntiAliasAlphaBits := alphaMaskWP at: pixelIndex.
							alphaMaskWP at: pixelIndex put: 0 ].
						
					strokeAntiAliasAlphaBits = 16r00
						ifTrue: [ 		"Not in the stroke at all. Either fully in the fill, or outside the shape (pixel is unaffected)."
							edgesUpToThisPixel ~= 0
								ifTrue: [
									"Fully inside the fill, far from anti aliasing."
									intFillAlpha = 16rFF
										ifTrue: [
											"Fill color is opaque. Just overwrite with fill color."
											targetBits at: pixelIndex put: fillColorWord.
											morphIds at: pixelIndex put: currentMorphId ]
										ifFalse: [
											"Translucent color. Do proper blend of fill over target."
											self blendFillOnlyWPAt: pixelIndex antiAliasAlphaByte: 16r7F
												fillAlpha: intFillAlpha fillRedBlue: intFillRedBlue fillGreen: intFillGreen ]]]
						ifFalse: [		"At least partially in the stroke."
							strokeAntiAliasAlphaBits = 16r7F
								ifTrue: [
									"Fully inside the stroke, far from anti aliasing."
									intStrokeAlpha = 16rFF
										ifTrue: [
											"Stroke color is opaque. Just overwrite with stroke color."
											targetBits at: pixelIndex put: strokeColorWord.
											morphIds at: pixelIndex put: currentMorphId ]
										ifFalse: [
											"Translucent color or target. Do proper blend of stroke over target."
											self blendStrokeOnlyWPAt: pixelIndex antiAliasAlphaByte: 16r7F
												strokeAlpha: intStrokeAlpha strokeRedBlue: intStrokeRedBlue strokeGreen: intStrokeGreen  ]]
								ifFalse: [
									"In an anti aliased part of the stroke. Either blend stroke over background, or pre-mix stroke and fill."
									edgesUpToThisPixel ~= 0
										ifTrue: [ 
											"Inside the shape. Blend stroke and fill, blend result over target."
											self blendStrokeAndFillWPAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits
												strokeAlpha: intStrokeAlpha strokeRedBlue: intStrokeRedBlue strokeGreen: intStrokeGreen
												fillAlpha: intFillAlpha fillRedBlue: intFillRedBlue fillGreen: intFillGreen ]
										ifFalse: [
											"In the outer anti aliasing area of the stroke. Blend stroke over background."
											self blendStrokeOnlyWPAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits
												strokeAlpha: intStrokeAlpha strokeRedBlue: intStrokeRedBlue strokeGreen: intStrokeGreen ]]].
							
					displayX := displayX + 1.
					pixelIndex := pixelIndex + 1 ]]
			ifFalse: [				"All alphas and edgeCounts are zero in this segment of length delta"
				displayX := displayX + segmentLength.
				pixelIndex := pixelIndex + segmentLength ]]].! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - blend stroke and fill' stamp: 'jmv 21/Aug/2026, 6:16:21 pm (UTC)'!
blendStrokeAndFillWPclipSpecLeft: l top: t right: r bottom: b
	"Blends stroke color and fill color over background.
	Do an appropriate (anti aliased) gradient between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	WP: Whole pixel anti aliasing.
	Ignore targetAssumedOpaque, take it as is true, target translucency is ignored ande set to 255,
	to allow integer only arithnetic in blend, and no division. This is a performance optimization.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]
	clippingSpec is not nil. Apply it.
	Sender is essentially a simplification of this method. Keep them in sync when modifying."
	| pixelIndex edgesUpToThisPixel edgesThisPixel strokeAntiAliasAlphaBits clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetAlpha displayX affectedBitsIndex segmentLength alphasOrEdgeCountsInThisSegment lastSegmentIndex 
	aux1 intStrokeAlpha intStrokeRedBlue intStrokeGreen intFillAlpha intFillRedBlue intFillGreen |
	<var: 'intStrokeAlpha' type: #uint32_t>
	<var: 'intStrokeRedBlue' type: #uint32_t>
	<var: 'intStrokeGreen' type: #uint32_t>
	<var: 'intFillAlpha' type: #uint32_t>
	<var: 'intFillRedBlue' type: #uint32_t>
	<var: 'intFillGreen' type: #uint32_t>
	<var: 'strokeAntiAliasAlphaBits' type: #'uint32_t'>
	<var: 'edgesThisPixel' type: #'uint8_t'>
	<var: 'edgesUpToThisPixel' type: #'uint8_t'>
	
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedClippedLeftPixel := targetWidth."targetWidth means effectively no AA for clipping"
	antiAliasedClippedRightPixel := targetWidth."targetWidth means effectively no AA for clipping"
	clippingSpecIndex := t * 2 + 1 - 1.
	intStrokeAlpha := strokeColorWord >>24. mustResetAlpha := false.
	intStrokeRedBlue := strokeColorWord bitAnd: 16r00FF00FF.
	intStrokeGreen  := strokeColorWord bitAnd: 16r0000FF00.
	intFillAlpha := fillColorWord >>24. mustResetAlpha := false.
	intFillRedBlue := fillColorWord bitAnd: 16r00FF00FF.
	intFillGreen  := fillColorWord bitAnd: 16r0000FF00.
	lastSegmentIndex := -1.
	t to: b do: [ :displayY |
		clippingSpecL := clippingSpec at: clippingSpecIndex.
		clippingSpecR := clippingSpec at: clippingSpecIndex+1.
		antiAliasedClippedLeftPixel := clippingSpecL >= l ifTrue: [clippingSpecL] ifFalse: [targetWidth].
		antiAliasedClippedRightPixel := clippingSpecR <= r ifTrue: [clippingSpecR] ifFalse: [targetWidth].
		edgesUpToThisPixel := 0.
		pixelIndex := displayY * targetWidth + l.
		displayX := l.
		[ displayX <= r ] whileTrue: [
			
			affectedBitsIndex := pixelIndex>>4.
			lastSegmentIndex = affectedBitsIndex ifFalse: [
				alphasOrEdgeCountsInThisSegment := (affectedBits at: affectedBitsIndex) = 1.
				lastSegmentIndex := affectedBitsIndex.
				affectedBits at: affectedBitsIndex put: 0 ].
			segmentLength := affectedBitsIndex+1 << 4 - pixelIndex.
			alphasOrEdgeCountsInThisSegment | (edgesUpToThisPixel ~= 0) ifTrue: [
				aux1 := r-displayX+1.
				1 to: (segmentLength min: aux1) do: [ :idx |
					
					strokeAntiAliasAlphaBits := 0.
					alphasOrEdgeCountsInThisSegment ifTrue: [
						edgesThisPixel := edgeCountsWP at: pixelIndex.
						edgesThisPixel = 0 ifFalse: [
							edgeCountsWP at: pixelIndex put: 0.
							self cCode: nil inSmalltalk: [
								"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
								edgesThisPixel > 127 ifTrue: [ edgesThisPixel := edgesThisPixel - 256 ]].
							edgesUpToThisPixel := edgesUpToThisPixel + edgesThisPixel ].
							strokeAntiAliasAlphaBits := alphaMaskWP at: pixelIndex.
							alphaMaskWP at: pixelIndex put: 0 ].
				
					(displayX >= clippingSpecL and: [ displayX <= clippingSpecR ]) ifTrue: [
						(displayX = antiAliasedClippedLeftPixel or: [ displayX = antiAliasedClippedRightPixel])
							ifTrue: [
								intStrokeAlpha := strokeColorWord >>26. intFillAlpha := fillColorWord >>26. mustResetAlpha := true ]
							ifFalse: [
								(displayX-1 = antiAliasedClippedLeftPixel or: [ displayX+1 = antiAliasedClippedRightPixel]) ifTrue: [
									intStrokeAlpha := (strokeColorWord >>25) + (strokeColorWord >>26).
									intFillAlpha := (fillColorWord >>25) + fillColorWord >>26. mustResetAlpha := true ]].
						
						strokeAntiAliasAlphaBits = 16r00
							ifTrue: [ 		"Not in the stroke at all. Either fully in the fill, or outside the shape (pixel is unaffected)."
								edgesUpToThisPixel ~= 0
									ifTrue: [
										"Fully inside the fill, far from anti aliasing."
										intFillAlpha = 16rFF
											ifTrue: [
												"Fill color is opaque. Just overwrite with fill color."
												targetBits at: pixelIndex put: fillColorWord.
												morphIds at: pixelIndex put: currentMorphId ]
											ifFalse: [
												"Translucent color. Do proper blend of fill over target."
												self blendFillOnlyWPAt: pixelIndex antiAliasAlphaByte: 16r7F
													fillAlpha: intFillAlpha fillRedBlue: intFillRedBlue fillGreen: intFillGreen ]]]
							ifFalse: [		"At least partially in the stroke."
								strokeAntiAliasAlphaBits = 16r7F
									ifTrue: [
										"Fully inside the stroke, far from anti aliasing."
										intStrokeAlpha = 16rFF
											ifTrue: [
												"Stroke color is opaque. Just overwrite with stroke color."
												targetBits at: pixelIndex put: strokeColorWord.
												morphIds at: pixelIndex put: currentMorphId ]
											ifFalse: [
												"Translucent color or target. Do proper blend of stroke over target."
												self blendStrokeOnlyWPAt: pixelIndex antiAliasAlphaByte: 16r7F
													strokeAlpha: intStrokeAlpha strokeRedBlue: intStrokeRedBlue strokeGreen: intStrokeGreen  ]]
									ifFalse: [
										"In an anti aliased part of the stroke. Either blend stroke over background, or pre-mix stroke and fill."
										edgesUpToThisPixel ~= 0
											ifTrue: [ 
												"Inside the shape. Blend stroke and fill, blend result over target."
												self blendStrokeAndFillWPAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits
													strokeAlpha: intStrokeAlpha strokeRedBlue: intStrokeRedBlue strokeGreen: intStrokeGreen
													fillAlpha: intFillAlpha fillRedBlue: intFillRedBlue fillGreen: intFillGreen ]
											ifFalse: [
												"In the outer anti aliasing area of the stroke. Blend stroke over background."
												self blendStrokeOnlyWPAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits
													strokeAlpha: intStrokeAlpha strokeRedBlue: intStrokeRedBlue strokeGreen: intStrokeGreen ]]].
							
							mustResetAlpha ifTrue: [
								intStrokeAlpha := strokeColorWord >>24. intFillAlpha := fillColorWord >>24. mustResetAlpha := false ]].
					displayX := displayX + 1.
					pixelIndex := pixelIndex + 1 ]]
			ifFalse: [				"All alphas and edgeCounts are zero in this segment of length delta"
				displayX := displayX + segmentLength.
				pixelIndex := pixelIndex + segmentLength ]].
		
		clippingSpecIndex := clippingSpecIndex + 2 ].! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - blend stroke and fill' stamp: 'jmv 21/Aug/2026, 2:08:11 pm (UTC)'!
blendStrokeOnlyWPAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits strokeAlpha: strokeAlpha strokeRedBlue: strokeRedBlue strokeGreen: strokeGreen
	"Blends stroke color over background.
	WP: Whole pixel anti aliasing.
	Target translucency is ignored and set to opaque.
	Optimized implementation using integer arithmetic and no division."

	<var: 'strokeAntiAliasAlphaBits' type: #'uint32_t'>
	<var: 'strokeAlpha' type: #uint32_t>
	<var: 'strokeRedBlue' type: #uint32_t>
	<var: 'strokeGreen' type: #uint32_t>
	| alpha unAlpha targetWord targetRedBlue targetGreen resultRedBlue resultGreen |

	<var: 'alpha' type: #uint32_t>
	<var: 'unAlpha' type: #uint32_t>
	<var: 'targetWord' type: #'uint32_t'>
	<var: 'targetRedBlue' type: #uint32_t>
	<var: 'targetGreen' type: #uint32_t>
	<var: 'resultRedBlue' type: #uint32_t>
	<var: 'resultGreen' type: #uint32_t>

	alpha := (strokeAntiAliasAlphaBits * strokeAlpha) >> 7.
	unAlpha := 255 - alpha.
	targetWord := targetBits at: pixelIndex.
	targetRedBlue  := targetWord  bitAnd: 16r00FF00FF.
	targetGreen   := targetWord  bitAnd: 16r0000FF00.

	"Blend Red and Blue channels simultaneously"
	resultRedBlue := ((((strokeRedBlue * alpha) + (targetRedBlue * unAlpha)) + 16r00FF00FF) >> 8) bitAnd: 16r00FF00FF.
	"Blend Green channel"
	resultGreen := ((((strokeGreen * alpha) + (targetGreen * unAlpha)) + 16r0000FF00) >> 8) bitAnd: 16r0000FF00.

	"Combine channels back together. Resuilt alpha set to opaque."
	targetWord := 16rFF000000 bitOr: (resultRedBlue bitOr: resultGreen).
	
	targetBits at: pixelIndex put: targetWord.
	morphIds at: pixelIndex put: currentMorphId.
! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - blend stroke and fill' stamp: 'jmv 21/Aug/2026, 6:13:44 pm (UTC)'!
blendStrokeOnlyWPLeft: l top: t right: r bottom: b
	"Blends stroke color over background.
	WP: Whole pixel anti aliasing.
	Ignore targetAssumedOpaque, take it as is true, target translucency is ignored ande set to 255,
	to allow integer only arithnetic in blend, and no division. This is a performance optimization.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]
	The clippingSpec notNil case is separated in another method. Assume it is nil after the check and call.
	This method is essentially a simplification of the clippingSpec variant. Keep it like that when modified."
	| pixelIndex strokeAntiAliasAlphaBits displayX affectedBitsIndex segmentLength alphasOrEdgeCountsInThisSegment lastSegmentIndex aux1 intStrokeAlpha intStrokeRedBlue intStrokeGreen |
	<var: 'intStrokeAlpha' type: #uint32_t>
	<var: 'intStrokeRedBlue' type: #uint32_t>
	<var: 'intStrokeGreen' type: #uint32_t>
	<var: 'strokeAntiAliasAlphaBits' type: #uint32_t>
	self
		primitive: 'primBlendStrokeOnlyWP'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger).
	
	clippingSpec ifNotNil: [
		^self blendStrokeOnlyWPclipSpecLeft: l top: t right: r bottom: b ].

	intStrokeAlpha := strokeColorWord >>24.
	intStrokeRedBlue := strokeColorWord bitAnd: 16r00FF00FF.
	intStrokeGreen  := strokeColorWord bitAnd: 16r0000FF00.
	lastSegmentIndex := -1.
	t to: b do: [ :displayY |
		pixelIndex := displayY * targetWidth + l.
		displayX := l.
		[ displayX <= r ] whileTrue: [
			
			affectedBitsIndex := pixelIndex>>4.
			lastSegmentIndex = affectedBitsIndex ifFalse: [
				alphasOrEdgeCountsInThisSegment := (affectedBits at: affectedBitsIndex) = 1.
				lastSegmentIndex := affectedBitsIndex.
				affectedBits at: affectedBitsIndex put: 0 ].
			segmentLength := affectedBitsIndex+1 << 4 - pixelIndex.
			alphasOrEdgeCountsInThisSegment ifTrue: [
				aux1 := r-displayX+1.
				1 to: (segmentLength min: aux1) do: [ :idx |
					strokeAntiAliasAlphaBits := alphaMaskWP at: pixelIndex.
					strokeAntiAliasAlphaBits = 16r00 ifFalse: [					"In the stroke"
						alphaMaskWP at: pixelIndex put: 0.
						(intStrokeAlpha = 16rFF and: [strokeAntiAliasAlphaBits = 16r7F])
							ifTrue: [ "Optimize inner part of a wide stroke: Fully opaque stroke, no anti aliasing, no clipping at this point."
								targetBits at: pixelIndex put: strokeColorWord.
								morphIds at: pixelIndex put: currentMorphId ]
							ifFalse: [ "General case."
								self blendStrokeOnlyWPAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits
									strokeAlpha: intStrokeAlpha strokeRedBlue: intStrokeRedBlue strokeGreen: intStrokeGreen ]].
					displayX := displayX + 1.
					pixelIndex := pixelIndex + 1 ]]
			ifFalse: [				"All alphas and edgeCounts are zero in this segment of length delta"
				displayX := displayX + segmentLength.
				pixelIndex := pixelIndex + segmentLength ]]].! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - blend stroke and fill' stamp: 'jmv 21/Aug/2026, 6:16:33 pm (UTC)'!
blendStrokeOnlyWPclipSpecLeft: l top: t right: r bottom: b
	"Blends stroke color over background.
	WP: Whole pixel anti aliasing.
	Ignore targetAssumedOpaque, take it as is true, target translucency is ignored ande set to 255,
	to allow integer only arithnetic in blend, and no division. This is a performance optimization.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]
	clippingSpec is not nil. Apply it.
	Sender is essentially a simplification of this method. Keep them in sync when modifying."
	| pixelIndex strokeAntiAliasAlphaBits clippingSpecL clippingSpecR clippingSpecIndex antiAliasedClippedLeftPixel antiAliasedClippedRightPixel mustResetAlpha displayX affectedBitsIndex segmentLength alphasOrEdgeCountsInThisSegment lastSegmentIndex aux1 intStrokeAlpha intStrokeRedBlue intStrokeGreen |
	<var: 'intStrokeAlpha' type: #uint32_t>
	<var: 'intStrokeRedBlue' type: #uint32_t>
	<var: 'intStrokeGreen' type: #uint32_t>
	<var: 'strokeAntiAliasAlphaBits' type: #uint32_t>
	
	clippingSpecL := 0.
	clippingSpecR := targetWidth-1.
	antiAliasedClippedLeftPixel := targetWidth."targetWidth means effectively no AA for clipping"
	antiAliasedClippedRightPixel := targetWidth."targetWidth means effectively no AA for clipping"
	clippingSpecIndex := t * 2 + 1 - 1.
	intStrokeAlpha := strokeColorWord >>24. mustResetAlpha := false.
	intStrokeRedBlue := strokeColorWord bitAnd: 16r00FF00FF.
	intStrokeGreen  := strokeColorWord bitAnd: 16r0000FF00.
	lastSegmentIndex := -1.
	t to: b do: [ :displayY |
		clippingSpecL := clippingSpec at: clippingSpecIndex.
		clippingSpecR := clippingSpec at: clippingSpecIndex+1.
		antiAliasedClippedLeftPixel := clippingSpecL >= l ifTrue: [clippingSpecL] ifFalse: [targetWidth].
		antiAliasedClippedRightPixel := clippingSpecR <= r ifTrue: [clippingSpecR] ifFalse: [targetWidth].
		pixelIndex := displayY * targetWidth + l.
		displayX := l.
		[ displayX <= r ] whileTrue: [
			
			affectedBitsIndex := pixelIndex>>4.
			lastSegmentIndex = affectedBitsIndex ifFalse: [
				alphasOrEdgeCountsInThisSegment := (affectedBits at: affectedBitsIndex) = 1.
				lastSegmentIndex := affectedBitsIndex.
				affectedBits at: affectedBitsIndex put: 0 ].
			segmentLength := affectedBitsIndex+1 << 4 - pixelIndex.
			alphasOrEdgeCountsInThisSegment ifTrue: [
				aux1 := r-displayX+1.
				1 to: (segmentLength min: aux1) do: [ :idx |
					
					strokeAntiAliasAlphaBits := alphaMaskWP at: pixelIndex.
					strokeAntiAliasAlphaBits = 16r00 ifFalse: [										"In the stroke"
						alphaMaskWP at: pixelIndex put: 0.
						
						(displayX >= clippingSpecL and: [ displayX <= clippingSpecR ]) ifTrue: [
							(displayX = antiAliasedClippedLeftPixel or: [ displayX = antiAliasedClippedRightPixel])
								ifTrue: [
									intStrokeAlpha := strokeColorWord >>26. mustResetAlpha := true ]
								ifFalse: [
									(displayX-1 = antiAliasedClippedLeftPixel or: [ displayX+1 = antiAliasedClippedRightPixel]) ifTrue: [
										intStrokeAlpha := (strokeColorWord >>25) + (strokeColorWord >>26). mustResetAlpha := true ]].
							
							(intStrokeAlpha = 16rFF and: [strokeAntiAliasAlphaBits = 16r7F])
								ifTrue: [ "Optimize inner part of a wide stroke: Fully opaque stroke, no anti aliasing, no clipping at this point."
									targetBits at: pixelIndex put: strokeColorWord.
									morphIds at: pixelIndex put: currentMorphId ]
								ifFalse: [ "General case."
									self blendStrokeOnlyWPAt: pixelIndex antiAliasAlphaByte: strokeAntiAliasAlphaBits
										strokeAlpha: intStrokeAlpha strokeRedBlue: intStrokeRedBlue strokeGreen: intStrokeGreen ].
								
								mustResetAlpha ifTrue: [
									intStrokeAlpha := strokeColorWord >>24. mustResetAlpha := false ]]].
					displayX := displayX + 1.
					pixelIndex := pixelIndex + 1 ]]
			ifFalse: [				"All alphas and edgeCounts are zero in this segment of length delta"
				displayX := displayX + segmentLength.
				pixelIndex := pixelIndex + segmentLength ]].
		
		clippingSpecIndex := clippingSpecIndex + 2 ].
! !

!VectorEnginePlugin methodsFor: 'other' stamp: 'eem 5/Feb/2025, 8:58:00 pm (UTC)'!
computeBoundsControlX: controlX Y: controlY startX: startX Y: startY endX: endX Y: endY
	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
	<inline: #always>
	| xMaxEnd xMinEnd yMaxEnd yMinEnd |
	xMinEnd := startX min: endX.
	xMaxEnd := startX max: endX.
	yMinEnd := startY min: endY.
	yMaxEnd := startY max: endY.
	spanLeft := spanLeft min: (xMinEnd min: (xMinEnd+controlX) / 2.0).
	spanRight := spanRight max: (xMaxEnd max: (xMaxEnd+controlX) / 2.0).
	spanTop := spanTop min: (yMinEnd min: (yMinEnd+controlY) / 2.0).
	spanBottom := spanBottom max: (yMaxEnd max: (yMaxEnd+controlY) / 2.0)! !

!VectorEnginePlugin methodsFor: 'other' stamp: 'jmv 20/Aug/2026, 6:27:45 pm (UTC)'!
displayStringLoop: aString displayIf: guardBlock wholePixel: wholePixel contourIndexAccessor: contourIndexBlock from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData
	<inline: #always>
	| nextGlyphX nextGlyphY ttX ttY advanceWidth numContours contourStartX contourStartY
	  numBeziers startX startY endX endY controlX controlY dx dy aux1 i length
	  t t0 x0 y0 oneLessT increment f1 f2 f3 x y correction |
	<var: 'nextGlyphX' type: #float>
	<var: 'nextGlyphY' type: #float>
	<var: 'ttX' type: #float>
	<var: 'ttY' type: #float>
	<var: 'advanceWidth' type: #float>
	"
	<var: 'boundsLeft' type: #float>
	<var: 'boundsRight' type: #float>
	<var: 'boundsBottom' type: #float>
	<var: 'boundsTop' type: #float>
	"
	<var: 'contourStartX' type: #float>
	<var: 'contourStartY' type: #float>
	<var: 'startX' type: #float>
	<var: 'startY' type: #float>
	<var: 'endX' type: #float>
	<var: 'endY' type: #float>
	<var: 'controlX' type: #float>
	<var: 'controlY' type: #float>
	<var: 'dx' type: #float>
	<var: 'dy' type: #float>
	<var: 'aux1' type: #float>
	<var: 'length' type: #float>
	<var: 'correction' type: #float>
	<var: 't' type: #float>
	<var: 't0' type: #float>
	<var: 'x0' type: #float>
	<var: 'y0' type: #float>
	<var: 'oneLessT' type: #float>
	<var: 'increment' type: #float>
	<var: 'f1' type: #float>
	<var: 'f2' type: #float>
	<var: 'f3' type: #float>
	<var: 'x' type: #float>
	<var: 'y' type: #float>

	trajectoryLength := 0.0.
	needsFullAlphaCircle := true.
	txA11 := txA11 * sx.
	txA12 := txA12 * sy.
	txA21 := txA21 * sx.
	txA22 := txA22 * sy.

	nextGlyphX := destX / sx.
	nextGlyphY := destY / sy.
	startIndex-1 to: stopIndex-1 do:
		[ :index | "Index points to a byte in a ByteString or UTF8String, or to a code point in an UTF32String"
		i := contourIndexBlock value: index.
		guardBlock value ifTrue:
			[i := i - 1.
			advanceWidth := contourData at: i.
			"boundsLeft := contourData at: i+1.
			boundsRight := contourData at: i+2.
			boundsBottom := contourData at: i+3.
			boundsTop := contourData at: i+4." i := i + 5.
			numContours := (contourData at: i) asInteger. i := i + 1.
			1 to: numContours do:
				[ :idx |
				numBeziers := (contourData at: i) asInteger.
				ttX := (contourData at: i + 1) + nextGlyphX.
				ttY := (contourData at: i + 2) + nextGlyphY.
				i := i + 3.
				contourStartX := startX := (ttX * txA11) + (ttY * txA12) + txA13.
				contourStartY := startY := (ttX * txA21) + (ttY * txA22) + txA23.
				self initializeTrajectoryFragment.
				1 to: numBeziers do:
					[ :idx2 |
					ttX := contourData at: i.
					ttY := contourData at: i + 1.
					endX := (ttX * txA11) + (ttY * txA12) + startX.
					endY := (ttX * txA21) + (ttY * txA22) + startY.
					ttX := contourData at: i + 2.
					ttY := contourData at: i + 3.
					controlX := (ttX * txA11) + (ttY * txA12) + startX.
					controlY := (ttX * txA21) + (ttY * txA22) + startY.
					i := i + 4.
					self computeBoundsControlX: controlX Y: controlY startX: startX Y: startY endX: endX Y: endY.
					"Compute Quadratic Bezier Curve,"
					"Case t = 0.0"
					x := startX.
					y := startY.
					wholePixel
						ifTrue:
							[self updateAlphasWPZeroStrokeForX: x y: y.
							 self updateEdgeCountWPAtX: x y: y]
						ifFalse:
							[self updateAlphasSPForX: x y: y.
							 self updateEdgeCountSPAtX: x y: y].
					"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
					dx := (endX-startX) abs. dy := (endY-startY) abs.
					aux1 := dx max: dy.
					aux1 := 0.5 / aux1.
					increment := aux1 min: 0.5.
					t := 0.0.
					[
						t0 := t. x0 := x. y0 := y.
						"Compute next point"
						t := t0 + increment. oneLessT := 1.0 - t.
						f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
						x := (f1 * startX) + (f2 * controlX) + (f3 * endX).
						y := (f1 * startY) + (f2 * controlY) + (f3 * endY).
						"Now adjust the increment to aim at the required hop length, and recompute next point."
						dx := x-x0. dy := y-y0.
						length := ((dx*dx) + (dy*dy)) sqrt.
						correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
						[
							increment := increment * correction.
							t := t0 + increment. oneLessT := 1.0 - t.
							f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
							x := (f1 * startX) + (f2 * controlX) + (f3 * endX).
							y := (f1 * startY) + (f2 * controlY) + (f3 * endY).
							dx := x-x0. dy := y-y0.
							length := ((dx*dx) + (dy*dy)) sqrt.
							correction := hop / (length max: 0.1). "Don't grow increment too much in one step. More importantly, don't divide by zero under any circumstances."
							correction < 0.99] whileTrue.		   "Keep adjusting if actual hop would be too big"
						t < 1.0 ]
					whileTrue:
						[wholePixel
							ifTrue:
								[self updateAlphasWPZeroStrokeForX: x y: y.
								 self updateEdgeCountWPAtX: x y: y]
							ifFalse:
								[self updateAlphasSPForX: x y: y.
								 self updateEdgeCountSPAtX: x y: y]].
					"Note: For TrueType font definitions, we assume that all contour fragments start exactly where the previous ends.
					This means that the end point is only added for the last fragment of the contour, and not for each one of them."
					startX := endX.
					startY := endY].
				wholePixel
					ifTrue:
						[self updateAlphasWPZeroStrokeForX: endX y: endY.
						 self updateEdgeCountWPAtX: endX y: endY.
						"Similar effect to ensureClosePath in #finishPath:,
						but assume the TrueType definition is essentially right, and there might only be a rounding error.
						So, don't draw a line, but just (possibly) correct edgeCountsWP. The possibility of rounding error is most likely zero.
						Anyway, this is cheap."
						self updateEdgeCountWPAtX: contourStartX y: contourStartY]
					ifFalse:
						[self updateAlphasSPForX: endX y: endY.
						 self updateEdgeCountSPAtX: endX y: endY.
						"Similar effect to ensureClosePath in #finishPath:,
						but assume the TrueType definition is essentially right, and there might only be a rounding error.
						So, don't draw a line, but just (possibly) correct edgeCounts. The possibility of rounding error is most likely zero.
						Anyway, this is cheap."
						self updateEdgeCountSPAtX: contourStartX y: contourStartY]].
			nextGlyphX := nextGlyphX + advanceWidth]].
	txA11 := txA11 / sx.
	txA12 := txA12 / sy.
	txA21 := txA21 / sx.
	txA22 := txA22 / sy.
	^nextGlyphX * sx asOop: Float! !

!VectorEnginePlugin methodsFor: 'other' stamp: 'jmv (auto pragmas eem 2024-10-17) 5/Jul/2021, 7:22:38 pm (UTC)'!
updateContourForX: x y: y
	"The Contour of the current morph is the pixel area affected, 	but without holes.
	For each y line where a pixel is affected, record the leftmost and rightmost affected pixels.
	Note: Only includes pen trajectory, but not stroke width."

	<var: 'x' type: #float>
	<var: 'y' type: #float>
	| thisYRounded |

	

	thisYRounded := (y + 0.5) asInteger.
	(thisYRounded between: 0 and: targetHeight-1) ifTrue: [
		thisYRounded = prevYRounded ifFalse: [
			prevYRounded = 16r7FFFFFFF ifFalse: [
				contour at: prevYRounded * 2 put: leftAtThisY.
				contour at: prevYRounded * 2 + 1 put: rightAtThisY ].
			leftAtThisY := contour at: thisYRounded * 2.
			rightAtThisY := contour at: thisYRounded * 2 + 1.
			prevYRounded := thisYRounded ].
		leftAtThisY := leftAtThisY min: x.
		rightAtThisY := rightAtThisY max: x ].! !

!VectorEnginePlugin methodsFor: 'other' stamp: 'jmv 22/Apr/2025, 3:29:00 pm (UTC)'!
updateContourLastLine
	self
		primitive: 'primUpdateContourLastLine'
		parameters: #().

	prevYRounded = 16r7FFFFFFF ifFalse: [
		contour at: prevYRounded * 2 put: leftAtThisY.
		contour at: prevYRounded * 2 + 1 put: rightAtThisY ].! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - text and multi paths' stamp: 'jmv 20/Aug/2026, 6:05:01 pm (UTC)'!
displayByteStringSP: aByteString from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes

	<var: 'byte' type: #'uint8_t'>
	self
		primitive: 'primDisplayByteString'
		parameters: #(String SmallInteger SmallInteger Float Float Float Float Float32Array IntegerArray).

	^self
		displayStringLoop: aByteString
		displayIf: [true]
		wholePixel: false
		contourIndexAccessor:
			[ :byteIndex | | byte i |
			byte := aByteString at: byteIndex.
			self cCode: '' inSmalltalk: [byte := (aByteString at: byteIndex+1) codePoint.].
			i := contourDataIndexes at: byte.
			i < 1 ifTrue: [ i := 1 ].
			i]
		from: startIndex
		to: stopIndex
		atx: destX
		y: destY
		scalex: sx
		y: sy
		contourData: contourData! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - text and multi paths' stamp: 'jmv 20/Aug/2026, 6:05:10 pm (UTC)'!
displayUtf32SP: aWordArray from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes

	self
		primitive: 'primDisplayUtf32'
		parameters: #(WordArray SmallInteger SmallInteger Float Float Float Float Float32Array IntegerArray).

	^self
		displayStringLoop: aWordArray
		displayIf: [true]
		wholePixel: false
		contourIndexAccessor:
			[ :utf32Index | | utf32 utf8Byte i |
			utf32 := aWordArray at: utf32Index.
			utf32 <= 16r7F
				ifTrue: [
					utf8Byte := utf32.
					contourDataIndexes at: utf8Byte ]
				ifFalse: [ utf32 <= 16r7FF
				ifTrue: [
					utf8Byte := (utf32 bitShift: -6) bitOr: 2r11000000.
					i := contourDataIndexes at: utf8Byte.
					utf8Byte := (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
					contourDataIndexes at: utf8Byte - i ]
				ifFalse: [ utf32 <= 16rFFFF
				ifTrue: [
					utf8Byte := (utf32 bitShift: -12) bitOr: 2r11100000.
					i := contourDataIndexes at: utf8Byte.
					utf8Byte := ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
					i := contourDataIndexes at: utf8Byte - i.
					utf8Byte := (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
					contourDataIndexes at: utf8Byte - i ]
				ifFalse: [
					utf8Byte := (utf32 bitShift: -18) bitOr: 2r11110000.
					i := contourDataIndexes at: utf8Byte.
					utf8Byte := ((utf32 bitShift: -12) bitAnd: 2r00111111) bitOr: 2r10000000.
					i := contourDataIndexes at: utf8Byte - i.
					utf8Byte := ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
					i := contourDataIndexes at: utf8Byte - i.
					utf8Byte := (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
					contourDataIndexes at: utf8Byte - i ]]]]
		from: startIndex
		to: stopIndex
		atx: destX
		y: destY
		scalex: sx
		y: sy
		contourData: contourData! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - text and multi paths' stamp: 'jmv 20/Aug/2026, 6:05:34 pm (UTC)'!
displayUtf8SP: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes

	| baseIndex |
	<var: 'byte' type: #'uint8_t'>
	self
		primitive: 'primDisplayUtf8'
		parameters: #(ByteArray SmallInteger SmallInteger Float Float Float Float Float32Array IntegerArray).

	baseIndex := 0.
	^self
		displayStringLoop: aByteArray
		displayIf: [baseIndex = 0]
		wholePixel: false
		contourIndexAccessor:
			[ :byteIndex | | byte i |
			byte := aByteArray at: byteIndex.
			i := contourDataIndexes at: baseIndex + byte.
			baseIndex := i >= 0 ifTrue: [0] ifFalse: [0 - i].
			i]
		from: byteStartIndex
		to: byteStopIndex
		atx: destX
		y: destY
		scalex: sx
		y: sy
		contourData: contourData! !

!VectorEnginePlugin methodsFor: 'sub pixel anti aliased - text and multi paths' stamp: 'jmv 20/Aug/2026, 5:56:50 pm (UTC)'!
pathSequenceSP: aFloat32Array size: size

	| i commandType startX startY endX endY control1X control1Y control2X control2Y |
	<var: 'startX' type: #float>
	<var: 'startY' type: #float>
	<var: 'endX' type: #float>
	<var: 'endY' type: #float>
	<var: 'control1X' type: #float>
	<var: 'control1Y' type: #float>
	<var: 'control2X' type: #float>
	<var: 'control2Y' type: #float>
	self
		primitive: 'primPathSequence'
		parameters: #(Float32Array SmallInteger).

	i := 0.
	[ i < size ] whileTrue: [
		commandType := (aFloat32Array at: i) asInteger. i := i + 1.
		commandType caseOf: {
			[0] -> [
				i+1 < size ifFalse: [ ^self "failure" ].
				startX := aFloat32Array at: i. i := i + 1.
				startY := aFloat32Array at: i. i := i + 1.
				self initializeTrajectoryFragment.
				].
			[1] -> [
				i+1 < size ifFalse: [ ^self "failure" ].
				endX := aFloat32Array at: i. i := i + 1.
				endY := aFloat32Array at: i. i := i + 1.
				self pvt_lineSPFromX: startX y: startY toX: endX y: endY.
				startX := endX.
				startY := endY.
				].
			[2] -> [
				i+3 < size ifFalse: [ ^self "failure" ].
				endX := aFloat32Array at: i. i := i + 1.
				endY := aFloat32Array at: i. i := i + 1.
				control1X := aFloat32Array at: i. i := i + 1.
				control1Y := aFloat32Array at: i. i := i + 1.
				self pvt_quadraticBezierSPFromX: startX y: startY
					toX: endX y: endY
					controlX: control1X y: control1Y.
				startX := endX.
				startY := endY.
				].
			[3] -> [
				i+5 < size ifFalse: [ ^self "failure" ].
				endX := aFloat32Array at: i. i := i + 1.
				endY := aFloat32Array at: i. i := i + 1.
				control1X := aFloat32Array at: i. i := i + 1.
				control1Y := aFloat32Array at: i. i := i + 1.
				control2X := aFloat32Array at: i. i := i + 1.
				control2Y := aFloat32Array at: i. i := i + 1.
				self pvt_cubicBezierSPFromX: startX y: startY
					toX: endX y: endY
					control1X: control1X y: control1Y
					control2X: control2X y: control2Y.
				startX := endX.
				startY := endY.
				]}
		otherwise: [ ^self "failure"]].! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - text and multi paths' stamp: 'jmv 24/Apr/2025, 1:06:00 pm (UTC)'!
displayByteStringWP: aByteString from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes

	<var: 'byte' type: #'uint8_t'>
	self
		primitive: 'primDisplayByteStringWP'
		parameters: #(String SmallInteger SmallInteger Float Float Float Float Float32Array IntegerArray).

	^self
		displayStringLoop: aByteString
		displayIf: [true]
		wholePixel: true
		contourIndexAccessor:
			[ :byteIndex | | byte i |
			byte := aByteString at: byteIndex.
			self cCode: '' inSmalltalk: [byte := (aByteString at: byteIndex+1) codePoint.].
			i := contourDataIndexes at: byte.
			i < 1 ifTrue: [ i := 1 ].
			i]
		from: startIndex
		to: stopIndex
		atx: destX
		y: destY
		scalex: sx
		y: sy
		contourData: contourData! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - text and multi paths' stamp: 'jmv 8/Apr/2025, 3:13:00 pm (UTC)'!
displayUtf32WP: aWordArray from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes

	self
		primitive: 'primDisplayUtf32WP'
		parameters: #(WordArray SmallInteger SmallInteger Float Float Float Float Float32Array IntegerArray).

	^self
		displayStringLoop: aWordArray
		displayIf: [true]
		wholePixel: true
		contourIndexAccessor:
			[ :utf32Index | | utf32 utf8Byte i |
			utf32 := aWordArray at: utf32Index.
			utf32 <= 16r7F
				ifTrue: [
					utf8Byte := utf32.
					contourDataIndexes at: utf8Byte ]
				ifFalse: [ utf32 <= 16r7FF
				ifTrue: [
					utf8Byte := (utf32 bitShift: -6) bitOr: 2r11000000.
					i := contourDataIndexes at: utf8Byte.
					utf8Byte := (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
					contourDataIndexes at: utf8Byte - i ]
				ifFalse: [ utf32 <= 16rFFFF
				ifTrue: [
					utf8Byte := (utf32 bitShift: -12) bitOr: 2r11100000.
					i := contourDataIndexes at: utf8Byte.
					utf8Byte := ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
					i := contourDataIndexes at: utf8Byte - i.
					utf8Byte := (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
					contourDataIndexes at: utf8Byte - i ]
				ifFalse: [
					utf8Byte := (utf32 bitShift: -18) bitOr: 2r11110000.
					i := contourDataIndexes at: utf8Byte.
					utf8Byte := ((utf32 bitShift: -12) bitAnd: 2r00111111) bitOr: 2r10000000.
					i := contourDataIndexes at: utf8Byte - i.
					utf8Byte := ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
					i := contourDataIndexes at: utf8Byte - i.
					utf8Byte := (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
					contourDataIndexes at: utf8Byte - i ]]]]
		from: startIndex
		to: stopIndex
		atx: destX
		y: destY
		scalex: sx
		y: sy
		contourData: contourData! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - text and multi paths' stamp: 'jmv 8/Apr/2025, 3:14:00 pm (UTC)'!
displayUtf8WP: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes

	| baseIndex |
	<var: 'byte' type: #'uint8_t'>
	self
		primitive: 'primDisplayUtf8WP'
		parameters: #(ByteArray SmallInteger SmallInteger Float Float Float Float Float32Array IntegerArray).

	baseIndex := 0.
	^self
		displayStringLoop: aByteArray
		displayIf: [baseIndex = 0]
		wholePixel: true
		contourIndexAccessor:
			[ :byteIndex | | byte i |
			byte := aByteArray at: byteIndex.
			i := contourDataIndexes at: baseIndex + byte.
			baseIndex := i >= 0 ifTrue: [0] ifFalse: [0 - i].
			i]
		from: byteStartIndex
		to: byteStopIndex
		atx: destX
		y: destY
		scalex: sx
		y: sy
		contourData: contourData! !

!VectorEnginePlugin methodsFor: 'whole pixel anti aliased - text and multi paths' stamp: 'jmv 8/Apr/2025, 2:58:00 pm (UTC)'!
pathSequenceWP: aFloat32Array size: size

	| i commandType startX startY endX endY control1X control1Y control2X control2Y |
	<var: 'startX' type: #float>
	<var: 'startY' type: #float>
	<var: 'endX' type: #float>
	<var: 'endY' type: #float>
	<var: 'control1X' type: #float>
	<var: 'control1Y' type: #float>
	<var: 'control2X' type: #float>
	<var: 'control2Y' type: #float>
	self
		primitive: 'primPathSequenceWP'
		parameters: #(Float32Array SmallInteger).

	i := 0.
	[ i < size ] whileTrue: [
		commandType := (aFloat32Array at: i) asInteger. i := i + 1.
		commandType caseOf: {
			[0] -> [
				i+1 < size ifFalse: [ ^self "failure" ].
				startX := aFloat32Array at: i. i := i + 1.
				startY := aFloat32Array at: i. i := i + 1.
				self initializeTrajectoryFragment.
				].
			[1] -> [
				i+1 < size ifFalse: [ ^self "failure" ].
				endX := aFloat32Array at: i. i := i + 1.
				endY := aFloat32Array at: i. i := i + 1.
				self pvt_lineWPFromX: startX y: startY toX: endX y: endY.
				startX := endX.
				startY := endY.
				].
			[2] -> [
				i+3 < size ifFalse: [ ^self "failure" ].
				endX := aFloat32Array at: i. i := i + 1.
				endY := aFloat32Array at: i. i := i + 1.
				control1X := aFloat32Array at: i. i := i + 1.
				control1Y := aFloat32Array at: i. i := i + 1.
				self pvt_quadraticBezierWPFromX: startX y: startY
					toX: endX y: endY
					controlX: control1X y: control1Y.
				startX := endX.
				startY := endY.
				].
			[3] -> [
				i+5 < size ifFalse: [ ^self "failure" ].
				endX := aFloat32Array at: i. i := i + 1.
				endY := aFloat32Array at: i. i := i + 1.
				control1X := aFloat32Array at: i. i := i + 1.
				control1Y := aFloat32Array at: i. i := i + 1.
				control2X := aFloat32Array at: i. i := i + 1.
				control2Y := aFloat32Array at: i. i := i + 1.
				self pvt_cubicBezierWPFromX: startX y: startY
					toX: endX y: endY
					control1X: control1X y: control1Y
					control2X: control2X y: control2Y.
				startX := endX.
				startY := endY.
				]}
		otherwise: [ ^self "failure"]].! !

!VectorEnginePlugin methodsFor: 'instance initialization' stamp: 'jmv 4/Apr/2025, 6:38:00 pm (UTC)'!
initialize
	"c.f. declareCVarsIn:"
	antiAliasingWidth :=
	subPixelDelta :=
	hop :=
	strokeWidth :=
	strokeR :=
	strokeG :=
	strokeB :=
	strokeA :=
	dashedStrokeBits :=
	dashBitCount :=
	dashBitLength :=
	dashBitOffset:=
	fillR :=
	fillG :=
	fillB :=
	fillA :=
	txA11 :=
	txA12 :=
	txA13 :=
	txA21 :=
	txA22 :=
	txA23 :=
	auxAntiAliasingWidthScaledInverse :=
	auxStrokeWidthDilatedHalf :=
	auxStrokeWidthDilatedHalfSquared :=
	auxStrokeWidthErodedHalfSquared :=
	spanLeft :=
	spanTop :=
	spanRight :=
	spanBottom :=
	leftAtThisY :=
	rightAtThisY :=
	trajectoryLength := 0.0! !

!VectorEnginePlugin class methodsFor: 'translation' stamp: 'jmv 20/Aug/2026, 6:18:58 pm (UTC)'!
declareCVarsIn: aCCodeGenerator


	aCCodeGenerator addHeaderFile: '<stdint.h>'.
	aCCodeGenerator var: 'targetBits' type: 'uint32_t *'.
	aCCodeGenerator var: 'morphIds' type: 'uint32_t *'.
	aCCodeGenerator var: 'edgeCountsSP' type: 'uint32_t *'.
	aCCodeGenerator var: 'edgeCountsWP' type: 'uint8_t *'.
	aCCodeGenerator var: 'alphaMaskSP' type: 'uint32_t *'.
	aCCodeGenerator var: 'alphaMaskWP' type: 'uint8_t *'.
	aCCodeGenerator var: 'affectedBits' type: 'uint8_t *'.
	aCCodeGenerator var: 'contour' type: 'float *'.
	aCCodeGenerator var: 'currentMorphId' type: 'uint32_t'.
	aCCodeGenerator var: 'clippingSpec' type: 'int *'.
	aCCodeGenerator var: 'strokeColorWord' type: 'uint32_t'.
	aCCodeGenerator var: 'fillColorWord' type: 'uint32_t'.
	#(antiAliasingWidth subPixelDelta hop strokeWidth strokeR strokeG strokeB strokeA dashBitLength fillR fillG fillB fillA txA11 txA12 txA13 txA21 txA22 txA23 auxAntiAliasingWidthScaledInverse auxStrokeWidthDilatedHalf auxStrokeWidthDilatedHalfSquared auxStrokeWidthErodedHalfSquared spanLeft spanTop spanRight spanBottom leftAtThisY rightAtThisY trajectoryLength) do: [ :varName |
		aCCodeGenerator var: varName type: 'float' ].! !

!VectorEnginePlugin class methodsFor: 'simulation' stamp: 'jmv 12/Nov/2025, 6:11:00 pm (UTC)'!
simulatorClass
	^VectorEnginePluginSimulator! !
