'From Cuis 5.0 [latest update: #4911] on 12 October 2021 at 10:46:22 am'!
'Description '!
!provides: 'VectorEnginePlugin' 1 31!
!requires: 'VectorGraphics' 1 201 nil!
!requires: 'VMMaker' 1 7 nil!
SystemOrganization addCategory: 'VectorEnginePlugin'!


!classDefinition: #VectorEnginePlugin category: 'VectorEnginePlugin'!
SmartSyntaxInterpreterPlugin subclass: #VectorEnginePlugin
	instanceVariableNames: 'targetBits morphIds edgeCounts edgeCountsWP alphaMask alphaMaskWP contour targetWidth targetHeight antiAliasingWidth subPixelDelta hop strokeWidth strokeR strokeG strokeB strokeA fillR fillG fillB fillA txA11 txA12 txA13 txA21 txA22 txA23 currentMorphId currentClipsSubmorphs clipCurrentMorph clipLeft clipTop clipRight clipBottom auxAntiAliasingWidthScaledInverse auxStrokeWidthDilatedHalf auxStrokeWidthDilatedHalfSquared auxStrokeWidthErodedHalfSquared spanLeft spanTop spanRight spanBottom prevYTruncated prevYRounded leftAtThisY rightAtThisY'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorEnginePlugin'!
!classDefinition: 'VectorEnginePlugin class' category: 'VectorEnginePlugin'!
VectorEnginePlugin class
	instanceVariableNames: ''!


!VectorEnginePlugin commentStamp: '<historical>' prior: 0!
Slang implementation of VectorGraphicsEngine. Based on https://www.researchgate.net/publication/267152327_Prefiltering_Antialiasing_for_General_Vector_Graphics

VectorEnginePlugin translateInDirectory: DirectoryEntry currentDirectory doInlining: true!

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 6/11/2021 12:05:12'!
antiAliasingWidth: aFloat subPixelDelta: otherFloat hopLength: anotherFloat
	self
		primitive: 'primAntiAliasingWidthsubPixelDeltaHopLength'
		parameters: #(Float Float Float).

	antiAliasingWidth := aFloat.
	auxAntiAliasingWidthScaledInverse := 127.0 / aFloat.
	subPixelDelta := otherFloat.
	hop := anotherFloat! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 9/21/2021 20:07:36'!
clipCurrentMorph: aBoolean morphIds: aWordArray
	self
		primitive: 'primClipCurrentMorph'
		parameters: #(Boolean WordArray).

	morphIds := aWordArray.
	(clipCurrentMorph and: [ aBoolean not ]) ifTrue: [
		| pixelIndex |
		clipTop to: clipBottom do: [ :displayY |
			pixelIndex := displayY * targetWidth + clipLeft.
			clipLeft to: clipRight do: [ :displayX |
				morphIds at: pixelIndex put: ((morphIds at: pixelIndex) bitAnd: 16rFFFFFF00).
				pixelIndex := pixelIndex + 1 ]]].
	clipCurrentMorph := aBoolean! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 3/11/2021 11:00:23'!
clipLeft: l clipTop: t clipRight: r clipBottom: b
	self
		primitive: 'primClipLeftclipTopclipRightclipBottom'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger).

	clipLeft := l.
	clipTop := t.
	clipRight := r.
	clipBottom := b! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 3/11/2021 11:00:29'!
currentMorphId: aNumber currentClipsSubmorphs: aBoolean
"Bound it someway to 31 or 32 bits (SmallInteger in 32 bits, or uint in 32 bits, etc...)"
	self
		primitive: 'primCurrentMorphIdcurrentClipsSubmorphs'
		parameters: #(SmallInteger Boolean).

	currentMorphId := aNumber.
	currentMorphId = 0 ifTrue: [ clipCurrentMorph := false ].												"Essentially for initialization"
	currentClipsSubmorphs := aBoolean.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 5/17/2021 16:29:46'!
fillR: r g: g b: b a: a
	self
		primitive: 'primFillRGBA'
		parameters: #(Float Float Float Float).

	fillR := r * 255.0.
	fillG := g * 255.0.
	fillB := b * 255.0.
	fillA := a.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 5/13/2021 11:46:17'!
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

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 7/5/2021 16:11:13'!
initializePath
	self
		primitive: 'primInitializePath'
		parameters: #().

	spanLeft := targetWidth.						"drawable right. Will later be refined."
	spanTop := targetHeight.						"drawable bottom. Will later be refined."
	spanRight := 0.						"drawable left. Will later be refined."
	spanBottom := 0.						"drawable top. Will later be refined."
	prevYRounded := 16r7FFFFFFF.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'dtl 7/27/2021 16:40:35'!
initializeTrajectoryFragment

	prevYTruncated := 16r7FFFFFFF.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'dtl 7/27/2021 16:41:01'!
newTrajectoryFragment
	self
		primitive: 'primNewTrajectoryFragment'
		parameters: #().

	^ self initializeTrajectoryFragment.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 8/22/2021 16:11:57'!
pluginApiVersion
	"
	VectorEngineWithPlugin isPluginAvailable
	API version is a SmallInteger.
	Note: this is Api version, not package version. If no Api change, Api version doesn't change, regardless of changes in the Plugin or Smalltalk code.
	See senders and implementors.
	"
	self
		primitive: 'pluginApiVersion'
		parameters: #().

	^ 6 asOop: SmallInteger.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 8/22/2021 16:09:48'!
resetContourTop: t bottom: b contour: aFloat32Array
	self
		primitive: 'primReset2Contour'
		parameters: #(SmallInteger SmallInteger Float32Array).

	contour := aFloat32Array.
	leftAtThisY := targetWidth.
	rightAtThisY := 0.
	t to: b do: [ :y |
		contour at: y * 2 put: targetWidth.
		contour at: y * 2 + 1 put: 0 ].! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 3/10/2021 10:53:54'!
spanBottom
	self
		primitive: 'primSpanBottom'
		parameters: #().

	^(spanBottom + auxStrokeWidthDilatedHalf) asInteger asOop: SmallInteger.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 4/6/2021 16:13:04'!
spanLeft
	self
		primitive: 'primSpanLeft'
		parameters: #().

	"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	^(spanLeft - auxStrokeWidthDilatedHalf - subPixelDelta + 1) asInteger asOop: SmallInteger! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 4/6/2021 16:13:38'!
spanRight
	self
		primitive: 'primSpanRight'
		parameters: #().

	"Make room not just for updated mask, but also edges (hence, +1)"
	^(spanRight + auxStrokeWidthDilatedHalf + subPixelDelta) asInteger +1 asOop: SmallInteger.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 4/6/2021 16:13:45'!
spanTop
	self
		primitive: 'primSpanTop'
		parameters: #().

	"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	^(spanTop - auxStrokeWidthDilatedHalf + 1) asInteger asOop: SmallInteger! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 5/17/2021 16:30:15'!
strokeR: r g: g b: b a: a
	self
		primitive: 'primStrokeRGBA'
		parameters: #(Float Float Float Float).

	strokeR := r * 255.0.
	strokeG := g * 255.0.
	strokeB := b * 255.0.
	strokeA := a.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 4/12/2021 11:21:40'!
strokeWidth: aNumber
	| swErodedHalf |
	self
		primitive: 'primStrokeWidth'
		parameters: #(Float).
	self var: #swErodedHalf type: 'float'.

	strokeWidth := aNumber.
	auxStrokeWidthDilatedHalf := strokeWidth + antiAliasingWidth * 0.5.
	auxStrokeWidthDilatedHalfSquared := auxStrokeWidthDilatedHalf * auxStrokeWidthDilatedHalf.
	swErodedHalf := strokeWidth - antiAliasingWidth * 0.5.
	self cCode: 
			'auxStrokeWidthErodedHalfSquared = swErodedHalf * fabs(swErodedHalf);'
		inSmalltalk: [ auxStrokeWidthErodedHalfSquared := swErodedHalf * swErodedHalf abs ].! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 7/5/2021 17:04:09'!
targetBits: aBitmap morphIds: aWordArray edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array targetWidth: aNumber targetHeight: otherNumber
	"All arrays could be pinned instead of passing them every time!!"

	self
		primitive: 'primSetTarget'
		parameters: #(WordArray WordArray WordArray WordArray Float32Array SmallInteger SmallInteger).

	targetBits := aBitmap.
	morphIds := aWordArray.
	edgeCounts := otherWordArray.
	alphaMask := anotherWordArray.
	contour := aFloat32Array.
	targetWidth := aNumber.
	targetHeight := otherNumber.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 7/5/2021 17:04:14'!
targetBits: aBitmap morphIds: aWordArray edgeCountsWP: otherByteArray alphaMaskWP: anotherByteArray contour: aFloat32Array targetWidth: aNumber targetHeight: otherNumber
	"All arrays could be pinned instead of passing them every time!!"

	self
		primitive: 'primSetTargetWP'
		parameters: #(WordArray WordArray ByteArray ByteArray Float32Array SmallInteger SmallInteger).

	targetBits := aBitmap.
	morphIds := aWordArray.
	edgeCountsWP := otherByteArray.
	alphaMaskWP := anotherByteArray.
	contour := aFloat32Array.
	targetWidth := aNumber.
	targetHeight := otherNumber.! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 9/15/2021 15:09:43'!
arcCenterX: centerX centerY: centerY radiusX: radiusPointX radiusY: radiusPointY start: startAngle sweep: sweepAngle rotationCos: tthetaCos rotationSin: tthetaSin edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array

	| tcx tcy trx try scale hops d angle xp yp x y |
	self
		primitive: 'primArc'
		parameters: #(Float Float Float Float Float Float Float Float WordArray WordArray Float32Array).

	self var: #tcx type: 'float'.
	self var: #tcy type: 'float'.
	self var: #trx type: 'float'.
	self var: #try type: 'float'.
	self var: #scale type: 'float'.
	self var: #angle type: 'float'.
	self var: #xp type: 'float'.
	self var: #yp type: 'float'.
	self var: #x type: 'float'.
	self var: #y type: 'float'.
	self var: #hops type: 'int'.
	self var: #d type: 'float'.

	tcx := (centerX * txA11) + (centerY * txA12) + txA13.
	tcy := (centerX * txA21) + (centerY * txA22) + txA23.
	self cCode: 'scale = sqrt(txA11*txA11 + txA21*txA21);'
		inSmalltalk: [scale := (txA11 squared + txA21 squared) sqrt].
	trx := radiusPointX * scale.
	try := radiusPointY * scale.
	hops := ((trx max: try) * (self cCode: 'fabs(sweepAngle)' inSmalltalk: [sweepAngle abs]) / hop) asInteger + 2.
	d := hops.

	edgeCounts := otherWordArray.
	alphaMask := anotherWordArray.
	contour := aFloat32Array.
	0 to: hops do: [ :h |
		self cCode: 'angle = ((float)h / d) * sweepAngle + startAngle;'
			inSmalltalk: [angle := h / d * sweepAngle + startAngle ].
		xp := angle cos * trx.
		yp := angle sin * try.
		x := tthetaCos * xp - (tthetaSin * yp) + tcx.
		y := tthetaSin * xp + (tthetaCos * yp) + tcy.
		spanLeft := spanLeft min: x.
		spanTop := spanTop min: y.
		spanRight := spanRight max: x.
		spanBottom := spanBottom max: y.
		self updateAlphasForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y. ].! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 7/5/2021 15:54:16'!
cubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2 edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array

	self
		primitive: 'primCubicBezier'
		parameters: #(Float Float Float Float Float Float Float Float WordArray WordArray Float32Array).

	edgeCounts := otherWordArray.
	alphaMask := anotherWordArray.
	contour := aFloat32Array.

	self pvt_cubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 7/5/2021 15:57:23'!
lineFromX: xFrom y: yFrom toX: xTo y: yTo edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array

	self
		primitive: 'primLine'
		parameters: #(Float Float Float Float WordArray WordArray Float32Array).

	edgeCounts := otherWordArray.
	alphaMask := anotherWordArray.
	contour := aFloat32Array.
	self pvt_lineFromX: xFrom y: yFrom toX: xTo y: yTo.! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 10/12/2021 09:58:38'!
pvt_cubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2

	| txFrom tyFrom txTo tyTo txControl1 tyControl1 txControl2 tyControl2 dx dy dx2 dy2 dx3 dy3 hops xMinEnd xMaxEnd yMinEnd yMaxEnd t oneLessT increment f1 f23 f2 f3 f4 x y |
	self var: #xFrom type: 'float'.
	self var: #yFrom type: 'float'.
	self var: #xTo type: 'float'.
	self var: #yTo type: 'float'.
	self var: #xControl1 type: 'float'.
	self var: #yControl1 type: 'float'.
	self var: #xControl2 type: 'float'.
	self var: #yControl2 type: 'float'.
	self var: #txFrom type: 'float'.
	self var: #tyFrom type: 'float'.
	self var: #txTo type: 'float'.
	self var: #tyTo type: 'float'.
	self var: #txControl1 type: 'float'.
	self var: #tyControl1 type: 'float'.
	self var: #txControl2 type: 'float'.
	self var: #tyControl2 type: 'float'.
	self var: #dx type: 'float'.
	self var: #dy type: 'float'.
	self var: #dx2 type: 'float'.
	self var: #dy2 type: 'float'.
	self var: #dx3 type: 'float'.
	self var: #dy3 type: 'float'.
	self var: #hops type: 'int'.

	self var: #t type: 'float'.
	self var: #oneLessT type: 'float'.
	self var: #increment type: 'float'.
	self var: #f1 type: 'float'.
	self var: #f23 type: 'float'.
	self var: #f2 type: 'float'.
	self var: #f3 type: 'float'.
	self var: #f4 type: 'float'.
	self var: #x type: 'float'.
	self var: #y type: 'float'.
	self var: #xMinEnd type: 'float'.
	self var: #xMaxEnd type: 'float'.
	self var: #yMinEnd type: 'float'.
	self var: #yMaxEnd type: 'float'.

	txFrom := (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom := (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo := (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo := (xTo * txA21) + (yTo * txA22) + txA23.
	txControl1 := (xControl1 * txA11) + (yControl1 * txA12) + txA13.
	tyControl1 := (xControl1 * txA21) + (yControl1 * txA22) + txA23.
	txControl2 := (xControl2 * txA11) + (yControl2 * txA12) + txA13.
	tyControl2 := (xControl2 * txA21) + (yControl2 * txA22) + txA23.
	self cCode: '
		dx = fabs(txControl1-txFrom);
		dx2 = fabs(txTo-txControl2);
		dx3 = fabs(txControl2-txControl1);
		dy = fabs(tyControl1-tyFrom);
		dy2 = fabs(tyTo-tyControl2);
		dy3 = fabs(tyControl2-tyControl1);'
	inSmalltalk: [
		dx := (txControl1-txFrom) abs.
		dx2 := (txTo-txControl2) abs.
		dx3 := (txControl2-txControl1) abs.
		dy := (tyControl1-tyFrom) abs.
		dy2 := (tyTo-tyControl2) abs.
		dy3 := (tyControl2-tyControl1) abs ].
	dx := (dx max: dx2) * 3 max: dx3 * 1.5.
	dy := (dy max: dy2) * 3 max: dy3 * 1.5.
	hops := ((dx max: dy) / hop) asInteger + 1.

	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
	xMinEnd := txFrom min: txTo.
	xMaxEnd := txFrom max: txTo.
	yMinEnd := tyFrom min: tyTo.
	yMaxEnd := tyFrom max: tyTo.
	spanLeft := spanLeft min: (xMinEnd min: xMinEnd*0.25 + ((txControl1 min: txControl2)*0.75)).
	spanRight := spanRight max: (xMaxEnd max: xMaxEnd*0.25 + ((txControl1 max: txControl2)*0.75)).
	spanTop := spanTop min: (yMinEnd min: yMinEnd*0.25 + ((tyControl1 min: tyControl2)*0.75)).
	spanBottom := spanBottom max: (yMaxEnd max: yMaxEnd*0.25 + ((tyControl1 max: tyControl2)*0.75)).

	t := 0.0.
	increment := 1.0 / hops.
	[ t < 1.0 ] whileTrue: [
		oneLessT := 1.0 - t.
		f1 := oneLessT * oneLessT * oneLessT.
		f23 := 3.0 * oneLessT * t.
		f2 := f23 * oneLessT.
		f3 := f23 * t.
		f4 := t * t * t.
		x := (f1 * txFrom) + (f2 * txControl1) + (f3 * txControl2) + (f4 * txTo).
		y := (f1 * tyFrom) + (f2 * tyControl1) + (f3 * tyControl2) + (f4 * tyTo).
		self updateAlphasForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y.
		t := t + increment. ].
	self updateAlphasForX: txTo y: tyTo.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 10/12/2021 10:18:55'!
pvt_lineFromX: xFrom y: yFrom toX: xTo y: yTo

	| txFrom tyFrom txTo tyTo dx dy hops t oneLessT increment x y |
	self var: #xFrom type: 'float'.
	self var: #yFrom type: 'float'.
	self var: #xTo type: 'float'.
	self var: #yTo type: 'float'.
	self var: #txFrom type: 'float'.
	self var: #tyFrom type: 'float'.
	self var: #txTo type: 'float'.
	self var: #tyTo type: 'float'.
	self var: #dx type: 'float'.
	self var: #dy type: 'float'.
	self var: #hops type: 'int'.

	self var: #t type: 'float'.
	self var: #oneLessT type: 'float'.
	self var: #increment type: 'float'.
	self var: #x type: 'float'.
	self var: #y type: 'float'.

	txFrom := (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom := (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo := (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo := (xTo * txA21) + (yTo * txA22) + txA23.
	dx := txTo-txFrom.
	dy := tyTo-tyFrom.
	self cCode: '
		dx = fabs(dx);
		dy = fabs(dy);'
	inSmalltalk: [
		dx := dx abs.
		dy := dy abs ].
	hops := ((dx max: dy) / hop) asInteger + 1.

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
		fillA = 0.0 ifFalse: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y.
		t := t + increment. ].
	self updateAlphasForX: txTo y: tyTo.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 7/22/2021 23:22:55'!
pvt_quadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl

	| txFrom tyFrom txTo tyTo txControl tyControl dx dy dx2 dy2 length xMinEnd xMaxEnd yMinEnd yMaxEnd t0 x0 y0 t oneLessT increment f1 f2 f3 x y correction |
	self var: #xFrom type: 'float'.
	self var: #yFrom type: 'float'.
	self var: #xTo type: 'float'.
	self var: #yTo type: 'float'.
	self var: #xControl type: 'float'.
	self var: #yControl type: 'float'.
	self var: #txFrom type: 'float'.
	self var: #tyFrom type: 'float'.
	self var: #txTo type: 'float'.
	self var: #tyTo type: 'float'.
	self var: #txControl type: 'float'.
	self var: #tyControl type: 'float'.
	self var: #dx type: 'float'.
	self var: #dy type: 'float'.
	self var: #dx2 type: 'float'.
	self var: #dy2 type: 'float'.
	self var: #length type: 'float'.
	self var: #correction type: 'float'.

	self var: #t0 type: 'float'.
	self var: #x0 type: 'float'.
	self var: #y0 type: 'float'.
	self var: #t type: 'float'.
	self var: #oneLessT type: 'float'.
	self var: #increment type: 'float'.
	self var: #f1 type: 'float'.
	self var: #f2 type: 'float'.
	self var: #f3 type: 'float'.
	self var: #x type: 'float'.
	self var: #y type: 'float'.
	self var: #xMinEnd type: 'float'.
	self var: #xMaxEnd type: 'float'.
	self var: #yMinEnd type: 'float'.
	self var: #yMaxEnd type: 'float'.

	"If control point is bogus, just draw a line"
	(xControl = xTo and: [ yControl = yTo ]) ifTrue: [
		^self pvt_lineFromX: xFrom y: yFrom toX: xTo y: yTo ].
	(xControl = xFrom and: [ yControl = yFrom ]) ifTrue: [
		^self pvt_lineFromX: xFrom y: yFrom toX: xTo y: yTo ].

	txFrom := (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom := (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo := (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo := (xTo * txA21) + (yTo * txA22) + txA23.
	txControl := (xControl * txA11) + (yControl * txA12) + txA13.
	tyControl := (xControl * txA21) + (yControl * txA22) + txA23.
	self cCode: '
		dx = fabs(txTo-txFrom);
		dx2 = fabs(txControl-txFrom);
		dy = fabs(tyTo-tyFrom);
		dy2 = fabs(tyControl-tyFrom);'
	inSmalltalk: [
		dx := (txTo-txFrom) abs.
		dx2 := (txControl-txFrom) abs.
		dy := (tyTo-tyFrom) abs.
		dy2 := (tyControl-tyFrom) abs.].

	"If almost a vertical line, just draw a line. (Ignoring control point)"
	(dx < 1.0 and: [dx2 < 1.0]) ifTrue: [
		^self pvt_lineFromX: xFrom y: yFrom toX: xTo y: yTo ].

	"If almost an horizontal line, just draw a line. (Ignoring control point)"
	(dy < 1.0 and: [dy2 < 1.0]) ifTrue: [
		^self pvt_lineFromX: xFrom y: yFrom toX: xTo y: yTo ].

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
	fillA = 0.0 ifFalse: [
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
		dx := x-x0. dy := y-y0.
		self cCode: '
			length = sqrt(dx*dx + dy*dy);'
			inSmalltalk: [ length := ((dx*dx) + (dy*dy)) sqrt ].
		correction := hop / length.
		[
			increment := increment / length * hop.
			t := t0 + increment. oneLessT := 1.0 - t.
			f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
			x := (f1 * txFrom) + (f2 * txControl) + (f3 * txTo).
			y := (f1 * tyFrom) + (f2 * tyControl) + (f3 * tyTo).
			dx := x-x0. dy := y-y0.
			self cCode: '
				length = sqrt(dx*dx + dy*dy);'
				inSmalltalk: [ length := ((dx*dx) + (dy*dy)) sqrt ].
			correction := hop / length.
			correction < 1.0]  whileTrue.		   "Keep adjusting if hop would be too big"
		t < 1.0 ]
	whileTrue: [
		self updateAlphasForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y ].

	"Case t= 1.0"
	self updateAlphasForX: txTo y: tyTo.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 7/5/2021 15:59:47'!
quadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array

	self
		primitive: 'primQuadraticBezier'
		parameters: #(Float Float Float Float Float Float WordArray WordArray Float32Array).

	edgeCounts := otherWordArray.
	alphaMask := anotherWordArray.
	contour := aFloat32Array.
	self pvt_quadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl.! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 5/18/2021 10:53:25'!
updateAlphasForX: x y: y

	| t b l r pixelIndex candidateAlpha alphaWord redAlpha greenAlpha blueAlpha doUpdate distanceToAxisSquared dy dySquared dx dxp |
	self var: #x type: 'float'.
	self var: #y type: 'float'.

	self var: #candidateAlpha type: 'uint32_t'.
	self var: #alphaWord type: 'uint32_t'.
	self var: #redAlpha type: 'uint32_t'.
	self var: #greenAlpha type: 'uint32_t'.
	self var: #blueAlpha type: 'uint32_t'.

	self var: #distanceToAxisSquared type: 'float'.
	self var: #dy type: 'float'.
	self var: #dySquared type: 'float'.
	self var: #dx type: 'float'.
	self var: #dxSquared type: 'float'.
	self var: #dxp type: 'float'.

	"Compute affected rect. Honor clipRect"
	t := (y - auxStrokeWidthDilatedHalf + 1) asInteger.														"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	t < clipTop ifTrue: [ t := clipTop ].
	b := (y + auxStrokeWidthDilatedHalf) asInteger.
	b > clipBottom ifTrue: [ b := clipBottom ].
	l := (x - auxStrokeWidthDilatedHalf - subPixelDelta + 1) asInteger.														"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	l < clipLeft ifTrue: [ l := clipLeft ].
	r := (x + auxStrokeWidthDilatedHalf + subPixelDelta) asInteger.
	r > clipRight ifTrue: [ r := clipRight ].

	t to: b do: [ :displayY |
		pixelIndex := displayY * targetWidth + l -1.
		dy := displayY - y.
		dySquared := dy * dy.
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			alphaWord := alphaMask at: pixelIndex.
			alphaWord = 16r007F7F7F ifFalse: [
				redAlpha := alphaWord bitAnd: 16r7F0000.
				greenAlpha := alphaWord bitAnd: 16r7F00.
				blueAlpha := alphaWord bitAnd: 16r7F.
				doUpdate := false.
				dx := displayX - x.
				"Red"
				dxp := dx - subPixelDelta.
				distanceToAxisSquared := dxp * dxp + dySquared.
				distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
					ifTrue: [
						distanceToAxisSquared <= auxStrokeWidthErodedHalfSquared
							ifTrue: [
								candidateAlpha := 16r7F0000 ]
							ifFalse: [
								self cCode: '
										candidateAlpha = (uint32_t)((auxStrokeWidthDilatedHalf - (sqrt(distanceToAxisSquared))) * auxAntiAliasingWidthScaledInverse);
										candidateAlpha = candidateAlpha << 16;'
									inSmalltalk: [
										candidateAlpha := (auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt * auxAntiAliasingWidthScaledInverse) asInteger.
										candidateAlpha := candidateAlpha bitShift: 16 ]].
						candidateAlpha > redAlpha ifTrue: [
							doUpdate := true.
							redAlpha := candidateAlpha ]].
				"Green"
				distanceToAxisSquared := dx * dx + dySquared.
				distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
					ifTrue: [
						distanceToAxisSquared <= auxStrokeWidthErodedHalfSquared
							ifTrue: [
								candidateAlpha := 16r7F00 ]
							ifFalse: [
								self cCode: '
										candidateAlpha = (uint32_t)((auxStrokeWidthDilatedHalf - (sqrt(distanceToAxisSquared))) * auxAntiAliasingWidthScaledInverse);
										candidateAlpha = candidateAlpha << 8;'
									inSmalltalk: [
										candidateAlpha := (auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt * auxAntiAliasingWidthScaledInverse) asInteger.
										candidateAlpha := candidateAlpha bitShift: 8 ]].
						candidateAlpha > greenAlpha ifTrue: [
							doUpdate := true.
							greenAlpha := candidateAlpha ]].
				"Blue"
				dxp := dx + subPixelDelta.
				distanceToAxisSquared := dxp * dxp + dySquared.
				distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
					ifTrue: [
						distanceToAxisSquared <= auxStrokeWidthErodedHalfSquared
							ifTrue: [
								candidateAlpha := 16r7F ]
							ifFalse: [
								self cCode: '
										candidateAlpha = (uint32_t)((auxStrokeWidthDilatedHalf - (sqrt(distanceToAxisSquared))) * auxAntiAliasingWidthScaledInverse);'
									inSmalltalk: [
										candidateAlpha := (auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt * auxAntiAliasingWidthScaledInverse) asInteger ].
								].
						candidateAlpha > blueAlpha ifTrue: [
							doUpdate := true.
							blueAlpha := candidateAlpha ]].
				doUpdate ifTrue: [
					alphaWord := (redAlpha bitOr: greenAlpha) bitOr: blueAlpha.
					alphaMask at: pixelIndex put: alphaWord ]]]]! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 7/5/2021 16:22:38'!
updateContourForX: x y: y
	"The Contour of the current morph is the pixel area affected, 	but without holes.
	For each y line where a pixel is affected, record the leftmost and rightmost affected pixels.
	Note: Only includes pen trajectory, but not stroke width."

	| thisYRounded |

	self var: #x type: 'float'.
	self var: #y type: 'float'.

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

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 8/22/2021 16:09:52'!
updateContourLastLine: aFloat32Array
	self
		primitive: 'primUpdate2ContourLastLine'
		parameters: #(Float32Array).

	contour := aFloat32Array.
	prevYRounded = 16r7FFFFFFF ifFalse: [
		contour at: prevYRounded * 2 put: leftAtThisY.
		contour at: prevYRounded * 2 + 1 put: rightAtThisY ].! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 5/18/2021 10:07:05'!
updateEdgeCountAtX: x y: y
	"Compute edges intersecting with this horizontal line, for fills."

	| thisYTruncated pixelY pixelIndexBase redPixelIndex greenPixelIndex bluePixelIndex redIncrement greenIncrement blueIncrement countWord redCount greenCount blueCount rest redOffset greenOffset blueOffset |

	self var: #x type: 'float'.
	self var: #y type: 'float'.

	self var: #countWord type: 'uint32_t'.
	self var: #redCount type: 'uint32_t'.
	self var: #greenCount type: 'uint32_t'.
	self var: #blueCount type: 'uint32_t'.
	self var: #redIncrement type: 'uint32_t'.
	self var: #greenIncrement type: 'uint32_t'.
	self var: #blueIncrement type: 'uint32_t'.

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
	redOffset := (x+subPixelDelta+1) asInteger max: clipLeft.						"take the next red subpixel center to the right of x"
	greenOffset := (x+1) asInteger max: clipLeft.											"take the next green subpixel center to the right of x"
	blueOffset := (x-subPixelDelta+1) asInteger max: clipLeft.		"take the next blue subpixel center to the right of x"
	redPixelIndex := pixelIndexBase + redOffset.
	greenPixelIndex := pixelIndexBase + greenOffset.
	bluePixelIndex := pixelIndexBase + blueOffset.

	"Three possible cases here: RGB in one word (pixel); RG in one, and G in another; R in one, GB in another"
	redPixelIndex = bluePixelIndex
		ifTrue: [						"First case: RGB in the same word"
			redOffset <= clipRight ifTrue: [
				countWord := edgeCounts at: redPixelIndex.
				redCount := (countWord + redIncrement) bitAnd: 16rFF0000.
				greenCount := (countWord + greenIncrement) bitAnd: 16rFF00.
				blueCount := (countWord + blueIncrement) bitAnd: 16rFF.
				countWord := (redCount bitOr: greenCount) bitOr: blueCount.
				edgeCounts at: redPixelIndex put: countWord ].
		]
		ifFalse: [
			redPixelIndex = greenPixelIndex
				ifTrue: [				"Second case: RG in one word, B in previous"
					redOffset <= clipRight ifTrue: [
						countWord := edgeCounts at: redPixelIndex.
						redCount := (countWord + redIncrement) bitAnd: 16rFF0000.
						greenCount := (countWord + greenIncrement) bitAnd: 16rFF00.
						rest := countWord bitAnd: 16rFF.
						countWord := (redCount bitOr: greenCount) bitOr: rest.
						edgeCounts at: redPixelIndex put: countWord ].

					blueOffset <= clipRight ifTrue: [
						countWord := edgeCounts at: bluePixelIndex.
						rest := countWord bitAnd: 16rFFFF00.
						blueCount := (countWord + blueIncrement) bitAnd: 16rFF.
						countWord := rest bitOr: blueCount.
						edgeCounts at: bluePixelIndex put: countWord ].
				]
				ifFalse: [			"Third case: R in one word, GB in the previous"
					redOffset <= clipRight ifTrue: [
						countWord := edgeCounts at: redPixelIndex.
						redCount := (countWord + redIncrement) bitAnd: 16rFF0000.
						rest := countWord bitAnd: 16rFFFF.
						countWord := redCount bitOr: rest.
						edgeCounts at: redPixelIndex put: countWord ].

					blueOffset <= clipRight ifTrue: [
						countWord := edgeCounts at: bluePixelIndex.
						rest := countWord bitAnd: 16rFF0000.
						greenCount := (countWord + greenIncrement) bitAnd: 16rFF00.
						blueCount := (countWord + blueIncrement) bitAnd: 16rFF.
						countWord := (rest bitOr: greenCount) bitOr: blueCount.
						edgeCounts at: bluePixelIndex put: countWord ].
				]
		].! !

!VectorEnginePlugin methodsFor: 'text and multi paths' stamp: 'dtl 7/27/2021 16:41:33'!
displayString: aString from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes useST80Glyphs: aBoolean edgeCounts: otherWordArray alphaMask: anotherWordArray

	| answer nextGlyphX nextGlyphY ttMoveToX ttMoveToY ttEndX ttEndY ttControlX ttControlY advanceWidth numContours contourStartX contourStartY numBeziers startX startY endX endY controlX controlY dx dy iso8859s15 i xMinEnd xMaxEnd yMinEnd yMaxEnd length t0 x0 y0 t oneLessT increment f1 f2 f3 x y correction |
	self
		primitive: 'primDisplayString'
		parameters: #(String SmallInteger SmallInteger Float Float Float Float Float32Array IntegerArray Boolean WordArray WordArray).

	self var: #answer type: 'double'.
	self var: #nextGlyphX type: 'float'.
	self var: #nextGlyphY type: 'float'.
	self var: #ttMoveToX type: 'float'.
	self var: #ttMoveToY type: 'float'.
	self var: #ttEndX type: 'float'.
	self var: #ttEndY type: 'float'.
	self var: #ttControlX type: 'float'.
	self var: #ttControlY type: 'float'.
	self var: #advanceWidth type: 'float'.
	self var: #contourStartX type: 'float'.
	self var: #contourStartY type: 'float'.
	self var: #startX type: 'float'.
	self var: #startY type: 'float'.
	self var: #endX type: 'float'.
	self var: #endY type: 'float'.
	self var: #controlX type: 'float'.
	self var: #controlY type: 'float'.
	self var: #dx type: 'float'.
	self var: #dy type: 'float'.
	self var: #iso8859s15 type: 'uint8_t'.

	self var: #length type: 'float'.
	self var: #correction type: 'float'.
	self var: #t0 type: 'float'.
	self var: #x0 type: 'float'.
	self var: #y0 type: 'float'.
	self var: #t type: 'float'.
	self var: #oneLessT type: 'float'.
	self var: #increment type: 'float'.
	self var: #f1 type: 'float'.
	self var: #f2 type: 'float'.
	self var: #f3 type: 'float'.
	self var: #x type: 'float'.
	self var: #y type: 'float'.
	self var: #xMinEnd type: 'float'.
	self var: #xMaxEnd type: 'float'.
	self var: #yMinEnd type: 'float'.
	self var: #yMaxEnd type: 'float'.

	edgeCounts := otherWordArray.
	alphaMask := anotherWordArray.
	txA11 := txA11 * sx.
	txA12 := txA12 * sy.
	txA21 := txA21 * sx.
	txA22 := txA22 * sy.

	nextGlyphX := destX / sx.
	nextGlyphY := destY / sy.
	startIndex-1 to: stopIndex-1 do: [ :charIndex |
		self cCode:
				'iso8859s15 = aString[charIndex];'
			inSmalltalk: [
				iso8859s15 := (aString at: charIndex+1) iso8859s15Code. ].
		aBoolean ifTrue: [
			iso8859s15 = 95 ifTrue: [ iso8859s15 := 28 ].												"If underscore, use left arrow"
			iso8859s15 = 94 ifTrue: [ iso8859s15 := 30 ].												"If caret, use up arrow"
			].
		i := contourDataIndexes at: iso8859s15.
		i := i - 1.
		advanceWidth := contourData at: i. i := i + 5.
		numContours := (contourData at: i) asInteger. i := i + 1.
		1 to: numContours do: [ :idx |
			numBeziers := (contourData at: i) asInteger. i := i + 1.
			ttMoveToX := (contourData at: i) + nextGlyphX. i := i + 1.
			ttMoveToY := (contourData at: i) + nextGlyphY. i := i + 1.
			startX := (ttMoveToX * txA11) + (ttMoveToY * txA12) + txA13.
			startY := (ttMoveToX * txA21) + (ttMoveToY * txA22) + txA23.
			contourStartX := startX.
			contourStartY := startY.
			self initializeTrajectoryFragment.
			1 to: numBeziers do: [ :idx2 |
				ttEndX := contourData at: i. i := i + 1.
				ttEndY := contourData at: i. i := i + 1.
				ttControlX := contourData at: i. i := i + 1.
				ttControlY := contourData at: i. i := i + 1.
				endX := (ttEndX * txA11) + (ttEndY * txA12) + startX.
				endY := (ttEndX * txA21) + (ttEndY * txA22) + startY.
				controlX := (ttControlX * txA11) + (ttControlY * txA12) + startX.
				controlY := (ttControlX * txA21) + (ttControlY * txA22) + startY.
				"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
				xMinEnd := startX min: endX.
				xMaxEnd := startX max: endX.
				yMinEnd := startY min: endY.
				yMaxEnd := startY max: endY.
				spanLeft := spanLeft min: (xMinEnd min: (xMinEnd+controlX) / 2.0).
				spanRight := spanRight max: (xMaxEnd max: (xMaxEnd+controlX) / 2.0).
				spanTop := spanTop min: (yMinEnd min: (yMinEnd+controlY) / 2.0).
				spanBottom := spanBottom max: (yMaxEnd max: (yMaxEnd+controlY) / 2.0).
				"Compute Quadratic Bezier Curve,"
				"Case t = 0.0"
				x := startX.
				y := startY.
				self updateAlphasForX: x y: y.
				self updateEdgeCountAtX: x y: y.
				"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
				self cCode: 'dx = fabs(endX-startX); dy = fabs(endY-startY);'
				inSmalltalk: [ dx := (endX-startX) abs. dy := (endY-startY) abs ].
				increment := 0.5 / (dx max: dy) min: 0.5.
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
					self cCode: '
						length = sqrt(dx*dx + dy*dy);'
						inSmalltalk: [ length := ((dx*dx) + (dy*dy)) sqrt ].
					correction := hop / length.
					[
						increment := increment / length * hop.
						t := t0 + increment. oneLessT := 1.0 - t.
						f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
						x := (f1 * startX) + (f2 * controlX) + (f3 * endX).
						y := (f1 * startY) + (f2 * controlY) + (f3 * endY).
						dx := x-x0. dy := y-y0.
						self cCode: '
							length = sqrt(dx*dx + dy*dy);'
							inSmalltalk: [ length := ((dx*dx) + (dy*dy)) sqrt ].
						correction := hop / length.
						correction < 1.0]  whileTrue.		   "Keep adjusting if hop would be too big"
					t < 1.0 ]
				whileTrue: [
					self updateAlphasForX: x y: y.
					self updateEdgeCountAtX: x y: y ].
				"Note: For TrueType font definitions, we assume that all contour fragments start exactly where the previous ends.
				This means that the end point is only added for the last fragment of the contour, and not for each one of them."
				startX := endX.
				startY := endY.	
			].
			self updateAlphasForX: endX y: endY.
			self updateEdgeCountAtX: endX y: endY.
			"Similar effect to ensureClosePath in #finishPath:,
			but assume the TrueType definition is essentially right, and there might only be a rounding error.
			So, don't draw a line, but just (possibly) correct edgeCounts. The possibility of rounding error is most likely zero.
			Anyway, this is cheap."
			self updateEdgeCountAtX: contourStartX y: contourStartY.
		].
		nextGlyphX := nextGlyphX + advanceWidth.
	].
	txA11 := txA11 / sx.
	txA12 := txA12 / sy.
	txA21 := txA21 / sx.
	txA22 := txA22 / sy.
	answer := nextGlyphX * sx.
	^answer asOop: Float! !

!VectorEnginePlugin methodsFor: 'text and multi paths' stamp: 'dtl 7/27/2021 16:41:42'!
displayUtf32: aWordArray from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes useST80Glyphs: aBoolean edgeCounts: otherWordArray alphaMask: anotherWordArray

	| answer nextGlyphX nextGlyphY ttMoveToX ttMoveToY ttEndX ttEndY ttControlX ttControlY advanceWidth numContours contourStartX contourStartY numBeziers startX startY endX endY controlX controlY dx dy utf32 utf8Byte i xMinEnd xMaxEnd yMinEnd yMaxEnd length t0 x0 y0 t oneLessT increment f1 f2 f3 x y correction |
	self
		primitive: 'primDisplayUtf32'
		parameters: #(WordArray SmallInteger SmallInteger Float Float Float Float Float32Array IntegerArray Boolean WordArray WordArray).

	self var: #answer type: 'double'.
	self var: #nextGlyphX type: 'float'.
	self var: #nextGlyphY type: 'float'.
	self var: #ttMoveToX type: 'float'.
	self var: #ttMoveToY type: 'float'.
	self var: #ttEndX type: 'float'.
	self var: #ttEndY type: 'float'.
	self var: #ttControlX type: 'float'.
	self var: #ttControlY type: 'float'.
	self var: #advanceWidth type: 'float'.
	self var: #contourStartX type: 'float'.
	self var: #contourStartY type: 'float'.
	self var: #startX type: 'float'.
	self var: #startY type: 'float'.
	self var: #endX type: 'float'.
	self var: #endY type: 'float'.
	self var: #controlX type: 'float'.
	self var: #controlY type: 'float'.
	self var: #dx type: 'float'.
	self var: #dy type: 'float'.

	self var: #length type: 'float'.
	self var: #correction type: 'float'.
	self var: #t0 type: 'float'.
	self var: #x0 type: 'float'.
	self var: #y0 type: 'float'.
	self var: #t type: 'float'.
	self var: #oneLessT type: 'float'.
	self var: #increment type: 'float'.
	self var: #f1 type: 'float'.
	self var: #f2 type: 'float'.
	self var: #f3 type: 'float'.
	self var: #x type: 'float'.
	self var: #y type: 'float'.
	self var: #xMinEnd type: 'float'.
	self var: #xMaxEnd type: 'float'.
	self var: #yMinEnd type: 'float'.
	self var: #yMaxEnd type: 'float'.

	edgeCounts := otherWordArray.
	alphaMask := anotherWordArray.
	txA11 := txA11 * sx.
	txA12 := txA12 * sy.
	txA21 := txA21 * sx.
	txA22 := txA22 * sy.

	nextGlyphX := destX / sx.
	nextGlyphY := destY / sy.
	startIndex-1 to: stopIndex-1 do: [ :utf32Index |
		utf32 := aWordArray at: utf32Index.
		aBoolean ifTrue: [
			utf32 = 95 ifTrue: [ utf32 := 8592 ].												"If underscore, use left arrow"
			utf32 = 94 ifTrue: [ utf32 := 8593 ].												"If caret, use up arrow"
			].
		utf32 <= 16r7F
			ifTrue: [
				utf8Byte := utf32.
				i := contourDataIndexes at: utf8Byte ]
			ifFalse: [ utf32 <= 16r7FF
			ifTrue: [
				utf8Byte := (utf32 bitShift: -6) bitOr: 2r11000000.
				i := contourDataIndexes at: utf8Byte.
				utf8Byte := (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
				i := contourDataIndexes at: utf8Byte - i ]
			ifFalse: [ utf32 <= 16rFFFF
			ifTrue: [
				utf8Byte := (utf32 bitShift: -12) bitOr: 2r11100000.
				i := contourDataIndexes at: utf8Byte.
				utf8Byte := ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
				i := contourDataIndexes at: utf8Byte - i.
				utf8Byte := (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
				i := contourDataIndexes at: utf8Byte - i ]
			ifFalse: [
				utf8Byte := (utf32 bitShift: -18) bitOr: 2r11110000.
				i := contourDataIndexes at: utf8Byte.
				utf8Byte := ((utf32 bitShift: -12) bitAnd: 2r00111111) bitOr: 2r10000000.
				i := contourDataIndexes at: utf8Byte - i.
				utf8Byte := ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
				i := contourDataIndexes at: utf8Byte - i.
				utf8Byte := (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
				i := contourDataIndexes at: utf8Byte - i ]]].
		i := i - 1.
		advanceWidth := contourData at: i. i := i + 5.
		numContours := (contourData at: i) asInteger. i := i + 1.
		1 to: numContours do: [ :idx |
			numBeziers := (contourData at: i) asInteger. i := i + 1.
			ttMoveToX := (contourData at: i) + nextGlyphX. i := i + 1.
			ttMoveToY := (contourData at: i) + nextGlyphY. i := i + 1.
			startX := (ttMoveToX * txA11) + (ttMoveToY * txA12) + txA13.
			startY := (ttMoveToX * txA21) + (ttMoveToY * txA22) + txA23.
			contourStartX := startX.
			contourStartY := startY.
			self initializeTrajectoryFragment.
			1 to: numBeziers do: [ :idx2 |
				ttEndX := contourData at: i. i := i + 1.
				ttEndY := contourData at: i. i := i + 1.
				ttControlX := contourData at: i. i := i + 1.
				ttControlY := contourData at: i. i := i + 1.
				endX := (ttEndX * txA11) + (ttEndY * txA12) + startX.
				endY := (ttEndX * txA21) + (ttEndY * txA22) + startY.
				controlX := (ttControlX * txA11) + (ttControlY * txA12) + startX.
				controlY := (ttControlX * txA21) + (ttControlY * txA22) + startY.
				"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
				xMinEnd := startX min: endX.
				xMaxEnd := startX max: endX.
				yMinEnd := startY min: endY.
				yMaxEnd := startY max: endY.
				spanLeft := spanLeft min: (xMinEnd min: (xMinEnd+controlX) / 2.0).
				spanRight := spanRight max: (xMaxEnd max: (xMaxEnd+controlX) / 2.0).
				spanTop := spanTop min: (yMinEnd min: (yMinEnd+controlY) / 2.0).
				spanBottom := spanBottom max: (yMaxEnd max: (yMaxEnd+controlY) / 2.0).
				"Compute Quadratic Bezier Curve,"
				"Case t = 0.0"
				x := startX.
				y := startY.
				self updateAlphasForX: x y: y.
				self updateEdgeCountAtX: x y: y.
				"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
				self cCode: 'dx = fabs(endX-startX); dy = fabs(endY-startY);'
				inSmalltalk: [ dx := (endX-startX) abs. dy := (endY-startY) abs ].
				increment := 0.5 / (dx max: dy) min: 0.5.
				t := 0.0.
				[
					t0 := t. x0 := x. y0 := y.
					"Compute next point. Only C version to avoid hitting a limit in Smalltalk compiler."
					self cCode: '
						t = t0 + increment; oneLessT = 1.0 - t;
						f1 = oneLessT * oneLessT; f2 = 2.0 * oneLessT * t; f3 = t * t;
						x = (f1 * startX) + (f2 * controlX) + (f3 * endX);
						y = (f1 * startY) + (f2 * controlY) + (f3 * endY);
						dx = x-x0; dy = y-y0;
						length = sqrt(dx*dx + dy*dy);'
						inSmalltalk: [ length := ((dx*dx) + (dy*dy)) sqrt ].
					correction := hop / length.
					"Now adjust the increment to aim at the required hop length, and recompute next point."
					[
						increment := increment / length * hop.
						t := t0 + increment. oneLessT := 1.0 - t.
						f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
						x := (f1 * startX) + (f2 * controlX) + (f3 * endX).
						y := (f1 * startY) + (f2 * controlY) + (f3 * endY).
						dx := x-x0. dy := y-y0.
						self cCode: '
							length = sqrt(dx*dx + dy*dy);'
							inSmalltalk: [ length := ((dx*dx) + (dy*dy)) sqrt ].
						correction := hop / length.
						correction < 1.0]  whileTrue.		   "Keep adjusting if hop would be too big"
					t < 1.0 ]
				whileTrue: [
					self updateAlphasForX: x y: y.
					self updateEdgeCountAtX: x y: y. ].
				"Note: For TrueType font definitions, we assume that all contour fragments start exactly where the previous ends.
				This means that the end point is only added for the last fragment of the contour, and not for each one of them."
				startX := endX.
				startY := endY.	
			].
			self updateAlphasForX: endX y: endY.
			self updateEdgeCountAtX: endX y: endY.
			"Similar effect to ensureClosePath in #finishPath:,
			but assume the TrueType definition is essentially right, and there might only be a rounding error.
			So, don't draw a line, but just (possibly) correct edgeCounts. The possibility of rounding error is most likely zero.
			Anyway, this is cheap."
			self updateEdgeCountAtX: contourStartX y: contourStartY.
		].
		nextGlyphX := nextGlyphX + advanceWidth.
	].
	txA11 := txA11 / sx.
	txA12 := txA12 / sy.
	txA21 := txA21 / sx.
	txA22 := txA22 / sy.
	answer := nextGlyphX * sx.
	^answer asOop: Float! !

!VectorEnginePlugin methodsFor: 'text and multi paths' stamp: 'dtl 7/27/2021 16:41:51'!
displayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes useST80Glyphs: aBoolean edgeCounts: otherWordArray alphaMask: anotherWordArray

	| answer nextGlyphX nextGlyphY ttMoveToX ttMoveToY ttEndX ttEndY ttControlX ttControlY advanceWidth numContours contourStartX contourStartY numBeziers startX startY endX endY controlX controlY dx dy baseIndex byte i xMinEnd xMaxEnd yMinEnd yMaxEnd length t0 x0 y0 t oneLessT increment f1 f2 f3 x y correction |
	self
		primitive: 'primDisplayUtf8'
		parameters: #(ByteArray SmallInteger SmallInteger Float Float Float Float Float32Array IntegerArray Boolean WordArray WordArray).

	self var: #answer type: 'double'.
	self var: #nextGlyphX type: 'float'.
	self var: #nextGlyphY type: 'float'.
	self var: #ttMoveToX type: 'float'.
	self var: #ttMoveToY type: 'float'.
	self var: #ttEndX type: 'float'.
	self var: #ttEndY type: 'float'.
	self var: #ttControlX type: 'float'.
	self var: #ttControlY type: 'float'.
	self var: #advanceWidth type: 'float'.
	self var: #contourStartX type: 'float'.
	self var: #contourStartY type: 'float'.
	self var: #startX type: 'float'.
	self var: #startY type: 'float'.
	self var: #endX type: 'float'.
	self var: #endY type: 'float'.
	self var: #controlX type: 'float'.
	self var: #controlY type: 'float'.
	self var: #dx type: 'float'.
	self var: #dy type: 'float'.
	self var: #byte type: 'uint8_t'.

	self var: #length type: 'float'.
	self var: #correction type: 'float'.
	self var: #t0 type: 'float'.
	self var: #x0 type: 'float'.
	self var: #y0 type: 'float'.
	self var: #t type: 'float'.
	self var: #oneLessT type: 'float'.
	self var: #increment type: 'float'.
	self var: #f1 type: 'float'.
	self var: #f2 type: 'float'.
	self var: #f3 type: 'float'.
	self var: #x type: 'float'.
	self var: #y type: 'float'.
	self var: #xMinEnd type: 'float'.
	self var: #xMaxEnd type: 'float'.
	self var: #yMinEnd type: 'float'.
	self var: #yMaxEnd type: 'float'.

	edgeCounts := otherWordArray.
	alphaMask := anotherWordArray.
	txA11 := txA11 * sx.
	txA12 := txA12 * sy.
	txA21 := txA21 * sx.
	txA22 := txA22 * sy.

	nextGlyphX := destX / sx.
	nextGlyphY := destY / sy.
	baseIndex := 0.
	byteStartIndex-1 to: byteStopIndex-1 do: [ :byteIndex |
		byte := aByteArray at: byteIndex.
		i := contourDataIndexes at: baseIndex + byte.
		aBoolean ifTrue: [
			byte = 95 ifTrue: [
				i := contourDataIndexes at: 226.
				i := contourDataIndexes at: 134 - i.
				i := contourDataIndexes at: 144 - i ].
			byte = 94 ifTrue: [ 
				i := contourDataIndexes at: 226.
				i := contourDataIndexes at: 134 - i.
				i := contourDataIndexes at: 145 - i ]].
		i < 0
			ifTrue: [ baseIndex := 0 - i ]
			ifFalse: [
				i := i - 1.
				advanceWidth := contourData at: i. i := i + 5.
				numContours := (contourData at: i) asInteger. i := i + 1.
				1 to: numContours do: [ :idx |
					numBeziers := (contourData at: i) asInteger. i := i + 1.
					ttMoveToX := (contourData at: i) + nextGlyphX. i := i + 1.
					ttMoveToY := (contourData at: i) + nextGlyphY. i := i + 1.
					startX := (ttMoveToX * txA11) + (ttMoveToY * txA12) + txA13.
					startY := (ttMoveToX * txA21) + (ttMoveToY * txA22) + txA23.
					contourStartX := startX.
					contourStartY := startY.
					self initializeTrajectoryFragment.
					1 to: numBeziers do: [ :idx2 |
						ttEndX := contourData at: i. i := i + 1.
						ttEndY := contourData at: i. i := i + 1.
						ttControlX := contourData at: i. i := i + 1.
						ttControlY := contourData at: i. i := i + 1.
						endX := (ttEndX * txA11) + (ttEndY * txA12) + startX.
						endY := (ttEndX * txA21) + (ttEndY * txA22) + startY.
						controlX := (ttControlX * txA11) + (ttControlY * txA12) + startX.
						controlY := (ttControlX * txA21) + (ttControlY * txA22) + startY.
						"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
						xMinEnd := startX min: endX.
						xMaxEnd := startX max: endX.
						yMinEnd := startY min: endY.
						yMaxEnd := startY max: endY.
						spanLeft := spanLeft min: (xMinEnd min: (xMinEnd+controlX) / 2.0).
						spanRight := spanRight max: (xMaxEnd max: (xMaxEnd+controlX) / 2.0).
						spanTop := spanTop min: (yMinEnd min: (yMinEnd+controlY) / 2.0).
						spanBottom := spanBottom max: (yMaxEnd max: (yMaxEnd+controlY) / 2.0).
						"Case t = 0.0"
						x := startX.
						y := startY.
						self updateAlphasForX: x y: y.
						self updateEdgeCountAtX: x y: y.
						"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
						self cCode: 'dx = fabs(endX-startX); dy = fabs(endY-startY);'
						inSmalltalk: [ dx := (endX-startX) abs. dy := (endY-startY) abs ].
						increment := 0.5 / (dx max: dy) min: 0.5.
						"Compute Quadratic Bezier Curve,"
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
							self cCode: '
								length = sqrt(dx*dx + dy*dy);'
								inSmalltalk: [ length := ((dx*dx) + (dy*dy)) sqrt ].
							correction := hop / length.
							[
								increment := increment / length * hop.
								t := t0 + increment. oneLessT := 1.0 - t.
								f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
								x := (f1 * startX) + (f2 * controlX) + (f3 * endX).
								y := (f1 * startY) + (f2 * controlY) + (f3 * endY).
								dx := x-x0. dy := y-y0.
								self cCode: '
									length = sqrt(dx*dx + dy*dy);'
									inSmalltalk: [ length := ((dx*dx) + (dy*dy)) sqrt ].
								correction := hop / length.
								correction < 1.0]  whileTrue.		   "Keep adjusting if hop would be too big"
							t < 1.0 ]
						whileTrue: [
							self updateAlphasForX: x y: y.
							self updateEdgeCountAtX: x y: y ].
						"Note: For TrueType font definitions, we assume that all contour fragments start exactly where the previous ends.
						This means that the end point is only added for the last fragment of the contour, and not for each one of them."
						startX := endX.
						startY := endY.	
					].
					self updateAlphasForX: endX y: endY.
					self updateEdgeCountAtX: endX y: endY.
					"Similar effect to ensureClosePath in #finishPath:,
					but assume the TrueType definition is essentially right, and there might only be a rounding error.
					So, don't draw a line, but just (possibly) correct edgeCounts. The possibility of rounding error is most likely zero.
					Anyway, this is cheap."
					self updateEdgeCountAtX: contourStartX y: contourStartY.
				].
				nextGlyphX := nextGlyphX + advanceWidth.
				baseIndex := 0.
			]
	].
	txA11 := txA11 / sx.
	txA12 := txA12 / sy.
	txA21 := txA21 / sx.
	txA22 := txA22 / sy.
	answer := nextGlyphX * sx.
	^answer asOop: Float! !

!VectorEnginePlugin methodsFor: 'text and multi paths' stamp: 'dtl 7/27/2021 16:41:59'!
pathSequence: aFloat32Array size: size edgeCounts: otherWordArray alphaMask: anotherWordArray contour: otherFloat32Array

	| i commandType startX startY endX endY control1X control1Y control2X control2Y |
	self
		primitive: 'primPathSequence'
		parameters: #(Float32Array SmallInteger WordArray WordArray Float32Array).

	self var: #startX type: 'float'.
	self var: #startY type: 'float'.
	self var: #endX type: 'float'.
	self var: #endY type: 'float'.
	self var: #control1X type: 'float'.
	self var: #control1Y type: 'float'.
	self var: #control2X type: 'float'.
	self var: #control2Y type: 'float'.

	edgeCounts := otherWordArray.
	alphaMask := anotherWordArray.
	contour := otherFloat32Array.
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
				self pvt_lineFromX: startX y: startY toX: endX y: endY.
				startX := endX.
				startY := endY.
				].
			[2] -> [
				i+3 < size ifFalse: [ ^self "failure" ].
				endX := aFloat32Array at: i. i := i + 1.
				endY := aFloat32Array at: i. i := i + 1.
				control1X := aFloat32Array at: i. i := i + 1.
				control1Y := aFloat32Array at: i. i := i + 1.
				self pvt_quadraticBezierFromX: startX y: startY
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
				self pvt_cubicBezierFromX: startX y: startY
					toX: endX y: endY
					control1X: control1X y: control1Y
					control2X: control2X y: control2Y.
				startX := endX.
				startY := endY.
				]}
		otherwise: [ ^self "failure"]].! !

!VectorEnginePlugin methodsFor: 'blend stroke and fill colors' stamp: 'jmv 9/30/2021 12:12:09'!
blendFillOnlyAt: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside antiAliasAlphasWord: antiAliasAlphasWord
	"Blends fill color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB resultRBits resultGBits resultBBits resultAlphaBits antiAliasRedAlphaBits antiAliasRedAlpha antiAliasGreenAlphaBits antiAliasGreenAlpha antiAliasBlueAlphaBits antiAliasBlueAlpha alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlphaR resultAlphaG resultAlphaB targetAlpha antiAliasGreenAlphaBitsShifted clippingAntiAliasBits clippingAntiAlias morphIdWord |

	self var: #antiAliasAlphasWord type: 'uint32_t'.
	self var: #antiAliasRedAlphaBits type: 'uint32_t'.
	self var: #antiAliasGreenAlphaBits type: 'uint32_t'.
	self var: #antiAliasBlueAlphaBits type: 'uint32_t'.

	self var: #targetWord type: 'uint32_t'.
	self var: #resultAlphaBits type: 'uint32_t'.
	self var: #resultRBits type: 'uint32_t'.
	self var: #resultGBits type: 'uint32_t'.
	self var: #resultBBits type: 'uint32_t'.

	self var: #morphIdWord type: 'uint32_t'.
	self var: #antiAliasGreenAlphaBitsShifted type: 'uint32_t'.
	self var: #clippingAntiAliasBits type: 'uint32_t'.

	self var: #clippingAntiAlias type: 'float'.
	self var: #resultR type: 'float'.
	self var: #resultG type: 'float'.
	self var: #resultB type: 'float'.
	self var: #antiAliasRedAlpha type: 'float'.
	self var: #antiAliasGreenAlpha type: 'float'.
	self var: #antiAliasBlueAlpha type: 'float'.
	self var: #alphaR type: 'float'.
	self var: #alphaG type: 'float'.
	self var: #alphaB type: 'float'.
	self var: #unAlphaR type: 'float'.
	self var: #unAlphaG type: 'float'.
	self var: #unAlphaB type: 'float'.
	self var: #resultAlphaR type: 'float'.
	self var: #resultAlphaG type: 'float'.
	self var: #resultAlphaB type: 'float'.
	self var: #targetAlpha type: 'float'.

	"In this method, antiAliasAlphas are not used to blend stroke, but fill. This means that in the inside of the shape, and away from the stroke, they must be 1.0 (not 0.0)."
	antiAliasRedAlphaBits := antiAliasAlphasWord bitAnd: 16r7F0000.
	antiAliasGreenAlphaBits := antiAliasAlphasWord bitAnd: 16r7F00.
	antiAliasBlueAlphaBits := antiAliasAlphasWord bitAnd: 16r7F.
	isRedInside ifTrue: [ antiAliasRedAlphaBits := 16r7F0000 - antiAliasRedAlphaBits ].
	isGreenInside ifTrue: [ antiAliasGreenAlphaBits := 16r7F00 - antiAliasGreenAlphaBits ].
	isBlueInside ifTrue: [ antiAliasBlueAlphaBits := 16r7F - antiAliasBlueAlphaBits ].
	antiAliasRedAlpha := antiAliasRedAlphaBits * (1.0 / (127.0 * 256 * 256)).
	antiAliasGreenAlpha := antiAliasGreenAlphaBits * (1.0 / (127.0 * 256)).
	antiAliasBlueAlpha := antiAliasBlueAlphaBits * (1.0 / 127.0).
	alphaR := antiAliasRedAlpha * fillA.
	alphaG := antiAliasGreenAlpha * fillA.
	alphaB := antiAliasBlueAlpha * fillA.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			morphIdWord := morphIds at: pixelIndex.
			clippingAntiAliasBits := morphIdWord bitAnd: 16r7F.
			self cCode: '
					antiAliasGreenAlphaBitsShifted = antiAliasGreenAlphaBits >> 8;'
				inSmalltalk: [
					antiAliasGreenAlphaBitsShifted := antiAliasGreenAlphaBits bitShift: -8 ].
			antiAliasGreenAlphaBitsShifted > clippingAntiAliasBits ifTrue: [
				clippingAntiAliasBits := antiAliasGreenAlphaBitsShifted]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					morphIdWord := morphIds at: pixelIndex.
					clippingAntiAliasBits := morphIdWord bitAnd: 16r7F.
					clippingAntiAlias := clippingAntiAliasBits * (1.0 / 127.0).
					alphaR := alphaR * clippingAntiAlias.
					alphaG := alphaG * clippingAntiAlias.
					alphaB := alphaB * clippingAntiAlias ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAliasBits := 0]].

	alphaR + alphaG + alphaB = 0.0 ifFalse: [
		targetWord := targetBits at: pixelIndex.
		resultAlphaBits := targetWord bitAnd: 16rFF000000.
		resultRBits := targetWord bitAnd: 16rFF0000.
		resultGBits := targetWord bitAnd: 16rFF00.
		resultBBits := targetWord bitAnd: 16rFF.
		targetAlpha := resultAlphaBits * ( 1.0 / (255.0 * 256 * 256 * 256)).

		"These if are not really needed. just ignore them if we use simd instructions."
		alphaR = 0.0 ifFalse: [
			unAlphaR := 1.0 - alphaR.
			resultAlphaR := alphaR + (unAlphaR * targetAlpha).
			self cCode: '
					resultR = (alphaR * fillR) + ((unAlphaR * (resultRBits >> 16)) * targetAlpha);
					resultRBits = (uint32_t)(resultR / resultAlphaR + 0.5) << 16;'
				inSmalltalk: [
					resultR := alphaR * fillR + (unAlphaR * (resultRBits bitShift: -16) * targetAlpha).
					resultRBits := (resultR / resultAlphaR + 0.5) asInteger bitShift: 16 ]].

		alphaG = 0.0 ifFalse: [
			unAlphaG := 1.0 - alphaG.
			resultAlphaG := alphaG + (unAlphaG * targetAlpha).
			self cCode: '
					resultG = (alphaG * fillG) + ((unAlphaG * (resultGBits >> 8)) * targetAlpha);
					resultGBits = (uint32_t)(resultG / resultAlphaG + 0.5) << 8;
					resultAlphaBits = (uint32_t)(resultAlphaG * 255.0 + 0.5) << 24;'
				inSmalltalk: [
					resultG := alphaG * fillG + (unAlphaG * (resultGBits bitShift: -8) * targetAlpha).
					resultGBits := (resultG / resultAlphaG + 0.5) asInteger bitShift: 8.
					resultAlphaBits := (resultAlphaG * 255.0 + 0.5) asInteger bitShift: 24 ]].

		alphaB = 0.0 ifFalse: [
			unAlphaB := 1.0 - alphaB.
			resultAlphaB := alphaB + (unAlphaB * targetAlpha).
			self cCode: '
					resultB = (alphaB * fillB) + ((unAlphaB * resultBBits) * targetAlpha);
					resultBBits = (uint32_t)(resultB / resultAlphaB + 0.5);'
				inSmalltalk: [
					resultB := alphaB * fillB + (unAlphaB * resultBBits * targetAlpha).
					resultBBits := (resultB / resultAlphaB + 0.5) asInteger ]].

		targetWord := ((resultAlphaBits bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		targetBits at: pixelIndex put: targetWord.
		morphIdWord := currentMorphId<<8 + clippingAntiAliasBits.
		morphIds at: pixelIndex put: morphIdWord ].! !

!VectorEnginePlugin methodsFor: 'blend stroke and fill colors' stamp: 'jmv 6/28/2021 17:18:07'!
blendFillOnlyLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherWordArray alphaMask: anotherWordArray
	"Blends fill color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"
	| pixelIndex edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixelWord edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside antiAliasAlphasWord |
	self
		primitive: 'primBlendFillOnly'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger WordArray WordArray WordArray WordArray).

	self var: #edgesThisPixelWord type: 'uint32_t'.
	self var: #edgesThisPixelR type: 'uint8_t'.
	self var: #edgesThisPixelG type: 'uint8_t'.
	self var: #edgesThisPixelB type: 'uint8_t'.
	self var: #edgesUpToThisPixelR type: 'uint8_t'.
	self var: #edgesUpToThisPixelG type: 'uint8_t'.
	self var: #edgesUpToThisPixelB type: 'uint8_t'.
	self var: #antiAliasAlphasWord type: 'uint32_t'.

	targetBits := aBitmap.
	morphIds := aWordArray.
	edgeCounts := otherWordArray.
	alphaMask := anotherWordArray.
	t to: b do: [ :displayY |
		edgesUpToThisPixelR := 0.
		edgesUpToThisPixelG := 0.
		edgesUpToThisPixelB := 0.
		pixelIndex := displayY * targetWidth + l - 1.
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.

			edgesThisPixelWord := edgeCounts at: pixelIndex.
			edgesThisPixelWord = 0 ifFalse: [
				edgeCounts at: pixelIndex put: 0 ].

			self cCode: '
					edgesThisPixelR = (uint32_t) (edgesThisPixelWord & 0xFF0000) >> 16;
					edgesThisPixelG = (uint32_t) (edgesThisPixelWord & 0xFF00) >> 8;
					edgesThisPixelB = (uint32_t) (edgesThisPixelWord & 0xFF);'
				inSmalltalk: [
					edgesThisPixelR := (edgesThisPixelWord bitAnd: 16rFF0000) bitShift: -16.
					edgesThisPixelG := (edgesThisPixelWord bitAnd: 16rFF00)bitShift: -8.
					edgesThisPixelB := edgesThisPixelWord bitAnd: 16rFF.
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
					isBlueInside := edgesUpToThisPixelB ~= 0 ].

			antiAliasAlphasWord := alphaMask at: pixelIndex.
			antiAliasAlphasWord ~= 16r00000000 ifTrue: [
				alphaMask at: pixelIndex put: 0 ].
			(antiAliasAlphasWord ~= 16r00000000 or: [
				isRedInside or: [ isGreenInside or: [ isBlueInside ]]]) ifTrue: [
				self blendFillOnlyAt: pixelIndex
					redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
					antiAliasAlphasWord: antiAliasAlphasWord ]]].! !

!VectorEnginePlugin methodsFor: 'blend stroke and fill colors' stamp: 'jmv 9/30/2021 12:12:34'!
blendStrokeAndFillAt: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside antiAliasAlphasWord: antiAliasAlphasWord
	"Blends stroke color and fill color over background.
	Do an appropriate (anti aliased) gradient between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB resultRBits resultGBits resultBBits resultAlphaBits antiAliasRedAlphaBits antiAliasRedAlpha antiAliasGreenAlphaBits antiAliasGreenAlpha antiAliasBlueAlphaBits antiAliasBlueAlpha foreR foreG foreB alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlphaR resultAlphaG resultAlphaB targetAlpha antiAliasGreenAlphaBitsShifted clippingAntiAliasBits clippingAntiAlias morphIdWord |

	self var: #antiAliasAlphasWord type: 'uint32_t'.
	self var: #antiAliasRedAlphaBits type: 'uint32_t'.
	self var: #antiAliasGreenAlphaBits type: 'uint32_t'.
	self var: #antiAliasBlueAlphaBits type: 'uint32_t'.

	self var: #targetWord type: 'uint32_t'.
	self var: #resultAlphaBits type: 'uint32_t'.
	self var: #resultRBits type: 'uint32_t'.
	self var: #resultGBits type: 'uint32_t'.
	self var: #resultBBits type: 'uint32_t'.

	self var: #antiAliasGreenAlphaBitsShifted type: 'uint32_t'.
	self var: #morphIdWord type: 'uint32_t'.
	self var: #clippingAntiAliasBits type: 'uint32_t'.

	self var: #clippingAntiAlias type: 'float'.
	self var: #resultR type: 'float'.
	self var: #resultG type: 'float'.
	self var: #resultB type: 'float'.
	self var: #antiAliasRedAlpha type: 'float'.
	self var: #antiAliasGreenAlpha type: 'float'.
	self var: #antiAliasBlueAlpha type: 'float'.
	self var: #foreR type: 'float'.
	self var: #foreG type: 'float'.
	self var: #foreB type: 'float'.
	self var: #alphaR type: 'float'.
	self var: #alphaG type: 'float'.
	self var: #alphaB type: 'float'.
	self var: #unAlphaR type: 'float'.
	self var: #unAlphaG type: 'float'.
	self var: #unAlphaB type: 'float'.
	self var: #resultAlphaR type: 'float'.
	self var: #resultAlphaG type: 'float'.
	self var: #resultAlphaB type: 'float'.
	self var: #targetAlpha type: 'float'.

	antiAliasRedAlphaBits := antiAliasAlphasWord bitAnd: 16r7F0000.
	antiAliasGreenAlphaBits := antiAliasAlphasWord bitAnd: 16r7F00.
	antiAliasBlueAlphaBits := antiAliasAlphasWord bitAnd: 16r7F.
	antiAliasRedAlpha := antiAliasRedAlphaBits * (1.0 / (127.0 * 256 * 256)).
	antiAliasGreenAlpha := antiAliasGreenAlphaBits * (1.0 / (127.0 * 256)).
	antiAliasBlueAlpha := antiAliasBlueAlphaBits * (1.0 / 127.0).

	isRedInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaR := antiAliasRedAlpha * strokeA + ((1.0-antiAliasRedAlpha) * fillA).
			foreR := antiAliasRedAlpha * strokeR + ((1.0-antiAliasRedAlpha) * fillR) ]
		ifFalse: [							"Blend stroke over background"
			alphaR := antiAliasRedAlpha * strokeA.
			foreR := strokeR ].

	isGreenInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaG := antiAliasGreenAlpha * strokeA + ((1.0-antiAliasGreenAlpha) * fillA).
			foreG := antiAliasGreenAlpha * strokeG + ((1.0-antiAliasGreenAlpha) * fillG) ]
		ifFalse: [							"Blend stroke over background"
			alphaG := antiAliasGreenAlpha * strokeA.
			foreG := strokeG ].

	isBlueInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaB := antiAliasBlueAlpha * strokeA + ((1.0-antiAliasBlueAlpha) * fillA).
			foreB := antiAliasBlueAlpha * strokeB + ((1.0-antiAliasBlueAlpha) * fillB) ]
		ifFalse: [							"Blend stroke over background"
			alphaB := antiAliasBlueAlpha * strokeA.
			foreB := strokeB ].

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			isGreenInside
				ifTrue: [clippingAntiAliasBits := 16r7F]
				ifFalse: [
					morphIdWord := morphIds at: pixelIndex.
					clippingAntiAliasBits := morphIdWord bitAnd: 16r7F.
					self cCode: '
							antiAliasGreenAlphaBitsShifted = antiAliasGreenAlphaBits >> 8;'
						inSmalltalk: [
							antiAliasGreenAlphaBitsShifted := antiAliasGreenAlphaBits bitShift: -8 ].
					antiAliasGreenAlphaBitsShifted > clippingAntiAliasBits ifTrue: [
						clippingAntiAliasBits := antiAliasGreenAlphaBitsShifted ]]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					morphIdWord := morphIds at: pixelIndex.
					clippingAntiAliasBits := morphIdWord bitAnd: 16r7F.
					clippingAntiAlias := clippingAntiAliasBits * (1.0 / 127.0).
					alphaR := alphaR * clippingAntiAlias.
					alphaG := alphaG * clippingAntiAlias.
					alphaB := alphaB * clippingAntiAlias ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAliasBits := 0]].

	alphaR + alphaG + alphaB = 0.0 ifFalse: [
		targetWord := targetBits at: pixelIndex.
		resultAlphaBits := targetWord bitAnd: 16rFF000000.
		resultRBits := targetWord bitAnd: 16rFF0000.
		resultGBits := targetWord bitAnd: 16rFF00.
		resultBBits := targetWord bitAnd: 16rFF.
		targetAlpha := resultAlphaBits * ( 1.0 / (255.0 * 256 * 256 * 256)).

		"These if are not really needed. just ignore them if we use simd instructions."
		alphaR = 0.0 ifFalse: [
			unAlphaR := 1.0 - alphaR.
			resultAlphaR := alphaR + (unAlphaR * targetAlpha).
			self cCode: '
					resultR = (alphaR * foreR) + ((unAlphaR * (resultRBits >> 16)) * targetAlpha);
					resultRBits = (uint32_t)(resultR / resultAlphaR + 0.5) << 16;'
				inSmalltalk: [
					resultR := alphaR * foreR + (unAlphaR * (resultRBits bitShift: -16) * targetAlpha).
					resultRBits := (resultR / resultAlphaR + 0.5) asInteger bitShift: 16 ]].

		alphaG = 0.0 ifFalse: [
			unAlphaG := 1.0 - alphaG.
			resultAlphaG := alphaG + (unAlphaG * targetAlpha).
			self cCode: '
					resultG = (alphaG * foreG) + ((unAlphaG * (resultGBits >> 8)) * targetAlpha);
					resultGBits = (uint32_t)(resultG / resultAlphaG + 0.5) << 8;
					resultAlphaBits = (uint32_t)(resultAlphaG * 255.0 + 0.5) << 24;'
				inSmalltalk: [
					resultG := alphaG * foreG + (unAlphaG * (resultGBits bitShift: -8) * targetAlpha).
					resultGBits := (resultG / resultAlphaG + 0.5) asInteger bitShift: 8.
					resultAlphaBits := (resultAlphaG * 255.0 + 0.5) asInteger bitShift: 24 ]].

		alphaB = 0.0 ifFalse: [
			unAlphaB := 1.0 - alphaB.
			resultAlphaB := alphaB + (unAlphaB * targetAlpha).
			self cCode: '
					resultB = (alphaB * foreB) + ((unAlphaB * resultBBits) * targetAlpha);
					resultBBits = (uint32_t)(resultB / resultAlphaB + 0.5);'
				inSmalltalk: [
					resultB := alphaB * foreB + (unAlphaB * resultBBits * targetAlpha).
					resultBBits := (resultB / resultAlphaB + 0.5) asInteger ]].

		targetWord := ((resultAlphaBits bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		targetBits at: pixelIndex put: targetWord.
		morphIdWord := currentMorphId<<8 + clippingAntiAliasBits.
		morphIds at: pixelIndex put: morphIdWord ].! !

!VectorEnginePlugin methodsFor: 'blend stroke and fill colors' stamp: 'jmv 6/28/2021 17:19:45'!
blendStrokeAndFillLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherWordArray alphaMask: anotherWordArray
	"Blends stroke color and fill color over background.
	Do an appropriate (anti aliased) gradient between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"
	| pixelIndex edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixelWord edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside antiAliasAlphasWord |
	self
		primitive: 'primBlendStrokeAndFill'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger WordArray WordArray WordArray WordArray).

	self var: #edgesThisPixelWord type: 'uint32_t'.
	self var: #edgesThisPixelR type: 'uint8_t'.
	self var: #edgesThisPixelG type: 'uint8_t'.
	self var: #edgesThisPixelB type: 'uint8_t'.
	self var: #edgesUpToThisPixelR type: 'uint8_t'.
	self var: #edgesUpToThisPixelG type: 'uint8_t'.
	self var: #edgesUpToThisPixelB type: 'uint8_t'.
	self var: #antiAliasAlphasWord type: 'uint32_t'.

	targetBits := aBitmap.
	morphIds := aWordArray.
	edgeCounts := otherWordArray.
	alphaMask := anotherWordArray.
	t to: b do: [ :displayY |
		edgesUpToThisPixelR := 0.
		edgesUpToThisPixelG := 0.
		edgesUpToThisPixelB := 0.
		pixelIndex := displayY * targetWidth + l - 1.
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.

			edgesThisPixelWord := edgeCounts at: pixelIndex.
			edgesThisPixelWord = 0 ifFalse: [
				edgeCounts at: pixelIndex put: 0 ].

			self cCode: '
					edgesThisPixelR = (uint32_t) (edgesThisPixelWord & 0xFF0000) >> 16;
					edgesThisPixelG = (uint32_t) (edgesThisPixelWord & 0xFF00) >> 8;
					edgesThisPixelB = (uint32_t) (edgesThisPixelWord & 0xFF);'
				inSmalltalk: [
					edgesThisPixelR := (edgesThisPixelWord bitAnd: 16rFF0000) bitShift: -16.
					edgesThisPixelG := (edgesThisPixelWord bitAnd: 16rFF00)bitShift: -8.
					edgesThisPixelB := edgesThisPixelWord bitAnd: 16rFF.
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
					isBlueInside := edgesUpToThisPixelB ~= 0 ].


			antiAliasAlphasWord := alphaMask at: pixelIndex.
			antiAliasAlphasWord ~= 16r00000000 ifTrue: [
				alphaMask at: pixelIndex put: 0 ].
			(antiAliasAlphasWord ~= 16r00000000 or: [
				isRedInside or: [ isGreenInside or: [ isBlueInside ]]]) ifTrue: [
				self blendStrokeAndFillAt: pixelIndex
					redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
					antiAliasAlphasWord: antiAliasAlphasWord ]]].! !

!VectorEnginePlugin methodsFor: 'blend stroke and fill colors' stamp: 'jmv 9/30/2021 12:13:12'!
blendStrokeOnlyAt: pixelIndex antiAliasAlphasWord: antiAliasAlphasWord
	"Blends stroke color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB resultRBits resultGBits resultBBits resultAlphaBits antiAliasRedAlphaBits antiAliasRedAlpha antiAliasGreenAlphaBits antiAliasGreenAlpha antiAliasBlueAlphaBits antiAliasBlueAlpha alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlphaR resultAlphaG resultAlphaB targetAlpha antiAliasGreenAlphaBitsShifted clippingAntiAliasBits clippingAntiAlias morphIdWord |

	self var: #antiAliasAlphasWord type: 'uint32_t'.
	self var: #antiAliasRedAlphaBits type: 'uint32_t'.
	self var: #antiAliasGreenAlphaBits type: 'uint32_t'.
	self var: #antiAliasBlueAlphaBits type: 'uint32_t'.

	self var: #targetWord type: 'uint32_t'.
	self var: #resultAlphaBits type: 'uint32_t'.
	self var: #resultRBits type: 'uint32_t'.
	self var: #resultGBits type: 'uint32_t'.
	self var: #resultBBits type: 'uint32_t'.

	self var: #morphIdWord type: 'uint32_t'.
	self var: #antiAliasGreenAlphaBitsShifted type: 'uint32_t'.
	self var: #clippingAntiAliasBits type: 'uint32_t'.

	self var: #clippingAntiAlias type: 'float'.
	self var: #resultR type: 'float'.
	self var: #resultG type: 'float'.
	self var: #resultB type: 'float'.
	self var: #antiAliasRedAlpha type: 'float'.
	self var: #antiAliasGreenAlpha type: 'float'.
	self var: #antiAliasBlueAlpha type: 'float'.
	self var: #alphaR type: 'float'.
	self var: #alphaG type: 'float'.
	self var: #alphaB type: 'float'.
	self var: #unAlphaR type: 'float'.
	self var: #unAlphaG type: 'float'.
	self var: #unAlphaB type: 'float'.
	self var: #resultAlphaR type: 'float'.
	self var: #resultAlphaG type: 'float'.
	self var: #resultAlphaB type: 'float'.
	self var: #targetAlpha type: 'float'.

	antiAliasRedAlphaBits := antiAliasAlphasWord bitAnd: 16r7F0000.
	antiAliasGreenAlphaBits := antiAliasAlphasWord bitAnd: 16r7F00.
	antiAliasBlueAlphaBits := antiAliasAlphasWord bitAnd: 16r7F.
	antiAliasRedAlpha := antiAliasRedAlphaBits * (1.0 / (127.0 * 256 * 256)).
	antiAliasGreenAlpha := antiAliasGreenAlphaBits * (1.0 / (127.0 * 256)).
	antiAliasBlueAlpha := antiAliasBlueAlphaBits * (1.0 / 127.0).
	alphaR := antiAliasRedAlpha * strokeA.
	alphaG := antiAliasGreenAlpha * strokeA.
	alphaB := antiAliasBlueAlpha * strokeA.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			morphIdWord := morphIds at: pixelIndex.
			clippingAntiAliasBits := morphIdWord bitAnd: 16r7F.
			self cCode: '
					antiAliasGreenAlphaBitsShifted = antiAliasGreenAlphaBits >> 8;'
				inSmalltalk: [
					antiAliasGreenAlphaBitsShifted := antiAliasGreenAlphaBits bitShift: -8 ].
			antiAliasGreenAlphaBitsShifted > clippingAntiAliasBits ifTrue: [
				clippingAntiAliasBits := antiAliasGreenAlphaBitsShifted]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					morphIdWord := morphIds at: pixelIndex.
					clippingAntiAliasBits := morphIdWord bitAnd: 16r7F.
					clippingAntiAlias := clippingAntiAliasBits * (1.0 / 127.0).
					alphaR := alphaR * clippingAntiAlias.
					alphaG := alphaG * clippingAntiAlias.
					alphaB := alphaB * clippingAntiAlias ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAliasBits := 0]].

	alphaR + alphaG + alphaB = 0.0 ifFalse: [
		targetWord := targetBits at: pixelIndex.
		resultAlphaBits := targetWord bitAnd: 16rFF000000.
		resultRBits := targetWord bitAnd: 16rFF0000.
		resultGBits := targetWord bitAnd: 16rFF00.
		resultBBits := targetWord bitAnd: 16rFF.
		targetAlpha := resultAlphaBits * ( 1.0 / (255.0 * 256 * 256 * 256)).

		"These if are not really needed. just ignore them if we use simd instructions."
		alphaR = 0.0 ifFalse: [
			unAlphaR := 1.0 - alphaR.
			resultAlphaR := alphaR + (unAlphaR * targetAlpha).
			self cCode: '
					resultR = (alphaR * strokeR) + ((unAlphaR * (resultRBits >> 16)) * targetAlpha);
					resultRBits = (uint32_t)(resultR / resultAlphaR + 0.5) << 16;'
				inSmalltalk: [
					resultR := alphaR * strokeR + (unAlphaR * (resultRBits bitShift: -16) * targetAlpha).
					resultRBits := (resultR / resultAlphaR + 0.5) asInteger bitShift: 16 ]].

		alphaG = 0.0 ifFalse: [
			unAlphaG := 1.0 - alphaG.
			resultAlphaG := alphaG + (unAlphaG * targetAlpha).
			self cCode: '
					resultG = (alphaG * strokeG) + ((unAlphaG * (resultGBits >> 8)) * targetAlpha);
					resultGBits = (uint32_t)(resultG / resultAlphaG + 0.5) << 8;
					resultAlphaBits = (uint32_t)(resultAlphaG * 255.0 + 0.5) << 24;'
				inSmalltalk: [
					resultG := alphaG * strokeG + (unAlphaG * (resultGBits bitShift: -8) * targetAlpha).
					resultGBits := (resultG / resultAlphaG + 0.5) asInteger bitShift: 8.
					resultAlphaBits := (resultAlphaG * 255.0 + 0.5) asInteger bitShift: 24 ]].

		alphaB = 0.0 ifFalse: [
			unAlphaB := 1.0 - alphaB.
			resultAlphaB := alphaB + (unAlphaB * targetAlpha).
			self cCode: '
					resultB = (alphaB * strokeB) + ((unAlphaB * resultBBits) * targetAlpha);
					resultBBits = (uint32_t)(resultB / resultAlphaB + 0.5);'
				inSmalltalk: [
					resultB := alphaB * strokeB + (unAlphaB * resultBBits * targetAlpha).
					resultBBits := (resultB / resultAlphaB + 0.5) asInteger ]].

		targetWord := ((resultAlphaBits bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		targetBits at: pixelIndex put: targetWord.
		morphIdWord := currentMorphId<<8 + clippingAntiAliasBits.
		morphIds at: pixelIndex put: morphIdWord ].! !

!VectorEnginePlugin methodsFor: 'blend stroke and fill colors' stamp: 'jmv 6/28/2021 17:18:34'!
blendStrokeOnlyLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray alphaMask: anotherWordArray
	"Blends stroke color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"
	| pixelIndex antiAliasAlphasWord |
	self
		primitive: 'primBlendStrokeOnly'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger WordArray WordArray WordArray).

	self var: #antiAliasAlphasWord type: 'uint32_t'.

	targetBits := aBitmap.
	morphIds := aWordArray.
	alphaMask := anotherWordArray.
	t to: b do: [ :displayY |
		pixelIndex := displayY * targetWidth + l - 1.
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			antiAliasAlphasWord := alphaMask at: pixelIndex.
			antiAliasAlphasWord = 16r00000000 ifFalse: [
				alphaMask at: pixelIndex put: 0.
				self blendStrokeOnlyAt: pixelIndex antiAliasAlphasWord: antiAliasAlphasWord ]]].! !

!VectorEnginePlugin methodsFor: 'whole pixel - path algorithms' stamp: 'jmv 9/15/2021 15:09:50'!
arcWPCenterX: centerX centerY: centerY radiusX: radiusPointX radiusY: radiusPointY start: startAngle sweep: sweepAngle rotationCos: tthetaCos rotationSin: tthetaSin edgeCounts: otherByteArray alphaMask: anotherByteArray contour: aFloat32Array

	| tcx tcy trx try scale hops d angle xp yp x y |
	self
		primitive: 'primArcWP'
		parameters: #(Float Float Float Float Float Float Float Float ByteArray ByteArray Float32Array).

	self var: #tcx type: 'float'.
	self var: #tcy type: 'float'.
	self var: #trx type: 'float'.
	self var: #try type: 'float'.
	self var: #scale type: 'float'.
	self var: #angle type: 'float'.
	self var: #xp type: 'float'.
	self var: #yp type: 'float'.
	self var: #x type: 'float'.
	self var: #y type: 'float'.
	self var: #hops type: 'int'.
	self var: #d type: 'float'.

	tcx := (centerX * txA11) + (centerY * txA12) + txA13.
	tcy := (centerX * txA21) + (centerY * txA22) + txA23.
	self cCode: 'scale = sqrt(txA11*txA11 + txA21*txA21);'
		inSmalltalk: [scale := (txA11 squared + txA21 squared) sqrt].
	trx := radiusPointX * scale.
	try := radiusPointY * scale.
	hops := ((trx max: try) * (self cCode: 'fabs(sweepAngle)' inSmalltalk: [sweepAngle abs]) / hop) asInteger + 2.
	d := hops.

	edgeCountsWP := otherByteArray.
	alphaMaskWP := anotherByteArray.
	contour := aFloat32Array.
	0 to: hops do: [ :h |
		self cCode: 'angle = ((float)h / d) * sweepAngle + startAngle;'
			inSmalltalk: [angle := h / d * sweepAngle + startAngle ].
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

!VectorEnginePlugin methodsFor: 'whole pixel - path algorithms' stamp: 'jmv 7/5/2021 15:54:18'!
cubicBezierWPFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2 edgeCounts: otherByteArray alphaMask: anotherByteArray contour: aFloat32Array

	self
		primitive: 'primCubicBezierWP'
		parameters: #(Float Float Float Float Float Float Float Float ByteArray ByteArray Float32Array).

	edgeCountsWP := otherByteArray.
	alphaMaskWP := anotherByteArray.
	contour := aFloat32Array.

	self pvt_cubicBezierWPFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2! !

!VectorEnginePlugin methodsFor: 'whole pixel - path algorithms' stamp: 'jmv 7/5/2021 15:57:23'!
lineWPFromX: xFrom y: yFrom toX: xTo y: yTo edgeCounts: otherByteArray alphaMask: anotherByteArray contour: aFloat32Array

	self
		primitive: 'primLineWP'
		parameters: #(Float Float Float Float ByteArray ByteArray Float32Array).

	edgeCountsWP := otherByteArray.
	alphaMaskWP := anotherByteArray.
	contour := aFloat32Array.
	self pvt_lineWPFromX: xFrom y: yFrom toX: xTo y: yTo.! !

!VectorEnginePlugin methodsFor: 'whole pixel - path algorithms' stamp: 'jmv 10/12/2021 09:57:51'!
pvt_cubicBezierWPFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2

	| txFrom tyFrom txTo tyTo txControl1 tyControl1 txControl2 tyControl2 dx dy dx2 dy2 dx3 dy3 hops xMinEnd xMaxEnd yMinEnd yMaxEnd t oneLessT increment f1 f23 f2 f3 f4 x y |
	self var: #xFrom type: 'float'.
	self var: #yFrom type: 'float'.
	self var: #xTo type: 'float'.
	self var: #yTo type: 'float'.
	self var: #xControl1 type: 'float'.
	self var: #yControl1 type: 'float'.
	self var: #xControl2 type: 'float'.
	self var: #yControl2 type: 'float'.
	self var: #txFrom type: 'float'.
	self var: #tyFrom type: 'float'.
	self var: #txTo type: 'float'.
	self var: #tyTo type: 'float'.
	self var: #txControl1 type: 'float'.
	self var: #tyControl1 type: 'float'.
	self var: #txControl2 type: 'float'.
	self var: #tyControl2 type: 'float'.
	self var: #dx type: 'float'.
	self var: #dy type: 'float'.
	self var: #dx2 type: 'float'.
	self var: #dy2 type: 'float'.
	self var: #dx3 type: 'float'.
	self var: #dy3 type: 'float'.
	self var: #hops type: 'int'.

	self var: #t type: 'float'.
	self var: #oneLessT type: 'float'.
	self var: #increment type: 'float'.
	self var: #f1 type: 'float'.
	self var: #f23 type: 'float'.
	self var: #f2 type: 'float'.
	self var: #f3 type: 'float'.
	self var: #f4 type: 'float'.
	self var: #x type: 'float'.
	self var: #y type: 'float'.
	self var: #xMinEnd type: 'float'.
	self var: #xMaxEnd type: 'float'.
	self var: #yMinEnd type: 'float'.
	self var: #yMaxEnd type: 'float'.

	txFrom := (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom := (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo := (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo := (xTo * txA21) + (yTo * txA22) + txA23.
	txControl1 := (xControl1 * txA11) + (yControl1 * txA12) + txA13.
	tyControl1 := (xControl1 * txA21) + (yControl1 * txA22) + txA23.
	txControl2 := (xControl2 * txA11) + (yControl2 * txA12) + txA13.
	tyControl2 := (xControl2 * txA21) + (yControl2 * txA22) + txA23.
	self cCode: '
		dx = fabs(txControl1-txFrom);
		dx2 = fabs(txTo-txControl2);
		dx3 = fabs(txControl2-txControl1);
		dy = fabs(tyControl1-tyFrom);
		dy2 = fabs(tyTo-tyControl2);
		dy3 = fabs(tyControl2-tyControl1);'
	inSmalltalk: [
		dx := (txControl1-txFrom) abs.
		dx2 := (txTo-txControl2) abs.
		dx3 := (txControl2-txControl1) abs.
		dy := (tyControl1-tyFrom) abs.
		dy2 := (tyTo-tyControl2) abs.
		dy3 := (tyControl2-tyControl1) abs ].
	dx := (dx max: dx2) * 3 max: dx3 * 1.5.
	dy := (dy max: dy2) * 3 max: dy3 * 1.5.
	hops := ((dx max: dy) / hop) asInteger + 1.

	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
	xMinEnd := txFrom min: txTo.
	xMaxEnd := txFrom max: txTo.
	yMinEnd := tyFrom min: tyTo.
	yMaxEnd := tyFrom max: tyTo.
	spanLeft := spanLeft min: (xMinEnd min: xMinEnd*0.25 + ((txControl1 min: txControl2)*0.75)).
	spanRight := spanRight max: (xMaxEnd max: xMaxEnd*0.25 + ((txControl1 max: txControl2)*0.75)).
	spanTop := spanTop min: (yMinEnd min: yMinEnd*0.25 + ((tyControl1 min: tyControl2)*0.75)).
	spanBottom := spanBottom max: (yMaxEnd max: yMaxEnd*0.25 + ((tyControl1 max: tyControl2)*0.75)).

	t := 0.0.
	increment := 1.0 / hops.
	[ t < 1.0 ] whileTrue: [
		oneLessT := 1.0 - t.
		f1 := oneLessT * oneLessT * oneLessT.
		f23 := 3.0 * oneLessT * t.
		f2 := f23 * oneLessT.
		f3 := f23 * t.
		f4 := t * t * t.
		x := (f1 * txFrom) + (f2 * txControl1) + (f3 * txControl2) + (f4 * txTo).
		y := (f1 * tyFrom) + (f2 * tyControl1) + (f3 * tyControl2) + (f4 * tyTo).
		self updateAlphasWPForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountWPAtX: x y: y ].
		self updateContourForX: x y: y.
		t := t + increment. ].
	self updateAlphasWPForX: txTo y: tyTo.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountWPAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEnginePlugin methodsFor: 'whole pixel - path algorithms' stamp: 'jmv 10/12/2021 10:15:26'!
pvt_lineWPFromX: xFrom y: yFrom toX: xTo y: yTo

	| txFrom tyFrom txTo tyTo dx dy hops t oneLessT increment x y |
	self var: #xFrom type: 'float'.
	self var: #yFrom type: 'float'.
	self var: #xTo type: 'float'.
	self var: #yTo type: 'float'.
	self var: #txFrom type: 'float'.
	self var: #tyFrom type: 'float'.
	self var: #txTo type: 'float'.
	self var: #tyTo type: 'float'.
	self var: #dx type: 'float'.
	self var: #dy type: 'float'.
	self var: #hops type: 'int'.

	self var: #t type: 'float'.
	self var: #oneLessT type: 'float'.
	self var: #increment type: 'float'.
	self var: #x type: 'float'.
	self var: #y type: 'float'.

	txFrom := (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom := (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo := (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo := (xTo * txA21) + (yTo * txA22) + txA23.
	dx := txTo-txFrom.
	dy := tyTo-tyFrom.
	self cCode: '
		dx = fabs(dx);
		dy = fabs(dy);'
	inSmalltalk: [
		dx := dx abs.
		dy := dy abs ].
	hops := ((dx max: dy) / hop) asInteger + 1.

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
		self updateAlphasWPForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountWPAtX: x y: y ].
		self updateContourForX: x y: y.
		t := t + increment. ].
	self updateAlphasWPForX: txTo y: tyTo.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountWPAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEnginePlugin methodsFor: 'whole pixel - path algorithms' stamp: 'jmv 7/22/2021 23:24:13'!
pvt_quadraticBezierWPFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl

	| txFrom tyFrom txTo tyTo txControl tyControl dx dy dx2 dy2 length xMinEnd xMaxEnd yMinEnd yMaxEnd t0 x0 y0 t oneLessT increment f1 f2 f3 x y correction |
	self var: #xFrom type: 'float'.
	self var: #yFrom type: 'float'.
	self var: #xTo type: 'float'.
	self var: #yTo type: 'float'.
	self var: #xControl type: 'float'.
	self var: #yControl type: 'float'.
	self var: #txFrom type: 'float'.
	self var: #tyFrom type: 'float'.
	self var: #txTo type: 'float'.
	self var: #tyTo type: 'float'.
	self var: #txControl type: 'float'.
	self var: #tyControl type: 'float'.
	self var: #dx type: 'float'.
	self var: #dy type: 'float'.
	self var: #dx2 type: 'float'.
	self var: #dy2 type: 'float'.
	self var: #length type: 'float'.
	self var: #correction type: 'float'.

	self var: #t0 type: 'float'.
	self var: #x0 type: 'float'.
	self var: #y0 type: 'float'.
	self var: #t type: 'float'.
	self var: #oneLessT type: 'float'.
	self var: #increment type: 'float'.
	self var: #f1 type: 'float'.
	self var: #f2 type: 'float'.
	self var: #f3 type: 'float'.
	self var: #x type: 'float'.
	self var: #y type: 'float'.
	self var: #xMinEnd type: 'float'.
	self var: #xMaxEnd type: 'float'.
	self var: #yMinEnd type: 'float'.
	self var: #yMaxEnd type: 'float'.

	"If control point is bogus, just draw a line"
	(xControl = xTo and: [ yControl = yTo ]) ifTrue: [
		^self pvt_lineWPFromX: xFrom y: yFrom toX: xTo y: yTo ].
	(xControl = xFrom and: [ yControl = yFrom ]) ifTrue: [
		^self pvt_lineWPFromX: xFrom y: yFrom toX: xTo y: yTo ].

	txFrom := (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom := (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo := (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo := (xTo * txA21) + (yTo * txA22) + txA23.
	txControl := (xControl * txA11) + (yControl * txA12) + txA13.
	tyControl := (xControl * txA21) + (yControl * txA22) + txA23.
	self cCode: '
		dx = fabs(txTo-txFrom);
		dx2 = fabs(txControl-txFrom);
		dy = fabs(tyTo-tyFrom);
		dy2 = fabs(tyControl-tyFrom);'
	inSmalltalk: [
		dx := (txTo-txFrom) abs.
		dx2 := (txControl-txFrom) abs.
		dy := (tyTo-tyFrom) abs.
		dy2 := (tyControl-tyFrom) abs.].

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
		dx := x-x0. dy := y-y0.
		self cCode: '
			length = sqrt(dx*dx + dy*dy);'
			inSmalltalk: [ length := ((dx*dx) + (dy*dy)) sqrt ].
		correction := hop / length.
		[
			increment := increment / length * hop.
			t := t0 + increment. oneLessT := 1.0 - t.
			f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
			x := (f1 * txFrom) + (f2 * txControl) + (f3 * txTo).
			y := (f1 * tyFrom) + (f2 * tyControl) + (f3 * tyTo).
			dx := x-x0. dy := y-y0.
			self cCode: '
				length = sqrt(dx*dx + dy*dy);'
				inSmalltalk: [ length := ((dx*dx) + (dy*dy)) sqrt ].
			correction := hop / length.
			correction < 1.0]  whileTrue.		   "Keep adjusting if hop would be too big"
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

!VectorEnginePlugin methodsFor: 'whole pixel - path algorithms' stamp: 'jmv 7/5/2021 15:59:48'!
quadraticBezierWPFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl edgeCounts: otherByteArray alphaMask: anotherByteArray contour: aFloat32Array

	self
		primitive: 'primQuadraticBezierWP'
		parameters: #(Float Float Float Float Float Float ByteArray ByteArray Float32Array).

	edgeCountsWP := otherByteArray.
	alphaMaskWP := anotherByteArray.
	contour := aFloat32Array.
	self pvt_quadraticBezierWPFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl.! !

!VectorEnginePlugin methodsFor: 'whole pixel - path algorithms' stamp: 'jmv 5/18/2021 17:55:56'!
updateAlphasWPForX: x y: y

	| t b l r pixelIndex candidateAlpha alphaByte distanceToAxisSquared dy dySquared dx dxSquared |
	self var: #x type: 'float'.
	self var: #y type: 'float'.

	self var: #candidateAlpha type: 'uint8_t'.
	self var: #alphaByte type: 'uint8_t'.

	self var: #distanceToAxisSquared type: 'float'.
	self var: #dy type: 'float'.
	self var: #dySquared type: 'float'.
	self var: #dx type: 'float'.
	self var: #dxSquared type: 'float'.

	"Compute affected rect. Honor clipRect"
	t := (y - auxStrokeWidthDilatedHalf + 1) asInteger.														"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	t < clipTop ifTrue: [ t := clipTop ].
	b := (y + auxStrokeWidthDilatedHalf) asInteger.
	b > clipBottom ifTrue: [ b := clipBottom ].
	l := (x - auxStrokeWidthDilatedHalf + 1) asInteger.														"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	l < clipLeft ifTrue: [ l := clipLeft ].
	r := (x + auxStrokeWidthDilatedHalf) asInteger.
	r > clipRight ifTrue: [ r := clipRight ].

	t to: b do: [ :displayY |
		pixelIndex := displayY * targetWidth + l -1.
		dy := displayY - y.
		dySquared := dy * dy.
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			alphaByte := alphaMaskWP at: pixelIndex.
			alphaByte = 16r7F ifFalse: [
				dx := displayX - x.
				dxSquared := dx * dx.
				distanceToAxisSquared := dxSquared + dySquared.
				distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
					ifTrue: [
						distanceToAxisSquared <= auxStrokeWidthErodedHalfSquared
							ifTrue: [
								candidateAlpha := 16r7F ]
							ifFalse: [
								self cCode: '
										candidateAlpha = (uint8_t)((auxStrokeWidthDilatedHalf - (sqrt(distanceToAxisSquared))) * auxAntiAliasingWidthScaledInverse);'
									inSmalltalk: [
										candidateAlpha := (auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt * auxAntiAliasingWidthScaledInverse) asInteger ]].
						candidateAlpha > alphaByte ifTrue: [
							alphaMaskWP at: pixelIndex put: candidateAlpha ]]]]]! !

!VectorEnginePlugin methodsFor: 'whole pixel - path algorithms' stamp: 'jmv 6/12/2021 20:31:41'!
updateAlphasWPZeroStrokeForX: x y: y

	"slight optimization possible when we know width = 0:
		- we know candidateAlpha is never 127
		- we know distanceToEdge is always > erodedHalfWidth."
	| t b l r pixelIndex candidateAlpha alphaByte distanceToAxisSquared dy dySquared dx |
	self var: #x type: 'float'.
	self var: #y type: 'float'.

	self var: #candidateAlpha type: 'uint8_t'.
	self var: #alphaByte type: 'uint8_t'.

	self var: #distanceToAxisSquared type: 'float'.
	self var: #dy type: 'float'.
	self var: #dySquared type: 'float'.
	self var: #dx type: 'float'.
	self var: #dxSquared type: 'float'.

	"Compute affected rect. Honor clipRect"
	t := (y - auxStrokeWidthDilatedHalf + 1) asInteger.														"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	t < clipTop ifTrue: [ t := clipTop ].
	b := (y + auxStrokeWidthDilatedHalf) asInteger.
	b > clipBottom ifTrue: [ b := clipBottom ].
	l := (x - auxStrokeWidthDilatedHalf + 1) asInteger.														"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	l < clipLeft ifTrue: [ l := clipLeft ].
	r := (x + auxStrokeWidthDilatedHalf) asInteger.
	r > clipRight ifTrue: [ r := clipRight ].

	t to: b do: [ :displayY |
		pixelIndex := displayY * targetWidth + l -1.
		dy := displayY - y.
		dySquared := dy * dy.
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			alphaByte := alphaMaskWP at: pixelIndex.
			alphaByte = 16r7F ifFalse: [
				dx := displayX - x.
				distanceToAxisSquared := dx * dx + dySquared.
				distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
					ifTrue: [
						self cCode: '
								candidateAlpha = (uint8_t)((auxStrokeWidthDilatedHalf - (sqrt(distanceToAxisSquared))) * auxAntiAliasingWidthScaledInverse);'
							inSmalltalk: [
								candidateAlpha := (auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt * auxAntiAliasingWidthScaledInverse) asInteger ].
						candidateAlpha > alphaByte ifTrue: [
							alphaMaskWP at: pixelIndex put: candidateAlpha ]]]]]! !

!VectorEnginePlugin methodsFor: 'whole pixel - path algorithms' stamp: 'jmv 5/17/2021 15:29:23'!
updateEdgeCountWPAtX: x y: y
	"Compute edges intersecting with this horizontal line, for fills."

	| thisYTruncated pixelY pixelIndex increment count pixelOffset |

	self var: #x type: 'float'.
	self var: #y type: 'float'.

	self var: #count type: 'uint8_t'.
	self var: #increment type: 'uint8_t'.

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
	pixelOffset := (x+1) asInteger max: clipLeft.											"take the next pixel center to the right of x"

	pixelOffset <= clipRight ifTrue: [
		pixelIndex := pixelY * targetWidth + pixelOffset.
		count := edgeCountsWP at: pixelIndex.
		count := count + increment.
		edgeCountsWP at: pixelIndex put: count ].! !

!VectorEnginePlugin methodsFor: 'whole pixel - text and multi paths' stamp: 'dtl 7/27/2021 16:41:37'!
displayStringWP: aString from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes useST80Glyphs: aBoolean edgeCounts: otherByteArray alphaMask: anotherByteArray

	| answer nextGlyphX nextGlyphY ttMoveToX ttMoveToY ttEndX ttEndY ttControlX ttControlY advanceWidth numContours contourStartX contourStartY numBeziers startX startY endX endY controlX controlY dx dy iso8859s15 i xMinEnd xMaxEnd yMinEnd yMaxEnd length t0 x0 y0 t oneLessT increment f1 f2 f3 x y correction |
	self
		primitive: 'primDisplayStringWP'
		parameters: #(String SmallInteger SmallInteger Float Float Float Float Float32Array IntegerArray Boolean ByteArray ByteArray).

	self var: #answer type: 'double'.
	self var: #nextGlyphX type: 'float'.
	self var: #nextGlyphY type: 'float'.
	self var: #ttMoveToX type: 'float'.
	self var: #ttMoveToY type: 'float'.
	self var: #ttEndX type: 'float'.
	self var: #ttEndY type: 'float'.
	self var: #ttControlX type: 'float'.
	self var: #ttControlY type: 'float'.
	self var: #advanceWidth type: 'float'.
	self var: #contourStartX type: 'float'.
	self var: #contourStartY type: 'float'.
	self var: #startX type: 'float'.
	self var: #startY type: 'float'.
	self var: #endX type: 'float'.
	self var: #endY type: 'float'.
	self var: #controlX type: 'float'.
	self var: #controlY type: 'float'.
	self var: #dx type: 'float'.
	self var: #dy type: 'float'.
	self var: #iso8859s15 type: 'uint8_t'.

	self var: #length type: 'float'.
	self var: #correction type: 'float'.
	self var: #t0 type: 'float'.
	self var: #x0 type: 'float'.
	self var: #y0 type: 'float'.
	self var: #t type: 'float'.
	self var: #oneLessT type: 'float'.
	self var: #increment type: 'float'.
	self var: #f1 type: 'float'.
	self var: #f2 type: 'float'.
	self var: #f3 type: 'float'.
	self var: #x type: 'float'.
	self var: #y type: 'float'.
	self var: #xMinEnd type: 'float'.
	self var: #xMaxEnd type: 'float'.
	self var: #yMinEnd type: 'float'.
	self var: #yMaxEnd type: 'float'.

	edgeCountsWP := otherByteArray.
	alphaMaskWP := anotherByteArray.
	txA11 := txA11 * sx.
	txA12 := txA12 * sy.
	txA21 := txA21 * sx.
	txA22 := txA22 * sy.

	nextGlyphX := destX / sx.
	nextGlyphY := destY / sy.
	startIndex-1 to: stopIndex-1 do: [ :charIndex |
		self cCode:
				'iso8859s15 = aString[charIndex];'
			inSmalltalk: [
				iso8859s15 := (aString at: charIndex+1) iso8859s15Code. ].
		aBoolean ifTrue: [
			iso8859s15 = 95 ifTrue: [ iso8859s15 := 28 ].												"If underscore, use left arrow"
			iso8859s15 = 94 ifTrue: [ iso8859s15 := 30 ].												"If caret, use up arrow"
			].
		i := contourDataIndexes at: iso8859s15.
		i := i - 1.
		advanceWidth := contourData at: i. i := i + 5.
		numContours := (contourData at: i) asInteger. i := i + 1.
		1 to: numContours do: [ :idx |
			numBeziers := (contourData at: i) asInteger. i := i + 1.
			ttMoveToX := (contourData at: i) + nextGlyphX. i := i + 1.
			ttMoveToY := (contourData at: i) + nextGlyphY. i := i + 1.
			startX := (ttMoveToX * txA11) + (ttMoveToY * txA12) + txA13.
			startY := (ttMoveToX * txA21) + (ttMoveToY * txA22) + txA23.
			contourStartX := startX.
			contourStartY := startY.
			self initializeTrajectoryFragment.
			1 to: numBeziers do: [ :idx2 |
				ttEndX := contourData at: i. i := i + 1.
				ttEndY := contourData at: i. i := i + 1.
				ttControlX := contourData at: i. i := i + 1.
				ttControlY := contourData at: i. i := i + 1.
				endX := (ttEndX * txA11) + (ttEndY * txA12) + startX.
				endY := (ttEndX * txA21) + (ttEndY * txA22) + startY.
				controlX := (ttControlX * txA11) + (ttControlY * txA12) + startX.
				controlY := (ttControlX * txA21) + (ttControlY * txA22) + startY.
				"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
				xMinEnd := startX min: endX.
				xMaxEnd := startX max: endX.
				yMinEnd := startY min: endY.
				yMaxEnd := startY max: endY.
				spanLeft := spanLeft min: (xMinEnd min: (xMinEnd+controlX) / 2.0).
				spanRight := spanRight max: (xMaxEnd max: (xMaxEnd+controlX) / 2.0).
				spanTop := spanTop min: (yMinEnd min: (yMinEnd+controlY) / 2.0).
				spanBottom := spanBottom max: (yMaxEnd max: (yMaxEnd+controlY) / 2.0).
				"Compute Quadratic Bezier Curve,"
				"Case t = 0.0"
				x := startX.
				y := startY.
				self updateAlphasWPZeroStrokeForX: x y: y.
				self updateEdgeCountWPAtX: x y: y.
				"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
				self cCode: 'dx = fabs(endX-startX); dy = fabs(endY-startY);'
				inSmalltalk: [ dx := (endX-startX) abs. dy := (endY-startY) abs ].
				increment := 0.5 / (dx max: dy) min: 0.5.
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
					self cCode: '
						length = sqrt(dx*dx + dy*dy);'
						inSmalltalk: [ length := ((dx*dx) + (dy*dy)) sqrt ].
					correction := hop / length.
					[
						increment := increment / length * hop.
						t := t0 + increment. oneLessT := 1.0 - t.
						f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
						x := (f1 * startX) + (f2 * controlX) + (f3 * endX).
						y := (f1 * startY) + (f2 * controlY) + (f3 * endY).
						dx := x-x0. dy := y-y0.
						self cCode: '
							length = sqrt(dx*dx + dy*dy);'
							inSmalltalk: [ length := ((dx*dx) + (dy*dy)) sqrt ].
						correction := hop / length.
						correction < 1.0]  whileTrue.		   "Keep adjusting if hop would be too big"
					t < 1.0 ]
				whileTrue: [
					self updateAlphasWPZeroStrokeForX: x y: y.
					self updateEdgeCountWPAtX: x y: y ].
				"Note: For TrueType font definitions, we assume that all contour fragments start exactly where the previous ends.
				This means that the end point is only added for the last fragment of the contour, and not for each one of them."
				startX := endX.
				startY := endY.	
			].
			self updateAlphasWPZeroStrokeForX: endX y: endY.
			self updateEdgeCountWPAtX: endX y: endY.
			"Similar effect to ensureClosePath in #finishPath:,
			but assume the TrueType definition is essentially right, and there might only be a rounding error.
			So, don't draw a line, but just (possibly) correct edgeCountsWP. The possibility of rounding error is most likely zero.
			Anyway, this is cheap."
			self updateEdgeCountWPAtX: contourStartX y: contourStartY.
		].
		nextGlyphX := nextGlyphX + advanceWidth.
	].
	txA11 := txA11 / sx.
	txA12 := txA12 / sy.
	txA21 := txA21 / sx.
	txA22 := txA22 / sy.
	answer := nextGlyphX * sx.
	^answer asOop: Float! !

!VectorEnginePlugin methodsFor: 'whole pixel - text and multi paths' stamp: 'dtl 7/27/2021 16:41:46'!
displayUtf32WP: aWordArray from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes useST80Glyphs: aBoolean edgeCounts: otherByteArray alphaMask: anotherByteArray

	| answer nextGlyphX nextGlyphY ttMoveToX ttMoveToY ttEndX ttEndY ttControlX ttControlY advanceWidth numContours contourStartX contourStartY numBeziers startX startY endX endY controlX controlY dx dy utf32 utf8Byte i xMinEnd xMaxEnd yMinEnd yMaxEnd length t0 x0 y0 t oneLessT increment f1 f2 f3 x y correction |
	self
		primitive: 'primDisplayUtf32WP'
		parameters: #(WordArray SmallInteger SmallInteger Float Float Float Float Float32Array IntegerArray Boolean ByteArray ByteArray).

	self var: #answer type: 'double'.
	self var: #nextGlyphX type: 'float'.
	self var: #nextGlyphY type: 'float'.
	self var: #ttMoveToX type: 'float'.
	self var: #ttMoveToY type: 'float'.
	self var: #ttEndX type: 'float'.
	self var: #ttEndY type: 'float'.
	self var: #ttControlX type: 'float'.
	self var: #ttControlY type: 'float'.
	self var: #advanceWidth type: 'float'.
	self var: #contourStartX type: 'float'.
	self var: #contourStartY type: 'float'.
	self var: #startX type: 'float'.
	self var: #startY type: 'float'.
	self var: #endX type: 'float'.
	self var: #endY type: 'float'.
	self var: #controlX type: 'float'.
	self var: #controlY type: 'float'.
	self var: #dx type: 'float'.
	self var: #dy type: 'float'.

	self var: #length type: 'float'.
	self var: #correction type: 'float'.
	self var: #t0 type: 'float'.
	self var: #x0 type: 'float'.
	self var: #y0 type: 'float'.
	self var: #t type: 'float'.
	self var: #oneLessT type: 'float'.
	self var: #increment type: 'float'.
	self var: #f1 type: 'float'.
	self var: #f2 type: 'float'.
	self var: #f3 type: 'float'.
	self var: #x type: 'float'.
	self var: #y type: 'float'.
	self var: #xMinEnd type: 'float'.
	self var: #xMaxEnd type: 'float'.
	self var: #yMinEnd type: 'float'.
	self var: #yMaxEnd type: 'float'.

	edgeCountsWP := otherByteArray.
	alphaMaskWP := anotherByteArray.
	txA11 := txA11 * sx.
	txA12 := txA12 * sy.
	txA21 := txA21 * sx.
	txA22 := txA22 * sy.

	nextGlyphX := destX / sx.
	nextGlyphY := destY / sy.
	startIndex-1 to: stopIndex-1 do: [ :utf32Index |
		utf32 := aWordArray at: utf32Index.
		aBoolean ifTrue: [
			utf32 = 95 ifTrue: [ utf32 := 8592 ].												"If underscore, use left arrow"
			utf32 = 94 ifTrue: [ utf32 := 8593 ].												"If caret, use up arrow"
			].
		utf32 <= 16r7F
			ifTrue: [
				utf8Byte := utf32.
				i := contourDataIndexes at: utf8Byte ]
			ifFalse: [ utf32 <= 16r7FF
			ifTrue: [
				utf8Byte := (utf32 bitShift: -6) bitOr: 2r11000000.
				i := contourDataIndexes at: utf8Byte.
				utf8Byte := (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
				i := contourDataIndexes at: utf8Byte - i ]
			ifFalse: [ utf32 <= 16rFFFF
			ifTrue: [
				utf8Byte := (utf32 bitShift: -12) bitOr: 2r11100000.
				i := contourDataIndexes at: utf8Byte.
				utf8Byte := ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
				i := contourDataIndexes at: utf8Byte - i.
				utf8Byte := (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
				i := contourDataIndexes at: utf8Byte - i ]
			ifFalse: [
				utf8Byte := (utf32 bitShift: -18) bitOr: 2r11110000.
				i := contourDataIndexes at: utf8Byte.
				utf8Byte := ((utf32 bitShift: -12) bitAnd: 2r00111111) bitOr: 2r10000000.
				i := contourDataIndexes at: utf8Byte - i.
				utf8Byte := ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
				i := contourDataIndexes at: utf8Byte - i.
				utf8Byte := (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
				i := contourDataIndexes at: utf8Byte - i ]]].
		i := i - 1.
		advanceWidth := contourData at: i. i := i + 5.
		numContours := (contourData at: i) asInteger. i := i + 1.
		1 to: numContours do: [ :idx |
			numBeziers := (contourData at: i) asInteger. i := i + 1.
			ttMoveToX := (contourData at: i) + nextGlyphX. i := i + 1.
			ttMoveToY := (contourData at: i) + nextGlyphY. i := i + 1.
			startX := (ttMoveToX * txA11) + (ttMoveToY * txA12) + txA13.
			startY := (ttMoveToX * txA21) + (ttMoveToY * txA22) + txA23.
			contourStartX := startX.
			contourStartY := startY.
			self initializeTrajectoryFragment.
			1 to: numBeziers do: [ :idx2 |
				ttEndX := contourData at: i. i := i + 1.
				ttEndY := contourData at: i. i := i + 1.
				ttControlX := contourData at: i. i := i + 1.
				ttControlY := contourData at: i. i := i + 1.
				endX := (ttEndX * txA11) + (ttEndY * txA12) + startX.
				endY := (ttEndX * txA21) + (ttEndY * txA22) + startY.
				controlX := (ttControlX * txA11) + (ttControlY * txA12) + startX.
				controlY := (ttControlX * txA21) + (ttControlY * txA22) + startY.
				"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
				xMinEnd := startX min: endX.
				xMaxEnd := startX max: endX.
				yMinEnd := startY min: endY.
				yMaxEnd := startY max: endY.
				spanLeft := spanLeft min: (xMinEnd min: (xMinEnd+controlX) / 2.0).
				spanRight := spanRight max: (xMaxEnd max: (xMaxEnd+controlX) / 2.0).
				spanTop := spanTop min: (yMinEnd min: (yMinEnd+controlY) / 2.0).
				spanBottom := spanBottom max: (yMaxEnd max: (yMaxEnd+controlY) / 2.0).
				"Compute Quadratic Bezier Curve,"
				"Case t = 0.0"
				x := startX.
				y := startY.
				self updateAlphasWPZeroStrokeForX: x y: y.
				self updateEdgeCountWPAtX: x y: y.
				"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
				self cCode: 'dx = fabs(endX-startX); dy = fabs(endY-startY);'
				inSmalltalk: [ dx := (endX-startX) abs. dy := (endY-startY) abs ].
				increment := 0.5 / (dx max: dy) min: 0.5.
				t := 0.0.
				[
					t0 := t. x0 := x. y0 := y.
					"Compute next point. Only C version to avoid hitting a limit in Smalltalk compiler."
					self cCode: '
						t = t0 + increment; oneLessT = 1.0 - t;
						f1 = oneLessT * oneLessT; f2 = 2.0 * oneLessT * t; f3 = t * t;
						x = (f1 * startX) + (f2 * controlX) + (f3 * endX);
						y = (f1 * startY) + (f2 * controlY) + (f3 * endY);
						dx = x-x0; dy = y-y0;
						length = sqrt(dx*dx + dy*dy);'
						inSmalltalk: [ length := ((dx*dx) + (dy*dy)) sqrt ].
					correction := hop / length.
					"Now adjust the increment to aim at the required hop length, and recompute next point."
					[
						increment := increment / length * hop.
						t := t0 + increment. oneLessT := 1.0 - t.
						f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
						x := (f1 * startX) + (f2 * controlX) + (f3 * endX).
						y := (f1 * startY) + (f2 * controlY) + (f3 * endY).
						dx := x-x0. dy := y-y0.
						self cCode: '
							length = sqrt(dx*dx + dy*dy);'
							inSmalltalk: [ length := ((dx*dx) + (dy*dy)) sqrt ].
						correction := hop / length.
						correction < 1.0]  whileTrue.		   "Keep adjusting if hop would be too big"
					t < 1.0 ]
				whileTrue: [
					self updateAlphasWPZeroStrokeForX: x y: y.
					self updateEdgeCountWPAtX: x y: y. ].
				"Note: For TrueType font definitions, we assume that all contour fragments start exactly where the previous ends.
				This means that the end point is only added for the last fragment of the contour, and not for each one of them."
				startX := endX.
				startY := endY.	
			].
			self updateAlphasWPZeroStrokeForX: endX y: endY.
			self updateEdgeCountWPAtX: endX y: endY.
			"Similar effect to ensureClosePath in #finishPath:,
			but assume the TrueType definition is essentially right, and there might only be a rounding error.
			So, don't draw a line, but just (possibly) correct edgeCountsWP. The possibility of rounding error is most likely zero.
			Anyway, this is cheap."
			self updateEdgeCountWPAtX: contourStartX y: contourStartY.
		].
		nextGlyphX := nextGlyphX + advanceWidth.
	].
	txA11 := txA11 / sx.
	txA12 := txA12 / sy.
	txA21 := txA21 / sx.
	txA22 := txA22 / sy.
	answer := nextGlyphX * sx.
	^answer asOop: Float! !

!VectorEnginePlugin methodsFor: 'whole pixel - text and multi paths' stamp: 'dtl 7/27/2021 16:41:54'!
displayUtf8WP: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes useST80Glyphs: aBoolean edgeCounts: otherByteArray alphaMask: anotherByteArray

	| answer nextGlyphX nextGlyphY ttMoveToX ttMoveToY ttEndX ttEndY ttControlX ttControlY advanceWidth numContours contourStartX contourStartY numBeziers startX startY endX endY controlX controlY dx dy baseIndex byte i xMinEnd xMaxEnd yMinEnd yMaxEnd length t0 x0 y0 t oneLessT increment f1 f2 f3 x y correction |
	self
		primitive: 'primDisplayUtf8WP'
		parameters: #(ByteArray SmallInteger SmallInteger Float Float Float Float Float32Array IntegerArray Boolean ByteArray ByteArray).

	self var: #answer type: 'double'.
	self var: #nextGlyphX type: 'float'.
	self var: #nextGlyphY type: 'float'.
	self var: #ttMoveToX type: 'float'.
	self var: #ttMoveToY type: 'float'.
	self var: #ttEndX type: 'float'.
	self var: #ttEndY type: 'float'.
	self var: #ttControlX type: 'float'.
	self var: #ttControlY type: 'float'.
	self var: #advanceWidth type: 'float'.
	self var: #contourStartX type: 'float'.
	self var: #contourStartY type: 'float'.
	self var: #startX type: 'float'.
	self var: #startY type: 'float'.
	self var: #endX type: 'float'.
	self var: #endY type: 'float'.
	self var: #controlX type: 'float'.
	self var: #controlY type: 'float'.
	self var: #dx type: 'float'.
	self var: #dy type: 'float'.
	self var: #byte type: 'uint8_t'.

	self var: #length type: 'float'.
	self var: #correction type: 'float'.
	self var: #t0 type: 'float'.
	self var: #x0 type: 'float'.
	self var: #y0 type: 'float'.
	self var: #t type: 'float'.
	self var: #oneLessT type: 'float'.
	self var: #increment type: 'float'.
	self var: #f1 type: 'float'.
	self var: #f2 type: 'float'.
	self var: #f3 type: 'float'.
	self var: #x type: 'float'.
	self var: #y type: 'float'.
	self var: #xMinEnd type: 'float'.
	self var: #xMaxEnd type: 'float'.
	self var: #yMinEnd type: 'float'.
	self var: #yMaxEnd type: 'float'.

	edgeCountsWP := otherByteArray.
	alphaMaskWP := anotherByteArray.
	txA11 := txA11 * sx.
	txA12 := txA12 * sy.
	txA21 := txA21 * sx.
	txA22 := txA22 * sy.

	nextGlyphX := destX / sx.
	nextGlyphY := destY / sy.
	baseIndex := 0.
	byteStartIndex-1 to: byteStopIndex-1 do: [ :byteIndex |
		byte := aByteArray at: byteIndex.
		i := contourDataIndexes at: baseIndex + byte.
		aBoolean ifTrue: [
			byte = 95 ifTrue: [
				i := contourDataIndexes at: 226.
				i := contourDataIndexes at: 134 - i.
				i := contourDataIndexes at: 144 - i ].
			byte = 94 ifTrue: [ 
				i := contourDataIndexes at: 226.
				i := contourDataIndexes at: 134 - i.
				i := contourDataIndexes at: 145 - i ]].
		i < 0
			ifTrue: [ baseIndex := 0 - i ]
			ifFalse: [
				i := i - 1.
				advanceWidth := contourData at: i. i := i + 5.
				numContours := (contourData at: i) asInteger. i := i + 1.
				1 to: numContours do: [ :idx |
					numBeziers := (contourData at: i) asInteger. i := i + 1.
					ttMoveToX := (contourData at: i) + nextGlyphX. i := i + 1.
					ttMoveToY := (contourData at: i) + nextGlyphY. i := i + 1.
					startX := (ttMoveToX * txA11) + (ttMoveToY * txA12) + txA13.
					startY := (ttMoveToX * txA21) + (ttMoveToY * txA22) + txA23.
					contourStartX := startX.
					contourStartY := startY.
					self initializeTrajectoryFragment.
					1 to: numBeziers do: [ :idx2 |
						ttEndX := contourData at: i. i := i + 1.
						ttEndY := contourData at: i. i := i + 1.
						ttControlX := contourData at: i. i := i + 1.
						ttControlY := contourData at: i. i := i + 1.
						endX := (ttEndX * txA11) + (ttEndY * txA12) + startX.
						endY := (ttEndX * txA21) + (ttEndY * txA22) + startY.
						controlX := (ttControlX * txA11) + (ttControlY * txA12) + startX.
						controlY := (ttControlX * txA21) + (ttControlY * txA22) + startY.
						"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
						xMinEnd := startX min: endX.
						xMaxEnd := startX max: endX.
						yMinEnd := startY min: endY.
						yMaxEnd := startY max: endY.
						spanLeft := spanLeft min: (xMinEnd min: (xMinEnd+controlX) / 2.0).
						spanRight := spanRight max: (xMaxEnd max: (xMaxEnd+controlX) / 2.0).
						spanTop := spanTop min: (yMinEnd min: (yMinEnd+controlY) / 2.0).
						spanBottom := spanBottom max: (yMaxEnd max: (yMaxEnd+controlY) / 2.0).
						"Case t = 0.0"
						x := startX.
						y := startY.
						self updateAlphasWPZeroStrokeForX: x y: y.
						self updateEdgeCountWPAtX: x y: y.
						"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
						self cCode: 'dx = fabs(endX-startX); dy = fabs(endY-startY);'
						inSmalltalk: [ dx := (endX-startX) abs. dy := (endY-startY) abs ].
						increment := 0.5 / (dx max: dy) min: 0.5.
						"Compute Quadratic Bezier Curve,"
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
							self cCode: '
								length = sqrt(dx*dx + dy*dy);'
								inSmalltalk: [ length := ((dx*dx) + (dy*dy)) sqrt ].
							correction := hop / length.
							[
								increment := increment / length * hop.
								t := t0 + increment. oneLessT := 1.0 - t.
								f1 := oneLessT * oneLessT. f2 := 2.0 * oneLessT * t. f3 := t * t.
								x := (f1 * startX) + (f2 * controlX) + (f3 * endX).
								y := (f1 * startY) + (f2 * controlY) + (f3 * endY).
								dx := x-x0. dy := y-y0.
								self cCode: '
									length = sqrt(dx*dx + dy*dy);'
									inSmalltalk: [ length := ((dx*dx) + (dy*dy)) sqrt ].
								correction := hop / length.
								correction < 1.0]  whileTrue.		   "Keep adjusting if hop would be too big"
							t < 1.0 ]
						whileTrue: [
							self updateAlphasWPZeroStrokeForX: x y: y.
							self updateEdgeCountWPAtX: x y: y ].
						"Note: For TrueType font definitions, we assume that all contour fragments start exactly where the previous ends.
						This means that the end point is only added for the last fragment of the contour, and not for each one of them."
						startX := endX.
						startY := endY.	
					].
					self updateAlphasWPZeroStrokeForX: endX y: endY.
					self updateEdgeCountWPAtX: endX y: endY.
					"Similar effect to ensureClosePath in #finishPath:,
					but assume the TrueType definition is essentially right, and there might only be a rounding error.
					So, don't draw a line, but just (possibly) correct edgeCountsWP. The possibility of rounding error is most likely zero.
					Anyway, this is cheap."
					self updateEdgeCountWPAtX: contourStartX y: contourStartY.
				].
				nextGlyphX := nextGlyphX + advanceWidth.
				baseIndex := 0.
			]
	].
	txA11 := txA11 / sx.
	txA12 := txA12 / sy.
	txA21 := txA21 / sx.
	txA22 := txA22 / sy.
	answer := nextGlyphX * sx.
	^answer asOop: Float! !

!VectorEnginePlugin methodsFor: 'whole pixel - text and multi paths' stamp: 'dtl 7/27/2021 16:42:07'!
pathSequenceWP: aFloat32Array size: size edgeCounts: otherByteArray alphaMask: anotherByteArray contour: otherFloat32Array

	| i commandType startX startY endX endY control1X control1Y control2X control2Y |
	self
		primitive: 'primPathSequenceWP'
		parameters: #(Float32Array SmallInteger ByteArray ByteArray Float32Array).

	self var: #startX type: 'float'.
	self var: #startY type: 'float'.
	self var: #endX type: 'float'.
	self var: #endY type: 'float'.
	self var: #control1X type: 'float'.
	self var: #control1Y type: 'float'.
	self var: #control2X type: 'float'.
	self var: #control2Y type: 'float'.

	edgeCountsWP := otherByteArray.
	alphaMaskWP := anotherByteArray.
	contour := otherFloat32Array.
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

!VectorEnginePlugin methodsFor: 'whole pixel - blend stroke and fill' stamp: 'jmv 9/30/2021 12:12:21'!
blendFillOnlyWPOTAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits
	"Blends fill color over background.
	WP: Whole pixel anti aliasing.
	OT: Target ignored on input and set to opaque on output.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB resultRBits resultGBits resultBBits antiAliasAlpha alpha unAlpha clippingAntiAliasBits clippingAntiAlias morphIdWord |

	self var: #antiAliasAlphaBits type: 'uint8_t'.

	self var: #targetWord type: 'uint32_t'.
	self var: #resultRBits type: 'uint32_t'.
	self var: #resultGBits type: 'uint32_t'.
	self var: #resultBBits type: 'uint32_t'.

	self var: #morphIdWord type: 'uint32_t'.
	self var: #clippingAntiAliasBits type: 'uint32_t'.

	self var: #clippingAntiAlias type: 'float'.
	self var: #resultR type: 'float'.
	self var: #resultG type: 'float'.
	self var: #resultB type: 'float'.
	self var: #antiAliasAlpha type: 'float'.
	self var: #alpha type: 'float'.
	self var: #unAlpha type: 'float'.

	antiAliasAlpha := antiAliasAlphaBits * 0.007874.								"1.0/127.0"
	alpha := antiAliasAlpha * fillA.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			morphIdWord := morphIds at: pixelIndex.
			clippingAntiAliasBits := morphIdWord bitAnd: 16r7F.
			antiAliasAlphaBits > clippingAntiAliasBits ifTrue: [
				clippingAntiAliasBits := antiAliasAlphaBits]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					morphIdWord := morphIds at: pixelIndex.
					clippingAntiAliasBits := morphIdWord bitAnd: 16r7F.
					clippingAntiAlias := clippingAntiAliasBits * 0.007874.								"1.0/127.0"
					alpha := alpha * clippingAntiAlias ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAliasBits := 0]].

	alpha = 0.0 ifFalse: [
		targetWord := targetBits at: pixelIndex.
		resultRBits := targetWord bitAnd: 16rFF0000.
		resultGBits := targetWord bitAnd: 16rFF00.
		resultBBits := targetWord bitAnd: 16rFF.
		unAlpha := 1.0 - alpha.
		self cCode: '
				resultR = (alpha * fillR) + (unAlpha * (resultRBits >> 16));
				resultRBits = (uint32_t)(resultR + 0.5) << 16;
				resultG = (alpha * fillG) + (unAlpha * (resultGBits >> 8));
				resultGBits = (uint32_t)(resultG + 0.5) << 8;
				resultB = (alpha * fillB) + (unAlpha * resultBBits);
				resultBBits = (uint32_t)(resultB + 0.5);'
			inSmalltalk: [
				resultR := alpha * fillR + (unAlpha * (resultRBits bitShift: -16)).
				resultRBits := (resultR + 0.5) asInteger bitShift: 16.
				resultG := alpha * fillG + (unAlpha * (resultGBits bitShift: -8)).
				resultGBits := (resultG + 0.5) asInteger bitShift: 8.
				resultB := alpha * fillB + (unAlpha * resultBBits).
				resultBBits := (resultB + 0.5) asInteger ].

		targetWord := ((16rFF000000 bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		targetBits at: pixelIndex put: targetWord.
		morphIdWord := currentMorphId<<8 + clippingAntiAliasBits.
		morphIds at: pixelIndex put: morphIdWord ].! !

!VectorEnginePlugin methodsFor: 'whole pixel - blend stroke and fill' stamp: 'jmv 9/30/2021 14:12:37'!
blendFillOnlyWPOTLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherByteArray alphaMask: anotherByteArray
	"Blends fill color over background.
	WP: Whole pixel anti aliasing.
	OT: Target ignored on input and set to opaque on output.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"
	| pixelIndex edgesUpToThisPixel edgesThisPixel antiAliasAlphaBits optimizedOpaqueTargetWord resultRBits resultGBits resultBBits optimizedOpaqueMorphIdWord |
	self
		primitive: 'primBlendFillOnlyWPOT'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger WordArray WordArray ByteArray ByteArray).

	self var: #edgesThisPixel type: 'uint8_t'.
	self var: #edgesUpToThisPixel type: 'uint8_t'.
	self var: #antiAliasAlphaBits type: 'uint8_t'.
	self var: #optimizedOpaqueTargetWord type: 'uint32_t'.
	self var: #resultRBits type: 'uint32_t'.
	self var: #resultGBits type: 'uint32_t'.
	self var: #resultBBits type: 'uint32_t'.
	self var: #optimizedOpaqueMorphIdWord type: 'uint32_t'.

	targetBits := aBitmap.
	morphIds := aWordArray.
	edgeCountsWP := otherByteArray.
	alphaMaskWP := anotherByteArray.
	"Optimize most common case: Fully opaque fill color, inside fill, away from stroke (no anti aliasing), no clipping at this point."
	optimizedOpaqueTargetWord := 0.
	fillA = 1.0 ifTrue: [
		self cCode: '
			resultRBits = (uint32_t)(fillR + 0.5) << 16;
			resultGBits = (uint32_t)(fillG + 0.5) << 8;
			resultBBits = (uint32_t)(fillB + 0.5);'
		inSmalltalk: [
			resultRBits := (fillR + 0.5) asInteger bitShift: 16.
			resultGBits := (fillG + 0.5) asInteger bitShift: 8.
			resultBBits := (fillB + 0.5) asInteger ].
		optimizedOpaqueTargetWord := ((16rFF000000 bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		(clipCurrentMorph or: [currentClipsSubmorphs])
			ifTrue: [ optimizedOpaqueMorphIdWord := currentMorphId<<8 + 16r7F ]
			ifFalse: [ optimizedOpaqueMorphIdWord := currentMorphId<<8 ]].
	t to: b do: [ :displayY |
		edgesUpToThisPixel := 0.
		pixelIndex := displayY * targetWidth + l - 1.
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			edgesThisPixel := edgeCountsWP at: pixelIndex.
			edgesThisPixel = 0 ifFalse: [
				edgeCountsWP at: pixelIndex put: 0.
				self cCode: '' inSmalltalk: [
					"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
					edgesThisPixel > 127 ifTrue: [ edgesThisPixel := edgesThisPixel - 256 ]].
				edgesUpToThisPixel := edgesUpToThisPixel + edgesThisPixel ].
			antiAliasAlphaBits := alphaMaskWP at: pixelIndex.
			edgesUpToThisPixel = 0
				ifFalse: [				"Inside the shape"
					(optimizedOpaqueTargetWord ~= 0 and: [
						antiAliasAlphaBits = 16r00 and: [
							clipCurrentMorph not or: [ ((morphIds at: pixelIndex) bitAnd: 16r7F) = 16r7F]]])
						ifTrue: [     		"Optimize most common case: Fully opaque fill color, inside fill, away from stroke (no anti aliasing), no clipping at this point."
							targetBits at: pixelIndex put: optimizedOpaqueTargetWord.
							morphIds at: pixelIndex put: optimizedOpaqueMorphIdWord ]
						ifFalse: [
							antiAliasAlphaBits = 16r00 ifFalse: [ alphaMaskWP at: pixelIndex put: 0 ].
							antiAliasAlphaBits := 16r7F - antiAliasAlphaBits.
							self blendFillOnlyWPOTAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits ]]
				ifTrue: [				"Still in the anti aliasing area, but outside the shape, strictly speaking."
					antiAliasAlphaBits = 16r00 ifFalse: [
						alphaMaskWP at: pixelIndex put: 0.
						self blendFillOnlyWPOTAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits ]]]].! !

!VectorEnginePlugin methodsFor: 'whole pixel - blend stroke and fill' stamp: 'jmv 9/30/2021 12:12:48'!
blendStrokeAndFillInsideWPOTAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits
	"Inside the shape: Do an appropriate (anti aliased) gradient between stoke color and fill color (or pick just stroke or just fill).
	Blend this over background.
	WP: Whole pixel anti aliasing.
	OT: Target ignored on input and set to opaque on output.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB resultRBits resultGBits resultBBits antiAliasAlpha antiAliasUnAlpha foreR foreG foreB alpha unAlpha clippingAntiAliasBits clippingAntiAlias morphIdWord |

	self var: #antiAliasAlphaBits type: 'uint8_t'.

	self var: #targetWord type: 'uint32_t'.
	self var: #resultRBits type: 'uint32_t'.
	self var: #resultGBits type: 'uint32_t'.
	self var: #resultBBits type: 'uint32_t'.

	self var: #morphIdWord type: 'uint32_t'.
	self var: #clippingAntiAliasBits type: 'uint32_t'.

	self var: #clippingAntiAlias type: 'float'.
	self var: #resultR type: 'float'.
	self var: #resultG type: 'float'.
	self var: #resultB type: 'float'.
	self var: #antiAliasAlpha type: 'float'.
	self var: #antiAliasUnAlpha type: 'float'.
	self var: #foreR type: 'float'.
	self var: #foreG type: 'float'.
	self var: #foreB type: 'float'.
	self var: #alpha type: 'float'.
	self var: #unAlpha type: 'float'.

	antiAliasAlpha := antiAliasAlphaBits * 0.007874.								"1.0/127.0"
	antiAliasUnAlpha := 1.0 - antiAliasAlpha.
	"We are inside the shape"
	alpha := antiAliasAlpha * strokeA + (antiAliasUnAlpha * fillA).
	foreR := antiAliasAlpha * strokeR + (antiAliasUnAlpha * fillR).
	foreG := antiAliasAlpha * strokeG + (antiAliasUnAlpha * fillG).
	foreB := antiAliasAlpha * strokeB + (antiAliasUnAlpha * fillB).

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			"We are inside the shape"
			clippingAntiAliasBits := 16r7F ]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					morphIdWord := morphIds at: pixelIndex.
					clippingAntiAliasBits := morphIdWord bitAnd: 16r7F.
					clippingAntiAlias := clippingAntiAliasBits * 0.007874.								"1.0/127.0"
					alpha := alpha * clippingAntiAlias ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAliasBits := 0]].

	alpha = 0.0 ifFalse: [
		targetWord := targetBits at: pixelIndex.
		resultRBits := targetWord bitAnd: 16rFF0000.
		resultGBits := targetWord bitAnd: 16rFF00.
		resultBBits := targetWord bitAnd: 16rFF.
		unAlpha := 1.0 - alpha.
		self cCode: '
				resultR = (alpha * foreR) + (unAlpha * (resultRBits >> 16));
				resultRBits = (uint32_t)(resultR + 0.5) << 16;
				resultG = (alpha * foreG) + (unAlpha * (resultGBits >> 8));
				resultGBits = (uint32_t)(resultG + 0.5) << 8;
				resultB = (alpha * foreB) + (unAlpha * resultBBits);
				resultBBits = (uint32_t)(resultB + 0.5);'
			inSmalltalk: [
				resultR := alpha * foreR + (unAlpha * (resultRBits bitShift: -16)).
				resultRBits := (resultR + 0.5) asInteger bitShift: 16.
				resultG := alpha * foreG + (unAlpha * (resultGBits bitShift: -8)).
				resultGBits := (resultG + 0.5) asInteger bitShift: 8.
				resultB := alpha * foreB + (unAlpha * resultBBits).
				resultBBits := (resultB + 0.5) asInteger ].

		targetWord := ((16rFF000000 bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		targetBits at: pixelIndex put: targetWord.
		morphIdWord := currentMorphId<<8 + clippingAntiAliasBits.
		morphIds at: pixelIndex put: morphIdWord ].! !

!VectorEnginePlugin methodsFor: 'whole pixel - blend stroke and fill' stamp: 'jmv 9/30/2021 12:13:01'!
blendStrokeAndFillOutsideWPOTAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits
	"Outside the shape, but still in the stroke: Blend stroke color over background.
	WP: Whole pixel anti aliasing.
	OT: Target ignored on input and set to opaque on output.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB resultRBits resultGBits resultBBits antiAliasAlpha foreR foreG foreB alpha unAlpha clippingAntiAliasBits clippingAntiAlias morphIdWord |

	self var: #antiAliasAlphaBits type: 'uint8_t'.

	self var: #targetWord type: 'uint32_t'.
	self var: #resultRBits type: 'uint32_t'.
	self var: #resultGBits type: 'uint32_t'.
	self var: #resultBBits type: 'uint32_t'.

	self var: #morphIdWord type: 'uint32_t'.
	self var: #clippingAntiAliasBits type: 'uint32_t'.

	self var: #clippingAntiAlias type: 'float'.
	self var: #resultR type: 'float'.
	self var: #resultG type: 'float'.
	self var: #resultB type: 'float'.
	self var: #antiAliasAlpha type: 'float'.
	self var: #foreR type: 'float'.
	self var: #foreG type: 'float'.
	self var: #foreB type: 'float'.
	self var: #alpha type: 'float'.
	self var: #unAlpha type: 'float'.

	antiAliasAlpha := antiAliasAlphaBits * 0.007874.								"1.0/127.0"
	"In the stroke, outside the shape"
	alpha := antiAliasAlpha * strokeA.
	foreR := strokeR.
	foreG := strokeG.
	foreB := strokeB.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			morphIdWord := morphIds at: pixelIndex.
			clippingAntiAliasBits := morphIdWord bitAnd: 16r7F.
			antiAliasAlphaBits > clippingAntiAliasBits ifTrue: [
				clippingAntiAliasBits := antiAliasAlphaBits ]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					morphIdWord := morphIds at: pixelIndex.
					clippingAntiAliasBits := morphIdWord bitAnd: 16r7F.
					clippingAntiAlias := clippingAntiAliasBits * 0.007874.								"1.0/127.0"
					alpha := alpha * clippingAntiAlias ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAliasBits := 0]].

	alpha = 0.0 ifFalse: [
		targetWord := targetBits at: pixelIndex.
		resultRBits := targetWord bitAnd: 16rFF0000.
		resultGBits := targetWord bitAnd: 16rFF00.
		resultBBits := targetWord bitAnd: 16rFF.
		unAlpha := 1.0 - alpha.
		self cCode: '
				resultR = (alpha * foreR) + (unAlpha * (resultRBits >> 16));
				resultRBits = (uint32_t)(resultR + 0.5) << 16;
				resultG = (alpha * foreG) + (unAlpha * (resultGBits >> 8));
				resultGBits = (uint32_t)(resultG + 0.5) << 8;
				resultB = (alpha * foreB) + (unAlpha * resultBBits);
				resultBBits = (uint32_t)(resultB + 0.5);'
			inSmalltalk: [
				resultR := alpha * foreR + (unAlpha * (resultRBits bitShift: -16)).
				resultRBits := (resultR + 0.5) asInteger bitShift: 16.
				resultG := alpha * foreG + (unAlpha * (resultGBits bitShift: -8)).
				resultGBits := (resultG + 0.5) asInteger bitShift: 8.
				resultB := alpha * foreB + (unAlpha * resultBBits).
				resultBBits := (resultB + 0.5) asInteger ].

		targetWord := ((16rFF000000 bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		targetBits at: pixelIndex put: targetWord.
		morphIdWord := currentMorphId<<8 + clippingAntiAliasBits.
		morphIds at: pixelIndex put: morphIdWord ].! !

!VectorEnginePlugin methodsFor: 'whole pixel - blend stroke and fill' stamp: 'jmv 9/30/2021 14:12:48'!
blendStrokeAndFillWPOTLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherByteArray alphaMask: anotherByteArray
	"Blends stroke color and fill color over background.
	Do an appropriate (anti aliased) gradient between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	WP: Whole pixel anti aliasing.
	OT: Target ignored on input and set to opaque on output.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"
	| pixelIndex edgesUpToThisPixel edgesThisPixel antiAliasAlphaBits optimizedOpaqueTargetWord resultRBits resultGBits resultBBits optimizedOpaqueMorphIdWord |
	self
		primitive: 'primBlendStrokeAndFillWPOT'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger WordArray WordArray ByteArray ByteArray).

	self var: #edgesThisPixel type: 'uint8_t'.
	self var: #edgesUpToThisPixel type: 'uint8_t'.
	self var: #antiAliasAlphaBits type: 'uint8_t'.
	self var: #optimizedOpaqueTargetWord type: 'uint32_t'.
	self var: #resultRBits type: 'uint32_t'.
	self var: #resultGBits type: 'uint32_t'.
	self var: #resultBBits type: 'uint32_t'.
	self var: #optimizedOpaqueMorphIdWord type: 'uint32_t'.

	targetBits := aBitmap.
	morphIds := aWordArray.
	edgeCountsWP := otherByteArray.
	alphaMaskWP := anotherByteArray.
	"Optimize most common case: Fully opaque fill color, inside fill, away from stroke (no anti aliasing), no clipping at this point."
	optimizedOpaqueTargetWord := 0.
	fillA = 1.0 ifTrue: [
		self cCode: '
			resultRBits = (uint32_t)(fillR + 0.5) << 16;
			resultGBits = (uint32_t)(fillG + 0.5) << 8;
			resultBBits = (uint32_t)(fillB + 0.5);'
		inSmalltalk: [
			resultRBits := (fillR + 0.5) asInteger bitShift: 16.
			resultGBits := (fillG + 0.5) asInteger bitShift: 8.
			resultBBits := (fillB + 0.5) asInteger ].
		optimizedOpaqueTargetWord := ((16rFF000000 bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		(clipCurrentMorph or: [currentClipsSubmorphs])
			ifTrue: [ optimizedOpaqueMorphIdWord := currentMorphId<<8 + 16r7F ]
			ifFalse: [ optimizedOpaqueMorphIdWord := currentMorphId<<8 ]].
	t to: b do: [ :displayY |
		edgesUpToThisPixel := 0.
		pixelIndex := displayY * targetWidth + l - 1.
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			edgesThisPixel := edgeCountsWP at: pixelIndex.
			edgesThisPixel = 0 ifFalse: [
				edgeCountsWP at: pixelIndex put: 0.
				self cCode: '' inSmalltalk: [
					"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
					edgesThisPixel > 127 ifTrue: [ edgesThisPixel := edgesThisPixel - 256 ]].
				edgesUpToThisPixel := edgesUpToThisPixel + edgesThisPixel ].
			antiAliasAlphaBits := alphaMaskWP at: pixelIndex.
			edgesUpToThisPixel = 0
				ifFalse: [				"Inside the shape"

					(optimizedOpaqueTargetWord ~= 0 and: [
						antiAliasAlphaBits = 16r00 and: [
							clipCurrentMorph not or: [ ((morphIds at: pixelIndex) bitAnd: 16r7F) = 16r7F]]])
						ifTrue: [     		"Optimize most common case: Fully opaque fill color, inside fill, away from stroke (no anti aliasing), no clipping at this point."
							targetBits at: pixelIndex put: optimizedOpaqueTargetWord.
							morphIds at: pixelIndex put: optimizedOpaqueMorphIdWord ]
						ifFalse: [
							antiAliasAlphaBits = 16r00 ifFalse: [ alphaMaskWP at: pixelIndex put: 0 ].
							self blendStrokeAndFillInsideWPOTAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits ]]
				ifTrue: [				"Still in the stroke, but outside the shape, strictly speaking."
					antiAliasAlphaBits = 16r00 ifFalse: [
						alphaMaskWP at: pixelIndex put: 0.
						antiAliasAlphaBits = 0 ifFalse: [self blendStrokeAndFillOutsideWPOTAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits]]]]].! !

!VectorEnginePlugin methodsFor: 'whole pixel - blend stroke and fill' stamp: 'jmv 9/30/2021 12:13:22'!
blendStrokeOnlyWPOTAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits
	"Blends stroke color over background.
	WP: Whole pixel anti aliasing.
	OT: Target ignored on input and set to opaque on output.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB resultRBits resultGBits resultBBits antiAliasAlpha alpha unAlpha clippingAntiAliasBits clippingAntiAlias morphIdWord |

	self var: #antiAliasAlphaBits type: 'uint8_t'.

	self var: #targetWord type: 'uint32_t'.
	self var: #resultRBits type: 'uint32_t'.
	self var: #resultGBits type: 'uint32_t'.
	self var: #resultBBits type: 'uint32_t'.

	self var: #morphIdWord type: 'uint32_t'.
	self var: #clippingAntiAliasBits type: 'uint32_t'.

	self var: #clippingAntiAlias type: 'float'.
	self var: #resultR type: 'float'.
	self var: #resultG type: 'float'.
	self var: #resultB type: 'float'.
	self var: #antiAliasAlpha type: 'float'.
	self var: #alpha type: 'float'.
	self var: #unAlpha type: 'float'.

	antiAliasAlpha := antiAliasAlphaBits * 0.007874.								"1.0/127.0"
	alpha := antiAliasAlpha * strokeA.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			morphIdWord := morphIds at: pixelIndex.
			clippingAntiAliasBits := morphIdWord bitAnd: 16r7F.
			antiAliasAlphaBits > clippingAntiAliasBits ifTrue: [
				clippingAntiAliasBits := antiAliasAlphaBits]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					morphIdWord := morphIds at: pixelIndex.
					clippingAntiAliasBits := morphIdWord bitAnd: 16r7F.
					clippingAntiAlias := clippingAntiAliasBits * 0.007874.								"1.0/127.0"
					alpha := alpha * clippingAntiAlias ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAliasBits := 0]].

	alpha = 0.0 ifFalse: [
		targetWord := targetBits at: pixelIndex.
		resultRBits := targetWord bitAnd: 16rFF0000.
		resultGBits := targetWord bitAnd: 16rFF00.
		resultBBits := targetWord bitAnd: 16rFF.
		unAlpha := 1.0 - alpha.
		self cCode: '
				resultR = (alpha * strokeR) + (unAlpha * (resultRBits >> 16));
				resultRBits = (uint32_t)(resultR + 0.5) << 16;
				resultG = (alpha * strokeG) + (unAlpha * (resultGBits >> 8));
				resultGBits = (uint32_t)(resultG + 0.5) << 8;
				resultB = (alpha * strokeB) + (unAlpha * resultBBits);
				resultBBits = (uint32_t)(resultB + 0.5);'
			inSmalltalk: [
				resultR := alpha * strokeR + (unAlpha * (resultRBits bitShift: -16)).
				resultRBits := (resultR + 0.5) asInteger bitShift: 16.
				resultG := alpha * strokeG + (unAlpha * (resultGBits bitShift: -8)).
				resultGBits := (resultG + 0.5) asInteger bitShift: 8.
				resultB := alpha * strokeB + (unAlpha * resultBBits).
				resultBBits := (resultB + 0.5) asInteger ].


		targetWord := ((16rFF000000 bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		targetBits at: pixelIndex put: targetWord.
		morphIdWord := currentMorphId<<8 + clippingAntiAliasBits.
		morphIds at: pixelIndex put: morphIdWord ].! !

!VectorEnginePlugin methodsFor: 'whole pixel - blend stroke and fill' stamp: 'jmv 9/30/2021 14:26:56'!
blendStrokeOnlyWPOTLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray alphaMask: anotherByteArray
	"Blends stroke color over background.
	WP: Whole pixel anti aliasing.
	OT: Target ignored on input and set to opaque on output.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"
	| pixelIndex antiAliasAlphaBits optimizedOpaqueTargetWord resultRBits resultGBits resultBBits optimizedOpaqueMorphIdWord |
	self
		primitive: 'primBlendStrokeOnlyWPOT'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger WordArray WordArray ByteArray).

	self var: #antiAliasAlphaBits type: 'uint8_t'.
	self var: #optimizedOpaqueTargetWord type: 'uint32_t'.
	self var: #resultRBits type: 'uint32_t'.
	self var: #resultGBits type: 'uint32_t'.
	self var: #resultBBits type: 'uint32_t'.
	self var: #optimizedOpaqueMorphIdWord type: 'uint32_t'.

	targetBits := aBitmap.
	morphIds := aWordArray.
	alphaMaskWP := anotherByteArray.
	"Optimize inner part of a wide stroke: Fully opaque stroke color, inside stroke, away from stroke border (no anti aliasing), no clipping at this point."
	optimizedOpaqueTargetWord := 0.
	fillA = 1.0 ifTrue: [
		self cCode: '
			resultRBits = (uint32_t)(fillR + 0.5) << 16;
			resultGBits = (uint32_t)(fillG + 0.5) << 8;
			resultBBits = (uint32_t)(fillB + 0.5);'
		inSmalltalk: [
			resultRBits := (fillR + 0.5) asInteger bitShift: 16.
			resultGBits := (fillG + 0.5) asInteger bitShift: 8.
			resultBBits := (fillB + 0.5) asInteger ].
		optimizedOpaqueTargetWord := ((16rFF000000 bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		(clipCurrentMorph or: [currentClipsSubmorphs])
			ifTrue: [ optimizedOpaqueMorphIdWord := currentMorphId<<8 + 16r7F ]
			ifFalse: [ optimizedOpaqueMorphIdWord := currentMorphId<<8 ]].
	t to: b do: [ :displayY |
		pixelIndex := displayY * targetWidth + l - 1.
		l to: r do: [ :displayX |
			pixelIndex := pixelIndex + 1.
			antiAliasAlphaBits := alphaMaskWP at: pixelIndex.
			(optimizedOpaqueTargetWord ~= 0 and: [
				antiAliasAlphaBits = 16r7F and: [
					clipCurrentMorph not or: [ ((morphIds at: pixelIndex) bitAnd: 16r7F) = 16r7F]]])
				ifTrue: [     		"Optimize inner part of a wide stroke: Fully opaque stroke color, inside stroke, away from stroke border (no anti aliasing), no clipping at this point."
					targetBits at: pixelIndex put: optimizedOpaqueTargetWord.
					morphIds at: pixelIndex put: optimizedOpaqueMorphIdWord ]
				ifFalse: [
					antiAliasAlphaBits = 16r00 ifFalse: [										"In the stroke"
						alphaMaskWP at: pixelIndex put: 0.
						self blendStrokeOnlyWPOTAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits ]]]].! !

!VectorEnginePlugin class methodsFor: 'translation' stamp: 'jmv 7/5/2021 16:21:10'!
declareCVarsIn: aCCodeGenerator


	aCCodeGenerator addHeaderFile: '<stdint.h>'.
	aCCodeGenerator var: 'targetBits' type: 'uint32_t *'.
	aCCodeGenerator var: 'morphIds' type: 'uint32_t *'.
	aCCodeGenerator var: 'edgeCounts' type: 'uint32_t *'.
	aCCodeGenerator var: 'edgeCountsWP' type: 'uint8_t *'.
	aCCodeGenerator var: 'alphaMask' type: 'uint32_t *'.
	aCCodeGenerator var: 'alphaMaskWP' type: 'uint8_t *'.
	aCCodeGenerator var: 'contour' type: 'float *'.
	aCCodeGenerator var: 'currentMorphId' type: 'uint32_t'.
	#(antiAliasingWidth subPixelDelta hop strokeWidth strokeR strokeG strokeB strokeA fillR fillG fillB fillA txA11 txA12 txA13 txA21 txA22 txA23 auxAntiAliasingWidthScaledInverse auxStrokeWidthDilatedHalf auxStrokeWidthDilatedHalfSquared auxStrokeWidthErodedHalfSquared spanLeft spanTop spanRight spanBottom leftAtThisY rightAtThisY) do: [ :varName |
		aCCodeGenerator var: varName type: 'float' ].! !
