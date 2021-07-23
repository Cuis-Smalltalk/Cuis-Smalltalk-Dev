'From Cuis 5.0 [latest update: #4690] on 23 July 2021 at 6:47:58 am'!
'Description '!
!provides: 'VectorEnginePlugin' 1 25!
!requires: 'VectorGraphics' 1 200 nil!
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

	antiAliasingWidth _ aFloat.
	auxAntiAliasingWidthScaledInverse _ 127.0 / aFloat.
	subPixelDelta _ otherFloat.
	hop _ anotherFloat! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 5/18/2021 10:07:15'!
clipCurrentMorph: aBoolean morphIds: aWordArray
	self
		primitive: 'primClipCurrentMorph'
		parameters: #(Boolean WordArray).

	morphIds _ aWordArray.
	(clipCurrentMorph and: [ aBoolean not ]) ifTrue: [
		| pixelIndex |
		clipTop to: clipBottom do: [ :displayY |
			pixelIndex _ displayY * targetWidth + clipLeft.
			clipLeft to: clipRight do: [ :displayX |
				pixelIndex _ pixelIndex + 1.
				morphIds at: pixelIndex put: ((morphIds at: pixelIndex) bitAnd: 16rFFFFFF00) ]]].
	clipCurrentMorph _ aBoolean! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 3/11/2021 11:00:23'!
clipLeft: l clipTop: t clipRight: r clipBottom: b
	self
		primitive: 'primClipLeftclipTopclipRightclipBottom'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger).

	clipLeft _ l.
	clipTop _ t.
	clipRight _ r.
	clipBottom _ b! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 3/11/2021 11:00:29'!
currentMorphId: aNumber currentClipsSubmorphs: aBoolean
"Bound it someway to 31 or 32 bits (SmallInteger in 32 bits, or uint in 32 bits, etc...)"
	self
		primitive: 'primCurrentMorphIdcurrentClipsSubmorphs'
		parameters: #(SmallInteger Boolean).

	currentMorphId _ aNumber.
	currentMorphId = 0 ifTrue: [ clipCurrentMorph _ false ].												"Essentially for initialization"
	currentClipsSubmorphs _ aBoolean.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 5/17/2021 16:29:46'!
fillR: r g: g b: b a: a
	self
		primitive: 'primFillRGBA'
		parameters: #(Float Float Float Float).

	fillR _ r * 255.0.
	fillG _ g * 255.0.
	fillB _ b * 255.0.
	fillA _ a.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 5/13/2021 11:46:17'!
geometryTxA11: a11 a12: a12 a13: a13 a21: a21 a22: a22 a23: a23
	self
		primitive: 'primGeometryTxSet'
		parameters: #(Float Float Float Float Float Float).

	txA11 _ a11.
	txA12 _ a12.
	txA13 _ a13.
	txA21 _ a21.
	txA22 _ a22.
	txA23 _ a23.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 7/5/2021 16:11:13'!
initializePath
	self
		primitive: 'primInitializePath'
		parameters: #().

	spanLeft _ targetWidth.						"drawable right. Will later be refined."
	spanTop _ targetHeight.						"drawable bottom. Will later be refined."
	spanRight _ 0.						"drawable left. Will later be refined."
	spanBottom _ 0.						"drawable top. Will later be refined."
	prevYRounded _ 16r7FFFFFFF.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 3/5/2021 16:27:16'!
newTrajectoryFragment
	self
		primitive: 'primNewTrajectoryFragment'
		parameters: #().

	prevYTruncated _ 16r7FFFFFFF.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 7/5/2021 17:13:44'!
pluginApiVersion
	"
	VectorEngineWithPlugin isPluginAvailable
	Semantic version numbering in a single SmallInteger:
	Version numbers 1 to 99 are backwards compatible. When incompatibility is needed. jump straight to 101.
	Version numbers 101 to 199 are backwards compatible. When incompatibility is needed. jump straight to 201.
	And so.
	Note: this is Api version, not package version. If no Api change, Api version doesn't change, regardless of changes in the Plugin or Smalltalk code.
	See senders and implementors.
	"
	self
		primitive: 'pluginApiVersion'
		parameters: #().

	^ 5 asOop: SmallInteger.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 7/5/2021 17:01:18'!
resetContourTop: t bottom: b
	self
		primitive: 'primResetContour'
		parameters: #(SmallInteger SmallInteger).

	leftAtThisY _ targetWidth.
	rightAtThisY _ 0.
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

	strokeR _ r * 255.0.
	strokeG _ g * 255.0.
	strokeB _ b * 255.0.
	strokeA _ a.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 4/12/2021 11:21:40'!
strokeWidth: aNumber
	| swErodedHalf |
	self
		primitive: 'primStrokeWidth'
		parameters: #(Float).
	self var: #swErodedHalf type: 'float'.

	strokeWidth _ aNumber.
	auxStrokeWidthDilatedHalf _ strokeWidth + antiAliasingWidth * 0.5.
	auxStrokeWidthDilatedHalfSquared _ auxStrokeWidthDilatedHalf * auxStrokeWidthDilatedHalf.
	swErodedHalf _ strokeWidth - antiAliasingWidth * 0.5.
	self cCode: 
			'auxStrokeWidthErodedHalfSquared = swErodedHalf * fabs(swErodedHalf);'
		inSmalltalk: [ auxStrokeWidthErodedHalfSquared _ swErodedHalf * swErodedHalf abs ].! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 7/5/2021 17:04:09'!
targetBits: aBitmap morphIds: aWordArray edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array targetWidth: aNumber targetHeight: otherNumber
	"All arrays could be pinned instead of passing them every time!!"

	self
		primitive: 'primSetTarget'
		parameters: #(WordArray WordArray WordArray WordArray Float32Array SmallInteger SmallInteger).

	targetBits _ aBitmap.
	morphIds _ aWordArray.
	edgeCounts _ otherWordArray.
	alphaMask _ anotherWordArray.
	contour _ aFloat32Array.
	targetWidth _ aNumber.
	targetHeight _ otherNumber.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 7/5/2021 17:04:14'!
targetBits: aBitmap morphIds: aWordArray edgeCountsWP: otherByteArray alphaMaskWP: anotherByteArray contour: aFloat32Array targetWidth: aNumber targetHeight: otherNumber
	"All arrays could be pinned instead of passing them every time!!"

	self
		primitive: 'primSetTargetWP'
		parameters: #(WordArray WordArray ByteArray ByteArray Float32Array SmallInteger SmallInteger).

	targetBits _ aBitmap.
	morphIds _ aWordArray.
	edgeCountsWP _ otherByteArray.
	alphaMaskWP _ anotherByteArray.
	contour _ aFloat32Array.
	targetWidth _ aNumber.
	targetHeight _ otherNumber.! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 7/22/2021 20:28:39'!
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

	tcx _ (centerX * txA11) + (centerY * txA12) + txA13.
	tcy _ (centerX * txA21) + (centerY * txA22) + txA23.
	self cCode: 'scale = sqrt(txA11*txA11 + txA21*txA21);'
		inSmalltalk: [scale _ (txA11 squared + txA21 squared) sqrt].
	trx _ radiusPointX * scale.
	try _ radiusPointY * scale.
	hops _ ((trx max: try) * (self cCode: 'fabs(sweepAngle)' inSmalltalk: [sweepAngle abs]) / hop) asInteger + 2.
	d _ sweepAngle / hops.

	edgeCounts _ otherWordArray.
	alphaMask _ anotherWordArray.
	contour _ aFloat32Array.
	angle _ startAngle.
	hops _ hops+1.
	1 to: hops do: [ :h |
		xp _ angle cos * trx.
		yp _ angle sin * try.
		x _ tthetaCos * xp - (tthetaSin * yp) + tcx.
		y _ tthetaSin * xp + (tthetaCos * yp) + tcy.
		spanLeft _ spanLeft min: x.
		spanTop _ spanTop min: y.
		spanRight _ spanRight max: x.
		spanBottom _ spanBottom max: y.
		self updateAlphasForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y.
		angle _ angle + d. ].! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 7/5/2021 15:54:16'!
cubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2 edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array

	self
		primitive: 'primCubicBezier'
		parameters: #(Float Float Float Float Float Float Float Float WordArray WordArray Float32Array).

	edgeCounts _ otherWordArray.
	alphaMask _ anotherWordArray.
	contour _ aFloat32Array.

	self pvt_cubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 7/5/2021 15:57:23'!
lineFromX: xFrom y: yFrom toX: xTo y: yTo edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array

	self
		primitive: 'primLine'
		parameters: #(Float Float Float Float WordArray WordArray Float32Array).

	edgeCounts _ otherWordArray.
	alphaMask _ anotherWordArray.
	contour _ aFloat32Array.
	self pvt_lineFromX: xFrom y: yFrom toX: xTo y: yTo.! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 7/5/2021 16:27:08'!
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

	txFrom _ (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom _ (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo _ (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo _ (xTo * txA21) + (yTo * txA22) + txA23.
	txControl1 _ (xControl1 * txA11) + (yControl1 * txA12) + txA13.
	tyControl1 _ (xControl1 * txA21) + (yControl1 * txA22) + txA23.
	txControl2 _ (xControl2 * txA11) + (yControl2 * txA12) + txA13.
	tyControl2 _ (xControl2 * txA21) + (yControl2 * txA22) + txA23.
	self cCode: '
		dx = fabs(txControl1-txFrom);
		dx2 = fabs(txTo-txControl2);
		dx3 = fabs(txControl2-txControl1);
		dy = fabs(tyControl1-tyFrom);
		dy2 = fabs(tyTo-tyControl2);
		dy3 = fabs(tyControl2-tyControl1);'
	inSmalltalk: [
		dx _ (txControl1-txFrom) abs.
		dx2 _ (txTo-txControl2) abs.
		dx3 _ (txControl2-txControl1) abs.
		dy _ (tyControl1-tyFrom) abs.
		dy2 _ (tyTo-tyControl2) abs.
		dy3 _ (tyControl2-tyControl1) abs ].
	dx _ (dx max: dx2) * 3 max: dx3 * 1.5.
	dy _ (dy max: dy2) * 3 max: dy3 * 1.5.
	hops _ ((dx max: dy) / hop) asInteger + 1.

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
	1 to: hops do: [ :h |
		oneLessT _ 1.0 - t.
		f1 _ oneLessT * oneLessT * oneLessT.
		f23 _ 3.0 * oneLessT * t.
		f2 _ f23 * oneLessT.
		f3 _ f23 * t.
		f4 _ t * t * t.
		x _ (f1 * txFrom) + (f2 * txControl1) + (f3 * txControl2) + (f4 * txTo).
		y _ (f1 * tyFrom) + (f2 * tyControl1) + (f3 * tyControl2) + (f4 * tyTo).
		self updateAlphasForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y.
		t _ t + increment. ].
	self updateAlphasForX: txTo y: tyTo.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 7/5/2021 16:29:12'!
pvt_lineFromX: xFrom y: yFrom toX: xTo y: yTo

	| txFrom tyFrom txTo tyTo dx dy hops incrementX incrementY x y |
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
	self var: #incrementX type: 'float'.
	self var: #incrementY type: 'float'.
	self var: #x type: 'float'.
	self var: #y type: 'float'.

	txFrom _ (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom _ (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo _ (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo _ (xTo * txA21) + (yTo * txA22) + txA23.
	dx _ txTo-txFrom.
	dy _ tyTo-tyFrom.
	incrementX _ dx.
	incrementY _ dy.
	self cCode: '
		dx = fabs(dx);
		dy = fabs(dy);'
	inSmalltalk: [
		dx _ dx abs.
		dy _ dy abs ].
	hops _ ((dx max: dy) / hop) asInteger + 1.
	incrementX _ incrementX / hops.
	incrementY _ incrementY / hops.

	spanLeft _ spanLeft min: (txFrom min: txTo).
	spanRight _ spanRight max: (txFrom max: txTo).
	spanTop _ spanTop min: (tyFrom min: tyTo).
	spanBottom _ spanBottom max: (tyFrom max: tyTo).

	x _ txFrom.
	y _ tyFrom.
	1 to: hops do: [ :h |
		self updateAlphasForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountAtX: x y: y ].
		self updateContourForX: x y: y.
		x _ x + incrementX.
		y _ y + incrementY ].
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

	txFrom _ (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom _ (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo _ (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo _ (xTo * txA21) + (yTo * txA22) + txA23.
	txControl _ (xControl * txA11) + (yControl * txA12) + txA13.
	tyControl _ (xControl * txA21) + (yControl * txA22) + txA23.
	self cCode: '
		dx = fabs(txTo-txFrom);
		dx2 = fabs(txControl-txFrom);
		dy = fabs(tyTo-tyFrom);
		dy2 = fabs(tyControl-tyFrom);'
	inSmalltalk: [
		dx _ (txTo-txFrom) abs.
		dx2 _ (txControl-txFrom) abs.
		dy _ (tyTo-tyFrom) abs.
		dy2 _ (tyControl-tyFrom) abs.].

	"If almost a vertical line, just draw a line. (Ignoring control point)"
	(dx < 1.0 and: [dx2 < 1.0]) ifTrue: [
		^self pvt_lineFromX: xFrom y: yFrom toX: xTo y: yTo ].

	"If almost an horizontal line, just draw a line. (Ignoring control point)"
	(dy < 1.0 and: [dy2 < 1.0]) ifTrue: [
		^self pvt_lineFromX: xFrom y: yFrom toX: xTo y: yTo ].

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
	fillA = 0.0 ifFalse: [
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
		dx _ x-x0. dy _ y-y0.
		self cCode: '
			length = sqrt(dx*dx + dy*dy);'
			inSmalltalk: [ length _ ((dx*dx) + (dy*dy)) sqrt ].
		correction _ hop / length.
		[
			increment _ increment / length * hop.
			t _ t0 + increment. oneLessT _ 1.0 - t.
			f1 _ oneLessT * oneLessT. f2 _ 2.0 * oneLessT * t. f3 _ t * t.
			x _ (f1 * txFrom) + (f2 * txControl) + (f3 * txTo).
			y _ (f1 * tyFrom) + (f2 * tyControl) + (f3 * tyTo).
			dx _ x-x0. dy _ y-y0.
			self cCode: '
				length = sqrt(dx*dx + dy*dy);'
				inSmalltalk: [ length _ ((dx*dx) + (dy*dy)) sqrt ].
			correction _ hop / length.
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

	edgeCounts _ otherWordArray.
	alphaMask _ anotherWordArray.
	contour _ aFloat32Array.
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
	t _ (y - auxStrokeWidthDilatedHalf + 1) asInteger.														"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	t < clipTop ifTrue: [ t _ clipTop ].
	b _ (y + auxStrokeWidthDilatedHalf) asInteger.
	b > clipBottom ifTrue: [ b _ clipBottom ].
	l _ (x - auxStrokeWidthDilatedHalf - subPixelDelta + 1) asInteger.														"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	l < clipLeft ifTrue: [ l _ clipLeft ].
	r _ (x + auxStrokeWidthDilatedHalf + subPixelDelta) asInteger.
	r > clipRight ifTrue: [ r _ clipRight ].

	t to: b do: [ :displayY |
		pixelIndex _ displayY * targetWidth + l -1.
		dy _ displayY - y.
		dySquared _ dy * dy.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			alphaWord _ alphaMask at: pixelIndex.
			alphaWord = 16r007F7F7F ifFalse: [
				redAlpha _ alphaWord bitAnd: 16r7F0000.
				greenAlpha _ alphaWord bitAnd: 16r7F00.
				blueAlpha _ alphaWord bitAnd: 16r7F.
				doUpdate _ false.
				dx _ displayX - x.
				"Red"
				dxp _ dx - subPixelDelta.
				distanceToAxisSquared _ dxp * dxp + dySquared.
				distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
					ifTrue: [
						distanceToAxisSquared <= auxStrokeWidthErodedHalfSquared
							ifTrue: [
								candidateAlpha _ 16r7F0000 ]
							ifFalse: [
								self cCode: '
										candidateAlpha = (uint32_t)((auxStrokeWidthDilatedHalf - (sqrt(distanceToAxisSquared))) * auxAntiAliasingWidthScaledInverse);
										candidateAlpha = candidateAlpha << 16;'
									inSmalltalk: [
										candidateAlpha _ (auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt * auxAntiAliasingWidthScaledInverse) asInteger.
										candidateAlpha _ candidateAlpha bitShift: 16 ]].
						candidateAlpha > redAlpha ifTrue: [
							doUpdate _ true.
							redAlpha _ candidateAlpha ]].
				"Green"
				distanceToAxisSquared _ dx * dx + dySquared.
				distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
					ifTrue: [
						distanceToAxisSquared <= auxStrokeWidthErodedHalfSquared
							ifTrue: [
								candidateAlpha _ 16r7F00 ]
							ifFalse: [
								self cCode: '
										candidateAlpha = (uint32_t)((auxStrokeWidthDilatedHalf - (sqrt(distanceToAxisSquared))) * auxAntiAliasingWidthScaledInverse);
										candidateAlpha = candidateAlpha << 8;'
									inSmalltalk: [
										candidateAlpha _ (auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt * auxAntiAliasingWidthScaledInverse) asInteger.
										candidateAlpha _ candidateAlpha bitShift: 8 ]].
						candidateAlpha > greenAlpha ifTrue: [
							doUpdate _ true.
							greenAlpha _ candidateAlpha ]].
				"Blue"
				dxp _ dx + subPixelDelta.
				distanceToAxisSquared _ dxp * dxp + dySquared.
				distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
					ifTrue: [
						distanceToAxisSquared <= auxStrokeWidthErodedHalfSquared
							ifTrue: [
								candidateAlpha _ 16r7F ]
							ifFalse: [
								self cCode: '
										candidateAlpha = (uint32_t)((auxStrokeWidthDilatedHalf - (sqrt(distanceToAxisSquared))) * auxAntiAliasingWidthScaledInverse);'
									inSmalltalk: [
										candidateAlpha _ (auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt * auxAntiAliasingWidthScaledInverse) asInteger ].
								].
						candidateAlpha > blueAlpha ifTrue: [
							doUpdate _ true.
							blueAlpha _ candidateAlpha ]].
				doUpdate ifTrue: [
					alphaWord _ (redAlpha bitOr: greenAlpha) bitOr: blueAlpha.
					alphaMask at: pixelIndex put: alphaWord ]]]]! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 7/5/2021 16:22:38'!
updateContourForX: x y: y
	"The Contour of the current morph is the pixel area affected, 	but without holes.
	For each y line where a pixel is affected, record the leftmost and rightmost affected pixels.
	Note: Only includes pen trajectory, but not stroke width."

	| thisYRounded |

	self var: #x type: 'float'.
	self var: #y type: 'float'.

	thisYRounded _ (y + 0.5) asInteger.
	(thisYRounded between: 0 and: targetHeight-1) ifTrue: [
		thisYRounded = prevYRounded ifFalse: [
			prevYRounded = 16r7FFFFFFF ifFalse: [
				contour at: prevYRounded * 2 put: leftAtThisY.
				contour at: prevYRounded * 2 + 1 put: rightAtThisY ].
			leftAtThisY _ contour at: thisYRounded * 2.
			rightAtThisY _ contour at: thisYRounded * 2 + 1.
			prevYRounded _ thisYRounded ].
		leftAtThisY _ leftAtThisY min: x.
		rightAtThisY _ rightAtThisY max: x ].! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 7/5/2021 16:42:59'!
updateContourLastLine
	self
		primitive: 'primUpdateContourLastLine'
		parameters: #().

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

	thisYTruncated _ y asInteger.	"truncated, both in C and Smalltalk"
	thisYTruncated = prevYTruncated ifTrue: [
		^ 0].
	(thisYTruncated between: clipTop-1 and: clipBottom) ifFalse: [
		^ 0 ].
	prevYTruncated = 16r7FFFFFFF ifTrue: [ 
		prevYTruncated _ thisYTruncated.
		^ 0 ].

	thisYTruncated > prevYTruncated
		ifTrue: [
			pixelY _ thisYTruncated.
			redIncrement _ 16r010000.
			greenIncrement _ 16r0100.
			blueIncrement _ 16r01 ]
		ifFalse: [
			pixelY _ prevYTruncated.
			redIncrement _ 16rFF0000.
			greenIncrement _ 16rFF00.
			blueIncrement _ 16rFF ].
	prevYTruncated _ thisYTruncated.

	"All edge count at the left of the clipRect are added there (at the left of the clipRect).
	The effect is the same, and we need to clean up less stuff afterwards.
	More important, it avoids trying to acess pixels outside our form, i.e. invalid array acesses."
	pixelIndexBase _ pixelY * targetWidth.
	redOffset _ (x+subPixelDelta+1) asInteger max: clipLeft.						"take the next red subpixel center to the right of x"
	greenOffset _ (x+1) asInteger max: clipLeft.											"take the next green subpixel center to the right of x"
	blueOffset _ (x-subPixelDelta+1) asInteger max: clipLeft.		"take the next blue subpixel center to the right of x"
	redPixelIndex _ pixelIndexBase + redOffset.
	greenPixelIndex _ pixelIndexBase + greenOffset.
	bluePixelIndex _ pixelIndexBase + blueOffset.

	"Three possible cases here: RGB in one word (pixel); RG in one, and G in another; R in one, GB in another"
	redPixelIndex = bluePixelIndex
		ifTrue: [						"First case: RGB in the same word"
			redOffset <= clipRight ifTrue: [
				countWord _ edgeCounts at: redPixelIndex.
				redCount _ (countWord + redIncrement) bitAnd: 16rFF0000.
				greenCount _ (countWord + greenIncrement) bitAnd: 16rFF00.
				blueCount _ (countWord + blueIncrement) bitAnd: 16rFF.
				countWord _ (redCount bitOr: greenCount) bitOr: blueCount.
				edgeCounts at: redPixelIndex put: countWord ].
		]
		ifFalse: [
			redPixelIndex = greenPixelIndex
				ifTrue: [				"Second case: RG in one word, B in previous"
					redOffset <= clipRight ifTrue: [
						countWord _ edgeCounts at: redPixelIndex.
						redCount _ (countWord + redIncrement) bitAnd: 16rFF0000.
						greenCount _ (countWord + greenIncrement) bitAnd: 16rFF00.
						rest _ countWord bitAnd: 16rFF.
						countWord _ (redCount bitOr: greenCount) bitOr: rest.
						edgeCounts at: redPixelIndex put: countWord ].

					blueOffset <= clipRight ifTrue: [
						countWord _ edgeCounts at: bluePixelIndex.
						rest _ countWord bitAnd: 16rFFFF00.
						blueCount _ (countWord + blueIncrement) bitAnd: 16rFF.
						countWord _ rest bitOr: blueCount.
						edgeCounts at: bluePixelIndex put: countWord ].
				]
				ifFalse: [			"Third case: R in one word, GB in the previous"
					redOffset <= clipRight ifTrue: [
						countWord _ edgeCounts at: redPixelIndex.
						redCount _ (countWord + redIncrement) bitAnd: 16rFF0000.
						rest _ countWord bitAnd: 16rFFFF.
						countWord _ redCount bitOr: rest.
						edgeCounts at: redPixelIndex put: countWord ].

					blueOffset <= clipRight ifTrue: [
						countWord _ edgeCounts at: bluePixelIndex.
						rest _ countWord bitAnd: 16rFF0000.
						greenCount _ (countWord + greenIncrement) bitAnd: 16rFF00.
						blueCount _ (countWord + blueIncrement) bitAnd: 16rFF.
						countWord _ (rest bitOr: greenCount) bitOr: blueCount.
						edgeCounts at: bluePixelIndex put: countWord ].
				]
		].! !

!VectorEnginePlugin methodsFor: 'text and multi paths' stamp: 'jmv 7/22/2021 23:25:33'!
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

	edgeCounts _ otherWordArray.
	alphaMask _ anotherWordArray.
	txA11 _ txA11 * sx.
	txA12 _ txA12 * sy.
	txA21 _ txA21 * sx.
	txA22 _ txA22 * sy.

	nextGlyphX _ destX / sx.
	nextGlyphY _ destY / sy.
	startIndex-1 to: stopIndex-1 do: [ :charIndex |
		self cCode:
				'iso8859s15 = aString[charIndex];'
			inSmalltalk: [
				iso8859s15 _ (aString at: charIndex+1) iso8859s15Code. ].
		aBoolean ifTrue: [
			iso8859s15 = 95 ifTrue: [ iso8859s15 _ 28 ].												"If underscore, use left arrow"
			iso8859s15 = 94 ifTrue: [ iso8859s15 _ 30 ].												"If caret, use up arrow"
			].
		i _ contourDataIndexes at: iso8859s15.
		i _ i - 1.
		advanceWidth _ contourData at: i. i _ i + 5.
		numContours _ (contourData at: i) asInteger. i _ i + 1.
		1 to: numContours do: [ :idx |
			numBeziers _ (contourData at: i) asInteger. i _ i + 1.
			ttMoveToX _ (contourData at: i) + nextGlyphX. i _ i + 1.
			ttMoveToY _ (contourData at: i) + nextGlyphY. i _ i + 1.
			startX _ (ttMoveToX * txA11) + (ttMoveToY * txA12) + txA13.
			startY _ (ttMoveToX * txA21) + (ttMoveToY * txA22) + txA23.
			contourStartX _ startX.
			contourStartY _ startY.
			self newTrajectoryFragment.
			1 to: numBeziers do: [ :idx2 |
				ttEndX _ contourData at: i. i _ i + 1.
				ttEndY _ contourData at: i. i _ i + 1.
				ttControlX _ contourData at: i. i _ i + 1.
				ttControlY _ contourData at: i. i _ i + 1.
				endX _ (ttEndX * txA11) + (ttEndY * txA12) + startX.
				endY _ (ttEndX * txA21) + (ttEndY * txA22) + startY.
				controlX _ (ttControlX * txA11) + (ttControlY * txA12) + startX.
				controlY _ (ttControlX * txA21) + (ttControlY * txA22) + startY.
				"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
				xMinEnd _ startX min: endX.
				xMaxEnd _ startX max: endX.
				yMinEnd _ startY min: endY.
				yMaxEnd _ startY max: endY.
				spanLeft _ spanLeft min: (xMinEnd min: (xMinEnd+controlX) / 2.0).
				spanRight _ spanRight max: (xMaxEnd max: (xMaxEnd+controlX) / 2.0).
				spanTop _ spanTop min: (yMinEnd min: (yMinEnd+controlY) / 2.0).
				spanBottom _ spanBottom max: (yMaxEnd max: (yMaxEnd+controlY) / 2.0).
				"Compute Quadratic Bezier Curve,"
				"Case t = 0.0"
				x _ startX.
				y _ startY.
				self updateAlphasForX: x y: y.
				self updateEdgeCountAtX: x y: y.
				"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
				self cCode: 'dx = fabs(endX-startX); dy = fabs(endY-startY);'
				inSmalltalk: [ dx _ (endX-startX) abs. dy _ (endY-startY) abs ].
				increment _ 0.5 / (dx max: dy) min: 0.5.
				t _ 0.0.
				[
					t0 _ t. x0 _ x. y0 _ y.
					"Compute next point"
					t _ t0 + increment. oneLessT _ 1.0 - t.
					f1 _ oneLessT * oneLessT. f2 _ 2.0 * oneLessT * t. f3 _ t * t.
					x _ (f1 * startX) + (f2 * controlX) + (f3 * endX).
					y _ (f1 * startY) + (f2 * controlY) + (f3 * endY).
					"Now adjust the increment to aim at the required hop length, and recompute next point."
					dx _ x-x0. dy _ y-y0.
					self cCode: '
						length = sqrt(dx*dx + dy*dy);'
						inSmalltalk: [ length _ ((dx*dx) + (dy*dy)) sqrt ].
					correction _ hop / length.
					[
						increment _ increment / length * hop.
						t _ t0 + increment. oneLessT _ 1.0 - t.
						f1 _ oneLessT * oneLessT. f2 _ 2.0 * oneLessT * t. f3 _ t * t.
						x _ (f1 * startX) + (f2 * controlX) + (f3 * endX).
						y _ (f1 * startY) + (f2 * controlY) + (f3 * endY).
						dx _ x-x0. dy _ y-y0.
						self cCode: '
							length = sqrt(dx*dx + dy*dy);'
							inSmalltalk: [ length _ ((dx*dx) + (dy*dy)) sqrt ].
						correction _ hop / length.
						correction < 1.0]  whileTrue.		   "Keep adjusting if hop would be too big"
					t < 1.0 ]
				whileTrue: [
					self updateAlphasForX: x y: y.
					self updateEdgeCountAtX: x y: y ].
				"Note: For TrueType font definitions, we assume that all contour fragments start exactly where the previous ends.
				This means that the end point is only added for the last fragment of the contour, and not for each one of them."
				startX _ endX.
				startY _ endY.	
			].
			self updateAlphasForX: endX y: endY.
			self updateEdgeCountAtX: endX y: endY.
			"Similar effect to ensureClosePath in #finishPath:,
			but assume the TrueType definition is essentially right, and there might only be a rounding error.
			So, don't draw a line, but just (possibly) correct edgeCounts. The possibility of rounding error is most likely zero.
			Anyway, this is cheap."
			self updateEdgeCountAtX: contourStartX y: contourStartY.
		].
		nextGlyphX _ nextGlyphX + advanceWidth.
	].
	txA11 _ txA11 / sx.
	txA12 _ txA12 / sy.
	txA21 _ txA21 / sx.
	txA22 _ txA22 / sy.
	answer _ nextGlyphX * sx.
	^answer asOop: Float! !

!VectorEnginePlugin methodsFor: 'text and multi paths' stamp: 'jmv 7/22/2021 23:29:19'!
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

	edgeCounts _ otherWordArray.
	alphaMask _ anotherWordArray.
	txA11 _ txA11 * sx.
	txA12 _ txA12 * sy.
	txA21 _ txA21 * sx.
	txA22 _ txA22 * sy.

	nextGlyphX _ destX / sx.
	nextGlyphY _ destY / sy.
	startIndex-1 to: stopIndex-1 do: [ :utf32Index |
		utf32 _ aWordArray at: utf32Index.
		aBoolean ifTrue: [
			utf32 = 95 ifTrue: [ utf32 _ 8592 ].												"If underscore, use left arrow"
			utf32 = 94 ifTrue: [ utf32 _ 8593 ].												"If caret, use up arrow"
			].
		utf32 <= 16r7F
			ifTrue: [
				utf8Byte _ utf32.
				i _ contourDataIndexes at: utf8Byte ]
			ifFalse: [ utf32 <= 16r7FF
			ifTrue: [
				utf8Byte _ (utf32 bitShift: -6) bitOr: 2r11000000.
				i _ contourDataIndexes at: utf8Byte.
				utf8Byte _ (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
				i _ contourDataIndexes at: utf8Byte - i ]
			ifFalse: [ utf32 <= 16rFFFF
			ifTrue: [
				utf8Byte _ (utf32 bitShift: -12) bitOr: 2r11100000.
				i _ contourDataIndexes at: utf8Byte.
				utf8Byte _ ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
				i _ contourDataIndexes at: utf8Byte - i.
				utf8Byte _ (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
				i _ contourDataIndexes at: utf8Byte - i ]
			ifFalse: [
				utf8Byte _ (utf32 bitShift: -18) bitOr: 2r11110000.
				i _ contourDataIndexes at: utf8Byte.
				utf8Byte _ ((utf32 bitShift: -12) bitAnd: 2r00111111) bitOr: 2r10000000.
				i _ contourDataIndexes at: utf8Byte - i.
				utf8Byte _ ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
				i _ contourDataIndexes at: utf8Byte - i.
				utf8Byte _ (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
				i _ contourDataIndexes at: utf8Byte - i ]]].
		i _ i - 1.
		advanceWidth _ contourData at: i. i _ i + 5.
		numContours _ (contourData at: i) asInteger. i _ i + 1.
		1 to: numContours do: [ :idx |
			numBeziers _ (contourData at: i) asInteger. i _ i + 1.
			ttMoveToX _ (contourData at: i) + nextGlyphX. i _ i + 1.
			ttMoveToY _ (contourData at: i) + nextGlyphY. i _ i + 1.
			startX _ (ttMoveToX * txA11) + (ttMoveToY * txA12) + txA13.
			startY _ (ttMoveToX * txA21) + (ttMoveToY * txA22) + txA23.
			contourStartX _ startX.
			contourStartY _ startY.
			self newTrajectoryFragment.
			1 to: numBeziers do: [ :idx2 |
				ttEndX _ contourData at: i. i _ i + 1.
				ttEndY _ contourData at: i. i _ i + 1.
				ttControlX _ contourData at: i. i _ i + 1.
				ttControlY _ contourData at: i. i _ i + 1.
				endX _ (ttEndX * txA11) + (ttEndY * txA12) + startX.
				endY _ (ttEndX * txA21) + (ttEndY * txA22) + startY.
				controlX _ (ttControlX * txA11) + (ttControlY * txA12) + startX.
				controlY _ (ttControlX * txA21) + (ttControlY * txA22) + startY.
				"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
				xMinEnd _ startX min: endX.
				xMaxEnd _ startX max: endX.
				yMinEnd _ startY min: endY.
				yMaxEnd _ startY max: endY.
				spanLeft _ spanLeft min: (xMinEnd min: (xMinEnd+controlX) / 2.0).
				spanRight _ spanRight max: (xMaxEnd max: (xMaxEnd+controlX) / 2.0).
				spanTop _ spanTop min: (yMinEnd min: (yMinEnd+controlY) / 2.0).
				spanBottom _ spanBottom max: (yMaxEnd max: (yMaxEnd+controlY) / 2.0).
				"Compute Quadratic Bezier Curve,"
				"Case t = 0.0"
				x _ startX.
				y _ startY.
				self updateAlphasForX: x y: y.
				self updateEdgeCountAtX: x y: y.
				"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
				self cCode: 'dx = fabs(endX-startX); dy = fabs(endY-startY);'
				inSmalltalk: [ dx _ (endX-startX) abs. dy _ (endY-startY) abs ].
				increment _ 0.5 / (dx max: dy) min: 0.5.
				t _ 0.0.
				[
					t0 _ t. x0 _ x. y0 _ y.
					"Compute next point. Only C version to avoid hitting a limit in Smalltalk compiler."
					self cCode: '
						t = t0 + increment; oneLessT = 1.0 - t;
						f1 = oneLessT * oneLessT; f2 = 2.0 * oneLessT * t; f3 = t * t;
						x = (f1 * startX) + (f2 * controlX) + (f3 * endX);
						y = (f1 * startY) + (f2 * controlY) + (f3 * endY);
						dx = x-x0; dy = y-y0;
						length = sqrt(dx*dx + dy*dy);'
						inSmalltalk: [ length _ ((dx*dx) + (dy*dy)) sqrt ].
					correction _ hop / length.
					"Now adjust the increment to aim at the required hop length, and recompute next point."
					[
						increment _ increment / length * hop.
						t _ t0 + increment. oneLessT _ 1.0 - t.
						f1 _ oneLessT * oneLessT. f2 _ 2.0 * oneLessT * t. f3 _ t * t.
						x _ (f1 * startX) + (f2 * controlX) + (f3 * endX).
						y _ (f1 * startY) + (f2 * controlY) + (f3 * endY).
						dx _ x-x0. dy _ y-y0.
						self cCode: '
							length = sqrt(dx*dx + dy*dy);'
							inSmalltalk: [ length _ ((dx*dx) + (dy*dy)) sqrt ].
						correction _ hop / length.
						correction < 1.0]  whileTrue.		   "Keep adjusting if hop would be too big"
					t < 1.0 ]
				whileTrue: [
					self updateAlphasForX: x y: y.
					self updateEdgeCountAtX: x y: y. ].
				"Note: For TrueType font definitions, we assume that all contour fragments start exactly where the previous ends.
				This means that the end point is only added for the last fragment of the contour, and not for each one of them."
				startX _ endX.
				startY _ endY.	
			].
			self updateAlphasForX: endX y: endY.
			self updateEdgeCountAtX: endX y: endY.
			"Similar effect to ensureClosePath in #finishPath:,
			but assume the TrueType definition is essentially right, and there might only be a rounding error.
			So, don't draw a line, but just (possibly) correct edgeCounts. The possibility of rounding error is most likely zero.
			Anyway, this is cheap."
			self updateEdgeCountAtX: contourStartX y: contourStartY.
		].
		nextGlyphX _ nextGlyphX + advanceWidth.
	].
	txA11 _ txA11 / sx.
	txA12 _ txA12 / sy.
	txA21 _ txA21 / sx.
	txA22 _ txA22 / sy.
	answer _ nextGlyphX * sx.
	^answer asOop: Float! !

!VectorEnginePlugin methodsFor: 'text and multi paths' stamp: 'jmv 7/22/2021 23:28:26'!
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

	edgeCounts _ otherWordArray.
	alphaMask _ anotherWordArray.
	txA11 _ txA11 * sx.
	txA12 _ txA12 * sy.
	txA21 _ txA21 * sx.
	txA22 _ txA22 * sy.

	nextGlyphX _ destX / sx.
	nextGlyphY _ destY / sy.
	baseIndex _ 0.
	byteStartIndex-1 to: byteStopIndex-1 do: [ :byteIndex |
		byte _ aByteArray at: byteIndex.
		i _ contourDataIndexes at: baseIndex + byte.
		aBoolean ifTrue: [
			byte = 95 ifTrue: [
				i _ contourDataIndexes at: 226.
				i _ contourDataIndexes at: 134 - i.
				i _ contourDataIndexes at: 144 - i ].
			byte = 94 ifTrue: [ 
				i _ contourDataIndexes at: 226.
				i _ contourDataIndexes at: 134 - i.
				i _ contourDataIndexes at: 145 - i ]].
		i < 0
			ifTrue: [ baseIndex _ 0 - i ]
			ifFalse: [
				i _ i - 1.
				advanceWidth _ contourData at: i. i _ i + 5.
				numContours _ (contourData at: i) asInteger. i _ i + 1.
				1 to: numContours do: [ :idx |
					numBeziers _ (contourData at: i) asInteger. i _ i + 1.
					ttMoveToX _ (contourData at: i) + nextGlyphX. i _ i + 1.
					ttMoveToY _ (contourData at: i) + nextGlyphY. i _ i + 1.
					startX _ (ttMoveToX * txA11) + (ttMoveToY * txA12) + txA13.
					startY _ (ttMoveToX * txA21) + (ttMoveToY * txA22) + txA23.
					contourStartX _ startX.
					contourStartY _ startY.
					self newTrajectoryFragment.
					1 to: numBeziers do: [ :idx2 |
						ttEndX _ contourData at: i. i _ i + 1.
						ttEndY _ contourData at: i. i _ i + 1.
						ttControlX _ contourData at: i. i _ i + 1.
						ttControlY _ contourData at: i. i _ i + 1.
						endX _ (ttEndX * txA11) + (ttEndY * txA12) + startX.
						endY _ (ttEndX * txA21) + (ttEndY * txA22) + startY.
						controlX _ (ttControlX * txA11) + (ttControlY * txA12) + startX.
						controlY _ (ttControlX * txA21) + (ttControlY * txA22) + startY.
						"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
						xMinEnd _ startX min: endX.
						xMaxEnd _ startX max: endX.
						yMinEnd _ startY min: endY.
						yMaxEnd _ startY max: endY.
						spanLeft _ spanLeft min: (xMinEnd min: (xMinEnd+controlX) / 2.0).
						spanRight _ spanRight max: (xMaxEnd max: (xMaxEnd+controlX) / 2.0).
						spanTop _ spanTop min: (yMinEnd min: (yMinEnd+controlY) / 2.0).
						spanBottom _ spanBottom max: (yMaxEnd max: (yMaxEnd+controlY) / 2.0).
						"Case t = 0.0"
						x _ startX.
						y _ startY.
						self updateAlphasForX: x y: y.
						self updateEdgeCountAtX: x y: y.
						"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
						self cCode: 'dx = fabs(endX-startX); dy = fabs(endY-startY);'
						inSmalltalk: [ dx _ (endX-startX) abs. dy _ (endY-startY) abs ].
						increment _ 0.5 / (dx max: dy) min: 0.5.
						"Compute Quadratic Bezier Curve,"
						t _ 0.0.
						[
							t0 _ t. x0 _ x. y0 _ y.
							"Compute next point"
							t _ t0 + increment. oneLessT _ 1.0 - t.
							f1 _ oneLessT * oneLessT. f2 _ 2.0 * oneLessT * t. f3 _ t * t.
							x _ (f1 * startX) + (f2 * controlX) + (f3 * endX).
							y _ (f1 * startY) + (f2 * controlY) + (f3 * endY).
							"Now adjust the increment to aim at the required hop length, and recompute next point."
							dx _ x-x0. dy _ y-y0.
							self cCode: '
								length = sqrt(dx*dx + dy*dy);'
								inSmalltalk: [ length _ ((dx*dx) + (dy*dy)) sqrt ].
							correction _ hop / length.
							[
								increment _ increment / length * hop.
								t _ t0 + increment. oneLessT _ 1.0 - t.
								f1 _ oneLessT * oneLessT. f2 _ 2.0 * oneLessT * t. f3 _ t * t.
								x _ (f1 * startX) + (f2 * controlX) + (f3 * endX).
								y _ (f1 * startY) + (f2 * controlY) + (f3 * endY).
								dx _ x-x0. dy _ y-y0.
								self cCode: '
									length = sqrt(dx*dx + dy*dy);'
									inSmalltalk: [ length _ ((dx*dx) + (dy*dy)) sqrt ].
								correction _ hop / length.
								correction < 1.0]  whileTrue.		   "Keep adjusting if hop would be too big"
							t < 1.0 ]
						whileTrue: [
							self updateAlphasForX: x y: y.
							self updateEdgeCountAtX: x y: y ].
						"Note: For TrueType font definitions, we assume that all contour fragments start exactly where the previous ends.
						This means that the end point is only added for the last fragment of the contour, and not for each one of them."
						startX _ endX.
						startY _ endY.	
					].
					self updateAlphasForX: endX y: endY.
					self updateEdgeCountAtX: endX y: endY.
					"Similar effect to ensureClosePath in #finishPath:,
					but assume the TrueType definition is essentially right, and there might only be a rounding error.
					So, don't draw a line, but just (possibly) correct edgeCounts. The possibility of rounding error is most likely zero.
					Anyway, this is cheap."
					self updateEdgeCountAtX: contourStartX y: contourStartY.
				].
				nextGlyphX _ nextGlyphX + advanceWidth.
				baseIndex _ 0.
			]
	].
	txA11 _ txA11 / sx.
	txA12 _ txA12 / sy.
	txA21 _ txA21 / sx.
	txA22 _ txA22 / sy.
	answer _ nextGlyphX * sx.
	^answer asOop: Float! !

!VectorEnginePlugin methodsFor: 'text and multi paths' stamp: 'jmv 7/5/2021 16:33:31'!
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

	edgeCounts _ otherWordArray.
	alphaMask _ anotherWordArray.
	contour _ otherFloat32Array.
	i _ 0.
	[ i < size ] whileTrue: [
		commandType _ (aFloat32Array at: i) asInteger. i _ i + 1.
		commandType caseOf: {
			[0] -> [
				i+1 < size ifFalse: [ ^self "failure" ].
				startX _ aFloat32Array at: i. i _ i + 1.
				startY _ aFloat32Array at: i. i _ i + 1.
				self newTrajectoryFragment.
				].
			[1] -> [
				i+1 < size ifFalse: [ ^self "failure" ].
				endX _ aFloat32Array at: i. i _ i + 1.
				endY _ aFloat32Array at: i. i _ i + 1.
				self pvt_lineFromX: startX y: startY toX: endX y: endY.
				startX _ endX.
				startY _ endY.
				].
			[2] -> [
				i+3 < size ifFalse: [ ^self "failure" ].
				endX _ aFloat32Array at: i. i _ i + 1.
				endY _ aFloat32Array at: i. i _ i + 1.
				control1X _ aFloat32Array at: i. i _ i + 1.
				control1Y _ aFloat32Array at: i. i _ i + 1.
				self pvt_quadraticBezierFromX: startX y: startY
					toX: endX y: endY
					controlX: control1X y: control1Y.
				startX _ endX.
				startY _ endY.
				].
			[3] -> [
				i+5 < size ifFalse: [ ^self "failure" ].
				endX _ aFloat32Array at: i. i _ i + 1.
				endY _ aFloat32Array at: i. i _ i + 1.
				control1X _ aFloat32Array at: i. i _ i + 1.
				control1Y _ aFloat32Array at: i. i _ i + 1.
				control2X _ aFloat32Array at: i. i _ i + 1.
				control2Y _ aFloat32Array at: i. i _ i + 1.
				self pvt_cubicBezierFromX: startX y: startY
					toX: endX y: endY
					control1X: control1X y: control1Y
					control2X: control2X y: control2Y.
				startX _ endX.
				startY _ endY.
				]}
		otherwise: [ ^self "failure"]].! !

!VectorEnginePlugin methodsFor: 'blend stroke and fill colors' stamp: 'jmv 6/28/2021 17:18:03'!
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
	antiAliasRedAlphaBits _ antiAliasAlphasWord bitAnd: 16r7F0000.
	antiAliasGreenAlphaBits _ antiAliasAlphasWord bitAnd: 16r7F00.
	antiAliasBlueAlphaBits _ antiAliasAlphasWord bitAnd: 16r7F.
	isRedInside ifTrue: [ antiAliasRedAlphaBits _ 16r7F0000 - antiAliasRedAlphaBits ].
	isGreenInside ifTrue: [ antiAliasGreenAlphaBits _ 16r7F00 - antiAliasGreenAlphaBits ].
	isBlueInside ifTrue: [ antiAliasBlueAlphaBits _ 16r7F - antiAliasBlueAlphaBits ].
	antiAliasRedAlpha _ antiAliasRedAlphaBits * (1.0 / (127.0 * 256 * 256)).
	antiAliasGreenAlpha _ antiAliasGreenAlphaBits * (1.0 / (127.0 * 256)).
	antiAliasBlueAlpha _ antiAliasBlueAlphaBits * (1.0 / 127.0).
	alphaR _ antiAliasRedAlpha * fillA.
	alphaG _ antiAliasGreenAlpha * fillA.
	alphaB _ antiAliasBlueAlpha * fillA.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			morphIdWord _ morphIds at: pixelIndex.
			clippingAntiAliasBits _ morphIdWord bitAnd: 16r7F.
			self cCode: '
					antiAliasGreenAlphaBitsShifted = antiAliasGreenAlphaBits >> 8;'
				inSmalltalk: [
					antiAliasGreenAlphaBitsShifted _ antiAliasGreenAlphaBits bitShift: -8 ].
			antiAliasGreenAlphaBitsShifted > clippingAntiAliasBits ifTrue: [
				clippingAntiAliasBits _ antiAliasGreenAlphaBitsShifted]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					morphIdWord _ morphIds at: pixelIndex.
					clippingAntiAliasBits _ morphIdWord bitAnd: 16r7F.
					clippingAntiAlias _ clippingAntiAliasBits * (1.0 / 127.0).
					alphaR _ alphaR * clippingAntiAlias.
					alphaG _ alphaG * clippingAntiAlias.
					alphaB _ alphaB * clippingAntiAlias ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAliasBits _ 0]].

	alphaR + alphaG + alphaB = 0.0 ifFalse: [
		targetWord _ targetBits at: pixelIndex.
		resultAlphaBits _ targetWord bitAnd: 16rFF000000.
		resultRBits _ targetWord bitAnd: 16rFF0000.
		resultGBits _ targetWord bitAnd: 16rFF00.
		resultBBits _ targetWord bitAnd: 16rFF.
		targetAlpha _ resultAlphaBits * ( 1.0 / (255.0 * 256 * 256 * 256)).

		"These if are not really needed. just ignore them if we use simd instructions."
		alphaR = 0.0 ifFalse: [
			unAlphaR _ 1.0 - alphaR.
			resultAlphaR _ alphaR + (unAlphaR * targetAlpha).
			self cCode: '
					resultR = (alphaR * fillR) + ((unAlphaR * (resultRBits >> 16)) * targetAlpha);
					resultRBits = (uint32_t)(resultR / resultAlphaR + 0.5) << 16;'
				inSmalltalk: [
					resultR _ alphaR * fillR + (unAlphaR * (resultRBits bitShift: -16) * targetAlpha).
					resultRBits _ (resultR / resultAlphaR + 0.5) asInteger bitShift: 16 ]].

		alphaG = 0.0 ifFalse: [
			unAlphaG _ 1.0 - alphaG.
			resultAlphaG _ alphaG + (unAlphaG * targetAlpha).
			self cCode: '
					resultG = (alphaG * fillG) + ((unAlphaG * (resultGBits >> 8)) * targetAlpha);
					resultGBits = (uint32_t)(resultG / resultAlphaG + 0.5) << 8;
					resultAlphaBits = (uint32_t)(resultAlphaG * 255.0 + 0.5) << 24;'
				inSmalltalk: [
					resultG _ alphaG * fillG + (unAlphaG * (resultGBits bitShift: -8) * targetAlpha).
					resultGBits _ (resultG / resultAlphaG + 0.5) asInteger bitShift: 8.
					resultAlphaBits _ (resultAlphaG * 255.0 + 0.5) asInteger bitShift: 24 ]].

		alphaB = 0.0 ifFalse: [
			unAlphaB _ 1.0 - alphaB.
			resultAlphaB _ alphaB + (unAlphaB * targetAlpha).
			self cCode: '
					resultB = (alphaB * fillB) + ((unAlphaB * resultBBits) * targetAlpha);
					resultBBits = (uint32_t)(resultB / resultAlphaB + 0.5);'
				inSmalltalk: [
					resultB _ alphaB * fillB + (unAlphaB * resultBBits * targetAlpha).
					resultBBits _ (resultB / resultAlphaB + 0.5) asInteger ]].

		targetWord _ ((resultAlphaBits bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		targetBits at: pixelIndex put: targetWord.

		currentMorphId = 0 ifFalse: [
			morphIdWord _ currentMorphId<<8 + clippingAntiAliasBits.
			morphIds at: pixelIndex put: morphIdWord ]].
! !

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

	targetBits _ aBitmap.
	morphIds _ aWordArray.
	edgeCounts _ otherWordArray.
	alphaMask _ anotherWordArray.
	t to: b do: [ :displayY |
		edgesUpToThisPixelR _ 0.
		edgesUpToThisPixelG _ 0.
		edgesUpToThisPixelB _ 0.
		pixelIndex _ displayY * targetWidth + l - 1.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			edgesThisPixelWord _ edgeCounts at: pixelIndex.
			edgesThisPixelWord = 0 ifFalse: [
				edgeCounts at: pixelIndex put: 0 ].

			self cCode: '
					edgesThisPixelR = (uint32_t) (edgesThisPixelWord & 0xFF0000) >> 16;
					edgesThisPixelG = (uint32_t) (edgesThisPixelWord & 0xFF00) >> 8;
					edgesThisPixelB = (uint32_t) (edgesThisPixelWord & 0xFF);'
				inSmalltalk: [
					edgesThisPixelR _ (edgesThisPixelWord bitAnd: 16rFF0000) bitShift: -16.
					edgesThisPixelG _ (edgesThisPixelWord bitAnd: 16rFF00)bitShift: -8.
					edgesThisPixelB _ edgesThisPixelWord bitAnd: 16rFF.
					"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
					edgesThisPixelR > 127 ifTrue: [ edgesThisPixelR _ edgesThisPixelR - 256 ].
					edgesThisPixelG > 127 ifTrue: [ edgesThisPixelG _ edgesThisPixelG - 256 ].
					edgesThisPixelB > 127 ifTrue: [ edgesThisPixelB _ edgesThisPixelB - 256 ]].

			edgesUpToThisPixelR _ edgesUpToThisPixelR + edgesThisPixelR.
			edgesUpToThisPixelG _ edgesUpToThisPixelG + edgesThisPixelG.
			edgesUpToThisPixelB _ edgesUpToThisPixelB + edgesThisPixelB.

			"In C, integers already behave like booleans"
			self cCode: '
					isRedInside = edgesUpToThisPixelR;
					isGreenInside = edgesUpToThisPixelG;
					isBlueInside = edgesUpToThisPixelB;'
				inSmalltalk: [
					isRedInside _ edgesUpToThisPixelR ~= 0.
					isGreenInside _ edgesUpToThisPixelG ~= 0.
					isBlueInside _ edgesUpToThisPixelB ~= 0 ].

			antiAliasAlphasWord _ alphaMask at: pixelIndex.
			antiAliasAlphasWord ~= 16r00000000 ifTrue: [
				alphaMask at: pixelIndex put: 0 ].
			(antiAliasAlphasWord ~= 16r00000000 or: [
				isRedInside or: [ isGreenInside or: [ isBlueInside ]]]) ifTrue: [
				self blendFillOnlyAt: pixelIndex
					redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
					antiAliasAlphasWord: antiAliasAlphasWord ]]].! !

!VectorEnginePlugin methodsFor: 'blend stroke and fill colors' stamp: 'jmv 6/28/2021 17:19:51'!
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

	antiAliasRedAlphaBits _ antiAliasAlphasWord bitAnd: 16r7F0000.
	antiAliasGreenAlphaBits _ antiAliasAlphasWord bitAnd: 16r7F00.
	antiAliasBlueAlphaBits _ antiAliasAlphasWord bitAnd: 16r7F.
	antiAliasRedAlpha _ antiAliasRedAlphaBits * (1.0 / (127.0 * 256 * 256)).
	antiAliasGreenAlpha _ antiAliasGreenAlphaBits * (1.0 / (127.0 * 256)).
	antiAliasBlueAlpha _ antiAliasBlueAlphaBits * (1.0 / 127.0).

	isRedInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaR _ antiAliasRedAlpha * strokeA + ((1.0-antiAliasRedAlpha) * fillA).
			foreR _ antiAliasRedAlpha * strokeR + ((1.0-antiAliasRedAlpha) * fillR) ]
		ifFalse: [							"Blend stroke over background"
			alphaR _ antiAliasRedAlpha * strokeA.
			foreR _ strokeR ].

	isGreenInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaG _ antiAliasGreenAlpha * strokeA + ((1.0-antiAliasGreenAlpha) * fillA).
			foreG _ antiAliasGreenAlpha * strokeG + ((1.0-antiAliasGreenAlpha) * fillG) ]
		ifFalse: [							"Blend stroke over background"
			alphaG _ antiAliasGreenAlpha * strokeA.
			foreG _ strokeG ].

	isBlueInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaB _ antiAliasBlueAlpha * strokeA + ((1.0-antiAliasBlueAlpha) * fillA).
			foreB _ antiAliasBlueAlpha * strokeB + ((1.0-antiAliasBlueAlpha) * fillB) ]
		ifFalse: [							"Blend stroke over background"
			alphaB _ antiAliasBlueAlpha * strokeA.
			foreB _ strokeB ].

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			isGreenInside
				ifTrue: [clippingAntiAliasBits _ 16r7F]
				ifFalse: [
					morphIdWord _ morphIds at: pixelIndex.
					clippingAntiAliasBits _ morphIdWord bitAnd: 16r7F.
					self cCode: '
							antiAliasGreenAlphaBitsShifted = antiAliasGreenAlphaBits >> 8;'
						inSmalltalk: [
							antiAliasGreenAlphaBitsShifted _ antiAliasGreenAlphaBits bitShift: -8 ].
					antiAliasGreenAlphaBitsShifted > clippingAntiAliasBits ifTrue: [
						clippingAntiAliasBits _ antiAliasGreenAlphaBitsShifted ]]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					morphIdWord _ morphIds at: pixelIndex.
					clippingAntiAliasBits _ morphIdWord bitAnd: 16r7F.
					clippingAntiAlias _ clippingAntiAliasBits * (1.0 / 127.0).
					alphaR _ alphaR * clippingAntiAlias.
					alphaG _ alphaG * clippingAntiAlias.
					alphaB _ alphaB * clippingAntiAlias ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAliasBits _ 0]].

	alphaR + alphaG + alphaB = 0.0 ifFalse: [
		targetWord _ targetBits at: pixelIndex.
		resultAlphaBits _ targetWord bitAnd: 16rFF000000.
		resultRBits _ targetWord bitAnd: 16rFF0000.
		resultGBits _ targetWord bitAnd: 16rFF00.
		resultBBits _ targetWord bitAnd: 16rFF.
		targetAlpha _ resultAlphaBits * ( 1.0 / (255.0 * 256 * 256 * 256)).

		"These if are not really needed. just ignore them if we use simd instructions."
		alphaR = 0.0 ifFalse: [
			unAlphaR _ 1.0 - alphaR.
			resultAlphaR _ alphaR + (unAlphaR * targetAlpha).
			self cCode: '
					resultR = (alphaR * foreR) + ((unAlphaR * (resultRBits >> 16)) * targetAlpha);
					resultRBits = (uint32_t)(resultR / resultAlphaR + 0.5) << 16;'
				inSmalltalk: [
					resultR _ alphaR * foreR + (unAlphaR * (resultRBits bitShift: -16) * targetAlpha).
					resultRBits _ (resultR / resultAlphaR + 0.5) asInteger bitShift: 16 ]].

		alphaG = 0.0 ifFalse: [
			unAlphaG _ 1.0 - alphaG.
			resultAlphaG _ alphaG + (unAlphaG * targetAlpha).
			self cCode: '
					resultG = (alphaG * foreG) + ((unAlphaG * (resultGBits >> 8)) * targetAlpha);
					resultGBits = (uint32_t)(resultG / resultAlphaG + 0.5) << 8;
					resultAlphaBits = (uint32_t)(resultAlphaG * 255.0 + 0.5) << 24;'
				inSmalltalk: [
					resultG _ alphaG * foreG + (unAlphaG * (resultGBits bitShift: -8) * targetAlpha).
					resultGBits _ (resultG / resultAlphaG + 0.5) asInteger bitShift: 8.
					resultAlphaBits _ (resultAlphaG * 255.0 + 0.5) asInteger bitShift: 24 ]].

		alphaB = 0.0 ifFalse: [
			unAlphaB _ 1.0 - alphaB.
			resultAlphaB _ alphaB + (unAlphaB * targetAlpha).
			self cCode: '
					resultB = (alphaB * foreB) + ((unAlphaB * resultBBits) * targetAlpha);
					resultBBits = (uint32_t)(resultB / resultAlphaB + 0.5);'
				inSmalltalk: [
					resultB _ alphaB * foreB + (unAlphaB * resultBBits * targetAlpha).
					resultBBits _ (resultB / resultAlphaB + 0.5) asInteger ]].

		targetWord _ ((resultAlphaBits bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		targetBits at: pixelIndex put: targetWord.

		currentMorphId = 0 ifFalse: [
			morphIdWord _ currentMorphId<<8 + clippingAntiAliasBits.
			morphIds at: pixelIndex put: morphIdWord ]].
! !

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

	targetBits _ aBitmap.
	morphIds _ aWordArray.
	edgeCounts _ otherWordArray.
	alphaMask _ anotherWordArray.
	t to: b do: [ :displayY |
		edgesUpToThisPixelR _ 0.
		edgesUpToThisPixelG _ 0.
		edgesUpToThisPixelB _ 0.
		pixelIndex _ displayY * targetWidth + l - 1.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			edgesThisPixelWord _ edgeCounts at: pixelIndex.
			edgesThisPixelWord = 0 ifFalse: [
				edgeCounts at: pixelIndex put: 0 ].

			self cCode: '
					edgesThisPixelR = (uint32_t) (edgesThisPixelWord & 0xFF0000) >> 16;
					edgesThisPixelG = (uint32_t) (edgesThisPixelWord & 0xFF00) >> 8;
					edgesThisPixelB = (uint32_t) (edgesThisPixelWord & 0xFF);'
				inSmalltalk: [
					edgesThisPixelR _ (edgesThisPixelWord bitAnd: 16rFF0000) bitShift: -16.
					edgesThisPixelG _ (edgesThisPixelWord bitAnd: 16rFF00)bitShift: -8.
					edgesThisPixelB _ edgesThisPixelWord bitAnd: 16rFF.
					"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
					edgesThisPixelR > 127 ifTrue: [ edgesThisPixelR _ edgesThisPixelR - 256 ].
					edgesThisPixelG > 127 ifTrue: [ edgesThisPixelG _ edgesThisPixelG - 256 ].
					edgesThisPixelB > 127 ifTrue: [ edgesThisPixelB _ edgesThisPixelB - 256 ]].

			edgesUpToThisPixelR _ edgesUpToThisPixelR + edgesThisPixelR.
			edgesUpToThisPixelG _ edgesUpToThisPixelG + edgesThisPixelG.
			edgesUpToThisPixelB _ edgesUpToThisPixelB + edgesThisPixelB.

			"In C, integers already behave like booleans"
			self cCode: '
					isRedInside = edgesUpToThisPixelR;
					isGreenInside = edgesUpToThisPixelG;
					isBlueInside = edgesUpToThisPixelB;'
				inSmalltalk: [
					isRedInside _ edgesUpToThisPixelR ~= 0.
					isGreenInside _ edgesUpToThisPixelG ~= 0.
					isBlueInside _ edgesUpToThisPixelB ~= 0 ].


			antiAliasAlphasWord _ alphaMask at: pixelIndex.
			antiAliasAlphasWord ~= 16r00000000 ifTrue: [
				alphaMask at: pixelIndex put: 0 ].
			(antiAliasAlphasWord ~= 16r00000000 or: [
				isRedInside or: [ isGreenInside or: [ isBlueInside ]]]) ifTrue: [
				self blendStrokeAndFillAt: pixelIndex
					redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside
					antiAliasAlphasWord: antiAliasAlphasWord ]]].! !

!VectorEnginePlugin methodsFor: 'blend stroke and fill colors' stamp: 'jmv 6/28/2021 17:18:30'!
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

	antiAliasRedAlphaBits _ antiAliasAlphasWord bitAnd: 16r7F0000.
	antiAliasGreenAlphaBits _ antiAliasAlphasWord bitAnd: 16r7F00.
	antiAliasBlueAlphaBits _ antiAliasAlphasWord bitAnd: 16r7F.
	antiAliasRedAlpha _ antiAliasRedAlphaBits * (1.0 / (127.0 * 256 * 256)).
	antiAliasGreenAlpha _ antiAliasGreenAlphaBits * (1.0 / (127.0 * 256)).
	antiAliasBlueAlpha _ antiAliasBlueAlphaBits * (1.0 / 127.0).
	alphaR _ antiAliasRedAlpha * strokeA.
	alphaG _ antiAliasGreenAlpha * strokeA.
	alphaB _ antiAliasBlueAlpha * strokeA.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			morphIdWord _ morphIds at: pixelIndex.
			clippingAntiAliasBits _ morphIdWord bitAnd: 16r7F.
			self cCode: '
					antiAliasGreenAlphaBitsShifted = antiAliasGreenAlphaBits >> 8;'
				inSmalltalk: [
					antiAliasGreenAlphaBitsShifted _ antiAliasGreenAlphaBits bitShift: -8 ].
			antiAliasGreenAlphaBitsShifted > clippingAntiAliasBits ifTrue: [
				clippingAntiAliasBits _ antiAliasGreenAlphaBitsShifted]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					morphIdWord _ morphIds at: pixelIndex.
					clippingAntiAliasBits _ morphIdWord bitAnd: 16r7F.
					clippingAntiAlias _ clippingAntiAliasBits * (1.0 / 127.0).
					alphaR _ alphaR * clippingAntiAlias.
					alphaG _ alphaG * clippingAntiAlias.
					alphaB _ alphaB * clippingAntiAlias ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAliasBits _ 0]].

	alphaR + alphaG + alphaB = 0.0 ifFalse: [
		targetWord _ targetBits at: pixelIndex.
		resultAlphaBits _ targetWord bitAnd: 16rFF000000.
		resultRBits _ targetWord bitAnd: 16rFF0000.
		resultGBits _ targetWord bitAnd: 16rFF00.
		resultBBits _ targetWord bitAnd: 16rFF.
		targetAlpha _ resultAlphaBits * ( 1.0 / (255.0 * 256 * 256 * 256)).

		"These if are not really needed. just ignore them if we use simd instructions."
		alphaR = 0.0 ifFalse: [
			unAlphaR _ 1.0 - alphaR.
			resultAlphaR _ alphaR + (unAlphaR * targetAlpha).
			self cCode: '
					resultR = (alphaR * strokeR) + ((unAlphaR * (resultRBits >> 16)) * targetAlpha);
					resultRBits = (uint32_t)(resultR / resultAlphaR + 0.5) << 16;'
				inSmalltalk: [
					resultR _ alphaR * strokeR + (unAlphaR * (resultRBits bitShift: -16) * targetAlpha).
					resultRBits _ (resultR / resultAlphaR + 0.5) asInteger bitShift: 16 ]].

		alphaG = 0.0 ifFalse: [
			unAlphaG _ 1.0 - alphaG.
			resultAlphaG _ alphaG + (unAlphaG * targetAlpha).
			self cCode: '
					resultG = (alphaG * strokeG) + ((unAlphaG * (resultGBits >> 8)) * targetAlpha);
					resultGBits = (uint32_t)(resultG / resultAlphaG + 0.5) << 8;
					resultAlphaBits = (uint32_t)(resultAlphaG * 255.0 + 0.5) << 24;'
				inSmalltalk: [
					resultG _ alphaG * strokeG + (unAlphaG * (resultGBits bitShift: -8) * targetAlpha).
					resultGBits _ (resultG / resultAlphaG + 0.5) asInteger bitShift: 8.
					resultAlphaBits _ (resultAlphaG * 255.0 + 0.5) asInteger bitShift: 24 ]].

		alphaB = 0.0 ifFalse: [
			unAlphaB _ 1.0 - alphaB.
			resultAlphaB _ alphaB + (unAlphaB * targetAlpha).
			self cCode: '
					resultB = (alphaB * strokeB) + ((unAlphaB * resultBBits) * targetAlpha);
					resultBBits = (uint32_t)(resultB / resultAlphaB + 0.5);'
				inSmalltalk: [
					resultB _ alphaB * strokeB + (unAlphaB * resultBBits * targetAlpha).
					resultBBits _ (resultB / resultAlphaB + 0.5) asInteger ]].

		targetWord _ ((resultAlphaBits bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		targetBits at: pixelIndex put: targetWord.

		currentMorphId = 0 ifFalse: [
			morphIdWord _ currentMorphId<<8 + clippingAntiAliasBits.
			morphIds at: pixelIndex put: morphIdWord ]].! !

!VectorEnginePlugin methodsFor: 'blend stroke and fill colors' stamp: 'jmv 6/28/2021 17:18:34'!
blendStrokeOnlyLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray alphaMask: anotherWordArray
	"Blends stroke color over background. Target translucency computed correctly.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"
	| pixelIndex antiAliasAlphasWord |
	self
		primitive: 'primBlendStrokeOnly'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger WordArray WordArray WordArray).

	self var: #antiAliasAlphasWord type: 'uint32_t'.

	targetBits _ aBitmap.
	morphIds _ aWordArray.
	alphaMask _ anotherWordArray.
	t to: b do: [ :displayY |
		pixelIndex _ displayY * targetWidth + l - 1.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			antiAliasAlphasWord _ alphaMask at: pixelIndex.
			antiAliasAlphasWord = 16r00000000 ifFalse: [
				alphaMask at: pixelIndex put: 0.
				self blendStrokeOnlyAt: pixelIndex antiAliasAlphasWord: antiAliasAlphasWord ]]].! !

!VectorEnginePlugin methodsFor: 'whole pixel - path algorithms' stamp: 'jmv 7/22/2021 20:28:30'!
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

	tcx _ (centerX * txA11) + (centerY * txA12) + txA13.
	tcy _ (centerX * txA21) + (centerY * txA22) + txA23.
	self cCode: 'scale = sqrt(txA11*txA11 + txA21*txA21);'
		inSmalltalk: [scale _ (txA11 squared + txA21 squared) sqrt].
	trx _ radiusPointX * scale.
	try _ radiusPointY * scale.
	hops _ ((trx max: try) * (self cCode: 'fabs(sweepAngle)' inSmalltalk: [sweepAngle abs]) / hop) asInteger + 2.
	d _ sweepAngle / hops.

	edgeCountsWP _ otherByteArray.
	alphaMaskWP _ anotherByteArray.
	contour _ aFloat32Array.
	angle _ startAngle.
	hops _ hops+1.
	1 to: hops do: [ :h |
		xp _ angle cos * trx.
		yp _ angle sin * try.
		x _ tthetaCos * xp - (tthetaSin * yp) + tcx.
		y _ tthetaSin * xp + (tthetaCos * yp) + tcy.
		spanLeft _ spanLeft min: x.
		spanTop _ spanTop min: y.
		spanRight _ spanRight max: x.
		spanBottom _ spanBottom max: y.
		self updateAlphasWPForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountWPAtX: x y: y ].
		self updateContourForX: x y: y.
		angle _ angle + d. ].! !

!VectorEnginePlugin methodsFor: 'whole pixel - path algorithms' stamp: 'jmv 7/5/2021 15:54:18'!
cubicBezierWPFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2 edgeCounts: otherByteArray alphaMask: anotherByteArray contour: aFloat32Array

	self
		primitive: 'primCubicBezierWP'
		parameters: #(Float Float Float Float Float Float Float Float ByteArray ByteArray Float32Array).

	edgeCountsWP _ otherByteArray.
	alphaMaskWP _ anotherByteArray.
	contour _ aFloat32Array.

	self pvt_cubicBezierWPFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2! !

!VectorEnginePlugin methodsFor: 'whole pixel - path algorithms' stamp: 'jmv 7/5/2021 15:57:23'!
lineWPFromX: xFrom y: yFrom toX: xTo y: yTo edgeCounts: otherByteArray alphaMask: anotherByteArray contour: aFloat32Array

	self
		primitive: 'primLineWP'
		parameters: #(Float Float Float Float ByteArray ByteArray Float32Array).

	edgeCountsWP _ otherByteArray.
	alphaMaskWP _ anotherByteArray.
	contour _ aFloat32Array.
	self pvt_lineWPFromX: xFrom y: yFrom toX: xTo y: yTo.! !

!VectorEnginePlugin methodsFor: 'whole pixel - path algorithms' stamp: 'jmv 7/5/2021 16:27:12'!
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

	txFrom _ (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom _ (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo _ (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo _ (xTo * txA21) + (yTo * txA22) + txA23.
	txControl1 _ (xControl1 * txA11) + (yControl1 * txA12) + txA13.
	tyControl1 _ (xControl1 * txA21) + (yControl1 * txA22) + txA23.
	txControl2 _ (xControl2 * txA11) + (yControl2 * txA12) + txA13.
	tyControl2 _ (xControl2 * txA21) + (yControl2 * txA22) + txA23.
	self cCode: '
		dx = fabs(txControl1-txFrom);
		dx2 = fabs(txTo-txControl2);
		dx3 = fabs(txControl2-txControl1);
		dy = fabs(tyControl1-tyFrom);
		dy2 = fabs(tyTo-tyControl2);
		dy3 = fabs(tyControl2-tyControl1);'
	inSmalltalk: [
		dx _ (txControl1-txFrom) abs.
		dx2 _ (txTo-txControl2) abs.
		dx3 _ (txControl2-txControl1) abs.
		dy _ (tyControl1-tyFrom) abs.
		dy2 _ (tyTo-tyControl2) abs.
		dy3 _ (tyControl2-tyControl1) abs ].
	dx _ (dx max: dx2) * 3 max: dx3 * 1.5.
	dy _ (dy max: dy2) * 3 max: dy3 * 1.5.
	hops _ ((dx max: dy) / hop) asInteger + 1.

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
	1 to: hops do: [ :h |
		oneLessT _ 1.0 - t.
		f1 _ oneLessT * oneLessT * oneLessT.
		f23 _ 3.0 * oneLessT * t.
		f2 _ f23 * oneLessT.
		f3 _ f23 * t.
		f4 _ t * t * t.
		x _ (f1 * txFrom) + (f2 * txControl1) + (f3 * txControl2) + (f4 * txTo).
		y _ (f1 * tyFrom) + (f2 * tyControl1) + (f3 * tyControl2) + (f4 * tyTo).
		self updateAlphasWPForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountWPAtX: x y: y ].
		self updateContourForX: x y: y.
		t _ t + increment. ].
	self updateAlphasWPForX: txTo y: tyTo.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountWPAtX: txTo y: tyTo ].
	self updateContourForX: txTo y: tyTo.! !

!VectorEnginePlugin methodsFor: 'whole pixel - path algorithms' stamp: 'jmv 7/5/2021 16:29:10'!
pvt_lineWPFromX: xFrom y: yFrom toX: xTo y: yTo

	| txFrom tyFrom txTo tyTo dx dy hops incrementX incrementY x y |
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
	self var: #incrementX type: 'float'.
	self var: #incrementY type: 'float'.
	self var: #x type: 'float'.
	self var: #y type: 'float'.

	txFrom _ (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom _ (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo _ (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo _ (xTo * txA21) + (yTo * txA22) + txA23.
	dx _ txTo-txFrom.
	dy _ tyTo-tyFrom.
	incrementX _ dx.
	incrementY _ dy.
	self cCode: '
		dx = fabs(dx);
		dy = fabs(dy);'
	inSmalltalk: [
		dx _ dx abs.
		dy _ dy abs ].
	hops _ ((dx max: dy) / hop) asInteger + 1.
	incrementX _ incrementX / hops.
	incrementY _ incrementY / hops.

	spanLeft _ spanLeft min: (txFrom min: txTo).
	spanRight _ spanRight max: (txFrom max: txTo).
	spanTop _ spanTop min: (tyFrom min: tyTo).
	spanBottom _ spanBottom max: (tyFrom max: tyTo).

	x _ txFrom.
	y _ tyFrom.
	1 to: hops do: [ :h |
		self updateAlphasWPForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountWPAtX: x y: y ].
		self updateContourForX: x y: y.
		x _ x + incrementX.
		y _ y + incrementY ].
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

	txFrom _ (xFrom * txA11) + (yFrom * txA12) + txA13.
	tyFrom _ (xFrom * txA21) + (yFrom * txA22) + txA23.
	txTo _ (xTo * txA11) + (yTo * txA12) + txA13.
	tyTo _ (xTo * txA21) + (yTo * txA22) + txA23.
	txControl _ (xControl * txA11) + (yControl * txA12) + txA13.
	tyControl _ (xControl * txA21) + (yControl * txA22) + txA23.
	self cCode: '
		dx = fabs(txTo-txFrom);
		dx2 = fabs(txControl-txFrom);
		dy = fabs(tyTo-tyFrom);
		dy2 = fabs(tyControl-tyFrom);'
	inSmalltalk: [
		dx _ (txTo-txFrom) abs.
		dx2 _ (txControl-txFrom) abs.
		dy _ (tyTo-tyFrom) abs.
		dy2 _ (tyControl-tyFrom) abs.].

	"If almost a vertical line, just draw a line. (Ignoring control point)"
	(dx < 1.0 and: [dx2 < 1.0]) ifTrue: [
		^self pvt_lineWPFromX: xFrom y: yFrom toX: xTo y: yTo ].

	"If almost an horizontal line, just draw a line. (Ignoring control point)"
	(dy < 1.0 and: [dy2 < 1.0]) ifTrue: [
		^self pvt_lineWPFromX: xFrom y: yFrom toX: xTo y: yTo ].

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
	self updateAlphasWPForX: x y: y.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountWPAtX: x y: y ].
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
		dx _ x-x0. dy _ y-y0.
		self cCode: '
			length = sqrt(dx*dx + dy*dy);'
			inSmalltalk: [ length _ ((dx*dx) + (dy*dy)) sqrt ].
		correction _ hop / length.
		[
			increment _ increment / length * hop.
			t _ t0 + increment. oneLessT _ 1.0 - t.
			f1 _ oneLessT * oneLessT. f2 _ 2.0 * oneLessT * t. f3 _ t * t.
			x _ (f1 * txFrom) + (f2 * txControl) + (f3 * txTo).
			y _ (f1 * tyFrom) + (f2 * tyControl) + (f3 * tyTo).
			dx _ x-x0. dy _ y-y0.
			self cCode: '
				length = sqrt(dx*dx + dy*dy);'
				inSmalltalk: [ length _ ((dx*dx) + (dy*dy)) sqrt ].
			correction _ hop / length.
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

	edgeCountsWP _ otherByteArray.
	alphaMaskWP _ anotherByteArray.
	contour _ aFloat32Array.
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
	t _ (y - auxStrokeWidthDilatedHalf + 1) asInteger.														"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	t < clipTop ifTrue: [ t _ clipTop ].
	b _ (y + auxStrokeWidthDilatedHalf) asInteger.
	b > clipBottom ifTrue: [ b _ clipBottom ].
	l _ (x - auxStrokeWidthDilatedHalf + 1) asInteger.														"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	l < clipLeft ifTrue: [ l _ clipLeft ].
	r _ (x + auxStrokeWidthDilatedHalf) asInteger.
	r > clipRight ifTrue: [ r _ clipRight ].

	t to: b do: [ :displayY |
		pixelIndex _ displayY * targetWidth + l -1.
		dy _ displayY - y.
		dySquared _ dy * dy.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			alphaByte _ alphaMaskWP at: pixelIndex.
			alphaByte = 16r7F ifFalse: [
				dx _ displayX - x.
				dxSquared _ dx * dx.
				distanceToAxisSquared _ dxSquared + dySquared.
				distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
					ifTrue: [
						distanceToAxisSquared <= auxStrokeWidthErodedHalfSquared
							ifTrue: [
								candidateAlpha _ 16r7F ]
							ifFalse: [
								self cCode: '
										candidateAlpha = (uint8_t)((auxStrokeWidthDilatedHalf - (sqrt(distanceToAxisSquared))) * auxAntiAliasingWidthScaledInverse);'
									inSmalltalk: [
										candidateAlpha _ (auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt * auxAntiAliasingWidthScaledInverse) asInteger ]].
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
	t _ (y - auxStrokeWidthDilatedHalf + 1) asInteger.														"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	t < clipTop ifTrue: [ t _ clipTop ].
	b _ (y + auxStrokeWidthDilatedHalf) asInteger.
	b > clipBottom ifTrue: [ b _ clipBottom ].
	l _ (x - auxStrokeWidthDilatedHalf + 1) asInteger.														"(int(z+1)) works equally well than the more intuitive but slower (int(ceil(z))"
	l < clipLeft ifTrue: [ l _ clipLeft ].
	r _ (x + auxStrokeWidthDilatedHalf) asInteger.
	r > clipRight ifTrue: [ r _ clipRight ].

	t to: b do: [ :displayY |
		pixelIndex _ displayY * targetWidth + l -1.
		dy _ displayY - y.
		dySquared _ dy * dy.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			alphaByte _ alphaMaskWP at: pixelIndex.
			alphaByte = 16r7F ifFalse: [
				dx _ displayX - x.
				distanceToAxisSquared _ dx * dx + dySquared.
				distanceToAxisSquared < auxStrokeWidthDilatedHalfSquared
					ifTrue: [
						self cCode: '
								candidateAlpha = (uint8_t)((auxStrokeWidthDilatedHalf - (sqrt(distanceToAxisSquared))) * auxAntiAliasingWidthScaledInverse);'
							inSmalltalk: [
								candidateAlpha _ (auxStrokeWidthDilatedHalf - distanceToAxisSquared sqrt * auxAntiAliasingWidthScaledInverse) asInteger ].
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

	thisYTruncated _ y asInteger.	"truncated, both in C and Smalltalk"
	thisYTruncated = prevYTruncated ifTrue: [
		^ 0].
	(thisYTruncated between: clipTop-1 and: clipBottom) ifFalse: [
		^ 0 ].
	prevYTruncated = 16r7FFFFFFF ifTrue: [ 
		prevYTruncated _ thisYTruncated.
		^ 0 ].

	thisYTruncated > prevYTruncated
		ifTrue: [
			pixelY _ thisYTruncated.
			increment _ 16r01 ]
		ifFalse: [
			pixelY _ prevYTruncated.
			increment _ 16rFF ].
	prevYTruncated _ thisYTruncated.

	"All edge count at the left of the clipRect are added there (at the left of the clipRect).
	The effect is the same, and we need to clean up less stuff afterwards.
	More important, it avoids trying to acess pixels outside our form, i.e. invalid array acesses."
	pixelOffset _ (x+1) asInteger max: clipLeft.											"take the next pixel center to the right of x"

	pixelOffset <= clipRight ifTrue: [
		pixelIndex _ pixelY * targetWidth + pixelOffset.
		count _ edgeCountsWP at: pixelIndex.
		count _ count + increment.
		edgeCountsWP at: pixelIndex put: count ].! !

!VectorEnginePlugin methodsFor: 'whole pixel - text and multi paths' stamp: 'jmv 7/22/2021 23:26:25'!
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

	edgeCountsWP _ otherByteArray.
	alphaMaskWP _ anotherByteArray.
	txA11 _ txA11 * sx.
	txA12 _ txA12 * sy.
	txA21 _ txA21 * sx.
	txA22 _ txA22 * sy.

	nextGlyphX _ destX / sx.
	nextGlyphY _ destY / sy.
	startIndex-1 to: stopIndex-1 do: [ :charIndex |
		self cCode:
				'iso8859s15 = aString[charIndex];'
			inSmalltalk: [
				iso8859s15 _ (aString at: charIndex+1) iso8859s15Code. ].
		aBoolean ifTrue: [
			iso8859s15 = 95 ifTrue: [ iso8859s15 _ 28 ].												"If underscore, use left arrow"
			iso8859s15 = 94 ifTrue: [ iso8859s15 _ 30 ].												"If caret, use up arrow"
			].
		i _ contourDataIndexes at: iso8859s15.
		i _ i - 1.
		advanceWidth _ contourData at: i. i _ i + 5.
		numContours _ (contourData at: i) asInteger. i _ i + 1.
		1 to: numContours do: [ :idx |
			numBeziers _ (contourData at: i) asInteger. i _ i + 1.
			ttMoveToX _ (contourData at: i) + nextGlyphX. i _ i + 1.
			ttMoveToY _ (contourData at: i) + nextGlyphY. i _ i + 1.
			startX _ (ttMoveToX * txA11) + (ttMoveToY * txA12) + txA13.
			startY _ (ttMoveToX * txA21) + (ttMoveToY * txA22) + txA23.
			contourStartX _ startX.
			contourStartY _ startY.
			self newTrajectoryFragment.
			1 to: numBeziers do: [ :idx2 |
				ttEndX _ contourData at: i. i _ i + 1.
				ttEndY _ contourData at: i. i _ i + 1.
				ttControlX _ contourData at: i. i _ i + 1.
				ttControlY _ contourData at: i. i _ i + 1.
				endX _ (ttEndX * txA11) + (ttEndY * txA12) + startX.
				endY _ (ttEndX * txA21) + (ttEndY * txA22) + startY.
				controlX _ (ttControlX * txA11) + (ttControlY * txA12) + startX.
				controlY _ (ttControlX * txA21) + (ttControlY * txA22) + startY.
				"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
				xMinEnd _ startX min: endX.
				xMaxEnd _ startX max: endX.
				yMinEnd _ startY min: endY.
				yMaxEnd _ startY max: endY.
				spanLeft _ spanLeft min: (xMinEnd min: (xMinEnd+controlX) / 2.0).
				spanRight _ spanRight max: (xMaxEnd max: (xMaxEnd+controlX) / 2.0).
				spanTop _ spanTop min: (yMinEnd min: (yMinEnd+controlY) / 2.0).
				spanBottom _ spanBottom max: (yMaxEnd max: (yMaxEnd+controlY) / 2.0).
				"Compute Quadratic Bezier Curve,"
				"Case t = 0.0"
				x _ startX.
				y _ startY.
				self updateAlphasWPZeroStrokeForX: x y: y.
				self updateEdgeCountWPAtX: x y: y.
				"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
				self cCode: 'dx = fabs(endX-startX); dy = fabs(endY-startY);'
				inSmalltalk: [ dx _ (endX-startX) abs. dy _ (endY-startY) abs ].
				increment _ 0.5 / (dx max: dy) min: 0.5.
				t _ 0.0.
				[
					t0 _ t. x0 _ x. y0 _ y.
					"Compute next point"
					t _ t0 + increment. oneLessT _ 1.0 - t.
					f1 _ oneLessT * oneLessT. f2 _ 2.0 * oneLessT * t. f3 _ t * t.
					x _ (f1 * startX) + (f2 * controlX) + (f3 * endX).
					y _ (f1 * startY) + (f2 * controlY) + (f3 * endY).
					"Now adjust the increment to aim at the required hop length, and recompute next point."
					dx _ x-x0. dy _ y-y0.
					self cCode: '
						length = sqrt(dx*dx + dy*dy);'
						inSmalltalk: [ length _ ((dx*dx) + (dy*dy)) sqrt ].
					correction _ hop / length.
					[
						increment _ increment / length * hop.
						t _ t0 + increment. oneLessT _ 1.0 - t.
						f1 _ oneLessT * oneLessT. f2 _ 2.0 * oneLessT * t. f3 _ t * t.
						x _ (f1 * startX) + (f2 * controlX) + (f3 * endX).
						y _ (f1 * startY) + (f2 * controlY) + (f3 * endY).
						dx _ x-x0. dy _ y-y0.
						self cCode: '
							length = sqrt(dx*dx + dy*dy);'
							inSmalltalk: [ length _ ((dx*dx) + (dy*dy)) sqrt ].
						correction _ hop / length.
						correction < 1.0]  whileTrue.		   "Keep adjusting if hop would be too big"
					t < 1.0 ]
				whileTrue: [
					self updateAlphasWPZeroStrokeForX: x y: y.
					self updateEdgeCountWPAtX: x y: y ].
				"Note: For TrueType font definitions, we assume that all contour fragments start exactly where the previous ends.
				This means that the end point is only added for the last fragment of the contour, and not for each one of them."
				startX _ endX.
				startY _ endY.	
			].
			self updateAlphasWPZeroStrokeForX: endX y: endY.
			self updateEdgeCountWPAtX: endX y: endY.
			"Similar effect to ensureClosePath in #finishPath:,
			but assume the TrueType definition is essentially right, and there might only be a rounding error.
			So, don't draw a line, but just (possibly) correct edgeCountsWP. The possibility of rounding error is most likely zero.
			Anyway, this is cheap."
			self updateEdgeCountWPAtX: contourStartX y: contourStartY.
		].
		nextGlyphX _ nextGlyphX + advanceWidth.
	].
	txA11 _ txA11 / sx.
	txA12 _ txA12 / sy.
	txA21 _ txA21 / sx.
	txA22 _ txA22 / sy.
	answer _ nextGlyphX * sx.
	^answer asOop: Float! !

!VectorEnginePlugin methodsFor: 'whole pixel - text and multi paths' stamp: 'jmv 7/22/2021 23:30:41'!
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

	edgeCountsWP _ otherByteArray.
	alphaMaskWP _ anotherByteArray.
	txA11 _ txA11 * sx.
	txA12 _ txA12 * sy.
	txA21 _ txA21 * sx.
	txA22 _ txA22 * sy.

	nextGlyphX _ destX / sx.
	nextGlyphY _ destY / sy.
	startIndex-1 to: stopIndex-1 do: [ :utf32Index |
		utf32 _ aWordArray at: utf32Index.
		aBoolean ifTrue: [
			utf32 = 95 ifTrue: [ utf32 _ 8592 ].												"If underscore, use left arrow"
			utf32 = 94 ifTrue: [ utf32 _ 8593 ].												"If caret, use up arrow"
			].
		utf32 <= 16r7F
			ifTrue: [
				utf8Byte _ utf32.
				i _ contourDataIndexes at: utf8Byte ]
			ifFalse: [ utf32 <= 16r7FF
			ifTrue: [
				utf8Byte _ (utf32 bitShift: -6) bitOr: 2r11000000.
				i _ contourDataIndexes at: utf8Byte.
				utf8Byte _ (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
				i _ contourDataIndexes at: utf8Byte - i ]
			ifFalse: [ utf32 <= 16rFFFF
			ifTrue: [
				utf8Byte _ (utf32 bitShift: -12) bitOr: 2r11100000.
				i _ contourDataIndexes at: utf8Byte.
				utf8Byte _ ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
				i _ contourDataIndexes at: utf8Byte - i.
				utf8Byte _ (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
				i _ contourDataIndexes at: utf8Byte - i ]
			ifFalse: [
				utf8Byte _ (utf32 bitShift: -18) bitOr: 2r11110000.
				i _ contourDataIndexes at: utf8Byte.
				utf8Byte _ ((utf32 bitShift: -12) bitAnd: 2r00111111) bitOr: 2r10000000.
				i _ contourDataIndexes at: utf8Byte - i.
				utf8Byte _ ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
				i _ contourDataIndexes at: utf8Byte - i.
				utf8Byte _ (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
				i _ contourDataIndexes at: utf8Byte - i ]]].
		i _ i - 1.
		advanceWidth _ contourData at: i. i _ i + 5.
		numContours _ (contourData at: i) asInteger. i _ i + 1.
		1 to: numContours do: [ :idx |
			numBeziers _ (contourData at: i) asInteger. i _ i + 1.
			ttMoveToX _ (contourData at: i) + nextGlyphX. i _ i + 1.
			ttMoveToY _ (contourData at: i) + nextGlyphY. i _ i + 1.
			startX _ (ttMoveToX * txA11) + (ttMoveToY * txA12) + txA13.
			startY _ (ttMoveToX * txA21) + (ttMoveToY * txA22) + txA23.
			contourStartX _ startX.
			contourStartY _ startY.
			self newTrajectoryFragment.
			1 to: numBeziers do: [ :idx2 |
				ttEndX _ contourData at: i. i _ i + 1.
				ttEndY _ contourData at: i. i _ i + 1.
				ttControlX _ contourData at: i. i _ i + 1.
				ttControlY _ contourData at: i. i _ i + 1.
				endX _ (ttEndX * txA11) + (ttEndY * txA12) + startX.
				endY _ (ttEndX * txA21) + (ttEndY * txA22) + startY.
				controlX _ (ttControlX * txA11) + (ttControlY * txA12) + startX.
				controlY _ (ttControlX * txA21) + (ttControlY * txA22) + startY.
				"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
				xMinEnd _ startX min: endX.
				xMaxEnd _ startX max: endX.
				yMinEnd _ startY min: endY.
				yMaxEnd _ startY max: endY.
				spanLeft _ spanLeft min: (xMinEnd min: (xMinEnd+controlX) / 2.0).
				spanRight _ spanRight max: (xMaxEnd max: (xMaxEnd+controlX) / 2.0).
				spanTop _ spanTop min: (yMinEnd min: (yMinEnd+controlY) / 2.0).
				spanBottom _ spanBottom max: (yMaxEnd max: (yMaxEnd+controlY) / 2.0).
				"Compute Quadratic Bezier Curve,"
				"Case t = 0.0"
				x _ startX.
				y _ startY.
				self updateAlphasWPZeroStrokeForX: x y: y.
				self updateEdgeCountWPAtX: x y: y.
				"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
				self cCode: 'dx = fabs(endX-startX); dy = fabs(endY-startY);'
				inSmalltalk: [ dx _ (endX-startX) abs. dy _ (endY-startY) abs ].
				increment _ 0.5 / (dx max: dy) min: 0.5.
				t _ 0.0.
				[
					t0 _ t. x0 _ x. y0 _ y.
					"Compute next point. Only C version to avoid hitting a limit in Smalltalk compiler."
					self cCode: '
						t = t0 + increment; oneLessT = 1.0 - t;
						f1 = oneLessT * oneLessT; f2 = 2.0 * oneLessT * t; f3 = t * t;
						x = (f1 * startX) + (f2 * controlX) + (f3 * endX);
						y = (f1 * startY) + (f2 * controlY) + (f3 * endY);
						dx = x-x0; dy = y-y0;
						length = sqrt(dx*dx + dy*dy);'
						inSmalltalk: [ length _ ((dx*dx) + (dy*dy)) sqrt ].
					correction _ hop / length.
					"Now adjust the increment to aim at the required hop length, and recompute next point."
					[
						increment _ increment / length * hop.
						t _ t0 + increment. oneLessT _ 1.0 - t.
						f1 _ oneLessT * oneLessT. f2 _ 2.0 * oneLessT * t. f3 _ t * t.
						x _ (f1 * startX) + (f2 * controlX) + (f3 * endX).
						y _ (f1 * startY) + (f2 * controlY) + (f3 * endY).
						dx _ x-x0. dy _ y-y0.
						self cCode: '
							length = sqrt(dx*dx + dy*dy);'
							inSmalltalk: [ length _ ((dx*dx) + (dy*dy)) sqrt ].
						correction _ hop / length.
						correction < 1.0]  whileTrue.		   "Keep adjusting if hop would be too big"
					t < 1.0 ]
				whileTrue: [
					self updateAlphasWPZeroStrokeForX: x y: y.
					self updateEdgeCountWPAtX: x y: y. ].
				"Note: For TrueType font definitions, we assume that all contour fragments start exactly where the previous ends.
				This means that the end point is only added for the last fragment of the contour, and not for each one of them."
				startX _ endX.
				startY _ endY.	
			].
			self updateAlphasWPZeroStrokeForX: endX y: endY.
			self updateEdgeCountWPAtX: endX y: endY.
			"Similar effect to ensureClosePath in #finishPath:,
			but assume the TrueType definition is essentially right, and there might only be a rounding error.
			So, don't draw a line, but just (possibly) correct edgeCountsWP. The possibility of rounding error is most likely zero.
			Anyway, this is cheap."
			self updateEdgeCountWPAtX: contourStartX y: contourStartY.
		].
		nextGlyphX _ nextGlyphX + advanceWidth.
	].
	txA11 _ txA11 / sx.
	txA12 _ txA12 / sy.
	txA21 _ txA21 / sx.
	txA22 _ txA22 / sy.
	answer _ nextGlyphX * sx.
	^answer asOop: Float! !

!VectorEnginePlugin methodsFor: 'whole pixel - text and multi paths' stamp: 'jmv 7/22/2021 23:27:29'!
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

	edgeCountsWP _ otherByteArray.
	alphaMaskWP _ anotherByteArray.
	txA11 _ txA11 * sx.
	txA12 _ txA12 * sy.
	txA21 _ txA21 * sx.
	txA22 _ txA22 * sy.

	nextGlyphX _ destX / sx.
	nextGlyphY _ destY / sy.
	baseIndex _ 0.
	byteStartIndex-1 to: byteStopIndex-1 do: [ :byteIndex |
		byte _ aByteArray at: byteIndex.
		i _ contourDataIndexes at: baseIndex + byte.
		aBoolean ifTrue: [
			byte = 95 ifTrue: [
				i _ contourDataIndexes at: 226.
				i _ contourDataIndexes at: 134 - i.
				i _ contourDataIndexes at: 144 - i ].
			byte = 94 ifTrue: [ 
				i _ contourDataIndexes at: 226.
				i _ contourDataIndexes at: 134 - i.
				i _ contourDataIndexes at: 145 - i ]].
		i < 0
			ifTrue: [ baseIndex _ 0 - i ]
			ifFalse: [
				i _ i - 1.
				advanceWidth _ contourData at: i. i _ i + 5.
				numContours _ (contourData at: i) asInteger. i _ i + 1.
				1 to: numContours do: [ :idx |
					numBeziers _ (contourData at: i) asInteger. i _ i + 1.
					ttMoveToX _ (contourData at: i) + nextGlyphX. i _ i + 1.
					ttMoveToY _ (contourData at: i) + nextGlyphY. i _ i + 1.
					startX _ (ttMoveToX * txA11) + (ttMoveToY * txA12) + txA13.
					startY _ (ttMoveToX * txA21) + (ttMoveToY * txA22) + txA23.
					contourStartX _ startX.
					contourStartY _ startY.
					self newTrajectoryFragment.
					1 to: numBeziers do: [ :idx2 |
						ttEndX _ contourData at: i. i _ i + 1.
						ttEndY _ contourData at: i. i _ i + 1.
						ttControlX _ contourData at: i. i _ i + 1.
						ttControlY _ contourData at: i. i _ i + 1.
						endX _ (ttEndX * txA11) + (ttEndY * txA12) + startX.
						endY _ (ttEndX * txA21) + (ttEndY * txA22) + startY.
						controlX _ (ttControlX * txA11) + (ttControlY * txA12) + startX.
						controlY _ (ttControlX * txA21) + (ttControlY * txA22) + startY.
						"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
						xMinEnd _ startX min: endX.
						xMaxEnd _ startX max: endX.
						yMinEnd _ startY min: endY.
						yMaxEnd _ startY max: endY.
						spanLeft _ spanLeft min: (xMinEnd min: (xMinEnd+controlX) / 2.0).
						spanRight _ spanRight max: (xMaxEnd max: (xMaxEnd+controlX) / 2.0).
						spanTop _ spanTop min: (yMinEnd min: (yMinEnd+controlY) / 2.0).
						spanBottom _ spanBottom max: (yMaxEnd max: (yMaxEnd+controlY) / 2.0).
						"Case t = 0.0"
						x _ startX.
						y _ startY.
						self updateAlphasWPZeroStrokeForX: x y: y.
						self updateEdgeCountWPAtX: x y: y.
						"Will be corrected for each hop. This, being close to pointFrom, is a good initial guess for first correction."
						self cCode: 'dx = fabs(endX-startX); dy = fabs(endY-startY);'
						inSmalltalk: [ dx _ (endX-startX) abs. dy _ (endY-startY) abs ].
						increment _ 0.5 / (dx max: dy) min: 0.5.
						"Compute Quadratic Bezier Curve,"
						t _ 0.0.
						[
							t0 _ t. x0 _ x. y0 _ y.
							"Compute next point"
							t _ t0 + increment. oneLessT _ 1.0 - t.
							f1 _ oneLessT * oneLessT. f2 _ 2.0 * oneLessT * t. f3 _ t * t.
							x _ (f1 * startX) + (f2 * controlX) + (f3 * endX).
							y _ (f1 * startY) + (f2 * controlY) + (f3 * endY).
							"Now adjust the increment to aim at the required hop length, and recompute next point."
							dx _ x-x0. dy _ y-y0.
							self cCode: '
								length = sqrt(dx*dx + dy*dy);'
								inSmalltalk: [ length _ ((dx*dx) + (dy*dy)) sqrt ].
							correction _ hop / length.
							[
								increment _ increment / length * hop.
								t _ t0 + increment. oneLessT _ 1.0 - t.
								f1 _ oneLessT * oneLessT. f2 _ 2.0 * oneLessT * t. f3 _ t * t.
								x _ (f1 * startX) + (f2 * controlX) + (f3 * endX).
								y _ (f1 * startY) + (f2 * controlY) + (f3 * endY).
								dx _ x-x0. dy _ y-y0.
								self cCode: '
									length = sqrt(dx*dx + dy*dy);'
									inSmalltalk: [ length _ ((dx*dx) + (dy*dy)) sqrt ].
								correction _ hop / length.
								correction < 1.0]  whileTrue.		   "Keep adjusting if hop would be too big"
							t < 1.0 ]
						whileTrue: [
							self updateAlphasWPZeroStrokeForX: x y: y.
							self updateEdgeCountWPAtX: x y: y ].
						"Note: For TrueType font definitions, we assume that all contour fragments start exactly where the previous ends.
						This means that the end point is only added for the last fragment of the contour, and not for each one of them."
						startX _ endX.
						startY _ endY.	
					].
					self updateAlphasWPZeroStrokeForX: endX y: endY.
					self updateEdgeCountWPAtX: endX y: endY.
					"Similar effect to ensureClosePath in #finishPath:,
					but assume the TrueType definition is essentially right, and there might only be a rounding error.
					So, don't draw a line, but just (possibly) correct edgeCountsWP. The possibility of rounding error is most likely zero.
					Anyway, this is cheap."
					self updateEdgeCountWPAtX: contourStartX y: contourStartY.
				].
				nextGlyphX _ nextGlyphX + advanceWidth.
				baseIndex _ 0.
			]
	].
	txA11 _ txA11 / sx.
	txA12 _ txA12 / sy.
	txA21 _ txA21 / sx.
	txA22 _ txA22 / sy.
	answer _ nextGlyphX * sx.
	^answer asOop: Float! !

!VectorEnginePlugin methodsFor: 'whole pixel - text and multi paths' stamp: 'jmv 7/5/2021 16:33:04'!
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

	edgeCountsWP _ otherByteArray.
	alphaMaskWP _ anotherByteArray.
	contour _ otherFloat32Array.
	i _ 0.
	[ i < size ] whileTrue: [
		commandType _ (aFloat32Array at: i) asInteger. i _ i + 1.
		commandType caseOf: {
			[0] -> [
				i+1 < size ifFalse: [ ^self "failure" ].
				startX _ aFloat32Array at: i. i _ i + 1.
				startY _ aFloat32Array at: i. i _ i + 1.
				self newTrajectoryFragment.
				].
			[1] -> [
				i+1 < size ifFalse: [ ^self "failure" ].
				endX _ aFloat32Array at: i. i _ i + 1.
				endY _ aFloat32Array at: i. i _ i + 1.
				self pvt_lineWPFromX: startX y: startY toX: endX y: endY.
				startX _ endX.
				startY _ endY.
				].
			[2] -> [
				i+3 < size ifFalse: [ ^self "failure" ].
				endX _ aFloat32Array at: i. i _ i + 1.
				endY _ aFloat32Array at: i. i _ i + 1.
				control1X _ aFloat32Array at: i. i _ i + 1.
				control1Y _ aFloat32Array at: i. i _ i + 1.
				self pvt_quadraticBezierWPFromX: startX y: startY
					toX: endX y: endY
					controlX: control1X y: control1Y.
				startX _ endX.
				startY _ endY.
				].
			[3] -> [
				i+5 < size ifFalse: [ ^self "failure" ].
				endX _ aFloat32Array at: i. i _ i + 1.
				endY _ aFloat32Array at: i. i _ i + 1.
				control1X _ aFloat32Array at: i. i _ i + 1.
				control1Y _ aFloat32Array at: i. i _ i + 1.
				control2X _ aFloat32Array at: i. i _ i + 1.
				control2Y _ aFloat32Array at: i. i _ i + 1.
				self pvt_cubicBezierWPFromX: startX y: startY
					toX: endX y: endY
					control1X: control1X y: control1Y
					control2X: control2X y: control2Y.
				startX _ endX.
				startY _ endY.
				]}
		otherwise: [ ^self "failure"]].! !

!VectorEnginePlugin methodsFor: 'whole pixel - blend stroke and fill' stamp: 'jmv 6/28/2021 17:18:38'!
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

	antiAliasAlpha _ antiAliasAlphaBits * 0.007874.								"1.0/127.0"
	alpha _ antiAliasAlpha * fillA.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			morphIdWord _ morphIds at: pixelIndex.
			clippingAntiAliasBits _ morphIdWord bitAnd: 16r7F.
			antiAliasAlphaBits > clippingAntiAliasBits ifTrue: [
				clippingAntiAliasBits _ antiAliasAlphaBits]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					morphIdWord _ morphIds at: pixelIndex.
					clippingAntiAliasBits _ morphIdWord bitAnd: 16r7F.
					clippingAntiAlias _ clippingAntiAliasBits * 0.007874.								"1.0/127.0"
					alpha _ alpha * clippingAntiAlias ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAliasBits _ 0]].

	alpha = 0.0 ifFalse: [
		targetWord _ targetBits at: pixelIndex.
		resultRBits _ targetWord bitAnd: 16rFF0000.
		resultGBits _ targetWord bitAnd: 16rFF00.
		resultBBits _ targetWord bitAnd: 16rFF.
		unAlpha _ 1.0 - alpha.
		self cCode: '
				resultR = (alpha * fillR) + (unAlpha * (resultRBits >> 16));
				resultRBits = (uint32_t)(resultR + 0.5) << 16;
				resultG = (alpha * fillG) + (unAlpha * (resultGBits >> 8));
				resultGBits = (uint32_t)(resultG + 0.5) << 8;
				resultB = (alpha * fillB) + (unAlpha * resultBBits);
				resultBBits = (uint32_t)(resultB + 0.5);'
			inSmalltalk: [
				resultR _ alpha * fillR + (unAlpha * (resultRBits bitShift: -16)).
				resultRBits _ (resultR + 0.5) asInteger bitShift: 16.
				resultG _ alpha * fillG + (unAlpha * (resultGBits bitShift: -8)).
				resultGBits _ (resultG + 0.5) asInteger bitShift: 8.
				resultB _ alpha * fillB + (unAlpha * resultBBits).
				resultBBits _ (resultB + 0.5) asInteger ].

		targetWord _ ((16rFF000000 bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		targetBits at: pixelIndex put: targetWord.

		currentMorphId = 0 ifFalse: [
			morphIdWord _ currentMorphId<<8 + clippingAntiAliasBits.
			morphIds at: pixelIndex put: morphIdWord ]].! !

!VectorEnginePlugin methodsFor: 'whole pixel - blend stroke and fill' stamp: 'jmv 6/29/2021 15:16:15'!
blendFillOnlyWPOTLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherByteArray alphaMask: anotherByteArray
	"Blends fill color over background.
	WP: Whole pixel anti aliasing.
	OT: Target ignored on input and set to opaque on output.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"
	| pixelIndex edgesUpToThisPixel edgesThisPixel antiAliasAlphaBits |
	self
		primitive: 'primBlendFillOnlyWPOT'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger WordArray WordArray ByteArray ByteArray).

	self var: #edgesThisPixel type: 'uint8_t'.
	self var: #edgesUpToThisPixel type: 'uint8_t'.
	self var: #antiAliasAlphaBits type: 'uint8_t'.

	targetBits _ aBitmap.
	morphIds _ aWordArray.
	edgeCountsWP _ otherByteArray.
	alphaMaskWP _ anotherByteArray.
	t to: b do: [ :displayY |
		edgesUpToThisPixel _ 0.
		pixelIndex _ displayY * targetWidth + l - 1.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			edgesThisPixel _ edgeCountsWP at: pixelIndex.
			edgesThisPixel = 0 ifFalse: [
				edgeCountsWP at: pixelIndex put: 0.
				self cCode: '' inSmalltalk: [
					"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
					edgesThisPixel > 127 ifTrue: [ edgesThisPixel _ edgesThisPixel - 256 ]].
				edgesUpToThisPixel _ edgesUpToThisPixel + edgesThisPixel ].
			antiAliasAlphaBits _ alphaMaskWP at: pixelIndex.
			edgesUpToThisPixel = 0
				ifFalse: [				"Inside the shape"
					antiAliasAlphaBits = 16r00 ifFalse: [ alphaMaskWP at: pixelIndex put: 0 ].
					antiAliasAlphaBits _ 16r7F - antiAliasAlphaBits.
					self blendFillOnlyWPOTAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits
					]
				ifTrue: [				"Still in the anti aliasing area, but outside the shape, strictly speaking."
					antiAliasAlphaBits = 16r00 ifFalse: [
						alphaMaskWP at: pixelIndex put: 0.
						self blendFillOnlyWPOTAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits ]]]].! !

!VectorEnginePlugin methodsFor: 'whole pixel - blend stroke and fill' stamp: 'jmv 6/28/2021 17:18:56'!
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

	antiAliasAlpha _ antiAliasAlphaBits * 0.007874.								"1.0/127.0"
	antiAliasUnAlpha _ 1.0 - antiAliasAlpha.
	"We are inside the shape"
	alpha _ antiAliasAlpha * strokeA + (antiAliasUnAlpha * fillA).
	foreR _ antiAliasAlpha * strokeR + (antiAliasUnAlpha * fillR).
	foreG _ antiAliasAlpha * strokeG + (antiAliasUnAlpha * fillG).
	foreB _ antiAliasAlpha * strokeB + (antiAliasUnAlpha * fillB).

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			"We are inside the shape"
			clippingAntiAliasBits _ 16r7F ]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					morphIdWord _ morphIds at: pixelIndex.
					clippingAntiAliasBits _ morphIdWord bitAnd: 16r7F.
					clippingAntiAlias _ clippingAntiAliasBits * 0.007874.								"1.0/127.0"
					alpha _ alpha * clippingAntiAlias ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAliasBits _ 0]].

	alpha = 0.0 ifFalse: [
		targetWord _ targetBits at: pixelIndex.
		resultRBits _ targetWord bitAnd: 16rFF0000.
		resultGBits _ targetWord bitAnd: 16rFF00.
		resultBBits _ targetWord bitAnd: 16rFF.
		unAlpha _ 1.0 - alpha.
		self cCode: '
				resultR = (alpha * foreR) + (unAlpha * (resultRBits >> 16));
				resultRBits = (uint32_t)(resultR + 0.5) << 16;
				resultG = (alpha * foreG) + (unAlpha * (resultGBits >> 8));
				resultGBits = (uint32_t)(resultG + 0.5) << 8;
				resultB = (alpha * foreB) + (unAlpha * resultBBits);
				resultBBits = (uint32_t)(resultB + 0.5);'
			inSmalltalk: [
				resultR _ alpha * foreR + (unAlpha * (resultRBits bitShift: -16)).
				resultRBits _ (resultR + 0.5) asInteger bitShift: 16.
				resultG _ alpha * foreG + (unAlpha * (resultGBits bitShift: -8)).
				resultGBits _ (resultG + 0.5) asInteger bitShift: 8.
				resultB _ alpha * foreB + (unAlpha * resultBBits).
				resultBBits _ (resultB + 0.5) asInteger ].

		targetWord _ ((16rFF000000 bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		targetBits at: pixelIndex put: targetWord.

		currentMorphId = 0 ifFalse: [
			morphIdWord _ currentMorphId<<8 + clippingAntiAliasBits.
			morphIds at: pixelIndex put: morphIdWord ]].
! !

!VectorEnginePlugin methodsFor: 'whole pixel - blend stroke and fill' stamp: 'jmv 6/28/2021 17:19:03'!
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

	antiAliasAlpha _ antiAliasAlphaBits * 0.007874.								"1.0/127.0"
	"In the stroke, outside the shape"
	alpha _ antiAliasAlpha * strokeA.
	foreR _ strokeR.
	foreG _ strokeG.
	foreB _ strokeB.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			morphIdWord _ morphIds at: pixelIndex.
			clippingAntiAliasBits _ morphIdWord bitAnd: 16r7F.
			antiAliasAlphaBits > clippingAntiAliasBits ifTrue: [
				clippingAntiAliasBits _ antiAliasAlphaBits ]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					morphIdWord _ morphIds at: pixelIndex.
					clippingAntiAliasBits _ morphIdWord bitAnd: 16r7F.
					clippingAntiAlias _ clippingAntiAliasBits * 0.007874.								"1.0/127.0"
					alpha _ alpha * clippingAntiAlias ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAliasBits _ 0]].

	alpha = 0.0 ifFalse: [
		targetWord _ targetBits at: pixelIndex.
		resultRBits _ targetWord bitAnd: 16rFF0000.
		resultGBits _ targetWord bitAnd: 16rFF00.
		resultBBits _ targetWord bitAnd: 16rFF.
		unAlpha _ 1.0 - alpha.
		self cCode: '
				resultR = (alpha * foreR) + (unAlpha * (resultRBits >> 16));
				resultRBits = (uint32_t)(resultR + 0.5) << 16;
				resultG = (alpha * foreG) + (unAlpha * (resultGBits >> 8));
				resultGBits = (uint32_t)(resultG + 0.5) << 8;
				resultB = (alpha * foreB) + (unAlpha * resultBBits);
				resultBBits = (uint32_t)(resultB + 0.5);'
			inSmalltalk: [
				resultR _ alpha * foreR + (unAlpha * (resultRBits bitShift: -16)).
				resultRBits _ (resultR + 0.5) asInteger bitShift: 16.
				resultG _ alpha * foreG + (unAlpha * (resultGBits bitShift: -8)).
				resultGBits _ (resultG + 0.5) asInteger bitShift: 8.
				resultB _ alpha * foreB + (unAlpha * resultBBits).
				resultBBits _ (resultB + 0.5) asInteger ].

		targetWord _ ((16rFF000000 bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		targetBits at: pixelIndex put: targetWord.

		currentMorphId = 0 ifFalse: [
			morphIdWord _ currentMorphId<<8 + clippingAntiAliasBits.
			morphIds at: pixelIndex put: morphIdWord ]].
! !

!VectorEnginePlugin methodsFor: 'whole pixel - blend stroke and fill' stamp: 'jmv 6/29/2021 15:37:21'!
blendStrokeAndFillWPOTLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherByteArray alphaMask: anotherByteArray
	"Blends stroke color and fill color over background.
	Do an appropriate (anti aliased) gradient between stoke color and fill color (or pick just stroke or just fill). Blend this over background.
	WP: Whole pixel anti aliasing.
	OT: Target ignored on input and set to opaque on output.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"
	| pixelIndex edgesUpToThisPixel edgesThisPixel antiAliasAlphaBits |
	self
		primitive: 'primBlendStrokeAndFillWPOT'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger WordArray WordArray ByteArray ByteArray).

	self var: #edgesThisPixel type: 'uint8_t'.
	self var: #edgesUpToThisPixel type: 'uint8_t'.
	self var: #antiAliasAlphaBits type: 'uint8_t'.

	targetBits _ aBitmap.
	morphIds _ aWordArray.
	edgeCountsWP _ otherByteArray.
	alphaMaskWP _ anotherByteArray.
	t to: b do: [ :displayY |
		edgesUpToThisPixel _ 0.
		pixelIndex _ displayY * targetWidth + l - 1.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			edgesThisPixel _ edgeCountsWP at: pixelIndex.
			edgesThisPixel = 0 ifFalse: [
				edgeCountsWP at: pixelIndex put: 0.
				self cCode: '' inSmalltalk: [
					"Not needed in C or OpenCL, just use a wraparound 8 bit type, such uint8_t"
					edgesThisPixel > 127 ifTrue: [ edgesThisPixel _ edgesThisPixel - 256 ]].
				edgesUpToThisPixel _ edgesUpToThisPixel + edgesThisPixel ].
			antiAliasAlphaBits _ alphaMaskWP at: pixelIndex.
			edgesUpToThisPixel = 0
				ifFalse: [				"Inside the shape"
					antiAliasAlphaBits = 16r00 ifFalse: [ alphaMaskWP at: pixelIndex put: 0 ].
					self blendStrokeAndFillInsideWPOTAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits]
				ifTrue: [				"Still in the stroke, but outside the shape, strictly speaking."
					antiAliasAlphaBits = 16r00 ifFalse: [
						alphaMaskWP at: pixelIndex put: 0.
						antiAliasAlphaBits = 0 ifFalse: [self blendStrokeAndFillOutsideWPOTAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits]]]]].! !

!VectorEnginePlugin methodsFor: 'whole pixel - blend stroke and fill' stamp: 'jmv 6/28/2021 17:19:21'!
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

	antiAliasAlpha _ antiAliasAlphaBits * 0.007874.								"1.0/127.0"
	alpha _ antiAliasAlpha * strokeA.

	currentClipsSubmorphs
		ifTrue: [
			"Don't clip us, but do clip submorphs"
			morphIdWord _ morphIds at: pixelIndex.
			clippingAntiAliasBits _ morphIdWord bitAnd: 16r7F.
			antiAliasAlphaBits > clippingAntiAliasBits ifTrue: [
				clippingAntiAliasBits _ antiAliasAlphaBits]]
		ifFalse: [
			clipCurrentMorph
				ifTrue: [
					"Clip ourselves to the border anti aliasing of the morph we are clipping at. Keep it for further use."
					morphIdWord _ morphIds at: pixelIndex.
					clippingAntiAliasBits _ morphIdWord bitAnd: 16r7F.
					clippingAntiAlias _ clippingAntiAliasBits * 0.007874.								"1.0/127.0"
					alpha _ alpha * clippingAntiAlias ]
				ifFalse: [
					"Don't do any additional clipping or preparation for further clipping"
					clippingAntiAliasBits _ 0]].

	alpha = 0.0 ifFalse: [
		targetWord _ targetBits at: pixelIndex.
		resultRBits _ targetWord bitAnd: 16rFF0000.
		resultGBits _ targetWord bitAnd: 16rFF00.
		resultBBits _ targetWord bitAnd: 16rFF.
		unAlpha _ 1.0 - alpha.
		self cCode: '
				resultR = (alpha * strokeR) + (unAlpha * (resultRBits >> 16));
				resultRBits = (uint32_t)(resultR + 0.5) << 16;
				resultG = (alpha * strokeG) + (unAlpha * (resultGBits >> 8));
				resultGBits = (uint32_t)(resultG + 0.5) << 8;
				resultB = (alpha * strokeB) + (unAlpha * resultBBits);
				resultBBits = (uint32_t)(resultB + 0.5);'
			inSmalltalk: [
				resultR _ alpha * strokeR + (unAlpha * (resultRBits bitShift: -16)).
				resultRBits _ (resultR + 0.5) asInteger bitShift: 16.
				resultG _ alpha * strokeG + (unAlpha * (resultGBits bitShift: -8)).
				resultGBits _ (resultG + 0.5) asInteger bitShift: 8.
				resultB _ alpha * strokeB + (unAlpha * resultBBits).
				resultBBits _ (resultB + 0.5) asInteger ].


		targetWord _ ((16rFF000000 bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		targetBits at: pixelIndex put: targetWord.

		currentMorphId = 0 ifFalse: [
			morphIdWord _ currentMorphId<<8 + clippingAntiAliasBits.
			morphIds at: pixelIndex put: morphIdWord ]].! !

!VectorEnginePlugin methodsFor: 'whole pixel - blend stroke and fill' stamp: 'jmv 6/29/2021 15:38:07'!
blendStrokeOnlyWPOTLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray alphaMask: anotherByteArray
	"Blends stroke color over background.
	WP: Whole pixel anti aliasing.
	OT: Target ignored on input and set to opaque on output.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"
	| pixelIndex antiAliasAlphaBits |
	self
		primitive: 'primBlendStrokeOnlyWPOT'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger WordArray WordArray ByteArray).

	self var: #antiAliasAlphaBits type: 'uint8_t'.

	targetBits _ aBitmap.
	morphIds _ aWordArray.
	alphaMaskWP _ anotherByteArray.
	t to: b do: [ :displayY |
		pixelIndex _ displayY * targetWidth + l - 1.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			antiAliasAlphaBits _ alphaMaskWP at: pixelIndex.
			antiAliasAlphaBits = 16r00 ifFalse: [										"In the stroke"
				alphaMaskWP at: pixelIndex put: 0.
				self blendStrokeOnlyWPOTAt: pixelIndex antiAliasAlphaByte: antiAliasAlphaBits ]]].! !

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
