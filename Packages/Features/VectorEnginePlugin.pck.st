'From Cuis 5.0 [latest update: #4567] on 12 April 2021 at 5:07:19 pm'!
'Description '!
!provides: 'VectorEnginePlugin' 1 9!
!requires: 'VectorGraphics' 1 200 nil!
!requires: 'VMMaker' 1 7 nil!
SystemOrganization addCategory: 'VectorEnginePlugin'!


!classDefinition: #VectorEnginePlugin category: 'VectorEnginePlugin'!
SmartSyntaxInterpreterPlugin subclass: #VectorEnginePlugin
	instanceVariableNames: 'targetBits morphIds edgeCounts alphaMask targetWidth targetHeight antiAliasingWidth subPixelDelta strokeWidth strokeR strokeG strokeB strokeA fillR fillG fillB fillA textTxA11 textTxA12 textTxA13 textTxA21 textTxA22 textTxA23 currentMorphId currentClipsSubmorphs clipCurrentMorph clipLeft clipTop clipRight clipBottom auxAntiAliasingWidthScaledInverse auxStrokeWidthDilatedHalf auxStrokeWidthDilatedHalfSquared auxStrokeWidthErodedHalfSquared prevYTruncated spanLeft spanTop spanRight spanBottom'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'VectorEnginePlugin'!
!classDefinition: 'VectorEnginePlugin class' category: 'VectorEnginePlugin'!
VectorEnginePlugin class
	instanceVariableNames: ''!


!VectorEnginePlugin commentStamp: 'jmv 3/25/2021 12:56:08' prior: 0!
"Currently, only with subpixel anti aliasing"
Slang implementation of VectorEngineSubPixel. Based on https://www.researchgate.net/publication/267152327_Prefiltering_Antialiasing_for_General_Vector_Graphics
VectorEnginePlugin translateInDirectory: DirectoryEntry currentDirectory doInlining: true!

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 3/22/2021 14:35:21'!
antiAliasingWidth: aFloat subPixelDelta: otherFloat
	self
		primitive: 'primAntiAliasingWidthsubPixelDelta'
		parameters: #(Float Float).

	antiAliasingWidth _ aFloat.
	auxAntiAliasingWidthScaledInverse _ 127.0 / aFloat.
	subPixelDelta _ otherFloat.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 4/12/2021 16:37:50'!
clipCurrentMorph: aBoolean morphIds: aWordArray
	self
		primitive: 'primClipCurrentMorph'
		parameters: #(Boolean WordArray).

	morphIds _ aWordArray.
	(clipCurrentMorph and: [ aBoolean not ]) ifTrue: [
		| pixelIndex |
		clipTop to: clipBottom do: [ :displayY |
			pixelIndex _ displayY * targetWidth +  clipLeft.
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

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 3/11/2021 11:00:34'!
fillR: r g: g b: b a: a
	self
		primitive: 'primFillRGBA'
		parameters: #(Float Float Float Float).

	fillR _ r.
	fillG _ g.
	fillB _ b.
	fillA _ a.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 3/9/2021 15:10:08'!
initializePath
	self
		primitive: 'primInitializePath'
		parameters: #().

	spanLeft _ targetWidth.						"drawable right. Will later be refined."
	spanTop _ targetHeight.						"drawable bottom. Will later be refined."
	spanRight _ 0.						"drawable left. Will later be refined."
	spanBottom _ 0.						"drawable top. Will later be refined."! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 3/5/2021 16:27:16'!
newTrajectoryFragment
	self
		primitive: 'primNewTrajectoryFragment'
		parameters: #().

	prevYTruncated _ 16r7FFFFFFF.! !

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

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 3/11/2021 11:00:50'!
strokeR: r g: g b: b a: a
	self
		primitive: 'primStrokeRGBA'
		parameters: #(Float Float Float Float).

	strokeR _ r.
	strokeG _ g.
	strokeB _ b.
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

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 3/11/2021 11:01:15'!
targetBits: aBitmap morphIds: aWordArray edgeCounts: otherWordArray alphaMask: anotherWordArray targetWidth: aNumber targetHeight: otherNumber
	"All arrays could be pinned instead of passing them every time!!"

	self
		primitive: 'primSetTarget'
		parameters: #(WordArray WordArray WordArray WordArray SmallInteger SmallInteger).

	targetBits _ aBitmap.
	morphIds _ aWordArray.
	edgeCounts _ otherWordArray.
	alphaMask _ anotherWordArray.
	targetWidth _ aNumber.
	targetHeight _ otherNumber.! !

!VectorEnginePlugin methodsFor: 'accessing' stamp: 'jmv 3/19/2021 11:45:50'!
textTxA11: a11 txA12: a12 txA13: a13 txA21: a21 txA22: a22 txA23: a23
	self
		primitive: 'primTextTxSet'
		parameters: #(Float Float Float Float Float Float).

	textTxA11 _ a11.
	textTxA12 _ a12.
	textTxA13 _ a13.
	textTxA21 _ a21.
	textTxA22 _ a22.
	textTxA23 _ a23.! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 3/19/2021 11:18:52'!
arcCenterX: cx centerY: cy radiusX: rx radiusY: ry start: startAngle rotationCos: thetaCos rotationSin: thetaSin hops: hops angleDelta: d edgeCounts: otherWordArray alphaMask: anotherWordArray

	| angle xp yp x y |
	self
		primitive: 'primArc'
		parameters: #(Float Float Float Float Float Float Float SmallInteger Float WordArray WordArray).
	self var: #angle type: 'float'.
	self var: #xp type: 'float'.
	self var: #yp type: 'float'.
	self var: #x type: 'float'.
	self var: #y type: 'float'.

	edgeCounts _ otherWordArray.
	alphaMask _ anotherWordArray.
	angle _ startAngle.
	1 to: hops+1 do: [ :h |
		xp _ angle cos * rx.
		yp _ angle sin * ry.
		x _ thetaCos * xp - (thetaSin * yp) + cx.
		y _ thetaSin * xp + (thetaCos * yp) + cy.
		spanLeft _ spanLeft min: x.
		spanTop _ spanTop min: y.
		spanRight _ spanRight max: x.
		spanBottom _ spanBottom max: y.
		self updateAlphasForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountAtX: x y: y ].
		angle _ angle + d. ].! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 3/19/2021 11:20:19'!
cubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2 hops: hops edgeCounts: otherWordArray alphaMask: anotherWordArray

	| xMinEnd xMaxEnd yMinEnd yMaxEnd t oneLessT increment f1 f23 f2 f3 f4 x y |
	self
		primitive: 'primCubicBezier'
		parameters: #(Float Float Float Float Float Float Float Float SmallInteger WordArray WordArray).
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

	edgeCounts _ otherWordArray.
	alphaMask _ anotherWordArray.

	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
	xMinEnd _ xFrom min: xTo.
	xMaxEnd _ xFrom max: xTo.
	yMinEnd _ yFrom min: yTo.
	yMaxEnd _ yFrom max: yTo.
	spanLeft _ spanLeft min: (xMinEnd min: xMinEnd*0.25 + ((xControl1 min: xControl2)*0.75)).
	spanRight _ spanRight max: (xMaxEnd max: xMaxEnd*0.25 + ((xControl1 max: xControl2)*0.75)).
	spanTop _ spanTop min: (yMinEnd min: yMinEnd*0.25 + ((yControl1 min: yControl2)*0.75)).
	spanBottom _ spanBottom max: (yMaxEnd max: yMaxEnd*0.25 + ((yControl1 max: yControl2)*0.75)).

	t _ 0.0.
	increment _ 1.0 / hops.
	1 to: hops do: [ :h |
		oneLessT _ 1.0 - t.
		f1 _ oneLessT * oneLessT * oneLessT.
		f23 _ 3.0 * oneLessT * t.
		f2 _ f23 * oneLessT.
		f3 _ f23 * t.
		f4 _ t * t * t.
		x _ (f1 * xFrom) + (f2 * xControl1) + (f3 * xControl2) + (f4 * xTo).
		y _ (f1 * yFrom) + (f2 * yControl1) + (f3 * yControl2) + (f4 * yTo).
		self updateAlphasForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountAtX: x y: y ].
		t _ t + increment. ].
	self updateAlphasForX: xTo y: yTo.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountAtX: xTo y: yTo ].! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 3/19/2021 11:20:34'!
lineFromX: xFrom y: yFrom toX: xTo y: yTo hops: hops incrementX: incrementX incrementY: incrementY edgeCounts: otherWordArray alphaMask: anotherWordArray

	| x y |
	self
		primitive: 'primLine'
		parameters: #(Float Float Float Float SmallInteger Float Float WordArray WordArray).
	self var: #x type: 'float'.
	self var: #y type: 'float'.

	edgeCounts _ otherWordArray.
	alphaMask _ anotherWordArray.

	spanLeft _ spanLeft min: (xFrom min: xTo).
	spanRight _ spanRight max: (xFrom max: xTo).
	spanTop _ spanTop min: (yFrom min: yTo).
	spanBottom _ spanBottom max: (yFrom max: yTo).

	x _ xFrom.
	y _ yFrom.
	1 to: hops do: [ :h |
		self updateAlphasForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountAtX: x y: y ].
		x _ x + incrementX.
		y _ y + incrementY ].
	self updateAlphasForX: xTo y: yTo.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountAtX: xTo y: yTo ].! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 3/19/2021 11:20:56'!
quadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl hops: hops edgeCounts: otherWordArray alphaMask: anotherWordArray

	| xMinEnd xMaxEnd yMinEnd yMaxEnd t oneLessT increment f1 f2 f3 x y |
	self
		primitive: 'primQuadraticBezier'
		parameters: #(Float Float Float Float Float Float SmallInteger WordArray WordArray).
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

	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
	xMinEnd _ xFrom min: xTo.
	xMaxEnd _ xFrom max: xTo.
	yMinEnd _ yFrom min: yTo.
	yMaxEnd _ yFrom max: yTo.
	spanLeft _ spanLeft min: (xMinEnd min: (xMinEnd+xControl) / 2.0).
	spanRight _ spanRight max: (xMaxEnd max: (xMaxEnd+xControl) / 2.0).
	spanTop _ spanTop min: (yMinEnd min: (yMinEnd+yControl) / 2.0).
	spanBottom _ spanBottom max: (yMaxEnd max: (yMaxEnd+yControl) / 2.0).

	t _ 0.0.
	increment _ 1.0 / hops.
	1 to: hops do: [ :h |
		oneLessT _ 1.0 - t.
		f1 _ oneLessT * oneLessT.
		f2 _ 2.0 * oneLessT * t.
		f3 _ t * t.
		x _ (f1 * xFrom) + (f2 * xControl) + (f3 * xTo).
		y _ (f1 * yFrom) + (f2 * yControl) + (f3 * yTo).
		self updateAlphasForX: x y: y.
		fillA = 0.0 ifFalse: [
			self updateEdgeCountAtX: x y: y ].
		t _ t + increment. ].
	self updateAlphasForX: xTo y: yTo.
	fillA = 0.0 ifFalse: [
		self updateEdgeCountAtX: xTo y: yTo ].! !

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 4/12/2021 15:29:08'!
updateAlphasForX: x y: y

	"slight optimization possible when we know width = 0... perhaps implement:
		-we know palpha is never 127
		- we know distanceToEdge is always > erodedHalfWidth.
	In smalltalk, performance icrease 1.5%. Perhaps in c/asm it is more than that...
	Mas importante:
		Cuando width es medio grande, entonces dibujar la bolita rellena de entrada es importante... en los casos en que no necesito calcular la distancia"
	| t b l r pixelIndex candidateAlpha alphaWord redAlpha greenAlpha blueAlpha doUpdate distanceToAxisSquared dy dySquared dx dxSquared dxp |
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
		pixelIndex _ displayY * targetWidth +  l -1.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.
			alphaWord _ alphaMask at: pixelIndex.
			alphaWord = 16r007F7F7F ifFalse: [
				redAlpha _ alphaWord bitAnd: 16r7F0000.
				greenAlpha _ alphaWord bitAnd: 16r7F00.
				blueAlpha _ alphaWord bitAnd: 16r7F.
				doUpdate _ false.
				dy _ displayY - y.
				dySquared _ dy * dy.
				dx _ displayX - x.
				"Red"
				dxp _ dx - subPixelDelta.
				dxSquared _ dxp * dxp.
				distanceToAxisSquared _ dxSquared + dySquared.
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
				dxSquared _ dx * dx.
				distanceToAxisSquared _ dxSquared + dySquared.
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
				dxSquared _ dxp * dxp.
				distanceToAxisSquared _ dxSquared + dySquared.
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

!VectorEnginePlugin methodsFor: 'paths algorithms' stamp: 'jmv 4/12/2021 14:54:14'!
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
	greenOffset _ (x+1) asInteger max: clipLeft.											"take the next green  subpixel center to the right of x"
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
						rest _ countWord  bitAnd: 16rFF.
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

!VectorEnginePlugin methodsFor: 'text' stamp: 'jmv 3/26/2021 11:27:42'!
displayString: aString from: startIndex to: stopIndex atx: destX y: destY contourData: contourData contourDataIndexes: contourDataIndexes useST80Glyphs: aBoolean hopsPerPixel: theHopsPerPixel edgeCounts: otherWordArray alphaMask: anotherWordArray

	| answer nextGlyphX nextGlyphY ttMoveToX ttMoveToY ttEndX ttEndY ttControlX ttControlY advanceWidth numContours contourStartX contourStartY numBeziers startX startY endX endY controlX controlY dx dy hops iso8859s15 i xMinEnd xMaxEnd yMinEnd yMaxEnd t oneLessT increment f1 f2 f3 x y |
	self
		primitive: 'primDisplayString'
		parameters: #(String SmallInteger SmallInteger Float Float FloatArray IntegerArray Boolean SmallInteger WordArray WordArray).
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

	nextGlyphX _ destX.
	nextGlyphY _ destY.
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
			startX _ (ttMoveToX * textTxA11) + (ttMoveToY * textTxA12) + textTxA13.
			startY _ (ttMoveToX * textTxA21) + (ttMoveToY * textTxA22) + textTxA23.
			contourStartX _ startX.
			contourStartY _ startY.
			self newTrajectoryFragment.
			1 to: numBeziers do: [ :idx2 |
				ttEndX _ contourData at: i. i _ i + 1.
				ttEndY _ contourData at: i. i _ i + 1.
				ttControlX _ contourData at: i. i _ i + 1.
				ttControlY _ contourData at: i. i _ i + 1.
				endX _ (ttEndX * textTxA11) + (ttEndY * textTxA12) + startX.
				endY _ (ttEndX * textTxA21) + (ttEndY * textTxA22) + startY.
				controlX _ (ttControlX * textTxA11) + (ttControlY * textTxA12) + startX.
				controlY _ (ttControlX * textTxA21) + (ttControlY * textTxA22) + startY.
				self cCode: '
					dx = fabs(controlX-startX) + fabs(endX-controlX);
					dy = fabs(controlY-startY) + fabs(endY-controlY);'
				inSmalltalk: [
					dx _ (controlX-startX) abs + (endX-controlX) abs.
					dy _ (controlY-startY) abs + (endY-controlY) abs ].
				hops _ ((dx max: dy) * theHopsPerPixel) asInteger + 1.
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
				t _ 0.0.
				increment _ 1.0 / hops.
				1 to: hops do: [ :h |
					oneLessT _ 1.0 - t.
					f1 _ oneLessT * oneLessT.
					f2 _ 2.0 * oneLessT * t.
					f3 _ t * t.
					x _ (f1 * startX) + (f2 * controlX) + (f3 * endX).
					y _ (f1 * startY) + (f2 * controlY) + (f3 * endY).
					self updateAlphasForX: x y: y.
					self updateEdgeCountAtX: x y: y.
					t _ t + increment. ].
				startX _ endX.
				startY _ endY.	
			].
			self updateAlphasForX: endX y: endY.
			self updateEdgeCountAtX: endX y: endY.
			"Similar effect to ensureClosePath in #finishPath:,
			but assume the TrueType definition is essentially right, and there might only be a rounding error.
			So, don't draw a line, but just (possibly) correct edgeCounts"
			self updateEdgeCountAtX: contourStartX y: contourStartY.
		].
		nextGlyphX _ nextGlyphX + advanceWidth.
	].
	answer _ nextGlyphX.
	^answer asOop: Float! !

!VectorEnginePlugin methodsFor: 'text' stamp: 'jmv 3/26/2021 11:27:27'!
displayUtf32: aWordArray from: startIndex to: stopIndex atx: destX y: destY contourData: contourData contourDataIndexes: contourDataIndexes useST80Glyphs: aBoolean hopsPerPixel: theHopsPerPixel edgeCounts: otherWordArray alphaMask: anotherWordArray

	| answer nextGlyphX nextGlyphY ttMoveToX ttMoveToY ttEndX ttEndY ttControlX ttControlY advanceWidth numContours contourStartX contourStartY numBeziers startX startY endX endY controlX controlY dx dy hops utf32 utf8Byte1 uft8Byte2 utf8Byte3 utf8Byte4 i xMinEnd xMaxEnd yMinEnd yMaxEnd t oneLessT increment f1 f2 f3 x y |
	self
		primitive: 'primDisplayUtf32'
		parameters: #(WordArray SmallInteger SmallInteger Float Float FloatArray IntegerArray Boolean SmallInteger WordArray WordArray).
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

	nextGlyphX _ destX.
	nextGlyphY _ destY.
	startIndex-1 to: stopIndex-1 do: [ :utf32Index |
		utf32 _ aWordArray at: utf32Index.
		aBoolean ifTrue: [
			utf32 = 95 ifTrue: [ utf32 _ 8592 ].												"If underscore, use left arrow"
			utf32 = 94 ifTrue: [ utf32 _ 8593 ].												"If caret, use up arrow"
			].
		utf32 <= 16r7F
			ifTrue: [
				utf8Byte1 _ utf32.
				i _ contourDataIndexes at: utf8Byte1.
				]
			ifFalse: [ utf32 <= 16r7FF
			ifTrue: [
				utf8Byte1 _ (utf32 bitShift: -6) bitOr: 2r11000000.
				uft8Byte2 _ (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
				i _ contourDataIndexes at: utf8Byte1.
				i _ contourDataIndexes at: uft8Byte2 - i.
				]
			ifFalse: [ utf32 <= 16rFFFF
			ifTrue: [
				utf8Byte1 _ (utf32 bitShift: -12) bitOr: 2r11100000.
				uft8Byte2 _ ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
				utf8Byte3 _ (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
				i _ contourDataIndexes at: utf8Byte1.
				i _ contourDataIndexes at: uft8Byte2 - i.
				i _ contourDataIndexes at: utf8Byte3 - i.
				]
			ifFalse: [
				utf8Byte1 _ (utf32 bitShift: -18) bitOr: 2r11110000.
				uft8Byte2 _ ((utf32 bitShift: -12) bitAnd: 2r00111111) bitOr: 2r10000000.
				utf8Byte3 _ ((utf32 bitShift: -6) bitAnd: 2r00111111) bitOr: 2r10000000.
				utf8Byte4 _ (utf32 bitAnd: 2r00111111) bitOr: 2r10000000.
				i _ contourDataIndexes at: utf8Byte1.
				i _ contourDataIndexes at: uft8Byte2 - i.
				i _ contourDataIndexes at: utf8Byte3 - i.
				i _ contourDataIndexes at: utf8Byte4 - i.
				]]].
		i _ i - 1.
		advanceWidth _ contourData at: i. i _ i + 5.
		numContours _ (contourData at: i) asInteger. i _ i + 1.
		1 to: numContours do: [ :idx |
			numBeziers _ (contourData at: i) asInteger. i _ i + 1.
			ttMoveToX _ (contourData at: i) + nextGlyphX. i _ i + 1.
			ttMoveToY _ (contourData at: i) + nextGlyphY. i _ i + 1.
			startX _ (ttMoveToX * textTxA11) + (ttMoveToY * textTxA12) + textTxA13.
			startY _ (ttMoveToX * textTxA21) + (ttMoveToY * textTxA22) + textTxA23.
			contourStartX _ startX.
			contourStartY _ startY.
			self newTrajectoryFragment.
			1 to: numBeziers do: [ :idx2 |
				ttEndX _ contourData at: i. i _ i + 1.
				ttEndY _ contourData at: i. i _ i + 1.
				ttControlX _ contourData at: i. i _ i + 1.
				ttControlY _ contourData at: i. i _ i + 1.
				endX _ (ttEndX * textTxA11) + (ttEndY * textTxA12) + startX.
				endY _ (ttEndX * textTxA21) + (ttEndY * textTxA22) + startY.
				controlX _ (ttControlX * textTxA11) + (ttControlY * textTxA12) + startX.
				controlY _ (ttControlX * textTxA21) + (ttControlY * textTxA22) + startY.
				self cCode: '
					dx = fabs(controlX-startX) + fabs(endX-controlX);
					dy = fabs(controlY-startY) + fabs(endY-controlY);'
				inSmalltalk: [
					dx _ (controlX-startX) abs + (endX-controlX) abs.
					dy _ (controlY-startY) abs + (endY-controlY) abs ].
				hops _ ((dx max: dy) * theHopsPerPixel) asInteger + 1.
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
				t _ 0.0.
				increment _ 1.0 / hops.
				1 to: hops do: [ :h |
					oneLessT _ 1.0 - t.
					f1 _ oneLessT * oneLessT.
					f2 _ 2.0 * oneLessT * t.
					f3 _ t * t.
					x _ (f1 * startX) + (f2 * controlX) + (f3 * endX).
					y _ (f1 * startY) + (f2 * controlY) + (f3 * endY).
					self updateAlphasForX: x y: y.
					self updateEdgeCountAtX: x y: y.
					t _ t + increment. ].
				startX _ endX.
				startY _ endY.	
			].
			self updateAlphasForX: endX y: endY.
			self updateEdgeCountAtX: endX y: endY.
			"Similar effect to ensureClosePath in #finishPath:,
			but assume the TrueType definition is essentially right, and there might only be a rounding error.
			So, don't draw a line, but just (possibly) correct edgeCounts"
			self updateEdgeCountAtX: contourStartX y: contourStartY.
		].
		nextGlyphX _ nextGlyphX + advanceWidth.
	].
	answer _ nextGlyphX.
	^answer asOop: Float! !

!VectorEnginePlugin methodsFor: 'text' stamp: 'jmv 3/26/2021 11:27:12'!
displayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY contourData: contourData contourDataIndexes: contourDataIndexes useST80Glyphs: aBoolean hopsPerPixel: theHopsPerPixel edgeCounts: otherWordArray alphaMask: anotherWordArray

	| answer nextGlyphX nextGlyphY ttMoveToX ttMoveToY ttEndX ttEndY ttControlX ttControlY advanceWidth numContours contourStartX contourStartY numBeziers startX startY endX endY controlX controlY dx dy hops baseIndex byte i xMinEnd xMaxEnd yMinEnd yMaxEnd t oneLessT increment f1 f2 f3 x y |
	self
		primitive: 'primDisplayUtf8'
		parameters: #(ByteArray SmallInteger SmallInteger Float Float FloatArray IntegerArray Boolean SmallInteger WordArray WordArray).
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

	nextGlyphX _ destX.
	nextGlyphY _ destY.
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
					startX _ (ttMoveToX * textTxA11) + (ttMoveToY * textTxA12) + textTxA13.
					startY _ (ttMoveToX * textTxA21) + (ttMoveToY * textTxA22) + textTxA23.
					contourStartX _ startX.
					contourStartY _ startY.
					self newTrajectoryFragment.
					1 to: numBeziers do: [ :idx2 |
						ttEndX _ contourData at: i. i _ i + 1.
						ttEndY _ contourData at: i. i _ i + 1.
						ttControlX _ contourData at: i. i _ i + 1.
						ttControlY _ contourData at: i. i _ i + 1.
						endX _ (ttEndX * textTxA11) + (ttEndY * textTxA12) + startX.
						endY _ (ttEndX * textTxA21) + (ttEndY * textTxA22) + startY.
						controlX _ (ttControlX * textTxA11) + (ttControlY * textTxA12) + startX.
						controlY _ (ttControlX * textTxA21) + (ttControlY * textTxA22) + startY.
						self cCode: '
							dx = fabs(controlX-startX) + fabs(endX-controlX);
							dy = fabs(controlY-startY) + fabs(endY-controlY);'
						inSmalltalk: [
							dx _ (controlX-startX) abs + (endX-controlX) abs.
							dy _ (controlY-startY) abs + (endY-controlY) abs ].
						hops _ ((dx max: dy) * theHopsPerPixel) asInteger + 1.
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
						t _ 0.0.
						increment _ 1.0 / hops.
						1 to: hops do: [ :h |
							oneLessT _ 1.0 - t.
							f1 _ oneLessT * oneLessT.
							f2 _ 2.0 * oneLessT * t.
							f3 _ t * t.
							x _ (f1 * startX) + (f2 * controlX) + (f3 * endX).
							y _ (f1 * startY) + (f2 * controlY) + (f3 * endY).
							self updateAlphasForX: x y: y.
							self updateEdgeCountAtX: x y: y.
							t _ t + increment. ].
						startX _ endX.
						startY _ endY.	
					].
					self updateAlphasForX: endX y: endY.
					self updateEdgeCountAtX: endX y: endY.
					"Similar effect to ensureClosePath in #finishPath:,
					but assume the TrueType definition is essentially right, and there might only be a rounding error.
					So, don't draw a line, but just (possibly) correct edgeCounts"
					self updateEdgeCountAtX: contourStartX y: contourStartY.
				].
				nextGlyphX _ nextGlyphX + advanceWidth.
				baseIndex _ 0.
			]
	].
	answer _ nextGlyphX.
	^answer asOop: Float! !

!VectorEnginePlugin methodsFor: 'blend stroke and fill colors' stamp: 'jmv 4/12/2021 16:32:54'!
blendFillOnlyAt: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside

	"targetForm only contains opaque colors. Assume its alpha is always one. Don't ask for it. Set it to one when writing.
	Blends stroke over background.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB resultRBits resultGBits resultBBits resultAlphaBits antiAliasAlphasWord antiAliasRedAlphaBits antiAliasRedAlpha antiAliasGreenAlphaBits antiAliasGreenAlpha antiAliasBlueAlphaBits antiAliasBlueAlpha alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlphaR resultAlphaG resultAlphaB targetAlpha antiAliasGreenAlphaBitsShifted clippingAntiAliasBits clippingAntiAlias morphIdWord |

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

	antiAliasAlphasWord _ alphaMask at: pixelIndex.
	antiAliasAlphasWord = 16r00000000 ifTrue: [		"Not in the stroke"
		(isRedInside or: [  isGreenInside or: [ isBlueInside ]]) ifFalse: [ 		"Not inside"
			^0]].	"Optimization: We are completely transparent at this point. Do nothing."

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
					resultR = ((alphaR * fillR) * 255.0) + ((unAlphaR * (resultRBits >> 16)) * targetAlpha);
					resultRBits = (uint32_t)(resultR  / resultAlphaR + 0.5) << 16;'
				inSmalltalk: [
					resultR _ alphaR * fillR * 255.0 + (unAlphaR * (resultRBits bitShift: -16) * targetAlpha).
					resultRBits _ (resultR / resultAlphaR + 0.5) asInteger bitShift: 16 ]].

		alphaG = 0.0 ifFalse: [
			unAlphaG _ 1.0 - alphaG.
			resultAlphaG _ alphaG + (unAlphaG * targetAlpha).
			self cCode: '
					resultG = ((alphaG * fillG) * 255.0) + ((unAlphaG * (resultGBits >> 8)) * targetAlpha);
					resultGBits = (uint32_t)(resultG / resultAlphaG + 0.5) << 8;
					resultAlphaBits = (uint32_t)(resultAlphaG * 255.0 + 0.5) << 24;'
				inSmalltalk: [
					resultG _ alphaG * fillG * 255.0 + (unAlphaG * (resultGBits bitShift: -8) * targetAlpha).
					resultGBits _ (resultG / resultAlphaG + 0.5) asInteger bitShift: 8.
					resultAlphaBits _ (resultAlphaG * 255.0 + 0.5) asInteger bitShift: 24 ]].

		alphaB = 0.0 ifFalse: [
			unAlphaB _ 1.0 - alphaB.
			resultAlphaB _ alphaB + (unAlphaB * targetAlpha).
			self cCode: '
					resultB = ((alphaB * fillB) * 255.0) + ((unAlphaB * resultBBits) * targetAlpha);
					resultBBits = (uint32_t)(resultB / resultAlphaB + 0.5);'
				inSmalltalk: [
					resultB _ alphaB * fillB * 255.0 + (unAlphaB * resultBBits * targetAlpha).
					resultBBits _ (resultB / resultAlphaB + 0.5) asInteger ]].

		targetWord _ ((resultAlphaBits bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		targetBits at: pixelIndex put: targetWord.

		currentMorphId = 0 ifFalse: [
			morphIdWord _ currentMorphId<<8 + clippingAntiAliasBits.
			morphIds at: pixelIndex put:  morphIdWord ]].
! !

!VectorEnginePlugin methodsFor: 'blend stroke and fill colors' stamp: 'jmv 4/12/2021 16:47:53'!
blendFillOnlyLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherWordArray alphaMask: anotherWordArray
	| pixelIndex edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixelWord edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside |
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

	targetBits _ aBitmap.
	morphIds _ aWordArray.
	edgeCounts _ otherWordArray.
	alphaMask _ anotherWordArray.
	t to: b do: [ :displayY |
		edgesUpToThisPixelR _ 0.
		edgesUpToThisPixelG _ 0.
		edgesUpToThisPixelB _ 0.
		pixelIndex _ displayY * targetWidth +  l - 1.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			edgesThisPixelWord _ edgeCounts at: pixelIndex.
			edgeCounts at: pixelIndex put: 0.

			self cCode: '
					edgesThisPixelR = (uint32_t) (edgesThisPixelWord & 0xFF0000) >> 16;
					edgesThisPixelG = (uint32_t) (edgesThisPixelWord & 0xFF00) >> 8;
					edgesThisPixelB = (uint32_t) (edgesThisPixelWord & 0xFF);'
				inSmalltalk: [
					edgesThisPixelR _ (edgesThisPixelWord bitAnd: 16rFF0000) bitShift: -16.
					edgesThisPixelG _ (edgesThisPixelWord bitAnd: 16rFF00)bitShift: -8.
					edgesThisPixelB _ edgesThisPixelWord bitAnd: 16rFF.
					"Not needed in C or OpenCL, just use a wraparound 8 bit type, such as signed char or unsigned char"
					edgesThisPixelR > 127 ifTrue: [ edgesThisPixelR _ edgesThisPixelR - 256 ].
					edgesThisPixelG > 127 ifTrue: [ edgesThisPixelG _ edgesThisPixelG - 256 ].
					edgesThisPixelB > 127 ifTrue: [ edgesThisPixelB _ edgesThisPixelB - 256 ]].

			edgesUpToThisPixelR _ edgesUpToThisPixelR + edgesThisPixelR.
			edgesUpToThisPixelG _ edgesUpToThisPixelG + edgesThisPixelG.
			edgesUpToThisPixelB _ edgesUpToThisPixelB + edgesThisPixelB.

			self cCode: '
					isRedInside = edgesUpToThisPixelR;
					isGreenInside = edgesUpToThisPixelG;
					isBlueInside = edgesUpToThisPixelB;'
				inSmalltalk: [
					"In C, integers already behave like booleans"
					isRedInside _ edgesUpToThisPixelR ~= 0.
					isGreenInside _ edgesUpToThisPixelG ~= 0.
					isBlueInside _ edgesUpToThisPixelB ~= 0 ].

			self blendFillOnlyAt: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside.

			alphaMask at: pixelIndex put: 0 ]].! !

!VectorEnginePlugin methodsFor: 'blend stroke and fill colors' stamp: 'jmv 4/12/2021 17:00:42'!
blendStrokeAndFillAt: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside

	"targetForm only contains opaque colors. Assume its alpha is always one. Don't ask for it. Set it to one when writing.
	Do an appropriate (anti aliased) mix between stoke and fill (or pick just stroke or just fill). Blend this over background.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB resultRBits resultGBits resultBBits resultAlphaBits antiAliasAlphasWord antiAliasRedAlphaBits antiAliasRedAlpha antiAliasGreenAlphaBits antiAliasGreenAlpha antiAliasBlueAlphaBits antiAliasBlueAlpha foreR foreG foreB alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlphaR resultAlphaG resultAlphaB targetAlpha antiAliasGreenAlphaBitsShifted clippingAntiAliasBits clippingAntiAlias morphIdWord |

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

	antiAliasAlphasWord _ alphaMask at: pixelIndex.
	antiAliasAlphasWord = 16r00000000 ifTrue: [			"Not in the stroke"
		(isRedInside or: [  isGreenInside or: [ isBlueInside ]]) ifFalse: [ 		"Not inside"
			^0]].	"Optimization: We are completely transparent at this point. Do nothing."

	antiAliasRedAlphaBits _ antiAliasAlphasWord bitAnd: 16r7F0000.
	antiAliasGreenAlphaBits _ antiAliasAlphasWord bitAnd: 16r7F00.
	antiAliasBlueAlphaBits _ antiAliasAlphasWord bitAnd: 16r7F.
	antiAliasRedAlpha _ antiAliasRedAlphaBits * (1.0 / (127.0 * 256 * 256)).
	antiAliasGreenAlpha _ antiAliasGreenAlphaBits * (1.0 / (127.0 * 256)).
	antiAliasBlueAlpha _ antiAliasBlueAlphaBits * (1.0 / 127.0).

	isRedInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaR _ antiAliasRedAlpha * strokeA + ((1-antiAliasRedAlpha) * fillA).
			foreR _ antiAliasRedAlpha * strokeR * 255.0 + ((1.0-antiAliasRedAlpha) * fillR * 255.0) ]
		ifFalse: [							"Blend stroke over background"
			alphaR _ antiAliasRedAlpha * strokeA.
			foreR _ strokeR * 255.0 ].

	isGreenInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaG _ antiAliasGreenAlpha * strokeA + ((1-antiAliasGreenAlpha) * fillA).
			foreG _ antiAliasGreenAlpha * strokeG * 255.0 + ((1.0-antiAliasGreenAlpha) * fillG * 255.0) ]
		ifFalse: [							"Blend stroke over background"
			alphaG _ antiAliasGreenAlpha * strokeA.
			foreG _ strokeG * 255.0 ].

	isBlueInside
		ifTrue: [ 							"Do gradient between stroke and fill. Blend the result over background"
			alphaB _ antiAliasBlueAlpha * strokeA + ((1-antiAliasBlueAlpha) * fillA).
			foreB _ antiAliasBlueAlpha * strokeB * 255.0 + ((1.0-antiAliasBlueAlpha) * fillB * 255.0) ]
		ifFalse: [							"Blend stroke over background"
			alphaB _ antiAliasBlueAlpha * strokeA.
			foreB _ strokeB * 255.0 ].

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
					resultRBits = (uint32_t)(resultR  / resultAlphaR + 0.5) << 16;'
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

!VectorEnginePlugin methodsFor: 'blend stroke and fill colors' stamp: 'jmv 4/12/2021 16:48:03'!
blendStrokeAndFillLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherWordArray alphaMask: anotherWordArray
	| pixelIndex edgesUpToThisPixelR edgesUpToThisPixelG edgesUpToThisPixelB edgesThisPixelWord edgesThisPixelR edgesThisPixelG edgesThisPixelB isRedInside isGreenInside isBlueInside |
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

	targetBits _ aBitmap.
	morphIds _ aWordArray.
	edgeCounts _ otherWordArray.
	alphaMask _ anotherWordArray.
	t to: b do: [ :displayY |
		edgesUpToThisPixelR _ 0.
		edgesUpToThisPixelG _ 0.
		edgesUpToThisPixelB _ 0.
		pixelIndex _ displayY * targetWidth +  l - 1.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			edgesThisPixelWord _ edgeCounts at: pixelIndex.
			edgeCounts at: pixelIndex put: 0.

			self cCode: '
					edgesThisPixelR = (uint32_t) (edgesThisPixelWord & 0xFF0000) >> 16;
					edgesThisPixelG = (uint32_t) (edgesThisPixelWord & 0xFF00) >> 8;
					edgesThisPixelB = (uint32_t) (edgesThisPixelWord & 0xFF);'
				inSmalltalk: [
					edgesThisPixelR _ (edgesThisPixelWord bitAnd: 16rFF0000) bitShift: -16.
					edgesThisPixelG _ (edgesThisPixelWord bitAnd: 16rFF00)bitShift: -8.
					edgesThisPixelB _ edgesThisPixelWord bitAnd: 16rFF.
					"Not needed in C or OpenCL, just use a wraparound 8 bit type, such as signed char or unsigned char"
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

			self blendStrokeAndFillAt: pixelIndex redIsInside: isRedInside greenIsInside: isGreenInside blueIsInside: isBlueInside.

			alphaMask at: pixelIndex put: 0 ]].! !

!VectorEnginePlugin methodsFor: 'blend stroke and fill colors' stamp: 'jmv 4/12/2021 16:34:40'!
blendStrokeOnlyAt: pixelIndex

	"targetForm only contains opaque colors. Assume its alpha is always one. Don't ask for it. Set it to one when writing.
	Blends stroke over background.
	For blending, alphas are in [0 .. 1.0] and R, G, B color components are in [0 .. 255]"

	| targetWord resultR resultG resultB resultRBits resultGBits resultBBits resultAlphaBits antiAliasAlphasWord antiAliasRedAlphaBits antiAliasRedAlpha antiAliasGreenAlphaBits antiAliasGreenAlpha antiAliasBlueAlphaBits antiAliasBlueAlpha alphaR alphaG alphaB unAlphaR unAlphaG unAlphaB resultAlphaR resultAlphaG resultAlphaB targetAlpha antiAliasGreenAlphaBitsShifted clippingAntiAliasBits clippingAntiAlias morphIdWord |

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

	antiAliasAlphasWord _ alphaMask at: pixelIndex.
	antiAliasAlphasWord = 16r00000000 ifTrue: [
		^0].	"Optimization: We are completely transparent at this point. Do nothing."

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
					resultR = ((alphaR * strokeR) * 255.0) + ((unAlphaR * (resultRBits >> 16)) * targetAlpha);
					resultRBits = (uint32_t)(resultR  / resultAlphaR + 0.5) << 16;'
				inSmalltalk: [
					resultR _ alphaR * strokeR * 255.0 + (unAlphaR * (resultRBits bitShift: -16) * targetAlpha).
					resultRBits _ (resultR / resultAlphaR + 0.5) asInteger bitShift: 16 ]].

		alphaG = 0.0 ifFalse: [
			unAlphaG _ 1.0 - alphaG.
			resultAlphaG _ alphaG + (unAlphaG * targetAlpha).
			self cCode: '
					resultG = ((alphaG * strokeG) * 255.0) + ((unAlphaG * (resultGBits >> 8)) * targetAlpha);
					resultGBits = (uint32_t)(resultG / resultAlphaG + 0.5) << 8;
					resultAlphaBits = (uint32_t)(resultAlphaG * 255.0 + 0.5) << 24;'
				inSmalltalk: [
					resultG _ alphaG * strokeG * 255.0 + (unAlphaG * (resultGBits bitShift: -8) * targetAlpha).
					resultGBits _ (resultG / resultAlphaG + 0.5) asInteger bitShift: 8.
					resultAlphaBits _ (resultAlphaG * 255.0 + 0.5) asInteger bitShift: 24 ]].

		alphaB = 0.0 ifFalse: [
			unAlphaB _ 1.0 - alphaB.
			resultAlphaB _ alphaB + (unAlphaB * targetAlpha).
			self cCode: '
					resultB = ((alphaB * strokeB) * 255.0) + ((unAlphaB * resultBBits) * targetAlpha);
					resultBBits = (uint32_t)(resultB / resultAlphaB + 0.5);'
				inSmalltalk: [
					resultB _ alphaB * strokeB * 255.0 + (unAlphaB * resultBBits * targetAlpha).
					resultBBits _ (resultB / resultAlphaB + 0.5) asInteger ]].

		targetWord _ ((resultAlphaBits bitOr: resultRBits) bitOr: resultGBits) bitOr: resultBBits.
		targetBits at: pixelIndex put: targetWord.

		currentMorphId = 0 ifFalse: [
			morphIdWord _ currentMorphId<<8 + clippingAntiAliasBits.
			morphIds at: pixelIndex put:  morphIdWord ]].! !

!VectorEnginePlugin methodsFor: 'blend stroke and fill colors' stamp: 'jmv 3/11/2021 11:42:35'!
blendStrokeOnlyLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray alphaMask: anotherWordArray
	| pixelIndex |
	self
		primitive: 'primBlendStrokeOnly'
		parameters: #(SmallInteger SmallInteger SmallInteger SmallInteger WordArray WordArray WordArray).

	targetBits _ aBitmap.
	morphIds _ aWordArray.
	alphaMask _ anotherWordArray.
	t to: b do: [ :displayY |
		pixelIndex _ displayY * targetWidth +  l - 1.
		l to: r do: [ :displayX |
			pixelIndex _ pixelIndex + 1.

			self blendStrokeOnlyAt: pixelIndex.

			alphaMask at: pixelIndex put: 0 ]].! !

!VectorEnginePlugin class methodsFor: 'translation' stamp: 'jmv 4/12/2021 14:49:50'!
declareCVarsIn: aCCodeGenerator


	aCCodeGenerator addHeaderFile: '<stdint.h>'.
	aCCodeGenerator var: 'targetBits' type: 'uint32_t *'.
	aCCodeGenerator var: 'morphIds' type: 'uint32_t *'.
	aCCodeGenerator var: 'edgeCounts' type: 'uint32_t *'.
	aCCodeGenerator var: 'alphaMask' type: 'uint32_t *'.
	aCCodeGenerator var: 'currentMorphId' type: 'uint32_t'.
	#(antiAliasingWidth subPixelDelta strokeWidth strokeR strokeG strokeB strokeA fillR fillG fillB fillA textTxA11 textTxA12 textTxA13 textTxA21 textTxA22 textTxA23 auxAntiAliasingWidthScaledInverse auxStrokeWidthDilatedHalf auxStrokeWidthDilatedHalfSquared auxStrokeWidthErodedHalfSquared spanLeft spanTop spanRight spanBottom) do: [ :varName |
		aCCodeGenerator var: varName type: 'float' ].! !
