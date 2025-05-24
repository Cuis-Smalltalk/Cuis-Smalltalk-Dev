'From Cuis7.3 [latest update: #7120] on 8 April 2025 at 3:22:11 pm'!

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 11:56:21'!
primResetContourTop: t bottom: b
	<primitive: 'primResetContour' module: 'VectorEnginePlugin'>
	self primitiveFailed.! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 3/27/2025 15:13:36'!
primSetClippingSpec: anIntegerArray
	<primitive: 'primSetClippingSpec' module: 'VectorEnginePlugin'>
'falle' print.
	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 11:56:57'!
primUpdateContourLastLine
	<primitive: 'primUpdateContourLastLine' module: 'VectorEnginePlugin'>
	self primitiveFailed.! !

!VectorEngineWithPlugin methodsFor: 'accessing' stamp: 'jmv 3/27/2025 15:08:58'!
clippingSpec: aClippingSpec
	super clippingSpec: aClippingSpec.
	self primSetClippingSpec: clippingSpec.! !


!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 11:50:21'!
primArcCenterX: centerX centerY: centerY radiusX: radiusPointX radiusY: radiusPointY start: startAngle sweep: sweepAngle rotationCos: tthetaCos rotationSin: tthetaSin
	<primitive: 'primArc' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 12:03:11'!
primBlendFillOnlyLeft: l top: t right: r bottom: b
	<primitive: 'primBlendFillOnly' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 12:03:47'!
primBlendStrokeAndFillLeft: l top: t right: r bottom: b
	<primitive: 'primBlendStrokeAndFill' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 12:04:27'!
primBlendStrokeOnlyLeft: l top: t right: r bottom: b
	<primitive: 'primBlendStrokeOnly' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 11:52:20'!
primCubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2
	<primitive: 'primCubicBezier' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 12:15:10'!
primDisplayByteString: aString from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes
	<primitive: 'primDisplayByteString' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 12:15:15'!
primDisplayUtf32: aWordArray from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes
	<primitive: 'primDisplayUtf32' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 12:15:22'!
primDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes
	<primitive: 'primDisplayUtf8' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 11:53:29'!
primLineFromX: xFrom y: yFrom toX: xTo y: yTo
	<primitive: 'primLine' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 11:58:37'!
primPathSequence: aFloat32Array size: s
	<primitive: 'primPathSequence' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 11:54:31'!
primQuadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl
	<primitive: 'primQuadraticBezier' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 15:14:49'!
primTargetBits: aBitmap morphIds: aWordArray edgeCounts: otherWordArray alphaMask: anotherWordArray affectedBits: aByteArray contour: aFloat32Array targetWidth: aNumber targetHeight: otherNumber
	<primitive: 'primSetTarget' module: 'VectorEnginePlugin'>

	self primitiveFailed! !


!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 11:50:28'!
primArcCenterX: centerX centerY: centerY radiusX: radiusPointX radiusY: radiusPointY start: startAngle sweep: sweepAngle rotationCos: tthetaCos rotationSin: tthetaSin
	<primitive: 'primArcWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 12:03:16'!
primBlendFillOnlyLeft: l top: t right: r bottom: b
	<primitive: 'primBlendFillOnlyWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 12:03:52'!
primBlendStrokeAndFillLeft: l top: t right: r bottom: b
	<primitive: 'primBlendStrokeAndFillWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 12:04:32'!
primBlendStrokeOnlyLeft: l top: t right: r bottom: b
	<primitive: 'primBlendStrokeOnlyWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 11:52:26'!
primCubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2
	<primitive: 'primCubicBezierWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 12:15:30'!
primDisplayByteString: aString from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes
	<primitive: 'primDisplayByteStringWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 12:15:37'!
primDisplayUtf32: aWordArray from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes
	<primitive: 'primDisplayUtf32WP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 12:15:43'!
primDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes
	<primitive: 'primDisplayUtf8WP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 11:53:35'!
primLineFromX: xFrom y: yFrom toX: xTo y: yTo
	<primitive: 'primLineWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 11:58:41'!
primPathSequence: aFloat32Array size: s
	<primitive: 'primPathSequenceWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 11:54:36'!
primQuadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl
	<primitive: 'primQuadraticBezierWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 4/8/2025 15:14:53'!
primTargetBits: aBitmap morphIds: aWordArray edgeCounts: otherByteArray alphaMask: anotherByteArray affectedBits: aByteArray contour: aFloat32Array targetWidth: aNumber targetHeight: otherNumber
	<primitive: 'primSetTargetWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !


!VectorEngineWithPlugin methodsFor: 'path algorithms' stamp: 'jmv 4/8/2025 11:50:40'!
arcCenterX: cx centerY: cy radiusX: rx radiusY: ry start: startAngle sweep: sweepAngle rotationCos: thetaCos rotationSin: thetaSin

	self
		primArcCenterX: cx asFloat centerY: cy asFloat
		radiusX: rx asFloat radiusY: ry asFloat
		start: startAngle asFloat
		sweep: sweepAngle asFloat
		rotationCos: thetaCos asFloat rotationSin: thetaSin asFloat! !

!VectorEngineWithPlugin methodsFor: 'path algorithms' stamp: 'jmv 4/8/2025 11:52:31'!
cubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2

	self
		primCubicBezierFromX: xFrom asFloat y: yFrom asFloat
		toX: xTo asFloat y: yTo asFloat
		control1X: xControl1 asFloat y: yControl1 asFloat
		control2X: xControl2 asFloat y: yControl2 asFloat! !

!VectorEngineWithPlugin methodsFor: 'path algorithms' stamp: 'jmv 4/8/2025 11:53:39'!
lineFromX: xFrom y: yFrom toX: xTo y: yTo

	self
		primLineFromX: xFrom asFloat y: yFrom asFloat
		toX: xTo asFloat y: yTo asFloat! !

!VectorEngineWithPlugin methodsFor: 'path algorithms' stamp: 'jmv 4/8/2025 11:54:44'!
quadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl

	self
		primQuadraticBezierFromX: xFrom asFloat y: yFrom asFloat
		toX: xTo asFloat y: yTo asFloat
		controlX: xControl asFloat y: yControl asFloat! !

!VectorEngineWithPlugin methodsFor: 'text and multi paths' stamp: 'jmv 4/8/2025 12:15:59'!
displayByteString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
"
| e f s |
'Try BOTH!!'.
e := VectorEngineWithPluginWholePixel onForm: Display.
e := VectorEngineWithPluginSubPixel onForm: Display.
f := 	FontFamily defaultFamilyAndPointSize fontDescription.
s := 'Hello World!! (Ascii Only. Latin-1 requires an additional single byte table in the Font, that Cuis does not include.'.
e displayByteString: s from: 1 to: s size at: 10@120 trueTypeFont: f color: Color black scale: 30 preventMirroring: true.
Display forceToScreen.
"

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale |
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale asFloat.
	sy := scale * trueTypeScale asFloat.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.	 "Should use a specific table with 256 entries"
	contourDataIndexes := aTTFontDescription contourDataIndexesByUtf8. "Should use a specific table with 256 entries"
	nextGlyphX := aPoint x asFloat.
	nextGlyphY := aPoint y asFloat.
	self fillColor: aColor do: [
		nextGlyphX := self
			primDisplayByteString: aString from: startIndex to: stopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes
		].
	^ nextGlyphX @ nextGlyphY! !

!VectorEngineWithPlugin methodsFor: 'text and multi paths' stamp: 'jmv 4/8/2025 12:16:11'!
displayUtf32: aWordArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
"
| e f utf32 |
'Try BOTH!!'.
e := VectorEngineWithPluginWholePixel onForm: Display.
e := VectorEngineWithPluginSubPixel onForm: Display.
f := 	FontFamily defaultFamilyAndPointSize fontDescription.
utf32 _ #(4314 4317 4320 4308 4315 32 4312 4324 4321 4323 4315 32 4307 4317 4314 4317 4320 32 4321 4312 4311 32 4304 4315 4308 4311 44 32 4327 4323 4317 32 4323 4311 32 4307 4312 4330 4304 4311 32 4304 4314 4312 4327 4323 4312 4307 44 32 4304 4307 32 4314 4312 4305 4320 4312 4321 32 4330 4308 4311 4308 4320 4317 4321 32 4320 4308 4324 4320 4308 4336 4308 4316 4307 4323 4316 4311 32 4308 4321 4311 46 32 4314 4308 4306 4308 4320 4308 32 4330 4317 4320 4324 4317 4320 4304 32 4308 4324 4324 4312 4330 4312 4304 4316 4311 4323 4320 32 4304 4311 32 4308 4304 4315 44 32 4316 4317 32 4327 4323 4304 4321 32 4315 4308 4316 4304 4316 4307 4320 4312 32 4320 4308 4324 4308 4320 4320 4308 4316 4311 4323 4320 32 4327 4323 4312 46 32 4311 4308 32 4324 4323 4311 4308 4316 4311 32 4315 4317 4314 4314 4312 4321 32 4330 4308 4311 4308 4320 4317 4321 32 4309 4312 4321 46 32 4309 4312 4315 32 4307 4308 4316 4312 4327 4323 4308 32 4309 4312 4309 4308 4316 4307 4323 4315 32 4311 4336 4308 4317 4324 4336 4320 4304 4321 4311 4323 4321 32 4308 4334 44 32 4308 4304 32 4336 4304 4321 32 4309 4317 4314 4323 4315 4323 4321 32 4324 4304 4321 4311 4312 4307 4312 4312 32 4307 4308 4321 4308 4320 4323 4316 4311 46 32 4308 4334 32 4317 4324 4324 4312 4330 4312 4312 4321 32 4311 4317 4320 4327 4323 4304 4311 4317 4321 32 4315 4308 4312 44 32 4307 4317 4330 4311 4323 4321 32 4309 4312 4309 4308 4316 4307 4323 4315 32 4312 4316 32 4327 4323 4317 46 32 4321 4323 4315 4315 4317 32 4311 4320 4304 4330 4311 4304 4311 4317 4321 32 4308 4312 32 4312 4323 4321 44 32 4323 4311 32 4317 4315 4316 4308 4321 32 4309 4312 4320 4311 4323 4311 4308 32 4307 4308 4311 4320 4304 4334 4312 4311 46) asWordArray.
e displayUtf32: utf32 from: 1 to: utf32 size at: 10@120 trueTypeFont: f color: Color black scale: 30 preventMirroring: true.
Display forceToScreen."

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale |
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale.
	sy := scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.
	contourDataIndexes := aTTFontDescription contourDataIndexesByUtf8.
	nextGlyphX := aPoint x asFloat.
	nextGlyphY := aPoint y asFloat.
	self fillColor: aColor do: [
		nextGlyphX := self
			primDisplayUtf32: aWordArray from: startIndex to: stopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes
		].
	^ nextGlyphX @ nextGlyphY! !

!VectorEngineWithPlugin methodsFor: 'text and multi paths' stamp: 'jmv 4/8/2025 12:16:20'!
displayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale |
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale.
	sy := scale * trueTypeScale.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.
	contourDataIndexes := aTTFontDescription contourDataIndexesByUtf8.
	nextGlyphX := aPoint x asFloat.
	nextGlyphY := aPoint y asFloat.
	self fillColor: aColor do: [
		nextGlyphX := self
			primDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes
		].
	^ nextGlyphX @ nextGlyphY! !

!VectorEngineWithPlugin methodsFor: 'text and multi paths' stamp: 'jmv 4/8/2025 11:58:59'!
pathSequence: aFloat32Array

	self primPathSequence: aFloat32Array asFloat32Array size: aFloat32Array size! !

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 4/8/2025 12:04:48'!
finishPath: ensureClosePath
	"Blend stroke and/or fill over background, according to the just computed pen alphas (distance from pixel to center of stroke)"
	| spanLeft spanTop spanRight spanBottom |

	"If we are doing a fill, and caller forgot to close the trajectory, then do it anyway."
	ensureClosePath ifTrue: [
		firstX isNaN ifFalse: [
			(firstX = currentX and: [ firstY = currentY ]) ifFalse: [
				"This is actually an error condition. If we are asked to fill, then the path should be closed.
				This is a workaround. If width > 0.0, we'll be adding a visible edge to the path."
				self lineToFirstPoint ]]].
	self primUpdateContourLastLine.

	spanLeft := self primSpanLeft.
	spanTop := self primSpanTop.
	spanRight := self primSpanRight.
	spanBottom := self primSpanBottom.

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
				ifTrue: [ self primBlendStrokeAndFillLeft: spanLeft top: spanTop right: spanRight bottom: spanBottom ]
				ifFalse: [ self primBlendFillOnlyLeft: spanLeft top: spanTop right: spanRight bottom: spanBottom ]]
			ifFalse: [
				self primBlendStrokeOnlyLeft: spanLeft top: spanTop right: spanRight bottom: spanBottom ]].! !

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 4/8/2025 15:15:08'!
initializePath

	super initializePath.

	"Set all plugin state. There might be several instances of this class, all using the same plugin.
	Only one of them will use the plugin during a path draw operation. See class variable AccessLock."
	self
		primTargetBits: targetForm bits
		morphIds: morphIds
		edgeCounts: edgeCounts
		alphaMask: alphaMask
		affectedBits: affectedBits
		contour: contour
		targetWidth: targetWidth
		targetHeight: targetHeight.
	self primTargetAssumedOpaque: targetAssumedOpaque.
"No estoy seguro si hace falta."
	self primSetClippingSpec: clippingSpec.
	self
		primAntiAliasingWidth: antiAliasingWidth asFloat
		subPixelDelta: self subPixelDelta asFloat.
	self primClipLeft: clipLeft clipTop: clipTop clipRight: clipRight clipBottom: clipBottom.
	self primCurrentMorphId: currentMorphId.
	self
		primGeometryTxA11: geometryTransformation a11 asFloat
		a12: geometryTransformation a12 asFloat
		a13: geometryTransformation a13 asFloat
		a21: geometryTransformation a21 asFloat
		a22: geometryTransformation a22 asFloat
		a23: geometryTransformation a23 asFloat.

	self primInitializePath.! !

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 4/8/2025 11:56:30'!
resetMorphBounds

	"Clear any possible contour."
	morphBoundsTop ifNotNil: [
		self
			primResetContourTop: (morphBoundsTop max: 0)
			bottom: (morphBoundsBottom min: targetHeight -1) ].
	super resetMorphBounds.! !

!methodRemoval: VectorEngineWithPluginWholePixel #primCubicBezierFromX:y:toX:y:control1X:y:control2X:y:edgeCounts:alphaMask:contour: stamp: 'jmv 4/8/2025 15:09:19'!
VectorEngineWithPluginWholePixel removeSelector: #primCubicBezierFromX:y:toX:y:control1X:y:control2X:y:edgeCounts:alphaMask:contour:!
!methodRemoval: VectorEngineWithPluginWholePixel #primArcCenterX:centerY:radiusX:radiusY:start:sweep:rotationCos:rotationSin:edgeCounts:alphaMask:contour: stamp: 'jmv 4/8/2025 15:09:20'!
VectorEngineWithPluginWholePixel removeSelector: #primArcCenterX:centerY:radiusX:radiusY:start:sweep:rotationCos:rotationSin:edgeCounts:alphaMask:contour:!
!methodRemoval: VectorEngineWithPluginWholePixel #primQuadraticBezierFromX:y:toX:y:controlX:y:edgeCounts:alphaMask:contour: stamp: 'jmv 4/8/2025 15:09:21'!
VectorEngineWithPluginWholePixel removeSelector: #primQuadraticBezierFromX:y:toX:y:controlX:y:edgeCounts:alphaMask:contour:!
!methodRemoval: VectorEngineWithPluginWholePixel #primBlendStrokeAndFillLeft:top:right:bottom:targetBits:morphIds:edgeCounts:alphaMask: stamp: 'jmv 4/8/2025 15:09:21'!
VectorEngineWithPluginWholePixel removeSelector: #primBlendStrokeAndFillLeft:top:right:bottom:targetBits:morphIds:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPluginWholePixel #primPathSequence:size:edgeCounts:alphaMask:contour: stamp: 'jmv 4/8/2025 15:09:22'!
VectorEngineWithPluginWholePixel removeSelector: #primPathSequence:size:edgeCounts:alphaMask:contour:!
!methodRemoval: VectorEngineWithPluginWholePixel #primBlendStrokeOnlyLeft:top:right:bottom:targetBits:morphIds:alphaMask: stamp: 'jmv 4/8/2025 15:09:22'!
VectorEngineWithPluginWholePixel removeSelector: #primBlendStrokeOnlyLeft:top:right:bottom:targetBits:morphIds:alphaMask:!
!methodRemoval: VectorEngineWithPluginWholePixel #primDisplayByteString:from:to:atx:y:scalex:y:contourData:contourDataIndexes:edgeCounts:alphaMask: stamp: 'jmv 4/8/2025 15:09:19'!
VectorEngineWithPluginWholePixel removeSelector: #primDisplayByteString:from:to:atx:y:scalex:y:contourData:contourDataIndexes:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPluginWholePixel #primTargetBits:morphIds:edgeCounts:alphaMask:contour:targetWidth:targetHeight: stamp: 'jmv 4/8/2025 15:15:11'!
VectorEngineWithPluginWholePixel removeSelector: #primTargetBits:morphIds:edgeCounts:alphaMask:contour:targetWidth:targetHeight:!
!methodRemoval: VectorEngineWithPluginWholePixel #primDisplayUtf8:fromByte:toByte:atx:y:scalex:y:contourData:contourDataIndexes:edgeCounts:alphaMask: stamp: 'jmv 4/8/2025 15:09:20'!
VectorEngineWithPluginWholePixel removeSelector: #primDisplayUtf8:fromByte:toByte:atx:y:scalex:y:contourData:contourDataIndexes:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPluginWholePixel #primLineFromX:y:toX:y:edgeCounts:alphaMask:contour: stamp: 'jmv 4/8/2025 15:09:23'!
VectorEngineWithPluginWholePixel removeSelector: #primLineFromX:y:toX:y:edgeCounts:alphaMask:contour:!
!methodRemoval: VectorEngineWithPluginWholePixel #primBlendFillOnlyLeft:top:right:bottom:targetBits:morphIds:edgeCounts:alphaMask: stamp: 'jmv 4/8/2025 15:09:24'!
VectorEngineWithPluginWholePixel removeSelector: #primBlendFillOnlyLeft:top:right:bottom:targetBits:morphIds:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPluginWholePixel #primDisplayUtf32:from:to:atx:y:scalex:y:contourData:contourDataIndexes:edgeCounts:alphaMask: stamp: 'jmv 4/8/2025 15:09:23'!
VectorEngineWithPluginWholePixel removeSelector: #primDisplayUtf32:from:to:atx:y:scalex:y:contourData:contourDataIndexes:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPluginSubPixel #primCubicBezierFromX:y:toX:y:control1X:y:control2X:y:edgeCounts:alphaMask:contour: stamp: 'jmv 4/8/2025 15:09:25'!
VectorEngineWithPluginSubPixel removeSelector: #primCubicBezierFromX:y:toX:y:control1X:y:control2X:y:edgeCounts:alphaMask:contour:!
!methodRemoval: VectorEngineWithPluginSubPixel #primArcCenterX:centerY:radiusX:radiusY:start:sweep:rotationCos:rotationSin:edgeCounts:alphaMask:contour: stamp: 'jmv 4/8/2025 15:09:25'!
VectorEngineWithPluginSubPixel removeSelector: #primArcCenterX:centerY:radiusX:radiusY:start:sweep:rotationCos:rotationSin:edgeCounts:alphaMask:contour:!
!methodRemoval: VectorEngineWithPluginSubPixel #primQuadraticBezierFromX:y:toX:y:controlX:y:edgeCounts:alphaMask:contour: stamp: 'jmv 4/8/2025 15:09:26'!
VectorEngineWithPluginSubPixel removeSelector: #primQuadraticBezierFromX:y:toX:y:controlX:y:edgeCounts:alphaMask:contour:!
!methodRemoval: VectorEngineWithPluginSubPixel #primBlendStrokeAndFillLeft:top:right:bottom:targetBits:morphIds:edgeCounts:alphaMask: stamp: 'jmv 4/8/2025 15:09:26'!
VectorEngineWithPluginSubPixel removeSelector: #primBlendStrokeAndFillLeft:top:right:bottom:targetBits:morphIds:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPluginSubPixel #primPathSequence:size:edgeCounts:alphaMask:contour: stamp: 'jmv 4/8/2025 15:09:27'!
VectorEngineWithPluginSubPixel removeSelector: #primPathSequence:size:edgeCounts:alphaMask:contour:!
!methodRemoval: VectorEngineWithPluginSubPixel #primBlendStrokeOnlyLeft:top:right:bottom:targetBits:morphIds:alphaMask: stamp: 'jmv 4/8/2025 15:09:27'!
VectorEngineWithPluginSubPixel removeSelector: #primBlendStrokeOnlyLeft:top:right:bottom:targetBits:morphIds:alphaMask:!
!methodRemoval: VectorEngineWithPluginSubPixel #primDisplayByteString:from:to:atx:y:scalex:y:contourData:contourDataIndexes:edgeCounts:alphaMask: stamp: 'jmv 4/8/2025 15:09:24'!
VectorEngineWithPluginSubPixel removeSelector: #primDisplayByteString:from:to:atx:y:scalex:y:contourData:contourDataIndexes:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPluginSubPixel #primTargetBits:morphIds:edgeCounts:alphaMask:contour:targetWidth:targetHeight: stamp: 'jmv 4/8/2025 15:15:10'!
VectorEngineWithPluginSubPixel removeSelector: #primTargetBits:morphIds:edgeCounts:alphaMask:contour:targetWidth:targetHeight:!
!methodRemoval: VectorEngineWithPluginSubPixel #primDisplayUtf8:fromByte:toByte:atx:y:scalex:y:contourData:contourDataIndexes:edgeCounts:alphaMask: stamp: 'jmv 4/8/2025 15:09:25'!
VectorEngineWithPluginSubPixel removeSelector: #primDisplayUtf8:fromByte:toByte:atx:y:scalex:y:contourData:contourDataIndexes:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPluginSubPixel #primLineFromX:y:toX:y:edgeCounts:alphaMask:contour: stamp: 'jmv 4/8/2025 15:09:27'!
VectorEngineWithPluginSubPixel removeSelector: #primLineFromX:y:toX:y:edgeCounts:alphaMask:contour:!
!methodRemoval: VectorEngineWithPluginSubPixel #primBlendFillOnlyLeft:top:right:bottom:targetBits:morphIds:edgeCounts:alphaMask: stamp: 'jmv 4/8/2025 15:09:29'!
VectorEngineWithPluginSubPixel removeSelector: #primBlendFillOnlyLeft:top:right:bottom:targetBits:morphIds:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPluginSubPixel #primDisplayUtf32:from:to:atx:y:scalex:y:contourData:contourDataIndexes:edgeCounts:alphaMask: stamp: 'jmv 4/8/2025 15:09:28'!
VectorEngineWithPluginSubPixel removeSelector: #primDisplayUtf32:from:to:atx:y:scalex:y:contourData:contourDataIndexes:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPlugin #primResetContourTop:bottom:contour: stamp: 'jmv 4/8/2025 15:09:30'!
VectorEngineWithPlugin removeSelector: #primResetContourTop:bottom:contour:!
!methodRemoval: VectorEngineWithPlugin #primUpdateContourLastLine: stamp: 'jmv 4/8/2025 15:09:31'!
VectorEngineWithPlugin removeSelector: #primUpdateContourLastLine:!

!VectorEngineWithPluginSubPixel reorganize!
('accessing' disableSubPixelSampling subPixelDelta)
('plugin calls' primArcCenterX:centerY:radiusX:radiusY:start:sweep:rotationCos:rotationSin: primBlendFillOnlyLeft:top:right:bottom: primBlendStrokeAndFillLeft:top:right:bottom: primBlendStrokeOnlyLeft:top:right:bottom: primCubicBezierFromX:y:toX:y:control1X:y:control2X:y: primDisplayByteString:from:to:atx:y:scalex:y:contourData:contourDataIndexes: primDisplayUtf32:from:to:atx:y:scalex:y:contourData:contourDataIndexes: primDisplayUtf8:fromByte:toByte:atx:y:scalex:y:contourData:contourDataIndexes: primLineFromX:y:toX:y: primPathSequence:size: primQuadraticBezierFromX:y:toX:y:controlX:y: primTargetBits:morphIds:edgeCounts:alphaMask:affectedBits:contour:targetWidth:targetHeight:)
('private' auxPixelDataClass pvtSetForm:)
!


!VectorEngineWithPluginWholePixel reorganize!
('plugin calls' primArcCenterX:centerY:radiusX:radiusY:start:sweep:rotationCos:rotationSin: primBlendFillOnlyLeft:top:right:bottom: primBlendStrokeAndFillLeft:top:right:bottom: primBlendStrokeOnlyLeft:top:right:bottom: primCubicBezierFromX:y:toX:y:control1X:y:control2X:y: primDisplayByteString:from:to:atx:y:scalex:y:contourData:contourDataIndexes: primDisplayUtf32:from:to:atx:y:scalex:y:contourData:contourDataIndexes: primDisplayUtf8:fromByte:toByte:atx:y:scalex:y:contourData:contourDataIndexes: primLineFromX:y:toX:y: primPathSequence:size: primQuadraticBezierFromX:y:toX:y:controlX:y: primTargetBits:morphIds:edgeCounts:alphaMask:affectedBits:contour:targetWidth:targetHeight:)
('private' auxPixelDataClass)
!

