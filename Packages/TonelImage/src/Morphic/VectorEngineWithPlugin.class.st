"
"Just an experiment to exercise drawing operations."

| m |
m := Sample01Star new.
m morphPosition: 150@120.
(VectorCanvas onForm: Display copy) fullDraw: m; inspect.
Display forceToScreen.

"Not sure if fallback code should use a pluginSmalltalkCode ivar, calling the VectorEnginePlugin in smalltalk, or duplicate that code here for fallback code (this would be better if those start to diverge because of optimizations)"
"
Class {
	#name : 'VectorEngineWithPlugin',
	#superclass : 'VectorEngineDrawer',
	#classVars : [
		'AccessLock'
	],
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'instance creation' }
VectorEngineWithPlugin class >> isPluginAvailable [
	"
	VectorEngineWithPlugin isPluginAvailable
	Check if services provided by VM plugin match this class requirements.
	"
	| pluginApiVersion |
	pluginApiVersion := self pluginApiVersion.
	^ pluginApiVersion = 7
]

{ #category : 'private' }
VectorEngineWithPlugin class >> pluginApiVersion [
	"
	VectorEngineWithPlugin isPluginAvailable
	Note: this is Api version, not package version. If no Api change, Api version doesn't change, regardless of changes in the Plugin or Smalltalk code.
	See senders and implementors.
	"
	<primitive: 'pluginApiVersion' module: 'VectorEnginePlugin'>

	^ 0
]

{ #category : 'private' }
VectorEngineWithPlugin class >> protected: aBlock [
	"VectorEnginePlugin is not reentrant.
	- Any proces that is not the main UI process must wait.
	- The main UI process will take the plugin without caring of some other process drawing breaks."
	
	(AccessLock notNil and: [ AccessLock ~= Processor activeProcess ]) ifTrue: [
		"Unless we are running the UI, wait"
		UISupervisor isUIProcessRunning ifFalse: [
			('VectorEnginePlugin is in use by: ', AccessLock printString) print.
			'Wait a bit.' print. 
			UISupervisor whenUIinSafeState: [ Processor activeProcess resume ].
			Processor activeProcess suspend ].
	].
	AccessLock := Processor activeProcess.
	aBlock value.
	AccessLock := nil.
]

{ #category : 'path algorithms' }
VectorEngineWithPlugin >> arcCenterX: cx centerY: cy radiusX: rx radiusY: ry start: startAngle sweep: sweepAngle rotationCos: thetaCos rotationSin: thetaSin [

	self
		primArcCenterX: cx asFloat centerY: cy asFloat
		radiusX: rx asFloat radiusY: ry asFloat
		start: startAngle asFloat
		sweep: sweepAngle asFloat
		rotationCos: thetaCos asFloat rotationSin: thetaSin asFloat
]

{ #category : 'accessing' }
VectorEngineWithPlugin >> clippingSpec: aClippingSpecOrNil [
	clippingSpec := aClippingSpecOrNil.
	clippingSpec
		ifNil: [ self primClearClippingSpec ]
		ifNotNil: [
			clippingSpec pin.
			self primSetClippingSpec: clippingSpec ].
]

{ #category : 'path algorithms' }
VectorEngineWithPlugin >> cubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2 [

	self
		primCubicBezierFromX: xFrom asFloat y: yFrom asFloat
		toX: xTo asFloat y: yTo asFloat
		control1X: xControl1 asFloat y: yControl1 asFloat
		control2X: xControl2 asFloat y: yControl2 asFloat
]

{ #category : 'private' }
VectorEngineWithPlugin >> doPath: pathCommandsBlock ensureClosePath: ensureClosePath [
	"Evaluate a sequence of path commands. Path parameters already set.
	VectorEnginePlugin is not reentrant.
	- Any other process must wait.
	- Nested calls in the same process are not supported, and ignored."

	self class protected: [
		super doPath: pathCommandsBlock ensureClosePath: ensureClosePath ].
]

{ #category : 'text and multi paths' }
VectorEngineWithPlugin >> drawByteString: aString from: startIndex to: stopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring [
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
"
| e f s |
'Try all FOUR of the,!'.
e := VectorEngineWholePixel onForm: Display.
e := VectorEngineSubPixel onForm: Display.
e := VectorEngineWithPluginWholePixel onForm: Display.
e := VectorEngineWithPluginSubPixel onForm: Display.
f := 	FontFamily defaultFamilyAndPointSize fontDescription.
s := 'Hello World! (Ascii Only in this example. Latin-1 requires an additional single byte table in the Font, that Cuis does not include.' asByteString.
e drawByteString: s from: 1 to: s size at: 10@120 trueTypeFont: f color: Color black scale: 30 preventMirroring: true.
Display forceToScreen.
"

	| nextGlyphX nextGlyphY sx sy contourData contourDataIndexes trueTypeScale |
	trueTypeScale := 1.0 / aTTFontDescription letterMTopSideBearing.	"Perhaps measures in TTFontDescription should be normalized"
	sx := scale * trueTypeScale asFloat.
	sy := scale * trueTypeScale asFloat.
	(doPreventMirroring and: [ geometryTransformation doesMirror not ]) ifTrue: [ sy := sy negated ].
	contourData := aTTFontDescription contourDataForUtf8.
	contourDataIndexes := aTTFontDescription contourDataIndexesByLatin1.
	nextGlyphX := aPoint x asFloat.
	nextGlyphY := aPoint y asFloat.
	self fillColor: aColor do: [
		nextGlyphX := self
			primDisplayByteString: aString from: startIndex to: stopIndex
			atx: nextGlyphX y: nextGlyphY
			scalex: sx y: sy
			contourData: contourData
			contourDataIndexes: contourDataIndexes ].
	^ nextGlyphX @ nextGlyphY.
]

{ #category : 'text and multi paths' }
VectorEngineWithPlugin >> drawUtf32String: aWordArray from: startIndex to: stopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring [
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
"
| e f utf32 |
'Try All four!'.
e := VectorEngineWholePixel onForm: Display.
e := VectorEngineSubPixel onForm: Display.
e := VectorEngineWithPluginWholePixel onForm: Display.
e := VectorEngineWithPluginSubPixel onForm: Display.
f := 	FontFamily defaultFamilyAndPointSize fontDescription.
utf32 _ #(4314 4317 4320 4308 4315 32 4312 4324 4321 4323 4315 32 4307 4317 4314 4317 4320 32 4321 4312 4311 32 4304 4315 4308 4311 44 32 4327 4323 4317 32 4323 4311 32 4307 4312 4330 4304 4311 32 4304 4314 4312 4327 4323 4312 4307 44 32 4304 4307 32 4314 4312 4305 4320 4312 4321 32 4330 4308 4311 4308 4320 4317 4321 32 4320 4308 4324 4320 4308 4336 4308 4316 4307 4323 4316 4311 32 4308 4321 4311 46 32 4314 4308 4306 4308 4320 4308 32 4330 4317 4320 4324 4317 4320 4304 32 4308 4324 4324 4312 4330 4312 4304 4316 4311 4323 4320 32 4304 4311 32 4308 4304 4315 44 32 4316 4317 32 4327 4323 4304 4321 32 4315 4308 4316 4304 4316 4307 4320 4312 32 4320 4308 4324 4308 4320 4320 4308 4316 4311 4323 4320 32 4327 4323 4312 46 32 4311 4308 32 4324 4323 4311 4308 4316 4311 32 4315 4317 4314 4314 4312 4321 32 4330 4308 4311 4308 4320 4317 4321 32 4309 4312 4321 46 32 4309 4312 4315 32 4307 4308 4316 4312 4327 4323 4308 32 4309 4312 4309 4308 4316 4307 4323 4315 32 4311 4336 4308 4317 4324 4336 4320 4304 4321 4311 4323 4321 32 4308 4334 44 32 4308 4304 32 4336 4304 4321 32 4309 4317 4314 4323 4315 4323 4321 32 4324 4304 4321 4311 4312 4307 4312 4312 32 4307 4308 4321 4308 4320 4323 4316 4311 46 32 4308 4334 32 4317 4324 4324 4312 4330 4312 4312 4321 32 4311 4317 4320 4327 4323 4304 4311 4317 4321 32 4315 4308 4312 44 32 4307 4317 4330 4311 4323 4321 32 4309 4312 4309 4308 4316 4307 4323 4315 32 4312 4316 32 4327 4323 4317 46 32 4321 4323 4315 4315 4317 32 4311 4320 4304 4330 4311 4304 4311 4317 4321 32 4308 4312 32 4312 4323 4321 44 32 4323 4311 32 4317 4315 4316 4308 4321 32 4309 4312 4320 4311 4323 4311 4308 32 4307 4308 4311 4320 4304 4334 4312 4311 46) asWordArray.
e drawUtf32String: utf32 from: 1 to: utf32 size at: 10@120 trueTypeFont: f color: Color black scale: 30 preventMirroring: true.
Display forceToScreen.
"
"
| e f utf8a utf8b utf8c utf32a utf32b utf32c |
e := VectorEngineWithPluginWholePixel onForm: Display.
e := VectorEngineWithPluginSubPixel onForm: Display.
e := VectorEngineWholePixel onForm: Display.
e := VectorEngineSubPixel onForm: Display.
f := 	FontFamily defaultFamilyAndPointSize fontDescription.
utf8a := #[105 105 204 129 32 105 204 128 32 105 204 130 32 105 204 131 32 105 204 132 32 105 204 133 32 105 204 134 32 105 204 135 32 105 204 136 32 105 204 137 ].
utf8b := #[97 	195 169 97 195 168 	97 	195 177 97 	195 145 195 188 64 97 	101 204 129 97 101 204 128 97 	110 204 131 97 	78 204 131 117 204 136 97	].
utf8c := #[97 	195 169 97 195 168 	97 	195 177 97 	195 145 195 188 105 195 173 64 97 	101 204 129 97 101 204 128 97 	110 204 131 97 	78 204 131 117 204 136 97 105 105 204 129 	].
utf32a := #(105 105 769 32 105 768 32 105 770 32 105 771 32 105 772 32 105 773 32 105 774 32 105 775 32 105 776 32 105 777) asWordArray.
utf32b :=  #(97 233 97 232 97 241 97 209 252 64 97 101 769 97 101 768 97 110 771 97 78 771 117 776 97) asWordArray.
utf32c :=  #(97 233 97 232 97 241 97 209 252 105 237 64 97 101 769 97 101 768 97 110 771 97 78 771 117 776 97 105 105 769) asWordArray.
e drawUtf8String: utf8a fromByte: 1 toByte: utf8a size at: 10@120 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
e drawUtf32String: utf32a from: 1 to: utf32a size at: 10@220 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
e drawUtf8String: utf8b fromByte: 1 toByte: utf8b size at: 10@320 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
e drawUtf32String: utf32b from: 1 to: utf32b size at: 10@420 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
e drawUtf8String: utf8c fromByte: 1 toByte: utf8c size at: 10@520 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
e drawUtf32String: utf32c from: 1 to: utf32c size at: 10@620 trueTypeFont: f color: Color black scale: 50 preventMirroring: true.
Display forceToScreen.
"

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
			contourDataIndexes: contourDataIndexes ].
	^ nextGlyphX @ nextGlyphY.
]

{ #category : 'text and multi paths' }
VectorEngineWithPlugin >> drawUtf8String: aByteArray fromByte: byteStartIndex toByte: byteStopIndex at: aPoint trueTypeFont: aTTFontDescription color: aColor scale: scale preventMirroring: doPreventMirroring [
	"TrueType assumes a Math like coordinate system (Y increasing upwards).
	But Display and Form Y coordinates in Cuis increase downwards, as it is done in most computer hardware and software.
	If doPreventMirroring is true, ensure that glyphs don't look mirrored, by having a negative y scale."
	"
	| e f s byteStartIndex byteStopIndex |
	'-----Try all FOUR of them!' print.
	Display fillColor: Color veryLightGray.
	e := VectorEngineWholePixel onForm: Display.
	e := VectorEngineSubPixel onForm: Display.
	e := VectorEngineWithPluginWholePixel onForm: Display.
	e := VectorEngineWithPluginSubPixel onForm: Display.
	f := 	FontFamily defaultFamilyAndPointSize fontDescription.
	s := 'Hello World! This is TrueType text rasterization in VectorEngine.' asUnicodeString.
	byteStartIndex := s byteIndexAt: 1.
	byteStopIndex := (s byteIndexAt: s size+1)-1.
	[e drawUtf8String: s asUtf8Bytes fromByte: byteStartIndex toByte: byteStopIndex at: 10@150 trueTypeFont: f color: Color random scale: 130 preventMirroring: true] timeToRun print.
	[e drawUtf8String: s asUtf8Bytes fromByte: byteStartIndex toByte: byteStopIndex at: 10@300 trueTypeFont: f color: Color random strokeWidth: 2 strokeColor: Color black scale: 130 preventMirroring: true] timeToRun print.
	Display forceToScreen.
	"
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
			contourDataIndexes: contourDataIndexes ].
	^ nextGlyphX @ nextGlyphY.
]

{ #category : 'private' }
VectorEngineWithPlugin >> finishPath: ensureClosePath [
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
		self clippingSpec: clippingSpec.
		fillColor notNil ifTrue: [
			strokeColor notNil
				ifTrue: [ self primBlendStrokeAndFillLeft: spanLeft top: spanTop right: spanRight bottom: spanBottom ]
				ifFalse: [ self primBlendFillOnlyLeft: spanLeft top: spanTop right: spanRight bottom: spanBottom ]]
			ifFalse: [
				self primBlendStrokeOnlyLeft: spanLeft top: spanTop right: spanRight bottom: spanBottom ]].
]

{ #category : 'private' }
VectorEngineWithPlugin >> initializePath [

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

	self primInitializePath.
]

{ #category : 'path algorithms' }
VectorEngineWithPlugin >> lineFromX: xFrom y: yFrom toX: xTo y: yTo [

	self
		primLineFromX: xFrom asFloat y: yFrom asFloat
		toX: xTo asFloat y: yTo asFloat
]

{ #category : 'private' }
VectorEngineWithPlugin >> newTrajectoryFragment [

	self primNewTrajectoryFragment.
]

{ #category : 'text and multi paths' }
VectorEngineWithPlugin >> pathSequence: aFloat32Array [

	self primPathSequence: aFloat32Array asFloat32Array size: aFloat32Array size
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primAntiAliasingWidth: aNumber subPixelDelta: otherFloat [
	<primitive: 'primAntiAliasingWidthsubPixelDelta' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primClearClippingSpec [
	<primitive: 'primClearClippingSpec' module: 'VectorEnginePlugin'>
	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primClipLeft: l clipTop: t clipRight: r clipBottom: b [
	<primitive: 'primClipLeftclipTopclipRightclipBottom' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primCurrentMorphId: aNumber [
	<primitive: 'primCurrentMorphId' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primDashedStrokeBits: onOffBitSequence dashBitCount: numberOfBitsInSequence dashBitLength: lengthOfEachBit dashBitOffset: offset [
	<primitive: 'dashedStrokeBitsSet' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'private' }
VectorEngineWithPlugin >> primDisplayByteString: aString from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes [
	self subclassResponsibility
]

{ #category : 'private' }
VectorEngineWithPlugin >> primDisplayUtf32: aWordArray from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes [
	self subclassResponsibility
]

{ #category : 'private' }
VectorEngineWithPlugin >> primDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes [
	self subclassResponsibility
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primFillR: r g: g b: b a: a [
	<primitive: 'primFillRGBA' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primGeometryTxA11: a11 a12: a12 a13: a13 a21: a21 a22: a22 a23: a23 [
	<primitive: 'primGeometryTxSet' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primInitializePath [
	<primitive: 'primInitializePath' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primNewTrajectoryFragment [
	<primitive: 'primNewTrajectoryFragment' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primResetContourTop: t bottom: b [
	<primitive: 'primResetContour' module: 'VectorEnginePlugin'>
	self primitiveFailed.
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primSetClippingSpec: anIntegerArray [
	<primitive: 'primSetClippingSpec' module: 'VectorEnginePlugin'>
	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primSpanBottom [
	<primitive: 'primSpanBottom' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primSpanLeft [
	<primitive: 'primSpanLeft' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primSpanRight [
	<primitive: 'primSpanRight' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primSpanTop [
	<primitive: 'primSpanTop' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primStrokeR: r g: g b: b a: a [
	<primitive: 'primStrokeRGBA' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primStrokeWidth: aNumber hop: aFloat [
	<primitive: 'primStrokeWidthHop' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primTargetAssumedOpaque: aBoolean [
	<primitive: 'primTargetAssumedOpaque' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPlugin >> primUpdateContourLastLine [
	<primitive: 'primUpdateContourLastLine' module: 'VectorEnginePlugin'>
	self primitiveFailed.
]

{ #category : 'private' }
VectorEngineWithPlugin >> pvtDashedStrokeBits: onOffBitSequence dashBitCount: numberOfBitsInSequence dashBitLength: lengthOfEachBit dashBitOffset: offset [
	"For dashed strokes"

	dashedStrokeBits := onOffBitSequence.
	dashBitCount := numberOfBitsInSequence.
	dashBitLength := lengthOfEachBit.
	dashBitOffset := offset.
	self
		primDashedStrokeBits: dashedStrokeBits
		dashBitCount: dashBitCount
		dashBitLength: dashBitLength
		dashBitOffset: dashBitOffset.
]

{ #category : 'private' }
VectorEngineWithPlugin >> pvtFillColor: aFillColorOrNil [
	fillColor := aFillColorOrNil.
	aFillColorOrNil notNil
		ifTrue: [
			self
				primFillR: aFillColorOrNil red
				g: aFillColorOrNil green
				b: aFillColorOrNil blue
				a: aFillColorOrNil alpha]
		ifFalse: [self primFillR: 0.0 g: 0.0 b: 0.0 a: 0.0].
]

{ #category : 'private' }
VectorEngineWithPlugin >> pvtStrokeColor: aStrokeColorOrNil [
	strokeColor := aStrokeColorOrNil.
	aStrokeColorOrNil notNil
		ifTrue: [
			self
				primStrokeR: aStrokeColorOrNil red
				g: aStrokeColorOrNil green
				b: aStrokeColorOrNil blue
				a: aStrokeColorOrNil alpha]
		ifFalse: [self primStrokeR: 0.0 g: 0.0 b: 0.0 a: 0.0]
]

{ #category : 'private' }
VectorEngineWithPlugin >> pvtStrokeWidth: aNumber hop: aFloat [

	strokeWidth := aNumber.
	hop := aFloat.
	"Set AntiAliasingWidth in the plugin, as #primStrokeWidth:hop: needs it."
	self
		primAntiAliasingWidth: antiAliasingWidth asFloat
		subPixelDelta: self subPixelDelta asFloat.
	"This call in the plugin uses AntiAliasingWith."
	self primStrokeWidth: strokeWidth hop: hop.
]

{ #category : 'path algorithms' }
VectorEngineWithPlugin >> quadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl [

	self
		primQuadraticBezierFromX: xFrom asFloat y: yFrom asFloat
		toX: xTo asFloat y: yTo asFloat
		controlX: xControl asFloat y: yControl asFloat
]

{ #category : 'private' }
VectorEngineWithPlugin >> resetMorphBounds [

	"Clear any possible contour."
	morphBoundsTop ifNotNil: [
		self
			primResetContourTop: (morphBoundsTop max: 0)
			bottom: (morphBoundsBottom min: targetHeight -1) ].
	super resetMorphBounds.
]

{ #category : 'testing' }
VectorEngineWithPlugin >> usesVectorEnginePlugin [
	^true
]
