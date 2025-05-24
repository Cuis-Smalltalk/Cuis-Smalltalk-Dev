'From Cuis7.3 [latest update: #7090] on 21 March 2025 at 3:37:48 pm'!

!VectorEngineWithPluginWholePixel commentStamp: 'jmv 3/21/2025 14:43:23' prior: 0!
Does only whole pixel antialiasing. Appropriate for very high resolution displays, or slower systems where the extra computational load of VectorEngineWithPluginSubPixel is not affordable.
Variables edgeCounts and alphaMask are single byte per pixel.

Uses VectorEnginePlugin for performance.!


!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 3/21/2025 15:23:35'!
primCurrentMorphId: aNumber
	<primitive: 'primCurrentMorphId' module: 'VectorEnginePlugin'>

	self primitiveFailed! !


!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 3/21/2025 14:59:58'!
primDisplayByteString: aString from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes edgeCounts: otherWordArray alphaMask: anotherWordArray
	<primitive: 'primDisplayByteString' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 3/21/2025 14:38:02'!
primDisplayUtf32: aWordArray from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes edgeCounts: otherWordArray alphaMask: anotherWordArray
	<primitive: 'primDisplayUtf32' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 3/21/2025 14:38:20'!
primDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes edgeCounts: otherWordArray alphaMask: anotherWordArray
	<primitive: 'primDisplayUtf8' module: 'VectorEnginePlugin'>

	self primitiveFailed! !


!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 3/21/2025 15:00:17'!
primDisplayByteString: aString from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes edgeCounts: otherByteArray alphaMask: anotherByteArray
	<primitive: 'primDisplayByteStringWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 3/21/2025 14:38:07'!
primDisplayUtf32: aWordArray from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes edgeCounts: otherByteArray alphaMask: anotherByteArray
	<primitive: 'primDisplayUtf32WP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 3/21/2025 14:38:26'!
primDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes edgeCounts: otherByteArray alphaMask: anotherByteArray
	<primitive: 'primDisplayUtf8WP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !


!Morph methodsFor: 'drawing' stamp: 'jmv 3/21/2025 14:54:35'!
imageFormVG: extentOrNil depth: depth
	"Draw using a VectorCanvas
	Scale as needed. Draw unrotated."

	| prevNotVisibleFlag bounds extent fauxLocation answer auxCanvas |
	"Position and scale us in order to fill required extent, but disregard any rotation.
	Do it without triggering any invalidation at all."
	prevNotVisibleFlag := self privateFlagAt: 3.
	[
		bounds := self findFullBoundsInOwner.
		extentOrNil
			ifNotNil: [ | scale |
				extent := extentOrNil.
				scale := extent x asFloat / bounds width min: extent y asFloat / bounds height.
				fauxLocation := AffineTransformation withScale: scale position: bounds origin negated +1 * scale ]
			ifNil: [
				extent := bounds extent.
				fauxLocation := MorphicTranslation withTranslation: bounds origin negated ].
		answer := Form extent: extent depth: 32.
		auxCanvas := VectorCanvas onForm: answer.
		auxCanvas engine enableTargetTranslucency.
		auxCanvas geometryTransformation: fauxLocation.
		depth = 32 ifFalse: [
			"Only 32 bpp can hold translucent anti aliasing over transparent background"
			answer fillColor: Color veryLightGray ].
		self privateFlagAt: 3 put: false.
		auxCanvas fullDraw: self.
	] ensure: [
		self privateFlagAt: 3 put: prevNotVisibleFlag ].
	^answer asFormOfDepth: depth.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 3/21/2025 14:54:24'!
bitMask
	"Answer an 1bpp Form with our bitMask.
	Include all pixels drawn by any morph in our submorphs tree.
	Query with #pixelValueAt:
	Pixels drawn by us (or any submorph) are 0. Pixels not drawn by us (or any submorph) are 1.
	Remember to do `self removeProperty: #bitMask when appropriate!!
	(i.e. when we change in such a way to make the bitMask invalid)
	
	Also see senders of #contour"

	| prevNotVisibleFlag bounds answer auxCanvas |

	(self valueOfProperty: #bitMask) ifNotNil: [ :bitMask |
		^bitMask ].

	"Position and scale us in order to fill required extent, but disregard any rotation.
	Do it without triggering any invalidation at all."
	prevNotVisibleFlag := self privateFlagAt: 3.
	bounds := self findFullBoundsInOwner.
	[
		answer := Form extent: bounds extent depth: 32.
		auxCanvas := VectorCanvas onForm: answer translation: bounds origin negated.
		auxCanvas engine enableTargetTranslucency.
		self privateFlagAt: 3 put: false.
		auxCanvas fullDraw: self.
	] ensure: [
		self privateFlagAt: 3 put: prevNotVisibleFlag ].
	^self setProperty: #bitMask toValue:
		"Reverse it so we get bit=1 for affected pixels."
		((answer asGrayForm: 1) asFormOfDepth: 1) reverse.! !


!VectorEngineBoundsFinder methodsFor: 'private' stamp: 'jmv 3/21/2025 15:21:18'!
initializePath

	super initializePath.

	spanTop := 16r3FFFFFFF.				"drawable bottom. Will later be refined."
	spanBottom := 0.					"drawable top. Will later be refined."
	spanLeft := 16r3FFFFFFF.				"drawable right. Will later be refined."
	spanRight := 0.						"drawable left. Will later be refined."! !


!VectorEngineSmalltalk methodsFor: 'private' stamp: 'jmv 3/21/2025 15:20:57'!
initializePath

	super initializePath.

	spanTop := targetHeight.				"drawable bottom. Will later be refined."
	spanBottom := 0.					"drawable top. Will later be refined."
	spanLeft := targetWidth.				"drawable right. Will later be refined."
	spanRight := 0.						"drawable left. Will later be refined."
	prevYRounded := 16r3FFFFFFF.! !


!VectorEngineWithPlugin methodsFor: 'text and multi paths' stamp: 'jmv 3/21/2025 14:37:19'!
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
			edgeCounts: edgeCounts
			alphaMask: alphaMask
		].
	^ nextGlyphX @ nextGlyphY! !

!VectorEngineWithPlugin methodsFor: 'text and multi paths' stamp: 'jmv 3/21/2025 14:38:09'!
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
			edgeCounts: edgeCounts
			alphaMask: alphaMask
		].
	^ nextGlyphX @ nextGlyphY! !

!VectorEngineWithPlugin methodsFor: 'text and multi paths' stamp: 'jmv 3/21/2025 14:38:34'!
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
			edgeCounts: edgeCounts
			alphaMask: alphaMask
		].
	^ nextGlyphX @ nextGlyphY! !

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 3/21/2025 15:23:45'!
initializePath

	super initializePath.

	"Set all plugin state. There might be several instances of this class, all using the same plugin.
	Only one of them will use the plugin during a path draw operation. See class variable AccessLock."
	self
		primTargetBits: targetForm bits
		morphIds: morphIds
		edgeCounts: edgeCounts
		alphaMask: alphaMask
		contour: contour
		targetWidth: targetWidth
		targetHeight: targetHeight.
	self
		primAntiAliasingWidth: antiAliasingWidth asFloat
		subPixelDelta: self subPixelDelta asFloat
		hopLength: hop asFloat.
	self primClipLeft: clipLeft clipTop: clipTop clipRight: clipRight clipBottom: clipBottom.
	self primCurrentMorphId: currentMorphId.
	self
		primGeometryTxA11: geometryTransformation a11 asFloat
		a12: geometryTransformation a12 asFloat
		a13: geometryTransformation a13 asFloat
		a21: geometryTransformation a21 asFloat
		a22: geometryTransformation a22 asFloat
		a23: geometryTransformation a23 asFloat.
	self primStrokeWidth: strokeWidth.
	self primStrokeColor: strokeColor.
	self primFillColor: fillColor.

	self primInitializePath.! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 3/21/2025 14:59:29'!
primResetContourTop: t bottom: b contour: aFloat32Array
	<primitive: 'primResetContour' module: 'VectorEnginePlugin'>
	self primitiveFailed.! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 3/21/2025 14:59:42'!
primUpdateContourLastLine: aFloat32Array
	<primitive: 'primUpdateContourLastLine' module: 'VectorEnginePlugin'>
	self primitiveFailed.! !


!VectorEngineWithPlugin class methodsFor: 'private' stamp: 'jmv 3/21/2025 14:58:53'!
pluginApiVersion
	"
	VectorEngineWithPlugin isPluginAvailable
	Note: this is Api version, not package version. If no Api change, Api version doesn't change, regardless of changes in the Plugin or Smalltalk code.
	See senders and implementors.
	"
	<primitive: 'pluginApiVersion' module: 'VectorEnginePlugin'>

	^ 0! !


!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 3/21/2025 14:42:38'!
primBlendFillOnlyLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherByteArray alphaMask: anotherByteArray
	<primitive: 'primBlendFillOnlyWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 3/21/2025 14:42:42'!
primBlendStrokeAndFillLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherByteArray alphaMask: anotherByteArray
	<primitive: 'primBlendStrokeAndFillWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 3/21/2025 14:42:47'!
primBlendStrokeOnlyLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray alphaMask: anotherByteArray
	<primitive: 'primBlendStrokeOnlyWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!methodRemoval: VectorEngineWithPluginWholePixel #primDisplayByteString:from:to:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask: stamp: 'jmv 3/21/2025 14:37:24'!
VectorEngineWithPluginWholePixel removeSelector: #primDisplayByteString:from:to:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPluginWholePixel #primDisplayUtf32:from:to:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask: stamp: 'jmv 3/21/2025 14:38:11'!
VectorEngineWithPluginWholePixel removeSelector: #primDisplayUtf32:from:to:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPluginWholePixel #primDisplayUtf8:fromByte:toByte:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask: stamp: 'jmv 3/21/2025 14:38:37'!
VectorEngineWithPluginWholePixel removeSelector: #primDisplayUtf8:fromByte:toByte:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPluginSubPixel #primDisplayByteString:from:to:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask: stamp: 'jmv 3/21/2025 14:37:23'!
VectorEngineWithPluginSubPixel removeSelector: #primDisplayByteString:from:to:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPluginSubPixel #primDisplayUtf32:from:to:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask: stamp: 'jmv 3/21/2025 14:38:11'!
VectorEngineWithPluginSubPixel removeSelector: #primDisplayUtf32:from:to:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPluginSubPixel #primDisplayUtf8:fromByte:toByte:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask: stamp: 'jmv 3/21/2025 14:38:36'!
VectorEngineWithPluginSubPixel removeSelector: #primDisplayUtf8:fromByte:toByte:atx:y:scalex:y:contourData:contourDataIndexes:useST80Glyphs:edgeCounts:alphaMask:!
!methodRemoval: VectorEngineWithPlugin #primClipCurrentMorph:morphIds: stamp: 'jmv 3/21/2025 15:18:15'!
VectorEngineWithPlugin removeSelector: #primClipCurrentMorph:morphIds:!
!methodRemoval: VectorEngineWithPlugin #primCurrentMorphId:currentClipsSubmorphs: stamp: 'jmv 3/21/2025 15:23:50'!
VectorEngineWithPlugin removeSelector: #primCurrentMorphId:currentClipsSubmorphs:!
