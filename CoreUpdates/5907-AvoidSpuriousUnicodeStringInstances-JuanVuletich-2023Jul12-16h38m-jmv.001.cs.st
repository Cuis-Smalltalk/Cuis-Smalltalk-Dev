'From Cuis 6.0 [latest update: #5906] on 12 July 2023 at 4:40:25 pm'!

!Object methodsFor: 'message handling' stamp: 'jmv 11/29/2022 17:59:16'!
perform: selector withArguments: argArray 
	"Send the selector, aSymbol, to the receiver with arguments in argArray.
	Fail if the number of arguments expected by the selector 
	does not match the size of argArray.
	Primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 84 error: errorCode>
	self error: errorCode.! !


!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/5/2023 17:30:23'!
float32SlotAt: index
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)! !

!ThirtyTwoBitSlotsObject methodsFor: 'private' stamp: 'jmv 5/5/2023 17:31:19'!
float32SlotAt: index put: value
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat 
		ifTrue: [ self basicAt: index put: value asIEEE32BitWord ]
		ifFalse: [ self float32SlotAt: index put: value asFloat ].
	^value! !


!UnicodeString methodsFor: 'converting' stamp: 'jmv 12/7/2022 09:38:30'!
surroundedBySingleQuotes
	"Answer the receiver with leading and trailing quotes.  "

	^ `(UnicodeString with: $')`, self, `(UnicodeString with: $')`! !


!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:36:38'!
crString
	"Answer a string containing a carriage return.
	UnicodeString crString
	"
	^ `UnicodeString with: Character cr`! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:36:44'!
crlfString
	"Answer a string containing a carriage return and a linefeed.
	UnicodeString crlfString
	"
	^ `UnicodeString with: Character cr with: Character lf`! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:41:13'!
lfString
	"Answer a string containing a single Lf character.
	UnicodeString lfString
	"
	^ `UnicodeString with: Character lf`! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:37:59'!
newLineString
	"Answer a string containing a single newLine (i.e. Lf) character.
	UnicodeString newLineString
	"
	^ `UnicodeString with: Character newLineCharacter`! !

!UnicodeString class methodsFor: 'instance creation' stamp: 'jmv 12/7/2022 09:38:35'!
tab
	"Answer a string containing a single tab character.
	UnicodeString tab
	"
	^ `UnicodeString with: Character tab`! !


!DisplayScreen class methodsFor: 'primitives' stamp: 'jmv 3/8/2023 17:39:32'!
primitiveScreenScaleFactor

	<primitive: 'primitiveScreenScaleFactor'>
	^1.0! !

!DisplayScreen class methodsFor: 'primitives' stamp: 'jmv 3/8/2023 17:06:58'!
primitiveWindowPosition: id

	<primitive: 'primitiveHostWindowPosition' module: 'HostWindowPlugin'>
	^self primitiveFailed ! !

!DisplayScreen class methodsFor: 'primitives' stamp: 'jmv 3/8/2023 17:39:15'!
primitiveWindowPosition: id x: x y: y

	<primitive: 'primitiveHostWindowPositionSet' module: 'HostWindowPlugin'>
	
	"The primitive seems to always fail on Linux, but it works nevertheless."
	"^self primitiveFailed"! !

!DisplayScreen class methodsFor: 'primitives' stamp: 'jmv 3/8/2023 17:40:15'!
primitiveWindowSize: id

	<primitive: 'primitiveHostWindowSize' module: 'HostWindowPlugin'>
	^self primitiveFailed ! !

!DisplayScreen class methodsFor: 'primitives' stamp: 'jmv 3/7/2023 16:23:13'!
primitiveWindowSize: id x: x y: y
	"Set the actual size of the host OS Window.

	DisplayScreen primitiveWindowSize: 1 x: 1024 y: 768
	DisplayScreen primitiveWindowSize: 1 x: 1024*2 y: 768*2
	Display extent
	"
	<primitive: 'primitiveHostWindowSizeSet' module: 'HostWindowPlugin'>
	^self primitiveFailed ! !

!DisplayScreen class methodsFor: 'primitives' stamp: 'jmv 3/8/2023 17:45:45'!
primitiveWindowTitle: id string: titleStringOrUtf8Bytes

	<primitive: 'primitiveHostWindowTitle' module: 'HostWindowPlugin'>
	^self primitiveFailed! !


!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 6/11/2021 12:05:33'!
primAntiAliasingWidth: aNumber subPixelDelta: otherFloat hopLength: anotherFloat
	<primitive: 'primAntiAliasingWidthsubPixelDeltaHopLength' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primClipCurrentMorph: aBoolean morphIds: aWordArray
	<primitive: 'primClipCurrentMorph' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primClipLeft: l clipTop: t clipRight: r clipBottom: b
	<primitive: 'primClipLeftclipTopclipRightclipBottom' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primCurrentMorphId: aNumber currentClipsSubmorphs: aBoolean
	<primitive: 'primCurrentMorphIdcurrentClipsSubmorphs' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primFillR: r g: g b: b a: a
	<primitive: 'primFillRGBA' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primGeometryTxA11: a11 a12: a12 a13: a13 a21: a21 a22: a22 a23: a23
	<primitive: 'primGeometryTxSet' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primInitializePath
	<primitive: 'primInitializePath' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primNewTrajectoryFragment
	<primitive: 'primNewTrajectoryFragment' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 8/22/2021 16:25:58'!
primResetContourTop: t bottom: b
	"Only for compatibility with VMs including plugin API v5"
	<primitive: 'primResetContour' module: 'VectorEnginePlugin'>

	self primitiveFailed.
	self flag: #VEPapi5.		"Eventually delete this method"! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 8/22/2021 16:27:01'!
primResetContourTop: t bottom: b contour: aFloat32Array
	<primitive: 'primReset2Contour' module: 'VectorEnginePlugin'>
	"
	self primitiveFailed.
	"
	self flag: #VEPapi5.		"Eventually delete this part, and just call primitiveFailed"
	"In case of primitive failure, assume it is because plugin is API version 5 and not 6."
	self
		primTargetBits: targetForm bits
		morphIds: morphIds
		edgeCounts: edgeCounts
		alphaMask: alphaMask
		contour: contour
		targetWidth: targetWidth
		targetHeight: targetHeight.
	self primResetContourTop: t bottom: b! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primSpanBottom
	<primitive: 'primSpanBottom' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primSpanLeft
	<primitive: 'primSpanLeft' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primSpanRight
	<primitive: 'primSpanRight' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primSpanTop
	<primitive: 'primSpanTop' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primStrokeR: r g: g b: b a: a
	<primitive: 'primStrokeRGBA' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:25:12'!
primStrokeWidth: aNumber
	<primitive: 'primStrokeWidth' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 8/22/2021 16:25:51'!
primUpdateContourLastLine
	"Only for compatibility with VMs including plugin API v5"
	<primitive: 'primUpdateContourLastLine' module: 'VectorEnginePlugin'>

	self primitiveFailed.
	self flag: #VEPapi5.		"Eventually delete this method"! !

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 8/22/2021 16:26:44'!
primUpdateContourLastLine: aFloat32Array
	<primitive: 'primUpdate2ContourLastLine' module: 'VectorEnginePlugin'>
	"
	self primitiveFailed.
	"
	self flag: #VEPapi5.		"Eventually delete this part, and just call primitiveFailed"
	"In case of primitive failure, assume it is because plugin is API version 5 and not 6."
	self
		primTargetBits: targetForm bits
		morphIds: morphIds
		edgeCounts: edgeCounts
		alphaMask: alphaMask
		contour: contour
		targetWidth: targetWidth
		targetHeight: targetHeight.
	self primUpdateContourLastLine! !


!VectorEngineWithPlugin class methodsFor: 'private' stamp: 'jmv 5/18/2021 12:51:46'!
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
	<primitive: 'pluginApiVersion' module: 'VectorEnginePlugin'>

	^ 0! !


!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 15:55:01'!
primArcCenterX: centerX centerY: centerY radiusX: radiusPointX radiusY: radiusPointY start: startAngle sweep: sweepAngle rotationCos: tthetaCos rotationSin: tthetaSin edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array
	<primitive: 'primArc' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 11:31:01'!
primBlendFillOnlyLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherWordArray alphaMask: anotherWordArray
	<primitive: 'primBlendFillOnly' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 12:35:55'!
primBlendStrokeAndFillLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherWordArray alphaMask: anotherWordArray
	<primitive: 'primBlendStrokeAndFill' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 11:32:47'!
primBlendStrokeOnlyLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray alphaMask: anotherWordArray
	<primitive: 'primBlendStrokeOnly' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 15:56:26'!
primCubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2 edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array
	<primitive: 'primCubicBezier' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 5/29/2021 16:50:53'!
primDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes useST80Glyphs: aBoolean edgeCounts: otherWordArray alphaMask: anotherWordArray
	<primitive: 'primDisplayUtf8' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 16:03:16'!
primLineFromX: xFrom y: yFrom toX: xTo y: yTo edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array
	<primitive: 'primLine' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 16:34:07'!
primPathSequence: aFloat32Array size: s edgeCounts: otherWordArray alphaMask: anotherWordArray contour: otherFloat32Array
	<primitive: 'primPathSequence' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 16:01:04'!
primQuadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array
	<primitive: 'primQuadraticBezier' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginSubPixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 16:05:29'!
primTargetBits: aBitmap morphIds: aWordArray edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array targetWidth: aNumber targetHeight: otherNumber
	<primitive: 'primSetTarget' module: 'VectorEnginePlugin'>

	self primitiveFailed! !


!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 15:55:07'!
primArcCenterX: centerX centerY: centerY radiusX: radiusPointX radiusY: radiusPointY start: startAngle sweep: sweepAngle rotationCos: tthetaCos rotationSin: tthetaSin edgeCounts: otherByteArray alphaMask: anotherByteArray contour: aFloat32Array
	<primitive: 'primArcWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 11:30:59'!
primBlendFillOnlyLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherByteArray alphaMask: anotherByteArray
	<primitive: 'primBlendFillOnlyWPOT' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 11:32:07'!
primBlendStrokeAndFillLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray edgeCounts: otherByteArray alphaMask: anotherByteArray
	<primitive: 'primBlendStrokeAndFillWPOT' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 5/18/2021 11:32:46'!
primBlendStrokeOnlyLeft: l top: t right: r bottom: b targetBits: aBitmap morphIds: aWordArray alphaMask: anotherByteArray
	<primitive: 'primBlendStrokeOnlyWPOT' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 15:56:31'!
primCubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2 edgeCounts: otherByteArray alphaMask: anotherByteArray contour: aFloat32Array
	<primitive: 'primCubicBezierWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 5/29/2021 16:50:57'!
primDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes useST80Glyphs: aBoolean edgeCounts: otherByteArray alphaMask: anotherByteArray
	<primitive: 'primDisplayUtf8WP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 16:03:20'!
primLineFromX: xFrom y: yFrom toX: xTo y: yTo edgeCounts: otherByteArray alphaMask: anotherByteArray contour: aFloat32Array
	<primitive: 'primLineWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 16:34:11'!
primPathSequence: aFloat32Array size: s edgeCounts: otherByteArray alphaMask: anotherByteArray contour: otherFloat32Array
	<primitive: 'primPathSequenceWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 16:00:40'!
primQuadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl edgeCounts: otherWordArray alphaMask: anotherWordArray contour: aFloat32Array
	<primitive: 'primQuadraticBezierWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

!VectorEngineWithPluginWholePixel methodsFor: 'plugin calls' stamp: 'jmv 7/5/2021 16:05:44'!
primTargetBits: aBitmap morphIds: aWordArray edgeCounts: otherByteArray alphaMask: anotherByteArray contour: aFloat32Array targetWidth: aNumber targetHeight: otherNumber
	<primitive: 'primSetTargetWP' module: 'VectorEnginePlugin'>

	self primitiveFailed! !

