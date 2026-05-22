"
Does sub pixel antialiasing, both for text and graphics. Appropriate for low to medium resolution displays, where subpixel antialiasing improves quality. Consumes more CPU than VectorEngineWithPluginWholePixel.
Variables edgeCounts and alphaMask are 4 bytes per pixel (3 actually used).
Uses VectorEnginePlugin for performance.
"
Class {
	#name : 'VectorEngineWithPluginSubPixel',
	#superclass : 'VectorEngineWithPlugin',
	#instVars : [
		'subPixelDelta'
	],
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'instance creation' }
VectorEngineWithPluginSubPixel class >> defaultAntiAliasingWidth [
	^1.4
]

{ #category : 'instance creation' }
VectorEngineWithPluginSubPixel class >> defaultSubPixelDelta [

	^0.25  			"Values closer to 0.333 might produce a bit of coloring on some displays. Used to be 0.28"
]

{ #category : 'private' }
VectorEngineWithPluginSubPixel >> auxPixelDataClass [
	"Because we hold data for subpixels"

	^WordArray
]

{ #category : 'accessing' }
VectorEngineWithPluginSubPixel >> disableSubPixelSampling [
	subPixelDelta := 0.0
]

{ #category : 'plugin calls' }
VectorEngineWithPluginSubPixel >> primArcCenterX: centerX centerY: centerY radiusX: radiusPointX radiusY: radiusPointY start: startAngle sweep: sweepAngle rotationCos: tthetaCos rotationSin: tthetaSin [
	<primitive: 'primArc' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginSubPixel >> primBlendFillOnlyLeft: l top: t right: r bottom: b [
	<primitive: 'primBlendFillOnly' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginSubPixel >> primBlendStrokeAndFillLeft: l top: t right: r bottom: b [
	<primitive: 'primBlendStrokeAndFill' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginSubPixel >> primBlendStrokeOnlyLeft: l top: t right: r bottom: b [
	<primitive: 'primBlendStrokeOnly' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginSubPixel >> primCubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2 [
	<primitive: 'primCubicBezier' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginSubPixel >> primDisplayByteString: aString from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes [
	<primitive: 'primDisplayByteString' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginSubPixel >> primDisplayUtf32: aWordArray from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes [
	<primitive: 'primDisplayUtf32' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginSubPixel >> primDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes [
	<primitive: 'primDisplayUtf8' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginSubPixel >> primLineFromX: xFrom y: yFrom toX: xTo y: yTo [
	<primitive: 'primLine' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginSubPixel >> primPathSequence: aFloat32Array size: s [
	<primitive: 'primPathSequence' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginSubPixel >> primQuadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl [
	<primitive: 'primQuadraticBezier' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginSubPixel >> primTargetBits: aBitmap morphIds: aWordArray edgeCounts: otherWordArray alphaMask: anotherWordArray affectedBits: aByteArray contour: aFloat32Array targetWidth: aNumber targetHeight: otherNumber [
	<primitive: 'primSetTarget' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'private' }
VectorEngineWithPluginSubPixel >> pvtSetForm: aForm [

	super pvtSetForm: aForm.
	subPixelDelta := self class defaultSubPixelDelta.
]

{ #category : 'accessing' }
VectorEngineWithPluginSubPixel >> subPixelDelta [

	^subPixelDelta
]
