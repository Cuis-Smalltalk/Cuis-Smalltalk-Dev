"
Does only whole pixel antialiasing. Appropriate for very high resolution displays, or slower systems where the extra computational load of VectorEngineWithPluginSubPixel is not affordable.
Variables edgeCounts and alphaMask are single byte per pixel.

Uses VectorEnginePlugin for performance.
"
Class {
	#name : 'VectorEngineWithPluginWholePixel',
	#superclass : 'VectorEngineWithPlugin',
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'instance creation' }
VectorEngineWithPluginWholePixel class >> defaultAntiAliasingWidth [
	^1.6
]

{ #category : 'private' }
VectorEngineWithPluginWholePixel >> auxPixelDataClass [
	"Because we hold data for whole pixels"

	^ByteArray
]

{ #category : 'plugin calls' }
VectorEngineWithPluginWholePixel >> primArcCenterX: centerX centerY: centerY radiusX: radiusPointX radiusY: radiusPointY start: startAngle sweep: sweepAngle rotationCos: tthetaCos rotationSin: tthetaSin [
	<primitive: 'primArcWP' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginWholePixel >> primBlendFillOnlyLeft: l top: t right: r bottom: b [
	<primitive: 'primBlendFillOnlyWP' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginWholePixel >> primBlendStrokeAndFillLeft: l top: t right: r bottom: b [
	<primitive: 'primBlendStrokeAndFillWP' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginWholePixel >> primBlendStrokeOnlyLeft: l top: t right: r bottom: b [
	<primitive: 'primBlendStrokeOnlyWP' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginWholePixel >> primCubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2 [
	<primitive: 'primCubicBezierWP' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginWholePixel >> primDisplayByteString: aString from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes [
	<primitive: 'primDisplayByteStringWP' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginWholePixel >> primDisplayUtf32: aWordArray from: startIndex to: stopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes [
	<primitive: 'primDisplayUtf32WP' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginWholePixel >> primDisplayUtf8: aByteArray fromByte: byteStartIndex toByte: byteStopIndex atx: destX y: destY scalex: sx y: sy contourData: contourData contourDataIndexes: contourDataIndexes [
	<primitive: 'primDisplayUtf8WP' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginWholePixel >> primLineFromX: xFrom y: yFrom toX: xTo y: yTo [
	<primitive: 'primLineWP' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginWholePixel >> primPathSequence: aFloat32Array size: s [
	<primitive: 'primPathSequenceWP' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginWholePixel >> primQuadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl [
	<primitive: 'primQuadraticBezierWP' module: 'VectorEnginePlugin'>

	self primitiveFailed
]

{ #category : 'plugin calls' }
VectorEngineWithPluginWholePixel >> primTargetBits: aBitmap morphIds: aWordArray edgeCounts: otherByteArray alphaMask: anotherByteArray affectedBits: aByteArray contour: aFloat32Array targetWidth: aNumber targetHeight: otherNumber [
	<primitive: 'primSetTargetWP' module: 'VectorEnginePlugin'>

	self primitiveFailed
]
