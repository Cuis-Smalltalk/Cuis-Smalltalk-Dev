'From Cuis7.3 [latest update: #7092] on 27 March 2025 at 11:34:30 am'!

!VectorEngineWithPlugin methodsFor: 'plugin calls' stamp: 'jmv 3/27/2025 11:21:52'!
primTargetAssumedOpaque: aBoolean
	<primitive: 'primTargetAssumedOpaque' module: 'VectorEnginePlugin'>

	self primitiveFailed! !


!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 3/27/2025 11:32:56'!
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
	self primTargetAssumedOpaque: targetAssumedOpaque.
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

