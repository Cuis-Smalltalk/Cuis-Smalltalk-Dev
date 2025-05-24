'From Cuis7.3 [latest update: #7127] on 9 April 2025 at 11:34:04 am'!

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 4/9/2025 11:31:12'!
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
	self
		primAntiAliasingWidth: antiAliasingWidth asFloat
		subPixelDelta: self subPixelDelta asFloat.
	self primClipLeft: clipLeft clipTop: clipTop clipRight: clipRight clipBottom: clipBottom.
	self clippingSpec: clippingSpec.
	self primCurrentMorphId: currentMorphId.
	self
		primGeometryTxA11: geometryTransformation a11 asFloat
		a12: geometryTransformation a12 asFloat
		a13: geometryTransformation a13 asFloat
		a21: geometryTransformation a21 asFloat
		a22: geometryTransformation a22 asFloat
		a23: geometryTransformation a23 asFloat.

	self primInitializePath.! !

