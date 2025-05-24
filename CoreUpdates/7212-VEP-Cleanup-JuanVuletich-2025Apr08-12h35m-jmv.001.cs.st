'From Cuis7.3 [latest update: #7118] on 8 April 2025 at 12:45:11 pm'!

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 4/8/2025 12:42:06'!
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

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 4/8/2025 12:39:36'!
pvtFillColor: aFillColorOrNil
	fillColor := aFillColorOrNil.
	aFillColorOrNil notNil
		ifTrue: [
			self
				primFillR: aFillColorOrNil red
				g: aFillColorOrNil green
				b: aFillColorOrNil blue
				a: aFillColorOrNil alpha]
		ifFalse: [self primFillR: 0.0 g: 0.0 b: 0.0 a: 0.0].! !

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 4/8/2025 12:39:43'!
pvtStrokeColor: aStrokeColorOrNil
	strokeColor := aStrokeColorOrNil.
	aStrokeColorOrNil notNil
		ifTrue: [
			self
				primStrokeR: aStrokeColorOrNil red
				g: aStrokeColorOrNil green
				b: aStrokeColorOrNil blue
				a: aStrokeColorOrNil alpha]
		ifFalse: [self primStrokeR: 0.0 g: 0.0 b: 0.0 a: 0.0]! !

!methodRemoval: VectorEngineWithPlugin #primStrokeColor: stamp: 'jmv 4/8/2025 12:42:31'!
VectorEngineWithPlugin removeSelector: #primStrokeColor:!
!methodRemoval: VectorEngineWithPlugin #primFillColor: stamp: 'jmv 4/8/2025 12:42:26'!
VectorEngineWithPlugin removeSelector: #primFillColor:!
