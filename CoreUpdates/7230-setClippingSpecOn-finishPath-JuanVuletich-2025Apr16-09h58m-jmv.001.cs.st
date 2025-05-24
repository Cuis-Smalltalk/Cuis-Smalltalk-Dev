'From Cuis7.3 [latest update: #7138] on 16 April 2025 at 9:59:24 am'!

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 4/16/2025 09:59:03'!
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
		self clippingSpec: clippingSpec.
		fillColor notNil ifTrue: [
			strokeColor notNil
				ifTrue: [ self primBlendStrokeAndFillLeft: spanLeft top: spanTop right: spanRight bottom: spanBottom ]
				ifFalse: [ self primBlendFillOnlyLeft: spanLeft top: spanTop right: spanRight bottom: spanBottom ]]
			ifFalse: [
				self primBlendStrokeOnlyLeft: spanLeft top: spanTop right: spanRight bottom: spanBottom ]].! !

!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 4/16/2025 09:58:53'!
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
	self primCurrentMorphId: currentMorphId.
	self
		primGeometryTxA11: geometryTransformation a11 asFloat
		a12: geometryTransformation a12 asFloat
		a13: geometryTransformation a13 asFloat
		a21: geometryTransformation a21 asFloat
		a22: geometryTransformation a22 asFloat
		a23: geometryTransformation a23 asFloat.

	self primInitializePath.! !

