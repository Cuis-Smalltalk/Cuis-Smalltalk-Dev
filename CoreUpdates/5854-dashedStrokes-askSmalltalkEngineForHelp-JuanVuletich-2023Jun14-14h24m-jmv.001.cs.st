'From Cuis 6.0 [latest update: #5853] on 14 June 2023 at 2:30:00 pm'!

!VectorEngineSmalltalk methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:26:33'!
dashedStroke_finishPath: ensureClosePath
	"Blend stroke and/or fill over background, according to the just computed pen alphas (distance from pixel to center of stroke)"

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.

	"If we are doing a fill, and caller forgot to close the trajectory, then do it anyway."
	ensureClosePath ifTrue: [
		firstX isNaN ifFalse: [
			(firstX = currentX and: [ firstY = currentY ]) ifFalse: [
				"This is actually an error condition. If we are asked to fill, then the path should be closed.
				This is a workaround. If width > 0.0, we'll be adding a visible edge to the path."
				self lineToFirstPoint ]]].
	self updateContourLastLine.

	spanLeft := (spanLeft - swDilatedHalf - self subPixelDelta + 1) truncated.																	"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	spanTop := (spanTop - swDilatedHalf + 1) truncated.																	"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	spanRight := (spanRight + swDilatedHalf + self subPixelDelta) truncated + 1. 																	"Make room not just for updated mask, but also edges (hence, +1)"
	spanBottom := (spanBottom + swDilatedHalf) truncated.

! !

!VectorEngineSmalltalk methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:26:57'!
dashedStroke_spanBottom

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
^spanBottom! !

!VectorEngineSmalltalk methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:27:00'!
dashedStroke_spanLeft

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
^spanLeft! !

!VectorEngineSmalltalk methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:27:04'!
dashedStroke_spanRight

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
^spanRight! !

!VectorEngineSmalltalk methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:27:07'!
dashedStroke_spanTop

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
^spanTop! !


!VectorEngineSubPixel methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:27:13'!
dashedStroke_ToFollow: aVectorEngineWithPlugin

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	
	self currentMorph: nil.	
	
	"Based on #pvtSetForm:"
	targetForm := aVectorEngineWithPlugin dashedStroke_targetForm.
	targetWidth := targetForm width.
	targetHeight := targetForm height.
	morphIds := aVectorEngineWithPlugin dashedStroke_morphIds.
	edgeCounts := aVectorEngineWithPlugin dashedStroke_edgeCounts.
	alphaMask := aVectorEngineWithPlugin dashedStroke_alphaMask.
	contour := aVectorEngineWithPlugin dashedStroke_contour.
	self resetMorphBounds.
	self antiAliasingWidth: self class defaultAntiAliasingWidth.
	clipLeft := aVectorEngineWithPlugin dashedStroke_clipLeft.
	clipTop := aVectorEngineWithPlugin dashedStroke_clipTop.
	clipRight := aVectorEngineWithPlugin dashedStroke_clipRight.
	clipBottom := aVectorEngineWithPlugin dashedStroke_clipBottom.
	geometryTransformation := aVectorEngineWithPlugin dashedStroke_geometryTransformation.

	subPixelDelta := self class defaultSubPixelDelta.
	
	self pvtStrokeWidth: aVectorEngineWithPlugin dashedStroke_strokeWidth.
	self pvtStrokeColor: aVectorEngineWithPlugin dashedStroke_strokeColor.
	self pvtFillColor: nil.
	self pvtHopLength: 0.5.

	strokeDashLength := aVectorEngineWithPlugin dashedStroke_strokeDashLength.
	strokeDashArray := aVectorEngineWithPlugin dashedStroke_strokeDashArray.
	strokeDashArrayLast := aVectorEngineWithPlugin dashedStroke_strokeDashArrayLast.
	strokeDashArrayOffset := aVectorEngineWithPlugin dashedStroke_strokeDashArrayOffset.! !


!VectorEngineWholePixel methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:27:18'!
dashedStroke_ToFollow: aVectorEngineWithPlugin

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	
	self currentMorph: nil.	
	
	"Based on #pvtSetForm:"
	targetForm := aVectorEngineWithPlugin dashedStroke_targetForm.
	targetWidth := targetForm width.
	targetHeight := targetForm height.
	morphIds := aVectorEngineWithPlugin dashedStroke_morphIds.
	edgeCounts := aVectorEngineWithPlugin dashedStroke_edgeCounts.
	alphaMask := aVectorEngineWithPlugin dashedStroke_alphaMask.
	contour := aVectorEngineWithPlugin dashedStroke_contour.
	self resetMorphBounds.
	self antiAliasingWidth: self class defaultAntiAliasingWidth.
	clipLeft := aVectorEngineWithPlugin dashedStroke_clipLeft.
	clipTop := aVectorEngineWithPlugin dashedStroke_clipTop.
	clipRight := aVectorEngineWithPlugin dashedStroke_clipRight.
	clipBottom := aVectorEngineWithPlugin dashedStroke_clipBottom.
	geometryTransformation := aVectorEngineWithPlugin dashedStroke_geometryTransformation.
	
	self pvtStrokeWidth: aVectorEngineWithPlugin dashedStroke_strokeWidth.
	self pvtStrokeColor: aVectorEngineWithPlugin dashedStroke_strokeColor.
	self pvtFillColor: nil.
	self pvtHopLength: 0.5.

	strokeDashLength := aVectorEngineWithPlugin dashedStroke_strokeDashLength.
	strokeDashArray := aVectorEngineWithPlugin dashedStroke_strokeDashArray.
	strokeDashArrayLast := aVectorEngineWithPlugin dashedStroke_strokeDashArrayLast.
	strokeDashArrayOffset := aVectorEngineWithPlugin dashedStroke_strokeDashArrayOffset.! !


!VectorEngineWithPlugin methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:27:21'!
dashedStroke_alphaMask

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	^alphaMask! !

!VectorEngineWithPlugin methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:27:25'!
dashedStroke_clipBottom

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	^clipBottom! !

!VectorEngineWithPlugin methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:27:28'!
dashedStroke_clipLeft

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	^clipLeft! !

!VectorEngineWithPlugin methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:27:31'!
dashedStroke_clipRight

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	^clipRight! !

!VectorEngineWithPlugin methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:27:34'!
dashedStroke_clipTop

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	^clipTop! !

!VectorEngineWithPlugin methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:27:37'!
dashedStroke_contour

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	^contour! !

!VectorEngineWithPlugin methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:27:39'!
dashedStroke_edgeCounts

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	^edgeCounts! !

!VectorEngineWithPlugin methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:27:44'!
dashedStroke_finishPath: ensureClosePath smalltalkVGE: smalltalkVGE
	"Blend stroke and/or fill over background, according to the just computed pen alphas (distance from pixel to center of stroke)"
	| spanLeft spanTop spanRight spanBottom |

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.

	smalltalkVGE dashedStroke_finishPath: ensureClosePath.

	spanLeft := smalltalkVGE dashedStroke_spanLeft.
	spanTop := smalltalkVGE dashedStroke_spanTop.
	spanRight := smalltalkVGE dashedStroke_spanRight.
	spanBottom := smalltalkVGE dashedStroke_spanBottom.

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
				ifTrue: [ self primBlendStrokeAndFillLeft: spanLeft top: spanTop right: spanRight bottom: spanBottom targetBits: targetForm bits morphIds: morphIds edgeCounts: edgeCounts alphaMask: alphaMask ]
				ifFalse: [ self primBlendFillOnlyLeft: spanLeft top: spanTop right: spanRight bottom: spanBottom targetBits: targetForm bits morphIds: morphIds edgeCounts: edgeCounts alphaMask: alphaMask ]]
			ifFalse: [
				self primBlendStrokeOnlyLeft: spanLeft top: spanTop right: spanRight bottom: spanBottom targetBits: targetForm bits morphIds: morphIds alphaMask: alphaMask ]].! !

!VectorEngineWithPlugin methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:27:52'!
dashedStroke_geometryTransformation

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	^geometryTransformation! !

!VectorEngineWithPlugin methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:27:55'!
dashedStroke_morphIds

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	^morphIds! !

!VectorEngineWithPlugin methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:27:58'!
dashedStroke_strokeColor

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	^strokeColor! !

!VectorEngineWithPlugin methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:28:02'!
dashedStroke_strokeDashArray

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	^strokeDashArray! !

!VectorEngineWithPlugin methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:28:06'!
dashedStroke_strokeDashArrayLast

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	^strokeDashArrayLast! !

!VectorEngineWithPlugin methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:28:09'!
dashedStroke_strokeDashArrayOffset

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	^strokeDashArrayOffset! !

!VectorEngineWithPlugin methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:28:13'!
dashedStroke_strokeDashLength

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	^strokeDashLength! !

!VectorEngineWithPlugin methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:28:16'!
dashedStroke_strokeWidth

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	^strokeWidth! !

!VectorEngineWithPlugin methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:28:21'!
dashedStroke_targetForm

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	^targetForm! !


!VectorEngineWithPluginSubPixel methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:28:25'!
dashedStroke_doPath: pathCommandsBlock ensureClosePath: ensureClosePath
	"Ask a VectorEngineSmalltalk for help"

	| smalltalkVGE |

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	self class protected: [
		self initializePath.
		smalltalkVGE := VectorEngineSubPixel new dashedStroke_ToFollow: self.
		smalltalkVGE initializePath.
		canvas instVarNamed: 'engine' put: smalltalkVGE.
		pathCommandsBlock valueWithPossibleArgument: (canvas ifNil:[smalltalkVGE]).
		self dashedStroke_finishPath: ensureClosePath smalltalkVGE: smalltalkVGE.
		canvas instVarNamed: 'engine' put: self.
		].
! !


!VectorEngineWithPluginWholePixel methodsFor: 'hacky dashed strokes' stamp: 'jmv 6/14/2023 14:28:32'!
dashedStroke_doPath: pathCommandsBlock ensureClosePath: ensureClosePath
	"Ask a VectorEngineSmalltalk for help"

	| smalltalkVGE |

	"Please delete this method when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	self class protected: [
		self initializePath.
		smalltalkVGE := VectorEngineWholePixel new dashedStroke_ToFollow: self.
		smalltalkVGE initializePath.
		canvas instVarNamed: 'engine' put: smalltalkVGE.
		pathCommandsBlock valueWithPossibleArgument: (canvas ifNil:[smalltalkVGE]).
		self dashedStroke_finishPath: ensureClosePath smalltalkVGE: smalltalkVGE.
		canvas instVarNamed: 'engine' put: self.
		].
! !


!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 6/14/2023 14:29:30'!
doPath: pathCommandsBlock ensureClosePath: ensureClosePath
	"Evaluate a sequence of path commands. Path parameters already set.
	VectorEnginePlugin is not reentrant.
	- Any other process must wait.
	- Nested calls in the same process are not supported, and ignored."

	"Currently, dashed strokes are only handled by VectorEngineSmalltalk.
	This is a hacky way to ask them for help."
	"Please delete this section when VectorEnginePlugin supports dashed strokes."
	self flag: #dashedStrokesNotInPlugin.
	strokeDashArrayLast = 0.0 ifFalse: [
		^ self dashedStroke_doPath: pathCommandsBlock ensureClosePath: ensureClosePath ].

	self class protected: [
		super doPath: pathCommandsBlock ensureClosePath: ensureClosePath ].! !

