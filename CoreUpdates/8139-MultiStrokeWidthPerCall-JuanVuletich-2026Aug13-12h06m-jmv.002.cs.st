'From Cuis7.9 [latest update: #8138] on 13 August 2026 at 12:07:24 pm'!
!classDefinition: #VectorEngine category: #'Morphic-Support'!
Object subclass: #VectorEngine
	instanceVariableNames: 'canvas antiAliasingWidth geometryTransformation strokeWidth morphBoundsLeft morphBoundsTop morphBoundsRight morphBoundsBottom hop firstX firstY currentX currentY maxStrokeWidth '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!AbstractVectorCanvas methodsFor: 'stroke & fill' stamp: 'jmv 12/Aug/2026, 4:23:56 pm (UTC)'!
strokeWidth: clientStrokeWidth
	"Can be called inside a pathCommandsBlock, but only if a non zero stroke width was specified.
	Note: No color compensation for ultra thin strokes is done, they may appear darker than
	they would if the thiner clientStrokeWidth was passed before the start of the pathCommandsBlock."

	engine strokeWidth: clientStrokeWidth.
! !


!VectorEngineBoundsFinder methodsFor: 'stroke & fill' stamp: 'jmv 13/Aug/2026, 2:43:53 pm (UTC)'!
basicStrokeWidth: aStrokeWidth

	self pvtStrokeWidth: (aStrokeWidth max: antiAliasingWidth).
	maxStrokeWidth := maxStrokeWidth max: strokeWidth.
! !

!VectorEngineBoundsFinder methodsFor: 'stroke & fill' stamp: 'jmv 12/Aug/2026, 4:19:19 pm (UTC)'!
strokeWidth: clientStrokeWidth
	"Can be called inside a pathCommandsBlock, but only if a non zero stroke width was specified."

	strokeWidth = 0.0 ifFalse: [
		self basicStrokeWidth: (geometryTransformation externalizeScalar: clientStrokeWidth) ].
! !


!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 13/Aug/2026, 2:44:05 pm (UTC)'!
basicStrokeWidth: aStrokeWidth

	| hopToUse |
	hopToUse := aStrokeWidth sqrt / 2.0 max: 0.5. 	"See #experimentHopsPerPixel2."
	fillColor ifNotNil: [
		"Correct edgeCount for fills requires visiting at least once every y coordinate, hop can
		not be larger than 1. 0.9 gives a security margin for hop estimation in Bezier curves."
		hopToUse := hopToUse min: 0.9 ].

	self pvtStrokeWidth: aStrokeWidth.
	maxStrokeWidth := maxStrokeWidth max: strokeWidth.
! !

!VectorEngineDrawer methodsFor: 'stroke & fill' stamp: 'jmv 12/Aug/2026, 4:23:48 pm (UTC)'!
strokeWidth: clientStrokeWidth
	"Can be called inside a pathCommandsBlock, but only if a non zero stroke width was specified.
	Note: No color compensation for ultra thin strokes is done, they may appear darker than
	they would if the thiner clientStrokeWidth was passed before the start of the pathCommandsBlock."

	strokeWidth = 0.0 ifFalse: [
		self basicStrokeWidth: (geometryTransformation externalizeScalar: clientStrokeWidth) ].
! !


!VectorEngine methodsFor: 'private' stamp: 'jmv 12/Aug/2026, 1:55:34 pm (UTC)'!
initializePath

	firstX := firstY := Float nan.
	currentX := currentY := 0.0.
	maxStrokeWidth := strokeWidth.
	self newTrajectoryFragment.! !


!VectorEngineBoundsFinder methodsFor: 'private' stamp: 'jmv 13/Aug/2026, 2:57:23 pm (UTC)'!
finishPath: ensureClosePath
	"Blend stroke and/or fill over background, according to the just computed pen alphas (distance from pixel to center of stroke)"

	| xtra |

	"This is a rather complicated way of dilating the span, first by the current set strokeWidth, and then trying to compensate, and use maxStrokeWidth.
	See the comment in VectorEngineWithPlugin >> #finishPath: for the rationale.
	This implementation mimics the behavior there."
	spanLeft := (spanLeft - swDilatedHalf - subPixelDelta + 1) truncated.	"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	spanTop := (spanTop - swDilatedHalf + 1) truncated.					"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	spanRight := (spanRight + swDilatedHalf + subPixelDelta) truncated + 1. 	"+1 not strictly needed in BoundsFinder, but just answer same bounds as other engines."
	spanBottom := (spanBottom + swDilatedHalf) truncated.
	maxStrokeWidth = strokeWidth ifFalse: [
		"Account for possible call of #strokeWidth: inside a pathCommandsBlock.
		Computed span is likely to be larger than strictly needed."
		xtra := ((maxStrokeWidth - strokeWidth) * 0.5) ceiling.
		spanLeft := spanLeft - xtra.
		spanTop := spanTop - xtra.
		spanRight := spanRight + xtra.
		spanBottom := spanBottom + xtra ].

	"morphBounds is not affected by clipRect"
	morphBoundsLeft := morphBoundsLeft min: spanLeft.
	morphBoundsTop := morphBoundsTop min: spanTop.
	morphBoundsRight := morphBoundsRight max: spanRight.
	morphBoundsBottom := morphBoundsBottom max: spanBottom.! !


!VectorEngineSmalltalk methodsFor: 'private' stamp: 'jmv 13/Aug/2026, 2:57:31 pm (UTC)'!
finishPath: ensureClosePath
	"Blend stroke and/or fill over background, according to the just computed pen alphas (distance from pixel to center of stroke)"
	| xtra |

	"If we are doing a fill, and caller forgot to close the trajectory, then do it anyway."
	ensureClosePath ifTrue: [
		firstX isNaN ifFalse: [
			(firstX = currentX and: [ firstY = currentY ]) ifFalse: [
				"This is actually an error condition. If we are asked to fill, then the path should be closed.
				This is a workaround. If width > 0.0, we'll be adding a visible edge to the path."
				self lineToFirstPoint ]]].
	self updateContourLastLine.

	"This is a rather complicated way of dilating the span, first by the current set strokeWidth, and then trying to compensate, and use maxStrokeWidth.
	See the comment in VectorEngineWithPlugin >> #finishPath: for the rationale.
	This implementation mimics the behavior there."
	spanLeft := (spanLeft - swDilatedHalf - self subPixelDelta + 1) truncated.	"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	spanTop := (spanTop - swDilatedHalf + 1) truncated.						"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	spanRight := (spanRight + swDilatedHalf + self subPixelDelta) truncated + 1. 	"Make room not just for updated mask, but also edges (hence, +1)"
	spanBottom := (spanBottom + swDilatedHalf) truncated.
	maxStrokeWidth = strokeWidth ifFalse: [
		"Account for possible call of #strokeWidth: inside a pathCommandsBlock.
		Computed span is likely to be larger than strictly needed."
		xtra := ((maxStrokeWidth - strokeWidth) * 0.5) ceiling.
		spanLeft := spanLeft - xtra.
		spanTop := spanTop - xtra.
		spanRight := spanRight + xtra.
		spanBottom := spanBottom + xtra ].

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
				ifTrue: [ self blendStrokeAndFill ]
				ifFalse: [ self blendFillOnly ]]
			ifFalse: [
				self blendStrokeOnly ]].! !


!VectorEngineWithPlugin methodsFor: 'private' stamp: 'jmv 13/Aug/2026, 3:01:35 pm (UTC)'!
finishPath: ensureClosePath
	"Blend stroke and/or fill over background, according to the just computed pen alphas (distance from pixel to center of stroke)"
	| spanLeft spanTop spanRight spanBottom xtra |

	"If we are doing a fill, and caller forgot to close the trajectory, then do it anyway."
	ensureClosePath ifTrue: [
		firstX isNaN ifFalse: [
			(firstX = currentX and: [ firstY = currentY ]) ifFalse: [
				"This is actually an error condition. If we are asked to fill, then the path should be closed.
				This is a workaround. If width > 0.0, we'll be adding a visible edge to the path."
				self lineToFirstPoint ]]].
	self primUpdateContourLastLine.

	"This is a complicated way of dilating the span, first by the current set strokeWidth in the plugin, and then trying to compensate, and use maxStrokeWidth.
	Note that a strictly correct implementation would require to compute spans separately for each possible strokeWidth, and merging them at the end.
	Most likely not worth the effort. The current approach is strict for the common case (a single strokeWidth) and not too bad for the uncommon case.
	Before further work on these span primitives, consider that #morphContourAfterDrawInto:into: uses a contour computed on the whole #drawOn:, so the
	reasonable approach of using a single stroke width for each drawing sequence is of no use. and its possible consequences are real on app behavior."
	spanLeft := self primSpanLeft.
	spanTop := self primSpanTop.
	spanRight := self primSpanRight.
	spanBottom := self primSpanBottom.
	maxStrokeWidth = strokeWidth ifFalse: [
		"Account for possible call of #strokeWidth: inside a pathCommandsBlock.
		Computed span is likely to be larger than strictly needed."
		xtra := ((maxStrokeWidth - strokeWidth) * 0.5) ceiling.
		spanLeft := spanLeft - xtra.
		spanTop := spanTop - xtra.
		spanRight := spanRight + xtra.
		spanBottom := spanBottom + xtra ].

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

!classDefinition: #VectorEngine category: #'Morphic-Support'!
Object subclass: #VectorEngine
	instanceVariableNames: 'canvas antiAliasingWidth geometryTransformation strokeWidth morphBoundsLeft morphBoundsTop morphBoundsRight morphBoundsBottom hop firstX firstY currentX currentY maxStrokeWidth'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
