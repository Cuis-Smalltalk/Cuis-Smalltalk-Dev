"
Morph Bounds Finder
"
Class {
	#name : 'VectorEngineBoundsFinder',
	#superclass : 'VectorEngine',
	#instVars : [
		'spanLeft',
		'spanTop',
		'spanRight',
		'spanBottom',
		'swDilatedHalf',
		'subPixelDelta'
	],
	#category : 'Morphic-Support',
	#package : 'Morphic',
	#tag : 'Support'
}

{ #category : 'primitive algorithms' }
VectorEngineBoundsFinder >> arcCenterX: cx centerY: cy radiusX: rx radiusY: ry start: startAngle sweep: sweepAngle rotationCos: thetaCos rotationSin: thetaSin [

	| dx dy trx try tcx tcy center angle d hops |
	dx := geometryTransformation externalizeDelta: rx@0.
	dy := geometryTransformation externalizeDelta: 0@ry.
	trx := dx r.
	try := dy r.
	hops := ((trx max: try) * sweepAngle abs / hop) truncated + 2.
	d := sweepAngle / hops.
	center := geometryTransformation externalizePosition: cx@cy.
	tcx := center x.
	tcy := center y.
	(trx = try and: [
		d * hops > 6.28]) ifTrue: [ 	"If a circle, not an ellipse, and a complete circle, or pretty close"
			spanLeft := spanLeft min: tcx - trx.
			spanRight := spanRight max: tcx + trx.
			spanTop := spanTop min: tcy - trx.
			spanBottom := spanBottom max: tcy + trx ]

		ifFalse: [						"Else go the more expensive way"
			angle := startAngle.
			hops := hops+1.
			hops timesRepeat: [
				| x xp y yp |
				xp := angle cos * trx.
				yp := angle sin * try.
				x := thetaCos * xp - (thetaSin * yp) + tcx.
				y := thetaSin * xp + (thetaCos * yp) + tcy.
				spanLeft := spanLeft min: x.
				spanRight := spanRight max: x.
				spanTop := spanTop min: y.
				spanBottom := spanBottom max: y.
				angle := angle + d. ]].
]

{ #category : 'stroke & fill' }
VectorEngineBoundsFinder >> basicStrokeWidth: aStrokeWidth color: aStrokeColor do: pathCommandsBlock ensureClosePath: ensureClosePath [
	"Prepare parameters for drawing stroke."

	(aStrokeColor isNil or: [ aStrokeColor alpha = 0.0]) ifTrue: [ ^ self ].
	aStrokeWidth = 0.0 ifTrue: [ ^self ].
	self
		pvtStrokeWidth: (aStrokeWidth max: antiAliasingWidth)
		hop: (aStrokeWidth sqrt / 2.0 max: 0.5). "See #experimentHopsPerPixel2"
	self doPath: pathCommandsBlock ensureClosePath: ensureClosePath.
]

{ #category : 'primitive algorithms' }
VectorEngineBoundsFinder >> cubicBezierFromX: xFrom y: yFrom toX: xTo y: yTo control1X: xControl1 y: yControl1 control2X: xControl2 y: yControl2 [
	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."

	| p txFrom tyFrom txTo tyTo txControl1 tyControl1 txControl2 tyControl2 xMinEnd xMaxEnd yMinEnd yMaxEnd |
	p := geometryTransformation externalizePosition: xFrom@yFrom. txFrom := p x. tyFrom := p y.
	p := geometryTransformation externalizePosition: xTo@yTo. txTo := p x. tyTo := p y.
	p := geometryTransformation externalizePosition: xControl1@yControl1. txControl1 := p x. tyControl1 := p y.
	p := geometryTransformation externalizePosition: xControl2@yControl2. txControl2 := p x. tyControl2 := p y.

	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
	xMinEnd := txFrom min: txTo.
	xMaxEnd := txFrom max: txTo.
	yMinEnd := tyFrom min: tyTo.
	yMaxEnd := tyFrom max: tyTo.
	spanLeft := spanLeft min: (xMinEnd min: xMinEnd*0.25 + ((txControl1 min: txControl2)*0.75)).
	spanRight := spanRight max: (xMaxEnd max: xMaxEnd*0.25 + ((txControl1 max: txControl2)*0.75)).
	spanTop := spanTop min: (yMinEnd min: yMinEnd*0.25 + ((tyControl1 min: tyControl2)*0.75)).
	spanBottom := spanBottom max: (yMaxEnd max: yMaxEnd*0.25 + ((tyControl1 max: tyControl2)*0.75)).
]

{ #category : 'accessing' }
VectorEngineBoundsFinder >> currentMorph: aMorph [

]

{ #category : 'stroke & fill' }
VectorEngineBoundsFinder >> fillColor: aFillColor do: pathCommandsBlock [
	"Prepare parameters for drawing fill."

	(aFillColor isNil or: [ aFillColor alpha = 0.0]) ifTrue: [ ^ self ].
	self pvtStrokeWidth: 0.0 hop: 0.5.	"See #experimentHopsPerPixel3"
	self doPath: pathCommandsBlock ensureClosePath: true.
]

{ #category : 'private' }
VectorEngineBoundsFinder >> finishPath: ensureClosePath [
	"Blend stroke and/or fill over background, according to the just computed pen alphas (distance from pixel to center of stroke)"

	spanLeft := (spanLeft - swDilatedHalf - subPixelDelta + 1) truncated.																	"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	spanTop := (spanTop - swDilatedHalf + 1) truncated.																	"(z+1) truncated works equally well than the more intuitive (z ceiling)"
	spanRight := (spanRight + swDilatedHalf + subPixelDelta) truncated + 1. 											"+1 not strictly needed in BoundsFinder, but just answer same bounds as other engines."
	spanBottom := (spanBottom + swDilatedHalf) truncated.

	"morphBounds is not affected by clipRect"
	morphBoundsLeft := morphBoundsLeft min: spanLeft.
	morphBoundsTop := morphBoundsTop min: spanTop.
	morphBoundsRight := morphBoundsRight max: spanRight.
	morphBoundsBottom := morphBoundsBottom max: spanBottom.
]

{ #category : 'initialization' }
VectorEngineBoundsFinder >> initialize [
	self resetMorphBounds.
]

{ #category : 'private' }
VectorEngineBoundsFinder >> initializePath [

	super initializePath.

	spanTop := 16r3FFFFFFF.				"drawable bottom. Will later be refined."
	spanBottom := 0.					"drawable top. Will later be refined."
	spanLeft := 16r3FFFFFFF.				"drawable right. Will later be refined."
	spanRight := 0.						"drawable left. Will later be refined."
]

{ #category : 'primitive algorithms' }
VectorEngineBoundsFinder >> lineFromX: xFrom y: yFrom toX: xTo y: yTo [

	| p txFrom tyFrom txTo tyTo |
	p := geometryTransformation externalizePosition: xFrom@yFrom. txFrom := p x. tyFrom := p y.
	p := geometryTransformation externalizePosition: xTo@yTo. txTo := p x. tyTo := p y.

	spanLeft := spanLeft min: (txFrom min: txTo).
	spanRight := spanRight max: (txFrom max: txTo).
	spanTop := spanTop min: (tyFrom min: tyTo).
	spanBottom := spanBottom max: (tyFrom max: tyTo).
]

{ #category : 'private' }
VectorEngineBoundsFinder >> pvtStrokeWidth: aNumber hop: aFloat [

	strokeWidth := aNumber.
	hop := aFloat.
	swDilatedHalf := strokeWidth + antiAliasingWidth * 0.5.
]

{ #category : 'primitive algorithms' }
VectorEngineBoundsFinder >> quadraticBezierFromX: xFrom y: yFrom toX: xTo y: yTo controlX: xControl y: yControl [
	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."

	| p txFrom tyFrom txTo tyTo txControl tyControl xMinEnd xMaxEnd yMinEnd yMaxEnd |
	p := geometryTransformation externalizePosition: xFrom@yFrom. txFrom := p x. tyFrom := p y.
	p := geometryTransformation externalizePosition: xTo@yTo. txTo := p x. tyTo := p y.
	p := geometryTransformation externalizePosition: xControl@yControl. txControl := p x. tyControl := p y.

	"This computed span of the Bezier curve is a bit pessimistic (larger than strict bounds), but safe."
	xMinEnd := txFrom min: txTo.
	xMaxEnd := txFrom max: txTo.
	yMinEnd := tyFrom min: tyTo.
	yMaxEnd := tyFrom max: tyTo.
	spanLeft := spanLeft min: (xMinEnd min: (xMinEnd+txControl) / 2.0).
	spanRight := spanRight max: (xMaxEnd max: (xMaxEnd+txControl) / 2.0).
	spanTop := spanTop min: (yMinEnd min: (yMinEnd+tyControl) / 2.0).
	spanBottom := spanBottom max: (yMaxEnd max: (yMaxEnd+tyControl) / 2.0).
]

{ #category : 'private' }
VectorEngineBoundsFinder >> resetMorphBounds [

	morphBoundsTop := 16r3FFFFFFF.
	morphBoundsBottom := 0.
	morphBoundsLeft := 16r3FFFFFFF.
	morphBoundsRight := 0
]

{ #category : 'stroke & fill' }
VectorEngineBoundsFinder >> strokeWidth: clientStrokeWidth color: aStrokeColor dashedStrokeBits: onOffBitSequence dashBitLength: clientLengthOfEachBit dashBitOffset: offsetOrNil do: pathCommandsBlock [

	^self strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock
]

{ #category : 'stroke & fill' }
VectorEngineBoundsFinder >> strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock [
	"Prepare parameters for drawing stroke."

	self
		basicStrokeWidth: (geometryTransformation externalizeScalar: clientStrokeWidth)
		color: aStrokeColor
		do: pathCommandsBlock
		ensureClosePath: false
]

{ #category : 'stroke & fill' }
VectorEngineBoundsFinder >> strokeWidth: clientStrokeWidth color: aStrokeColor fillColor: aFillColor do: pathCommandsBlock [
	"Prepare parameters for drawing stroke, fill, or both."

	(aStrokeColor isNil or: [ aStrokeColor alpha = 0.0]) ifTrue: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	clientStrokeWidth = 0.0 ifTrue: [ ^ self fillColor: aFillColor do: pathCommandsBlock ].
	^ self strokeWidth: clientStrokeWidth color: aStrokeColor do: pathCommandsBlock
]

{ #category : 'private' }
VectorEngineBoundsFinder >> subPixelDelta: aNumber [
	subPixelDelta := aNumber
]
