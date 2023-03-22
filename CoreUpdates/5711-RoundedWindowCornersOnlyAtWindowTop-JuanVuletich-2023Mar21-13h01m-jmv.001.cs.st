'From Cuis 6.0 [latest update: #5710] on 21 March 2023 at 1:02:09 pm'!

!SystemWindow methodsFor: 'geometry services' stamp: 'jmv 3/21/2023 12:58:14'!
fullyCoveredRectangle
	"Answer a Rectangle whe are certain is completely covered by us.
	Answer nil if we can't be certain about a better answer.
	This implementation is correct, although simplistic and uninteresting.
	This implementation is a good optimization for many common cases."

	| b e |
	"If we can't know better."
	self isOrAnyOwnerIsRotated ifTrue: [
		^ nil ].
	color mightBeTranslucent ifTrue: [
		^ nil ].
	b := self displayBounds.
	b isNil ifTrue: [
		^nil ].
	Theme current roundWindowCorners ifFalse: [
		^ b ].
	e := (self externalizeDistanceToWorld: 0@Theme current roundedWindowRadius) ceiling.
	^ b origin + e corner: b corner! !


!AbstractVectorCanvas methodsFor: 'drawing - windows' stamp: 'jmv 3/21/2023 12:45:05'!
drawRoundedFrame: aRectangle border: borderWidth color: borderColor insideColor: insideColor labelHeight: labelHeight
	"
	BitBltCanvas onForm: Display ::  windowFrame: (10@10 extent: 200@100) color: Color red radius: 10 border: 5 labelHeight: 25 gradientTop: 1.0 gradientBottom: 0.5 insideColor: Color green.
	VectorCanvas onForm: Display :: drawRoundedFrame: (210@10 extent: 200@100) border: 5 color: Color red insideColor: Color green labelHeight: 25.
	Display forceToScreen
	"
	"
	| hbw |
	self strokeWidth: morphStrokeWidth color: aColor fillColor: insideColor do: [
		self roundRectangleMinX: aRectangle left maxX: aRectangle right minY: aRectangle top maxY: aRectangle bottom rx: r ry: r ].
	hbw _ morphStrokeWidth *0.5.
	self fillColor: aColor do: [
		self rectangleMinX: aRectangle left+hbw maxX: aRectangle right-hbw minY: aRectangle top+hbw maxY: aRectangle top + lh-hbw ].
	"

	| d lh r |
	d := borderWidth*0.5.
	lh := labelHeight + borderWidth.
	r := Theme current roundedWindowRadius.

	self strokeWidth: borderWidth color: borderColor fillColor: borderColor do: [
		self 
			roundTopRectangleMinX: aRectangle left+d maxX: aRectangle right-d minY: aRectangle top+d maxY: aRectangle top+lh-d rx: r ry: r ].
	self strokeWidth: borderWidth color: borderColor fillColor: insideColor do: [
		self
			rectangleMinX: aRectangle left+d maxX: aRectangle right-d minY: aRectangle top+lh-d maxY: aRectangle bottom-d ]! !


!BitBltCanvas methodsFor: 'drawing-windows' stamp: 'jmv 3/21/2023 12:43:30'!
drawRoundedFrame: aRectangle border: borderWidth color: borderColor insideColor: insideColor labelHeight: labelHeight
	"Title area is not inside window borders"
	| bottomFactor topFactor |
"Quizas limpiar esta sarlanga. no se"
	Theme current useWindowTitleGradient
		ifTrue: [
			topFactor := Theme current titleGradientTopFactor.
			bottomFactor := Theme current titleGradientBottomFactor ]
		ifFalse: [
			topFactor := 1.
			bottomFactor := 1 ].
	self
		windowFrame: aRectangle
		color: borderColor * Theme current titleGradientExtraLightness
		radius: Theme current roundedWindowRadius
		border: borderWidth
		labelHeight: labelHeight + borderWidth
		gradientTop: topFactor
		gradientBottom: bottomFactor
		insideColor: insideColor! !

!BitBltCanvas methodsFor: 'drawing-windows' stamp: 'jmv 3/21/2023 12:42:22'!
windowFrame: aRectangle color: aColor radius: r border: bw labelHeight: labelHeight gradientTop: topFactor gradientBottom: bottomFactor insideColor: insideColor
	"
	BitBltCanvas releaseClassCachedState; releaseClassState.
	(BitBltCanvas onForm: Display) windowFrame: (10@10 extent: 200@100) color: Color red radius: 10  border: 5 labelHeight: 25 gradientTop: 1.0 gradientBottom: 0.5 insideColor: Color green.
	Display forceToScreen
	"

	| lh bottomColor he tl tr |
	"top stripe"
	lh := labelHeight roundedHAFZ.
	self
		image: (self class topLeftCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topLeft.
	self
		image: (self class topRightCorner: r height: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor
		at: aRectangle topRight - (r@0).
	self
		fillRectangle: ((aRectangle withHeight: lh) insetBy: r@0)
		tilingWith: (self class verticalGrayGradient: lh gradientTop: topFactor gradientBottom: bottomFactor)
		multipliedBy: aColor.

	bottomColor := aColor * bottomFactor.

	"left and right borders"
	tl := aRectangle topLeft + (0@lh).
	tr := aRectangle topRight + (bw negated@lh).
	he := bw@(aRectangle height - lh).
	self fillRectangle: (tl extent: he) color: bottomColor.
	self fillRectangle: (tr extent: he) color: bottomColor.
	
	"bottom stripe"
	self fillRectangle: ((aRectangle bottomLeft + (0@bw negated)) extent: (aRectangle width - 0@bw)) color: bottomColor.

	"inside"
	self fillRectangle: (aRectangle insetBy: (bw@lh corner: bw@bw)) color: insideColor! !

