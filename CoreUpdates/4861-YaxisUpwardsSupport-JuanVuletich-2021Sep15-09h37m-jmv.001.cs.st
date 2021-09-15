'From Cuis 5.0 [latest update: #4860] on 15 September 2021 at 9:48:57 am'!

!GeometryTransformation methodsFor: 'modifying' stamp: 'jmv 9/14/2021 18:16:54'!
invertingYAxis: mustInvertYAxis
	"Answer an instance (either the receiver or a new one) with the prescribed behavior on the Y axis:
	- If mustInvertYAxis, the Y axis in inner and outer space point in opposite directions.
	- If mustInvertYAxis is false, the Y axis in inner and outer space point in the same direction (either up or down).
	Senders should always use the returned object, but not assume it is a new one:
		it could also be the receiver itself."

	self doesMirror = mustInvertYAxis ifFalse: [
		^self withCurrentYAxisInverted ].
	^self! !

!GeometryTransformation methodsFor: 'modifying' stamp: 'jmv 9/14/2021 18:15:52'!
withCurrentYAxisInverted
	"Answer an instance (either the receiver or a new one) that flips the current direction of the Y axis.
	This means that whatever we answer when externalizing x@y, it will answer when externalizing x @ -y.
	Senders should always use the returned object, but not assume it is a new one:
		it could also be the receiver itself."

	self subclassResponsibility! !


!AffineTransformation methodsFor: 'modifying' stamp: 'jmv 9/14/2021 18:15:58'!
withCurrentYAxisInverted
	"Answer an instance (either the receiver or a new one) that flips the current direction of the Y axis.
	This means that whatever we answer when externalizing x@y, it will answer when externalizing x @ -y.
	Senders should always use the returned object, but not assume it is a new one (like for MorphicTranslation):
		it could also be the receiver itself, like when the receiver is already a AffineTransformation."

	self a12: self a12  negated.
	self a22: self a22  negated.
	^self! !


!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 9/14/2021 18:16:01'!
withCurrentYAxisInverted
	"Answer an instance (either the receiver or a new one) that flips the current direction of the Y axis.
	This means that whatever we answer when externalizing x@y, it will answer when externalizing x @ -y.
	Senders should always use the returned object, but not assume it is a new one (like here):
		it could also be the receiver itself, like when the receiver is already a AffineTransformation."

	^(AffineTransformation withTranslation: self translation) withCurrentYAxisInverted! !


!MovableMorph methodsFor: 'geometry testing' stamp: 'jmv 9/14/2021 18:21:50'!
yAxisPointsUp
	"By default, most morphs assume the usual convention in 2d computer graphics:
	- x points to the right (i.e. increasing x values move from left to right)
	- y points down (i.e. increasing y values move from top to bottom)
	Subclasses wanting to follow the standard math convention, making increasing y values move upwards
	should redefine this method to answer true."

	^false! !

!MovableMorph methodsFor: 'private' stamp: 'jmv 9/14/2021 18:24:55'!
fixYAxisDirection
	"Ensure the direction of the Y axis used by our location for coordinate transformations matches our #yAxisPointsUp."

	| ownersYAxisPointsUp |
	ownersYAxisPointsUp _ owner ifNil: [false] ifNotNil: [owner yAxisPointsUp].
	location _ location invertingYAxis: (self yAxisPointsUp = ownersYAxisPointsUp) not! !


!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/14/2021 18:58:27'!
drawString: s atWaistCenter: pt font: fontOrNil color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| font dy |
	font _ self fontToUse: fontOrNil.
	dy _ currentTransformation doesMirror
		ifFalse: [ font ascent * 0.4 ]
		ifTrue: [ font ascent * -0.4 ].
	^self
		drawString: s
		from: 1 to: s size
		atBaseline: pt + ((font widthOfString: s) negated / 2 @ dy)
		font: font color: aColor! !


!GeometryTransformation methodsFor: 'testing' stamp: 'jmv 9/13/2021 17:01:20'!
doesMirror
	"Return true if the receiver mirrors points around some rect.
	Usually this is interpreted as (and used for) inverting the direction of the Y axis between the inner and the outer coordinates systems."

	^false! !


!AffineTransformation methodsFor: 'testing' stamp: 'jmv 9/13/2021 17:01:13'!
doesMirror
	"Return true if the receiver mirrors points around some rect.
	Usually this is interpreted as (and used for) inverting the direction of the Y axis between the inner and the outer coordinates systems."

	| f |
	f _ self a11 * self a22.
	^ f = 0.0
		ifTrue: [ self a12 * self a21 > 0.0]
		ifFalse: [ f < 0.0 ]! !


!MovableMorph methodsFor: 'accessing' stamp: 'jmv 9/14/2021 18:27:26'!
location: aGeometryTransformation
	location _ aGeometryTransformation.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self fixYAxisDirection.
	self redrawNeeded.! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 9/15/2021 09:45:04'!
rotateBy: radians
	"Change the rotation of this morph. Argument is an angle (possibly negative), to be added to current rotation."

	| r |
	r _ self yAxisPointsUp ifTrue: [ radians negated ] ifFalse: [ radians ].
	location _ location rotatedBy: r.
	self fixYAxisDirection.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

!MovableMorph methodsFor: 'geometry' stamp: 'jmv 9/15/2021 09:44:18'!
rotation: radians scale: scale
	"Change the rotation and scale of this morph. Arguments are an angle and a scale."

	| r |
	r _ self yAxisPointsUp ifTrue: [ radians negated ] ifFalse: [ radians ].
	location _ location withRotation: r scale: scale.
	self fixYAxisDirection.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded.! !

!MovableMorph methodsFor: 'private' stamp: 'jmv 9/14/2021 18:26:53'!
privateOwner: aMorph
	"Private!! Should only be used by methods that maintain the ower/submorph invariant."

	| oldGlobalPosition prevOwner |

	self flag: #jmvVer2.
	"Is this the best behavior???"
	prevOwner _ owner.
	prevOwner
		ifNotNil: [
			"Had an owner. Maintain my global position..."
			oldGlobalPosition _ self morphPositionInWorld ].
	owner _ aMorph.
	owner
		ifNil: [
			"Won't have any owner. Keep local position, as it will be maintained in my new owner later"
			]
		ifNotNil: [
			prevOwner
				ifNil: [
					"Didn't have any owner. Assume my local position is to be maintained in my new owner"
					]
				ifNotNil: [
					"Had an owner. Maintain my global position..."
					location _ location withTranslation: (owner internalizeFromWorld: oldGlobalPosition).
					self flag: #jmvVer2.
					"extent _ owner internalizeDistanceFromWorld: oldGlobalExtent" 	"or something like this!!"
					]].
	self fixYAxisDirection.! !


!HaloMorph methodsFor: 'drawing' stamp: 'jmv 9/14/2021 18:59:01'!
drawCoordinateSystemOn: aCanvas

	| x0 x1 y0 y1 c stepX stepY haloTargetTx prevTx font strokeWidth tickLength stepXDecimals stepYDecimals |
	haloTargetTx _ MorphicTranslation identity.
	target allOwnersReverseDo: [ :o | haloTargetTx _ haloTargetTx composedWith: o location ].
	haloTargetTx _ haloTargetTx composedWith: target location.

	target knowsOwnLocalBounds
		ifTrue: [ | r |
			r _ target morphLocalBounds.
			x0 _ r left.
			x1 _ r right.
			y0 _ r top.
			y1 _ r bottom ]
		ifFalse: [
			x0 _ x1 _ y0 _ y1 _ 0.
			target displayFullBounds corners collect: [ :pt | | p |
				p _ haloTargetTx inverseTransform: pt.
				x0 _ x0 min: p x.
				x1 _ x1 max: p x.
				y0 _ y0 min: p y.
				y1 _ y1 max: p y.]].

	font _ FontFamily defaultFamilyPointSize: FontFamily defaultPointSize * 1.5 / haloTargetTx scale.
	stepX _ (font pointSize * 9) round4perMagnitudeOrder asFloat.
	stepXDecimals _ stepX log rounded negated + 1.
	stepY _ (font pointSize * 5) round4perMagnitudeOrder asFloat.
	stepYDecimals _ stepY log rounded negated + 1.
	strokeWidth _ 3/ haloTargetTx scale.
	tickLength _ 5 / haloTargetTx scale.

	prevTx _ aCanvas currentTransformation.
	aCanvas geometryTransformation: haloTargetTx.

	c _ `Color black alpha: 0.4`.
	aCanvas line: x0@0 to: x1@0 width: strokeWidth color: c.
	aCanvas line: 0@y0 to: 0@y1 width: strokeWidth color: c.

	(x0 truncateTo: stepX) to: x1 by: stepX do: [ :x |
		aCanvas line: x @ tickLength negated to: x @ tickLength width: strokeWidth color: c.
	   	aCanvas drawString: (x printStringFractionDigits: stepXDecimals) atWaistCenter: x @ (tickLength*4) negated font: font color: c ].
	  aCanvas drawString: 'x' atCenterX: x1 - (tickLength*3) @ 0 font: font color: c.

	(y0 truncateTo: stepY) to: y1 by: stepY do: [ :y |
		aCanvas line: tickLength negated @ y to: tickLength @ y width: strokeWidth color: c.
	   	aCanvas drawString: (y printStringFractionDigits: stepYDecimals), ' ' atWaistRight: tickLength negated @ y font: font color: c ].
	  aCanvas drawString: 'y' atWaist: tickLength @ (y1 - (tickLength*4)) font: font color: c.

	aCanvas geometryTransformation: prevTx.! !

!HaloMorph methodsFor: 'private' stamp: 'jmv 9/15/2021 09:46:28'!
doRot: evt with: rotHandle
	"Update the rotation of my target if it is rotatable."

	| radians prevLocation deltaRadians |
	evt hand obtainHalo: self.
	radians _ (evt eventPosition - target referencePosition) theta + angleOffset.
	radians _ radians detentBy: 0.05 atMultiplesOf: Float pi / 4 snap: false.
	rotHandle color: (radians = 0.0
		ifTrue: [`Color lightBlue`]
		ifFalse: [`Color blue`]).
	rotHandle submorphsDo: [ :m |
		m color: rotHandle color makeForegroundColor].
	prevLocation _ target location.
	deltaRadians _ radians-prevLocation radians.
	target yAxisPointsUp ifTrue: [ deltaRadians _ deltaRadians negated ].
	target location: (prevLocation composedWith: (
		AffineTransformation withRadians: deltaRadians around: target rotationCenter)).
	rotHandle morphPositionInWorld: evt eventPosition - (rotHandle morphExtent // 2).	
	self redrawNeeded.! !


!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 9/14/2021 19:00:41'!
drawString: aString from: firstIndex to: lastIndex atWaist: aPoint font: font color: aColor
	"Answer position to place next glyph
	Answer nil if nothing was done"

	| dy |
	dy _ currentTransformation doesMirror
		ifFalse: [ font ascent * 0.4 ]
		ifTrue: [ font ascent * -0.4 ].
	^self drawString: aString from: firstIndex to: lastIndex
		atBaseline: aPoint + (0 @ dy)
		font: font color: aColor! !

!methodRemoval: MorphicTranslation #withYAxisNegated stamp: 'jmv 9/15/2021 09:37:42'!
MorphicTranslation removeSelector: #withYAxisNegated!
!methodRemoval: AffineTransformation #withYAxisNegated stamp: 'jmv 9/15/2021 09:37:45'!
AffineTransformation removeSelector: #withYAxisNegated!
