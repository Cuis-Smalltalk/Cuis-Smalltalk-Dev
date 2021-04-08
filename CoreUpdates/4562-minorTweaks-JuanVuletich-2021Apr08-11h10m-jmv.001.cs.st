'From Cuis 5.0 [latest update: #4561] on 8 April 2021 at 11:10:49 am'!

!Morph methodsFor: 'change reporting' stamp: 'jmv 4/8/2021 10:57:19'!
invalidateDisplayRect: damageRect for: aMorph
	"
	If we clip submorphs, then we clip damageRect.
	
	aMorph is the morph that changed and therefore should be redrawn. In some cases, damage reporting is done by no longer occupying some area, and requesting whatever appropriate morph to be drawn there. In such cases, aMorph should be nil. See senders."

	| clippedRect b |
	self visible ifFalse: [ ^self].

	clippedRect _ damageRect.
	self clipsSubmorphsReally ifTrue: [
		b _ self displayBounds.
		b ifNil: [ ^self ].
		clippedRect _ damageRect intersect: b ].
	owner ifNotNil: [
		owner invalidateDisplayRect: clippedRect for: aMorph ].! !


!KernelMorph methodsFor: 'geometry' stamp: 'jmv 4/8/2021 10:53:31'!
referencePosition
	"Return the current reference position of the receiver"
	"a rather ugly way to say #center . At least, we avoid false polymorphism"
	"remove some day"
	self flag: #jmvVer2.
	self displayBounds ifNotNil: [ :r | r center ].
	^ self morphExtentInWorld // 2 + self morphPositionInWorld! !


!HandMorph methodsFor: 'geometry' stamp: 'jmv 4/8/2021 10:59:15'!
displayBounds
	^ self morphPosition extent: self morphExtent ! !


!WorldMorph methodsFor: 'geometry' stamp: 'jmv 4/8/2021 10:59:19'!
displayBounds
	^ 0@0 extent: extent! !


!BitBltCanvas methodsFor: 'drawing' stamp: 'jmv 4/8/2021 10:52:51'!
drawCoordinateSystem: aRectangle
	| c stepX stepY x0 x1 y0 y1 prevClipRect |
	stepX _ 100.
	stepY _ 50.

	prevClipRect _ self clipRect.
	self setClipRect: (prevClipRect intersect: currentMorph displayBounds).

	self frameRectangle: aRectangle borderWidth: 4 color: `Color lightGray alpha: 0.4`.

	x0 _ aRectangle left.
	x1 _ aRectangle right-1.
	y0 _ aRectangle top.
	y1 _ aRectangle bottom-1.

	c _ `Color lightGray alpha: 0.6`.
	self line: x0@0 to: x1@0 width: 2 color: c.
	self line: 0@y0 to: 0@y1 width: 2 color: c.

	(x0 truncateTo: stepX) to: (x1 - stepX truncateTo: stepX) by: stepX do: [ :x |
		self line: x @ -5 to: x @ 5 width: 2 color: c.
	   	self drawString: x printString atCenterX: x @ 5 font: nil color: c ].
	  self drawString: 'x' atCenterX: x1 - 15 @ 0 font: nil color: c.

	(y0 truncateTo: stepY) to: (y1 - stepY truncateTo: stepY) by: stepY do: [ :y |
		self line: -5 @ y to: 5 @ y width: 2 color: c.
	   	self drawString: y printString, ' ' atWaist: 5 @ y font: nil color: c ].
	  self drawString: 'y' atWaist: 0 @ (y1 - 20) font: nil color: c.

	self setClipRect: prevClipRect.! !

