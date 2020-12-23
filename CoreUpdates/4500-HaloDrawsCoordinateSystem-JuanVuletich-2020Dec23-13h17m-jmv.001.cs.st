'From Cuis 5.0 [latest update: #4499] on 23 December 2020 at 1:18:50 pm'!

!StrikeFont methodsFor: 'accessing' stamp: 'jmv 12/23/2020 13:11:47'!
lineGap
	"Leading of the font."
	^0! !


!Morph methodsFor: 'drawing' stamp: 'jmv 12/22/2020 14:59:45'!
postDrawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.
	Possible second stage of drawing, after drawing submorphs, and on top of them"! !


!BitBltCanvas methodsFor: 'drawing' stamp: 'jmv 12/23/2020 13:15:20'!
drawCoordinateSystem: aRectangle
	| c stepX stepY x0 x1 y0 y1 |
	stepX _ 100.
	stepY _ 50.

	self fillRectangle: aRectangle color: `Color lightGray alpha: 0.4`.

	x0 _ aRectangle left.
	x1 _ aRectangle right-1.
	y0 _ aRectangle top.
	y1 _ aRectangle bottom-1.

	c _ `Color white alpha: 0.6`.
	self line: x0@0 to: x1@0 width: 2 color: c.
	self line: 0@y0 to: 0@y1 width: 2 color: c.

	(x0 truncateTo: stepX) to: (x1 - stepX truncateTo: stepX) by: stepX do: [ :x |
		self line: x @ -5 to: x @ 5 width: 2 color: c.
	   	self drawString: x printString atCenterX: x @ 5 font: nil color: c ].
	  self drawString: 'x' atCenterX: x1 - 15 @ 0 font: nil color: c.

	(y0 truncateTo: stepY) to: (y1 - stepY truncateTo: stepY) by: stepY do: [ :y |
		self line: -5 @ y to: 5 @ y width: 2 color: c.
	   	self drawString: y printString, ' ' atWaist: 5 @ y font: nil color: c ].
	  self drawString: 'y' atWaist: 0 @ (y1 - 20) font: nil color: c.! !

!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 12/23/2020 13:15:32'!
drawCurrentAndSubmorphs
	| b |
	self isCurrentMorphVisible ifTrue: [
		currentMorph drawOn: self.
		world notNil ifTrue: [					"Only if we are drawing the world"
			currentMorph halo ifNotNil: [ b _ currentMorph morphLocalBounds ].
			currentMorph displayBounds: self boundingRectOfCurrentMorphAfterDraw ].
		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]].
		currentMorph postDrawOn: self.
		b ifNotNil: [
			self drawCoordinateSystem: b ].
		world notNil ifTrue: [					"Only if we are drawing the world"
			currentMorph displayBounds:
				(currentMorph displayBounds quickMerge: self boundingRectOfCurrentMorphAfterDraw) ]].! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 12/23/2020 12:52:05'!
drawCurrentAndSubmorphs

	self isCurrentMorphVisible ifTrue: [
		currentMorph drawOn: self.
		world notNil ifTrue: [					"Only if we are drawing the world"
			currentMorph displayBounds: self boundingRectOfCurrentMorphAfterDraw ].
		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]].
		currentMorph postDrawOn: self.
		world notNil ifTrue: [					"Only if we are drawing the world"
			currentMorph displayBounds:
				(currentMorph displayBounds quickMerge: self boundingRectOfCurrentMorphAfterDraw) ]].! !

