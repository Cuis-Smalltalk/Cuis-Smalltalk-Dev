'From Cuis 5.0 [latest update: #4533] on 15 March 2021 at 4:33:30 pm'!

!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 3/15/2021 16:33:03'!
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
				(currentMorph displayBounds quickMerge: self boundingRectOfCurrentMorphAfterDraw) ].

		currentMorph isHighlighted ifTrue: [
			self drawCurrentMorphHighlight ].
		world notNil ifTrue: [
			currentMorph halo ifNotNil: [
				self drawCoordinateSystem: currentMorph morphLocalBounds ]].

		world notNil ifTrue: [					"Only if we are drawing the world"
			currentMorph displayBounds:
				(currentMorph displayBounds quickMerge: self boundingRectOfCurrentMorphAfterDraw) ].
	].! !

