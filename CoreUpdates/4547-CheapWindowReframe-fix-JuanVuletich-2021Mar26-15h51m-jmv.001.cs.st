'From Cuis 5.0 [latest update: #4546] on 26 March 2021 at 3:52:33 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 3/26/2021 15:52:10'!
drawCurrentAsOutline

	self isCurrentMorphVisible ifTrue: [
		self
			reverseRectangleBorder: currentMorph morphLocalBounds
			borderWidth: 2.
		world notNil ifTrue: [					"Only if we are drawing the world"
			currentMorph displayBounds: self boundingRectOfCurrentMorphAfterDraw ]].! !

