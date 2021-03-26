'From Cuis 5.0 [latest update: #4547] on 26 March 2021 at 4:19:22 pm'!

!Preferences class methodsFor: 'standard queries' stamp: 'jmv 3/26/2021 16:16:18'!
halosShowCoordinateSystem
	^ self
		valueOfFlag: #halosShowCoordinateSystem
		ifAbsent: [ true ]! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 3/26/2021 16:16:00'!
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
				Preferences halosShowCoordinateSystem ifTrue: [
					self drawCoordinateSystem: currentMorph morphLocalBounds ]]].

		world notNil ifTrue: [					"Only if we are drawing the world"
			currentMorph displayBounds:
				(currentMorph displayBounds quickMerge: self boundingRectOfCurrentMorphAfterDraw) ].
	].! !

