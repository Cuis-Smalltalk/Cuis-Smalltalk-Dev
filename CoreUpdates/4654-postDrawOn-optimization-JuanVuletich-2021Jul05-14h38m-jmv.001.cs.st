'From Cuis 5.0 [latest update: #4651] on 5 July 2021 at 2:42:08 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 7/5/2021 14:03:10'!
postDrawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target.
	Possible second stage of drawing, after drawing submorphs, and on top of them.
	Answer true if anything was drawn."

	^false! !


!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 7/5/2021 14:05:34'!
fullUpdateCurrentBounds

	| isKnownFailing |
	isKnownFailing _ currentMorph isKnownFailing.
	isKnownFailing
		ifTrue: [
			self drawCurrentAsError ]
		ifFalse: [
			currentMorph isOrthoRectangularMorph
			ifTrue: [currentMorph displayBoundsSetFrom: self]
			ifFalse: [
				"In these cases, #drawOn: is required to find bounds.
				#displayBoundsSetFrom: will use the bounds found by us in these case.
				See #displayBoundsSetFrom:"
				currentMorph
					drawOn: self;
					displayBoundsSetFrom: self.
				(currentMorph postDrawOn: self) ifTrue: [
					currentMorph displayBoundsUpdateFrom: self ]].
			currentMorph submorphsDo: [ :m |
				self fullUpdateBounds: m ]].! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 7/5/2021 14:03:57'!
drawCurrentAndSubmorphs
	
	self isCurrentMorphVisible ifTrue: [

		currentMorph drawOn: self.
		currentMorph displayBoundsSetFrom: self.

		"Display submorphs back to front"
		"coordinate system stack already set up for aMorph
		('ivars transformations' and 'currentTransformation')"
		self clippingByCurrentMorphDo: [
			currentMorph submorphsReverseDo: [ :m | self fullDraw: m ]].

		(currentMorph postDrawOn: self) ifTrue: [
			currentMorph displayBoundsUpdateFrom: self ].

		currentMorph isHighlighted ifTrue: [
			self drawHighlight: currentMorph ].
	].! !

