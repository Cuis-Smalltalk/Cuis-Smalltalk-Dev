'From Cuis 5.0 [latest update: #4253] on 10 July 2020 at 11:15:42 pm'!

!RectangleLikeMorph methodsFor: 'drawing' stamp: 'jmv 7/10/2020 17:19:27'!
addPossiblyUncoveredAreasIn: aRectangle to: aCollection
	"Answer an array of rectangles encompassing those areas in aRectangle not completely
	covered by self.
	All areas that might possibly be uncovered must be included."
	(self isOrthoRectangularMorph and: [ self isOpaqueMorph ]) ifTrue: [
		aRectangle areasOutside: self morphBoundsInWorld do: [ :r |  aCollection add: r ].
		^self ].
	aCollection add: aRectangle! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/10/2020 20:01:06'!
drawCurrentAndSubmorphs

	self isCurrentMorphVisible
		ifTrue: [
			currentMorph drawOn: self.

			"Display submorphs back to front"
			"coordinate system stack already set up for aMorph
			('ivars transformations' and 'currentTransformation')"
			currentMorph hasSubmorphs ifTrue: [
				currentMorph clippedSubmorph ifNotNil: [ :clipped |
					self clippingByCurrentMorphDo: [ self fullDraw: clipped ]].
				currentMorph unclippedSubmorphsReverseDo: [ :m |
					self fullDraw: m ].
				]].! !

!MorphicCanvas methodsFor: 'private' stamp: 'jmv 7/9/2020 16:17:45'!
canvasToUse

	^self.! !


!Morph methodsFor: 'drawing' stamp: 'jmv 7/10/2020 17:20:04'!
addPossiblyUncoveredAreasIn: aRectangle to: aCollection
	"Answer an array of rectangles encompassing those areas in aRectangle not completely
	covered by self.
	All areas that might possibly be uncovered must be included."
	aCollection add: aRectangle! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 7/9/2020 18:35:09'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	self flag: #jmvVer3.
	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	currentMorph isKnownFailing ifTrue: [
		self drawCurrentAsError.
		self outOfMorph.
		^ self].

	(currentMorph isOwnedByHand and: [ Preferences cheapWindowReframe ]) ifTrue: [
		self drawCurrentAsOutline.
		self outOfMorph.
		^ self].

	"Draw current Morph and submorphs"	
	self canvasToUse drawCurrentAndSubmorphs.

	self outOfMorph! !

