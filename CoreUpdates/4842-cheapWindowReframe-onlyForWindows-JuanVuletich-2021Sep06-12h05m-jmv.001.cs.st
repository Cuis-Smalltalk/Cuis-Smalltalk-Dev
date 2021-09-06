'From Cuis 5.0 [latest update: #4841] on 6 September 2021 at 12:08:56 pm'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 9/6/2021 12:08:14'!
fullDraw: aMorph
	"Draw the full Morphic structure on us"

	"We are already set with a proper transformation from aMorph owner's coordinates to those of our target form."

	self flag: #jmvVer3.
	aMorph visible ifFalse: [^ self].
	self into: aMorph.

	currentMorph layoutSubmorphsIfNeeded.

	currentMorph isKnownFailing ifTrue: [
		self canvasToUse drawCurrentAsError.
		self outOfMorph.
		^ self].

	(currentMorph isOwnedByHand and: [
			Preferences cheapWindowReframe and: [currentMorph is: #SystemWindow]]) ifTrue: [
		self drawCurrentAsOutline.
		self outOfMorph.
		^ self].

	"Draw current Morph and submorphs"	
	self canvasToUse drawCurrentAndSubmorphs.

	self outOfMorph! !

