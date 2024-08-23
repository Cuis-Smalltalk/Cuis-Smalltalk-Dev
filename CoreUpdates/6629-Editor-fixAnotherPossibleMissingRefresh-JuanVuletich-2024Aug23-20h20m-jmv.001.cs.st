'From Cuis7.1 [latest update: #6628] on 23 August 2024 at 8:23:32 pm'!

!InnerTextMorph methodsFor: 'notifications' stamp: 'jmv 8/23/2024 20:22:49'!
possiblyChanged
	| embeddedMorphs |
	embeddedMorphs := model actualContents embeddedMorphs.
	self submorphsDo: [:each| 
		(embeddedMorphs includes: each) ifFalse: [
			self privateRemove: each.
			each privateOwner: nil ]].
	embeddedMorphs do: [ :each|
		each owner == self ifFalse: [
			self addMorphFront: each.
			each hide "Show it only when properly located"]].
	(owner is: #ScrollPane) ifTrue: [
		owner possiblyChanged ].
	self redrawNeeded.! !

