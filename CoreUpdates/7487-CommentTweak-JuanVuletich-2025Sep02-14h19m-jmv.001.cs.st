'From Cuis7.5 [latest update: #7483] on 2 September 2025 at 2:26:21 pm'!

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 9/2/2025 14:25:49'!
allMorphsDo: aBlock
	"Evaluate the given block for all morphs in this composite morph (including the receiver).
	Note: Redefined in WorldMorph to also include the Hand and any carried morphs."

	aBlock value: self.
	self allSubmorphsDo: aBlock! !

