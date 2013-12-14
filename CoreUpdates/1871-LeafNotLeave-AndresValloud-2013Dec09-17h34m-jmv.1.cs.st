'From Cuis 4.2 of 25 July 2013 [latest update: #1870] on 9 December 2013 at 5:35:31.48867 pm'!

!Morph methodsFor: 'structure' stamp: 'sqr 12/8/2013 13:06'!
veryLastLeaf
	"Answer the last submorph, recursively, i.e. the very last leaf of the morph tree"

	^submorphs size = 0
		ifTrue: [ self ]
		ifFalse: [ submorphs last veryLastLeaf ]! !


!Morph methodsFor: 'iteration of all morphs' stamp: 'sqr 12/8/2013 13:07'!
previousMorph
	"Iterate over all morphs in the receiver structure (usually all morphs living in the receiver's world),
	in 'backward'  direction.
	Do a DFS (Depth First Seach) over the morphs tree"
	
	| prev |
	owner ifNotNil: [
		prev _ owner submorphInFrontOf: self.
		prev ifNotNil: [ ^prev veryLastLeaf ].
		^owner ].
	^self veryLastLeaf! !

!methodRemoval: Morph #veryLastLeave!
Morph removeSelector: #veryLastLeave!
