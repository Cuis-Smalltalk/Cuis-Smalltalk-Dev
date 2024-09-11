'From Cuis7.1 [latest update: #6688] on 10 September 2024 at 2:52:38 pm'!

!BoxMorph methodsFor: 'geometry' stamp: 'jmv 9/10/2024 14:51:06'!
minimumExtent
	"This returns the minimum extent that the morph may be shrunk to.
	It is expressed in the morph own coordinates, like morphExtent."

	^ `1@1`! !


!BoxedMorph methodsFor: 'geometry' stamp: 'jmv 9/10/2024 14:51:03'!
minimumExtent
	"This returns the minimum extent that the morph may be shrunk to.
	It is expressed in the morph own coordinates, like morphExtent."

	^ `1@1`! !


!Morph methodsFor: 'geometry' stamp: 'jmv 9/10/2024 14:45:32'!
minimumExtent
	"This returns the minimum extent that the morph may be shrunk to.
	It is expressed in the morph own coordinates, like morphExtent."

	^ `20@20`! !

!methodRemoval: LayoutAdjustingMorph #minimumExtent stamp: 'jmv 9/10/2024 14:51:33'!
LayoutAdjustingMorph removeSelector: #minimumExtent!
