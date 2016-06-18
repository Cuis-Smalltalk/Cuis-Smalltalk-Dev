'From Cuis 4.2 of 25 July 2013 [latest update: #2811] on 12 June 2016 at 10:38:32.088431 pm'!

!Object methodsFor: 'converting' stamp: 'len 6/12/2016 22:06'!
as: aSimilarClass
	"Answer an object of class aSimilarClass that has similar contents to the receiver."

	^ self class == aSimilarClass ifTrue: [self] ifFalse: [aSimilarClass newFrom: self]! !

!methodRemoval: IdentitySet #asIdentitySet!
IdentitySet removeSelector: #asIdentitySet!
!methodRemoval: Set #asSet!
Set removeSelector: #asSet!
!methodRemoval: SortedCollection #asSortedCollection!
SortedCollection removeSelector: #asSortedCollection!
!methodRemoval: OrderedCollection #asOrderedCollection!
OrderedCollection removeSelector: #asOrderedCollection!
!methodRemoval: WordArray #asWordArray!
WordArray removeSelector: #asWordArray!
!methodRemoval: IntegerArray #asIntegerArray!
IntegerArray removeSelector: #asIntegerArray!
!methodRemoval: FloatArray #asFloatArray!
FloatArray removeSelector: #asFloatArray!
!methodRemoval: ColorArray #asColorArray!
ColorArray removeSelector: #asColorArray!
!methodRemoval: Bag #asBag!
Bag removeSelector: #asBag!
