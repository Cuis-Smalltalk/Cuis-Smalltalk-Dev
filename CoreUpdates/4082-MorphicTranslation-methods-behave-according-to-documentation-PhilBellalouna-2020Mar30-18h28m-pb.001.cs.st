'From Cuis 5.0 [latest update: #4081] on 30 March 2020 at 6:31:58 pm'!

!MorphicTranslation methodsFor: 'composing' stamp: 'pb 3/15/2020 16:23:20'!
composedWith: aTransformation into: result
	"Return the composition of the receiver and the transformation passed in.
	Store the composed matrix into result.
	Please see the comment at: #composedWith:"

	result setTranslation: self translation + aTransformation translation.
	^ result! !

