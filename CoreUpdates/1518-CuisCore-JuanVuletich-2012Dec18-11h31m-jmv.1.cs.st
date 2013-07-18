'From Cuis 4.1 of 12 December 2012 [latest update: #1517] on 18 December 2012 at 11:31:38 am'!

!MatrixTransform2x3 methodsFor: 'composing' stamp: 'jmv 12/13/2012 16:29'!
composedWith: aTransformation
	"Return the composition of the receiver and the transformation passed in.

	The result is a MT2x3 that has the following effect:
		self externalize: (aTransformation externalize: aPoint)
		aTransformation internalize: (self internalize: aPoint).

	This means that aTransformation is considered the inner transform, and we are considered the outer transform.
	
	These are equivalent:
		((MatrixTransform2x3 withRadians: 0.3) composedWith: (MatrixTransform2x3 withPosition: 3@5)) print
		((MatrixTransform2x3 withRadians: 0.3) asMatrix * (MatrixTransform2x3 withPosition: 3@5) asMatrix) print

	These are also equivalent:
		((MatrixTransform2x3 withPosition: 3@5) composedWith: (MatrixTransform2x3 withRadians: 0.3)) print
		((MatrixTransform2x3 withPosition: 3@5) asMatrix * (MatrixTransform2x3 withRadians: 0.3) asMatrix) print
	"
	^self composedWith: aTransformation into: self class new! !

