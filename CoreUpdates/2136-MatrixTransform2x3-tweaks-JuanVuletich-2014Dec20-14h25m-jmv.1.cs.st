'From Cuis 4.2 of 25 July 2013 [latest update: #2135] on 20 December 2014 at 2:47:40.411575 pm'!

!MatrixTransform2x3 commentStamp: 'jmv 12/20/2014 14:28' prior: 0!
This class represents a transformation for points, that is a combination of scale, offset, and rotation. It is implemented as a 2x3 matrix. 

The direct transformation is equivalent as multiplying the matrix by a column vector (with an extra element of value 1). The inverse transformation is multiplying the inverse of the matrix by a column vector (with an extra element of value 1). By convention, we say that the direct transform is outwards (#externalizePosition:). Therefore, the inverse transform is called #internalizePosition: .

Direct transform (#externalizePosition:) is fast and cheap, while inverse transform (#internalizePosition:) is relatively expensive.

We can consider the matrix to be of 3x3 with an extra row with #( 0 0 1) at the bottom, especially for having a properly defined inverse matrix.

Implementation Note: In the original version, from Squeak, it is assumed that the transformation deals with Integer points. All transformations will return Integer coordinates (even though float points may be passed in here). In this version, both alternatives are available, with explicit protocols, and a global setting for the Squeak protocol.!


!MorphicCanvas commentStamp: 'jmv 12/18/2014 20:34' prior: 0!
A MorphicCanvas offers 2D drawing services. It works on a 'form', usually the Display. These services are used, for example, in #drawOn: methods.

Subclasses are specific implementations. BitBltCanvas is based on BitBlt, the raster operation invented by Dan Ingalls for Smalltalk, and included in Smalltalk-80 and Squeak. Morphic3Canvas is based on its VectorEngine, using a novel technique for the rasterization (sampling) of vector graphics, invented by Juan Vuletich.!


!MatrixTransform2x3 methodsFor: 'modifying' stamp: 'jmv 12/20/2014 14:40'!
scaleBy: aPoint
	"Multiply by a scale.
	Argument can be a point, applying different scaling in x and in y directions.
	Keep the transformed position of 0@0, i.e. don't change offset."
	| pt sx sy |
	pt _ aPoint asPoint.
	sx _ pt x asFloat.
	sy _ pt y asFloat.
	self a11: self a11 * sx.
	self a12: self a12 * sx.
	self a21: self a21 * sy.
	self a22: self a22 * sy.! !

!MatrixTransform2x3 methodsFor: 'as yet unclassified' stamp: 'jmv 12/20/2014 14:34'!
negateYAxis
	"Swap inneer point Y sign.
	Make y increment upwards.
	This makes the any matrix transform from standard mathematical coordinates
	to standard display coordinates (in addition to the transform it was already doing)"
	self a12: self a12  negated.
	self a22: self a22  negated! !

!methodRemoval: MatrixTransform2x3 #negateYAxisAndAngle!
MatrixTransform2x3 removeSelector: #negateYAxisAndAngle!
