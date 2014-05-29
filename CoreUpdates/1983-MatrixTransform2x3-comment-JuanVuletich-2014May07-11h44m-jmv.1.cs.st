'From Cuis 4.2 of 25 July 2013 [latest update: #1982] on 7 May 2014 at 2:43:52.415543 pm'!

!MatrixTransform2x3 commentStamp: '<historical>' prior: 0!
This class represents a transformation for points, that is a combination of scale, offset, and rotation. It is implemented as a 2x3 matrix. 

The direct transformation is equivalent as multiplying a vector by the matrix. The inverse transformation is multiplying a vector by the inverse of the matrix. By convention, we say that the direct transform is outwards (#externalizePosition:). Therefore, the inverse transform is called #internalizePosition: .

Direct transform (#externalizePosition:) is fast and cheap, while inverse transform (#internalizePosition:) is relatively expensive.

Implementation Note: In the original version, from Squeak, it is assumed that the transformation deals with Integer points. All transformations will return Integer coordinates (even though float points may be passed in here). In this version, both alternatives are available, with explicit protocols, and a global setting for the Squeak protocol.!

