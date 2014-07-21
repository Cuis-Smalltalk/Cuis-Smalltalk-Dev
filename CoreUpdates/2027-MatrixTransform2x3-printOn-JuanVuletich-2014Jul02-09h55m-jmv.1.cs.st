'From Cuis 4.2 of 25 July 2013 [latest update: #2026] on 2 July 2014 at 9:56:10.77431 am'!

!MatrixTransform2x3 methodsFor: 'printing' stamp: 'jmv 4/11/2014 20:28'!
printOn: aStream
	"Note:
	Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."
	aStream
		nextPutAll: self class name;
		nextPutAll: '(scale: '.
	self scale printOn: aStream.
	aStream nextPutAll: '. degrees: '.
	self degrees printOn: aStream.
	aStream nextPutAll: '. position: '.
	self position printOn: aStream.
	aStream nextPutAll: ') '! !

