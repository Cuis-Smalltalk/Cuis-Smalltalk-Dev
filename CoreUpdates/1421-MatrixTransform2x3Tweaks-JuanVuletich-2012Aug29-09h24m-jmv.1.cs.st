'From Cuis 4.0 of 21 April 2012 [latest update: #1420] on 29 August 2012 at 9:27:56 am'!

!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'jmv 8/29/2012 09:26'!
degrees
	"Answer the angle in radians applied by the receiver. Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^ self radians radiansToDegrees! !


!MatrixTransform2x3 methodsFor: 'printing' stamp: 'jmv 8/29/2012 09:27'!
printOn: aStream
	"Note:
	Will not work correctly for shear (skew) transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."
	aStream
		nextPutAll: self class name;
		nextPutAll: '( scale: '.
	self scale printOn: aStream.
	aStream nextPutAll: '. degrees: '.
	self degrees printOn: aStream.
	aStream nextPutAll: '. position: '.
	self position printOn: aStream.
	aStream nextPutAll: ') '! !

!MatrixTransform2x3 methodsFor: 'inspecting' stamp: 'jmv 8/29/2012 09:27'!
explorerContents

	^{
		ObjectExplorerWrapper
			with: self scale
			name: 'scale'
			model: self. 
		ObjectExplorerWrapper
			with: self degrees
			name: 'degrees'
			model: self. 
		ObjectExplorerWrapper
			with: self position
			name: 'position'
			model: self }! !


!MatrixTransform2x3 reorganize!
('converting coordinates' externalizeDistance: externalizePosition: externalizeScalar: internalizeDistance: internalizePosition: internalizeScalar: inverseTransform: inverseTransformPoints: transform: transformPositions:)
('accessing' at: at:put: degrees inverseTransformation position radians scale translation)
('comparing' = hash)
('composing' composedWith: composedWith:into:)
('element access' a11 a11: a12 a12: a13 a13: a21 a21: a22 a22: a23 a23:)
('initialize' setIdentiy)
('objects from disk' byteSize bytesPerBasicElement restoreEndianness writeOn:)
('printing' matrixPrintString print printMatrixOn: printOn:)
('testing' isIdentity isPureTranslation)
('private' setPosition: setRadians: setRadians:scale:position: setScale: setTranslation:)
('modifying' addOffset: rotateBy: scaleBy: scaleByNumber:rotateBy:)
('converting' asMatrix)
('transforming rects' displayBoundsOfInverseTransformOf: displayBoundsOfInverseTransformOf:into: displayBoundsOfTransformOf: displayBoundsOfTransformOf:into:)
('inspecting' customizeExplorerContents explorerContents inspectorClass)
!

