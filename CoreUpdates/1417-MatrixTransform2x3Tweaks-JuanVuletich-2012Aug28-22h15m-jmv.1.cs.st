'From Cuis 4.0 of 21 April 2012 [latest update: #1416] on 28 August 2012 at 10:37:16 pm'!

!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'jmv 8/28/2012 22:30'!
radians
	"Answer the angle in radians applied by the receiver. Will not work correctly for shear transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^ self a21 arcTan: self a11! !

!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'jmv 8/28/2012 22:26'!
scalarScale
	"Answer the angle in radians applied by the receiver. Will not work correctly for shear transformations, or different scale in x and y.
	Will work correctly for scaling (if equal in all directions, i.e. if scale is a scalar), for rotations, translations, and compositions of these."

	^(self a11 squared + self a21 squared) sqrt! !

!MatrixTransform2x3 methodsFor: 'accessing' stamp: 'jmv 8/28/2012 22:22'!
translation
	^self a13 @ self a23! !

!MatrixTransform2x3 methodsFor: 'private' stamp: 'jmv 8/28/2012 22:34'!
setRadians: radians scale: aNumber translation: aPoint
	"Set the raw rotation angle in the receiver"
	| s c pt |
	s _ radians sin * aNumber.
	c _ radians cos * aNumber.
	self a11: c.
	self a12: s negated.
	self a21: s.
	self a22: c.
	pt _ aPoint asPoint.
	self a13: pt x asFloat.
	self a23: pt y asFloat.! !

!MatrixTransform2x3 methodsFor: 'private' stamp: 'jmv 8/28/2012 22:21'!
setTranslation: aPoint
	"Set the raw offset in the receiver"
	| pt |
	pt := aPoint asPoint.
	self a13: pt x asFloat.
	self a23: pt y asFloat.! !


!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 8/28/2012 22:36'!
withRadians: radians scale: aNumber translation: aPoint
	"Translation is added at the end. This means that aPoint is in the outer coordinate space.
	MatrixTransform2x3 withRadians: -3 scale: 12 translation: 4.5@3
	"
	^self new setRadians: radians scale: aNumber translation: aPoint! !

!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 8/28/2012 22:21'!
withTranslation: aPoint
	^self identity setTranslation: aPoint! !


!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 8/28/2012 22:25'!
externalizeScalar: aNumber
	"Externalize a distance (without a direction).
	 Only valid if we preserve aspect ratio (meaning that the scale factor is the same in all directions)."

	^aNumber * self scalarScale! !

!MatrixTransform2x3 methodsFor: 'converting coordinates' stamp: 'jmv 8/28/2012 22:26'!
internalizeScalar: aNumber
	"Internalize a distance (without a direction). 
	 Only valid if we preserve aspect ratio (meaning that the scale factor is the same in all directions)."

	^aNumber / self scalarScale! !


!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 8/28/2012 22:21'!
transformFromLocal: localBounds toExternal: externalBounds
	"Answer a Transform to translate coordinates inside localBounds into coordinates inside externalBounds.
	Parameters are instances of Rectangle. Therefore, there's no rotation involved, just scale and offset."

	^((self withTranslation: (externalBounds topLeft + externalBounds bottomRight / 2.0)) composedWith:
		(self withScale: (externalBounds extent / localBounds extent) asFloatPoint)) composedWith:
			(self withTranslation: (localBounds topLeft + localBounds bottomRight / 2.0) negated)! !

!methodRemoval: MatrixTransform2x3 class #withOffset:!
MatrixTransform2x3 class removeSelector: #withOffset:!
!methodRemoval: MatrixTransform2x3 #offset!
MatrixTransform2x3 removeSelector: #offset!
!methodRemoval: MatrixTransform2x3 #offset:!
MatrixTransform2x3 removeSelector: #offset:!
!methodRemoval: MatrixTransform2x3 #scalarScaleFactor!
MatrixTransform2x3 removeSelector: #scalarScaleFactor!
!methodRemoval: MatrixTransform2x3 #scale!
MatrixTransform2x3 removeSelector: #scale!
!methodRemoval: MatrixTransform2x3 #setOffset:!
MatrixTransform2x3 removeSelector: #setOffset:!

!MatrixTransform2x3 reorganize!
('converting coordinates' externalizeDistance: externalizePosition: externalizeScalar: internalizeDistance: internalizePosition: internalizeScalar: inverseTransform: inverseTransformPoints: transform: transformPositions:)
('accessing' at: at:put: inverseTransformation radians scalarScale translation)
('comparing' = hash)
('composing' composedWith: composedWith:into:)
('element access' a11 a11: a12 a12: a13 a13: a21 a21: a22 a22: a23 a23:)
('initialize' setIdentiy)
('objects from disk' byteSize bytesPerBasicElement restoreEndianness writeOn:)
('printing' print printOn:)
('testing' isIdentity isPureTranslation)
('private' setRadians: setRadians:scale:translation: setScale: setTranslation:)
('modifying' addOffset: rotateBy: scaleBy: scaleByNumber:rotateBy:)
('converting' asMatrix)
('transforming rects' displayBoundsOfInverseTransformOf: displayBoundsOfInverseTransformOf:into: displayBoundsOfTransformOf: displayBoundsOfTransformOf:into:)
!

