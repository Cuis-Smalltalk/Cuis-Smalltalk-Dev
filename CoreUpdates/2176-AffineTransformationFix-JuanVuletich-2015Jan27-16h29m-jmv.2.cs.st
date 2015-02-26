'From Cuis 4.2 of 25 July 2013 [latest update: #2171] on 27 January 2015 at 4:31:52.995293 pm'!

!AffineTransformation methodsFor: 'private' stamp: 'jmv 1/27/2015 12:37'!
setPointScale: scalePoint
	"Set the raw scale in x and y in the receiver. No rotation. private"

	self a11: scalePoint x.
	self a12: 0.
	self a21: 0.
	self a22: scalePoint y! !


!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 1/27/2015 12:45'!
transformFrom: originalBounds to: resultBounds
	"Answer a Transform to translate coordinates inside localBounds into coordinates inside externalBounds.
	Parameters are instances of Rectangle. Therefore, there's no rotation involved, just scale and offset."

	^((self withTranslation: (resultBounds topLeft + resultBounds bottomRight / 2.0)) composedWith:
		(self withPointScale: (resultBounds extent / originalBounds extent) asFloatPoint)) composedWith:
			(self withTranslation: (originalBounds topLeft + originalBounds bottomRight / 2.0) negated)! !

!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 1/27/2015 12:39'!
withPointScale: aPoint
	"
	(AffineTransformation withPointScale: 4@3) transform: 1@1
	"
	^self new setPointScale: aPoint! !


!AffineTransformation methodsFor: 'private' stamp: 'jmv 1/27/2015 12:37'!
setRadians: radians scale: scaleNumber
	"Set the raw rotation angle and scale in the receiver. private"

	| s c |
	s _ radians sin * scaleNumber.
	c _ radians cos * scaleNumber.
	self a11: c.
	self a12: s negated.
	self a21: s.
	self a22: c! !

!AffineTransformation methodsFor: 'private' stamp: 'jmv 1/27/2015 12:37'!
setTranslation: aPoint
	"private. Set the raw offset in the receiver"

	| pt |
	pt _ aPoint asPoint.
	self a13: pt x.
	self a23: pt y! !


!AffineTransformation class methodsFor: 'instance creation' stamp: 'jmv 1/27/2015 12:27'!
withScale: aNumber
	^self new setRadians: 0.0 scale: aNumber! !

!methodRemoval: AffineTransformation class #forScaleFactor:originalExtent:!
AffineTransformation class removeSelector: #forScaleFactor:originalExtent:!
!methodRemoval: AffineTransformation class #transformFromLocal:toExternal:!
AffineTransformation class removeSelector: #transformFromLocal:toExternal:!

!AffineTransformation class reorganize!
('instance creation' new new: newFromStream: transformFrom:to: withPointScale: withRadians: withRadians:scale:position: withScale: withTranslation:)
!


!AffineTransformation reorganize!
('converting coordinates' externalizeDelta: externalizePosition: externalizeScalar: internalizeDelta: internalizePosition: internalizeScalar: inverseTransform: transform:)
('accessing' at: at:put: degrees inverseTransformation radians scale translation)
('comparing' = hash)
('composing' composedWith: composedWith:into: innerComposedWithAffineTransformation: innerComposedWithTranslation:)
('element access' a11 a11: a12 a12: a13 a13: a21 a21: a22 a22: a23 a23:)
('objects from disk' byteSize bytesPerBasicElement restoreEndianness writeOn:)
('printing' matrixPrintString print printMatrixOn: printOn:)
('testing' doesNotRotate isIdentity isPureTranslation isTranslation:)
('private' setPointScale: setRadians:scale: setTranslation:)
('modifying' italizing italizing2 rotatedBy: scaledBy: scaledByNumber:rotatedBy: translatedBy: withRotation:scale: withTranslation: withYAxisNegated)
('transforming rects' displayBoundsOfTransformOf: primDisplayBoundsOfTransformOf:into:)
('inspecting' customizeExplorerContents explorerContents inspectorClass)
('initialization' initialize)
!

