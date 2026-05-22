"
Tests for AffineTransformation
"
Class {
	#name : 'AffineTransformationTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Morphic-Kernel',
	#package : 'BaseImageTests',
	#tag : 'Morphic-Kernel'
}

{ #category : 'testing' }
AffineTransformationTest >> testComposition [
	"
	AffineTransformationTest new testComposition
	"
	| composition inner outer |

	outer _ AffineTransformation withTranslation: 3@5.
	inner _ AffineTransformation withRadians: 0.3.
	composition _ outer composedWith: inner.
	self assert: composition translation = outer translation.
	self assert: (outer externalizePosition: (inner externalizePosition: 3@4)) = (composition externalizePosition: 3@4).
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: (inner internalizePosition: (outer internalizePosition: 3@4)) = (composition internalizePosition: 3@4).
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).


	outer _ AffineTransformation withRadians: 0.3.
	inner _ AffineTransformation withTranslation: 3@5.
	composition _ outer composedWith: inner.
	self assert: composition radians = outer radians.
	self assert: ((outer externalizePosition: (inner externalizePosition: 3@4)) - (composition externalizePosition: 3@4)) r < 0.0001.
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: ((inner internalizePosition: (outer internalizePosition: 3@4)) - (composition internalizePosition: 3@4)) r < 0.0001.
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).
]

{ #category : 'testing' }
AffineTransformationTest >> testDisplayBounds [
	"
	AffineTransformationTest new testDisplayBounds
	"
	self assert: ((AffineTransformation withRadians: 0.3) externalBoundingRectOf: (10@10 extent: 20@30))
			encompassingIntegerRectangle = (-3@12 corner: 26@48).
	self assert: (AffineTransformation new externalBoundingRectOf: (-2@ 2 extent: 10@10))
		encompassingIntegerRectangle = (-2@2 corner: 8@12).
	self assert: (AffineTransformation new externalBoundingRectOf: (-12@ 12 extent: 10@10))
		encompassingIntegerRectangle = (-12@12 corner: -2@22).
	self assert: ((AffineTransformation withTranslation: 2) externalBoundingRectOf: (-4@ 2  extent: 10@10))
		encompassingIntegerRectangle = (-2@4 corner: 8@14).
	self assert: ((AffineTransformation withTranslation: -4) externalBoundingRectOf: (2@ 2  extent: 10@10))
		encompassingIntegerRectangle = (-2@ -2 corner: 8@8).
	self assert: ((AffineTransformation withTranslation: 2) externalBoundingRectOf: (-14@ 2  extent: 10@10))
		encompassingIntegerRectangle = (-12@4 corner: -2@14).
	self assert: ((AffineTransformation withTranslation: 4) externalBoundingRectOf: (-12@ 2  extent: 10@10))
		encompassingIntegerRectangle = (-8@6 corner: 2@16).
	self assert: ((AffineTransformation withTranslation: -4) externalBoundingRectOf: (12@ 2  extent: 10@10))
		encompassingIntegerRectangle = (8 @ -2 corner: 18@8).
	self assert: ((AffineTransformation withTranslation: -2) externalBoundingRectOf: (4@ 2  extent: 10@10))
		encompassingIntegerRectangle = (2@0 corner: 12@10).
	self assert: ((AffineTransformation withTranslation: 4) externalBoundingRectOf: (-2@ 2  extent: 10@10))
		encompassingIntegerRectangle = (2@6 corner: 12@16).
]

{ #category : 'testing' }
AffineTransformationTest >> testFloatInverseTransform [
	"
	AffineTransformationTest new testFloatInverseTransform
	"
	self assert: (AffineTransformation new inverseTransform: (-2@ 2)) = (-2@2).
	self assert: ((AffineTransformation withTranslation: 2) inverseTransform: (-4@ 2)) = (-6@0).
	self assert: ((AffineTransformation withTranslation: 4) inverseTransform: (-2@ 2)) = (-6@ -2).
	self assert: ((AffineTransformation withTranslation: -2) inverseTransform: (4@ 2)) = (6@4).
	self assert: ((AffineTransformation withTranslation: -4) inverseTransform: (2@ 2)) = (6@6)
]

{ #category : 'testing' }
AffineTransformationTest >> testFloatTransform [
	"
	AffineTransformationTest new testFloatTransform
	"
	self assert: (AffineTransformation new transform: (-2@ 2)) = (-2@2).
	self assert: ((AffineTransformation withTranslation: 2) transform: (-4@ 2)) = (-2@4).
	self assert: ((AffineTransformation withTranslation: 4) transform: (-2@ 2)) = (2@6).
	self assert: ((AffineTransformation withTranslation: -2) transform: (4@ 2)) = (2@0).
	self assert: ((AffineTransformation withTranslation: -4) transform: (2@ 2)) = (-2@ -2)
]

{ #category : 'testing' }
AffineTransformationTest >> testInverseTransformation [
	"
	AffineTransformationTest new testInverseTransformation
	"
	| forward inverse |

	forward _ AffineTransformation withTranslation: 3@5.
	inverse _ forward inverseTransformation.
	
	self assert: inverse translation = forward translation negated.
	self assert: (inverse externalizePosition: 3@4) = (forward internalizePosition: 3@4).
	self assert: (inverse externalizeDelta: 3@4) = (forward internalizeDelta: 3@4).
	self assert: (inverse externalizeScalar: 7) = (forward internalizeScalar: 7).
	self assert: (inverse internalizePosition: 3@4) = (forward externalizePosition: 3@4).
	self assert: (inverse internalizeDelta: 3@4) = (forward externalizeDelta: 3@4).
	self assert: (inverse internalizeScalar: 7) = (forward externalizeScalar: 7).


	forward _ AffineTransformation withRadians: 0.25.
	inverse _ forward inverseTransformation.

	self assert: inverse radians = forward radians negated.
	self assert: ((inverse externalizePosition: 3@4) - (forward internalizePosition: 3@4)) r < 0.0001.
	self assert: ((inverse externalizeDelta: 3@4) - (forward internalizeDelta: 3@4)) r < 0.0001.
	self assert: ((inverse externalizeScalar: 7) - (forward internalizeScalar: 7)) abs < 0.0001.
	self assert: ((inverse internalizePosition: 3@4) - (forward externalizePosition: 3@4)) r < 0.0001.
	self assert: ((inverse internalizeDelta: 3@4) - (forward externalizeDelta: 3@4)) r < 0.0001.
	self assert: ((inverse internalizeScalar: 7) - (forward externalizeScalar: 7)) abs < 0.0001.
]
