"
Tests for composition of AffineTransformation and MorphicTranslation
"
Class {
	#name : 'MorphicLocationTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Morphic-Kernel',
	#package : 'BaseImageTests',
	#tag : 'Morphic-Kernel'
}

{ #category : 'testing' }
MorphicLocationTest >> testComposition [
	"
	MorphicLocationTest new testComposition
	"
	| composition inner outer |

	outer _ MorphicTranslation withTranslation: 3@5.
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
	inner _ MorphicTranslation withTranslation: 3@5.
	composition _ outer composedWith: inner.
	self assert: composition radians = outer radians.
	self assert: ((outer externalizePosition: (inner externalizePosition: 3@4)) - (composition externalizePosition: 3@4)) r < 0.0001.
	self assert: (outer externalizeDelta: (inner externalizeDelta: 3@4)) = (composition externalizeDelta: 3@4).
	self assert: (outer externalizeScalar: (inner externalizeScalar: 7)) = (composition externalizeScalar: 7).
	self assert: ((inner internalizePosition: (outer internalizePosition: 3@4)) - (composition internalizePosition: 3@4)) r < 0.0001.
	self assert: (inner internalizeDelta: (outer internalizeDelta: 3@4)) = (composition internalizeDelta: 3@4).
	self assert: (inner internalizeScalar: (outer internalizeScalar: 7)) = (composition internalizeScalar: 7).
]
