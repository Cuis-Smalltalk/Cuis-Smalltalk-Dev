'From Cuis7.9 [latest update: #8021] on 25 June 2026 at 10:39:06 am'!

!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 25/Jun/2026, 1:28:14 pm (UTC)'!
intoLocation: aLocation do: aBlock
	"Compose the current transformation with aLocation and evaluate aBlock, usually a drawing operation.
	Restore the current location afterwards.
	Assume we are drawing currentMorph. Intended to be used inside #drawOn: methods for specific needs."

	| previous |
	previous := currentTransformation.
	self geometryTransformation: (currentTransformation composedWith: aLocation).
	aBlock value.
	self geometryTransformation: previous.! !


!HybridCanvas methodsFor: 'accessing' stamp: 'jmv 25/Jun/2026, 1:35:16 pm (UTC)'!
geometryTransformation: aGeometryTransformation
	"Note: Usually, transformations are handled by the framework as we traverse the Morphs tree."

	super geometryTransformation: aGeometryTransformation.
	vectorCanvas geometryTransformation: aGeometryTransformation.! !


!MorphicCanvas methodsFor: 'accessing' stamp: 'jmv 25/Jun/2026, 1:35:05 pm (UTC)'!
geometryTransformation: aGeometryTransformation
	"Note: Usually, transformations are handled by the framework as we traverse the Morphs tree."

	currentTransformation := aGeometryTransformation.! !


!AbstractVectorCanvas methodsFor: 'accessing' stamp: 'jmv 25/Jun/2026, 1:35:08 pm (UTC)'!
geometryTransformation: aGeometryTransformation
	"Note: Usually, transformations are handled by the framework as we traverse the Morphs tree."

	currentTransformation := aGeometryTransformation.
	engine geometryTransformation: aGeometryTransformation.! !

