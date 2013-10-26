'From Cuis 4.0 of 21 April 2012 [latest update: #1419] on 29 August 2012 at 9:18:28 am'!

!MatrixTransform2x3 methodsFor: 'inspecting' stamp: 'jmv 8/29/2012 09:11'!
customizeExplorerContents

	^ true! !

!MatrixTransform2x3 methodsFor: 'inspecting' stamp: 'jmv 8/29/2012 09:17'!
explorerContents

	^#(a11 a12 a13 a21 a22 a23) collect: [ :symbol |
		ObjectExplorerWrapper
			with: (self perform: symbol)
			name: symbol printString
			model: self ]! !

!MatrixTransform2x3 methodsFor: 'inspecting' stamp: 'jmv 8/29/2012 09:07'!
inspectorClass 
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^OrderedCollectionInspector! !

!methodRemoval: MatrixTransform2x3 #explorerContentsWithIndexCollect:!
MatrixTransform2x3 removeSelector: #explorerContentsWithIndexCollect:!

!MatrixTransform2x3 reorganize!
('converting coordinates' externalizeDistance: externalizePosition: externalizeScalar: internalizeDistance: internalizePosition: internalizeScalar: inverseTransform: inverseTransformPoints: transform: transformPositions:)
('accessing' at: at:put: inverseTransformation position printOn: radians scale translation)
('comparing' = hash)
('composing' composedWith: composedWith:into:)
('element access' a11 a11: a12 a12: a13 a13: a21 a21: a22 a22: a23 a23:)
('initialize' setIdentiy)
('objects from disk' byteSize bytesPerBasicElement restoreEndianness writeOn:)
('printing' matrixPrintString print printMatrixOn:)
('testing' isIdentity isPureTranslation)
('private' setPosition: setRadians: setRadians:scale:position: setScale: setTranslation:)
('modifying' addOffset: rotateBy: scaleBy: scaleByNumber:rotateBy:)
('converting' asMatrix)
('transforming rects' displayBoundsOfInverseTransformOf: displayBoundsOfInverseTransformOf:into: displayBoundsOfTransformOf: displayBoundsOfTransformOf:into:)
('inspecting' customizeExplorerContents explorerContents inspectorClass)
!


!FloatArray reorganize!
('accessing' at: at:put: defaultElement length squaredLength)
('arithmetic' * *= + += - -= / /= adaptToNumber:andSend: dot: negated normalize \\=)
('comparing' = hash)
('primitives-plugin' primAddArray: primAddScalar: primDivArray: primDivScalar: primMulArray: primMulScalar: primSubArray: primSubScalar: sum)
('converting' asFloatArray)
('private' replaceFrom:to:with:startingAt:)
('inspecting' inspectorClass)
!

