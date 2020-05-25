'From Cuis 5.0 [latest update: #4164] on 22 May 2020 at 7:53:38 pm'!
Smalltalk renameClassNamed: #OrderedCollectionInspector as: #SequenceableCollectionInspector!

!Float64Array methodsFor: 'inspecting' stamp: 'sqr 5/22/2020 19:53:21'!
inspectorClass 
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^SequenceableCollectionInspector! !


!FloatArray methodsFor: 'inspecting' stamp: 'sqr 5/22/2020 19:53:21'!
inspectorClass 
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^SequenceableCollectionInspector! !


!OrderedCollection methodsFor: 'inspecting' stamp: 'sqr 5/22/2020 19:53:21'!
inspectorClass 
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^SequenceableCollectionInspector! !


!AffineTransformation methodsFor: 'inspecting' stamp: 'sqr 5/22/2020 19:53:21'!
inspectorClass 
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^SequenceableCollectionInspector! !


!Float64Array reorganize!
('accessing' at: at:put: defaultElement floatAt: floatAt:put: length size squaredLength)
('initialization' loadFrom:)
('private' privateLoadFrom: replaceWordsFrom:to:with:startingAt: swapWords)
('objects from disk' bytesPerElement writeOn:)
('inspecting' inspectorClass)
('arithmetic' * *= + += - -= / /= divideBy:ifDivisorZero:ifBothZero: dot: negated normalize)
('primitives-plugin' divideByArray:ifDivisorZero:ifBothZero: divideByScalar:ifDivisorZero:ifBothZero: primAddArray: primAddScalar: primMulArray: primMulScalar: primSubArray: primSubScalar:)
('converting' asIEEE32BitPrecisionFloat)
('testing' isLiteral)
('printing' printOn: storeOn:)
('interpolating' interpolateValues:at: interpolatedValueAt:)
!


!FloatArray reorganize!
('accessing' at: at:put: defaultElement floatAt: floatAt:put: length squaredLength)
('arithmetic' * *= + += - -= / /= \\= adaptToNumber:andSend: divideBy:ifDivisorZero:ifBothZero: dot: negated normalize sum)
('comparing' = hash hashFull primitiveEqual:)
('primitives-plugin' divideByArray:ifDivisorZero:ifBothZero: divideByScalar:ifDivisorZero:ifBothZero: primAddArray: primAddScalar: primDivArray: primDivScalar: primMulArray: primMulScalar: primSubArray: primSubScalar:)
('private' replaceFrom:to:with:startingAt:)
('inspecting' inspectorClass)
('testing' is:)
('interpolating' interpolateValues:at: interpolatedValueAt:)
('printing' printElementsOn:)
!


!OrderedCollection reorganize!
('accessing' at: at:put: size)
('adding' add: add:after: add:afterIndex: add:before: add:beforeIndex: addAllFirst: addAllLast: addFirst: addLast: at:ifAbsentPut: growSize)
('copying' copy copyFrom:to: copyReplaceFrom:to:with: copyWith: reversed)
('enumerating' collect: collect:from:to: do: reverseDo: select: with:collect: with:with:collect: withIndexCollect:)
('inspecting' inspectorClass)
('removing' remove:ifAbsent: removeAll removeAllSuchThat: removeAt: removeFirst removeFirst: removeLast removeLast:)
('sorting' sort sort:)
('testing' hasContentsInExplorer)
('private' errorNoSuchElement find: insert:before: makeRoomAtFirst makeRoomAtLast removeIndex: reset resetTo: setCollection: setContents: species)
('comparing' =)
('converting' asNewArray)
('initialization' initializeOfSize:)
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

