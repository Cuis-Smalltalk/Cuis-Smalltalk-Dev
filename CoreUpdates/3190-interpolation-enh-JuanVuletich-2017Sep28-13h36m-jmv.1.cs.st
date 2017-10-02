'From Cuis 5.0 of 7 November 2016 [latest update: #3187] on 28 September 2017 at 1:44:54 pm'!

!Float64Array methodsFor: 'interpolating' stamp: 'jmv 9/28/2017 13:39:15'!
interpolateValues: valuesArray at: x
	"Interpret self as a domain and valuesArray as a function samples."
	
	^self
		findBinaryIndex: [ :arg | x - arg ]
		do: [ :i | valuesArray at: i ]
		ifNone: [ :i :j  |
			((valuesArray at: i) interpolateTo: (valuesArray at: j) at: (x - (self at: i)) / ((self at: j) - (self at: i)))]! !

!Float64Array methodsFor: 'interpolating' stamp: 'jmv 9/28/2017 13:39:12'!
interpolatedValueAt: floatIndex
	"Do a linear interpolation.
	Gives usual error if argument outside bounds:
	#[ 4 5 ] asFloatArray interpolatedValueAt: 0.999
	#[ 4 5 ] asFloatArray interpolatedValueAt: 1.0
	#[ 4 5 ] asFloatArray interpolatedValueAt: 1.5
	#[ 4 5 ] asFloatArray interpolatedValueAt: 2.0
	#[ 4 5 ] asFloatArray interpolatedValueAt: 2.000001
	#[ 4 5 ] asFloatArray interpolatedValueAt: 3
	"
	| size index0 index1 weight0 weight1 |

	size _ self size.
	index0 _ floatIndex truncated. 		"Could be #floor. But as we only care for values >=1, it is the same. But faster."

	weight1 _ floatIndex - index0.
	weight0 _ 1.0 - weight1.

	index1 _ (index0 = size and: [ weight1 = 0.0 ]) 			"Avoid the invalid access if this was true, but don't make it slower the most common, general case."
		ifFalse: [ index0 + 1 ]
		ifTrue: [ index0 ].

	"/* perform interpolation */"
	^ (weight0 * (self at: index0)) + (weight1 * (self at: index1))! !


!FloatArray methodsFor: 'interpolating' stamp: 'jmv 9/28/2017 13:36:31'!
interpolateValues: valuesArray at: x
	"Interpret self as a domain and valuesArray as a function samples."
	
	^self
		findBinaryIndex: [ :arg | x - arg ]
		do: [ :i | valuesArray at: i ]
		ifNone: [ :i :j  |
			((valuesArray at: i) interpolateTo: (valuesArray at: j) at: (x - (self at: i)) / ((self at: j) - (self at: i)))]! !


!Float64Array reorganize!
('accessing' at: at:put: defaultElement floatAt: floatAt:put: length size squaredLength)
('initialization' loadFrom:)
('private' privateLoadFrom: replaceWordsFrom:to:with:startingAt: swapWords)
('objects from disk' bytesPerElement writeOn:)
('inspecting' inspectorClass)
('arithmetic' * *= + += - -= / /= dot: negated normalize)
('primitives-plugin' primAddArray: primAddScalar: primDivArray: primDivScalar: primMulArray: primMulScalar: primSubArray: primSubScalar:)
('converting' asIEEE32BitPrecisionFloat)
('testing' isLiteral)
('printing' printOn: storeOn:)
('interpolating' interpolateValues:at: interpolatedValueAt:)
!

