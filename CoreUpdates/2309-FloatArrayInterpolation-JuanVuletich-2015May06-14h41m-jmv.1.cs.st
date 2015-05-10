'From Cuis 4.2 of 25 July 2013 [latest update: #2308] on 6 May 2015 at 5:52:00.985373 pm'!

!FloatArray methodsFor: 'interpolating' stamp: 'jmv 5/6/2015 15:02'!
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


!FloatArray reorganize!
('accessing' at: at:put: defaultElement length squaredLength)
('arithmetic' * *= + += - -= / /= adaptToNumber:andSend: dot: negated normalize \\=)
('comparing' = hash)
('primitives-plugin' primAddArray: primAddScalar: primDivArray: primDivScalar: primMulArray: primMulScalar: primSubArray: primSubScalar: sum)
('converting' asFloatArray)
('private' replaceFrom:to:with:startingAt:)
('inspecting' inspectorClass)
('testing' is:)
('math functions' derivative)
('interpolating' interpolatedValueAt:)
!

