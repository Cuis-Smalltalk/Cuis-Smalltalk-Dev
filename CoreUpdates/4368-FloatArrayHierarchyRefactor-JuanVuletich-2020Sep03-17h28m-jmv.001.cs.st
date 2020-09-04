'From Cuis 5.0 [latest update: #4367] on 3 September 2020 at 5:47:35 pm'!

!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 5/14/2015 09:52'!
* anObject

	^self copy *= anObject! !

!FloatArray methodsFor: 'arithmetic' stamp: 'ar 9/14/1998 22:49'!
*= anObject
	^anObject isNumber
		ifTrue:[self primMulScalar: anObject asFloat]
		ifFalse:[self primMulArray: anObject]! !

!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 5/14/2015 09:52'!
+ anObject

	^self copy += anObject! !

!FloatArray methodsFor: 'arithmetic' stamp: 'ar 9/14/1998 22:48'!
+= anObject
	^anObject isNumber
		ifTrue:[self primAddScalar: anObject asFloat]
		ifFalse:[self primAddArray: anObject]! !

!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 5/14/2015 09:52'!
- anObject

	^self copy -= anObject! !

!FloatArray methodsFor: 'arithmetic' stamp: 'ar 9/14/1998 22:49'!
-= anObject
	^anObject isNumber
		ifTrue:[self primSubScalar: anObject asFloat]
		ifFalse:[self primSubArray: anObject]! !

!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 5/14/2015 09:52'!
/ anObject

	^self copy /= anObject! !

!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 6/25/2019 17:43:52'!
/= anObject

	^self divideBy: anObject
		ifDivisorZero: [ZeroDivide new signalReceiver: self selector: #/= argument: anObject]
		ifBothZero: [ZeroDivide new signalReceiver: self selector: #/= argument: anObject]! !

!FloatArray methodsFor: 'arithmetic' stamp: 'yo 9/14/2004 17:12'!
\\= other

	other isNumber ifTrue: [
		1 to: self size do: [:i |
			self at: i put: (self at: i) \\ other
		].
		^ self.
	].
	1 to: (self size min: other size) do: [:i |
		self at: i put: (self at: i) \\ (other at: i).
	].

! !

!FloatArray methodsFor: 'arithmetic' stamp: 'nice 11/24/2007 00:10'!
adaptToNumber: rcvr andSend: selector
	"If I am involved in arithmetic with a Number. If possible,
	convert it to a float and perform the (more efficient) primitive operation."
	selector == #+ ifTrue:[^self + rcvr].
	selector == #* ifTrue:[^self * rcvr].
	selector == #- ifTrue:[^self negated += rcvr].
	selector == #/ ifTrue:[
		"DO NOT USE TRIVIAL CODE
			^self reciprocal * rcvr
		BECAUSE OF GRADUAL UNDERFLOW
		self should: (1.0e-39 / (FloatArray with: 1.0e-39)) first < 2."
			^(self class new: self size withAll: rcvr) / self
		].
	^super adaptToNumber: rcvr andSend: selector! !

!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 7/9/2018 09:41:43'!
divideBy: aFloatArrayOrNumber ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue
	^aFloatArrayOrNumber isNumber
		ifTrue:[self divideByScalar: aFloatArrayOrNumber asFloat ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue]
		ifFalse:[self divideByArray: aFloatArrayOrNumber ifDivisorZero: zeroDivisionBlockOrValue ifBothZero: indeterminateBlockOrValue]! !

!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 5/14/2015 09:52'!
negated

	^self copy *= -1! !

!FloatArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:11:02'!
at: index
	^self floatAt: index! !

!FloatArray methodsFor: 'accessing' stamp: 'jmv 2/28/2020 12:10:34'!
at: index put: value
	^self floatAt: index put: value! !

!FloatArray methodsFor: 'accessing' stamp: 'ar 11/2/1998 12:19'!
defaultElement
	"Return the default element of the receiver"
	^0.0! !

!FloatArray methodsFor: 'accessing' stamp: 'laza 3/24/2000 13:08'!
length
	"Return the length of the receiver"
	^self squaredLength sqrt! !

!FloatArray methodsFor: 'accessing' stamp: 'laza 3/24/2000 13:08'!
squaredLength
	"Return the squared length of the receiver"
	^self dot: self! !

!FloatArray methodsFor: 'interpolating' stamp: 'jmv 9/28/2017 13:36:31'!
interpolateValues: valuesArray at: x
	"Interpret self as a domain and valuesArray as a function samples."
	
	^self
		findBinaryIndex: [ :arg | x - arg ]
		do: [ :i | valuesArray at: i ]
		ifNone: [ :i :j  |
			((valuesArray at: i) interpolateTo: (valuesArray at: j) at: (x - (self at: i)) / ((self at: j) - (self at: i)))]! !

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

!FloatArray methodsFor: 'inspecting' stamp: 'sqr 5/22/2020 19:53:21'!
inspectorClass 
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^SequenceableCollectionInspector! !

!methodRemoval: Float64Array #interpolatedValueAt: stamp: 'jmv 9/3/2020 17:33:30'!
Float64Array removeSelector: #interpolatedValueAt:!
!methodRemoval: Float64Array #squaredLength stamp: 'jmv 9/3/2020 17:34:12'!
Float64Array removeSelector: #squaredLength!
!methodRemoval: Float64Array #* stamp: 'jmv 9/3/2020 17:30:24'!
Float64Array removeSelector: #*!
!methodRemoval: Float64Array #*= stamp: 'jmv 9/3/2020 17:30:25'!
Float64Array removeSelector: #*=!
!methodRemoval: Float64Array #-= stamp: 'jmv 9/3/2020 17:30:30'!
Float64Array removeSelector: #-=!
!methodRemoval: Float64Array #+= stamp: 'jmv 9/3/2020 17:30:28'!
Float64Array removeSelector: #+=!
!methodRemoval: Float64Array #- stamp: 'jmv 9/3/2020 17:30:29'!
Float64Array removeSelector: #-!
!methodRemoval: Float64Array #at: stamp: 'jmv 9/3/2020 17:31:49'!
Float64Array removeSelector: #at:!
!methodRemoval: Float64Array #negated stamp: 'jmv 9/3/2020 17:35:02'!
Float64Array removeSelector: #negated!
!methodRemoval: Float64Array #inspectorClass stamp: 'jmv 9/3/2020 17:46:49'!
Float64Array removeSelector: #inspectorClass!
!methodRemoval: Float64Array #+ stamp: 'jmv 9/3/2020 17:30:26'!
Float64Array removeSelector: #+!
!methodRemoval: Float64Array #/= stamp: 'jmv 9/3/2020 17:30:33'!
Float64Array removeSelector: #/=!
!methodRemoval: Float64Array #interpolateValues:at: stamp: 'jmv 9/3/2020 17:33:26'!
Float64Array removeSelector: #interpolateValues:at:!
!methodRemoval: Float64Array #length stamp: 'jmv 9/3/2020 17:34:25'!
Float64Array removeSelector: #length!
!methodRemoval: Float64Array #divideBy:ifDivisorZero:ifBothZero: stamp: 'jmv 9/3/2020 17:35:54'!
Float64Array removeSelector: #divideBy:ifDivisorZero:ifBothZero:!
!methodRemoval: Float64Array #at:put: stamp: 'jmv 9/3/2020 17:31:52'!
Float64Array removeSelector: #at:put:!
!methodRemoval: Float64Array #defaultElement stamp: 'jmv 9/3/2020 17:32:24'!
Float64Array removeSelector: #defaultElement!
!methodRemoval: Float64Array #/ stamp: 'jmv 9/3/2020 17:30:32'!
Float64Array removeSelector: #/!
!methodRemoval: Float32Array #interpolatedValueAt: stamp: 'jmv 9/3/2020 17:33:35'!
Float32Array removeSelector: #interpolatedValueAt:!
!methodRemoval: Float32Array #squaredLength stamp: 'jmv 9/3/2020 17:34:10'!
Float32Array removeSelector: #squaredLength!
!methodRemoval: Float32Array #+= stamp: 'jmv 9/3/2020 17:30:14'!
Float32Array removeSelector: #+=!
!methodRemoval: Float32Array #-= stamp: 'jmv 9/3/2020 17:30:18'!
Float32Array removeSelector: #-=!
!methodRemoval: Float32Array #- stamp: 'jmv 9/3/2020 17:30:17'!
Float32Array removeSelector: #-!
!methodRemoval: Float32Array #/= stamp: 'jmv 9/3/2020 17:30:20'!
Float32Array removeSelector: #/=!
!methodRemoval: Float32Array #interpolateValues:at: stamp: 'jmv 9/3/2020 17:33:33'!
Float32Array removeSelector: #interpolateValues:at:!
!methodRemoval: Float32Array #'\\=' stamp: 'jmv 9/3/2020 17:39:10'!
Float32Array removeSelector: #'\\='!
!methodRemoval: Float32Array #at:put: stamp: 'jmv 9/3/2020 17:32:00'!
Float32Array removeSelector: #at:put:!
!methodRemoval: Float32Array #defaultElement stamp: 'jmv 9/3/2020 17:32:22'!
Float32Array removeSelector: #defaultElement!
!methodRemoval: Float32Array #negated stamp: 'jmv 9/3/2020 17:35:03'!
Float32Array removeSelector: #negated!
!methodRemoval: Float32Array #* stamp: 'jmv 9/3/2020 17:30:10'!
Float32Array removeSelector: #*!
!methodRemoval: Float32Array #*= stamp: 'jmv 9/3/2020 17:30:12'!
Float32Array removeSelector: #*=!
!methodRemoval: Float32Array #at: stamp: 'jmv 9/3/2020 17:31:57'!
Float32Array removeSelector: #at:!
!methodRemoval: Float32Array #inspectorClass stamp: 'jmv 9/3/2020 17:46:51'!
Float32Array removeSelector: #inspectorClass!
!methodRemoval: Float32Array #+ stamp: 'jmv 9/3/2020 17:30:13'!
Float32Array removeSelector: #+!
!methodRemoval: Float32Array #length stamp: 'jmv 9/3/2020 17:34:23'!
Float32Array removeSelector: #length!
!methodRemoval: Float32Array #divideBy:ifDivisorZero:ifBothZero: stamp: 'jmv 9/3/2020 17:35:56'!
Float32Array removeSelector: #divideBy:ifDivisorZero:ifBothZero:!
!methodRemoval: Float32Array #/ stamp: 'jmv 9/3/2020 17:30:19'!
Float32Array removeSelector: #/!
!methodRemoval: Float32Array #adaptToNumber:andSend: stamp: 'jmv 9/3/2020 17:45:35'!
Float32Array removeSelector: #adaptToNumber:andSend:!

!Float32Array reorganize!
('accessing' floatAt: floatAt:put:)
('arithmetic' dot: normalize sum)
('comparing' = hash hashFull primitiveEqual:)
('primitives-plugin' divideByArray:ifDivisorZero:ifBothZero: divideByScalar:ifDivisorZero:ifBothZero: primAddArray: primAddScalar: primDivArray: primDivScalar: primMulArray: primMulScalar: primSubArray: primSubScalar:)
('private' replaceFrom:to:with:startingAt:)
('testing' is:)
('printing' printElementsOn:)
!


!Float64Array reorganize!
('accessing' floatAt: floatAt:put: size)
('initialization' loadFrom:)
('private' privateLoadFrom: replaceWordsFrom:to:with:startingAt: swapWords)
('objects from disk' bytesPerElement writeOn:)
('arithmetic' dot: normalize)
('primitives-plugin' divideByArray:ifDivisorZero:ifBothZero: divideByScalar:ifDivisorZero:ifBothZero: primAddArray: primAddScalar: primMulArray: primMulScalar: primSubArray: primSubScalar:)
('converting' asIEEE32BitPrecisionFloat)
('testing' isLiteral)
('printing' printOn: storeOn:)
!

