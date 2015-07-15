'From Cuis 4.2 of 25 July 2013 [latest update: #2400] on 4 July 2015 at 11:55:56.95276 am'!

!Float64Array methodsFor: 'arithmetic' stamp: 'jmv 7/4/2015 11:50'!
* anObject

	^self copy *= anObject! !

!Float64Array methodsFor: 'arithmetic' stamp: 'jmv 7/4/2015 11:50'!
*= anObject
	^anObject isNumber
		ifTrue:[self primMulScalar: anObject asFloat]
		ifFalse:[self primMulArray: anObject]! !

!Float64Array methodsFor: 'arithmetic' stamp: 'jmv 7/4/2015 11:52'!
+ anObject

	^self copy += anObject! !

!Float64Array methodsFor: 'arithmetic' stamp: 'jmv 7/4/2015 11:52'!
+= anObject
	^anObject isNumber
		ifTrue:[self primAddScalar: anObject asFloat]
		ifFalse:[self primAddArray: anObject]! !

!Float64Array methodsFor: 'arithmetic' stamp: 'jmv 7/4/2015 11:53'!
- anObject

	^self copy -= anObject! !

!Float64Array methodsFor: 'arithmetic' stamp: 'jmv 7/4/2015 11:53'!
-= anObject
	^anObject isNumber
		ifTrue:[self primSubScalar: anObject asFloat]
		ifFalse:[self primSubArray: anObject]! !

!Float64Array methodsFor: 'arithmetic' stamp: 'jmv 7/4/2015 11:53'!
/ anObject

	^self copy /= anObject! !

!Float64Array methodsFor: 'arithmetic' stamp: 'jmv 7/3/2015 16:22'!
/= anObject
	^anObject isNumber
		ifTrue:[self primDivScalar: anObject asFloat]
		ifFalse:[self primDivArray: anObject]! !

!Float64Array methodsFor: 'arithmetic' stamp: 'jmv 7/4/2015 11:54'!
negated

	^self copy *= -1! !

!Float64Array methodsFor: 'arithmetic' stamp: 'jmv 7/4/2015 11:54'!
normalize
	"Unsafely normalize the receiver in-place (become a unit vector).
 	 Div-by-Zero raised if len 0."
	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveNormalize' module: 'FloatArrayPlugin'>"
	self /= self length.! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'jmv 7/4/2015 11:53'!
primAddArray: floatArray

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveAddFloatArray' module: 'FloatArrayPlugin'>"
	1 to: self size do:[:i| self at: i put: (self at: i) + (floatArray at: i)].! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'jmv 7/4/2015 11:53'!
primAddScalar: scalarValue

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveAddScalar' module: 'FloatArrayPlugin'>"
	1 to: self size do:[:i| self at: i put: (self at: i) + scalarValue].! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'jmv 7/3/2015 16:21'!
primDivArray: floatArray

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveDivFloatArray' module: 'FloatArrayPlugin'>"
	1 to: self size do:[:i| self at: i put: (self at: i) / (floatArray at: i)].! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'jmv 7/3/2015 16:21'!
primDivScalar: scalarValue

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveDivScalar' module: 'FloatArrayPlugin'>"
	1 to: self size do:[:i| self at: i put: (self at: i) / scalarValue].! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'jmv 7/4/2015 11:52'!
primMulArray: floatArray

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveMulFloatArray' module: 'FloatArrayPlugin'>"
	1 to: self size do:[:i| self at: i put: (self at: i) * (floatArray at: i)].! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'jmv 7/4/2015 11:52'!
primMulScalar: scalarValue

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveMulScalar' module: 'FloatArrayPlugin'>"
	1 to: self size do:[:i| self at: i put: (self at: i) * scalarValue].! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'jmv 7/4/2015 11:55'!
primSubArray: floatArray

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveSubFloatArray' module: 'FloatArrayPlugin'>"
	1 to: self size do:[:i| self at: i put: (self at: i) - (floatArray at: i)].! !

!Float64Array methodsFor: 'primitives-plugin' stamp: 'jmv 7/4/2015 11:55'!
primSubScalar: scalarValue

	"It would be nice to have FloatArrayPlugin or equivalent for Float64Array...
	<primitive: 'primitiveSubScalar' module: 'FloatArrayPlugin'>"
	1 to: self size do:[:i| self at: i put: (self at: i) - scalarValue].! !


!FloatArray methodsFor: 'arithmetic' stamp: 'jmv 7/4/2015 11:53'!
sum

	<primitive: 'primitiveSum' module: 'FloatArrayPlugin'>
	^ super sum! !


!FloatArray reorganize!
('accessing' at: at:put: defaultElement length squaredLength)
('arithmetic' * *= + += - -= / /= adaptToNumber:andSend: dot: negated normalize sum \\=)
('comparing' = hash)
('primitives-plugin' primAddArray: primAddScalar: primDivArray: primDivScalar: primMulArray: primMulScalar: primSubArray: primSubScalar:)
('converting' asFloatArray)
('private' replaceFrom:to:with:startingAt:)
('inspecting' inspectorClass)
('testing' is:)
('math functions' derivative)
('interpolating' interpolatedValueAt:)
!

