'From Cuis7.9 [latest update: #7998] on 16 June 2026 at 9:40:40 am'!

!BoxedFloat64 methodsFor: 'truncation and round off' library: #MinimalKernel03Float stamp: 'jmv 2/Aug/2019 09:34:43'!
truncated
	"Answer with a SmallInteger equal to the value of the receiver without 
	its fractional part. The primitive fails if the truncated value cannot be 
	represented as a SmallInteger. In that case, the code below will compute 
	a LargeInteger truncated value.
	Raise an exception if no conversion to integer is possible, i.e. for Infinities and NaN.
	Essential. See Object documentation whatIsAPrimitive. "

	<primitive: 51>

	^ self partValues: [ :sign :exponent :mantissa |
		sign * (mantissa bitShift: exponent - 52) ]! !

