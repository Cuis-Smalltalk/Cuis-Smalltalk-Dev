'From Cuis 5.0 [latest update: #4162] on 21 May 2020 at 10:31:06 pm'!

!BoxedFloat64 methodsFor: 'truncation and round off' stamp: 'jmv 5/21/2020 22:23:44'!
truncated
	"Answer with a SmallInteger equal to the value of the receiver without 
	its fractional part. The primitive fails if the truncated value cannot be 
	represented as a SmallInteger. In that case, the code below will compute 
	a LargeInteger truncated value.
	Raise an exception if no conversion to integer is possible, i.e. for Infinities and NaN.
	Essential. See Object documentation whatIsAPrimitive. "

	<primitive: 51>
	self isFinite ifFalse: [ ^self ].
	^ self partValues: [ :sign :exponent :mantissa |
		sign * (mantissa bitShift: exponent - 52) ]! !

