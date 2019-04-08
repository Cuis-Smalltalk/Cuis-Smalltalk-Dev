'From Cuis 5.0 of 7 November 2016 [latest update: #3693] on 8 April 2019 at 9:47:04 am'!

!Float methodsFor: 'testing' stamp: 'jmv 4/8/2019 09:43:05'!
isDenormalized
	"Denormalized numbers are only represented as BoxedFloat64"

	^ false! !


!BoxedFloat64 methodsFor: 'testing' stamp: 'jmv 4/8/2019 09:43:27'!
isDenormalized
	"Denormalized numbers are only represented as BoxedFloat64"

	^ self partBits: [ :signBit :exponentBits :mantissaBits |
		exponentBits = 0 and: [mantissaBits ~=0]]! !


!Complex methodsFor: 'arithmetic' stamp: 'jmv 4/5/2019 19:37:44'!
safeAbs
	"Answer the distance of the receiver from zero (0 + 0 i).
	Try avoiding overflow and/or underflow"

	| scale a b |
	scale _ real abs max: imaginary abs.
	scale = 0.0 
		ifTrue: [^0.0].
	a _ real / scale.
	b _ imaginary / scale.
	^((a * a) + (b * b)) sqrt * scale! !

!Complex methodsFor: 'arithmetic' stamp: 'jmv 4/8/2019 09:44:54'!
safeDivideBy: aComplex
	"Answer the result of dividing receiver by aNumber"
	" Both operands are scaled to avoid arithmetic overflow. 
	  This algorithm works for a wide range of values, and it needs only three divisions."
	| r d newReal newImaginary |
	aComplex real abs > aComplex imaginary abs
		ifTrue: [
			r _ aComplex imaginary / aComplex real.
			d _ r * aComplex imaginary + aComplex real.
			newReal _ r * imaginary + real / d.
			newImaginary _ r negated * real + imaginary / d ]
		ifFalse: [
			r _ aComplex real / aComplex imaginary.
			d _ r * aComplex real + aComplex imaginary.
			newReal _ r * real + imaginary / d.
			newImaginary _ r * imaginary - real / d ].
	^ Complex
		real: newReal
		imaginary: newImaginary.! !


!Complex methodsFor: 'arithmetic' stamp: 'jmv 4/8/2019 09:45:16'!
/ aNumber
	"Answer the result of dividing receiver by aNumber"
	| c d newReal newImaginary s e f |
	aNumber isComplex
		ifTrue: [
			c _ aNumber real.
			d _ aNumber imaginary.
			e _ (real * c) + (imaginary * d).
			e isFinite ifFalse: [ ^ self safeDivideBy: aNumber ].
			f _ (imaginary * c) - (real * d).
			s _ (c * c) + (d * d).
			(e isFloat and: [ s  = 0.0 ]) ifTrue: [ ^ self safeDivideBy: aNumber ].
			newReal _ e / s.
			newImaginary _ f / s ]
		ifFalse: [
			newReal _ real / aNumber.
			newImaginary _ imaginary / aNumber ].
	^ Complex real: newReal imaginary: newImaginary! !

!Complex methodsFor: 'arithmetic' stamp: 'jmv 4/5/2019 19:56:25'!
abs
	"Answer the distance of the receiver from zero (0 + 0 i)."

	| absSquared |
	absSquared _ (real * real) + (imaginary * imaginary).
	absSquared isFloat ifTrue: [
		absSquared < Float fminNormalized ifTrue: [
			^ self safeAbs ].
		absSquared isFinite ifFalse: [
			^ self safeAbs ]].
	^absSquared sqrt! !

!methodRemoval: Complex #absSecure!
Complex removeSelector: #absSecure!
!methodRemoval: Complex #divideFastAndSecureBy:!
Complex removeSelector: #divideFastAndSecureBy:!
