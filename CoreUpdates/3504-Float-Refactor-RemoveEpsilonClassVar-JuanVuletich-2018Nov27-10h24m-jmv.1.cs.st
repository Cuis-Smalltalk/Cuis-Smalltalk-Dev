'From Cuis 5.0 of 7 November 2016 [latest update: #3501] on 27 November 2018 at 1:42:06 pm'!
!classDefinition: #Float category: #'Kernel-Numbers'!
Number subclass: #Float
	instanceVariableNames: ''
	classVariableNames: 'E Epsilon Halfpi Infinity Ln10 Ln2 MaxVal MaxValLn MinValLogBase2 NaN NegativeInfinity NegativeZero Pi RadiansPerDegree Sqrt2 Twopi '
	poolDictionaries: ''
	category: 'Kernel-Numbers'!

!Float methodsFor: 'mathematical functions' stamp: 'jmv 11/27/2018 13:10:52'!
arcTanNonPrimitive
	"Answer the angle in radians."

	| theta delta sinTheta cosTheta |

	"Newton-Raphson"
	self < 0.0 ifTrue: [ ^ 0.0 - (0.0 - self) arcTan ].

	"first guess"
	theta _ (self * Halfpi) / (self + 1.0).

	"iterate"
	[
		sinTheta _ theta sin.
		cosTheta _ theta cos.
		delta _ (sinTheta * cosTheta) - (self * cosTheta * cosTheta).
		delta abs >= theta ulp ]
		whileTrue: [
			theta _ theta - delta ].
	^ theta! !

!Float methodsFor: 'mathematical functions' stamp: 'jmv 11/27/2018 13:24:16'!
expNonPrimitive
	"Answer E raised to the receiver power."

	| base fract correction delta div |

	"Taylor series"
	"check the special cases"
	self < 0.0 ifTrue: [^ (self negated exp) reciprocal].
	self = 0.0 ifTrue: [^ 1].
	self abs > MaxValLn ifTrue: [self error: 'exp overflow'].

	"get first approximation by raising e to integer power"
	base _ E raisedToInteger: (self truncated).

	"now compute the correction with a short Taylor series"
	"fract will be 0..1, so correction will be 1..E"
	"in the worst case, convergance time is logarithmic with 1/Epsilon"
	fract _ self fractionPart.
	fract = 0.0 ifTrue: [ ^ base ].  "no correction required"

	correction _ 1.0 + fract.
	delta _ fract * fract / 2.0.
	div _ 2.0.
	[delta >= base ulp] whileTrue: [
		correction _ correction + delta.
		div _ div + 1.0.
		delta _ delta * fract / div].
	correction _ correction + delta.
	^ base * correction! !

!Float methodsFor: 'mathematical functions' stamp: 'jmv 11/27/2018 13:32:39'!
lnNonPrimitive
	"Answer the natural logarithm of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	| expt n mant x div pow delta sum |

	"Taylor series"
	self <= 0.0 ifTrue: [DomainError signal: 'ln is only defined for x > 0.0'].

	"get a rough estimate from binary exponent"
	expt := self exponent.
	n := Ln2 * expt.
	mant := self timesTwoPower: 0 - expt.

	"compute fine correction from mantinssa in Taylor series"
	"mant is in the range [0..2]"
	"we unroll the loop to avoid use of abs"
	x := mant - 1.0.
	div := 1.0.
	pow := delta := sum := x.
	x := x negated.  "x <= 0"
	[delta > (n + sum) ulp] whileTrue: [
		"pass one: delta is positive"
		div := div + 1.0.
		pow := pow * x.
		delta := pow / div.
		sum := sum + delta.
		"pass two: delta is negative"
		div := div + 1.0.
		pow := pow * x.
		delta := pow / div.
		sum := sum + delta].

	^ n + sum

	"Float e ln 1.0"! !

!Float methodsFor: 'mathematical functions' stamp: 'jmv 11/27/2018 13:40:01'!
sinNonPrimitive
	"Answer the sine of the receiver taken as an angle in radians."

	| sum delta self2 i |

	"Taylor series"
	"normalize to the range [0..Pi/2]"
	self < 0.0 ifTrue: [^ (0.0 - ((0.0 - self) sin))].
	self > Twopi ifTrue: [^ (self \\ Twopi) sin].
	self > Pi ifTrue: [^ (0.0 - (self - Pi) sin)].
	self > Halfpi ifTrue: [^ (Pi - self) sin].

	"unroll loop to avoid use of abs"
	sum _ delta _ self.
	self2 _ 0.0 - (self * self).
	i _ 2.0.
	[delta >= sum ulp] whileTrue: [
		"once"
		delta _ (delta * self2) / (i * (i + 1.0)).
		i _ i + 2.0.
		sum _ sum + delta.
		"twice"
		delta _ (delta * self2) / (i * (i + 1.0)).
		i _ i + 2.0.
		sum _ sum + delta].
	^ sum! !

!Float methodsFor: 'mathematical functions' stamp: 'jmv 11/27/2018 13:10:59'!
sqrtNonPrimitive
	"Answer the square root of the receiver. "
	| exp guess delta |

	"Complex answer"
	self <= 0.0
		ifTrue: [
			^ self = 0.0
				ifFalse: [ (0.0 - self) sqrt i ]
				ifTrue: [ self ]	"Answer 0.0 for 0.0, but -0.0 for -0.0. See IEEE 754 standard"
				].

	"NaN and Infinity"
	self isFinite ifFalse: [
		^ self ].

	"Newton-Raphson"
	"first guess is half the exponent"
	exp _ self exponent // 2.
	guess _ self timesTwoPower: 0 - exp.
	[
		delta _ self - (guess * guess) / (guess * 2.0).
		delta abs >= guess ulp ]
	whileTrue: [
		guess _ guess + delta ].
	^ guess! !


!Number methodsFor: 'mathematical functions' stamp: 'jmv 11/27/2018 11:09:00'!
arcTan
	"The receiver is the tangent of an angle. Answer the angle measured in  radians.
	Note: If the purpose is to recover the angle of some vector, prefer #arcTan:
		See, for example, Complex>>#argument"

	^self asFloat arcTan! !


!Float methodsFor: 'mathematical functions' stamp: 'jmv 11/27/2018 10:57:09'!
sqrt
	"Answer the square root of the receiver. 
	 Use primitive if available, or Smalltalk code if primitive is unavailable or fails."

	| prim |
	prim _ self primSqrt.
	prim isNaN ifFalse: [ ^prim ].

	^ self sqrtNonPrimitive! !


!Float class methodsFor: 'class initialization' stamp: 'jmv 11/27/2018 13:40:39'!
initClassCachedState
	"Float initialize"
	"Constants from Computer Approximations, pp. 182-183:
		Pi = 3.14159265358979323846264338327950288
		Pi/2 = 1.57079632679489661923132169163975144
		Pi*2 = 6.28318530717958647692528676655900576
		Pi/180 = 0.01745329251994329576923690768488612
		2.0 ln = 0.69314718055994530941723212145817657
		2.0 sqrt = 1.41421356237309504880168872420969808"

	Pi _ 3.14159265358979323846264338327950288.
	Halfpi _ Pi / 2.0.
	Twopi _ Pi * 2.0.
	RadiansPerDegree _ Pi / 180.0.

	Ln2 _ 0.69314718055994530941723212145817657.
	Ln10 _ 10.0 ln.
	Sqrt2 _ 1.41421356237309504880168872420969808.
	E _ 2.718281828459045235360287471353.

	MaxVal _ 1.7976931348623157e308.
	MaxValLn _ 709.782712893384.
	MinValLogBase2 _ -1074.

	Infinity _ MaxVal * MaxVal.
	NegativeInfinity _ 0.0 - Infinity.
	NaN _ Infinity - Infinity.
	NegativeZero _ 1.0 / Infinity negated.
! !

!Float class methodsFor: 'class initialization' stamp: 'jmv 11/27/2018 13:40:43'!
releaseClassCachedState

	Pi _ nil.
	Halfpi _ nil.
	Twopi _ nil.
	RadiansPerDegree _ nil.

	Ln2 _ nil.
	Ln10 _ nil.
	Sqrt2 _ nil.
	E _ nil.

	MaxVal _ nil.
	MaxValLn _ nil.
	MinValLogBase2 _ nil.

	Infinity _ nil.
	NegativeInfinity _ nil.
	NaN _ nil.
	NegativeZero _ nil! !


!BoxedFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 11/27/2018 11:12:00'!
arcTan
	"Answer the angle in radians.
	 Optional. See Object documentation whatIsAPrimitive.
	Note: If the purpose is to recover the angle of some vector, prefer #arcTan:
		See, for example, Complex>>#argument"

	<primitive: 57>
	^ self arcTanNonPrimitive! !

!BoxedFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 11/27/2018 11:14:01'!
exp
	"Answer E raised to the receiver power.
	 Optional. See Object documentation whatIsAPrimitive." 

	<primitive: 59>
	^ self expNonPrimitive! !

!BoxedFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 11/27/2018 11:14:45'!
ln
	"Answer the natural logarithm of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 58>
	^ self lnNonPrimitive! !

!BoxedFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 11/27/2018 11:15:17'!
sin
	"Answer the sine of the receiver taken as an angle in radians.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 56>
	^ self sinNonPrimitive! !


!SmallFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 11/27/2018 11:11:58'!
arcTan
	"Answer the angle in radians.
	 Optional. See Object documentation whatIsAPrimitive.
	Note: If the purpose is to recover the angle of some vector, prefer #arcTan:
		See, for example, Complex>>#argument"

	<primitive: 557>
	^self arcTanNonPrimitive! !

!SmallFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 11/27/2018 11:13:51'!
exp
	"Answer E raised to the receiver power.
	 Optional. See Object documentation whatIsAPrimitive." 

	<primitive: 559>
	^ self expNonPrimitive! !

!SmallFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 11/27/2018 11:14:36'!
ln
	"Answer the natural logarithm of the receiver.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 558>
	^ self lnNonPrimitive! !

!SmallFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 11/27/2018 11:15:23'!
sin
	"Answer the sine of the receiver taken as an angle in radians.
	 Optional. See Object documentation whatIsAPrimitive."

	<primitive: 556>
	^ self sinNonPrimitive! !


!Integer methodsFor: 'mathematical functions' stamp: 'jmv 11/27/2018 10:30:26'!
sqrt
	"Answer the square root of the receiver."

	| selfAsFloat floatResult guess |
	selfAsFloat _ self asFloat.
	floatResult _ selfAsFloat sqrt.

	floatResult isInfinite ifFalse: [
		guess _ floatResult truncated.

		"If got an exact answer, answer it. Otherwise answer float approximate answer."
		guess squared = self
			ifTrue: [ ^ guess ]].

	"In this case, maybe it failed because we are such a big integer that the Float method becomes
	inexact, even if we are a whole square number. So, try the slower but more general method."
	selfAsFloat >= Float maxExactInteger asFloat squared
		ifTrue: [
			guess _ self sqrtFloor.
			guess squared = self
				ifTrue: [ ^guess ].
			
			"Nothing else can be done. No exact answer means answer must be a Float.
			Answer the best we can which is the rounded sqrt."
			^ self sqrtRounded asFloat ].

	"We need an approximate result"
	^floatResult! !

!classDefinition: #Float category: #'Kernel-Numbers'!
Number subclass: #Float
	instanceVariableNames: ''
	classVariableNames: 'E Halfpi Infinity Ln10 Ln2 MaxVal MaxValLn MinValLogBase2 NaN NegativeInfinity NegativeZero Pi RadiansPerDegree Sqrt2 Twopi'
	poolDictionaries: ''
	category: 'Kernel-Numbers'!
