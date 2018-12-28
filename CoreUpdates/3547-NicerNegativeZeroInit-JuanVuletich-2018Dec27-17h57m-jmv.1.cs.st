'From Cuis 5.0 of 7 November 2016 [latest update: #3546] on 27 December 2018 at 5:58:46 pm'!

!Float class methodsFor: 'constants' stamp: 'jmv 12/26/2018 12:50:56'!
emax
	"Answer exponent of the maximal representable finite value"
	
	^1023! !

!Float class methodsFor: 'class initialization' stamp: 'jmv 12/26/2018 13:08:28'!
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
	NegativeZero _ Float fmin negated nextTowardsZero.
! !

