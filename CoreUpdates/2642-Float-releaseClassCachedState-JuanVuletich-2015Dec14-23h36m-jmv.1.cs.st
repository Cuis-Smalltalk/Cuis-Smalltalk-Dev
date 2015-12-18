'From Cuis 4.2 of 25 July 2013 [latest update: #2636] on 14 December 2015 at 11:50:27.677492 pm'!

!Float class methodsFor: 'class initialization' stamp: 'jmv 12/14/2015 23:47'!
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

	Epsilon _ 0.000000000001.  "Defines precision of mathematical functions"

	MaxVal _ 1.7976931348623157e308.
	MaxValLn _ 709.782712893384.
	MinValLogBase2 _ -1074.

	Infinity _ MaxVal * MaxVal.
	NegativeInfinity _ 0.0 - Infinity.
	NaN _ Infinity - Infinity.
	NegativeZero _ 1.0 / Infinity negated.
! !

!Float class methodsFor: 'class initialization' stamp: 'jmv 12/14/2015 23:50'!
releaseClassCachedState

	Pi _ nil.
	Halfpi _ nil.
	Twopi _ nil.
	RadiansPerDegree _ nil.

	Ln2 _ nil.
	Ln10 _ nil.
	Sqrt2 _ nil.
	E _ nil.

	Epsilon _ nil.

	MaxVal _ nil.
	MaxValLn _ nil.
	MinValLogBase2 _ nil.

	Infinity _ nil.
	NegativeInfinity _ nil.
	NaN _ nil.
	NegativeZero _ nil! !


!Float class methodsFor: 'class initialization' stamp: 'jmv 12/14/2015 23:48'!
initialize
	"
	Float initialize
	"
	"Constants from Computer Approximations, pp. 182-183:
		Pi = 3.14159265358979323846264338327950288
		Pi/2 = 1.57079632679489661923132169163975144
		Pi*2 = 6.28318530717958647692528676655900576
		Pi/180 = 0.01745329251994329576923690768488612
		2.0 ln = 0.69314718055994530941723212145817657
		2.0 sqrt = 1.41421356237309504880168872420969808"

	self initClassCachedState! !

Float initialize!
