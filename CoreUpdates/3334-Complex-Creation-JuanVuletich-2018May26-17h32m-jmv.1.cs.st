'From Cuis 5.0 of 7 November 2016 [latest update: #3333] on 26 May 2018 at 5:40:37 pm'!

!Complex methodsFor: 'testing' stamp: 'jmv 5/20/2018 21:10:51'!
isInfinite
	^real isInfinite or: [ imaginary isInfinite ]! !

!Complex methodsFor: 'private' stamp: 'jmv 5/20/2018 20:29:34'!
setReal: aNumber1 imaginary: aNumber2
	"Private - initialize the real and imaginary parts of a Complex"
	real _ aNumber1.
	imaginary _ aNumber2! !


!Complex class methodsFor: 'instance creation' stamp: 'jmv 5/20/2018 20:32:34'!
basicReal: realPart imaginary: imaginaryPart
	"Answer a Complex even if imaginary part is zero. Usually you don't want this and just call #real:imaginary:"
	^self basicNew setReal: realPart imaginary: imaginaryPart! !


!Number methodsFor: 'converting' stamp: 'jmv 5/20/2018 20:33:20'!
asComplex
	"Answer a Complex number that represents value of the the receiver."

	^ Complex basicReal: self imaginary: 0! !


!Complex methodsFor: 'arithmetic' stamp: 'jmv 5/20/2018 20:30:21'!
absSecure
	"Answer the distance of the receiver from zero (0 + 0 i).
	Try avoiding overflow and/or underflow"

	| scale |
	scale := real abs max: imaginary abs.
	^scale isZero 
		ifTrue: [scale]
		ifFalse: [(self class basicReal: real / scale imaginary: imaginary / scale) squaredNorm sqrt * scale]! !


!Complex class methodsFor: 'instance creation' stamp: 'jmv 5/20/2018 20:27:23'!
abs: rho arg: theta
	| theta1 |
	"So that if theta is any integer multiple of twoPi, answer is real"
	theta1 _ theta \\ Float twoPi.
	^ self
		real: rho * theta1 cos
		imaginary: rho * theta1 sin! !

!Complex class methodsFor: 'instance creation' stamp: 'jmv 5/20/2018 17:14:55'!
real: realPart imaginary: imaginaryPart
	^ imaginaryPart isZero
		ifTrue: [ realPart ]
		ifFalse: [ self basicReal: realPart imaginary: imaginaryPart ]! !

!methodRemoval: Complex #real:imaginary:!
Complex removeSelector: #real:imaginary:!
