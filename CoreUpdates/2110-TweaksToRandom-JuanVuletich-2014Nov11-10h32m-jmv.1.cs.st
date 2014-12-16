'From Cuis 4.2 of 25 July 2013 [latest update: #2109] on 11 November 2014 at 10:33:05.247552 am'!

!Random methodsFor: 'initialization' stamp: 'jmv 11/11/2014 09:52'!
initialize
	" Set a reasonable Park-Miller starting seed "
	[
		seed _ (Time localMillisecondClock bitAnd: 16r3FFFFFFF) bitXor: self hash.
		seed = 0] whileTrue. 	"Try again if ever get a seed = 0"

	seed _ seed asFloat.
	a _ 16r000041A7 asFloat. 		" magic constant = 16807 "
	m _ 16r7FFFFFFF asFloat. 		" magic constant = 2147483647 "
	q _ m quo: a.
	r _ m rem: a! !

!Random methodsFor: 'private' stamp: 'jmv 11/11/2014 09:53'!
nextValue
	"This method generates random instances of Integer 	in the interval
	0 to 16r7FFFFFFF. This method does NOT update the seed; repeated sends
	answer the same value.
	The algorithm is described in detail in 'Random Number Generators: 
	Good Ones Are Hard to Find' by Stephen K. Park and Keith W. Miller 
	(Comm. Asso. Comp. Mach., 31(10):1192--1201, 1988)."

	| lo hi aLoRHi answer |
	hi _ seed quo: q.
	lo _ seed - (hi * q).  " = seed rem: q"  
	aLoRHi _ (a * lo) - (r * hi).
	answer _ (aLoRHi > 0.0)
		ifTrue:  [ aLoRHi ]
		ifFalse: [ aLoRHi + m ].
	^ answer! !

