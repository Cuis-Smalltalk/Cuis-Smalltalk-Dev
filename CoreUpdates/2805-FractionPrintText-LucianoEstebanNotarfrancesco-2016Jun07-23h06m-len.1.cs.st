'From Cuis 4.2 of 25 July 2013 [latest update: #2795] on 7 June 2016 at 11:09:04.891512 pm'!

!Fraction methodsFor: 'printing' stamp: 'len 6/7/2016 21:42'!
printOn: aStream base: base
	aStream isText
		ifTrue:
			[aStream nextPutAll: (numerator printStringBase: base) super; nextPut: $/; nextPutAll: (denominator printStringBase: base) sub.
			^ self].
	aStream nextPut: $(.
	numerator printOn: aStream base: base.
	aStream nextPut: $/.
	denominator printOn: aStream base: base.
	aStream nextPut: $).
! !

