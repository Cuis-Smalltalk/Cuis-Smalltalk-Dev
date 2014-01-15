'From Cuis 4.2 of 25 July 2013 [latest update: #1919] on 5 January 2014 at 8:03:07.037848 pm'!

!Number methodsFor: 'printing' stamp: 'jmv 1/5/2014 19:55'!
printString

	^String streamContents: [:str | self printOn: str base: 10]! !

!methodRemoval: LargePositiveInteger #printStringBase:!
LargePositiveInteger removeSelector: #printStringBase:!
!methodRemoval: Integer #printOn:!
Integer removeSelector: #printOn:!
!methodRemoval: Integer #printString!
Integer removeSelector: #printString!
!methodRemoval: Fraction #printOn:!
Fraction removeSelector: #printOn:!
