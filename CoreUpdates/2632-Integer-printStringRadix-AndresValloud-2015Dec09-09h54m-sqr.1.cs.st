'From Cuis 4.2 of 25 July 2013 [latest update: #2631] on 9 December 2015 at 9:55:15.16569 am'!

!Integer methodsFor: 'printing' stamp: 'sqr 12/6/2015 17:48'!
printStringRadix: anInteger

	^anInteger printString, 'r', (self printStringBase: anInteger)! !

