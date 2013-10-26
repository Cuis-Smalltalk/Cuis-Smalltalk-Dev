'From Cuis 4.0 of 21 April 2012 [latest update: #1344] on 24 July 2012 at 3:13:10 pm'!

!Matrix methodsFor: 'copying' stamp: 'jmv 7/24/2012 09:02'!
postCopy
	elements _ elements copy! !

!methodRemoval: Matrix #copy!
Matrix removeSelector: #copy!
