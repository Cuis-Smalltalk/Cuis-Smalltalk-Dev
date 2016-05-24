'From Cuis 4.2 of 25 July 2013 [latest update: #2770] on 19 May 2016 at 12:02:04.071772 pm'!

!Duration methodsFor: 'public protocol' stamp: 'jmv 5/19/2016 11:58'!
totalMiliseconds

	^ (seconds * 1000) + (nanos // 1e6)! !

