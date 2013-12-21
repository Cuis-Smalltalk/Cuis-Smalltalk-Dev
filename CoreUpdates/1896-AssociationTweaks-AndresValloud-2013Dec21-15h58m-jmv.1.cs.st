'From Cuis 4.2 of 25 July 2013 [latest update: #1895] on 21 December 2013 at 4:00:37.608042 pm'!

!Association methodsFor: 'printing' stamp: 'sqr 12/20/2013 18:24'!
printOn: aStream

	aStream
		print: self key;
		nextPutAll: ' -> ';
		print: self value! !

!methodRemoval: WeakKeyAssociation #printOn:!
WeakKeyAssociation removeSelector: #printOn:!
