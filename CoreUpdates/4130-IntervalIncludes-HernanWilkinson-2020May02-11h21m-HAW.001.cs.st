'From Cuis 5.0 [latest update: #4129] on 2 May 2020 at 12:48:49 pm'!

!Interval methodsFor: 'testing' stamp: 'HAW 5/2/2020 11:53:30'!
includes: aNumber

	| index |
	
	aNumber isNumber ifFalse: [ ^ false ].
	
	^ start = stop 
		ifTrue: [ start = aNumber ]
		ifFalse: [ 
			index := (aNumber - start) / (stop-start) * (count-1) + 1.
			index isInteger and: [ index between: 1 and: count ]]! !

