'From Cuis7.9 [latest update: #8183] on 15 September 2026 at 11:17:12 am'!

!Interval methodsFor: 'accessing' stamp: 'jmv 15/Sep/2026, 1:57:14 pm (UTC)'!
max
	^ start max: stop! !

!Interval methodsFor: 'accessing' stamp: 'jmv 15/Sep/2026, 1:57:08 pm (UTC)'!
min
	^ start min: stop! !


!Interval methodsFor: 'testing' stamp: 'jmv 15/Sep/2026, 1:57:39 pm (UTC)'!
includesAllOf: aCollection

	^aCollection isInterval
		ifTrue: [ (self includes: aCollection first)
			and: [ (self includes: aCollection last)
			and: [ self increment abs = aCollection increment abs or: [ super includesAllOf:aCollection ]]]]
		ifFalse: [ super includesAllOf: aCollection ]! !

!Interval methodsFor: 'testing' stamp: 'jmv 15/Sep/2026, 2:03:21 pm (UTC)'!
intersectsRange: anInterval
	"Range comparision ignore count and increment, and only considers bounds."

	^ (self min <= anInterval max) and: [ self max >= anInterval min ].! !

!Interval methodsFor: '*Tools-Refactoring' stamp: 'jmv 15/Sep/2026, 2:06:29 pm (UTC)'!
strictlyIncludesRange: anInterval
	"Range comparision ignore count and increment, and only considers bounds."

	((self min = anInterval min) and: [ self max = anInterval max ])
		ifTrue: [ ^false ].
	^ (self min <= anInterval min) and: [ self max >= anInterval max ].! !

