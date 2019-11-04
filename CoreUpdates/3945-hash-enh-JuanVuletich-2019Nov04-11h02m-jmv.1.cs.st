'From Cuis 5.0 [latest update: #3944] on 4 November 2019 at 11:11:21 am'!

!Duration methodsFor: 'ansi protocol' stamp: 'jmv 11/4/2019 11:10:43'!
hash
	^seconds hash bitXor: nanos hash! !


!Time methodsFor: 'ansi protocol' stamp: 'jmv 11/4/2019 11:11:16'!
hash

	^ seconds hash bitXor: nanos hash! !


!Character methodsFor: 'comparing' stamp: 'jmv 11/4/2019 11:03:26'!
hash
	"Hash is reimplemented because = is implemented."

	^self numericValue hash! !


!Timespan methodsFor: 'ansi protocol' stamp: 'jmv 11/4/2019 11:02:38'!
hash

	^ start hash bitXor: duration hash
! !


!Interval methodsFor: 'comparing' stamp: 'jmv 11/4/2019 11:07:17'!
hash
	"Hash is reimplemented because = is implemented."

	^ (start hash bitXor: stop hash) bitXor: count hash! !


!KeyboardEvent methodsFor: 'comparing' stamp: 'jmv 11/4/2019 11:02:03'!
hash
	^buttons hash bitXor: keyValue hash
! !

