'From Cuis 5.0 of 7 November 2016 [latest update: #2974] on 26 November 2016 at 8:42:22 am'!

!HaloMorph methodsFor: 'stepping' stamp: 'len 7/25/2016 21:39'!
step
	(target isNil or: [target isInWorld not]) ifTrue: [self delete]! !

!HaloMorph methodsFor: 'stepping' stamp: 'len 7/25/2016 21:41'!
stepTime
	^ 100! !

!HaloMorph methodsFor: 'stepping' stamp: 'len 7/25/2016 21:38'!
wantsSteps
	^ true! !

