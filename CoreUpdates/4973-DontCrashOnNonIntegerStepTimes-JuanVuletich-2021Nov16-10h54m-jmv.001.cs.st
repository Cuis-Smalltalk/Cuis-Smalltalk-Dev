'From Cuis 5.0 [latest update: #4972] on 16 November 2021 at 10:56:28 am'!

!StepMessage methodsFor: 'testing' stamp: 'jmv 11/16/2021 10:54:58'!
stepTime
	"Return the step time for this message. If nil, the receiver of the message will be asked for its #stepTime."
	^stepTime ifNil: [ receiver stepTime asInteger ]! !

