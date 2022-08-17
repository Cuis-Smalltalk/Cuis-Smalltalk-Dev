'From Cuis 6.0 [latest update: #5458] on 17 August 2022 at 3:39:48 pm'!

!Process methodsFor: 'testing' stamp: 'jmv 8/17/2022 15:38:56'!
isTerminated
	"Answer if the receiver is terminated. A process is considered terminated
	if the suspendedContext is the bottomContext and the pc is at the endPC,
	or if all sender context are at the endPC, up to the bottomContext."

	| ctx |
	self isRunning ifTrue: [^ false].
	suspendedContext isNil ifTrue: [ ^true ].
	ctx _ suspendedContext.
	[
		"Any context that can further run means we're not terminated"
		(ctx isDead or: [ctx atEnd]) ifFalse: [ ^false ].
		"But if we already checked the whole chain, we're done"
		ctx isBottomContext ifTrue: [ ^true ].
		ctx _ ctx sender ] repeat.! !

