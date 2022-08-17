'From Cuis 6.0 [latest update: #5457] on 17 August 2022 at 1:14:10 pm'!

!Process methodsFor: 'testing' stamp: 'jmv 8/17/2022 13:13:28'!
isTerminated
	"Answer if the receiver is terminated. A process is considered terminated
	if the suspendedContext is the bottomContext and the pc is at the endPC"

	| ctx |
	self isRunning ifTrue: [^ false].
	suspendedContext isNil ifTrue: [ ^true ].
	suspendedContext isBottomContext ifTrue: [
		^suspendedContext isDead or: [suspendedContext atEnd] ].

	"This will only happen if prim 578 in #suspend fails because we are running on an old VM,
	and #suspendAndUnblock gets called"
	suspendedContext method == (Process>>#suspend) ifFalse: [^false].
	suspendedContext atEnd ifFalse: [^false].
	ctx _ suspendedContext sender.
	^ ctx isBottomContext and: [
		ctx isDead or: [ctx atEnd]].! !

