'From Cuis 5.0 [latest update: #4384] on 28 September 2020 at 12:05:40 pm'!

!StepMessage methodsFor: 'evaluating' stamp: 'jmv 9/28/2020 12:05:32'!
valueAtTime: millisecondClock

	|  nArgs millisecondsSinceLast answer |
	numArgs ifNil: [numArgs _ selector numArgs].
	nArgs _ arguments ifNil: [0] ifNotNil: [arguments size].
	lastEvaluationTime ifNil: [ lastEvaluationTime _ millisecondClock ].
	millisecondsSinceLast _ millisecondClock - lastEvaluationTime.
	lastEvaluationTime _ millisecondClock.
	nArgs = numArgs ifTrue: [
		"Ignore extra argument"
		^self value ].
	[
		arguments
			ifNil: [ answer _ receiver perform: selector with: millisecondsSinceLast]
			ifNotNil: [ answer _ receiver perform: selector withArguments: (arguments copyWith: millisecondsSinceLast) ]
	] ifError: [ :err :rcvr |
		receiver stopStepping.
		{ 'Error while stepping: '. self. rcvr. err } print.
		"Install the old error handler, so we can re-raise the error"
		rcvr error: err.
	].
	^answer! !

