Class {
	#name : 'StepMessage',
	#superclass : 'MorphicAlarm',
	#instVars : [
		'stepTime',
		'lastEvaluationTime'
	],
	#category : 'Morphic-Events',
	#package : 'Morphic',
	#tag : 'Events'
}

{ #category : 'instance creation' }
StepMessage class >> receiver: aTarget selector: aSelector at: scheduledTime stepTime: timeIntervalMilliseconds [

	^ (self receiver: aTarget selector: aSelector arguments: nil)
		scheduledTime: scheduledTime;
		stepTime: timeIntervalMilliseconds
]

{ #category : 'printing' }
StepMessage >> printOn: aStream [
	super printOn: aStream.
	aStream 
		nextPut: $(;
		print: receiver;
		space;
		print: selector;
		space;
		print: scheduledTime;
		nextPut: $).
]

{ #category : 'evaluating' }
StepMessage >> rescheduleAfter: millisecondTimer [
	"Schedule next run.
	Not before millisecondTimer"
	scheduledTime := scheduledTime + self stepTime max: millisecondTimer+1
]

{ #category : 'testing' }
StepMessage >> stepTime [
	"Return the step time interval for this message in milliseconds.
	If nil, the receiver of the message will be asked for its #stepTime."
	^stepTime ifNil: [ receiver stepTime asInteger ]
]

{ #category : 'accessing' }
StepMessage >> stepTime: aNumber [
	"Set the step time interval for this message in milliseconds.
	If nil, the receiver of the message will be asked for its #stepTime."
	stepTime := aNumber
]

{ #category : 'evaluating' }
StepMessage >> valueAtTime: millisecondClock [

	|  nArgs millisecondsSinceLast answer |
	numArgs ifNil: [numArgs := selector numArgs].
	nArgs := arguments ifNil: [0] ifNotNil: [arguments size].
	lastEvaluationTime ifNil: [ lastEvaluationTime := millisecondClock ].
	millisecondsSinceLast := millisecondClock - lastEvaluationTime.
	lastEvaluationTime := millisecondClock.
	nArgs = numArgs ifTrue: [
		"Ignore extra argument"
		^self value ].
	[
		arguments
			ifNil: [ answer := receiver perform: selector with: millisecondsSinceLast]
			ifNotNil: [ answer := receiver perform: selector withArguments: (arguments copyWith: millisecondsSinceLast) ]
	] ifError: [ :err :rcvr |
		receiver
			stopStepping;
			setProperty: #errorOnStep toValue: true.
		{ 'Error while stepping: '. self. rcvr. err } print.
		"Install the old error handler, so we can re-raise the error"
		rcvr error: err.
	].
	^answer
]
