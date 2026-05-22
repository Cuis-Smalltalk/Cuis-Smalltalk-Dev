Class {
	#name : 'MorphicAlarm',
	#superclass : 'MessageSend',
	#instVars : [
		'scheduledTime',
		'numArgs'
	],
	#category : 'Morphic-Events',
	#package : 'Morphic',
	#tag : 'Events'
}

{ #category : 'instance creation' }
MorphicAlarm class >> receiver: aTarget selector: aSelector arguments: argArray at: scheduledTime [

	^ (self receiver: aTarget selector: aSelector arguments: argArray)
		scheduledTime: scheduledTime
]

{ #category : 'accessing' }
MorphicAlarm >> scheduledTime [
	"Return the time (in milliseconds) that the receiver is scheduled to be executed"
	^scheduledTime
]

{ #category : 'accessing' }
MorphicAlarm >> scheduledTime: msecs [
	"Set the time (in milliseconds) that the receiver is scheduled to be executed"
	scheduledTime := msecs
]

{ #category : 'evaluating' }
MorphicAlarm >> valueAtTime: millisecondClock [

	|  nArgs |
	numArgs ifNil: [numArgs := selector numArgs].
	nArgs := arguments ifNil: [0] ifNotNil: [arguments size].
	nArgs = numArgs ifTrue: [
		"Ignore extra argument"
		^self value ].
	^arguments
		ifNil: [ receiver perform: selector with: millisecondClock]
		ifNotNil: [ receiver perform: selector withArguments: (arguments copyWith: millisecondClock) ]
]
