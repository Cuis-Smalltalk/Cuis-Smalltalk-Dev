"
Any message sent to me is returned as a Message object.

"Message catcher" creates an instance of me.

"
Class {
	#name : 'MessageCatcher',
	#superclass : 'ProtoObject',
	#instVars : [
		'accumulator'
	],
	#category : 'Kernel-Methods',
	#package : 'Kernel',
	#tag : 'Methods'
}

{ #category : 'error handling' }
MessageCatcher >> doesNotUnderstand: aMessage [

	accumulator ifNotNil: [accumulator add: aMessage].
	^ aMessage
]

{ #category : 'error handling' }
MessageCatcher >> error: aString  [
	"Throw a generic Error exception."

	^Error new signal: aString
]

{ #category : 'accessing' }
MessageCatcher >> privAccumulator [

	^ accumulator
]

{ #category : 'accessing' }
MessageCatcher >> privAccumulator: collection [

	accumulator := collection
]
