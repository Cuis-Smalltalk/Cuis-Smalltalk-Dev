"
Instances of MessageSend encapsulate message sends to objects. Arguments can be either predefined or supplied when the message send is performed. MessageSends are used to implement the #when:send:to: event system.

Use #value to perform a message send with its predefined arguments and #valueWithArguments: if additonal arguments have to supplied.

Structure:
 receiver		Object -- object receiving the message send
 selector			Symbol -- message selector
 arguments		Array -- bound arguments
"
Class {
	#name : 'MessageSend',
	#superclass : 'Object',
	#instVars : [
		'receiver',
		'selector',
		'arguments'
	],
	#category : 'Kernel-Objects',
	#package : 'Kernel',
	#tag : 'Objects'
}

{ #category : 'instance creation' }
MessageSend class >> receiver: anObject selector: aSymbol [
	^ self receiver: anObject selector: aSymbol arguments: #()
]

{ #category : 'instance creation' }
MessageSend class >> receiver: anObject selector: aSymbol argument: aParameter [
	^ self receiver: anObject selector: aSymbol arguments: (Array with: aParameter)
]

{ #category : 'instance creation' }
MessageSend class >> receiver: anObject selector: aSymbol arguments: anArray [
	"Note: Arguments set here will take preceence over those set when doing #valueWithArguments: and #triggerEvent:withArguments:. If you want them to take precedence, pass here #() or individual nil elements in anArray. See #collectArguments:"

	^ self new
		receiver: anObject;
		selector: aSymbol;
		arguments: anArray
]

{ #category : 'comparing' }
MessageSend >> = anObject [

	"Any object is equal to itself"
	self == anObject ifTrue: [ ^ true ].

	^ anObject species == self species 
		and: [receiver == anObject receiver
		and: [selector == anObject selector
		and: [arguments = anObject arguments]]]
]

{ #category : 'accessing' }
MessageSend >> arguments [
	^ arguments
]

{ #category : 'accessing' }
MessageSend >> arguments: anArray [
	arguments := anArray
]

{ #category : 'converting' }
MessageSend >> asMinimalRepresentation [
	^self
]

{ #category : 'private' }
MessageSend >> collectArguments: anArgArray [
	"Arguments set when builing the receiver (i.e. set when registering for an event)
	take precedence over anArgArray (i.e. set when triggering the event).
	If you want to use arguments set at trigger event time, set to an empty array, or set individual elements to nil."

	| staticArgs answer |
	staticArgs := self arguments.
	answer := Array new: selector numArgs.
	1 to: answer size do: [ :i | | each |
		each := staticArgs at: i ifAbsent: nil.
		each isNil ifTrue: [
			each := anArgArray at: i ifAbsent: nil ].
		answer at: i put: each ].
	^answer
]

{ #category : 'comparing' }
MessageSend >> hash [
	^ receiver hash bitXor: selector hash
]

{ #category : 'testing' }
MessageSend >> is: aSymbol [
	^ aSymbol == #MessageSend or: [ super is: aSymbol ]
]

{ #category : 'testing' }
MessageSend >> isValid [
	^true
]

{ #category : 'printing' }
MessageSend >> printOn: aStream [

        aStream
                nextPutAll: self class name;
                nextPut: $(.
        selector printOn: aStream.
        aStream nextPutAll: ' -> '.
        receiver printOn: aStream.
        aStream nextPut: $)
]

{ #category : 'accessing' }
MessageSend >> receiver [
	^ receiver
]

{ #category : 'accessing' }
MessageSend >> receiver: anObject [
	receiver := anObject
]

{ #category : 'accessing' }
MessageSend >> selector [
	^ selector
]

{ #category : 'accessing' }
MessageSend >> selector: aSymbol [
	selector := aSymbol
]

{ #category : 'evaluating' }
MessageSend >> value [
	"Send the message and answer the return value"

	arguments ifNil: [^ receiver perform: selector].

	^ receiver 
		perform: selector 
		withArguments: (self collectArguments: arguments)
]

{ #category : 'evaluating' }
MessageSend >> valueWithArguments: anArray [

	^ receiver 
		perform: selector 
		withArguments: (self collectArguments: anArray)
]

{ #category : 'evaluating' }
MessageSend >> valueWithPossibleArgument: anObject [

	^selector last == $:
		ifFalse: [ receiver perform: selector ]
		ifTrue: [ receiver perform: selector with: anObject ]
]
