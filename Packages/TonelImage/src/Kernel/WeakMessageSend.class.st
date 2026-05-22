"
Instances of WeakMessageSend encapsulate message sends to objects, like MessageSend. Unlike MessageSend it is not necessarily a valid mesage.  A request to value only results in a send if in fact it is valid. 

See MessageSend comment also. WeakMessageSend is used primarily for event registration. 

Note: References receiver and arguments weakly. If receiver or arguments get collected, #value will do nothing.
"
Class {
	#name : 'WeakMessageSend',
	#superclass : 'Object',
	#instVars : [
		'selector',
		'shouldBeNil',
		'arguments'
	],
	#category : 'Kernel-Objects',
	#package : 'Kernel',
	#tag : 'Objects'
}

{ #category : 'instance creation' }
WeakMessageSend class >> new [
	^self new: 1

]

{ #category : 'instance creation' }
WeakMessageSend class >> receiver: anObject selector: aSymbol [
	^ self receiver: anObject selector: aSymbol arguments: #()

]

{ #category : 'instance creation' }
WeakMessageSend class >> receiver: anObject selector: aSymbol argument: aParameter [
	^ self receiver: anObject selector: aSymbol arguments: (Array with: aParameter)

]

{ #category : 'instance creation' }
WeakMessageSend class >> receiver: anObject selector: aSymbol arguments: anArray [
	"Note: Arguments set here will take preceence over those set when doing #valueWithArguments: and #triggerEvent:withArguments:. If you want them to take precedence, pass here #() or individual nil elements in anArray. See #collectArguments:"

	^ self new
		receiver: anObject;
		selector: aSymbol;
		arguments: anArray
]

{ #category : 'comparing' }
WeakMessageSend >> = anObject [

	"Any object is equal to itself"
	self == anObject ifTrue: [ ^ true ].

	"Compare equal to equivalent MessageSend"
	^ (anObject is: #MessageSend)
		and: [self receiver == anObject receiver
		and: [selector == anObject selector
		and: [(Array withAll: arguments) = (Array withAll: anObject arguments)]]]

]

{ #category : 'accessing' }
WeakMessageSend >> arguments [
	^arguments ifNil: [ #() ]

]

{ #category : 'accessing' }
WeakMessageSend >> arguments: anArray [
	arguments := WeakArray withAll: anArray.
	"no reason this should be a WeakArray"
	shouldBeNil := Array withAll: (anArray collect: [ :ea | ea isNil ]).

]

{ #category : 'converting' }
WeakMessageSend >> asActionSequence [

	^WeakActionSequence with: self
]

{ #category : 'converting' }
WeakMessageSend >> asMessageSend [
	^MessageSend receiver: self receiver selector: selector arguments: (Array withAll: self arguments) 

]

{ #category : 'converting' }
WeakMessageSend >> asMinimalRepresentation [

	^self isReceiverOrAnyArgumentGarbage
		ifTrue: [ nil ]
		ifFalse: [ self ]
]

{ #category : 'private' }
WeakMessageSend >> collectArguments: anArgArray [
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
WeakMessageSend >> hash [
	"work like MessageSend>>hash"
	^self receiver hash bitXor: selector hash

]

{ #category : 'testing' }
WeakMessageSend >> is: aSymbol [
	^ aSymbol == #MessageSend or: [ super is: aSymbol ]
]

{ #category : 'private' }
WeakMessageSend >> isAnyArgumentGarbage [
	"Make sure that my arguments haven't gone away"
	
	^arguments notNil and: [ arguments with: shouldBeNil anySatisfy: [ :arg :flag | flag not and: [arg isNil]]].
	

]

{ #category : 'private' }
WeakMessageSend >> isReceiverGarbage [
	"Make sure that my receiver hasn't gone away"
	^self receiver isNil

]

{ #category : 'private' }
WeakMessageSend >> isReceiverOrAnyArgumentGarbage [
	"Make sure that my receiver hasn't gone away"
	^self isReceiverGarbage 
		or: [self isAnyArgumentGarbage]

]

{ #category : 'printing' }
WeakMessageSend >> printOn: aStream [

        aStream
                nextPutAll: self class name;
                nextPut: $(.
        selector printOn: aStream.
        aStream nextPutAll: ' -> '.
        self receiver printOn: aStream.
        aStream nextPut: $)

]

{ #category : 'accessing' }
WeakMessageSend >> receiver [
	^self at: 1

]

{ #category : 'accessing' }
WeakMessageSend >> receiver: anObject [
	self at: 1 put: anObject

]

{ #category : 'accessing' }
WeakMessageSend >> selector [
	^selector

]

{ #category : 'accessing' }
WeakMessageSend >> selector: aSymbol [
	selector := aSymbol

]

{ #category : 'evaluating' }
WeakMessageSend >> value [
	^ arguments
		ifNil: [
			self withEnsuredReceiverDo: [ :r | r perform: selector ]]
		ifNotNil: [
			self withEnsuredReceiverAndArgumentsDo: [ :r :a |
				r
					perform: selector
					withArguments: a ]]
]

{ #category : 'evaluating' }
WeakMessageSend >> valueWithArguments: anArray [
	| argsToUse |
	
	"Safe to use, because they are built before ensureing receiver and args..."
	argsToUse := self collectArguments: anArray.
	^ self withEnsuredReceiverAndArgumentsDo: [ :r :a |
		r
			perform: selector
			withArguments: argsToUse ]
]

{ #category : 'private' }
WeakMessageSend >> withEnsuredReceiverAndArgumentsDo: aBlock [
	"Grab real references to receiver and arguments. If they still exist, evaluate aBlock."

	"Return if my receiver has gone away"
	| r a |
	r := self receiver.
	r ifNil: [ ^nil ].

	
	"Make sure that my arguments haven't gone away"
	a := Array withAll: arguments.
	a with: shouldBeNil do: [ :arg :flag |
		arg ifNil: [ flag ifFalse: [ ^nil ]]
	].

	^aBlock value: r value: a
]

{ #category : 'private' }
WeakMessageSend >> withEnsuredReceiverDo: aBlock [
	"Grab a real reference to receive. If still there, evaluate aBlock."
	"Return nil if my receiver has gone away"

	^ self receiver ifNotNil: [ :r | aBlock value: r ]
]
