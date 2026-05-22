"
ProtoObject establishes minimal behavior required of any object in Squeak, even objects that should balk at normal object behavior. Generally these are proxy objects designed to read themselves in from the disk, or to perform some wrapper behavior, before responding to a message. Current examples are ObjectOut and ImageSegmentRootStub, and one could argue that ObjectTracer should also inherit from this class.

ProtoObject has no instance variables, nor should any be added.
"
Class {
	#name : 'ProtoObject',
	#superclass : 'nil',
	#category : 'Kernel-Objects',
	#package : 'Kernel',
	#tag : 'Objects'
}

{ #category : 'as yet unclassified' }
ProtoObject class >> initializedInstance [
	^ nil
]

{ #category : 'comparing' }
ProtoObject >> == anObject  [
	"Primitive. Answer whether the receiver and the argument are the same 
	object (have the same object pointer). Do not redefine the message == in 
	any other class! Essential. No Lookup. Do not override in any subclass. 
	See Object documentation whatIsAPrimitive."

	<primitive: 110>
	self primitiveFailed
]

{ #category : 'comparing' }
ProtoObject >> ~~ anObject [
	"Answer whether the receiver and the argument are not the same object 
	(do not have the same object pointer)."

	self == anObject
		ifTrue: [^ false]
		ifFalse: [^ true]
]

{ #category : 'system primitives' }
ProtoObject >> become: otherObject  [
	"Primitive. Swap the object pointers of the receiver and the argument.
	All variables in the entire system that used to point to the 
	receiver now point to the argument, and vice-versa.
	Fails if either object is a SmallInteger.
	This implementation is appropriate for minimal bootstrapped images.
	Regular images will automatically use the implementation in Object (if present)."

	{ self } elementsExchangeIdentityWith: { otherObject }
]

{ #category : 'system primitives' }
ProtoObject >> cannotInterpret: aMessage  [
	 "Handle the fact that there was an attempt to send the given message to the receiver but a null methodDictionary was encountered while looking up the message selector.  Hopefully this is the result of encountering a stub for a swapped out class which induces this exception on purpose."

"If this is the result of encountering a swap-out stub, then simulating the lookup in Smalltalk should suffice to install the class properly, and the message may be resent."

	(self class lookupSelector: aMessage selector) ifNotNil: [
		"Simulated lookup succeeded -- resend the message."
		^ aMessage sentTo: self].

	"Could not recover by simulated lookup -- it's an error"
	Error signal: 'MethodDictionary fault'.

	"Try again in case an error handler fixed things"
	^ aMessage sentTo: self
]

{ #category : 'error handling' }
ProtoObject >> doesNotUnderstand: aMessage [
	"
	Note: Subclasses of ProtoObject will redefine this message.
	This implementation will only be used when the Exception based one at Object is missing.
	This may happen in minimal bootstrapped images.
	"
	| exception context count |

	"Use the MEssageNotUnderstood exception if present"
	exception := Exception deepSubclassNamed: 'MessageNotUnderstood'.
	exception notNil ifTrue:
		[ ^ exception new 
			message: aMessage;
			receiver: self;
			signal ].

	"Or do the best we can in a bare bones system"
	StdIOWriteStream stdout flush.
	StdIOWriteStream stdout newLine;
		nextPutAll: 'Message not understood.'; newLine;
		nextPutAll: 'Receiver: '.
	self printOn: StdIOWriteStream stdout.
	StdIOWriteStream stdout
		nextPutAll: ' selector: #';
		nextPutAll: aMessage selector; newLine;
		nextPutAll: '------ Call Stack: '; newLine;
		flush.

	context := thisContext sender.
	count := 0.
	[context notNil and: [ count < 10 ]] whileTrue: [
		context printOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout newLine.
		context := context sender.
		count := count + 1 ].

	StdIOWriteStream stdout newLine;
		nextPutAll: 'Will exit now.';
		newLine;
		flush.

	Smalltalk quitPrimitive: 127.
]

{ #category : 'error handling' }
ProtoObject >> error: aString  [
	"
	Note: Subclasses of ProtoObject will redefine this message.
	This implementation will only be used when the Exception based one at Object is missing.
	This may happen in minimal bootstrapped images.
	"

	| context count |
	StdIOWriteStream stdout newLine;
		nextPutAll: 'Error: ';
		nextPutAll: aString; newLine;
		nextPutAll: '------ Call Stack: '; newLine;
		flush.

	context := thisContext sender.
	count := 0.
	[context notNil and: [ count < 10 ]] whileTrue: [
		context printOn: StdIOWriteStream stdout.
		StdIOWriteStream stdout newLine.
		context := context sender.
		count := count + 1 ].

	StdIOWriteStream stdout newLine;
		nextPutAll: 'Will exit Cuis now.';
		newLine;
		flush.
	Smalltalk quitPrimitive: 128.
]

{ #category : 'debugging' }
ProtoObject >> flag: aSymbol [
	"Send this message, with a relevant symbol as argument, to flag a message for subsequent retrieval.  For example, you might put the following line in a number of messages:
	self flag: #returnHereUrgently
	Then, to retrieve all such messages, browse all senders of #returnHereUrgently."
]

{ #category : 'comparing' }
ProtoObject >> identityHash [
	"Answer a SmallInteger whose value is related to the receiver's identity.
	This method must not be overridden, except by immediate classes such as
	SmallInteger, Character and SmallFloat64.
	Primitive. Fails if the receiver is a SmallInteger. Essential.
	See Object documentation whatIsAPrimitive.

	Do not override."

	<primitive: 75>
	self primitiveFailed
]

{ #category : 'testing' }
ProtoObject >> ifNil: nilBlock [
	"Return self, or evaluate the block if I'm == nil (q.v.)"

	^ self
]

{ #category : 'testing' }
ProtoObject >> ifNil: nilBlock ifNotNil: ifNotNilBlock [
	"Evaluate the block, unless I'm == nil (q.v.)"

	^ ifNotNilBlock valueWithPossibleArgument: self
]

{ #category : 'testing' }
ProtoObject >> ifNotNil: ifNotNilBlock [
	"Evaluate the block, unless I'm == nil (q.v.)"

	^ ifNotNilBlock valueWithPossibleArgument: self
]

{ #category : 'testing' }
ProtoObject >> ifNotNil: ifNotNilBlock ifNil: nilBlock  [
	"If I got here, I am not nil, so evaluate the block ifNotNilBlock"

	^ ifNotNilBlock valueWithPossibleArgument: self
]

{ #category : 'initialization' }
ProtoObject >> initialize [
	"Subclasses should redefine this method to perform initializations on instance creation"
]

{ #category : 'testing' }
ProtoObject >> isNil [
	"Coerces nil to true and everything else to false."

	^false
]

{ #category : 'testing' }
ProtoObject >> isObject [
	^false
]

{ #category : 'system primitives' }
ProtoObject >> nextInstance [
	"Answer the next instance after the receiver in the enumeration of all instances of this class.
	Fails if all instances have been enumerated.
	See Object>>#someInstance.

	#someInstance and #nextInstance are meant to be used together starting with #someInstance,
	and following with #nextInstance until nil.

	Warning: Until all the instances are iterated and nil is answered, the VM holds a strong reference
	to the last one retrieved, preventing collecting and finalization.
	Best practice is to always iterate until nil.
	
	Warning: Spur VMs can not guarantee that every instance is visited and that it is visited only once.
		Use of this primitive is strongly discouraged."

	<primitive: 78>
	^nil
]

{ #category : 'system primitives' }
ProtoObject >> nextObject [
	"Primitive. Answer the next object after the receiver in the 
	enumeration of all objects. Return 0 when all objects have been 
	enumerated."

	<primitive: 139>
	self primitiveFailed.
]

{ #category : 'testing' }
ProtoObject >> notNil [
	"Coerces nil to false and everything else to true."

	^true
]

{ #category : 'tracing' }
ProtoObject >> pointsTo: anObject [
"Answers true if I hold a reference to anObject, or false otherwise. Or stated another way:

Answers true if the garbage collector would fail to collect anObject because I hold a reference to it, or false otherwise"

	^ (self statePointsTo: anObject)
		or: [ self class == anObject ]
]

{ #category : 'printing' }
ProtoObject >> printOn: aStream [
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver.
	"
	self class isMeta
		ifFalse: [ aStream nextPutAll: 'an instance of ' ]
		ifTrue: [ aStream nextPutAll: 'class ' ].
	self class theNonMetaClass printNameOn: aStream.
]

{ #category : 'objects from disk' }
ProtoObject >> rehash [
	"Do nothing.  Here so sending this to a Set does not have to do a time consuming respondsTo:"
]

{ #category : 'comparing' }
ProtoObject >> scaledIdentityHash [
	"For identityHash values returned by primitive 75, answer
	 such values times 2^8.  Otherwise, match the existing
	 identityHash implementation"

	^self identityHash * 256 "bitShift: 8"
]

{ #category : 'system primitives' }
ProtoObject >> statePointsTo: anObject [
	"Answers true if anObject is among my named or indexed instance variables, and false otherwise"

	<primitive: 132>
	1 to: self class instSize do: [ :i |
		(self instVarAt: i) == anObject ifTrue: [ ^ true ]].
	1 to: self basicSize do: [ :i |
		(self basicAt: i) == anObject ifTrue: [ ^ true ]].
	^ false
]

{ #category : 'apply primitives' }
ProtoObject >> tryPrimitive: primIndex withArgs: argumentArray [
	"This method is a template that the Smalltalk simulator uses to 
	execute primitives. See Object documentation whatIsAPrimitive."

	<primitive: 118 error: errorCode>
	^ContextPart primitiveFailTokenFor: errorCode
]

{ #category : 'error handling' }
ProtoObject >> warning: aString [
	"May be redefined."
	"
	7  warning: 'El totón!'
	"
	StdIOWriteStream stdout
		nextPutAll: 'Warning: ';
		nextPutAll: aString; newLine;
		flush.
]

{ #category : 'debugging' }
ProtoObject >> withArgs: argArray executeMethod: compiledMethod [
	"Execute compiledMethod against the receiver and args in argArray"

	<primitive: 188>
	self primitiveFailed
]
