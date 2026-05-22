"
My instances hold all the dynamic state associated with the execution of either a method activation resulting from a message send or a block activation resulting from a block evaluation.  In addition to their inherited state, this includes the receiver (self), the closure for a BlockClosure activation (which is nil for a method activation), a CompiledMethod, and space in the variable part of the context for arguments and temporary variables.
	
MethodContexts, though normal in their variable size, are actually only used in two sizes, small and large, which are determined by the temporary space required by the method being executed.

MethodContexts must only be created using the method newForMethod:.  Note that it is impossible to determine the real object size of a MethodContext except by asking for the frameSize of its method.  Any fields above the stack pointer (stackp) are truly invisible -- even (and especially!) to the garbage collector.  Any store into stackp other than by the primitive method stackp: is potentially fatal.
"
Class {
	#name : 'MethodContext',
	#superclass : 'ContextPart',
	#instVars : [
		'method',
		'closureOrNil',
		'receiver'
	],
	#category : 'Kernel-Methods',
	#package : 'Kernel',
	#tag : 'Methods'
}

{ #category : 'instance creation' }
MethodContext class >> sender: s receiver: r method: m arguments: args  [
	"Answer an instance of me with attributes set to the arguments."

	^(self newForMethod: m) setSender: s receiver: r method: m arguments: args
]

{ #category : 'private' }
MethodContext >> aboutToReturn: result through: firstUnwindContext  [
	"Called from VM when an unwindBlock is found between self and its home.
	 Return to home's sender, executing unwind blocks on the way."

	^firstUnwindContext unwindAndResume: self home sender evaluating: [result]
]

{ #category : 'accessing' }
MethodContext >> activeHome [
	"If executing closure, search senders for the activation of the original
	 (outermost) method that (indirectly) created my closure (the closureHome).
	 If the closureHome is not found on the sender chain answer nil."

	| methodReturnContext |
	self isExecutingBlock ifFalse: [^self].
	self sender ifNil: [^nil].
	methodReturnContext := self methodReturnContext.
	^self sender findContextSuchThat: [:ctxt | ctxt = methodReturnContext]
]

{ #category : 'accessing' }
MethodContext >> activeOuterContext [
	"If executing closure, search senders for the activation in which the receiver's
	 closure was created (the receiver's outerContext).  If the outerContext is not
	 found on the sender chain answer nil."

	| outerContext |
	self isExecutingBlock ifFalse: [^self].
	self sender ifNil: [^nil].
	outerContext := self outerContext.
	^self sender findContextSuchThat: [:ctxt | ctxt = outerContext]
]

{ #category : 'closure support' }
MethodContext >> asContext [

	^ self
]

{ #category : 'testing' }
MethodContext >> atEnd: aPC [
	^ self isExecutingBlock
		ifTrue: [ aPC == self closure endpc ]
		ifFalse: [ aPC >= self method endPC ]
]

{ #category : 'instruction decoding (closures)' }
MethodContext >> blockReturnTop [
	"Simulate the interpreter's action when a ReturnTopOfStackToCaller bytecode is 
	 encountered in the receiver.  This should only happen in a closure activation."
	self assert: closureOrNil isClosure.
	^self return: self pop from: self
]

{ #category : 'instruction decoding (closures)' }
MethodContext >> callPrimitive: primNumber [
	"Evaluate the primitive, either normal or inlined, and answer the new context resulting from that
	 (either the sender if a successful non-inlined primitive, or the current context, if not)."
	"Copied from Squeak, Context>>#callPrimitive:
	The message callInlinedPrimitive: is not implemented in Squeak also - Hernan"
	
	| maybePrimFailToken |
	primNumber >= (1 << 15) ifTrue: "Inlined primitive, cannot fail"
		[^self callInlinedPrimitive: primNumber].
	maybePrimFailToken := self doPrimitive: primNumber
								method: method
								receiver: receiver
								args: self arguments.
	"Normal primitive. Always at the beginning of methods."
	(self isPrimFailToken: maybePrimFailToken) ifFalse: "On success return the result"
		[^self methodReturnTop].
	"On failure, store the error code if appropriate and keep interpreting the method"
	(method encoderClass isStoreAt: pc in: method) ifTrue:
		[self at: stackp put: maybePrimFailToken last].
	^self
]

{ #category : 'private' }
MethodContext >> cannotReturn: result [
	closureOrNil ifNotNil: [
		^self cannotReturn: result to: sender].
	self error: 'computation has been terminated'
]

{ #category : 'accessing' }
MethodContext >> closure [
	^closureOrNil
]

{ #category : 'accessing' }
MethodContext >> contextForLocalVariables [
	"Answer the context in which local variables (temporaries) are stored."

	^self
]

{ #category : 'private' }
MethodContext >> endpc [
	^closureOrNil
		ifNil:	[self method endPC]
		ifNotNil: [closureOrNil endpc]
]

{ #category : 'system simulation' }
MethodContext >> failPrimitiveWith: maybePrimFailToken [
	"The receiver is a freshly-created context on a primitive method.  Skip the callPrimitive:
	 bytecode and store the primitive fail code if there is one and the method consumes it."
	self skipCallPrimitive.
	((self isPrimFailToken: maybePrimFailToken)
	  and: [method encoderClass isStoreAt: pc in: method]) ifTrue:
		[self at: stackp put: maybePrimFailToken last]
]

{ #category : 'accessing' }
MethodContext >> hasNonLocalReturn [
	^closureOrNil hasNonLocalReturn
]

{ #category : 'accessing' }
MethodContext >> home  [
	"Answer the context in which the receiver was defined."

	closureOrNil
		ifNil: [^ self].
	^closureOrNil outerContext home
]

{ #category : 'inspecting' }
MethodContext >> inspectorClass [
	"Answer the class of the inspector to be used on the receiver.  Called by inspect; 
	use basicInspect to get a normal (less useful) type of inspector."

	^ ContextInspector
]

{ #category : 'private' }
MethodContext >> instVarAt: index put: value [
	index = 3 ifTrue: [self stackp: value. ^ value].
	^ super instVarAt: index put: value
]

{ #category : 'accessing' }
MethodContext >> isExecutingBlock [
	"Is this executing a block versus a method?  In the new closure
	 implemetation this is true if closureOrNil is not nil, in which case
	 it should be holding a BlockClosure."

	^closureOrNil isClosure
]

{ #category : 'private-exceptions' }
MethodContext >> isHandlerOrSignalingContext [
	"Both BlockClosure>>on:do: (handler) and ContextPart>>evaluateSignal: (signaling) 
	are marked with primitive 199."

	^method primitive = 199
]

{ #category : 'private-exceptions' }
MethodContext >> isUnwindContext [
"is this context for  method that is marked?"
	^method primitive = 198
]

{ #category : 'accessing' }
MethodContext >> method [
	^method
]

{ #category : 'accessing' }
MethodContext >> methodReturnContext [
	"Answer the context from which an ^-return should return from."

	closureOrNil
		ifNil: [^ self].
	^closureOrNil outerContext methodReturnContext
]

{ #category : 'accessing' }
MethodContext >> outerContext [
	"Answer the context within which the receiver is nested."

	^ closureOrNil
		ifNotNil: [closureOrNil outerContext]
]

{ #category : 'accessing' }
MethodContext >> outerContextsDo: aBlock  [
	"Answer the context in which the receiver was defined."

	aBlock value: self.
	closureOrNil
		ifNotNil: [ closureOrNil outerContextsDo: aBlock ].
]

{ #category : 'printing' }
MethodContext >> printDetails: strm [
	"Put my class>>selector and instance variables and arguments and temporaries on the stream.  Protect against errors during printing."

	| pe str pos |
	self printOn: strm.
	strm newLine.
	strm tab; nextPutAll: 'Receiver: '.
	pe := '<<error during printing>>'.
	strm nextPutAll: ([receiver shortPrintString] ifError: [:err :rcvr | pe]).

	strm newLine; tab; nextPutAll: 'Arguments and temporary variables: '; newLine.
	str := [(self tempsAndValuesLimitedTo: 160 indent: 2) 
				padded: #right to: 1 with: $x] ifError: [:err :rcvr | pe].
	strm nextPutAll: (str allButLast).

	strm newLine; tab; nextPutAll: 'Receiver''s instance variables: '; newLine.
	pos := strm position.
	[receiver longPrintOn: strm limitedTo: 160 indent: 2] ifError: [:err :rcvr | 
				strm nextPutAll: pe].
	pos = strm position ifTrue: ["normal printString for an Array (it has no inst vars)"
		strm nextPutAll: ([receiver shortPrintString] ifError: [:err :rcvr | pe])].
	strm peekLast isLineSeparator ifFalse: [strm newLine].
]

{ #category : 'printing' }
MethodContext >> printOn: aStream [

	self outerContext
		ifNil: [super printOn: aStream]
		ifNotNil:
			[:outerContext|
			 aStream nextPutAll: '[] in '.
			 outerContext printOn: aStream]
]

{ #category : 'printing' }
MethodContext >> printString [
	"Answer an emphasized string in case of a breakpoint method"
	(self method notNil and: [ self method hasBreakpoint ])
		ifFalse: [ ^ super printString ].
	^ super printString , ' [break]' asText allBold
]

{ #category : 'initialization' }
MethodContext >> privRefresh [
	"Reinitialize the receiver so that it is in the state it was at its creation."

	closureOrNil
		ifNotNil:
			[pc := closureOrNil startpc.
			self stackp: closureOrNil numArgs + closureOrNil numCopiedValues.
			1 to: closureOrNil numCopiedValues do:
				[:i | self tempAt: closureOrNil numArgs + i put: (closureOrNil at: i)]]
		ifNil:
			[pc := method initialPC.
			self stackp: method numTemps.
			method numArgs+1 to: method numTemps do:
				[:i | self tempAt: i put: nil]]
]

{ #category : 'initialization' }
MethodContext >> privRefreshWith: aCompiledMethod  [
	"Reinitialize the receiver as though it had been for a different method. 
	 Used by a Debugger when one of the methods to which it refers is 
	 recompiled."

	(aCompiledMethod is: #CompiledMethod) ifFalse: [
		self error: 'method can only be set to aCompiledMethod'].
	method := aCompiledMethod.
	self assert: closureOrNil == nil.
	"was: receiverMap := nil."
	self privRefresh
]

{ #category : 'system simulation' }
MethodContext >> pushArgs: args "<Array>" from: sendr "<ContextPart>"  [
	"Helps simulate action of the value primitive for closures.
	 This is used by ContextPart>>runSimulated:contextAtEachStep:"

	closureOrNil
		ifNil: [self error: 'context needs a closure!']
		ifNotNil:
			["See BlockClosure>>asContextWithSender:"
			 stackp ~= (closureOrNil numArgs + closureOrNil numCopiedValues) ifTrue:
				[self error: 'stack pointer is incorrect!'].].

	1 to: closureOrNil numArgs do:
		[:i| self at: i put: (args at: i)].
	sender := sendr
]

{ #category : 'instruction decoding (closures)' }
MethodContext >> pushConsArrayWithElements: numElements  [
	| array |
	array := Array new: numElements.
	numElements to: 1 by: -1 do:
		[:i|
		array at: i put: self pop].
	self push: array
]

{ #category : 'accessing' }
MethodContext >> receiver [
	^receiver
]

{ #category : 'private-exceptions' }
MethodContext >> receiver: r [

	receiver := r
]

{ #category : 'accessing' }
MethodContext >> removeSelf [
	"Nil the receiver pointer and answer the former value."

	| tempSelf |
	tempSelf := receiver.
	receiver := nil.
	^tempSelf
]

{ #category : 'private-exceptions' }
MethodContext >> restartWithNewReceiver: obj [

	self
		swapReceiver: obj;
		restart
]

{ #category : 'private' }
MethodContext >> setSender: s receiver: r method: m arguments: args  [
	"Create the receiver's initial state."

	sender := s.
	receiver := r.
	method := m.
	closureOrNil := nil.
	pc := method initialPC.
	self stackp: method numTemps.
	1 to: args size do: [:i | self at: i put: (args at: i)]
]

{ #category : 'private' }
MethodContext >> setSender: s receiver: r method: m closure: c startpc: startpc [
	"Create the receiver's initial state."

	sender := s.
	receiver := r.
	method := m.
	closureOrNil := c.
	pc := startpc.
	stackp := 0
]

{ #category : 'private' }
MethodContext >> startpc [
	^closureOrNil
		ifNil:	[self method initialPC]
		ifNotNil: [closureOrNil startpc]
]

{ #category : 'private-exceptions' }
MethodContext >> swapReceiver: r [

	receiver := r
]

{ #category : 'accessing' }
MethodContext >> tempAt: index  [
	"Answer the value of the temporary variable whose index is the 
	 argument, index.  Primitive. Assumes receiver is indexable. Answer the
	 value of an indexable element in the receiver. Fail if the argument index
	 is not an Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default at: primitive to give latitude to the
	 VM in context management."

	<primitive: 210>
	^self at: index
]

{ #category : 'accessing' }
MethodContext >> tempAt: index put: value  [
	"Store the argument, value, as the temporary variable whose index is the 
	 argument, index.  Primitive. Assumes receiver is indexable. Answer the
	 value of an indexable element in the receiver. Fail if the argument index
	 is not an Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default at:put: primitive to give latitude to
	 the VM in context management."

	<primitive: 211>
	^self at: index put: value
]
