"
To the instruction parsing ability of InstructionStream I add the actual semantics for execution. The execution state is stored in the indexable fields of my subclasses. This includes temporary variables and a stack of values used in evaluating expressions. The actual semantics of execution can be found in my category "system simulation" and "instruction decode". These methods exactly parallel the operation of the Smalltalk machine itself.
	
The simulator is a group of my methods that do what the Smalltalk interpreter does: execute Smalltalk bytecodes. By adding code to the simulator, you may take statistics on the running of Smalltalk methods. For example,
	Transcript show: (ContextPart runSimulated: [3 factorial]) printString.
"
Class {
	#name : 'ContextPart',
	#superclass : 'InstructionStream',
	#instVars : [
		'stackp'
	],
	#classVars : [
		'PrimitiveFailToken',
		'QuickStep',
		'ValueIndex'
	],
	#category : 'Kernel-Methods',
	#package : 'Kernel',
	#tag : 'Methods'
}

{ #category : 'instance creation' }
ContextPart class >> basicNew: size [

	self error: 'Contexts must only be created with newForMethod:'
]

{ #category : 'private' }
ContextPart class >> carefullyPrint: anObject on: aStream [

	| printString |
	printString := [anObject printString]
		on: UnhandledError
		do: [:ex | ex return: 'unprintable ' , anObject class printString].
	aStream nextPutAll: printString
]

{ #category : 'special context creation' }
ContextPart class >> contextEnsure: block [
	"Create an #ensure: context that is ready to return from executing its receiver.
	 Note: push a fake return value to the ctxt stack before making the jump as
	 #jump would remove it before returning (see #jump method and comments).
	See https://lists.cuis.st/mailman/archives/cuis-dev/2023-January/006841.html"

	| ctxt chain |
	ctxt := thisContext.
	[chain := thisContext sender cut: ctxt. ctxt push: nil; jump] ensure: block.
	"jump above will resume here without unwinding chain"
	^ chain
]

{ #category : 'special context creation' }
ContextPart class >> contextOn: exceptionClass do: block [
	"Create an #on:do: context that is ready to return from executing its receiver.
	 Note: push a fake return value to the ctxt stack before making the jump as 
	 #jump would remove it before returning (see #jump and #contextEnsure:)."

	| ctxt chain |
	ctxt := thisContext.
	[chain := thisContext sender cut: ctxt. ctxt push: nil; jump] on: exceptionClass do: block.
	"jump above will resume here without unwinding chain"
	^ chain
]

{ #category : 'class initialization' }
ContextPart class >> initialize [

	PrimitiveFailToken := Object new. "A unique token."
	ValueIndex := Association allInstVarNames indexOf: 'value'.
	QuickStep := nil "The context stepping in the debugger"
]

{ #category : 'instance creation' }
ContextPart class >> initializedInstance [
	^ nil
]

{ #category : 'private' }
ContextPart class >> isContextClass [
	^true
]

{ #category : 'instance creation' }
ContextPart class >> new [

	self error: 'Contexts must only be created with newForMethod:'
]

{ #category : 'instance creation' }
ContextPart class >> new: size [

	self error: 'Contexts must only be created with newForMethod:'
]

{ #category : 'instance creation' }
ContextPart class >> newForMethod: aMethod [
	"This is the only method for creating new contexts, other than primitive cloning.
	Any other attempts, such as inherited methods like shallowCopy, should be
	avoided or must at least be rewritten to determine the proper size from the
	method being activated.  This is because asking a context its size (even basicSize!)
	will not return the real object size but only the number of fields currently
	accessible, as determined by stackp."

	^ super basicNew: aMethod frameSize
]

{ #category : 'simulation' }
ContextPart class >> primitiveFailTokenFor: errorCode [

	^{PrimitiveFailToken. errorCode}
]

{ #category : 'cached class state' }
ContextPart class >> releaseClassCachedState [

	QuickStep := nil
]

{ #category : 'examples' }
ContextPart class >> runSimulated: aBlock [
	"The simulator is a group of methods in class ContextPart which do what the 
	Smalltalk interpreter does.  They execute Smalltalk bytecodes.  By adding code 
	to the simulator, you could take statistics on the running of Smalltalk methods.
	See also trace: callStatistics: and instructionStatistics: for sample uses"

	^ thisContext sender
		runSimulated: aBlock
		contextAtEachStep: [:ignored]

	"ContextPart runSimulated: [Pen new defaultNib: 5; go: 100]"
]

{ #category : 'examples' }
ContextPart class >> tallyInstructions: aBlock [
	"This method uses the simulator to count the number of occurrences of
	each of the Smalltalk instructions executed during evaluation of aBlock.
	Results appear in order of the byteCode set."
	| tallies |
	tallies := Bag new.
	thisContext sender
		runSimulated: aBlock
		contextAtEachStep:
			[:current | tallies add: current nextByte].
	^tallies sortedElements

	"ContextPart tallyInstructions: [3.14159 printString]"
]

{ #category : 'examples' }
ContextPart class >> tallyMethods: aBlock [
	"This method uses the simulator to count the number of calls on each method
	invoked in evaluating aBlock. Results are given in order of decreasing counts."
	| prev tallies |
	tallies := Bag new.
	prev := aBlock.
	thisContext sender
		runSimulated: aBlock
		contextAtEachStep:
			[:current |
			current == prev ifFalse: [ "call or return"
				prev sender ifNotNil: [ "call only"
					tallies add: current printString].
				prev := current]].
	^tallies sortedCounts

	"ContextPart tallyMethods: [3.14159 printString]"
]

{ #category : 'special context creation' }
ContextPart class >> theReturnMethod [

	| meth |
	meth := self lookupSelector: #return:.
	meth primitive = 0 ifFalse: [^ self error: 'expected #return: to not be a primitive'].
	^ meth
]

{ #category : 'examples' }
ContextPart class >> trace: aBlock		"ContextPart trace: [3 factorial]" [
	"This method uses the simulator to print calls and returned values in the Transcript."

	Transcript clear.
	^ self trace: aBlock on: Transcript
]

{ #category : 'examples' }
ContextPart class >> trace: aBlock on: aStream		"ContextPart trace: [3 factorial]" [
	"This method uses the simulator to print calls to a file."
	| prev |
	prev := aBlock.
	^ thisContext sender
		runSimulated: aBlock
		contextAtEachStep: [ :current |
			Sensor isAnyButtonPressed ifTrue: [^ nil].
			current == prev
				ifFalse: [
					prev sender ifNil: [
						aStream space; nextPut: $^.
						self carefullyPrint: current top on: aStream].
					aStream newLine.
					(current depthBelow: aBlock) timesRepeat: [aStream space].
					self carefullyPrint: current receiver on: aStream.
					aStream space; nextPutAll: current selector.
					prev := current]]
]

{ #category : 'examples' }
ContextPart class >> trace: aBlock onFileNamed: fileName [
	"
	ContextPart trace: [3 factorial] onFileNamed: 'trace'
	"
	"This method uses the simulator to print calls to a file."

	DirectoryEntry logsDirectory // fileName forceWriteStreamDo: [ :stream |
		self trace: aBlock on: stream ].
]

{ #category : 'controlling' }
ContextPart >> activateMethod: newMethod withArgs: args receiver: rcvr [
	"Answer a MethodContext initialized with the arguments."

	^MethodContext 
		sender: self
		receiver: rcvr
		method: newMethod
		arguments: args
]

{ #category : 'private' }
ContextPart >> activateReturn: aContext value: value [
	"Activate 'aContext return: value' in place of self, so execution will return to aContext's sender"

	^MethodContext 
		sender: self
		receiver: aContext
		method: MethodContext theReturnMethod
		arguments: {value}
]

{ #category : 'debugger access' }
ContextPart >> arguments [

	| arguments |

	arguments := Array new: self selector numArgs.
	1 to: arguments size do: [ :index | arguments at: index put: (self tempAt: index)].

	^arguments.
	
	
]

{ #category : 'accessing' }
ContextPart >> at: index [
	"Primitive. Assumes receiver is indexable. Answer the value of an
	 indexable element in the receiver. Fail if the argument index is not an
	 Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 210>
	index isInteger ifTrue:
		[self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self at: index asInteger]
		ifFalse: [self errorNonIntegerIndex]
]

{ #category : 'accessing' }
ContextPart >> at: index put: value [
	"Primitive. Assumes receiver is indexable. Answer the value of an
	 indexable element in the receiver. Fail if the argument index is not
	 an Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 211>
	index isInteger ifTrue:
		[self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self at: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]
]

{ #category : 'accessing' }
ContextPart >> basicAt: index [
	"Primitive. Assumes receiver is indexable. Answer the value of an
	 indexable element in the receiver. Fail if the argument index is not an
	 Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 210>
	index isInteger ifTrue:
		[self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self at: index asInteger]
		ifFalse: [self errorNonIntegerIndex]
]

{ #category : 'accessing' }
ContextPart >> basicAt: index put: value [
	"Primitive. Assumes receiver is indexable. Answer the value of an
	 indexable element in the receiver. Fail if the argument index is not
	 an Integer or is out of bounds. Essential. See Object documentation
	 whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 211>
	index isInteger ifTrue:
		[self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self at: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]
]

{ #category : 'accessing' }
ContextPart >> basicSize [
	"Primitive. Answer the number of indexable variables in the receiver. 
	This value is the same as the largest legal subscript. Essential. Do not 
	override in any subclass. See Object documentation whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 212>
	"The number of indexable fields of fixed-length objects is 0"
	^self primitiveFailed
]

{ #category : 'query' }
ContextPart >> bottomContext [
	"Return the last context (the first context invoked) in my sender chain"

	^ self findContextSuchThat: [:c | c sender isNil]
]

{ #category : 'private-exceptions' }
ContextPart >> canHandleSignal: exception [
	"Sent to handler (on:do:) contexts only.  If my exception class (first arg) handles exception then return true, otherwise forward this message to the next handler context.  If none left, return false (see nil>>canHandleSignal:)"

	^ (self exceptionClass handles: exception)
		or: [ self nextHandlerContext canHandleSignal: exception ].
]

{ #category : 'private' }
ContextPart >> cannotReturn: result to: homeContext [
	"The receiver tried to return result to homeContext that cannot be returned from.
	 Capture the return context/pc in a BlockCannotReturn. Nil the pc to prevent repeat
	 attempts and/or invalid continuation. Answer the result of raising the exception."

	| exception previousPc |
	exception := BlockCannotReturn new.
	previousPc := pc ifNotNil: [self previousPc].
	exception
		result: result;
		deadHome: homeContext;
		finalContext: self;
		pc: previousPc.
	pc := nil.
	^exception signal
]

{ #category : 'accessing' }
ContextPart >> client [
	"Answer the client, that is, the object that sent the message that created this context."

	^sender receiver
]

{ #category : 'controlling' }
ContextPart >> closureCopy: numArgs copiedValues: anArray [
	"Distinguish a block of code from its enclosing method by 
	creating a BlockClosure for that block. The compiler inserts into all 
	methods that contain blocks the bytecodes to send the message 
	closureCopy:copiedValues:. Do not use closureCopy:copiedValues: in code that you write! Only the 
	compiler can decide to send the message closureCopy:copiedValues:. Fail if numArgs is 
	not a SmallInteger. Optional. No Lookup. See Object documentation 
	whatIsAPrimitive."

	<primitive: 200>
	^BlockClosure outerContext: self startpc: pc + 2 numArgs: numArgs copiedValues: anArray
]

{ #category : 'system simulation' }
ContextPart >> completeCallee: aContext [
	"Simulate the execution of bytecodes until a return to the receiver."
	| ctxt current ctxt1 |
	ctxt := aContext.
	[ctxt == current or: [ctxt hasSender: self]]
		whileTrue: 
			[current := ctxt.
			ctxt1 := ctxt quickStep.
			ctxt1 ifNil: [self halt].
			ctxt := ctxt1].
	^self stepToSendOrReturn
]

{ #category : 'accessing' }
ContextPart >> contextForLocalVariables [
	"Answer the context in which local variables (temporaries) are stored."

	self subclassResponsibility
]

{ #category : 'debugger access' }
ContextPart >> contextStack  [
	"Answer an Array of the contexts on the receiver's sender chain."

	^self stackOfSize: 100000
]

{ #category : 'closure support' }
ContextPart >> contextTag [
	"Context tags may be used for referring to contexts instead of contexts themselves as they can be copied and will continue to work in other processes (continuations). By default, we use the context itself to as its tag."
	^self
]

{ #category : 'query' }
ContextPart >> copyStack [

	^ self copyTo: nil
]

{ #category : 'query' }
ContextPart >> copyTo: aContext [
	"Copy self and my sender chain down to, but not including, aContext.  End of copied chain will have nil sender."

	| copy |
	self == aContext ifTrue: [ ^nil ].
	copy := self copy.
	sender ifNotNil: [ copy privSender: (sender copyTo: aContext) ].
	^copy
]

{ #category : 'private' }
ContextPart >> cut: aContext [
	"Cut aContext and its senders from my sender chain"

	| ctxt callee |
	ctxt := self.
	[ctxt == aContext] whileFalse: [
		callee := ctxt.
		ctxt := ctxt sender.
		ctxt ifNil: [aContext ifNotNil: [self error: 'aContext not a sender']].
	].
	callee privSender: nil.

]

{ #category : 'accessing' }
ContextPart >> depthBelow [

	^self depthBelow: nil
]

{ #category : 'debugger access' }
ContextPart >> depthBelow: aContext [
	"Answer how many calls between this and aContext."
	| this depth |
	this := self.
	depth := 0.
	[this == aContext or: [this == nil]]
		whileFalse:
			[this := this sender.
			depth := depth + 1].
	^depth
]

{ #category : 'instruction decoding' }
ContextPart >> directedSuperSend: selector numArgs: numArgs [
	"Simulate the action of bytecodes that send a message with selector, selector,
	 starting the message lookup in the superclass of the class on top of stack.
	 The arguments of the message are found in the next numArgs locations on
	 the stack and the receiver just below them."

	| class newReceiver arguments |
	class := self pop.
	arguments := Array new: numArgs.
	numArgs to: 1 by: -1 do: [ :i | arguments at: i put: self pop].
	newReceiver := self pop.
	QuickStep == self ifTrue:
		[QuickStep := nil.
		^self quickSend: selector to: newReceiver with: arguments lookupIn: class superclass].
	^self send: selector to: newReceiver with: arguments lookupIn: class superclass
]

{ #category : 'instruction decoding' }
ContextPart >> doDup [
	"Simulates the action of a 'duplicate top of stack' bytecode."

	self push: self top
]

{ #category : 'instruction decoding' }
ContextPart >> doNop [
	"Simulates the action of a 'No Operation' bytecode."
]

{ #category : 'instruction decoding' }
ContextPart >> doPop [
	"Simulates the action of a 'remove top of stack' bytecode."

	self pop
]

{ #category : 'private' }
ContextPart >> doPrimitive: primitiveIndex method: meth receiver: receiver args: arguments [
	"Simulate a primitive method whose index is primitiveIndex.  The simulated receiver and
	 arguments are given as arguments to this message. If successful, push result and return
	 resuming context, else ^ {errCode, PrimitiveFailToken}. Any primitive which provokes
	 execution needs to be intercepted and simulated to avoid execution running away."

	| value |
	"Test for unsimulatable side effects (that is, code that will be triggered in the image outside of the simulator range). This includes simulation guards, which are traditionally flagged using primitive 19 (a null primitive that doesn't do anything), as well as certain control primitives that might trigger code on other processes. If a side effect is detected, raise a warning to give the user/client a chance to cancel or virtualize the operation."
	"#(19 87) do: [:primitive | self systemNavigation browseAllSelect: [:m | m primitive = primitive]]"
	(primitiveIndex = 19 "simulationGuard" or: [primitiveIndex = 87 "primitiveResume"]) ifTrue:
		[[(SimulationSideEffectWarning forPrimitive: primitiveIndex)
			context: self method: meth receiver: receiver arguments: arguments;
			signalIfSkipped: [^ self]]
				ifCurtailed:
					[self push: receiver "Cheap fix of the context's internal state. Note that unwinding the receiver -- so that the next step would invoke the primitive again -- would be challenging due to to the variety of senders to this method."]].
	
	((primitiveIndex between: 201 and: 222)
	 and: [(self objectClass: receiver) includesBehavior: BlockClosure]) ifTrue:
		[(primitiveIndex = 206
		  or: [primitiveIndex = 208]) ifTrue:						"[Full]BlockClosure>>valueWithArguments:"
			[^receiver simulateValueWithArguments: arguments first caller: self].
		 ((primitiveIndex between: 201 and: 209)			 "[Full]BlockClosure>>value[:value:...]"
		  or: [primitiveIndex between: 221 and: 222]) ifTrue: "[Full]BlockClosure>>valueNoContextSwitch[:]"
			[^receiver simulateValueWithArguments: arguments caller: self]].

	primitiveIndex = 83 ifTrue: "afr 9/11/1998 19:50" "Object>>perform:[with:...]"
		[| selector |
		selector := arguments at: 1 ifAbsent:
			[^ self class primitiveFailTokenFor: #'bad argument'].
		^self send: selector to: receiver with: arguments allButFirst].
	primitiveIndex = 84 ifTrue: "afr 9/11/1998 19:50 & eem 8/18/2009 17:04" "Object>>perform:withArguments:"
		[| selector args |
		arguments size = 2 ifFalse:
			[^ self class primitiveFailTokenFor: #'bad argument'].
		selector := arguments first.
		args := arguments second.
		args isArray ifFalse:
			[^ self class primitiveFailTokenFor: #'bad argument'].
		^self send: selector to: receiver with: args].
	primitiveIndex = 100 ifTrue: "eem 8/18/2009 16:57" "Object>>perform:withArguments:inSuperclass:"
		[| rcvr selector args superclass |
		arguments size
			caseOf: {
				[3] -> [
					rcvr := receiver.
					selector := arguments first.
					args := arguments second.
					superclass := arguments third].
				[4] -> ["mirror primitive"
					rcvr := arguments first.
					selector := arguments second.
					args := arguments third.
					superclass := arguments fourth] }
			otherwise: [^ self class primitiveFailTokenFor: #'bad number of arguments'].
		args isArray ifFalse:
			[^ self class primitiveFailTokenFor: #'bad argument'].
		((self objectClass: rcvr) includesBehavior: superclass) ifFalse:
			[^ self class primitiveFailTokenFor: #'bad argument'].
		^self send: selector to: rcvr with: args lookupIn: superclass].

	"Mutex>>primitiveEnterCriticalSectionOnBehalfOf:
	 Mutex>>primitiveTestAndSetOwnershipOfCriticalSectionOnBehalfOf:"
	(primitiveIndex = 186 or: [primitiveIndex = 187]) ifTrue:
		[| effective |
		 effective := Processor activeProcess.
		 value := primitiveIndex = 186
					ifTrue: [receiver primitiveEnterCriticalSectionOnBehalfOf: effective]
					ifFalse: [receiver primitiveTestAndSetOwnershipOfCriticalSectionOnBehalfOf: effective].
		 ^(self isPrimFailToken: value)
			ifTrue: [value]
			ifFalse: [self push: value]].
	
	(primitiveIndex = 188 or: [primitiveIndex = 189]) ifTrue:
		[| n args methodArg thisReceiver |
		 primitiveIndex caseOf:
			{[188 "primitiveExecuteMethodArgsArray"] ->
				["Object>>withArgs:executeMethod:
				  CompiledMethod class>>receiver:withArguments:executeMethod:
				  VMMirror>>ifFail:object:with:executeMethod: et al"
				 ((n := arguments size) between: 2 and: 4) ifFalse:
					[^self class primitiveFailTokenFor: #'unsupported operation'].
				 ((self objectClass: (args := arguments at: n - 1)) == Array
				  and: [(self objectClass: (methodArg := arguments at: n)) includesBehavior: CompiledMethod]) ifFalse:
					[^self class primitiveFailTokenFor: #'bad argument'].
				 thisReceiver := arguments at: n - 2 ifAbsent: [receiver]].
			 [189 "primitiveExecuteMethod"] ->
				["Object>>executeMethod:
				  Object>>with:...executeMethod:"
				 (arguments size > 0) ifFalse:
					[^self class primitiveFailTokenFor: #'bad argument'].
				 ((self objectClass: (methodArg := arguments atLast: 1)) includesBehavior: CompiledMethod) ifFalse:
					[^self class primitiveFailTokenFor: #'bad argument'].
				 args := arguments allButLast.
				 thisReceiver := receiver]}.
		 methodArg numArgs = args size ifFalse:
			[^self class primitiveFailTokenFor: #'bad number of arguments'].
		 methodArg primitive > 0 ifTrue:
			[methodArg isQuick ifTrue:
				[^self push: (methodArg valueWithReceiver: thisReceiver arguments: args)].
			 ^self doPrimitive: methodArg primitive method: methodArg receiver: thisReceiver args: args].
		 ^self
			activateMethod: methodArg
			withArgs: args
			receiver: thisReceiver].

	primitiveIndex = 118 ifTrue: "[receiver:]tryPrimitive:withArgs:; avoid recursing in the VM"
		[(arguments size = 3
		  and: [(self objectClass: arguments second) == SmallInteger
		  and: [(self objectClass: arguments last) == Array]]) ifTrue:
			[^self doPrimitive: arguments second method: meth receiver: arguments first args: arguments last].
		 (arguments size = 2
		 and: [(self objectClass: arguments first) == SmallInteger
		 and: [(self objectClass: arguments last) == Array]]) ifFalse:
			[^self class primitiveFailTokenFor: -3].
		 ^self doPrimitive: arguments first method: meth receiver: receiver args: arguments last].

	value := primitiveIndex = 120 "FFI method"
				ifTrue: [(meth literalAt: 1) tryInvokeWithArguments: arguments]
				ifFalse:
					[primitiveIndex = 117 "named primitives"
						ifTrue: [
							"primDecl := meth literalAt: 1.
							moduleNameOrNil := primDecl at: 1.
							primitiveName := primDecl at: 2."
							(#(VectorEnginePlugin) statePointsTo: ((meth literalAt: 1) at: 1))
								ifTrue: [self class primitiveFailTokenFor: 1]
								ifFalse: [self tryNamedPrimitiveIn: meth for: receiver withArgs: arguments].
							]
						ifFalse: "should use self receiver: receiver tryPrimitive: primitiveIndex withArgs: arguments but this is only in later VMs (and appears to be broken)"
							[receiver tryPrimitive: primitiveIndex withArgs: arguments]].

	^(self isPrimFailToken: value)
		ifTrue: [value]
		ifFalse: [self push: value]
]

{ #category : 'debugger access' }
ContextPart >> errorReportOn: strm [
	"Write a detailed error report on the stack (above me) on a stream.  For both the error file, and emailing a bug report.  Suppress any errors while getting printStrings.  Limit the length."

	| cnt aContext startPos |
 	strm print: Date today; space; print: Time now; newLine.
	strm newLine.
	strm nextPutAll: 'VM: ';
		nextPutAll: Smalltalk platformName;
		nextPutAll: ' - ';
		nextPutAll: Smalltalk vmVersion;
		newLine.
	strm nextPutAll: 'Image: ';
		nextPutAll: Smalltalk versionString;
		nextPutAll: ' [';
		nextPutAll: Smalltalk lastUpdateString;
		nextPutAll: ']';
		newLine.
	strm newLine.
	
	"Note: The following is an open-coded version of ContextPart>>stackOfSize: since this method may be called during a low space condition and we might run out of space for allocating the full stack."
	cnt := 0.  startPos := strm position.
	aContext := self.
	[aContext notNil and: [(cnt := cnt + 1) < Debugger defaultDebugStackSize]] whileTrue: [
		aContext printDetails: strm.	"variable values"
		strm newLine.
		aContext := aContext sender].

	strm newLine; nextPutAll: '--- The full stack ---'; newLine.
	aContext := self.
	cnt := 0.
	[aContext == nil] whileFalse: [
		cnt := cnt + 1.
		cnt = Debugger defaultDebugStackSize ifTrue: [strm nextPutAll: ' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'; newLine ].
		strm print: aContext; newLine.  "just class>>selector"	

		strm position > (startPos+40000) ifTrue: [strm nextPutAll: '...etc...'.
			^ self]. 	"exit early"
		cnt > 100 ifTrue: [strm nextPutAll: '-- and more not shown --'.  ^ self].
		aContext := aContext sender].

]

{ #category : 'private-exceptions' }
ContextPart >> evaluateSignal: exception [
	"The following primitive is just a marker used to find the evaluation context. 
	See MethodContext>>#isHandlerOrSignalingContext. "

	<primitive: 199>
	| value |
	exception pushHandlerContext: self contextTag.
	value := exception evaluateHandlerBlock: self exceptionHandlerBlock.
	"return from self if not otherwise directed in handle block"
	self return: value
]

{ #category : 'private-exceptions' }
ContextPart >> exceptionClass [

	^self tempAt: 1
]

{ #category : 'private-exceptions' }
ContextPart >> exceptionHandlerBlock [
	"handler context only. access temporaries from BlockClosure>>#on:do:"

	^self tempAt: 2
]

{ #category : 'controlling' }
ContextPart >> executeMethod: meth forSelector: selector withArgs: arguments receiver: rcvr [

	| primIndex val ctxt isPrimitive |
	(self objectClass: meth) isCompiledMethodClass ifFalse:
		["Object as Methods (OaM) protocol: 'The contract is that, when the VM encounters an ordinary object (rather than a compiled method) in the method dictionary during lookup, it sends it the special selector #run:with:in: providing the original selector, arguments, and receiver.'. DOI: 10.1145/2991041.2991062."
		^self send: #run:with:in:
			to: meth
			with: {selector. arguments. rcvr}].
	
	meth numArgs = arguments size ifFalse:
		[^ self class primitiveFailTokenFor: #'bad number of arguments'].
	primIndex := meth primitive.
	isPrimitive := primIndex > 0 and: [ meth isQuick not ].
	isPrimitive ifTrue:
		[val := self doPrimitive: primIndex method: meth receiver: rcvr args: arguments.
		(self isPrimFailToken: val) ifFalse:
			[^val]].
	
	ctxt := self activateMethod: meth withArgs: arguments receiver: rcvr.
	isPrimitive ifTrue:
		[ctxt failPrimitiveWith: val].
	
	^ctxt
]

{ #category : 'query' }
ContextPart >> findContextSuchThat: testBlock [
	"Search self and my sender chain for first one that satisfies testBlock.  Return nil if none satisfy"

	| ctxt |
	ctxt := self.
	[ctxt isNil] whileFalse: [
		(testBlock value: ctxt) ifTrue: [^ ctxt].
		ctxt := ctxt sender.
	].
	^ nil
]

{ #category : 'private-exceptions' }
ContextPart >> findNextHandlerContext [
	"Return the next handler marked context, returning nil if there is none.  Search starts
	with self and proceeds up to nil.  If context isn't a handler context, it must be a signaling
	context.  When we reach a signaling context we must skip over any handlers that might
	be on the stack between the signaling context and the handler context for that signal."

	| context searchStartContext |
	context := self findNextHandlerOrSignalingContext.
	context isNil ifTrue: [^nil].
	context isHandlerContext ifTrue: [^context]. 
	searchStartContext := context exceptionClass topHandlerContext ifNil: [context].
	^searchStartContext nextHandlerContext
]

{ #category : 'private-exceptions' }
ContextPart >> findNextHandlerOrSignalingContext [
	"Return the next handler/signaling marked context, answering nil if there is none. 
	Search starts with self and proceeds up to nil."

	<primitive: 197>
	| context |
	context := self.
	[ 
	context isHandlerOrSignalingContext
		ifTrue: [ ^ context ].
	(context := context sender) == nil ] whileFalse.
	^ nil
]

{ #category : 'private-exceptions' }
ContextPart >> findNextUnwindContextUpTo: aContext [
	"Return the next unwind marked above the receiver, returning nil if there is none.  Search proceeds up to but not including aContext."

	| ctx |
	<primitive: 195>
	ctx := self.
		[(ctx := ctx sender) == nil or: [ctx == aContext]] whileFalse:
		[ ctx isUnwindContext ifTrue: [^ctx]].
	^nil
]

{ #category : 'query' }
ContextPart >> findSecondToOldestSimilarSender [
	"Search the stack for the second-to-oldest occurance of self's method.  Very useful for an infinite recursion.  Gets back to the second call so you can see one complete recursion cycle, and how it was called at the beginning."

	| sec ctxt bot |
	sec := self.
	ctxt := self.
	[	bot := ctxt findSimilarSender.
		bot isNil
	] whileFalse: [
		sec := ctxt.
		ctxt := bot.
	].
	^ sec

]

{ #category : 'query' }
ContextPart >> findSimilarSender [
	"Return the closest sender with the same method, return nil if none found"

	| meth |
	meth := self method.
	^ self sender findContextSuchThat: [:c | c method == meth]
]

{ #category : 'private' }
ContextPart >> finishIncompleteUnwindsUpTo: aContext [
	"A helper method to Process #unwindTo:safely:. Find out if there are unfinished
	 unwind blocks between self and aContext and complete the outer-most one;
	 all nested unwind blocks will be completed in the process.
	 Answer self or the outer-most unfinished context's sender."
	
	| ctx outerMost |
	ctx := self.
	ctx isUnwindContext ifFalse: [ctx := ctx findNextUnwindContextUpTo: aContext]. "Note 1"
	[ctx isNil] whileFalse: [  "ctx is an #ensure: or #ifCurtailed: context"
		(ctx tempAt: 2) == #unwindRunnable ifTrue: [outerMost := ctx]. "Note 2"
		ctx := ctx findNextUnwindContextUpTo: aContext].
	outerMost ifNotNil: [^self runUntilReturnFrom: outerMost]

"Note 1: #findNextUnwindContextUpTo: starts searching from the receiver's sender hence we must check the receiver whether it is an unwind context; see testTerminateEnsureAsStackTop.

Note 2: Halfway-through unwind blocks have already set their completionStatus variable (ctx tempAt: 2) in their defining #ensure:/#ifCurtailed contexts to #unwindRunnable which means the unwind block execution has started but not finished yet; we'll search for the outer-most one and finish its execution."
]

{ #category : 'private-exceptions' }
ContextPart >> handleSignal: exception [
	"Sent to handler (on:do:) contexts only.  If my exception class (first arg) handles exception then execute my handle block (second arg), otherwise forward this message to the next handler context.  If none left, execute exception's defaultAction (see nil>>handleSignal:)."

	(self exceptionClass handles: exception)
		ifFalse: [ ^ self nextHandlerContext handleSignal: exception ].
	self evaluateSignal: exception
]

{ #category : 'query' }
ContextPart >> hasContext: aContext  [
	"Answer whether aContext is me or one of my senders"

	^ (self findContextSuchThat: [:c | c == aContext]) notNil
]

{ #category : 'debugger access' }
ContextPart >> hasSender: context  [
	"Answer true if the receiver is strictly above context on the stack."

	| s |
	self == context ifTrue: [^false].
	s := sender.
	[s == nil]
		whileFalse: 
			[s == context ifTrue: [^true].
			s := s sender].
	^false
]

{ #category : 'accessing' }
ContextPart >> home [
	"Answer the context in which the receiver was defined."

	self subclassResponsibility
]

{ #category : 'private' }
ContextPart >> insertSender: aContext [
	"Insert aContext and its sender chain between me and my sender.  Return new callee of my original sender."

	| ctxt |
	ctxt := aContext bottomContext.
	ctxt privSender: self sender.
	self privSender: aContext.
	^ ctxt
]

{ #category : 'query' }
ContextPart >> isBottomContext [
	"Answer if this is the last context (the first context invoked) in my sender chain"

	^sender isNil
]

{ #category : 'query' }
ContextPart >> isContext [
	^true
]

{ #category : 'query' }
ContextPart >> isDead [
	"Has self finished"

	^ pc isNil
]

{ #category : 'query' }
ContextPart >> isDeadOrAtEnd [
	"Doing
	aMethodContext isDead or: [aMethodContext isAtEnd]
	can occasionally break. It is possible for pc to become nil in between the calls."

	^ pc
		ifNil: [ true ]
		ifNotNil: [ :aPC | self atEnd: pc ]
]

{ #category : 'query' }
ContextPart >> isDone [
	"Answer if the receiver has no unexecuted code left.
	Written carefully to avoid suspension points.
	See https://lists.cuis.st/mailman/archives/cuis-dev/2023-January/006814.html
	See https://lists.cuis.st/mailman/archives/cuis-dev/2023-January/006818.html"

	^pc
		ifNil: [ true ]
		ifNotNil: [ :aPC | (self atEnd: aPC) and: [
			sender
				ifNil: [ true ]
				ifNotNil: [ :ctx | ctx isDone]]]
]

{ #category : 'private-exceptions' }
ContextPart >> isHandlerContext [
	"is this context for #on:do:?"
	^self isHandlerOrSignalingContext and: [ self selector == #on:do: ]
]

{ #category : 'private-exceptions' }
ContextPart >> isHandlerOrSignalingContext [
	"Both BlockClosure>>on:do: (handler) and ContextPart>>evaluateSignal: (signaling) 
	are marked with primitive 199."

	^false
]

{ #category : 'private' }
ContextPart >> isPrimFailToken: anObject [
	^(self objectClass: anObject) == Array
	  and: [anObject size = 2
	  and: [anObject first == PrimitiveFailToken]]
]

{ #category : 'private-exceptions' }
ContextPart >> isUnwindContext [

	^false
]

{ #category : 'controlling' }
ContextPart >> jump [
	"Abandon thisContext and resume self instead (using the same current process).  You may want to save thisContext's sender before calling this so you can jump back to it.
	Self MUST BE a top context (ie. a suspended context or a abandoned context that was jumped out of).  A top context already has its return value on its stack (see Interpreter>>primitiveSuspend and other suspending primitives).
	thisContext's sender is converted to a top context (by pushing a nil return value on its stack) so it can be jump back to."

	| top |
	"Make abandoned context a top context (has return value (nil)) so it can be jumped back to"
	thisContext sender push: nil.

	"Pop self return value then return it to self (since we jump to self by returning to it)"
	stackp = 0 ifTrue: [self stepToSendOrReturn].
	stackp = 0 ifTrue: [self push: nil].  "must be quick return self/constant"
	top := self pop.
	thisContext privSender: self.
	^ top
]

{ #category : 'instruction decoding' }
ContextPart >> jump: distance  [
	"Simulates the action of a 'unconditional jump' bytecode whose
	offset is the argument, distance."

	pc := pc + distance
]

{ #category : 'instruction decoding' }
ContextPart >> jump: distance if: condition  [
	"Simulate the action of a 'conditional jump' bytecode whose offset is the 
	 argument, distance, and whose condition is the argument, condition."

	| bool |
	bool := self pop.
	condition == bool
		ifTrue: [self jump: distance]
		ifFalse:
			[(true == bool or: [false == bool]) ifFalse:
				[^self send: #mustBeBoolean to: bool with: #()]]
]

{ #category : 'debugger access' }
ContextPart >> messageForYourself [

	^Message selector: self selector arguments: self arguments.
	
]

{ #category : 'accessing' }
ContextPart >> method [
	"Answer the method of this context."

	self subclassResponsibility
]

{ #category : 'debugger access' }
ContextPart >> methodClass  [
	"Answer the class in which the receiver's method was found."
	
	^self method methodClass ifNil: [self objectClass: self receiver].
]

{ #category : 'accessing' }
ContextPart >> methodNode [
	^ self method methodNode.
]

{ #category : 'instruction decoding' }
ContextPart >> methodReturnConstant: value [
	"Simulate the action of a 'return constant' bytecode whose value is the
	 argument, value. This corresponds to a source expression like '^0'."

	^self return: value from: self methodReturnContext
]

{ #category : 'accessing' }
ContextPart >> methodReturnContext [
	"Answer the context from which an ^-return should return from."

	self subclassResponsibility
]

{ #category : 'instruction decoding' }
ContextPart >> methodReturnReceiver [
	"Simulate the action of a 'return receiver' bytecode. This corresponds to
	 the source expression '^self'."

	^self return: self receiver from: self methodReturnContext
]

{ #category : 'instruction decoding implicit literals' }
ContextPart >> methodReturnSpecialConstant: value [
	"Simulate the action of a 'return constant' bytecode whose value is the
	 argument, value. This corresponds to a source expression like '^0'."

	^self return: value from: self methodReturnContext
]

{ #category : 'instruction decoding' }
ContextPart >> methodReturnTop [
	"Simulate the action of a 'return top of stack' bytecode. This corresponds
	 to source expressions like '^something'."

	^self return: self pop from: self methodReturnContext
]

{ #category : 'read-only objects' }
ContextPart >> modificationForbiddenFor: target at: index put: anObject [
	"This is a place-holder before proper read-only object support is implemented."
	^self error: 'no modification'
]

{ #category : 'read-only objects' }
ContextPart >> modificationForbiddenFor: target instVarAt: index put: anObject [
	"This is a place-holder before proper read-only object support is implemented."
	^self error: 'no modification'
]

{ #category : 'debugger access' }
ContextPart >> namedTempAt: index [
	"Answer the value of the temp at index in the receiver's sequence of tempNames."
	^self debuggerMap namedTempAt: index in: self
]

{ #category : 'debugger access' }
ContextPart >> namedTempAt: index put: aValue [
	"Set the value of the temp at index in the receiver's sequence of tempNames.
	 (Note that if the value is a copied value it is also set out along the lexical chain,
	  but alas not in along the lexical chain.)."
	^self debuggerMap namedTempAt: index put: aValue in: self
]

{ #category : 'private-exceptions' }
ContextPart >> nextHandlerContext [

	^ self sender ifNotNil: [ :sndr | sndr findNextHandlerContext ]
]

{ #category : 'mirror primitives' }
ContextPart >> object: anObject basicAt: index  [
	"Answer the value of an indexable element in the argument anObject without sending
	 it a message. Fail if the argument index is not an Integer or is out of bounds, or if
	 anObject is not indexable. This mimics the action of the VM when it indexes an object.
	 Used to simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 60>
	index isInteger ifTrue: [self errorSubscriptBounds: index].
	index isNumber
		ifTrue: [^self object: anObject basicAt: index asInteger]
		ifFalse: [self errorNonIntegerIndex]
]

{ #category : 'mirror primitives' }
ContextPart >> object: anObject basicAt: index put: value  [
	"Store the last argument 
	 value in the indexable element of the argument anObject indicated by index without sending
	 anObject a message. Fail if the argument index is not an Integer or is out of bounds, or if
	 anObject is not indexable, or if value is an inappropriate value for anObject's indexable slots.
	 This mimics the action of the VM when it indexes an object.
	 Used to simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 61>
	index isInteger
		ifTrue: [(index >= 1 and: [index <= (self objectSize: anObject)])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]].
	index isNumber
		ifTrue: [^self object: anObject basicAt: index asInteger put: value]
		ifFalse: [self errorNonIntegerIndex]
]

{ #category : 'mirror primitives' }
ContextPart >> object: anObject eqeq: anOtherObject  [
	"Answer whether the first and second arguments are the same object (have the
	 same object pointer) without sending a message to the first argument.  This
	 mimics the action of the VM when it compares two object pointers.  Used to
	 simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 110>
	self primitiveFailed
]

{ #category : 'mirror primitives' }
ContextPart >> object: anObject instVarAt: anIndex [
	"Primitive. Answer a fixed variable in an object. The numbering of the 
	 variables corresponds to the named instance variables. Fail if the index 
	 is not an Integer or is not the index of a fixed variable. Essential for the
	 debugger. See  Object documentation whatIsAPrimitive."

	<primitive: 173 error: ec>
	self primitiveFailed
]

{ #category : 'mirror primitives' }
ContextPart >> object: anObject instVarAt: anIndex put: aValue  [
	"Primitive. Store a value into a fixed variable in the argument anObject.
	 The numbering of the variables corresponds to the named instance
	 variables.  Fail if the index is not an Integer or is not the index of a
	 fixed variable, or if anObject is read-only.  Answer the value stored as
	 the result. Using this message violates the  principle that each object
	 has sovereign control over the storing of values into its instance variables.
	 Essential for the debugger. See Object documentation whatIsAPrimitive."

	<primitive: 174 error: ec>
	ec == #'no modification' ifTrue:
		[^self modificationForbiddenFor: anObject instVarAt: anIndex put: aValue].
	self primitiveFailed
]

{ #category : 'mirror primitives' }
ContextPart >> object: anObject perform: selector withArguments: argArray inClass: lookupClass [
	"Send the selector, aSymbol, to anObject with arguments in argArray.
	 Fail if the number of arguments expected by the selector 
	 does not match the size of argArray, or if lookupClass
	 cannot be found among the anObject's superclasses.
	 Primitive. Essential for the debugger."

	<primitive: 100 error: error>
	(selector isSymbol) ifFalse:
		[^self error: 'selector argument must be a Symbol'].
	(argArray isMemberOf: Array) ifFalse:
		[^self error: 'argArray must be an Array'].
	(selector numArgs = argArray size)
		ifFalse: [^self error: 'incorrect number of arguments'].
	((self objectClass: anObject) == lookupClass
	 or: [(self objectClass: anObject) inheritsFrom: lookupClass]) ifFalse:
		[^self error: 'lookupClass is not in anObject''s inheritance chain'].
	self primitiveFailed
]

{ #category : 'mirror primitives' }
ContextPart >> objectClass: anObject [

	<primitive: 111>
	self primitiveFailed
]

{ #category : 'mirror primitives' }
ContextPart >> objectSize: anObject [
	"Answer the number of indexable variables in the argument anObject without sending
	 it a message. This mimics the action of the VM when it fetches an object's variable size.
	 Used to simulate the execution machinery by, for example, the debugger.
	 Primitive.  See Object documentation whatIsAPrimitive."

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0
]

{ #category : 'controlling' }
ContextPart >> pop [
	"Answer the top of the receiver's stack and remove the top of the stack."
	| val |
	val := self at: stackp.
	self stackp: stackp - 1.
	^ val
]

{ #category : 'instruction decoding' }
ContextPart >> popIntoLiteralVariable: value  [
	"Simulate the action of bytecode that removes the top of the stack and 
	 stores it into a literal variable of my method. If the receiver is read-only
	 this will provoke a send of #attemptToAssign:withIndex:"

	| top result |
	top := self pop.
	result := self simulatedObject: value instVarAt: ValueIndex put: top.
	^(self object: result eqeq: top)
		ifTrue: [self]
		ifFalse: [result]
]

{ #category : 'instruction decoding' }
ContextPart >> popIntoReceiverVariable: offset  [
	"Simulate the action of bytecode that removes the top of the stack and 
	 stores it into an instance variable of my receiver. If the receiver is read-only
	 this will provoke a send of #attemptToAssign:withIndex:"

	| top result |
	top := self pop.
	result := self simulatedObject: self receiver instVarAt: offset + 1 put: top.
	^(self object: result eqeq: top)
		ifTrue: [self]
		ifFalse: [result]
]

{ #category : 'instruction decoding' }
ContextPart >> popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex [
	"Simulate the action of bytecode that removes the top of the stack and  stores
	 it into an offset in one of my local variables being used as a remote temp vector."

	self object: (self at: tempVectorIndex + 1) basicAt: remoteTempIndex + 1 put: self pop
]

{ #category : 'instruction decoding' }
ContextPart >> popIntoTemporaryVariable: offset  [
	"Simulate the action of bytecode that removes the top of the stack and 
	stores it into one of my temporary variables."

	self at: offset + 1 put: self pop
]

{ #category : 'debugger access' }
ContextPart >> print: anObject on: aStream [
	"Safely print anObject in the face of direct ProtoObject subclasses."
	| objClass |
	objClass := self objectClass: anObject.
	(objClass canUnderstand: #printOn:) ifTrue: [
		^anObject printOn: aStream].
	aStream nextPutAll: objClass name withArticle
]

{ #category : 'printing' }
ContextPart >> printDetails: strm [
	"Put my class>>selector and arguments and temporaries on the stream.  Protect against errors during printing."

	| str |
	self printOn: strm.  

	strm newLine; tab; nextPutAll: 'Arguments and temporary variables: '; newLine.
	str := [self tempsAndValuesLimitedTo: 160 indent: 2] ifError: [:err :rcvr | 
						'<<error during printing>>'].
	strm nextPutAll: str.
	strm peekLast isLineSeparator ifFalse: [strm newLine]
]

{ #category : 'printing' }
ContextPart >> printOn: aStream  [
	| selector class mclass |
	self method
		ifNil: [^ super printOn: aStream].
	class := self receiver class.
	mclass := self methodClass.
	selector := self selector ifNil: [ self method defaultSelector].
	class printNameOn: aStream.
	mclass == class 
		ifFalse: [
			aStream nextPut: $(.
			mclass printNameOn: aStream.
			aStream nextPut: $)].
	aStream nextPutAll: '>>'.
	aStream nextPutAll: selector.
	selector == #doesNotUnderstand: ifTrue: [
		aStream space.
		(self tempAt: 1) selector printOn: aStream.
	].

]

{ #category : 'printing' }
ContextPart >> printStack: depth [
	self print.
	depth > 0 ifTrue: [
		self sender ifNotNil: [ :s | s printStack: depth-1 ]]
]

{ #category : 'private' }
ContextPart >> privSender: aContext  [

	sender := aContext
]

{ #category : 'controlling' }
ContextPart >> push: val  [
	"Push val on the receiver's stack."

	self stackp: stackp + 1.
	self at: stackp put: val
]

{ #category : 'private' }
ContextPart >> push: numObjects fromIndexable: anIndexableCollection [
	"Push the elements of anIndexableCollection onto the receiver's stack.
	 Do not call directly.  Called indirectly by {1. 2. 3} constructs."

	1 to: numObjects do:
		[:i | self push: (anIndexableCollection at: i)]
]

{ #category : 'instruction decoding' }
ContextPart >> pushActiveContext [
	"Simulates the action of bytecode that pushes the the active
	context on the top of its own stack."

	self push: self
]

{ #category : 'instruction decoding' }
ContextPart >> pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize [
	"Simulate the action of a 'closure copy' bytecode whose result is the
	 new BlockClosure for the following code"
	| copiedValues |
	numCopied > 0
		ifTrue:
			[copiedValues := Array new: numCopied.
			 numCopied to: 1 by: -1 do:
				[:i|
				copiedValues at: i put: self pop]]
		ifFalse:
			[copiedValues := nil].
	self push: (BlockClosure
				outerContext: self
				startpc: pc
				numArgs: numArgs
				copiedValues: copiedValues).
	self jump: blockSize
]

{ #category : 'instruction decoding' }
ContextPart >> pushConstant: value  [
	"Simulates the action of bytecode that pushes the constant, value, on
	the top of the stack."

	self push: value
]

{ #category : 'instruction decoding' }
ContextPart >> pushLiteralVariable: value  [
	"Simulate the action of bytecode that pushes the contents of the literal 
	variable whose index is the argument, index, on the top of the stack."

	self push: (self object: value instVarAt: ValueIndex)
]

{ #category : 'instruction decoding' }
ContextPart >> pushNewArrayOfSize: arraySize  [
	self push: (Array new: arraySize)
]

{ #category : 'instruction decoding' }
ContextPart >> pushReceiver [
	"Simulates the action of bytecode that pushes the the active
	context's receiver on the top of the stack."

	self push: self receiver
]

{ #category : 'instruction decoding' }
ContextPart >> pushReceiverVariable: offset  [
	"Simulate the action of bytecode that pushes the contents of the receiver's 
	instance variable whose index is the argument, index, on the top of the 
	stack."

	self push: (self object: self receiver instVarAt: offset + 1)
]

{ #category : 'instruction decoding' }
ContextPart >> pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex [
	"Simulate the action of bytecode that pushes the value at remoteTempIndex
	 in one of my local variables being used as a remote temp vector."
	self push: (self object: (self at: tempVectorIndex + 1) basicAt: remoteTempIndex + 1)
]

{ #category : 'instruction decoding implicit literals' }
ContextPart >> pushSpecialConstant: value  [
	"Simulate the action of bytecode that pushes the constant, value, on the 
	top of the stack."

	self push: value
]

{ #category : 'instruction decoding' }
ContextPart >> pushTemporaryVariable: offset  [
	"Simulate the action of bytecode that pushes the contents of the 
	temporary variable whose index is the argument, index, on the top of 
	the stack."

	self push: (self contextForLocalVariables at: offset + 1)
]

{ #category : 'controlling' }
ContextPart >> quickSend: selector to: rcvr with: arguments lookupIn: lookupClass [
	"Send the given selector with arguments in an environment which closely resembles
	 the non-simulating environment, with an interjected unwind-protected block to catch
	 nonlocal returns.  Attention: don't get lost!  This beautiful method is originally due to
	 Hans-Martin Mosner."
	| oldSender contextToReturnTo result |
	contextToReturnTo := self.
	[oldSender := thisContext sender swapSender: self.
	 result := self object: rcvr perform: selector withArguments: arguments inClass: lookupClass.
	 thisContext sender swapSender: oldSender] ifCurtailed:
		[contextToReturnTo := thisContext sender receiver.	"The block context returning nonlocally"
		contextToReturnTo pc: contextToReturnTo previousPc.	"skip to front of return bytecode causing this unwind"
		contextToReturnTo willReturnTopFromMethod ifTrue:
			"If it was a returnTop, push the value to be returned.
			Otherwise the value is implicit in the bytecode"
			[contextToReturnTo push: (thisContext sender tempAt: 1)].
		thisContext swapSender: thisContext home sender.	"Make this block return to the method's sender"
		contextToReturnTo].
	contextToReturnTo push: result.
	^contextToReturnTo
]

{ #category : 'system simulation' }
ContextPart >> quickStep [
	"If the next instruction is a send, just perform it.
	Otherwise, do a normal step."

	self willSend ifTrue: [QuickStep := self].
	^self step
]

{ #category : 'accessing' }
ContextPart >> receiver [
	"Answer the receiver of the message that created this context."

	self subclassResponsibility
]

{ #category : 'debugger access' }
ContextPart >> releaseTo: caller  [
	"Remove information from the receiver and the contexts on its 
	sender chain up to caller in order to break circularities."

	| c s |
	c := self.
	[c == nil or: [c == caller]]
		whileFalse: 
			[s := c sender.
			c singleRelease.
			c := s]
]

{ #category : 'controlling' }
ContextPart >> restart [
	"Unwind thisContext to self and resume from beginning. 
	 Execute any unwind blocks while unwinding.
	 ASSUMES self is a sender of thisContext."

	^thisContext unwindAndResume: self evaluating: [self privRefresh]
]

{ #category : 'controlling' }
ContextPart >> resume [
	"Roll back thisContext to self and resume.  Execute unwind blocks when rolling back.  ASSUMES self is a sender of thisContext"

	self resume: nil
]

{ #category : 'controlling' }
ContextPart >> resume: value [
	"Unwind thisContext to self and resume with value as result of last send.
	 Execute any unwind blocks while unwinding.
	 ASSUMES self is a sender of thisContext."

	^thisContext unwindAndResume: self evaluating: [value]
]

{ #category : 'controlling' }
ContextPart >> resumeEvaluating: aBlock [
	"Unwind thisContext to self and resume with aBlock value as result of last send. 
	 Execute any unwind blocks while unwinding.
	 ASSUMES self is a sender of thisContext."

	^thisContext unwindAndResume: self evaluating: aBlock
]

{ #category : 'controlling' }
ContextPart >> return [
	"Unwind until my sender is on top"

	self return: self receiver
]

{ #category : 'controlling' }
ContextPart >> return: value [
	"Unwind thisContext to self's sender and resume with value as result of last send.
	 Execute any unwind blocks while unwinding.
	 ASSUMES self's sender is a sender of thisContext."

	^thisContext unwindAndResume: sender evaluating: [value]
]

{ #category : 'instruction decoding' }
ContextPart >> return: value from: aSender  [
	"For simulation.  Roll back self to aSender and return value from it.  Execute any unwind blocks on the way.  ASSUMES aSender is a sender of self"

	| newTop |
	newTop := aSender sender.
	(aSender isDead or: [newTop isNil or: [newTop isDead]]) ifTrue:
		[^self pc: nil; send: #cannotReturn: to: self with: {value}].
	(self findNextUnwindContextUpTo: newTop) ifNotNil: 
		[:unwindProtectCtxt|
		^self send: #aboutToReturn:through: to: self with: {value. unwindProtectCtxt}].
	self releaseTo: newTop.
	newTop ifNotNil: [newTop push: value].
	^newTop
]

{ #category : 'controlling' }
ContextPart >> return: value to: sendr  [
	"Simulate the return of value to sendr."

	self releaseTo: sendr.
	sendr ifNil: [^ nil].
	^ sendr push: value
]

{ #category : 'controlling' }
ContextPart >> returnEvaluating: aBlock [
	"Unwind thisContext to self's sender and resume with aBlock value as result of last send'.
	 Execute any unwind blocks while unwinding.
	 ASSUMES self's sender is a sender of thisContext."

	^thisContext unwindAndResume: sender evaluating: aBlock
]

{ #category : 'system simulation' }
ContextPart >> runSimulated: aBlock contextAtEachStep: block2 [
	"Simulate the execution of the argument, aBlock, until it ends. aBlock 
	MUST NOT contain an '^'. Evaluate block2 with the current context 
	prior to each instruction executed. Answer the simulated value of aBlock."
	| current |
	aBlock hasNonLocalReturn
		ifTrue: [self error: 'simulation of blocks with ^ can run loose'].
	current := aBlock asContext.
	current pushArgs: #() from: self.
	[current == self]
		whileFalse:
			[block2 value: current.
			current := current step].
	^self pop
]

{ #category : 'controlling' }
ContextPart >> runUntilErrorOrReturnFrom: aSenderContext [
	"ASSUMES aSenderContext is a sender of self.  Execute self's stack until aSenderContext returns or an unhandled exception is raised.  Return a pair containing the new top context and a possibly nil exception.  The exception is not nil if it was raised before aSenderContext returned and it was not handled.  The exception is returned rather than openning the debugger, giving the caller the choice of how to handle it."
	"Self is run by jumping directly to it (the active process abandons thisContext and executes self).  However, before jumping to self we insert an ensure block under aSenderContext that jumps back to thisContext when evaluated.  We also insert an exception handler under aSenderContext that jumps back to thisContext when an unhandled exception is raised.  In either case, the inserted ensure and exception handler are removed once control jumps back to thisContext."

	| error exceptionHandlerContext ensureContext ctxt here topContext |
	aSenderContext method isQuick ifTrue: [ 
		^ {aSenderContext step. nil}
	].
	
	here := thisContext.

	"Insert ensure and exception handler contexts under aSenderContext.
	As Halt may not raise an UnhandledError (see #noHandler), it may need to be handled explicitly here."
	error := nil.
	exceptionHandlerContext := ContextPart
		contextOn: UnhandledError, UnhandledHalt do: [ :unhandledError |
			error ifNil: [
				error := unhandledError exception.
				topContext := thisContext.
				Debugger updateDebuggersLeadingTo: aSenderContext for: error.
				unhandledError resumeUnchecked: here jump ]
			ifNotNil: [unhandledError pass ]].
	ctxt := aSenderContext insertSender: exceptionHandlerContext.
	ensureContext := ContextPart
		contextEnsure: [
			error ifNil: [
				topContext := thisContext.
				here jump ]].
	ctxt := ctxt insertSender: ensureContext.
	self jump.  "Control jumps to self"

	"Control resumes here once above ensure block or exception handler is executed"
	^ error ifNil: [
		"No error was raised, remove ensure context by stepping until popped"
		[ctxt isDead or: [topContext isNil]] whileFalse: [topContext := topContext stepToSenderOrNil].
		{topContext. nil}

	] ifNotNil: [
		"Error was raised, remove inserted above contexts then return signaler context"
		aSenderContext terminateTo: ctxt sender.  "remove above ensure and handler contexts"
		{topContext. error}
	].
]

{ #category : 'private' }
ContextPart >> runUntilReturnFrom: unwindBottom [
	"Run the receiver (which must be a stack top context) until unwindBottom context returns. 
	 This is a helper method for #unwindTo:safely: to unwind non-local returns inside unwind blocks."
	
	| here newTop |
	here := thisContext.
	newTop := unwindBottom sender.  "Note 1"
	unwindBottom insertSender: (self class contextEnsure: [here jump]).  "Note 2"
	self jump.  "run the receiver until the above inserted ensure block is executed"
	^newTop  "control resumes here once the above inserted ensure block is executed"
	
"Note 1: It doesn't matter newTop is not a proper stack top context because the unwind machinery will only use it as a starting point in the search for the next unwind context and the computation will never return here. We could make newTop a proper top context by pushing nil to its stack (^newTop push: nil) if need be (see #jump comments).
	
Note 2: The receiver (self) is run by jumping directly to it (the active process abandons thisContext and executes self on its own stack; self must be the top context). However, before jumping to self we insert an ensure block under the unwindBottom context that will execute a jump back to thisContext.

To avoid an infinite recursion of MNU errors inside unwind blocks, Object>>doesNotUnderstand: needs to detect and prevent it.

To avoid a VM crash during termination, #cannotReturn:to: has to make sure its sender's pc is nilled before returning. To try, **save your image**, comment out `pc := nil.` in #cannotReturn:to: and run the following Workspace example:
	 ([[] ensure: [^2]] forkAt: Processor activePriority + 1) terminate
"
]

{ #category : 'query' }
ContextPart >> secondFromBottom [
	"Return the second from bottom of my sender chain"

	self sender ifNil: [^ nil].
	^ self findContextSuchThat: [:c | c sender sender isNil]
]

{ #category : 'debugger access' }
ContextPart >> selector [
	"Answer the selector of the method that created the receiver."

	^self method selector ifNil: [self method defaultSelector].
]

{ #category : 'instruction decoding' }
ContextPart >> send: selector super: superFlag numArgs: numArgs [
	"Simulate the action of bytecodes that send a message with selector, 
	 selector. The argument, superFlag, tells whether the receiver of the 
	 message was specified with 'super' in the source method. The arguments 
	 of the message are found in the top numArgs locations on the stack and 
	 the receiver just below them."

	| thisReceiver arguments lookupClass |
	arguments := Array new: numArgs.
	numArgs to: 1 by: -1 do: [ :i | arguments at: i put: self pop].
	thisReceiver := self pop.
	lookupClass := superFlag
					ifTrue: [self method methodClassAssociation value superclass]
					ifFalse: [self objectClass: thisReceiver].
	QuickStep == self ifTrue:
		[QuickStep := nil.
		^self quickSend: selector to: thisReceiver with: arguments lookupIn: lookupClass].
	^self send: selector to: thisReceiver with: arguments lookupIn: lookupClass
]

{ #category : 'controlling' }
ContextPart >> send: selector to: rcvr with: arguments [
	"Simulate the action of sending a message with selector arguments to rcvr."

	^self send: selector to: rcvr with: arguments lookupIn: (self objectClass: rcvr)
]

{ #category : 'controlling' }
ContextPart >> send: selector to: rcvr with: arguments lookupIn: lookupClass [
	"Simulate the action of sending a message with selector and arguments to rcvr. The argument, lookupClass, is the class in which to lookup the message. This is the receiver's class for normal messages, but for super messages it will be some specific class related to the source method."

	| meth |
	(meth := lookupClass lookupSelector: selector) ifNil:
		[selector == #doesNotUnderstand: ifTrue:
			[self error: 'Recursive message not understood!' translated].
		^self send: #doesNotUnderstand:
				to: rcvr
				with: {(Message selector: selector arguments: arguments) lookupClass: lookupClass}
				lookupIn: lookupClass].
	
	^ self
		executeMethod: meth
		forSelector: selector
		withArgs: arguments
		receiver: rcvr
]

{ #category : 'controlling' }
ContextPart >> send: selector to: rcvr with: arguments super: superFlag  [
	"Simulate the action of sending a message with selector arguments
	 to rcvr. The argument, superFlag, tells whether the receiver of the
	 message was specified with 'super' in the source method."

	^self send: selector
		to: rcvr
		with: arguments
		lookupIn: (superFlag
					ifTrue: [self method methodClassAssociation value superclass]
					ifFalse: [self objectClass: rcvr])
]

{ #category : 'instruction decoding implicit literals' }
ContextPart >> sendSpecial: selector numArgs: numArgs [
	"Simulate the action of bytecodes that send a message with selector, 
	 selector. The arguments  of the message are found in the top numArgs
	 locations on the stack and the receiver just below them."

	| thisReceiver arguments lookupClass |
	arguments := Array new: numArgs.
	numArgs to: 1 by: -1 do: [ :i | arguments at: i put: self pop].
	thisReceiver := self pop.
	lookupClass := self objectClass: thisReceiver.
	QuickStep == self ifTrue:
		[QuickStep := nil.
		^self quickSend: selector to: thisReceiver with: arguments lookupIn: lookupClass].
	^self send: selector to: thisReceiver with: arguments lookupIn: lookupClass
]

{ #category : 'debugger access' }
ContextPart >> sender [
	"Answer the context that sent the message that created the receiver."

	^sender
]

{ #category : 'debugger access' }
ContextPart >> shortErrorReportOn: strm [
	"Write a short error report on the stack (above me) on a stream.  For both the error file, and emailing a bug report. "

	| cnt aContext |
 	strm print: Date today; space; print: Time now; newLine.
	aContext := self.
	cnt := 0.
	[aContext notNil and: [(cnt := cnt + 1) < Debugger defaultDebugStackSize]] whileTrue: [
		strm print: aContext; newLine.  "just class>>selector"	
		aContext := aContext sender]
]

{ #category : 'debugger access' }
ContextPart >> shortStack [
	"Answer a String showing the top ten contexts on my sender chain."

	^ String streamContents: [ :strm |
		(self stackOfSize: Debugger defaultDebugStackSize)
			do: [:item | strm print: item; newLine]]
]

{ #category : 'read-only objects' }
ContextPart >> simulatedObject: anObject instVarAt: anIndex put: aValue  [
	"Primitive. Store a value into a fixed variable in the argument anObject.
	 The numbering of the variables corresponds to the named instance
	 variables.  Fail if the index is not an Integer or is not the index of a
	 fixed variable, or if anObject is read-only. This version correctly simulates
	 assigning to a read-only object. Answer the value stored as the result.
	 Using this message violates the  principle that each object has sovereign
	 control over the storing of values into its instance variables. Essential for
	 the debugger. See Object documentation whatIsAPrimitive."

	<primitive: 174 error: ec>
	ec == #'no modification' ifTrue:
		[^self send: #attemptToAssign:to:withIndex: to: self with: {aValue. anObject. anIndex} lookupIn: self class].
	self primitiveFailed
]

{ #category : 'debugger access' }
ContextPart >> singleRelease [
	"Remove information from the receiver in order to break circularities."

	stackp ifNotNil: [1 to: stackp do: [:i | self at: i put: nil]].
	sender := nil.
	pc := nil
]

{ #category : 'accessing' }
ContextPart >> size [
	"Primitive. Answer the number of indexable variables in the receiver. 
	This value is the same as the largest legal subscript. Essential. See Object 
	documentation whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 212>
	"The number of indexable fields of fixed-length objects is 0"
	^self primitiveFailed
]

{ #category : 'debugger access' }
ContextPart >> sourceCode [
	^self method getSource.
	
	"Note: The above is a bit safer than
		^ methodClass sourceCodeAt: selector
	which may fail if the receiver's method has been changed in
	the debugger (e.g., the method is no longer in the methodDict
	and thus the above selector is something like #Doit:with:with:with:)
	but the source code is still available."
]

{ #category : 'debugger access' }
ContextPart >> stack  [
	"Answer an array of the contexts on the receiver's sender chain."
	^self stackOfSize: 9999
]

{ #category : 'debugger access' }
ContextPart >> stackOfSize: limit  [
	"Answer an OrderedCollection of the top 'limit' contexts
	 on the receiver's sender chain."

	| stack ctxt |
	stack := OrderedCollection new.
	stack addLast: (ctxt := self).
	[(ctxt := ctxt sender) notNil
	 and: [stack size < limit]] whileTrue:
		[stack addLast: ctxt].
	^stack
]

{ #category : 'private' }
ContextPart >> stackPtr  "For use only by the SystemTracer" [
	^ stackp
]

{ #category : 'private' }
ContextPart >> stackp: newStackp [
	"Storing into the stack pointer is a potentially dangerous thing.
	This primitive stores nil into any cells that become accessible as a result,
	and it performs the entire operation atomically."
	"Once this primitive is implemented, failure code should cause an error"

	<primitive: 76>
	self error: 'stackp store failure'.
"
	stackp == nil ifTrue: [stackp := 0].
	newStackp > stackp  'effectively checks that it is a number'
		ifTrue: [oldStackp := stackp.
				stackp := newStackp.
				'Nil any newly accessible cells'
				oldStackp + 1 to: stackp do: [:i | self at: i put: nil]]
		ifFalse: [stackp := newStackp]
"
]

{ #category : 'system simulation' }
ContextPart >> step [
	"Simulate the execution of the receiver's next bytecode.
	Answer the context that would be the active context
	after this bytecode."

	^self interpretNextInstructionFor: self
]

{ #category : 'system simulation' }
ContextPart >> stepToCallee [
	"Step to callee or sender"

	| ctxt |
	ctxt := self.
	[(ctxt := ctxt step) == self] whileTrue.
	^ ctxt
]

{ #category : 'private' }
ContextPart >> stepToCalleeOrNil [
	"Step to callee or sender; step to return and answer nil in case sender cannot be returned to."

	| ctxt |
	ctxt := self.
	[ctxt willFailReturn not and: [(ctxt := ctxt step) == self]] whileTrue.
	ctxt == self ifTrue: [^nil].
	^ctxt
]

{ #category : 'system simulation' }
ContextPart >> stepToSendOrReturn [
	"Simulate the execution of bytecodes until either sending a message or 
	 returning a value to the receiver (that is, until switching contexts)."

	| ctxt |
	[self willSend or: [self willReturn or: [self willReallyStore]]] whileFalse:
		[ctxt := self step.
		 ctxt == self ifFalse:
			[self halt. 
			 "Caused by mustBeBoolean handling"
			 ^ctxt]]
]

{ #category : 'private' }
ContextPart >> stepToSenderOrNil [
	"Step to sender or step to return and answer nil in case sender cannot be returned to."

	| ctxt |
	ctxt := self.
	[(ctxt willReturn and: [ctxt sender isNil or: [ctxt sender isDead]]) not and: [(ctxt := ctxt step) == self or: [ctxt hasSender: self]]] whileTrue.
	ctxt == self ifTrue: [^nil].
	^ctxt
]

{ #category : 'objects from disk' }
ContextPart >> storeDataOn: aDataStream [
	"Contexts are not allowed go to out in DataStreams.  They must be included inside an ImageSegment."

	self error: 'Contexts are not allowed go to out in DataStreams'.
	^ nil
]

{ #category : 'instruction decoding' }
ContextPart >> storeIntoLiteralVariable: value  [
	"Simulate the action of bytecode that stores the top of the stack into a 
	literal variable of my method. If the receiver is read-only
	 this will provoke a send of #attemptToAssign:withIndex:"

	| top result |
	top := self top.
	result := self simulatedObject: value instVarAt: ValueIndex put: top.
	^(self object: result eqeq: top)
		ifTrue: [self]
		ifFalse: [result]
]

{ #category : 'instruction decoding' }
ContextPart >> storeIntoReceiverVariable: offset  [
	"Simulate the action of bytecode that stores the top of the stack into an 
	instance variable of my receiver. If the receiver is read-only
	 this will provoke a send of #attemptToAssign:withIndex:"

	| top result |
	top := self top.
	result := self simulatedObject: self receiver instVarAt: offset + 1 put: top.
	^(self object: result eqeq: top)
		ifTrue: [self]
		ifFalse: [result]
]

{ #category : 'instruction decoding' }
ContextPart >> storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex [
	"Simulate the action of bytecode that stores the top of the stack at
	 an offset in one of my local variables being used as a remote temp vector."

	self object: (self at: tempVectorIndex + 1) basicAt: remoteTempIndex + 1 put: self top
]

{ #category : 'instruction decoding' }
ContextPart >> storeIntoTemporaryVariable: offset  [
	"Simulate the action of bytecode that stores the top of the stack into one 
	of my temporary variables."

	self at: offset + 1 put: self top
]

{ #category : 'debugger access' }
ContextPart >> swapSender: coroutine  [
	"Replace the receiver's sender with coroutine and answer the receiver's previous sender.
	For use in coroutining."

	| oldSender |
	oldSender := sender.
	sender := coroutine.
	^oldSender
]

{ #category : 'temporaries' }
ContextPart >> tempAt: index [
	"Answer the value of the temporary variable whose index is the argument, index."
	self subclassResponsibility
]

{ #category : 'temporaries' }
ContextPart >> tempAt: index put: value  [
	"Store the argument, value, as the temporary variable whose
	index is the argument, index."
	self subclassResponsibility
]

{ #category : 'debugger access' }
ContextPart >> tempNames [
	"Answer a SequenceableCollection of the names of the receiver's temporary 
	 variables, which are strings."

	^ self debuggerMap tempNamesForContext: self
]

{ #category : 'debugger access' }
ContextPart >> tempsAndValues [
	"Return a string of the temporary variabls and their current values"
	^self debuggerMap tempsAndValuesForContext: self
]

{ #category : 'debugger access' }
ContextPart >> tempsAndValuesLimitedTo: sizeLimit indent: indent [
	"Return a string of the temporary variabls and their current values"

	| tempNames title |
	^ String streamContents: [ :strm |
		tempNames := self tempNames.
		1 to: self size do: [ :index |
			title := tempNames size = self size ifTrue: [tempNames at: index]  ifFalse: [ 'argOrTemp', index printString ].
			indent timesRepeat: [strm tab].
			strm nextPutAll: title; nextPut: $:; space; tab.
			strm nextPutAll: 
				((self tempAt: index) printStringLimitedTo: (sizeLimit -3 -title size max: 1)).
			strm newLine ]].
]

{ #category : 'controlling' }
ContextPart >> terminate [
	"Make myself unresumable."

	sender := nil.
	pc := nil.

]

{ #category : 'controlling' }
ContextPart >> terminateTo: previousContext [
	"Terminate all the Contexts between me and previousContext, if previousContext is on my Context stack. Make previousContext my sender."

	| currentContext sendingContext |
	<primitive: 196>
	(self hasSender: previousContext) ifTrue: [
		currentContext := sender.
		[currentContext == previousContext] whileFalse: [
			sendingContext := currentContext sender.
			currentContext terminate.
			currentContext := sendingContext]].
	sender := previousContext
]

{ #category : 'controlling' }
ContextPart >> top [
	"Answer the top of the receiver's stack."

	^self at: stackp
]

{ #category : 'private' }
ContextPart >> tryNamedPrimitiveIn: aCompiledMethod for: aReceiver withArgs: arguments [
	"Invoke the named primitive for aCompiledMethod, answering its result, or,
	 if the primiitve fails, answering the error code."
	<primitive: 218 error: ec>
	ec ifNotNil:
		["If ec is an integer other than -1 there was a problem with primitive 218,
		  not with the external primitive itself.  -1 indicates a generic failure (where
		  ec should be nil) but ec = nil means primitive 218 is not implemented.  So
		  interpret -1 to mean the external primitive failed with a nil error code."
		 ec isInteger ifTrue:
			[ec = -1
				ifTrue: [ec := nil]
				ifFalse: [self primitiveFailed]]].
	^self class primitiveFailTokenFor: ec
]

{ #category : 'private' }
ContextPart >> unwindAndResume: aContext evaluating: aBlock [
	"Unwind the receiver to aContext and resume with aBlock value as result of last send.
	 Execute any unwind blocks while unwinding.
	 ASSUMES aContext is a sender of thisContext."
	
	| ctx |
	ctx := self.
	ctx isUnwindContext ifFalse: [ctx := ctx findNextUnwindContextUpTo: aContext]. "Note 1"
	[ctx isNil] whileFalse: [  "ctx is an #ensure: or #ifCurtailed: context"
		(ctx tempAt: 2) ifNil: [ | unwindBlock |   "unwind block hasn't run yet; see #ensure:"
			ctx tempAt: 2 put: #unwindRunnable.
			unwindBlock := ctx tempAt: 1.   "#ensure:/ifCurtailed: argument block"
			thisContext terminateTo: ctx.   "Note 2"
			unwindBlock value.   "execute the unwind block"
			ctx tempAt: 2 put: #unwindComplete].   "unwind block has been completed"
		ctx := ctx findNextUnwindContextUpTo: aContext].
	thisContext terminateTo: aContext.
	^aBlock value

"Note 1: #findNextUnwindContextUpTo: starts searching from the receiver's sender hence we must check the receiver whether it is an unwind context; see testTerminateEnsureAsStackTop.

Note 2: Non-local returns must be evaluated in the evaluation context (sender chain) in which they were defined. Because the unwind algorithm runs on top of the context chain being unwound we need to keep removing parts of the sender chain so that each unwind block is executed in the right evaluation context."
]

{ #category : 'private' }
ContextPart >> unwindAndStop [
	"A helper method to Process #terminate. Create and answer
	 a helper stack for a terminating process to unwind itself from.
	 Note: push a fake return value to create a proper top context."

	^(self class contextEnsure: [self unwindTo: nil]) push: nil; 
		privSender: [Processor activeProcess suspend] asContext
]

{ #category : 'private-exceptions' }
ContextPart >> unwindTo: aContext [
	"Unwind the receiver to aContext to execute all pending unwind blocks."

	self unwindTo: aContext safely: true
]

{ #category : 'private' }
ContextPart >> unwindTo: aContext safely: aBoolean [
	"Unwind the receiver to aContext. Execute any unwind blocks while unwinding.
	 If aBoolean is false, unwind only blocks that have not run yet, otherwise complete
	 all unwind blocks including those currently in the middle of their execution.
	 Run all unwinds on their original stack."
	
	| ctx |
	ctx := self.
	aBoolean ifTrue: [ctx := (self finishIncompleteUnwindsUpTo: aContext) ifNil: [^self]].
	ctx isUnwindContext ifFalse: [ctx := ctx findNextUnwindContextUpTo: aContext]. "Note 1"
	[ctx isNil] whileFalse: [  "ctx is an #ensure: or #ifCurtailed: context"
		(ctx tempAt: 2) ifNil: [ | top |   "unwind block hasn't run yet; see #ensure:"
			ctx tempAt: 2 put: #unwindRunnable.
			top := (ctx tempAt: 1) asContextWithSender: ctx.   "Note 2"
			top runUntilReturnFrom: top.   "execute the unwind block and return here"
			ctx tempAt: 2 put: #unwindComplete].   "unwind block has been completed"
		ctx := ctx findNextUnwindContextUpTo: aContext]

"Note 1: #findNextUnwindContextUpTo: starts searching from the receiver's sender hence we must check the receiver whether it is an unwind context; see testTerminateEnsureAsStackTop.

Note 2: Create a new top context for each unexecuted unwind block (ctx tempAt: 1) and attach it to its defining context (ctx) on the original stack to execute it in the correct evaluation context (sender chain). Do not allow non-local returns jump out from inside unwind blocks. For more information see #runUntilReturnFrom:.

Cf. the unwind pattern in #unwindAndResume:evaluating:. Using #value instead of #runUntilReturnFrom: for termination would allow non-local returns to escape termination. Note that the approach implemented in this method differs from the intent stated in the comment to #valueUninterruptably. Note, however, that *any* non-local return from an unwind block is specified as undefined by the ANSI standard.

Also note that #unwindTo:safely: can be used to unwind a process (or a sender chain) by another process, as well as to unwind the active process by itself, unlike #unwindAndResume:evaluating:, which only unwinds the active process."
]

{ #category : 'private' }
ContextPart >> willFailReturn [
	"Answer whether self step will cause an illegal return."

	^self willReturn and: [self sender isNil or: [self sender isDead]]
]
