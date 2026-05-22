"
I am a block closure for Eliot's closure implementation. Not to be confused with the old BlockClosure (they were never part of Cuis anyway).

This is a closure converted image. With full closure support, you can finally use recursive blocks like here:

| fac |
	fac := [:n| n > 1 ifTrue:[n * (fac value: n-1)] ifFalse:[1]].
	fac value: 5. "120"

and close over temps correctly, such as here:

	(1 to: 10) do:[:i| UISupervisor whenUIinSafeState:[Transcript newLine; show: i]].

Another good example:

| fib |
fib := [:n| n < 2 ifTrue:[1] ifFalse:[(fib value:n-1) + (fib value:n-2)]].
fib value: 10. "89"
"
Class {
	#name : 'BlockClosure',
	#superclass : 'Object',
	#instVars : [
		'outerContext',
		'startpc',
		'numArgs'
	],
	#category : 'Kernel-Methods',
	#package : 'Kernel',
	#tag : 'Methods'
}

{ #category : 'instance creation' }
BlockClosure class >> outerContext: aContext startpc: aStartpc numArgs: argCount copiedValues: anArrayOrNil [
	^(self new: anArrayOrNil basicSize)
		outerContext: aContext
		startpc: aStartpc
		numArgs: argCount
		copiedValues: anArrayOrNil
]

{ #category : 'accessing' }
BlockClosure >> argumentCount [
	"Answers the number of arguments needed to evaluate the receiver.
	For ansi compatibility."
	^ self numArgs
]

{ #category : 'scheduling' }
BlockClosure >> asContext [
	"Create a MethodContext that is ready to execute self.  Assumes self takes no args (if it does the args will be nil)"

	^self asContextWithSender: nil
]

{ #category : 'private' }
BlockClosure >> asContextWithSender: aContext [
	"Inner private support method for evaluation.  Do not use unless you know what you're doing."

	^(MethodContext newForMethod: outerContext method)
		setSender: aContext
		receiver: outerContext receiver
		method: outerContext method
		closure: self
		startpc: startpc;
		privRefresh
]

{ #category : 'events-support' }
BlockClosure >> asMinimalRepresentation [
	"For use in the when:evaluate: protocol, i.e.,
		foo when: #bar evaluate:[self handleBar].
	Return the receiver."
	^self
]

{ #category : 'objects from disk' }
BlockClosure >> asSerializable [
	^SerializableBlockClosure onBlockClosure: self
]

{ #category : 'exceptions' }
BlockClosure >> assert [
	self assert: self
]

{ #category : 'evaluating' }
BlockClosure >> bench [
	"See how many times I can value in 5 seconds.  I'll answer a meaningful description.
	[ Float pi printString ] bench print.
	[ 80000 factorial printString ] bench print.
	"

	| secondsPerRun startTime endTime count run elapsedTime gcStart gcTime |
	count := 0.
	run := true.
	Smalltalk garbageCollect.
	[ (Delay forSeconds: 5) wait. run := false ] forkAt: Processor timingPriority - 1.
	startTime := Time localMillisecondClock.
	gcStart := (Smalltalk vmParameterAt: 8) + (Smalltalk vmParameterAt: 10).
	[ run ] whileTrue: [ self value. count := count + 1 ].
	endTime := Time localMillisecondClock.
	elapsedTime := endTime - startTime.
	gcTime := (Smalltalk vmParameterAt: 8) + (Smalltalk vmParameterAt: 10) - gcStart.
	secondsPerRun := (endTime - startTime) / (count * 1000).
	^String streamContents: [ :strm |
		secondsPerRun < 1 ifTrue: [
			1.0 / secondsPerRun withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName |
				value printOn: strm fractionDigits: 2.
				strm
					space;
					nextPutAll: unitPrefixSymbol;
					nextPutAll: ' runs per second. ' ]].
		secondsPerRun withDecimalUnitPrefixAndValue: [ :value  :unitPrefixSymbol :unitPrefixName |
			value printOn: strm fractionDigits: 2.
			strm
				space;
				nextPutAll: unitPrefixSymbol;
				nextPutAll: ' seconds per run. '].
		gcTime / elapsedTime * 100 printOn: strm fractionDigits: 2.
		strm
			space;
			nextPutAll: ' % GC time.' ]
]

{ #category : 'accessing' }
BlockClosure >> capturedValues [
	| n copiedValues |
	n := self numCopiedValues.
	copiedValues := Array new: n.
	1 to: n do: [ :i |
		copiedValues at: i put: (self copiedValueAt: i) ].
	^copiedValues
]

{ #category : 'accessing' }
BlockClosure >> copiedValueAt: i [
	<primitive: 60>
	^self basicAt: i
]

{ #category : 'private' }
BlockClosure >> copyForSaving [
	"Answer a copy of the receiver suitable for serialization.
	 Notionally, if the receiver's outerContext has been returned from then nothing
	 needs to be done and we can use the receiver. But there's a race condition
	 determining if the receiver has been returned from (it could be executing in a
	 different process). So answer a copy anyway."
	^self copy
]

{ #category : 'lambda calculus' }
BlockClosure >> curried [
	"
	https://en.wikipedia.org/wiki/Currying
	[ :a :b | a + b ] value: 1 value: 2
	[ :a :b | a + b ] curried value: 1 :: value: 2
	"
	^self argumentCount caseOf: {
		[ 1] -> [[ :arg1 | [ self value: arg1 ]]].
		[ 2] -> [[ :arg1 | [ :arg2 | self value: arg1 value: arg2 ]]].
		[ 3] -> [[ :arg1 | [ :arg2 :arg3 | self value: arg1 value: arg2 value: arg3 ]]].
		[ 4] -> [[ :arg1 | [ :arg2 :arg3 :arg4 | self value: arg1 value: arg2 value: arg3 value: arg4 ]]] }
	otherwise: [ self error: 'can not curry' ]
]

{ #category : 'printing' }
BlockClosure >> decompile [
	^Decompiler new decompileBlock: self
]

{ #category : 'evaluating' }
BlockClosure >> durationToRun [
	"Answer the duration taken to execute this block."

	^ Duration milliSeconds: self millisecondsToRun.
]

{ #category : 'accessing' }
BlockClosure >> endpc [
	"Determine end of block from the instruction preceding it.
	 Find the instruction by using an MNU handler to capture
	 the instruction message sent by the scanner."
	| myMethod scanner preceedingBytecodeMessage end |
	myMethod := self method.
	scanner := InstructionStream new method: myMethod pc: myMethod initialPC.
	[scanner pc < startpc] whileTrue:
		[[scanner interpretNextInstructionFor: nil]
			on: MessageNotUnderstood
			do: [:ex| preceedingBytecodeMessage := ex message]].
	end := preceedingBytecodeMessage arguments last + startpc - 1.
	^end
]

{ #category : 'exceptions' }
BlockClosure >> ensure: aBlock [
	"Evaluate a termination block after evaluating the receiver, regardless of
	 whether the receiver's evaluation completes.  N.B.  This method is *not*
	 implemented as a primitive.  Primitive 198 always fails.  The VM uses prim
	 198 in a context's method as the mark for an ensure:/ifCurtailed: activation."

	| completionStatus returnValue |
	<primitive: 198>
	returnValue := self valueNoContextSwitch.
	completionStatus ifNil:[
		completionStatus := #unwindRunnable.
		aBlock value.
	].
	^returnValue

"The completionStatus temporary variable tracks the progress of the #ensure:/#ifCurtailed: contexts execution using values:
	nil | #unwindRunnable | #unwindComplete
nil indicates the receiver block execution is in progress, #unwindRunnable indicates the receiver block execution has finished but the unwind block (the argument) is still in progress, and #unwindComplete indicates the execution of the unwind block has finished.

The completionStatus variable is being used by the unwinding machinery (#unwindAndResume:evaluating:, #unwindTo:safely:, and #finishIncompleteUnwindsUpTo:) during abnormal termination (i.e. non-local return or termination) to find all unexecuted or partially executed unwind blocks in the sender chain that need to be executed. Note that during an abnormal termination the sender chain is not executed, but only searched for unwind contexts that may contain unexecuted unwind blocks. The completionStatus variable is then accessed by the execution machinery via `ctx tempAt: 2` and the unwind block (#ensure:/#ifCurtailed: argument) as `ctx tempAt: 2`, where ctx refers to the #ensure:/#ifCurtailed: context found in the sender chain.

Originally, only two values (nil | true) were used to indicate whether the receiver block had already run. We introduce a third value to mark the completion of unwind blocks to prevent erroneous repeat evaluation of unwind blocks during process termination. The VM may suspend the execution of the #unwindAndResume:evaluating: or #unwindTo:safely: methods at the suspension point at the target of the backward jump. Without tracking the completion status of unwind blocks, the #finishIncompleteUnwindsUpTo: would incorrectly evaluate the unwind block twice when the process was terminated at such a suspension point. See also #testTerminateEverywhere tests."
]

{ #category : 'scheduling' }
BlockClosure >> fork [
	"Create and schedule a Process running the code in the receiver."
	
	"jmv - Do NOT answer the new process.
	
	See http://lists.squeakfoundation.org/pipermail/squeak-dev/2008-February/124960.html
	
	Most times, these methods return before resuming the new process (if priority of new process is less 
	or equal than current). But they might return afterwards.
	
	This means it is very dangerous to use the returned process in code that stores it in some variable 
	and checks for nil to start a new one. If these methods happen to return after the new process is forked,
	chances are the code that starts all this runs again, that variable is nil, and a second process is forked,
	perhaps breaking some shared state. This kind of bug is hard to spot and debug.
	
	Callers wanting the new process object, should call #newProcess, store the answer, and then #resume.
	
	A way to ensure this bug will not ever happen again is just to answer nil"

	self newProcess resume.
	^nil
]

{ #category : 'scheduling' }
BlockClosure >> forkAndWait [
	"Suspend current process and execute self in new process, when it completes resume current process"

	| semaphore |
	semaphore := Semaphore new.
	[self ensure: [semaphore signal]] fork.
	semaphore wait.

]

{ #category : 'scheduling' }
BlockClosure >> forkAt: priority  [
	"Create and schedule a Process running the code in the receiver at the given priority."
	
	"jmv - Do NOT answer the new process.
	Please see the comment at #fork"

	| forkedProcess |
	forkedProcess := self newProcess.
	forkedProcess priority: priority.
	forkedProcess resume.
	^nil
]

{ #category : 'scheduling' }
BlockClosure >> forkAt: priority named: name [
	"Create and schedule a Process running the code in the receiver at the
	given priority and having the given name."
	
	"jmv - Do NOT answer the new process.
	Please see the comment at #fork"

	| forkedProcess |
	forkedProcess := self newProcess.
	forkedProcess priority: priority.
	forkedProcess name: name.
	 forkedProcess resume.
	^nil
]

{ #category : 'scheduling' }
BlockClosure >> forkNamed: aString [
	"Create and schedule a Process running the code in the receiver and
	having the given name."
	
	"jmv - Do NOT answer the new process.
	Please see the comment at #fork"

	self newProcess name: aString; resume.
	^nil
]

{ #category : 'private' }
BlockClosure >> grabProcessor [
	"Grab the CPU above most application processes and evaluate.
	Return to usual proprity after that."

	^self grabProcessorFor: nil onTimeout: nil
]

{ #category : 'private' }
BlockClosure >> grabProcessorFor: milliseconds onTimeout: timeoutBlock [
	"Evaluate the receiver (block), without the possibility of preemption by regular priority processes.
	If not finished after milliseconds, evaluate timeoutBlock and restore original priority.
	Use with care!"
	"Based on #valueUnpreemptively"
	
	| activeProcess oldPriority result done |
	activeProcess := Processor activeProcess.
	oldPriority := activeProcess priority.
	done := false.
	
	activeProcess priority: Processor highIOPriority + Processor lowIOPriority // 2.
	milliseconds ifNotNil: [
		[
			(Delay forMilliseconds: milliseconds) wait.
			done ifFalse: [
				activeProcess
					suspend;
					priority: oldPriority.
				timeoutBlock value.
				"The timeoutBlock might terminate it, or even resume it!"
				activeProcess isSuspended ifTrue: [
					activeProcess resume ]]
		] forkAt: Processor highIOPriority named: '#grabProcessorFor:onTimeout:' ].

	result := self ensure: [
		done := true.
		activeProcess priority: oldPriority].
	
	"Yield after restoring priority to give the preempted processes a chance to run"
	Processor yield.
	^result
]

{ #category : 'private' }
BlockClosure >> grabProcessorOnlyFor: milliseconds [
	"Grab the CPU above most application processes and evaluate, but only for some milliseconds.
	Return to usual prority after that and finish evaluation if incomplete."

	^self grabProcessorFor: milliseconds onTimeout: nil
]

{ #category : 'error handing' }
BlockClosure >> handles: anException [

	"This allows a block to be the handling condition of an exception handling.
	See Exception class>>handles:"
	
	^self value: anException 
]

{ #category : 'testing' }
BlockClosure >> hasNonLocalReturn [
	"Answer whether the receiver has a method-return ('^') in its code."
	| myMethod scanner end |
	myMethod := self method.
	scanner := InstructionStream new method: myMethod pc: startpc.
	end := self endpc.
	scanner scanFor: [ :byte |
		((myMethod encoderClass nonLocalReturnScanBlockUsing: scanner)
			value: byte)
				or: [scanner pc > end]].
	^scanner pc <= end
]

{ #category : 'accessing' }
BlockClosure >> home [
	^outerContext home
]

{ #category : 'exceptions' }
BlockClosure >> ifCurtailed: aBlock [
	"Evaluate the receiver with an abnormal termination action.
	 Evaluate aBlock only if execution is unwound during execution
	 of the receiver.  If execution of the receiver finishes normally do
	 not evaluate aBlock.  N.B.  This method is *not* implemented as a
	 primitive.  Primitive 198 always fails.  The VM uses prim 198 in a
	 context's method as the mark for an ensure:/ifCurtailed: activation."
	| completionStatus returnValue |
	<primitive: 198>
	returnValue := self valueNoContextSwitch.
	completionStatus := #unwindRunnable.
	^returnValue
]

{ #category : 'evaluating' }
BlockClosure >> ifError: errorHandlerBlock [
	"Evaluate the block represented by the receiver, and normally return it's value.  If an error occurs, the errorHandlerBlock is evaluated, and it's value is instead returned.  The errorHandlerBlock must accept zero, one, or two parameters (the error message and the receiver)."
	"Examples:
		[1 whatsUpDoc] ifError: [:err :rcvr | 'huh?'].
		[1 / 0] ifError: [:err :rcvr |
			'ZeroDivide' = err
				ifTrue: [Float infinity]
				ifFalse: [self error: err]]
"

	^ self on: Error do: [ :ex |
		errorHandlerBlock valueWithPossibleArgument: ex description and: ex receiver ]
]

{ #category : 'accessing' }
BlockClosure >> isBlock [

	^ true
]

{ #category : 'testing' }
BlockClosure >> isCleanClosure [
	"A clean closure is one that doesn't really need the home context because:
		- It doesn't send messages to self or super
		- It doesn't use self in any way
		- It doesn't access any instance variable
		- It doesn't access any outer temp
		- It doesn't do ^ return (Non local return, return from method to caller)
	Therefore it doesn't close over a lexical scope, and in this sense they are trivial.
	They can also be called 'context free' or 'simple' block.
	"

	| recreated source |
	source := self decompile decompileString.

	"This catches any acess to outer context!"
	recreated := [ Compiler evaluate: source. ] on: UndeclaredVariableWarning do: [ :ex | ex return].
	recreated isNil ifTrue: [^false].
	
	recreated hasNonLocalReturn ifTrue: [ ^false ].
	recreated sendsToSuper ifTrue: [ ^false ].
	recreated pushesSelf ifTrue: [ ^false ].
	recreated readsReceiverIvar ifTrue: [ ^false ].
	recreated writesReceiverIvar ifTrue: [ ^false ].

	"Ok."
	^true
]

{ #category : 'testing' }
BlockClosure >> isClosure [
	^true
]

{ #category : 'testing' }
BlockClosure >> isDead [
	"Has self finished"
	^false
]

{ #category : 'events-support' }
BlockClosure >> isReceiverOrAnyArgumentGarbage [
	"For use in the when:evaluate: protocol, i.e.,
 		foo when: #bar evaluate:[self handleBar].."
	^ false
]

{ #category : 'accessing' }
BlockClosure >> method [
	^outerContext method
]

{ #category : 'evaluating' }
BlockClosure >> microsecondsToRun [
	"Answer the number of microseconds taken to execute this block."

	^ Time microsecondsToRun: self

]

{ #category : 'evaluating' }
BlockClosure >> millisecondsToRun [
	"Answer the number of milliseconds taken to execute this block."

	^ Time millisecondsToRun: self

]

{ #category : 'evaluating' }
BlockClosure >> millisecondsToRunWithoutGC [
	"Answer the number of milliseconds taken to execute this block without GC time."

	^(Smalltalk vmParameterAt: 8) + 
		(Smalltalk vmParameterAt: 10) +
		self millisecondsToRun -
		(Smalltalk vmParameterAt: 8) - 
		(Smalltalk vmParameterAt: 10)

]

{ #category : 'scheduling' }
BlockClosure >> newProcess [
	"Answer a Process running the code in the receiver. The process is not 
	scheduled."
	<primitive: 19> "Simulation guard"
	| p |
	p := Process
		forContext: [
			self value.
			p suspend] asContext
		priority: Processor activePriority.
	^p
]

{ #category : 'scheduling' }
BlockClosure >> newProcessForDebugging [
	"Answer a Process running the code in the receiver. The process is not 
	scheduled."
	<primitive: 19> "Simulation guard"
	| p |
	p := Process
		forContext: [
			self value.
			p name: Debugger debuggingTerminatedProcessName.
			p terminate] asContext
		priority: Processor activePriority.
	^p
]

{ #category : 'scheduling' }
BlockClosure >> newProcessWith: anArray  [
	"Answer a Process running the code in the receiver. The receiver's block 
	arguments are bound to the contents of the argument, anArray. The 
	process is not scheduled."
	<primitive: 19> "Simulation guard"
	| p |
	p := Process
		forContext: [
			self valueWithArguments: anArray.
			p suspend] asContext
		priority: Processor activePriority.
	^p
]

{ #category : 'accessing' }
BlockClosure >> numArgs [
	"Answer the number of arguments that must be used to evaluate this block"

	^numArgs
]

{ #category : 'error handing' }
BlockClosure >> numArgsError: numArgsForInvocation [

	| printNArgs |
	printNArgs := [:n| n printString, ' argument', (n = 1 ifTrue: [''] ifFalse:['s'])]. 
	self error: 
			'This block accepts ', (printNArgs value: numArgs), 
			', but was called with ', (printNArgs value: numArgsForInvocation), '.'
]

{ #category : 'accessing' }
BlockClosure >> numCopiedValues [
	"Answer the number of copied values of the receiver.  Since these are
	 stored in the receiver's indexable fields this is the receiver's basic size.
	 Primitive. Answer the number of indexable variables in the receiver. 
	 This value is the same as the largest legal subscript."

	<primitive: 62>
	^self basicSize
]

{ #category : 'objects from disk' }
BlockClosure >> objectForDataStream: refStrm [
	"I am about to be written on an object file.
	Write a textual reference if possible. If not, attempt converting to a serializable object.
	This might also fail. See #onBlockClosure:"

	self isCleanClosure ifTrue: [
		^ DiskProxy
			global: #Compiler
			selector: #evaluate:
			args: (Array with: self decompile decompileString) ].
	^self asSerializable
]

{ #category : 'exceptions' }
BlockClosure >> on: exception do: handlerAction [
	"Evaluate the receiver in the scope of an exception handler.
	The following primitive is just a marker used to find the error handling context. 
	See MethodContext>>#isHandlerOrSignalingContext. "
	<primitive: 199>  
	^ self value
]

{ #category : 'exceptions' }
BlockClosure >> on: exc1 do: block1 on: exc2 do: block2 [

	^[
		self
			on: exc1
			do: block1 ]
		on: exc2
		do: block2
]

{ #category : 'exceptions' }
BlockClosure >> on: exc1 do: block1 on: exc2 do: block2 on: exc3 do: block3 [

	^[
		self
			on: exc1
			do: block1 ]
		on: exc2
		do: block2
		on: exc3
		do: block3
]

{ #category : 'exceptions' }
BlockClosure >> onDNU: selector do: handleBlock [
	"Catch MessageNotUnderstood exceptions but only those of the given selector (DNU stands for doesNotUnderstand:)"

	^ self on: MessageNotUnderstood do: [ :exception |
		exception message selector = selector
			ifTrue: [ handleBlock valueWithPossibleArgument: exception ]
			ifFalse: [ exception pass ]
	  ]
]

{ #category : 'accessing' }
BlockClosure >> outerContext [
	^outerContext
]

{ #category : 'initialization' }
BlockClosure >> outerContext: aContext startpc: aStartpc numArgs: argCount copiedValues: anArrayOrNil [
	outerContext := aContext.
	startpc := aStartpc.
	numArgs := argCount.
	1 to: self numCopiedValues do:
		[:i|
		self at: i put: (anArrayOrNil at: i)]
]

{ #category : 'accessing' }
BlockClosure >> outerContextsDo: aBlock [

	outerContext outerContextsDo: aBlock
]

{ #category : 'printing' }
BlockClosure >> printOn: aStream [
	aStream nextPutAll: '[closure] in '.
	outerContext printOn: aStream
]

{ #category : 'printing' }
BlockClosure >> printStack: depth [
	self print.
	depth > 0 ifTrue: [
		self sender ifNotNil: [ :s | s printStack: depth-1 ]]
]

{ #category : 'testing' }
BlockClosure >> pushesSelf [
	"Answer whether the receiver pushes self to send a message or pass as argument."
	| myMethod scanner end |
	myMethod := self method.
	scanner := InstructionStream new method: myMethod pc: startpc.
	end := self endpc.
	scanner scanFor: [ :byte |
		((myMethod encoderClass pushReceiverScanBlock)
			value: byte)
				or: [scanner pc > end]].
	^scanner pc <= end
]

{ #category : 'testing' }
BlockClosure >> readsReceiverIvar [
	"Answer whether the receiver reads an instance variable of 'self'."
	| myMethod scanner end |
	myMethod := self method.
	scanner := InstructionStream new method: myMethod pc: startpc.
	end := self endpc.
	scanner scanFor: [ :byte |
		((myMethod encoderClass instVarReadScanBlockUsing: scanner)
			value: byte)
				or: [scanner pc > end]].
	^scanner pc <= end
]

{ #category : 'accessing' }
BlockClosure >> receiver [
	^outerContext receiver
]

{ #category : 'private' }
BlockClosure >> reentrant [
	"Answer a version of the recever that can be reentered.
	 Closures are reentrant (unlike BlockContect) so simply answer self."
	^self
]

{ #category : 'controlling' }
BlockClosure >> repeat [
	"Evaluate the receiver repeatedly, ending only if the block explicitly returns."

	[self value. true] whileTrue
]

{ #category : 'controlling' }
BlockClosure >> repeatWithGCIf: testBlock [
	| ans |
	"run the receiver, and if testBlock returns true, garbage collect and run the receiver again"
	ans := self value.
	(testBlock value: ans) ifTrue: [ Smalltalk garbageCollect. ans := self value ].
	^ans
]

{ #category : 'debugger access' }
BlockClosure >> sender [
	"Answer the context that sent the message that created the receiver."

	^outerContext sender
]

{ #category : 'testing' }
BlockClosure >> sendsToSuper [
	"Answer whether the receiver sends any message to super."
	| myMethod scanner end |
	myMethod := self method.
	scanner := InstructionStream new method: myMethod pc: startpc.
	end := self endpc.
	scanner scanFor: [ :byte |
		((myMethod encoderClass superSendScanBlockUsing: scanner)
			value: byte)
				or: [scanner pc > end]].
	^scanner pc <= end
]

{ #category : 'system simulation' }
BlockClosure >> simulateValueWithArguments: anArray caller: aContext [
	"Simulate the valueWithArguments: primitive. Fail if anArray is not an array of the right arity."
	| newContext sz |
	newContext := (MethodContext newForMethod: outerContext method)
						setSender: aContext
						receiver: outerContext receiver
						method: outerContext method
						closure: self
						startpc: startpc.
	((newContext objectClass: anArray) ~~ Array
	 or: [numArgs ~= anArray size]) ifTrue:
		[^ContextPart primitiveFailTokenFor: nil].
	sz := self basicSize.
	newContext stackp: sz + numArgs.
	1 to: numArgs do:
		[:i| newContext at: i put: (anArray at: i)].
	1 to: sz do:
		[:i| newContext at: i + numArgs put: (self at: i)].
	^newContext
]

{ #category : 'accessing' }
BlockClosure >> size [
	"Extract this closure's bytecode size (number of bytes) by accessing the closure
	creation bytecode in the enclosing method."
	
	^ ((self method at: self startpc - 2) bitShift: 8) + (self method at: self startpc - 1)
]

{ #category : 'accessing' }
BlockClosure >> startpc [
	^startpc
]

{ #category : 'printing' }
BlockClosure >> storeOn: aStream [
	"
	[] storeString
	"
	aStream nextPut: $(.
	self asSerializable storeOn: aStream.
	aStream nextPutAll: ' asEvaluable)'
]

{ #category : 'services' }
BlockClosure >> timeProfile [

	^TimeProfileBrowser onBlock: self
]

{ #category : 'evaluating' }
BlockClosure >> timeToRun [
	"Answer the number of milliseconds taken to execute this block."

	^ Time millisecondsToRun: self

]

{ #category : 'evaluating' }
BlockClosure >> value [
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the copied values to the activation as its copied
	 temps. Primitive. Essential."
	<primitive: 201>

	numArgs ~= 0 ifTrue:
		[self numArgsError: 0].
	self primitiveFailed.
]

{ #category : 'evaluating' }
BlockClosure >> value: firstArg [
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the argument and copied values to the activation
	 as its argument and copied temps. Primitive. Essential."
	<primitive: 202>
	
	numArgs ~= 1 ifTrue:
		[self numArgsError: 1].
	self primitiveFailed.
]

{ #category : 'evaluating' }
BlockClosure >> value: firstArg value: secondArg [
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments and copied values to the activation
	 as its arguments and copied temps. Primitive. Essential."
	<primitive: 203>

	numArgs ~= 2 ifTrue:
		[self numArgsError: 2].
	self primitiveFailed.
]

{ #category : 'evaluating' }
BlockClosure >> value: firstArg value: secondArg value: thirdArg [
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments and copied values to the activation
	 as its arguments and copied temps. Primitive. Essential."
	<primitive: 204>
	
	numArgs ~= 3 ifTrue:
		[self numArgsError: 3].
	self primitiveFailed.
]

{ #category : 'evaluating' }
BlockClosure >> value: firstArg value: secondArg value: thirdArg value: fourthArg [
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments and copied values to the activation
	 as its arguments and copied temps. Primitive. Essential."
	<primitive: 205>

	numArgs ~= 4 ifTrue:
		[self numArgsError: 4].
	self primitiveFailed.
]

{ #category : 'private' }
BlockClosure >> valueAndWaitWhileUnwinding: contextToUnwind [
	"Evaluate the receiver and suspend current process until argument's sender chain is unwound."

	| semaphore newBottom |
	contextToUnwind ifNil: [^self].
	semaphore := Semaphore new.
	newBottom := contextToUnwind class contextEnsure: [semaphore signal].
	contextToUnwind bottomContext insertSender: newBottom.
	self value: contextToUnwind.
	semaphore wait
]

{ #category : 'evaluating' }
BlockClosure >> valueAt: blockPriority  [
	"Evaluate the receiver (block), with another priority as the actual one 
	and restore it afterwards. The caller should be careful with using 
	higher priorities."
	| activeProcess result outsidePriority |
	activeProcess := Processor activeProcess.
	outsidePriority := activeProcess priority.
	activeProcess priority: blockPriority.
	result := self ensure: [activeProcess priority: outsidePriority].
	"Yield after restoring lower priority to give the preempted processes a  
	chance to run."
	blockPriority > outsidePriority
		ifTrue: [Processor yield].
	^ result
]

{ #category : 'private' }
BlockClosure >> valueEnsured [
	"Protect against process termination.
	Suggested by Esteban Maringolo at Martin McClure's 'Threads, Critical Sections, and Termination' (Smalltalks 2019 conference)"
	[] ensure: self
]

{ #category : 'evaluating' }
BlockClosure >> valueNoContextSwitch [
	"An exact copy of BlockClosure>>value except that this version will not preempt
	 the current process on block activation if a higher-priority process is runnable.
	 Primitive. Essential."
	<primitive: 221>
	numArgs ~= 0 ifTrue:
		[self numArgsError: 0].
	self primitiveFailed
]

{ #category : 'evaluating' }
BlockClosure >> valueNoContextSwitch: anArg [
	"An exact copy of BlockClosure>>value: except that this version will not preempt
	 the current process on block activation if a higher-priority process is runnable.
	 Primitive. Essential."
	<primitive: 222>
	numArgs ~= 1 ifTrue:
		[self numArgsError: 1].
	self primitiveFailed
]

{ #category : 'private' }
BlockClosure >> valueUnpreemptively [
	"Evaluate the receiver (block), without the possibility of preemption by higher priority processes. Use this facility VERY sparingly!"
	"Think about using Block>>valueUninterruptably first, and think about using Semaphore>>critical: before that, and think about redesigning your application even before that! 
	After you've done all that thinking, go right ahead and use it..."
	| activeProcess oldPriority result |
	activeProcess := Processor activeProcess.
	oldPriority := activeProcess priority.
	activeProcess priority: Processor highestPriority.
	result := self ensure: [activeProcess priority: oldPriority].
	"Yield after restoring priority to give the preempted processes a chance to run"
	Processor yield.
	^result
]

{ #category : 'evaluating' }
BlockClosure >> valueWithArguments: anArray [
	"Activate the receiver, creating a closure activation (MethodContext)
	 whose closure is the receiver and whose caller is the sender of this
	 message. Supply the arguments in an anArray and copied values to
	 the activation as its arguments and copied temps. Primitive. Essential."
	<primitive: 206>

	numArgs ~= anArray size ifTrue:
		[self numArgsError: anArray size].
	self primitiveFailed
]

{ #category : 'evaluating' }
BlockClosure >> valueWithExit  [
	  self value: [ ^nil ]
]

{ #category : 'evaluating' }
BlockClosure >> valueWithPossibleArgs: anArray [
	"Generally, prefer #valueWithPossibleArgument: and #valueWithPossibleArgument:and:
	for performance."
	^ numArgs = 0
		ifTrue: [ self value ]
		ifFalse: [
			self valueWithArguments:
				(numArgs = anArray size
					ifTrue: [ anArray ]
					ifFalse: [
						numArgs > anArray size
							ifTrue: [ anArray , (Array new: numArgs - anArray size) ]
							ifFalse: [
								anArray
									copyFrom: 1
									to: numArgs ]]) ].
]

{ #category : 'evaluating' }
BlockClosure >> valueWithPossibleArgument: anArg  [
	"Evaluate the block represented by the receiver. 
	 If the block requires one argument, use anArg"

	numArgs = 0 ifTrue: [ ^self value ].
	^self value: anArg
]

{ #category : 'evaluating' }
BlockClosure >> valueWithPossibleArgument: anArg and: secondArg [
	"Evaluate the block represented by the receiver. 
	 If the block requires one argument, use anArg, 
	if it requires two, use anArg and secondArg.
	Squeak uses #cull:, #cull:cull:, etc. I (jmv) find that name quite obscure"

	numArgs = 0 ifTrue: [ ^self value ].
	numArgs = 1 ifTrue: [ ^self value: anArg ].
	^self value: anArg value: secondArg
]

{ #category : 'evaluating' }
BlockClosure >> valueWithin: aDuration onTimeout: timeoutBlock [
	"Evaluate the receiver.
	If the evaluation does not complete in less than aDuration evaluate the timeoutBlock instead"

	| theProcess delay watchdog tag |

	aDuration <= `Duration zero` ifTrue: [^ timeoutBlock value ].

	"the block will be executed in the current process"
	theProcess := Processor activeProcess.
	delay := aDuration asDelay.
	tag := self.

	"make a watchdog process"
	watchdog := [
		delay wait. 	"wait for timeout or completion"
		theProcess ifNotNil:[ theProcess signalException: (TimedOut new tag: tag)] 
	] newProcess.

	"Watchdog needs to run at high priority to do its job (but not at timing priority)"
	watchdog priority: Processor timingPriority-1.

	"catch the timeout signal"
	^ [	watchdog resume.				"start up the watchdog"
		self ensure:[						"evaluate the receiver"
			theProcess := nil.				"it has completed, so ..."
			delay delaySemaphore signal.	"arrange for the watchdog to exit"
		]] on: TimedOut do: [ :e | 
			e tag == tag 
				ifTrue:[ timeoutBlock value ]
				ifFalse:[ e pass]].
]

{ #category : 'controlling' }
BlockClosure >> whileFalse [
	"Ordinarily compiled in-line, and therefore not overridable.
	This is in case the message is sent to other than a literal block.
	Evaluate the receiver, as long as its value is false."
 
	^ [self value] whileFalse: []
]

{ #category : 'controlling' }
BlockClosure >> whileFalse: aBlock  [
	"Ordinarily compiled in-line, and therefore not overridable.
	This is in case the message is sent to other than a literal block.
	Evaluate the argument, aBlock, as long as the value of the receiver is false."

	^ [self value] whileFalse: [aBlock value]
]

{ #category : 'controlling' }
BlockClosure >> whileNil: aBlock  [
	"Unlike #whileTrue/False: this is not compiled inline."
	^ [self value == nil] whileTrue: [ aBlock value ]
]

{ #category : 'controlling' }
BlockClosure >> whileNotNil: aBlock  [
	"Unlike #whileTrue/False: this is not compiled inline."
	^ [self value == nil] whileFalse: [aBlock value]
]

{ #category : 'controlling' }
BlockClosure >> whileTrue [
	"Ordinarily compiled in-line, and therefore not overridable.
	This is in case the message is sent to other than a literal block.
	Evaluate the receiver, as long as its value is true."
 
	^ [self value] whileTrue: []
]

{ #category : 'controlling' }
BlockClosure >> whileTrue: aBlock  [
	"Ordinarily compiled in-line, and therefore not overridable.
	This is in case the message is sent to other than a literal block.
	Evaluate the argument, aBlock, as long as the value of the receiver is true."

	^ [self value] whileTrue: [aBlock value]
]

{ #category : 'lambda calculus' }
BlockClosure >> withFirstArg: arg1 [
	"
	https://en.wikipedia.org/wiki/Partial_application

	[ :a :b | a + b ] value: 1 value: 2
	[ :a :b | a + b ] withFirstArg: 1
	([ :a :b | a + b ] withFirstArg: 1) value: 2
	([ :a :b | a + b ] withFirstArg: 1) withFirstArg: 2
	(([ :a :b | a + b ] withFirstArg: 1) withFirstArg: 2) value

	([ :a :b | a - b ] withFirstArg: 1) value: 2
	"
	^self argumentCount caseOf: {
		[ 1] -> [[ self value: arg1 ]].
		[ 2] -> [[ :arg2 | self value: arg1 value: arg2 ]].
		[ 3] -> [[ :arg2 :arg3 | self value: arg1 value: arg2 value: arg3 ]].
		[ 4] -> [[ :arg2 :arg3 :arg4 | self value: arg1 value: arg2 value: arg3 value: arg4 ]] }
	otherwise: [ self error: 'can not un-curry' ]
]

{ #category : 'testing' }
BlockClosure >> writesReceiverIvar [
	"Answer whether the receiver writes an instance variable of 'self'."
	| myMethod scanner end |
	myMethod := self method.
	scanner := InstructionStream new method: myMethod pc: startpc.
	end := self endpc.
	scanner scanFor: [ :byte |
		((myMethod encoderClass instVarWriteScanBlockUsing: scanner)
			value: byte)
				or: [scanner pc > end]].
	^scanner pc <= end
]
