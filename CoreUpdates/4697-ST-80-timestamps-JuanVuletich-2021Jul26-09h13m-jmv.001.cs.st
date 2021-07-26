'From Cuis 5.0 [latest update: #4695] on 26 July 2021 at 9:18:13 am'!

!Date class methodsFor: 'general inquiries' stamp: 'ST-80 5/31/1983 9:10:35'!
dateAndTimeNow
	"Answer an array with first element Date today and second element Time now."

	^Time dateAndTimeNow! !


!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
blockReturnTop
	"Print the ReturnTopOfStack bytecode."
	self print: 'blockReturn'! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
doDup
	"Print the Duplicate Top of Stack bytecode."
	self print: 'dup'! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
doPop
	"Print the Remove Top of Stack bytecode."
	self print: 'pop'! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
methodReturnConstant: value 
	"Print the Return Constant bytecode."
	self print: 'return: ' , value printString! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
methodReturnReceiver
	"Print the Return Self bytecode."
	self print: 'returnSelf'! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
methodReturnTop
	"Print the Return Top of Stack bytecode."
	self print: 'returnTop'! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
popIntoLiteralVariable: anAssociation 
	"Print the Removes the Top of the Stack and Stores it into a Literal Variable
	bytecode."
	self print: 'popIntoLit: ' , anAssociation key! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
popIntoReceiverVariable: offset 
	"Print the Removes the Top of the Stack and Stores it into an Instance Variable
	bytecode."
	self print: 'popIntoRcvr: ' , offset printString! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
popIntoTemporaryVariable: offset 
	"Print the Removes the Top of the Stack and Stores it into a Temporary Variable
	bytecode."
	self print: 'popIntoTemp: ' , offset printString! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
pushActiveContext
	"Print the Push the Active Context on the Top of its Own Stack bytecode."
	self print: 'pushThisContext: '! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
pushLiteralVariable: anAssociation
	"Print the Push the Contents of anAssociation on the Top of the Stack bytecode."
	self print: 'pushLit: ' , anAssociation key! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
pushReceiver
	"Print the Push the Active Context's Receiver on the Top of the Stack bytecode."
	self print: 'self'! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
pushReceiverVariable: offset
	"Print the Push the Contents of the Receiver's Instance Variable whose Index
	is the argument, offset, on the Top of the Stack bytecode."
	self print: 'pushRcvr: ' , offset printString! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
pushTemporaryVariable: offset
	"Print the Push the Contents of the Temporary Variable whose Index is the
	argument, offset, on the Top of the Stack bytecode."
	self print: 'pushTemp: ' , offset printString! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
storeIntoLiteralVariable: anAssociation 
	"Print the Store the Top of the Stack into a Literal Variable of the Method bytecode."
	self print: 'storeIntoLit: ' , anAssociation key! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
storeIntoReceiverVariable: offset 
	"Print the Store the Top of the Stack into an Instance Variable of the Method
	bytecode."
	self print: 'storeIntoRcvr: ' , offset printString! !

!InstructionPrinter methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
storeIntoTemporaryVariable: offset 
	"Print the Store the Top of the Stack into a Temporary Variable of the Method
	bytecode."
	self print: 'storeIntoTemp: ' , offset printString! !


!InstructionStream methodsFor: 'scanning' stamp: 'ST-80 5/31/1983 9:10:35'!
followingByte
	"Answer the following bytecode."
	^self method at: pc + 1! !

!InstructionStream methodsFor: 'scanning' stamp: 'ST-80 5/31/1983 9:10:35'!
nextByte
	"Answer the next bytecode."
	^self method at: pc! !

!InstructionStream methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
method: method pc: startpc 
	sender _ method. 
	"allows this class to stand alone as a method scanner"
	pc _ startpc! !


!InstructionStream class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
on: method 
	"Answer a new InstructionStream on the argument, method."

	^self new method: method pc: method initialPC! !


!ContextPart methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
doDup
	"Simulates the action of a 'duplicate top of stack' bytecode."

	self push: self top! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
doPop
	"Simulates the action of a 'remove top of stack' bytecode."

	self pop! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
jump: distance 
	"Simulates the action of a 'unconditional jump' bytecode whose
	offset is the argument, distance."

	pc _ pc + distance! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
pushActiveContext
	"Simulates the action of bytecode that pushes the the active
	context on the top of its own stack."

	self push: self! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
pushConstant: value 
	"Simulates the action of bytecode that pushes the constant, value, on
	the top of the stack."

	self push: value! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
pushReceiver
	"Simulates the action of bytecode that pushes the the active
	context's receiver on the top of the stack."

	self push: self receiver! !

!ContextPart methodsFor: 'debugger access' stamp: 'ST-80 5/31/1983 9:10:35'!
depthBelow: aContext
	"Answer how many calls between this and aContext."
	| this depth |
	this _ self.
	depth _ 0.
	[this == aContext or: [this == nil]]
		whileFalse:
			[this _ this sender.
			depth _ depth + 1].
	^depth! !

!ContextPart methodsFor: 'debugger access' stamp: 'ST-80 5/31/1983 9:10:35'!
hasSender: context 
	"Answer true if the receiver is strictly above context on the stack."

	| s |
	self == context ifTrue: [^false].
	s _ sender.
	[s == nil]
		whileFalse: 
			[s == context ifTrue: [^true].
			s _ s sender].
	^false! !

!ContextPart methodsFor: 'debugger access' stamp: 'ST-80 5/31/1983 9:10:35'!
releaseTo: caller 
	"Remove information from the receiver and the contexts on its 
	sender chain up to caller in order to break circularities."

	| c s |
	c _ self.
	[c == nil or: [c == caller]]
		whileFalse: 
			[s _ c sender.
			c singleRelease.
			c _ s]! !

!ContextPart methodsFor: 'debugger access' stamp: 'ST-80 5/31/1983 9:10:35'!
stack 
	"Answer an array of the contexts on the receiver's sender chain."
	^self stackOfSize: 9999! !

!ContextPart methodsFor: 'debugger access' stamp: 'ST-80 5/31/1983 9:10:35'!
swapSender: coroutine 
	"Replace the receiver's sender with coroutine and answer the receiver's previous sender.
	For use in coroutining."

	| oldSender |
	oldSender _ sender.
	sender _ coroutine.
	^oldSender! !

!ContextPart methodsFor: 'system simulation' stamp: 'ST-80 5/31/1983 9:10:35'!
step
	"Simulate the execution of the receiver's next bytecode.
	Answer the context that would be the active context
	after this bytecode."

	^self interpretNextInstructionFor: self! !

!ContextPart methodsFor: 'temporaries' stamp: 'ST-80 5/31/1983 9:10:35'!
tempAt: index
	"Answer the value of the temporary variable whose index is the argument, index."
	self subclassResponsibility! !

!ContextPart methodsFor: 'temporaries' stamp: 'ST-80 5/31/1983 9:10:35'!
tempAt: index put: value 
	"Store the argument, value, as the temporary variable whose
	index is the argument, index."
	self subclassResponsibility! !


!ContextPart class methodsFor: 'examples' stamp: 'ST-80 5/31/1983 9:10:35'!
runSimulated: aBlock
	"The simulator is a group of methods in class ContextPart which do what the 
	Smalltalk interpreter does.  They execute Smalltalk bytecodes.  By adding code 
	to the simulator, you could take statistics on the running of Smalltalk methods.
	See also trace: callStatistics: and instructionStatistics: for sample uses"

	^ thisContext sender
		runSimulated: aBlock
		contextAtEachStep: [:ignored]

	"ContextPart runSimulated: [Pen new defaultNib: 5; go: 100]"! !


!MethodContext methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
method
	^method! !

!MethodContext methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
receiver
	^receiver! !

!MethodContext methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
removeSelf
	"Nil the receiver pointer and answer the former value."

	| tempSelf |
	tempSelf _ receiver.
	receiver _ nil.
	^tempSelf! !


!Decompiler methodsFor: 'control' stamp: 'ST-80 5/31/1983 9:10:35'!
statementsTo: end
	| blockPos stackPos t |
	"Decompile the method from pc up to end and return an array of
	expressions.  If at run time this block will leave a value on
	the stack, set hasValue to true.  If the block ends with a jump or return,
	set exit to the destination of the jump, or the end of the method;
	otherwise, set exit = end.  Leave pc = end."
	blockPos _ statements size.
	stackPos _ stack size.
	[pc < end]
		whileTrue:
			[lastPc _ pc.  limit _ end.  "for performs"
			self interpretNextInstructionFor: self].
	"If there is an additional item on the stack, it will be the value
	of this block."
	(hasValue _ stack size > stackPos)
		ifTrue:
			[statements addLast: stack removeLast].
	lastJumpPc = lastPc ifFalse: [exit _ pc].
	^self popTo: blockPos! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
methodReturnConstant: value
	self pushConstant: value; methodReturnTop! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
methodReturnReceiver
	self pushReceiver; methodReturnTop! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
popIntoLiteralVariable: value
	self pushLiteralVariable: value; doStore: statements! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
popIntoReceiverVariable: offset
	self pushReceiverVariable: offset; doStore: statements! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
pushActiveContext
	stack addLast: constructor codeThisContext! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
pushConstant: value
	| node |
	node _ value == true ifTrue: [constTable at: 2]
		ifFalse: [value == false ifTrue: [constTable at: 3]
		ifFalse: [value == nil ifTrue: [constTable at: 4]
		ifFalse: [constructor codeAnyLiteral: value]]].
	stack addLast: node! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
pushLiteralVariable: assoc
	stack addLast: (constructor codeAnyLitInd: assoc)! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
pushReceiver
	stack addLast: (constTable at: 1)! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
storeIntoLiteralVariable: assoc
	self pushLiteralVariable: assoc; doStore: stack! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
storeIntoReceiverVariable: offset
	self pushReceiverVariable: offset; doStore: stack! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
storeIntoTemporaryVariable: offset
	self pushTemporaryVariable: offset; doStore: stack! !

!Decompiler methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
popTo: oldPos
	| t |
	t _ Array new: statements size - oldPos.
	(t size to: 1 by: -1) do:
		[:i | t at: i put: statements removeLast].
	^t! !


!Message methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
arguments
	"Answer the arguments of the receiver."
	^args! !

!Message methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
selector
	"Answer the selector of the receiver."
	^selector! !

!Message methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
setSelector: aSymbol arguments: anArray 
	selector _ aSymbol.
	args _ anArray! !


!Message class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
selector: aSymbol argument: anObject 
	"Answer an instance of me whose selector is aSymbol and single argument
	is anObject."

	^self new setSelector: aSymbol arguments: (Array with: anObject)! !

!Message class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
selector: aSymbol arguments: anArray 
	"Answer an instance of me with selector, aSymbol, and arguments,
	anArray."

	^self new setSelector: aSymbol arguments: anArray! !


!Delay methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
resumptionTime
	"Answer the value of the system's millisecondClock at which the receiver's 
	suspended Process will resume."

	^resumptionTime! !


!ContextPart reorganize!
('accessing' at: at:put: basicAt: basicAt:put: basicSize client contextForLocalVariables depthBelow home method methodNode methodReturnContext receiver size)
('instruction decoding' doDup doPop jump: jump:if: methodReturnConstant: methodReturnReceiver methodReturnTop popIntoLiteralVariable: popIntoReceiverVariable: popIntoRemoteTemp:inVectorAt: popIntoTemporaryVariable: pushActiveContext pushClosureCopyNumCopiedValues:numArgs:blockSize: pushConstant: pushLiteralVariable: pushNewArrayOfSize: pushReceiver pushReceiverVariable: pushRemoteTemp:inVectorAt: pushTemporaryVariable: return:from: send:super:numArgs: storeIntoLiteralVariable: storeIntoReceiverVariable: storeIntoRemoteTemp:inVectorAt: storeIntoTemporaryVariable:)
('debugger access' arguments contextStack depthBelow: errorReportOn: hasSender: messageForYourself methodClass namedTempAt: namedTempAt:put: print:on: releaseTo: selector sender shortErrorReportOn: shortStack singleRelease sourceCode stack stackOfSize: swapSender: tempNames tempsAndValues tempsAndValuesLimitedTo:indent:)
('controlling' closureCopy:copiedValues: jump pop push: quickSend:to:with:super: restart resume resume: resume:through: resumeEvaluating: return return: return:through: return:to: runUntilErrorOrReturnFrom: send:to:with:lookupIn: send:to:with:super: terminate terminateTo: top)
('printing' printDetails: printOn: printStack:)
('system simulation' completeCallee: quickStep runSimulated:contextAtEachStep: step stepToCallee stepToSendOrReturn)
('private' activateReturn:value: cannotReturn:to: cut: doPrimitive:method:receiver:args: insertSender: isPrimFailToken: privSender: push:fromIndexable: runUnwindUntilErrorOrReturnFrom: stackPtr stackp: tryNamedPrimitiveIn:for:withArgs:)
('private-exceptions' canHandleSignal: evaluateSignal: exceptionClass exceptionHandlerBlock findNextHandlerContext findNextHandlerOrSignalingContext findNextUnwindContextUpTo: handleSignal: isHandlerContext isHandlerOrSignalingContext isUnwindContext nextHandlerContext unwindTo:)
('objects from disk' storeDataOn:)
('query' bottomContext copyStack copyTo: findContextSuchThat: findSecondToOldestSimilarSender findSimilarSender hasContext: isBottomContext isContext isDead secondFromBottom)
('mirror primitives' object:basicAt: object:basicAt:put: object:eqeq: object:instVarAt: object:instVarAt:put: object:perform:withArguments:inClass: objectClass: objectSize:)
('closure support' contextTag)
('temporaries' tempAt: tempAt:put:)
!


!ContextPart class reorganize!
('examples' runSimulated: tallyInstructions: tallyMethods: trace: trace:on: trace:onFileNamed:)
('simulation' primitiveFailTokenFor:)
('instance creation' basicNew: initializedInstance new new: newForMethod:)
('private' carefullyPrint:on: isContextClass)
('special context creation' contextEnsure: contextOn:do: theReturnMethod)
('cached class state' primitiveFailToken releaseClassCachedState valueIndex)
!


!Delay reorganize!
('delaying' isExpired wait)
('private' adjustResumptionTimeOldBase:newBase: resumptionTime: schedule setDelay: setDelay:forSemaphore: signalWaitingProcess unschedule)
('public' beingWaitedOn beingWaitedOn: delayDuration delaySemaphore)
('printing' printOn:)
('accessing' resumptionTime)
!

