'From Cuis 4.2 of 25 July 2013 [latest update: #2895] on 16 August 2016 at 8:03:39 pm'!

!MethodContext commentStamp: '<historical>' prior: 0!
My instances hold all the dynamic state associated with the execution of either a method activation resulting from a message send or a block activation resulting from a block evaluation.  In addition to their inherited state, this includes the receiver (self), the closure for a BlockClosure activation (which is nil for a method activation), a CompiledMethod, and space in the variable part of the context for arguments and temporary variables.
	
MethodContexts, though normal in their variable size, are actually only used in two sizes, small and large, which are determined by the temporary space required by the method being executed.

MethodContexts must only be created using the method newForMethod:.  Note that it is impossible to determine the real object size of a MethodContext except by asking for the frameSize of its method.  Any fields above the stack pointer (stackp) are truly invisible -- even (and especially!!) to the garbage collector.  Any store into stackp other than by the primitive method stackp: is potentially fatal.!


!ContextPart methodsFor: 'controlling' stamp: 'eem 4/25/2012 10:48'!
resume: value through: firstUnwindCtxt
	"Unwind thisContext to self and resume with value as result of last send.
	 Execute any unwind blocks while unwinding.
	 ASSUMES self is a sender of thisContext."

	| ctxt unwindBlock |
	self isDead ifTrue: [self cannotReturn: value to: self].
	ctxt := firstUnwindCtxt.
	[ctxt isNil] whileFalse:
		[(ctxt tempAt: 2) ifNil:
			[ctxt tempAt: 2 put: true.
			 unwindBlock := ctxt tempAt: 1.
			 thisContext terminateTo: ctxt.
			 unwindBlock value].
		 ctxt := ctxt findNextUnwindContextUpTo: self].
	thisContext terminateTo: self.
	^value
! !

!ContextPart methodsFor: 'controlling' stamp: 'eem 4/25/2012 10:47'!
return: value through: firstUnwindContext
	"Unwind thisContext to self and return value to self's sender.
	 Execute any unwind blocks while unwinding.
	 ASSUMES self is a sender of thisContext."

	sender ifNil: [self cannotReturn: value to: sender].
	sender resume: value through: firstUnwindContext! !


!InstructionStream methodsFor: 'testing' stamp: 'eem 5/19/2014 10:58'!
willBlockReturn
	"Answer whether the next bytecode is a return."
	| method |
	method := self method.
	^method encoderClass isBlockReturnAt: pc in: method! !

!InstructionStream methodsFor: 'testing' stamp: 'eem 5/18/2014 13:37'!
willJump
	"Answer whether the next bytecode is an uncoinditional jump."
	| method |
	method := self method.
	^method encoderClass isJumpAt: pc in: method! !

!InstructionStream methodsFor: 'testing' stamp: 'eem 5/18/2014 13:34'!
willJumpIfFalse
	"Answer whether the next bytecode is a jump-if-false."
	| method |
	method := self method.
	^method encoderClass isBranchIfFalseAt: pc in: method! !

!InstructionStream methodsFor: 'testing' stamp: 'eem 5/18/2014 13:34'!
willJumpIfTrue
	"Answer whether the next bytecode is a jump-if-true."
	| method |
	method := self method.
	^method encoderClass isBranchIfTrueAt: pc in: method! !

!InstructionStream methodsFor: 'testing' stamp: 'eem 5/19/2014 10:59'!
willJustPop
	"Answer whether the bytecode at pc is a pop."
	| method |
	method := self method.
	^method encoderClass isJustPopAt: pc in: method! !

!InstructionStream methodsFor: 'testing' stamp: 'eem 5/19/2014 10:59'!
willReallySend
	"Answer whether the next bytecode is a real message-send, not blockCopy:."
	| method |
	method := self method.
	^method encoderClass isRealSendAt: pc in: method! !

!InstructionStream methodsFor: 'testing' stamp: 'eem 5/19/2014 10:59'!
willReturn
	"Answer whether the next bytecode is a return."
	| method |
	method := self method.
	^method encoderClass isReturnAt: pc in: method! !

!InstructionStream methodsFor: 'testing' stamp: 'eem 5/19/2014 10:59'!
willSend
	"Answer whether the next bytecode is a message-send."
	| method |
	method := self method.
	^method encoderClass isSendAt: pc in: method! !

!InstructionStream methodsFor: 'testing' stamp: 'eem 5/19/2014 10:59'!
willStore
	"Answer whether the bytecode at pc is a store or store-pop."
	| method |
	method := self method.
	^method encoderClass isStoreAt: pc in: method! !

!InstructionStream methodsFor: 'testing' stamp: 'eem 5/19/2014 11:00'!
willStorePop
	"Answer whether the bytecode at pc is a store-pop."
	| method |
	method := self method.
	^method encoderClass isStorePopAt: pc in: method! !

!InstructionStream methodsFor: 'decoding' stamp: 'eem 5/18/2014 14:22'!
interpretJump
	"If the instruction at pc is an unconditional jump, interpret it, advancing the pc,
	 and answering the jump distance. Otherwise answer nil."
	^self method encoderClass interpretJumpIn: self! !

!InstructionStream methodsFor: 'decoding' stamp: 'eem 5/18/2014 14:21'!
interpretJumpIfCond
	"If the instruction at pc is a conditional jump, interpret it, advancing the pc,
	 and answering the jump distance. Otherwise answer nil."
	^self method encoderClass interpretJumpIfCondIn: self! !

!InstructionStream methodsFor: 'decoding' stamp: 'eem 8/11/2014 13:22'!
skipCallPrimitive
	"If the receiver's method starts with a callPrimitive: bytecode, skip it."
	| method encoderClass callPrimitiveCode |
	method := self method.
	encoderClass := method  encoderClass.
	callPrimitiveCode := encoderClass callPrimitiveCode.
	(method byteAt: pc) = callPrimitiveCode ifTrue:
		[pc := pc + (encoderClass bytecodeSize: callPrimitiveCode)]! !

!InstructionStream methodsFor: 'scanning' stamp: 'eem 5/17/2014 17:23'!
followingPc
	"Answer the pc of the following bytecode."
	| method |
	method := self method.
	^pc + (method encoderClass bytecodeSize: (method at: pc))! !

!InstructionStream methodsFor: 'scanning' stamp: 'eem 5/17/2014 17:22'!
scanFor: scanBlock
	"Check all bytecode instructions with scanBlock, answer true if scanBlock answers true.
	This can be used to, e.g., check whether a method contains 'push closure' bytecodes like this:
	aMethod scanFor: [ :b | b = 143 ]"

	| method encoderClass end byte |
	method := self method.
	end := method endPC.
	encoderClass := method encoderClass.
	[pc <= end] whileTrue: 
		[(scanBlock value: (byte := method at: pc)) ifTrue:
			[^true].
		 pc := pc + (encoderClass bytecodeSize: byte)].
	^false! !

!InstructionStream methodsFor: 'scanning' stamp: 'eem 8/5/2014 21:04'!
selectorToSendOrSelf
	"If this instruction is a send, answer the selector, otherwise answer self."

	| method |
	method := self method.
	^method encoderClass selectorToSendOrItselfFor: self in: method at: pc! !

!InstructionStream methodsFor: 'scanning' stamp: 'eem 2/6/2013 10:35'!
skipBackBeforeJump
	"Assuming that the receiver is positioned just after a jump, skip back one or two bytes,
	 depending on the size of the previous jump instruction."
	| scanner client prevPc |
	scanner := InstructionStream on: self method.
	client := InstructionClient new.
	[scanner pc < pc] whileTrue:
		[prevPc := scanner pc.
		 scanner interpretNextInstructionFor: client].
	scanner pc: prevPc.
	(scanner willJumpIfTrue or: [scanner willJumpIfFalse]) ifFalse:
		[self error: 'Where''s the jump??'].
	self jump: prevPc - pc! !

!InstructionStream methodsFor: 'private' stamp: 'eem 5/17/2014 17:21'!
nextPc: currentByte
	"Answer the pc of the next bytecode following the current one, given the current bytecode.."

	^pc + (self method encoderClass bytecodeSize: currentByte)! !


!ContextPart methodsFor: 'accessing' stamp: 'eem 9/17/2014 17:13'!
basicSize
	"Primitive. Answer the number of indexable variables in the receiver. 
	This value is the same as the largest legal subscript. Essential. Do not 
	override in any subclass. See Object documentation whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 212>
	"The number of indexable fields of fixed-length objects is 0"
	^self primitiveFailed! !

!ContextPart methodsFor: 'accessing' stamp: 'eem 9/17/2014 17:13'!
size
	"Primitive. Answer the number of indexable variables in the receiver. 
	This value is the same as the largest legal subscript. Essential. See Object 
	documentation whatIsAPrimitive.  Override the default primitive to give latitude to
	 the VM in context management."

	<primitive: 212>
	"The number of indexable fields of fixed-length objects is 0"
	^self primitiveFailed! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 5/12/2009 11:58'!
popIntoLiteralVariable: value 
	"Simulate the action of bytecode that removes the top of the stack and 
	stores it into a literal variable of my method."

	self object: value instVarAt: ValueIndex put: self pop! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 5/12/2009 12:00'!
popIntoReceiverVariable: offset 
	"Simulate the action of bytecode that removes the top of the stack and 
	stores it into an instance variable of my receiver."

	self object: self receiver instVarAt: offset + 1 put: self pop! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 5/12/2009 13:07'!
popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Simulate the action of bytecode that removes the top of the stack and  stores
	 it into an offset in one of my local variables being used as a remote temp vector."

	self object: (self at: tempVectorIndex + 1) basicAt: remoteTempIndex + 1 put: self pop! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 5/12/2009 11:59'!
pushLiteralVariable: value 
	"Simulate the action of bytecode that pushes the contents of the literal 
	variable whose index is the argument, index, on the top of the stack."

	self push: (self object: value instVarAt: ValueIndex)! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 5/12/2009 12:00'!
pushReceiverVariable: offset 
	"Simulate the action of bytecode that pushes the contents of the receiver's 
	instance variable whose index is the argument, index, on the top of the 
	stack."

	self push: (self object: self receiver instVarAt: offset + 1)! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 5/12/2009 13:07'!
pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Simulate the action of bytecode that pushes the value at remoteTempIndex
	 in one of my local variables being used as a remote temp vector."
	self push: (self object: (self at: tempVectorIndex + 1) basicAt: remoteTempIndex + 1)! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 3/22/2013 14:43'!
send: selector super: superFlag numArgs: numArgs
	"Simulate the action of bytecodes that send a message with selector, 
	selector. The argument, superFlag, tells whether the receiver of the 
	message was specified with 'super' in the source method. The arguments 
	of the message are found in the top numArgs locations on the stack and 
	the receiver just below them."

	| receiver arguments |
	arguments := Array new: numArgs.
	numArgs to: 1 by: -1 do: [ :i | arguments at: i put: self pop].
	receiver := self pop.
	QuickStep == self ifTrue:
		[QuickStep := nil.
		^self quickSend: selector to: receiver with: arguments super: superFlag].
	^self send: selector to: receiver with: arguments super: superFlag! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 5/12/2009 11:58'!
storeIntoLiteralVariable: value 
	"Simulate the action of bytecode that stores the top of the stack into a 
	literal variable of my method."

	self object: value instVarAt: ValueIndex put: self top! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 5/12/2009 12:00'!
storeIntoReceiverVariable: offset 
	"Simulate the action of bytecode that stores the top of the stack into an 
	instance variable of my receiver."

	self object: self receiver instVarAt: offset + 1 put: self top! !

!ContextPart methodsFor: 'instruction decoding' stamp: 'eem 5/12/2009 13:05'!
storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex
	"Simulate the action of bytecode that stores the top of the stack at
	 an offset in one of my local variables being used as a remote temp vector."

	self object: (self at: tempVectorIndex + 1) basicAt: remoteTempIndex + 1 put: self top! !

!ContextPart methodsFor: 'debugger access' stamp: 'eem 11/27/2014 13:11'!
methodClass 
	"Answer the class in which the receiver's method was found."
	
	^self method methodClass ifNil: [self objectClass: self receiver].! !

!ContextPart methodsFor: 'debugger access' stamp: 'eem 11/27/2014 13:07'!
print: anObject on: aStream
	"Safely print anObject in the face of direct ProtoObject subclasses."
	| objClass title |
	objClass := self objectClass: anObject.
	(objClass canUnderstand: #printOn:) ifTrue:
		[^anObject printOn: aStream].
	title := objClass name.
	aStream
		nextPutAll: (title first isVowel ifTrue: ['an '] ifFalse: ['a ']);
		nextPutAll: title! !

!ContextPart methodsFor: 'controlling' stamp: 'eem 9/2/2014 14:14'!
quickSend: selector to: rcvr with: arguments super: superFlag
	"Send the given selector with arguments in an environment which closely resembles
	 the non-simulating environment, with an interjected unwind-protected block to catch
	 nonlocal returns.  Attention: don't get lost!!  This beautiful method is due to
	 Hans-Martin Mosner.  Eliot Miranda merely added the mirror primitive code."
	| oldSender contextToReturnTo result lookupClass |
	contextToReturnTo := self.
	lookupClass := superFlag
					ifTrue: [self method methodClassAssociation value superclass]
					ifFalse: [self objectClass: rcvr].
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
	^contextToReturnTo! !

!ContextPart methodsFor: 'system simulation' stamp: 'eem 5/13/2016 12:29'!
stepToSendOrReturn
	"Simulate the execution of bytecodes until either sending a message or 
	 returning a value to the receiver (that is, until switching contexts)."

	| ctxt |
	[self willReallySend or: [self willReturn or: [self willReallyStore]]] whileFalse:
		[ctxt := self step.
		 ctxt == self ifFalse:
			[self halt. 
			 "Caused by mustBeBoolean handling"
			 ^ctxt]]! !

!ContextPart methodsFor: 'query' stamp: 'ul 4/27/2011 22:57'!
copyTo: aContext
	"Copy self and my sender chain down to, but not including, aContext.  End of copied chain will have nil sender."

	| copy |
	self == aContext ifTrue: [ ^nil ].
	copy := self copy.
	sender ifNotNil: [ copy privSender: (sender copyTo: aContext) ].
	^copy! !

!ContextPart methodsFor: 'mirror primitives' stamp: 'dtl 12/11/2011 19:39'!
object: anObject perform: selector withArguments: argArray inClass: lookupClass
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
	self primitiveFailed! !


!MethodContext methodsFor: 'closure support' stamp: 'eem 1/7/2016 11:48'!
isClosureContext
	^closureOrNil ~~ nil! !

!MethodContext methodsFor: 'private' stamp: 'eem 4/25/2012 10:48'!
aboutToReturn: result through: firstUnwindContext 
	"Called from VM when an unwindBlock is found between self and its home.
	 Return to home's sender, executing unwind blocks on the way."

	self methodReturnContext return: result through: firstUnwindContext! !


!InstructionStream reorganize!
('testing' willBlockReturn willJump willJumpIfFalse willJumpIfTrue willJustPop willReallySend willReturn willSend willStore willStorePop)
('decoding' atEnd interpret interpretJump interpretJumpIfCond interpretNextInstructionFor: skipCallPrimitive)
('scanning' addSelectorTo: firstByte followingByte followingBytecode followingPc fourthByte method nextByte nextInstruction pc peekInstruction previousPc scanFor: secondByte selectorToSendOrSelf skipBackBeforeJump thirdByte)
('private' interpretExtension:in:for: method:pc: nextPc: pc: unusedBytecode:at:)
('debugger access' abstractPC debuggerMap)
('Squeak Compatibility')
('decoding - private - v3 plus closures' interpretNextV3ClosuresInstructionFor: interpretNextV3InstructionFor: interpretV3ClosuresExtension:in:for: interpretV3Extension:in:for: interpretV3Jump interpretV3JumpIfCond)
!

