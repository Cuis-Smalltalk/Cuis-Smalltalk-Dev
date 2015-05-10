'From Cuis 4.2 of 25 July 2013 [latest update: #2280] on 1 May 2015 at 9:59:12.262014 pm'!
!classDefinition: #Decompiler category: #'Compiler-Kernel'!
InstructionStream subclass: #Decompiler
	instanceVariableNames: 'constructor method instVars tempVars constTable stack statements lastPc exit caseExits lastJumpPc lastReturnPc limit hasValue blockStackBase numLocalTemps blockStartsToTempVars tempVarCount lastJumpIfPcStack '
	classVariableNames: 'ArgumentFlag CascadeFlag CaseFlag IfNilFlag '
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!InstructionStream class methodsFor: 'cached class state' stamp: 'jmv 5/1/2015 21:52'!
initSpecialConstants
	"Initialize an array of special constants returned by single-bytecode returns."

	SpecialConstants _ 
		(Array with: true with: false with: nil)
			, (Array with: -1 with: 0 with: 1 with: 2)	
	"
	InstructionStream initSpecialConstants
	"
! !

!InstructionStream class methodsFor: 'cached class state' stamp: 'jmv 5/1/2015 21:54'!
releaseClassCachedState

	SpecialConstants _ nil! !


!ContextPart class methodsFor: 'cached class state' stamp: 'jmv 5/1/2015 21:59'!
releaseClassCachedState

	PrimitiveFailToken _ nil.
	QuickStep _ nil! !


!InstructionStream methodsFor: 'decoding' stamp: 'jmv 5/1/2015 21:54'!
interpretNextInstructionFor: client 
	"Send to the argument, client, a message that specifies the type of the 
	next instruction."

	| byte type offset method |
	SpecialConstants ifNil: [
		InstructionStream initSpecialConstants ].
	method := self method.  
	byte := method at: pc.
	type := byte // 16.  
	offset := byte \\ 16.  
	pc := pc+1.
	"We do an inline binary search on each of the possible 16 values of type:
	The old, cleaner but slowe code is retained as a comment below"
	type < 8
	ifTrue: [type < 4
				ifTrue: [type < 2
						ifTrue: [type < 1
								ifTrue: ["type = 0"
									^ client pushReceiverVariable: offset]
								ifFalse: ["type = 1"
									^ client pushTemporaryVariable: offset]]
						ifFalse: [type < 3
								ifTrue: ["type = 2"
									^ client
										pushConstant: (method literalAt: offset + 1)]
								ifFalse: ["type = 3"
									^ client
										pushConstant: (method literalAt: offset + 17)]]]
				ifFalse: [type < 6
						ifTrue: [type < 5
								ifTrue: ["type = 4"
									^ client
										pushLiteralVariable: (method literalAt: offset + 1)]
								ifFalse: ["type = 5"
									^ client
										pushLiteralVariable: (method literalAt: offset + 17)]]
						ifFalse: [type < 7
								ifTrue: ["type = 6"
									offset < 8
										ifTrue: [^ client popIntoReceiverVariable: offset]
										ifFalse: [^ client popIntoTemporaryVariable: offset - 8]]
								ifFalse: ["type = 7"
									offset = 0
										ifTrue: [^ client pushReceiver].
									offset < 8
										ifTrue: [^ client
												pushConstant: (SpecialConstants at: offset)].
									offset = 8
										ifTrue: [^ client methodReturnReceiver].
									offset < 12
										ifTrue: [^ client
												methodReturnConstant: (SpecialConstants at: offset - 8)].
									offset = 12
										ifTrue: [^ client methodReturnTop].
									offset = 13
										ifTrue: [^ client blockReturnTop].
									offset > 13
										ifTrue: [^ self error: 'unusedBytecode']]]]]
		ifFalse: [type < 12
				ifTrue: [type < 10
						ifTrue: [type < 9
								ifTrue: ["type = 8"
									^ self
										interpretExtension: offset
										in: method
										for: client]
								ifFalse: ["type = 9 (short jumps)"
									offset < 8
										ifTrue: [^ client jump: offset + 1].
									^ client jump: offset - 8 + 1 if: false]]
						ifFalse: [type < 11
								ifTrue: ["type = 10 (long jumps)"
									byte := method at: pc.
									pc := pc + 1.
									offset < 8
										ifTrue: [^ client jump: offset - 4 * 256 + byte].
									^ client jump: (offset bitAnd: 3)
											* 256 + byte if: offset < 12]
								ifFalse: ["type = 11"
									^ client
										send: (Smalltalk specialSelectorAt: offset + 1)
										super: false
										numArgs: (Smalltalk specialNargsAt: offset + 1)]]]
				ifFalse: [type = 12
						ifTrue: [^ client
								send: (Smalltalk specialSelectorAt: offset + 17)
								super: false
								numArgs: (Smalltalk specialNargsAt: offset + 17)]
						ifFalse: ["type = 13, 14 or 15"
							^ client
								send: (method literalAt: offset + 1)
								super: false
								numArgs: type - 13]]].


"    old code 
	type=0 ifTrue: [^client pushReceiverVariable: offset].
	type=1 ifTrue: [^client pushTemporaryVariable: offset].
	type=2 ifTrue: [^client pushConstant: (method literalAt: offset+1)].
	type=3 ifTrue: [^client pushConstant: (method literalAt: offset+17)].
	type=4 ifTrue: [^client pushLiteralVariable: (method literalAt: offset+1)].
	type=5 ifTrue: [^client pushLiteralVariable: (method literalAt: offset+17)].
	type=6 
		ifTrue: [offset<8
					ifTrue: [^client popIntoReceiverVariable: offset]
					ifFalse: [^client popIntoTemporaryVariable: offset-8]].
	type=7
		ifTrue: [offset=0 ifTrue: [^client pushReceiver].
				offset<8 ifTrue: [^client pushConstant: (SpecialConstants at: offset)].
				offset=8 ifTrue: [^client methodReturnReceiver].
				offset<12 ifTrue: [^client methodReturnConstant: 
												(SpecialConstants at: offset-8)].
				offset=12 ifTrue: [^client methodReturnTop].
				offset=13 ifTrue: [^client blockReturnTop].
				offset>13 ifTrue: [^self error: 'unusedBytecode']].
	type=8 ifTrue: [^self interpretExtension: offset in: method for: client].
	type=9
		ifTrue:  short jumps
			[offset<8 ifTrue: [^client jump: offset+1].
			^client jump: offset-8+1 if: false].
	type=10 
		ifTrue:  long jumps
			[byte:= method at: pc.  pc:= pc+1.
			offset<8 ifTrue: [^client jump: offset-4*256 + byte].
			^client jump: (offset bitAnd: 3)*256 + byte if: offset<12].
	type=11 
		ifTrue: 
			[^client 
				send: (Smalltalk specialSelectorAt: offset+1) 
				super: false
				numArgs: (Smalltalk specialNargsAt: offset+1)].
	type=12 
		ifTrue: 
			[^client 
				send: (Smalltalk specialSelectorAt: offset+17) 
				super: false
				numArgs: (Smalltalk specialNargsAt: offset+17)].
	type>12
		ifTrue: 
			[^client send: (method literalAt: offset+1) 
					super: false
					numArgs: type-13]"! !


!ContextPart methodsFor: 'controlling' stamp: 'jmv 5/1/2015 21:57'!
send: selector to: rcvr with: args super: superFlag 
	"Simulate the action of sending a message with selector, selector, and 
	arguments, args, to receiver. The argument, superFlag, tells whether the 
	receiver of the message was specified with 'super' in the source method."

	| class meth val |
	class _ superFlag
			ifTrue: [(self method literalAt: self method numLiterals) value superclass]
			ifFalse: [rcvr class].
	meth _ class lookupSelector: selector.
	meth
		ifNil: [^ self send: #doesNotUnderstand:
					to: rcvr
					with: (Array with: (Message selector: selector arguments: args))
					super: superFlag].
	val _ self tryPrimitiveFor: meth
			receiver: rcvr
			args: args.
	val == ContextPart primitiveFailToken ifFalse: [^ val].
	(selector == #doesNotUnderstand: and: [class == ProtoObject]) ifTrue: [
		^ self error: 'Simulated message ' , (args at: 1) selector
						, ' not understood'].
	^ self activateMethod: meth
		withArgs: args
		receiver: rcvr
		class: class! !

!ContextPart methodsFor: 'private' stamp: 'jmv 5/1/2015 21:57'!
doPrimitive: primitiveIndex method: meth receiver: receiver args: arguments 
	"Simulate a primitive method whose index is primitiveIndex.  The
	 simulated receiver and arguments are given as arguments to this message.
	 Any primitive which provikes execution needs to be intercepted and simulated
	 to avoid execution running away."

	| value |
	<primitive: 19> "Simulation guard"
	"If successful, push result and return resuming context,
		else ^ PrimitiveFailToken"
	(primitiveIndex = 19) ifTrue:[
		Debugger 
			openContext: self
			label:'Code simulation error'
			contents: nil].

	"ContextPart>>blockCopy:; simulated to get startpc right"
	(primitiveIndex = 80 and: [receiver isKindOf: ContextPart]) 
		ifTrue: [^self push: ((BlockContext newForMethod: receiver method)
						home: receiver home
						startpc: pc + 2
						nargs: (arguments at: 1))].
	(primitiveIndex = 81 and: [receiver isMemberOf: BlockContext]) "BlockContext>>value[:value:...]"
		ifTrue: [^receiver pushArgs: arguments from: self].
	(primitiveIndex = 82 and: [receiver isMemberOf: BlockContext]) "BlockContext>>valueWithArguments:"
		ifTrue: [^receiver pushArgs: arguments first from: self].
	primitiveIndex = 83 "afr 9/11/1998 19:50" "Object>>perform:[with:...]"
		ifTrue: [^self send: arguments first to: receiver
					with: arguments allButFirst
					super: false].
	primitiveIndex = 84 "afr 9/11/1998 19:50" "Object>>perform:withArguments:"
		ifTrue: [^self send: arguments first to: receiver
					with: (arguments at: 2)
					super: false].
	primitiveIndex = 188 ifTrue: [
		arguments size = 2 ifTrue: [ "eem 5/27/2008 11:10 Object>>withArgs:executeMethod:"
			^MethodContext
				sender: self
				receiver: receiver
				method: (arguments at: 2)
				arguments: (arguments at: 1) ].
		arguments size = 3 ifTrue: [ "CompiledMethod class >> #receiver:withArguments:executeMethod:"
			^MethodContext
				sender: self
				receiver: (arguments at: 1)
				method: (arguments at: 3)
				arguments: (arguments at: 2) ] ].
	primitiveIndex = 189 ifTrue: [ "Object >> (#with:)*executeMethod"
		^MethodContext
			sender: self
			receiver: receiver
			method: arguments last
			arguments: arguments allButLast ].

	"Closure primitives"
	(primitiveIndex = 200 and: [receiver == self]) ifTrue:
		"ContextPart>>closureCopy:copiedValues:; simulated to get startpc right"
		[^self push: (BlockClosure
						outerContext: receiver
						startpc: pc + 2
						numArgs: arguments first
						copiedValues: arguments last)].
	((primitiveIndex between: 201 and: 205)			 "BlockClosure>>value[:value:...]"
	or: [primitiveIndex between: 221 and: 222]) ifTrue: "BlockClosure>>valueNoContextSwitch[:]"
		[^receiver simulateValueWithArguments: arguments caller: self].
	primitiveIndex = 206 ifTrue:						"BlockClosure>>valueWithArguments:"
		[^receiver simulateValueWithArguments: arguments first caller: self].

	primitiveIndex = 120 ifTrue:[ "FFI method"
		value := meth literals first tryInvokeWithArguments: arguments.
	] ifFalse:[
		arguments size > 6 ifTrue: [^ContextPart primitiveFailToken].
		value := primitiveIndex = 117 "named primitives"
				ifTrue:[self tryNamedPrimitiveIn: meth for: receiver withArgs: arguments]
				ifFalse:[receiver tryPrimitive: primitiveIndex withArgs: arguments].
	].
	^value == ContextPart primitiveFailToken
		ifTrue: [ContextPart primitiveFailToken]
		ifFalse: [self push: value]! !

!ContextPart methodsFor: 'private' stamp: 'jmv 5/1/2015 21:57'!
tryNamedPrimitiveIn: aCompiledMethod for: aReceiver withArgs: arguments
	"Hack. Attempt to execute the named primitive from the given compiled method"
	| selector theMethod spec |
	arguments size > 8 ifTrue:[^ContextPart primitiveFailToken].
	selector _ #(
		tryNamedPrimitive 
		tryNamedPrimitive: 
		tryNamedPrimitive:with: 
		tryNamedPrimitive:with:with: 
		tryNamedPrimitive:with:with:with:
		tryNamedPrimitive:with:with:with:with:
		tryNamedPrimitive:with:with:with:with:with:
		tryNamedPrimitive:with:with:with:with:with:with:
		tryNamedPrimitive:with:with:with:with:with:with:with:) at: arguments size+1.
	theMethod _ aReceiver class lookupSelector: selector.
	theMethod ifNil: [ ^ContextPart primitiveFailToken].
	spec _ theMethod literalAt: 1.
	spec replaceFrom: 1 to: spec size with: (aCompiledMethod literalAt: 1) startingAt: 1.
	theMethod flushCache.
	selector flushCache.
	^aReceiver perform: selector withArguments: arguments! !

!ContextPart methodsFor: 'private' stamp: 'jmv 5/1/2015 21:57'!
tryPrimitiveFor: method receiver: receiver args: arguments 
	"If this method has a primitive index, then run the primitive and return its result.
	Otherwise (and also if the primitive fails) return PrimitiveFailToken,
	as an indication that the method should be activated and run as bytecodes."
	| primIndex |
	(primIndex _ method primitive) = 0 ifTrue: [^ ContextPart primitiveFailToken].
	^ self doPrimitive: primIndex method: method receiver: receiver args: arguments! !


!ContextPart class methodsFor: 'simulation' stamp: 'jmv 5/1/2015 21:56'!
primitiveFailToken

	"A unique object to be returned when a primitive fails during simulation"
	PrimitiveFailToken ifNil: [ PrimitiveFailToken _ Object new ].
	^ PrimitiveFailToken! !


!Decompiler methodsFor: 'control' stamp: 'jmv 5/1/2015 21:48'!
checkForBlockCopy: receiver
	"We just saw a blockCopy: message. Check for a following block."

	| savePc jump args argPos block |
	receiver == constructor codeThisContext ifFalse: [^false].
	savePc := pc.
	(jump := self interpretJump) ifNil:
		[pc := savePc.  ^false].
	self sawBlueBookBlock.
	"Definitely a block"
	jump := jump + pc.
	argPos := statements size.
	[self willStorePop]
		whileTrue:
			[stack addLast: #ArgumentFlag.  "Flag for doStore:"
			self interpretNextInstructionFor: self].
	args := Array new: statements size - argPos.
	1 to: args size do:  "Retrieve args"
		[:i | args at: i put: statements removeLast.
		(args at: i) scope: -1  "flag args as block temps"].
	block := self blockTo: jump.
	stack addLast: (constructor codeArguments: args block: block).
	^true! !

!Decompiler methodsFor: 'control' stamp: 'jmv 5/1/2015 21:46'!
statementsForCaseTo: end
	"Decompile the method from pc up to end and return an array of
	expressions. If at run time this block will leave a value on the stack,
	set hasValue to true. If the block ends with a jump or return, set exit
	to the destination of the jump, or the end of the method; otherwise, set
	exit = end. Leave pc = end.
	Note that stack initially contains a #CaseFlag which will be removed by
	a subsequent Pop instruction, so adjust the StackPos accordingly."

	| blockPos stackPos |
	blockPos := statements size.
	stackPos := stack size - 1. "Adjust for #CaseFlag"
	[pc < end]
		whileTrue:
			[lastPc := pc.  limit := end.  "for performs"
			self interpretNextInstructionFor: self].
	"If there is an additional item on the stack, it will be the value
	of this block."
	(hasValue := stack size > stackPos)
		ifTrue:
			[stack last == #CaseFlag
				ifFalse: [ statements addLast: stack removeLast] ].
	lastJumpPc = lastPc ifFalse: [exit := pc].
	caseExits add: exit.
	^self popTo: blockPos! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'jmv 5/1/2015 21:47'!
case: dist
	"statements = keyStmts #CascadeFlag keyValueBlock ... keyStmts"

	| nextCase thenJump stmtStream elements b node cases otherBlock myExits |
	nextCase := pc + dist.

	"Now add #CascadeFlag & keyValueBlock to statements"
	statements addLast: stack removeLast.
	stack addLast: #CaseFlag. "set for next pop"
	statements addLast: (self blockForCaseTo: nextCase).

	stack last == #CaseFlag
		ifTrue: "Last case"
			["ensure jump is within block (in case thenExpr returns weirdly I guess)"
			stack removeLast. "get rid of #CaseFlag"
			stmtStream := ReadStream on: (self popTo: stack removeLast).
			
			elements := OrderedCollection new.
			b := OrderedCollection new.
			[stmtStream atEnd] whileFalse:
				[(node := stmtStream next) == #CascadeFlag
					ifTrue:
						[elements addLast: (constructor
							codeMessage: (constructor codeBlock: b returns: false)
							selector: (constructor codeSelector: #-> code: #macro)
							arguments: (Array with: stmtStream next)).
						 b := OrderedCollection new]
					ifFalse: [b addLast: node]].
			b size > 0 ifTrue: [self error: 'Bad cases'].
			cases := constructor codeBrace: elements.
			
			"try find the end of the case"
			myExits := caseExits removeLast: elements size.
			myExits := myExits reject: [ :e | e isNil or: [ e < 0 or: [ e > method endPC ] ] ].
			thenJump := myExits isEmpty
							ifTrue: [ nextCase ]
							ifFalse: [ myExits max ].
			
			otherBlock := self blockTo: thenJump.
			stack addLast:
				(constructor
					codeMessage: stack removeLast
					selector: (constructor codeSelector: #caseOf:otherwise: code: #macro)
					arguments: (Array with: cases with: otherBlock))].! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'jmv 5/1/2015 21:47'!
doDup

	stack last == #CascadeFlag
		ifFalse: [
			"Save position and mark cascade"
			stack addLast: statements size.
			stack addLast: #CascadeFlag].
	stack addLast: #CascadeFlag! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'jmv 5/1/2015 21:46'!
doPop

	stack isEmpty ifTrue:
		["Ignore pop in first leg of ifNil for value"
		^ self].
	stack last == #CaseFlag
		ifTrue: [stack removeLast]
		ifFalse: [statements addLast: stack removeLast].! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'jmv 5/1/2015 21:48'!
doStore: stackOrBlock
	"Only called internally, not from InstructionStream. StackOrBlock is stack
	for store, statements for storePop."

	| var expr |
	var := stack removeLast.
	expr := stack removeLast.
	stackOrBlock addLast: (expr == #ArgumentFlag
		ifTrue: [var]
		ifFalse: [constructor codeAssignTo: var value: expr])! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'jmv 5/1/2015 21:47'!
jump: dist if: condition

	| savePc sign elsePc elseStart end cond ifExpr thenBlock elseBlock
	  thenJump elseJump condHasValue isIfNil saveStack blockBody blockArgs |
	lastJumpIfPcStack addLast: lastPc.
	stack last == #CascadeFlag ifTrue: [^ [self case: dist] ensure: [lastJumpIfPcStack removeLast]].
	elsePc := lastPc.
	elseStart := pc + dist.
	end := limit.
	"Check for bfp-jmp to invert condition.
	Don't be fooled by a loop with a null body."
	sign := condition.
	savePc := pc.
	self interpretJump ifNotNil:
		[:elseDist|
		 (elseDist >= 0 and: [elseStart = pc]) ifTrue:
			 [sign := sign not.  elseStart := pc + elseDist]].
	pc := savePc.
	ifExpr := stack removeLast.
	(isIfNil := stack size > 0 and: [stack last == #IfNilFlag]) ifTrue:
		[stack removeLast].
	saveStack := stack.
	stack := OrderedCollection new.
	thenBlock := self blockTo: elseStart.
	condHasValue := hasValue or: [isIfNil].
	"ensure jump is within block (in case thenExpr returns)"
	thenJump := exit <= end ifTrue: [exit] ifFalse: [elseStart].
	"if jump goes back, then it's a loop"
	thenJump < elseStart
		ifTrue:
			["Must be a while loop...
			  thenJump will jump to the beginning of the while expr.  In the case of while's
			  with a block in the condition, the while expr should include more than just
			  the last expression: find all the statements needed by re-decompiling."
			stack := saveStack.
			pc := thenJump.
			blockBody := self statementsTo: elsePc.
			"discard unwanted statements from block"
			blockBody size - 1 timesRepeat: [statements removeLast].
			blockArgs := thenBlock statements = constructor codeEmptyBlock statements
							ifTrue: [#()]
							ifFalse: [{ thenBlock }].
			statements addLast:
				(constructor
					codeMessage: (constructor codeBlock: blockBody returns: false)
					selector: (constructor
								codeSelector: (blockArgs isEmpty
												ifTrue:
													[sign
														ifTrue: [#whileFalse]
														ifFalse: [#whileTrue]]
												ifFalse:
													[sign
														ifTrue: [#whileFalse:]
														ifFalse: [#whileTrue:]])
								code: #macro)
					arguments: blockArgs).
			pc := elseStart.
			self convertToDoLoop]
		ifFalse:
			["Must be a conditional..."
			elseBlock := self blockTo: thenJump.
			elseJump := exit.
			"if elseJump is backwards, it is not part of the elseExpr"
			elseJump < elsePc ifTrue:
				[pc := lastPc].
			cond := isIfNil
						ifTrue:
							[constructor
								codeMessage: ifExpr ifNilReceiver
								selector: (constructor
											codeSelector: (sign ifTrue: [#ifNotNil:] ifFalse: [#ifNil:])
											code: #macro)
								arguments: (Array with: thenBlock)]
						ifFalse:
							[constructor
								codeMessage: ifExpr
								selector: (constructor codeSelector: #ifTrue:ifFalse: code: #macro)
								arguments:	(sign
												ifTrue: [{elseBlock. thenBlock}]
												ifFalse: [{thenBlock. elseBlock}])].
			stack := saveStack.
			condHasValue
				ifTrue: [stack addLast: cond]
				ifFalse: [statements addLast: cond]].
	lastJumpIfPcStack removeLast.! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'jmv 5/1/2015 21:48'!
send: selector super: superFlag numArgs: numArgs

	| args rcvr selNode msgNode messages |
	args := Array new: numArgs.
	(numArgs to: 1 by: -1) do:
		[:i | args at: i put: stack removeLast].
	rcvr := stack removeLast.
	superFlag ifTrue: [rcvr := constructor codeSuper].
	((#(blockCopy: closureCopy:copiedValues:) includes: selector)
	  and: [self checkForBlock: rcvr selector: selector arguments: args]) ifFalse:
		[selNode := constructor codeAnySelector: selector.
		rcvr == #CascadeFlag
			ifTrue:
				["May actually be a cascade or an ifNil: for value."
				self willJumpIfFalse
					ifTrue: "= generated by a case macro"
						[selector == #= ifTrue:
							[" = signals a case statement..."
							statements addLast: args first.
							stack addLast: rcvr. "restore #CascadeFlag"
							^ self].
						selector == #== ifTrue:
							[" == signals an ifNil: for value..."
							stack removeLast; removeLast.
							rcvr := stack removeLast.
							stack addLast: #IfNilFlag;
								addLast: (constructor
									codeMessage: rcvr
									selector: selNode
									arguments: args).
							^ self]]
					ifFalse:
						[(self willJumpIfTrue and: [selector == #==]) ifTrue:
							[" == signals an ifNotNil: for value..."
							stack removeLast; removeLast.
							rcvr := stack removeLast.
							stack addLast: #IfNilFlag;
								addLast: (constructor
									codeMessage: rcvr
									selector: selNode
									arguments: args).
							^ self]].
				msgNode := constructor
								codeCascadedMessage: selNode
								arguments: args.
				stack last == #CascadeFlag ifFalse:
					["Last message of a cascade"
					statements addLast: msgNode.
					messages := self popTo: stack removeLast.  "Depth saved by first dup"
					msgNode := constructor
									codeCascade: stack removeLast
									messages: messages]]
			ifFalse:
				[msgNode := constructor
							codeMessage: rcvr
							selector: selNode
							arguments: args].
		stack addLast: msgNode]! !

!methodRemoval: Decompiler class #initialize!
Decompiler class removeSelector: #initialize!
Decompiler initialize!
!classDefinition: #Decompiler category: #'Compiler-Kernel'!
InstructionStream subclass: #Decompiler
	instanceVariableNames: 'constructor method instVars tempVars constTable stack statements lastPc exit caseExits lastJumpPc lastReturnPc limit hasValue blockStackBase numLocalTemps blockStartsToTempVars tempVarCount lastJumpIfPcStack'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Compiler-Kernel'!
!methodRemoval: ContextPart class #initialize!
ContextPart class removeSelector: #initialize!
ContextPart initialize!

!ContextPart class reorganize!
('examples' tallyInstructions: tallyMethods: trace: trace:on: trace:onFileNamed:)
('simulation' primitiveFailToken runSimulated:)
('instance creation' basicNew: initializedInstance new new: newForMethod:)
('private' carefullyPrint:on: isContextClass)
('special context creation' contextEnsure: contextOn:do: theReturnMethod)
('cached class state' releaseClassCachedState)
!

!methodRemoval: InstructionStream class #initialize!
InstructionStream class removeSelector: #initialize!
InstructionStream initialize!

!InstructionStream class reorganize!
('class initialization')
('instance creation' on:)
('compiling' instVarNamesAndOffsetsDo: isContextClass)
('cached class state' initSpecialConstants releaseClassCachedState)
!

