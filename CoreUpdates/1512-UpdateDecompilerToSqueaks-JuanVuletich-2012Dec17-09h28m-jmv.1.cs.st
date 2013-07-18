'From Cuis 4.1 of 12 December 2012 [latest update: #1511] on 17 December 2012 at 9:56:32 am'!
!classDefinition: #Decompiler category: #'Compiler-Kernel'!
InstructionStream subclass: #Decompiler
	instanceVariableNames: 'constructor method instVars tempVars constTable stack statements lastPc exit caseExits lastJumpPc lastReturnPc limit hasValue blockStackBase numLocalTemps blockStartsToTempVars tempVarCount lastJumpIfPcStack '
	classVariableNames: 'ArgumentFlag CascadeFlag CaseFlag IfNilFlag '
	poolDictionaries: ''
	category: 'Compiler-Kernel'!

!Decompiler commentStamp: 'nice 2/3/2011 22:54' prior: 0!
I decompile a method in three phases:
	Reverser: postfix byte codes -> prefix symbolic codes (nodes and atoms)
	Parser: prefix symbolic codes -> node tree (same as the compiler)
	Printer: node tree -> text (done by the nodes)
	

instance vars:

	constructor <DecompilerConstructor> an auxiliary knowing how to generate Abstract Syntax Tree (node tree)
	method <CompiledMethod> the method being decompiled
	instVars <Array of: String> the instance variables of the class implementing method
	tempVars <String | (OrderedCollection of: String)> hold the names of temporary variables (if known)
		NOTE: POLYMORPHISM WILL BE RESOLVED IN #initSymbols:
	constTable <Collection of: ParseNode> parse node associated with byte encoded constants (nil true false 0 1 -1 etc...)
	stack <OrderedCollection of: (ParseNode | String | Integer) > multipurpose...
	statements <OrderedCollection of: ParseNode> the statements of the method being decompiled 
	lastPc <Integer>
	exit <Integer>
	caseExits <OrderedCollection of: Integer> - stack of exit addresses that have been seen in the branches of caseOf:'s
	lastJumpPc <Integer>
	lastReturnPc <Integer>
	limit <Integer>
	hasValue <Boolean>
	blockStackBase <Integer>
	numLocaltemps <Integer | Symbol> - number of temps local to a block; also a flag indicating decompiling a block
	blockStartsToTempVars <Dictionary key: Integer value: (OrderedCollection of: String)>
	tempVarCount <Integer> number of temp vars used by the method
	lastJumpIfPcStack <OrderedCollection of: Integer> the value of program counter just before the last encountered conditional jumps!


!CompiledMethod methodsFor: 'decompiling' stamp: 'eem 9/26/2011 17:24'!
methodForDecompile
	"This is a hook to allow recursive methods like MwMethodWrapper to avoid infinite recursion."
	^self! !


!ClosureCompilerTest methodsFor: 'tests' stamp: 'nice 2/23/2011 21:39'!
testDecompiledDoitMethodTempNames
	"self new testDecompiledDoitMethodTempNames"
	"Test that a decompiled doit that has been copied with temps decompiles to the input"
	| removeComments |
	removeComments := [:n| n comment: nil].
	self closureCases do:
		[:source| | mns m mps mnps |
		"Need to compare an ungenerated tree with the generated method's methodNode
		 because generating code alters the tree when it introduces remote temp vectors."
		mns := #(first last) collect:
					[:ignored|
					source first isLetter
						ifTrue:
							[self class compilerClass new
								compile: source
								in: self class
								notifying: nil
								ifFail: [self error: 'compilation error']]
						ifFalse:
							[self class compilerClass new
								compileNoPattern: source
								in: self class
								context: nil
								notifying: nil
								ifFail: [self error: 'compilation error']]].
		m := (mns last generateWithTempNames).
		removeComments value: mns first.
		mns first nodesDo: removeComments.
		self assert: (mnps := mns first printString) = (mps := m methodNode printString)]! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'nice 2/3/2011 22:56'!
jump: dist
	| blockBody destPc nextPC |
	destPc := pc + dist.
	(lastJumpIfPcStack isEmpty or: [dist < 0 and: [destPc > lastJumpIfPcStack last]])
		ifTrue:
			["Rule: aBackward jump not crossing a Bfp/Btp must be a repeat"
			nextPC := pc.
			pc := destPc.
			blockBody := self statementsTo: lastPc.
			blockBody size timesRepeat: [statements removeLast].
			pc := nextPC.
			statements addLast:
				(constructor
					codeMessage: (constructor codeBlock: blockBody returns: false)
					selector: (constructor
								codeSelector: #repeat
								code: #macro)
					arguments: #()).
			]
		ifFalse:
			[exit := destPc.
			lastJumpPc := lastPc]! !

!Decompiler methodsFor: 'instruction decoding' stamp: 'eem 11/6/2012 12:53'!
jump: dist if: condition

	| savePc sign elsePc elseStart end cond ifExpr thenBlock elseBlock
	  thenJump elseJump condHasValue isIfNil saveStack blockBody blockArgs |
	lastJumpIfPcStack addLast: lastPc.
	stack last == CascadeFlag ifTrue: [^ [self case: dist] ensure: [lastJumpIfPcStack removeLast]].
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
	(isIfNil := stack size > 0 and: [stack last == IfNilFlag]) ifTrue:
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

!Decompiler methodsFor: 'public access' stamp: 'eem 9/27/2011 08:49'!
decompile: aSelector in: aClass 
	"See Decompiler|decompile:in:method:. The method is found by looking up 
	the message, aSelector, in the method dictionary of the class, aClass."

	^self
		decompile: aSelector
		in: aClass
		method: (aClass compiledMethodAt: aSelector) methodForDecompile! !

!Decompiler methodsFor: 'public access' stamp: 'nice 2/3/2011 22:53'!
decompile: aSelector in: aClass method: aMethod using: aConstructor

	| block node |
	constructor := aConstructor.
	method := aMethod.
	self initSymbols: aClass.  "create symbol tables"
	method isQuick
		ifTrue: [block := self quickMethod]
		ifFalse: 
			[stack := OrderedCollection new: method frameSize.
			lastJumpIfPcStack := OrderedCollection new.
			caseExits := OrderedCollection new.
			statements := OrderedCollection new: 20.
			numLocalTemps := 0.
			super method: method pc: method initialPC.
			"skip primitive error code store if necessary"
			(method primitive ~= 0 and: [self willStore]) ifTrue:
				[pc := pc + 2.
				 tempVars := tempVars asOrderedCollection].
			block := self blockTo: method endPC + 1.
			stack isEmpty ifFalse: [self error: 'stack not empty']].
	node := constructor
				codeMethod: aSelector
				block: block
				tempVars: tempVars
				primitive: method primitive
				class: aClass.
	method primitive > 0 ifTrue:
		[node removeAndRenameLastTempIfErrorCode].
	^node preen! !

!Decompiler methodsFor: 'public access' stamp: 'nice 4/22/2012 16:34'!
decompileBlock: aBlock 
	"Decompile aBlock, returning the result as a BlockNode.  
	Show temp names from source if available."
	"Decompiler new decompileBlock: [3 + 4]"
	| startpc end homeClass blockNode methodNode home |
	(home := aBlock home) ifNil: [^ nil].
	method := home method.
	(homeClass := home methodClass) == #unknown ifTrue: [^ nil].
	aBlock isClosure ifTrue:
		[(methodNode := method decompileWithTemps)
			ifNil: [^nil]
			ifNotNil: [methodNode nodesDo: [:node| node pc = aBlock startpc ifTrue: [^node]]].
		 ^self error: 'cannot find block node matching aBlock'].
	constructor := self constructorForMethod: aBlock method.
	
	self withTempNames: method methodNode tempNames.

	self initSymbols: homeClass.
	startpc := aBlock startpc.
	end := aBlock endPC.
	stack := OrderedCollection new: method frameSize.
	lastJumpIfPcStack := OrderedCollection new.
	caseExits := OrderedCollection new.
	statements := OrderedCollection new: 20.
	super method: method pc: startpc - 5.
	blockNode := self blockTo: end.
	stack isEmpty ifFalse: [self error: 'stack not empty'].
	^blockNode statements first! !

!Decompiler methodsFor: 'initialize-release' stamp: 'jmv 12/17/2012 09:36'!
initSymbols: aClass
	| argCount |
	constructor method: method class: aClass literals: method literals.
	constTable _ constructor codeConstants.
	instVars _ Array new: aClass instSize.
	tempVarCount _ method numTemps.
	argCount _ method numArgs.
	"(tempVars isNil
	 and: [method holdsTempNames]) ifTrue:
		[tempVars := method tempNamesString]."
	tempVars isString
		ifTrue:
			[blockStartsToTempVars _ self mapFromBlockStartsIn: method
											toTempVarsFrom: tempVars
											constructor: constructor.
			 tempVars _ blockStartsToTempVars at: method initialPC]
		ifFalse:
			[| namedTemps |
			namedTemps _ tempVars ifNil: [(1 to: tempVarCount) collect: [ :i | 
					(i <= argCount ifTrue: ['arg'] ifFalse: ['temp']), i printString]].
			tempVars _ (1 to: tempVarCount) collect:
							[:i | i <= namedTemps size
								ifTrue: [constructor codeTemp: i - 1 named: (namedTemps at: i)]
								ifFalse: [constructor codeTemp: i - 1]]].
	1 to: method numArgs do:
		[:i|
		(tempVars at: i)
			beMethodArg]! !

!classDefinition: #Decompiler category: #'Compiler-Kernel'!
InstructionStream subclass: #Decompiler
	instanceVariableNames: 'constructor method instVars tempVars constTable stack statements lastPc exit caseExits lastJumpPc lastReturnPc limit hasValue blockStackBase numLocalTemps blockStartsToTempVars tempVarCount lastJumpIfPcStack'
	classVariableNames: 'ArgumentFlag CascadeFlag CaseFlag IfNilFlag'
	poolDictionaries: ''
	category: 'Compiler-Kernel'!
