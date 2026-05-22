"
I am an abstract superclass for different bytecode set encoders.  Subclasses inherit the literal management of Encoder and encapsulate the mapping of opcodes to specific bytecodes.
"
Class {
	#name : 'BytecodeEncoder',
	#superclass : 'Encoder',
	#instVars : [
		'stream',
		'position',
		'rootNode',
		'blockExtentsToLocals'
	],
	#category : 'Compiler-Kernel',
	#package : 'Compiler',
	#tag : 'Kernel'
}

{ #category : 'compiled method support' }
BytecodeEncoder class >> bindingReadScanBlockFor: litVarIndex using: scanner [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the value of the binding with zero-relative index litVarIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	self subclassResponsibility
]

{ #category : 'compiled method support' }
BytecodeEncoder class >> bindingWriteScanBlockFor: litVarIndex using: scanner [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the value of the binding with zero-relative index litVarIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> bytecodeSize: aByte [
	self subclassResponsibility
]

{ #category : 'bytecode decoding' }
BytecodeEncoder class >> callPrimitiveCode [
	"Answer the call primitive bytecode, if it exists in the encoder's byetcode set, or nil if not."
	^nil
]

{ #category : 'testing' }
BytecodeEncoder class >> canBeSpecialLiteral: aLiteral [
	"This check can be used to prevent unnecessary use of #scanBlockOrNilForLiteral:. For performance, this method summarizes specializations from all known bytecode encoders. It is not meant to be refined per bytecode encoder."

	aLiteral isVariableBinding ifTrue: [^false]. "a common case; don't waste time analysing..."
	
	aLiteral isSymbol ifTrue: [^ Smalltalk specialSelectors identityIncludes: aLiteral].
	aLiteral isCharacter ifTrue: [^ aLiteral asInteger <= 65535].
	aLiteral isInteger ifTrue: [^ aLiteral between: -32768 and: 32767].

	aLiteral == true ifTrue: [^ true].
	aLiteral == false ifTrue: [^ true].
	aLiteral == nil ifTrue: [^ true].

	^ false
]

{ #category : 'bytecode decoding' }
BytecodeEncoder class >> createClosureCode [
	"Answer the create closure bytecode, if it exists in the encoder's byetcode set, or nil if not."
	^nil
]

{ #category : 'compiled method support' }
BytecodeEncoder class >> createClosureScanBlock [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for block closure creation bytecodes."
	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> extensionsAt: pc in: aCompiledMethod into: trinaryBlock [
	"If the bytecode at pc is an extension then evaluate aTrinaryBlock
	 with the values of extA and extB and number of extension *bytes*.
	 If the bytecode at pc is not an extension then evaluate with 0, 0, 0."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> extensionsFor: pc in: aCompiledMethod into: trinaryBlock [
	"If the bytecode at pc is an extension, or if the bytecode at pc is preceeded by extensions,
	 then evaluate aTrinaryBlock with the values of extA and extB and number of extension *bytes*.
	 If the bytecode at pc is neither an extension or extended then evaluate with 0, 0, 0."
 
	| prevPC |
	"If there is what appears to be an extension bytecode before this bytecode
	 then scan for the previous pc to confirm."
	(pc - 2 >= aCompiledMethod initialPC
	 and: [self isExtension: (aCompiledMethod at: pc - 2)]) ifTrue:
		[prevPC := aCompiledMethod pcPreviousTo: pc.
		 (self nonExtensionPcAt: prevPC in: aCompiledMethod) = pc ifTrue:
			[^self extensionsAt: prevPC in: aCompiledMethod into: trinaryBlock]].
	^self extensionsAt: pc in: aCompiledMethod into: trinaryBlock
]

{ #category : 'compiled method support' }
BytecodeEncoder class >> instVarReadScanBlockFor: varIndexCode using: scanner [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	self subclassResponsibility
]

{ #category : 'compiled method support' }
BytecodeEncoder class >> instVarReadScanBlockUsing: scanner [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of any inst var."
	self subclassResponsibility
]

{ #category : 'compiled method support' }
BytecodeEncoder class >> instVarWriteScanBlockFor: varIndexCode using: scanner [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	self subclassResponsibility
]

{ #category : 'compiled method support' }
BytecodeEncoder class >> instVarWriteScanBlockUsing: scanner [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of any inst var."
	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> interpretJumpIfCondIn: anInstructionStream [
	"Double-dispatch through the encoder to select the correct conditional jump decoder for the instruction set."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> interpretJumpIn: anInstructionStream [
	"Double-dispatch through the encoder to select the correct jump decoder for the instruction set."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> interpretNextInstructionFor: aClient in: anInstructionStream [
	"Double-dispatch instruction interpretation through the encoder
	 to select the correct instruction set decoder."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> isBlockReturnAt: pc in: method [
	"Answer whether the bytecode at pc is a return from block."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> isBranchIfFalseAt: pc in: method [
	"Answer whether the bytecode at pc is a conditional branch-if-false."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> isBranchIfTrueAt: pc in: method [
	"Answer whether the bytecode at pc is a conditional branch-if-true."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> isExtension: bytecode [
	"Answer if the bytecode is an extension bytecode, i.e. one that extends
	 the range of the following bytecode."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> isJumpAt: pc in: method [
	"Answer whether the bytecode at pc is an (unconditional) jump."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> isJustPopAt: pc in: method [
	"Answer whether the bytecode at pc is a pop."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> isNonSyntheticStoreAt: pc in: method for: anInstructionStream [
	"Answer whether the bytecode at pc is a store or store-pop into an explicit variable.
	 This eliminates stores into indirect temp vectors, which implement mutable closed-over
	 variables in the the closure implementation, and hence stores into temp vectors are not real stores."

	^(self isStoreAt: pc in: method)
	  and: [(self isSyntheticStoreAt: pc in: method for: anInstructionStream) not]
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> isReturnAt: pc in: method [
	"Answer whether the bytecode at pc is a return."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> isReturnSelfFromMethodAt: pc in: method [
	"Answer whether the bytecode at pc is a return self from method."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> isReturnTopFromMethodAt: pc in: method [
	"Answer whether the bytecode at pc is a return stack top from method."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> isSendAt: pc in: method [
	"Answer whether the bytecode at pc is a message-send."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> isStoreAt: pc in: method [
	"Answer whether the bytecode at pc is a store or store-pop."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> isStorePopAt: pc in: method [
	"Answer whether the bytecode at pc is a store-pop."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> isSuperSendAt: pc in: method [
	"Answer whether the bytecode at pc is a message-send to super."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> isSyntheticStoreAt: pc in: method for: anInstructionStream [
	"Answer whether the bytecode at pc is a store or store-pop of an indirect temp vector,
	 which implement mutable closed-over variables in the the closure implementation.
	 Stores into temp vectors are not real stores."

	self subclassResponsibility
]

{ #category : 'compiled method support' }
BytecodeEncoder class >> markerOrNilFor: aMethod [
	"If aMethod is a marker method, answer the symbol used to mark it.  Otherwise
	 answer nil.  What is a marker method?  It is method with body like 
		'self subclassResponsibility' or '^ self subclassResponsibility' 
	 used to indicate ('mark') a special property.

	Marker methods compile to two bytecode forms, this:
		self
		send: <literal 1>
		pop
		returnSelf
	or this:
		self
		send: <literal 1>
		returnTop"
	^self subclassResponsibility
]

{ #category : 'behavior support' }
BytecodeEncoder class >> maxAccessibleInstSize [
	"Answer the maximum number of accessible named instance variables supported by the bytecode set.
	C.f. genPushInstVarLong:, genStoreInstVarLong:, and genStorePopInstVarLong:"
	^self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> nonExtensionBytecodeAt: pc in: method [
	"Answer the actual bytecode at pc in method, skipping past any preceeding extensions."

	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> nonExtensionPcAt: pc in: method [
	"Answer the pc of the actual bytecode at pc in method, skipping past any preceeding extensions."
	| thePC bytecode |
	thePC := pc.
	[self isExtension: (bytecode := method at: thePC)] whileTrue:
		[thePC := thePC + (self bytecodeSize: bytecode)].
	^thePC
]

{ #category : 'compiled method support' }
BytecodeEncoder class >> nonLocalReturnScanBlockUsing: scanner [
	"Answer a block argument for InstructionStream>>scanFor:
	 that answers true for non-local returns."
	self subclassResponsibility
]

{ #category : 'bytecode decoding' }
BytecodeEncoder class >> pcFollowingBlockAt: pc in: method [
	"Assuming the pc is that of a block creation bytecode, answer the pc immediately following the block,
	 i.e. the next pc after the block creation."
	self subclassResponsibility
]

{ #category : 'bytecode decoding' }
BytecodeEncoder class >> pcOfBlockCreationBytecodeForBlockStartingAt: startpc in: method [
	"Answer the pc of the push closure bytecode whose block starts at startpc in method."
	^self subclassResponsibility
]

{ #category : 'bytecode decoding' }
BytecodeEncoder class >> pcPreviousTo: thePC in: method for: anInstructionStreamOrContext [
	"Answer the pc of the bytecode before the bytecode at thePC.
	 Unlike CompiledMethod>>pcPreviousTo:, this version answers nil for
	 the first bytecode of an embedded block, and answers the pc of the
	 block creation bytecode for a bytecode following an embedded block."
	| pc nextPc prevPc byte createClosureCode |
	thePC > method endPC ifTrue:
		[^method endPC].
	pc := method initialPC.
	"We could save time by scanning from the block creation bytecode of an embedded block,
	 using the following, but it saves less time than it loses in additional tests."
	"(anInstructionStreamOrContext isContext
	 and: [anInstructionStreamOrContext isClosureContext
	 and: [(nextPc := anInstructionStreamOrContext startpc) > pc]]) ifTrue:
		[pc := self pcOfBlockCreationBytecodeForBlockStartingAt: nextPc in: method]."
	createClosureCode := self createClosureCode.
	[pc < thePC] whileTrue:
		[byte := method at: (prevPc := pc).
		 [pc := createClosureCode == byte
					ifTrue:
						[nextPc := self pcFollowingBlockAt: pc in: method.
						 nextPc = thePC ifTrue: "first bytecode following block"
							[^prevPc].
						 nextPc > thePC
							ifTrue:
								[pc + (self bytecodeSize: byte) = thePC ifTrue: "first bytecode of block"
									[^nil].
								 pc + (self bytecodeSize: byte)]
						 	ifFalse: [nextPc]]
					ifFalse: [pc + (self bytecodeSize: byte)].
		  self isExtension: byte] whileTrue:
			[byte := method at: pc]].
	^prevPc

"Here's code to measure the effect of short-cutting scanning for blocks by starting at the startpc.
 It measures how much time is used to scan for the pcs from the last block to the end of all mwetods containing blocks.  Uncomment out the short-cut above to compare time with the optimization and time without.  I see approximately 290ms for all such methods with the optimization and 292 ms without, so given that this slows down the substantial majority of methods without blocks, we KISS."
"| candidates |
candidates := Dictionary new.
self systemNavigation allSelect:
	[:m| | ebc |
	(m isQuick or: [(ebc := m embeddedBlockClosures) isEmpty]) ifFalse:
		[candidates at: m put: { ebc last.
								Array streamContents:
									[:s| | is |
									(is:= InstructionStream on: m)
										pc: ebc last startpc;
										scanFor:
											[:b|
											s nextPut: is pc.
											false]] }].
	 false].
(1 to: 10) collect:
	[:ign|
	{ [candidates keysAndValuesDo:
		[:m :tuple|
		[:ebc :pcs| | c |
		c := ebc outerContext.
		pcs do:
			[:pc| m encoderClass pcPreviousTo: pc in: m for: c]] valueWithArguments: tuple]] timeToRun.
	  [candidates keysAndValuesDo:
		[:m :tuple|
		[:ebc :pcs| | c |
		c := ebc outerContext.
		pcs do:
			[:pc| m encoderClass pcPreviousTo: pc in: m for: nil]] valueWithArguments: tuple]] timeToRun. }]"
]

{ #category : 'bytecode decoding' }
BytecodeEncoder class >> pushClosureBytecodeSize [
	"Answer the size of the push closure bytecode, if there is one."
	^self subclassResponsibility
]

{ #category : 'bytecode decoding' }
BytecodeEncoder class >> pushNewArrayCode [
	"Answer the pushNewArray bytecode, if it exists in the encoder's byetcode set, or nil if not."
	^nil
]

{ #category : 'bytecode decoding' }
BytecodeEncoder class >> pushNilCode [
	"Answer the pushNil bytecode."
	^self subclassResponsibility
]

{ #category : 'compiled method support' }
BytecodeEncoder class >> pushReceiverScanBlock [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for push receiver bytecode."
	self subclassResponsibility
]

{ #category : 'instruction stream support' }
BytecodeEncoder class >> selectorToSendOrItselfFor: anInstructionStream in: method at: pc [
	"If anInstructionStream is at a send bytecode then answer the send's selector,
	 otherwise answer anInstructionStream itself.  The rationale for answering
	 anInstructionStream instead of, say, nil, is that potentially any existing object
	 can be used as a selector, but since anInstructionStream postdates the method,
	 it can't be one of them."

	self subclassResponsibility
]

{ #category : 'bytecode decoding' }
BytecodeEncoder class >> stackDeltaForPrimitive: primitiveIndex in: method [
	"This is the default implementation.  Subclasses with inline primitives will need to override."
	^0
]

{ #category : 'compiled method support' }
BytecodeEncoder class >> superSendScanBlockUsing: scanner [
	"Answer a block argument for InstructionStream>>scanFor:
	 that answers true for super sends."
	self subclassResponsibility
]

{ #category : 'bytecode decoding' }
BytecodeEncoder class >> unusedBytecode [
	"Answer the opcode of a single-byte unused bytecode, if it exists in the encoder's bytecode set, or nil if not."
	^nil
]

{ #category : 'temps' }
BytecodeEncoder >> bindAndJuggle: name [
	"This is used to insert a new temp and reorcder temps on editing.
	 It doesn't really work for closure compilation since we have multiple
	 locations for temps.  Simply signal a reparse is necessary."

	ReparseAfterSourceEditing signal
]

{ #category : 'temps' }
BytecodeEncoder >> bindBlockArg: name within: aBlockNode [
	| nArgs |
	(nArgs := aBlockNode nArgsSlot) isNil ifTrue:
		[aBlockNode nArgsSlot: (nArgs := 0)].
	nArgs >= 15 ifTrue:
		[^self notify: 'Too many arguments'].
	aBlockNode nArgsSlot: nArgs + 1.
	^(self bindTemp: name)
		beBlockArg;
		nowHasDef;
		nowHasRef;
		yourself
]

{ #category : 'temps' }
BytecodeEncoder >> bindBlockTemp: name [
	"This shouldn't be used with BytecodeEncoder.  Use bindBlockTemp:within: instead."
	self shouldNotImplement
]

{ #category : 'temps' }
BytecodeEncoder >> bindBlockTemp: name within: aBlockNode [
	| nArgs |
	(nArgs := aBlockNode nArgsSlot) ifNil:
		[aBlockNode nArgsSlot: (nArgs := 0)].
	nArgs >= (CompiledMethod fullFrameSize - 1) ifTrue:
		[^self notify: 'Too many temporaries'].
	aBlockNode nArgsSlot: nArgs + 1.
	^self bindTemp: name
]

{ #category : 'temps' }
BytecodeEncoder >> bindTemp: name [
	"Declare a temporary; error not if a field or class variable or out-of-scope temp."

	scopeTable
		at: name
		ifPresent: [ :node |
			"When non-interactive raise the error only if it is a duplicate"
			node isTemp
				ifTrue: [ node scope >= 0 ifTrue: [
						^self warnAboutShadowed: node evenIfDuringFileIn: true ]]
				ifFalse: [ 
					self warnAboutShadowed: node evenIfDuringFileIn: false]].
	^ self reallyBind: name.
]

{ #category : 'temps' }
BytecodeEncoder >> blockExtentsToTempsMap [
	"Answer a Dictionary of blockExtent to temp locations for the current method.
	 This is used by the debugger to locate temp vars in contexts.  A temp map
	 entry is a pair of the temp's name and its index, where an index is either an
	 integer for a normal temp or a pair of the index of the indirect temp vector
	 containing  the temp and the index of the temp in its indirect temp vector."
	| blockExtentsToTempsMap |
	blockExtentsToLocals ifNil:
		[^nil].
	blockExtentsToTempsMap := Dictionary new.
	blockExtentsToLocals keysAndValuesDo:
		[:blockExtent :locals|
		blockExtentsToTempsMap
			at: blockExtent
			put: (Array streamContents:
					[:strm|		"Do not call it stream, it would be shadowed"
					locals withIndexDo:
						[:local :index|
						local isIndirectTempVector
							ifTrue: [local remoteTemps withIndexDo:
										[:remoteLocal :innerIndex| strm nextPut: { remoteLocal key. { index. innerIndex } }]]
							ifFalse: [strm nextPut: { local key. index }]]])].
	^blockExtentsToTempsMap
]

{ #category : 'method generation' }
BytecodeEncoder >> computeMethodHeaderForNumArgs: numArgs numTemps: numTemps numLits: numLits primitive: primitiveIndex [
	numArgs > 15
		ifTrue: [^self error: 'Cannot compile -- too many arguments'].
	numTemps > 63
		ifTrue: [^self error: 'Cannot compile -- too many temporary variables'].	
	numLits > 65535
		ifTrue: [^self error: 'Cannot compile -- too many literals'].
	^(CompiledMethod headerFlagForEncoderClass: self class)
		+ (numArgs bitShift: 24)
		+ (numTemps bitShift: 18)
		"+ (largeBit bitShift: 17)" "largeBit gets filled in later"
		+ (primitiveIndex > 0 ifTrue: [1 bitShift: 16] ifFalse: [0])
		+ numLits
]

{ #category : 'bytecode generation' }
BytecodeEncoder >> genPushNClosureTemps: numTemps [
	"To reduce the number of bytecodes required, the embedded
	 block closure implementation uses explicit push nil instructions
	 to create block-local temps.  In bytecode sets supporting
	 FullBlockClosure/CompiledBlock this isn't needed and the
	 number of temps is derived from the block method header."
	numTemps timesRepeat: [self genPushSpecialLiteral: nil]
]

{ #category : 'testing' }
BytecodeEncoder >> hasGeneratedMethod [
	^blockExtentsToLocals notNil
]

{ #category : 'testing' }
BytecodeEncoder >> hasLocalNamed: aName [

	^ scopeTable includesKey: aName 
]

{ #category : 'special literal encodings' }
BytecodeEncoder >> if: code isSpecialLiteralForPush: aBlock [
	"If code is that of a special literal for push then evaluate aBlock with the special literal
	 The special literals for push are nil true false -1 0 1 & 2 which have special encodings
	 in the blue book bytecode set.  Answer whether it was a special literal."
	^(code between: LdTrue and: LdNil + 4)
	    and: [aBlock value: (#(true false nil -1 0 1 2) at: code - LdSelf).
			true]
]

{ #category : 'special literal encodings' }
BytecodeEncoder >> if: code isSpecialLiteralForReturn: aBlock [
	"If code is that of a special literal for return then evaluate aBlock with the special literal.
	 The special literals for return are nil true false which have special encodings
	 in the blue book bytecode set.  Answer whether it was a special literal."
	^(code between: LdTrue and: LdNil)
	   and: [aBlock value: (#(true false nil) at: code - LdSelf).
			true]
]

{ #category : 'special literal encodings' }
BytecodeEncoder >> isSpecialLiteralForPush: literal [

	^self subclassResponsibility
]

{ #category : 'special literal encodings' }
BytecodeEncoder >> isSpecialLiteralForReturn: literal [
	^literal == false
	  or: [literal == true
	  or: [literal == nil]]
]

{ #category : 'accessing' }
BytecodeEncoder >> methodStreamPosition [
	^stream position
]

{ #category : 'accessing' }
BytecodeEncoder >> nextPC [
	"Answer the pc to store in a node for source range identification when the node is associated with its following pc."
	^stream position + 1
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> nextPut: aByte [
	"For sizing make the encoder its own stream and
	 keep track of position with this version of nextPut:"
	position := position + 1
]

{ #category : 'temps' }
BytecodeEncoder >> noteBlockExtent: blockExtent hasLocals: tempNodes [
	blockExtentsToLocals ifNil:
		[blockExtentsToLocals := Dictionary new].
	blockExtentsToLocals at: blockExtent put: tempNodes asArray
]

{ #category : 'bytecode generation' }
BytecodeEncoder >> outOfRangeError: string index: index range: rangeStart to: rangeEnd [
	"For now..."
	^self error: thisContext sender method selector, ' ', string
				, ' index ', index printString
				, ' is out of range ', rangeStart printString, ' to ', rangeEnd printString
]

{ #category : 'accessing' }
BytecodeEncoder >> rootNode "^<BlockNode>" [
	^rootNode
]

{ #category : 'accessing' }
BytecodeEncoder >> rootNode: node "<BlockNode>" [
	rootNode := node
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeBranchPopFalse: distance [
	^self sizeOpcodeSelector: #genBranchPopFalse: withArguments: {distance}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeBranchPopTrue: distance [
	^self sizeOpcodeSelector: #genBranchPopTrue: withArguments: {distance}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeCallPrimitive: primitiveIndex [
	^self sizeOpcodeSelector: #genCallPrimitive: withArguments: {primitiveIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeDup [
	^self sizeOpcodeSelector: #genDup withArguments: #()
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeJump: distance [
	^self sizeOpcodeSelector: #genJump: withArguments: {distance}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeJumpLong: distance [
	^self sizeOpcodeSelector: #genJumpLong: withArguments: {distance}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeOpcodeSelector: genSelector withArguments: args [
	stream := self.
	position := 0.
	self perform: genSelector withArguments: args.
	^position
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePop [
	^self sizeOpcodeSelector: #genPop withArguments: #()
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushClosureCopyNumCopiedValues: numCopied numArgs: numArgs jumpSize: jumpSize [
	^self
		sizeOpcodeSelector: #genPushClosureCopyNumCopiedValues:numArgs:jumpSize:
		withArguments: {numCopied. numArgs. jumpSize}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushConsArray: numElements [
	^self sizeOpcodeSelector: #genPushConsArray: withArguments: {numElements}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushInstVar: instVarIndex [
	^self sizeOpcodeSelector: #genPushInstVar: withArguments: {instVarIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushInstVarLong: instVarIndex [
	^self sizeOpcodeSelector: #genPushInstVarLong: withArguments: {instVarIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushLiteral: literalIndex [
	^self sizeOpcodeSelector: #genPushLiteral: withArguments: {literalIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushLiteralVar: literalIndex [
	^self sizeOpcodeSelector: #genPushLiteralVar: withArguments: {literalIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushNClosureTemps: numTemps [
	^self sizeOpcodeSelector: #genPushNClosureTemps: withArguments: {numTemps}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushNewArray: size [
	^self sizeOpcodeSelector: #genPushNewArray: withArguments: {size}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushReceiver [
	^self sizeOpcodeSelector: #genPushReceiver withArguments: #()
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	^self sizeOpcodeSelector: #genPushRemoteTemp:inVectorAt: withArguments: {tempIndex. tempVectorIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushSpecialLiteral: specialLiteral [
	^self sizeOpcodeSelector: #genPushSpecialLiteral: withArguments: {specialLiteral}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushTemp: tempIndex [
	^self sizeOpcodeSelector: #genPushTemp: withArguments: {tempIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushTempLong: tempIndex [
	^self sizeOpcodeSelector: #genPushTempLong: withArguments: {tempIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizePushThisContext [
	^self sizeOpcodeSelector: #genPushThisContext withArguments: #()
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeReturnReceiver [
	^self sizeOpcodeSelector: #genReturnReceiver withArguments: #()
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeReturnSpecialLiteral: specialLiteral [
	^self sizeOpcodeSelector: #genReturnSpecialLiteral: withArguments: {specialLiteral}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeReturnTop [
	^self sizeOpcodeSelector: #genReturnTop withArguments: #()
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeReturnTopToCaller [
	^self sizeOpcodeSelector: #genReturnTopToCaller withArguments: #()
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeSend: selectorLiteralIndex numArgs: nArgs [
	^self sizeOpcodeSelector: #genSend:numArgs: withArguments: {selectorLiteralIndex. nArgs}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeSendSpecial: specialSelectorIndex numArgs: nArgs [
	^self sizeOpcodeSelector: #genSendSpecial:numArgs: withArguments: {specialSelectorIndex. nArgs}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeSendSuper: selectorLiteralIndex numArgs: nArgs [
	^self sizeOpcodeSelector: #genSendSuper:numArgs: withArguments: {selectorLiteralIndex. nArgs}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStoreInstVar: instVarIndex [
	^self sizeOpcodeSelector: #genStoreInstVar: withArguments: {instVarIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStoreInstVarLong: instVarIndex [
	^self sizeOpcodeSelector: #genStoreInstVarLong: withArguments: {instVarIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStoreLiteralVar: literalIndex [
	^self sizeOpcodeSelector: #genStoreLiteralVar: withArguments: {literalIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStorePopInstVar: instVarIndex [
	^self sizeOpcodeSelector: #genStorePopInstVar: withArguments: {instVarIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStorePopInstVarLong: instVarIndex [
	^self sizeOpcodeSelector: #genStorePopInstVarLong: withArguments: {instVarIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStorePopLiteralVar: literalIndex [
	^self sizeOpcodeSelector: #genStorePopLiteralVar: withArguments: {literalIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStorePopRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	^self sizeOpcodeSelector: #genStorePopRemoteTemp:inVectorAt: withArguments: {tempIndex. tempVectorIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStorePopTemp: tempIndex [
	^self sizeOpcodeSelector: #genStorePopTemp: withArguments: {tempIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStoreRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	^self sizeOpcodeSelector: #genStoreRemoteTemp:inVectorAt: withArguments: {tempIndex. tempVectorIndex}
]

{ #category : 'opcode sizing' }
BytecodeEncoder >> sizeStoreTemp: tempIndex [
	^self sizeOpcodeSelector: #genStoreTemp: withArguments: {tempIndex}
]

{ #category : 'initialization' }
BytecodeEncoder >> streamToMethod: aCompiledMethod [
	stream := WriteStream with: aCompiledMethod.
	stream position: aCompiledMethod initialPC - 1
]

{ #category : 'testing' }
BytecodeEncoder >> supportsFullBlocks [
	"Answer if the instruction set supports full closures (closure creation from
	 specfic methods instead of bytecodes embedded in an outer home method)."

	^self subclassResponsibility
]
