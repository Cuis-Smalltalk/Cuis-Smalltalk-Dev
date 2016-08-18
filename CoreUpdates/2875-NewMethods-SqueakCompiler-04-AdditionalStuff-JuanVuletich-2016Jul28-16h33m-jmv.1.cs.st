'From Cuis 4.2 of 25 July 2013 [latest update: #2866] on 28 July 2016 at 4:33:46 pm'!

!MessageSend methodsFor: 'evaluating' stamp: 'ul 3/15/2011 01:01'!
cull: firstArg cull: secondArg
	"Send the message with these optional arguments and answer the return value"

	| numArgs |
	(numArgs := selector numArgs) >= 2 ifTrue: [ ^self value: firstArg value: secondArg ].	
	numArgs = 1 ifTrue: [ ^self value: firstArg ].
	^self value! !


!UndefinedObject methodsFor: 'class hierarchy' stamp: 'cwp 6/22/2012 15:39'!
literalScannedAs: scannedLiteral environment: anEnvironment notifying: requestor 
	^ scannedLiteral! !


!Behavior methodsFor: 'printing' stamp: 'cwp 6/22/2012 15:37'!
literalScannedAs: scannedLiteral environment: anEnvironment notifying: requestor
	"Postprocesses a literal scanned by Scanner scanToken (esp. xLitQuote).
	If scannedLiteral is not an association, answer it.
	Else, if it is of the form:
		nil->#NameOfMetaclass
	answer nil->theMetaclass, if any has that name, else report an error.
	Else, if it is of the form:
		#NameOfGlobalVariable->anythiEng
	answer the global, class, or pool association with that nameE, if any, else
	add it to Undeclared a answer the new Association."

	| key value |
	(scannedLiteral isVariableBinding)
		ifFalse: [^ scannedLiteral].
	key := scannedLiteral key.
	value := scannedLiteral value.
	key ifNil: "###<metaclass soleInstance name>"
		[(self bindingOf: value environment: anEnvironment) ifNotNil:
			[:assoc|
			(assoc value isKindOf: Behavior) ifTrue: 
				[^ nil->assoc value class]].
			 requestor notify: 'No such metaclass'.
			 ^false].
	(key isSymbol) ifTrue: "##<global var name>"
		[(self bindingOf: key environment: anEnvironment) ifNotNil:
			[:assoc | ^assoc].
		^ anEnvironment undeclared: key].
	requestor notify: '## must be followed by a non-local variable name'.
	^false

"	Form literalScannedAs: 14 notifying: nil 14
	Form literalScannedAs: #OneBitForm notiEfying: nil  OneBitForm
	Form literalScannedAs: ##OneBitForm notifying: nil  OneBitForm->a Form
	Form literalScannedAs: ##Form notifying: nil   Form->Form
	Form literalScannedAs: ###Form notifying: nil   nilE->Form class
"! !

!Behavior methodsFor: 'compiling' stamp: 'nice 2/23/2011 21:36'!
newCompiler
	"Answer a Compiler suitable for compiling this Behavior"
	^self compilerClass new parser: self newParser! !

!Behavior methodsFor: '*Squeak Compatibility' stamp: 'jmv 5/24/2016 22:35'!
bindingOf: value environment: anEnvironment
^ self bindingOf: value! !

!Behavior methodsFor: '*Squeak Compatibility' stamp: 'jmv 5/23/2016 21:44'!
environment
	"Return the environment in which the receiver is declared"
	^Smalltalk! !

!Behavior methodsFor: '*Squeak Compatibility' stamp: 'jmv 5/23/2016 21:42'!
newParser
	"Answer a Parser suitable for parsing source code in this Behavior"
	^self parserClass new! !


!Number methodsFor: 'squeak compatibility' stamp: 'jmv 5/24/2016 22:26'!
asScaledDecimal: scale
	^ self ! !


!BlockClosure methodsFor: 'evaluating' stamp: 'ul 2/21/2010 17:29'!
cull: firstArg cull: secondArg
	"Activate the receiver, with two or less arguments."
	
	numArgs >= 2 ifTrue: [ ^self value: firstArg value: secondArg ].	
	numArgs = 1 ifTrue: [ ^self value: firstArg ].
	^self value! !


!InstructionStream methodsFor: 'private' stamp: 'bwesterg 1/15/2014 13:18'!
unusedBytecode: client at: targetPC
	[client unusedBytecode]
		on: MessageNotUnderstood
		do: [:ex|
			(ex receiver == client
			 and: [ex message selector == #unusedBytecode])
				ifTrue: [self error: 'unusedBytecode']
				ifFalse: [ex pass]]! !

!InstructionStream methodsFor: 'Squeak Compatibility' stamp: 'jmv 5/24/2016 22:39'!
skipCallPrimitive
	self method encoderClass callPrimitiveCode ifNotNil:
		[:callPrimitiveCode|
		(self method byteAt: pc) = callPrimitiveCode ifTrue:
			[pc := pc + (self method encoderClass bytecodeSize: callPrimitiveCode)]]! !

!InstructionStream methodsFor: 'decoding - private - v3 plus closures' stamp: 'eem 10/9/2014 13:30'!
interpretNextV3ClosuresInstructionFor: client 
	"Send to the argument, client, a message that specifies the type of the 
	 next instruction."

	| byte type offset method |
	method := self method.  
	byte := method at: pc.
	type := byte // 16.  
	offset := byte \\ 16.  
	pc := pc+1.
	"We do an inline binary search on each of the possible 16 values of type."
	type < 8 ifTrue:
		[type < 4 ifTrue:
			[type < 2 ifTrue:
				[type = 0 ifTrue:
					[^ client pushReceiverVariable: offset].
				^ client pushTemporaryVariable: offset].				"type = 1"
			type = 2 ifTrue: 
				[^ client pushConstant: (method literalAt: offset + 1)].
			^ client pushConstant: (method literalAt: offset + 17)].		"type = 3"
		type < 6 ifTrue:
			[type = 4 ifTrue:
				[^ client pushLiteralVariable: (method literalAt: offset + 1)].
			^ client pushLiteralVariable: (method literalAt: offset + 17)]."type = 5"
		type = 6 ifTrue:
			[offset < 8 ifTrue:
				[^ client popIntoReceiverVariable: offset].
			^ client popIntoTemporaryVariable: offset - 8].
		"type = 7"
		offset = 0 ifTrue: [^ client pushReceiver].
		offset < 8 ifTrue: [^ client pushConstant: (SpecialConstants at: offset)].
		offset = 8 ifTrue: [^ client methodReturnReceiver].
		offset < 12 ifTrue: [^ client methodReturnConstant: (SpecialConstants at: offset - 8)].
		offset = 12 ifTrue: [^ client methodReturnTop].
		offset = 13 ifTrue: [^ client blockReturnTop].
		^ self unusedBytecode: client at: pc - 1]. "offset = 14 & offset = 15, 126 & 127"
	type < 12 ifTrue:
		[type < 10 ifTrue:
			[type = 8 ifTrue:
				[^ self
					interpretV3ClosuresExtension: offset
					in: method
					for: client].
			"type = 9 (short jumps)"
			offset < 8 ifTrue: [^ client jump: offset + 1].
			^ client jump: offset - 8 + 1 if: false].
		type = 10 ifTrue: "(long jumps)"
			[byte := method at: pc.
			pc := pc + 1.
			offset < 8 ifTrue: [^ client jump: offset - 4 * 256 + byte].
			^ client jump: (offset bitAnd: 3) * 256 + byte if: offset < 12].
		"type = 11; arithmetic special selector sends"
		^ client
			send: (Smalltalk specialSelectorAt: offset + 1)
			super: false
			numArgs: (Smalltalk specialNargsAt: offset + 1)].
		type = 12 ifTrue: "non-arithmetic special selector sends"
			[^ client
				send: (Smalltalk specialSelectorAt: offset + 17)
				super: false
				numArgs: (Smalltalk specialNargsAt: offset + 17)].
	"type = 13, 14 or 15"
	^ client
		send: (method literalAt: offset + 1)
		super: false
		numArgs: type - 13 "0, 1 & 2"! !

!InstructionStream methodsFor: 'decoding - private - v3 plus closures' stamp: 'eem 10/9/2014 13:30'!
interpretNextV3InstructionFor: client 
	"Send to the argument, client, a message that specifies the type of the 
	 next instruction."

	| byte type offset method |
	method := self method.  
	byte := method at: pc.
	type := byte // 16.  
	offset := byte \\ 16.  
	pc := pc+1.
	"We do an inline binary search on each of the possible 16 values of type."
	type < 8 ifTrue:
		[type < 4 ifTrue:
			[type < 2 ifTrue:
				[type = 0 ifTrue:
					[^ client pushReceiverVariable: offset].
				^ client pushTemporaryVariable: offset].				"type = 1"
			type = 2 ifTrue: 
				[^ client pushConstant: (method literalAt: offset + 1)].
			^ client pushConstant: (method literalAt: offset + 17)].		"type = 3"
		type < 6 ifTrue:
			[type = 4 ifTrue:
				[^ client pushLiteralVariable: (method literalAt: offset + 1)].
			^ client pushLiteralVariable: (method literalAt: offset + 17)]."type = 5"
		type = 6 ifTrue:
			[offset < 8 ifTrue:
				[^ client popIntoReceiverVariable: offset].
			^ client popIntoTemporaryVariable: offset - 8].
		"type = 7"
		offset = 0 ifTrue: [^ client pushReceiver].
		offset < 8 ifTrue: [^ client pushConstant: (SpecialConstants at: offset)].
		offset = 8 ifTrue: [^ client methodReturnReceiver].
		offset < 12 ifTrue: [^ client methodReturnConstant: (SpecialConstants at: offset - 8)].
		offset = 12 ifTrue: [^ client methodReturnTop].
		offset = 13 ifTrue: [^ client blockReturnTop].
		^ self unusedBytecode: client at: pc - 1]. "offset = 14 & offset = 15, 126 & 127"
	type < 12 ifTrue:
		[type < 10 ifTrue:
			[type = 8 ifTrue:
				[^ self
					interpretV3Extension: offset
					in: method
					for: client].
			"type = 9 (short jumps)"
			offset < 8 ifTrue: [^ client jump: offset + 1].
			^ client jump: offset - 8 + 1 if: false].
		type = 10 ifTrue: "(long jumps)"
			[byte := method at: pc.
			pc := pc + 1.
			offset < 8 ifTrue: [^ client jump: offset - 4 * 256 + byte].
			^ client jump: (offset bitAnd: 3) * 256 + byte if: offset < 12].
		"type = 11; arithmetic special selector sends"
		^ client
			send: (Smalltalk specialSelectorAt: offset + 1)
			super: false
			numArgs: (Smalltalk specialNargsAt: offset + 1)].
		type = 12 ifTrue: "non-arithmetic special selector sends"
			[^ client
				send: (Smalltalk specialSelectorAt: offset + 17)
				super: false
				numArgs: (Smalltalk specialNargsAt: offset + 17)].
	"type = 13, 14 or 15"
	^ client
		send: (method literalAt: offset + 1)
		super: false
		numArgs: type - 13 "0, 1 & 2"! !

!InstructionStream methodsFor: 'decoding - private - v3 plus closures' stamp: 'eem 10/9/2014 13:32'!
interpretV3ClosuresExtension: offset in: method for: client
	| type offset2 byte2 byte3 byte4 |
	offset <= 6 ifTrue: 
		["Extended op codes 128-134"
		byte2 := method at: pc. pc := pc + 1.
		offset <= 2 ifTrue:
			["128-130:  extended pushes and pops"
			type := byte2 // 64.
			offset2 := byte2 \\ 64.
			offset = 0 ifTrue: 
				[type = 0 ifTrue: [^client pushReceiverVariable: offset2].
				type = 1 ifTrue: [^client pushTemporaryVariable: offset2].
				type = 2  ifTrue: [^client pushConstant: (method literalAt: offset2 + 1)].
				type = 3 ifTrue: [^client pushLiteralVariable: (method literalAt: offset2 + 1)]].
			offset = 1 ifTrue: 
				[type = 0 ifTrue: [^client storeIntoReceiverVariable: offset2].
				type = 1 ifTrue: [^client storeIntoTemporaryVariable: offset2].
				type = 2 ifTrue: [self error: 'illegalStore'].
				type = 3 ifTrue: [^client storeIntoLiteralVariable: (method literalAt: offset2 + 1)]].
			offset = 2 ifTrue: 
				[type = 0 ifTrue: [^client popIntoReceiverVariable: offset2].
				type = 1 ifTrue: [^client popIntoTemporaryVariable: offset2].
				type = 2 ifTrue: [self error: 'illegalStore'].
				type = 3  ifTrue: [^client popIntoLiteralVariable: (method literalAt: offset2 + 1)]]].
		"131-134: extended sends"
		offset = 3 ifTrue:  "Single extended send"
			[^client send: (method literalAt: byte2 \\ 32 + 1)
					super: false numArgs: byte2 // 32].
		offset = 4 ifTrue:    "Double extended do-anything"
			[byte3 := method at: pc. pc := pc + 1.
			type := byte2 // 32.
			type = 0 ifTrue: [^client send: (method literalAt: byte3 + 1)
									super: false numArgs: byte2 \\ 32].
			type = 1 ifTrue: [^client send: (method literalAt: byte3 + 1)
									super: true numArgs: byte2 \\ 32].
			type = 2 ifTrue: [^client pushReceiverVariable: byte3].
			type = 3 ifTrue: [^client pushConstant: (method literalAt: byte3 + 1)].
			type = 4 ifTrue: [^client pushLiteralVariable: (method literalAt: byte3 + 1)].
			type = 5 ifTrue: [^client storeIntoReceiverVariable: byte3].
			type = 6 ifTrue: [^client popIntoReceiverVariable: byte3].
			type = 7 ifTrue: [^client storeIntoLiteralVariable: (method literalAt: byte3 + 1)]].
		offset = 5 ifTrue:  "Single extended send to super"
			[^client send: (method literalAt: byte2 \\ 32 + 1)
					super: true
					numArgs: byte2 // 32].
		offset = 6 ifTrue:   "Second extended send"
			[^client send: (method literalAt: byte2 \\ 64 + 1)
					super: false
					numArgs: byte2 // 64]].
	offset = 7 ifTrue: [^client doPop].
	offset = 8 ifTrue: [^client doDup].
	offset = 9 ifTrue: [^client pushActiveContext].
	byte2 := method at: pc. pc := pc + 1.
	offset = 10 ifTrue:
		[^byte2 < 128
			ifTrue: [client pushNewArrayOfSize: byte2]
			ifFalse: [client pushConsArrayWithElements: byte2 - 128]].
	offset = 11 ifTrue: [^self unusedBytecode: client at: pc - 1].
	byte3 := method at: pc.  pc := pc + 1.
	offset = 12 ifTrue: [^client pushRemoteTemp: byte2 inVectorAt: byte3].
	offset = 13 ifTrue: [^client storeIntoRemoteTemp: byte2 inVectorAt: byte3].
	offset = 14 ifTrue: [^client popIntoRemoteTemp: byte2 inVectorAt: byte3].
	"offset = 15"
	byte4 := method at: pc.  pc := pc + 1.
	^client
		pushClosureCopyNumCopiedValues: (byte2 bitShift: -4)
		numArgs: (byte2 bitAnd: 16rF)
		blockSize: (byte3 * 256) + byte4! !

!InstructionStream methodsFor: 'decoding - private - v3 plus closures' stamp: 'eem 10/9/2014 13:33'!
interpretV3Extension: offset in: method for: client
	| type offset2 byte2 byte3 |
	offset <= 6 ifTrue: 
		["Extended op codes 128-134"
		byte2 := method at: pc. pc := pc + 1.
		offset <= 2 ifTrue:
			["128-130:  extended pushes and pops"
			type := byte2 // 64.
			offset2 := byte2 \\ 64.
			offset = 0 ifTrue: 
				[type = 0 ifTrue: [^client pushReceiverVariable: offset2].
				type = 1 ifTrue: [^client pushTemporaryVariable: offset2].
				type = 2  ifTrue: [^client pushConstant: (method literalAt: offset2 + 1)].
				type = 3 ifTrue: [^client pushLiteralVariable: (method literalAt: offset2 + 1)]].
			offset = 1 ifTrue: 
				[type = 0 ifTrue: [^client storeIntoReceiverVariable: offset2].
				type = 1 ifTrue: [^client storeIntoTemporaryVariable: offset2].
				type = 2 ifTrue: [self error: 'illegalStore'].
				type = 3 ifTrue: [^client storeIntoLiteralVariable: (method literalAt: offset2 + 1)]].
			offset = 2 ifTrue: 
				[type = 0 ifTrue: [^client popIntoReceiverVariable: offset2].
				type = 1 ifTrue: [^client popIntoTemporaryVariable: offset2].
				type = 2 ifTrue: [self error: 'illegalStore'].
				type = 3  ifTrue: [^client popIntoLiteralVariable: (method literalAt: offset2 + 1)]]].
		"131-134: extended sends"
		offset = 3 ifTrue:  "Single extended send"
			[^client send: (method literalAt: byte2 \\ 32 + 1)
					super: false numArgs: byte2 // 32].
		offset = 4 ifTrue:    "Double extended do-anything"
			[byte3 := method at: pc. pc := pc + 1.
			type := byte2 // 32.
			type = 0 ifTrue: [^client send: (method literalAt: byte3 + 1)
									super: false numArgs: byte2 \\ 32].
			type = 1 ifTrue: [^client send: (method literalAt: byte3 + 1)
									super: true numArgs: byte2 \\ 32].
			type = 2 ifTrue: [^client pushReceiverVariable: byte3].
			type = 3 ifTrue: [^client pushConstant: (method literalAt: byte3 + 1)].
			type = 4 ifTrue: [^client pushLiteralVariable: (method literalAt: byte3 + 1)].
			type = 5 ifTrue: [^client storeIntoReceiverVariable: byte3].
			type = 6 ifTrue: [^client popIntoReceiverVariable: byte3].
			type = 7 ifTrue: [^client storeIntoLiteralVariable: (method literalAt: byte3 + 1)]].
		offset = 5 ifTrue:  "Single extended send to super"
			[^client send: (method literalAt: byte2 \\ 32 + 1)
					super: true
					numArgs: byte2 // 32].
		offset = 6 ifTrue:   "Second extended send"
			[^client send: (method literalAt: byte2 \\ 64 + 1)
					super: false
					numArgs: byte2 // 64]].
	offset = 7 ifTrue: [^client doPop].
	offset = 8 ifTrue: [^client doDup].
	offset = 9 ifTrue: [^client pushActiveContext].
	^self unusedBytecode: client at: pc! !

!InstructionStream methodsFor: 'decoding - private - v3 plus closures' stamp: 'eem 5/18/2014 14:19'!
interpretV3Jump
	"If the instruction at pc is an unconditional jump, interpret it, advancing the pc,
	 and answering the target pc. Otherwise answer nil."

	"144-151 	10010iii 		Jump iii + 1 (i.e., 1 through 8)
	 160-167 	10100iii jjjjjjjj 	Jump(iii - 4) *256+jjjjjjjj"
	| byte |
	byte := self method at: pc.
	(byte between: 144 and: 151) ifTrue:
		[pc := pc + 1.
		 ^byte - 143].
	(byte between: 160 and: 167) ifTrue:
		[pc := pc + 2.
		 ^(byte - 164) * 256 + (self method at: pc - 1)].
	^nil! !

!InstructionStream methodsFor: 'decoding - private - v3 plus closures' stamp: 'eem 5/18/2014 14:21'!
interpretV3JumpIfCond
	"If the instruction at pc is a conditional jump, interpret it, advancing the pc,
	 and answering the jump distance. Otherwise answer nil."

	"152-159 	10011iii 		Pop and Jump 0n False iii +1 (i.e., 1 through 8)
	 168-171 	101010ii jjjjjjjj 	Pop and Jump On True ii *256+jjjjjjjj
	 172-175 	101011ii jjjjjjjj 	Pop and Jump On False ii *256+jjjjjjjj"
	| byte |
	byte := self method at: pc.
	(byte between: 152 and: 159) ifTrue:
		[pc := pc + 1.
		 ^byte - 151].
	(byte between: 168 and: 175) ifTrue:
		[pc := pc + 2.
		 ^(byte bitAnd: 3) * 256 + (self method at: pc - 1)].
	^nil! !


!BlockContext methodsFor: 'evaluating' stamp: 'nice 10/11/2010 23:32'!
cull: firstArg cull: secondArg
	"Activate the receiver, with two or less arguments."
	
	self numArgs >= 2 ifTrue: [ ^self value: firstArg value: secondArg ].	
	self numArgs = 1 ifTrue: [ ^self value: firstArg ].
	^self value! !


!PseudoClass methodsFor: 'accessing' stamp: 'ul 4/2/2011 14:01'!
newCompiler
	"Answer a Compiler suitable for compiling this Behavior"
	^self compilerClass new parser: self newParser! !

!PseudoClass methodsFor: 'printing' stamp: 'cwp 6/22/2012 15:39'!
literalScannedAs: scannedLiteral environment: anEnvironment notifying: requestor 
	^ scannedLiteral! !

!PseudoClass methodsFor: '*Squeak Compatibility' stamp: 'jmv 5/23/2016 21:43'!
newParser
	"Answer a Parser suitable for parsing source code in this Behavior"
	^self parserClass new! !

