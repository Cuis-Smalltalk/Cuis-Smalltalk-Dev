"
I add behaviour to Encoder to size and emit bytecodes for the Squeak V3.x VM bytecode set, a close variant of the original Smalltalk-80 bytecode set defined in the Blue Book.

	0-15 		0000iiii 			Push Receiver Variable #iiii
	16-31 		0001iiii 			Push Temporary Location #iiii
	32-63 		001iiiii 			Push Literal Constant #iiiii
	64-95 		010iiiii 			Push Literal Variable #iiiii
	96-103 		01100iii 			Pop and Store Receiver Variable #iii
	104-111 	01101iii 			Pop and Store Temporary Location #iii
	112-119 	01110iii 			Push (receiver, true, false, nil, -1, 0, 1, 2) [iii]
	120-123 	011110ii 		Return (receiver, true, false, nil) [ii] From Message
	124-125 	0111110i 		Return Stack Top From (Message, Block) [i]
	(126-127 unassigned)
	128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk
	129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk
	130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk
	131 	10000011 jjjkkkkk 	Send Literal Selector #kkkkk With jjj Arguments
	132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj (for sends jjjjj = numArgs)
	133 	10000011 jjjkkkkk 	Send Literal Selector #kkkkk To Superclass With jjj Arguments
	134 	10000011 jjjkkkkk 	Send Literal Selector #kkkkk With jjj Arguments
	135 		10000111 		Pop Stack Top
	136 		10001000 		Duplicate Stack Top
	137 		10001001 		Push Active Context
	(138-143 unassigned)
	144-151 	10010iii 			Jump iii + 1 (i.e., 1 through 8)
	152-159 	10011iii 			Pop and Jump 0n False iii +1 (i.e., 1 through 8)
	160-167 	10100iii jjjjjjjj 	Jump(iii - 4) *256+jjjjjjjj
	168-171 	101010ii jjjjjjjj 	Pop and Jump On True ii *256+jjjjjjjj
	172-175 	101011ii jjjjjjjj 	Pop and Jump On False ii *256+jjjjjjjj
	176-191 	1011iiii 			Send Arithmetic Message #iiii
	192-207 	1100iiii 			Send Special Message #iiii
	208-223 	1101iiii 			Send Literal Selector #iiii With No Arguments
	224-239 	1110iiii 			Send Literal Selector #iiii With 1 Argument
	240-255 	1111iiii 			Send Literal Selector #iiii With 2 Arguments

An encoder for the V3 bytecode set augmented with the following bytecodes that are part of the full closure implementation.
	138   10001010 jkkkkkkk		Push (Array new: kkkkkkk) (j = 0)
								or	Pop kkkkkkk elements into: (Array new: kkkkkkk) (j = 1)
	139   10001011 kkkkkkkk jjjjjjjj 	Invoke primitive number jjjjjjjjkkkkkkkk
	140   10001100 kkkkkkkk jjjjjjjj 	Push Temp At kkkkkkkk In Temp Vector At: jjjjjjjj
	141   10001101 kkkkkkkk jjjjjjjj 	Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj
	142   10001110 kkkkkkkk jjjjjjjj 	Pop and Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj
	143   10001111 llllkkkk jjjjjjjj iiiiiiii	Push Closure Num Copied llll Num Args kkkk BlockSize jjjjjjjjiiiiiiii
This is an exact duplicate of EncoderForLongFormV3PlusClosures.
Could be a trait (or in Newspeak, a Mixin).
For now we impose upon you to synchronise any and all changes between these two classes.
"
Class {
	#name : 'EncoderForV3PlusClosures',
	#superclass : 'BytecodeEncoder',
	#category : 'Compiler-Kernel',
	#package : 'Compiler',
	#tag : 'Kernel'
}

{ #category : 'compiled method support' }
EncoderForV3PlusClosures class >> bindingReadScanBlockFor: bindingLitIndex using: scanner [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the value of the binding with zero-relative index litVarIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^[:b|
	   b >= 64
	   and: [b <= 95
			ifTrue: [b - 64 = bindingLitIndex]
			ifFalse:
				[b = 128
					ifTrue: [scanner followingByte - 192 = bindingLitIndex]
					ifFalse:
						[b = 132
						 and: [(scanner followingByte between: 128 and: 159)
						 and: [scanner thirdByte = bindingLitIndex]]]]]]
]

{ #category : 'compiled method support' }
EncoderForV3PlusClosures class >> bindingWriteScanBlockFor: bindingLitIndex using: scanner [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the value of the binding with zero-relative index bindingLitIndex.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^[:b|
	  (b = 129 or: [b = 130])
		ifTrue: [scanner followingByte - 192 = bindingLitIndex]
		ifFalse:
			[b = 132
			 and: [scanner followingByte >= 224
			 and: [scanner thirdByte = bindingLitIndex]]]]
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> bytecodeSize: bytecode [
	"Answer the number of bytes in the bytecode."
	bytecode <= 125 ifTrue:
		[^1].
	bytecode >= 176 ifTrue:
		[^1].
	bytecode >= 160 ifTrue: "long jumps"
		[^2].
	bytecode >= 144 ifTrue: "short jumps"
		[^1].
	"extensions"
	bytecode >= 128 ifTrue:
		[^#(2 2 2 2 3 2 2 1 1 1 2 3 3 3 3 4) at: bytecode - 127].
	^nil
]

{ #category : 'bytecode decoding' }
EncoderForV3PlusClosures class >> callPrimitiveCode [
	"139	11101111	iiiiiiii jjjjjjjj	Call Primitive #iiiiiiii + (jjjjjjjj * 256)"
	^139
]

{ #category : 'testing' }
EncoderForV3PlusClosures class >> canBeSpecialLiteral: aLiteral [
	"This check can be used to prevent unnecessary use of #scanBlockOrNilForLiteral:."

	aLiteral isVariableBinding ifTrue: [^false]. "a common case; don't waste time analysing..."

	aLiteral isSymbol ifTrue: [^ Smalltalk specialSelectors identityIncludes: aLiteral].
	aLiteral isInteger ifTrue: [^ aLiteral between: -1 and: 2].

	aLiteral == true ifTrue: [^ true].
	aLiteral == false ifTrue: [^ true].
	aLiteral == nil ifTrue: [^ true].

	^ false
]

{ #category : 'bytecode decoding' }
EncoderForV3PlusClosures class >> createClosureCode [
	"Answer the create closure bytecode, if it exists in the encoder's byetcode set, or nil if not."
	^143
]

{ #category : 'compiled method support' }
EncoderForV3PlusClosures class >> createClosureScanBlock [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for block closure creation bytecodes."
	^[ :bc | bc = 143]
]

{ #category : 'services' }
EncoderForV3PlusClosures class >> displaceInstVarAccessesIn: aCompiledMethodOrMethodSpec starting: firstIvarToDisplace by: delta doIt: aBoolean [
	"Remap instance variable accesses according to parameters.
	Affect instance variables access starting with ivar number firstIvarToDisplace. Displace them by delta.
	Instance variable numbers are 1-based.
	If aBoolean is false, just check if it is possible and answer true if so.
	If aBoolean is true, go ahead and do it. Raise an error on failure, because method(s) are now inconsistent.
	Similar to #remapInstVarAccessesIn:asPer:doIt:
	Note: Assumes the V3PlusClosures bytecode set is in use.
	Note: Based on InstructionStream>>scanFor:
	See also
		#genPushInstVarAllowForIvarsRemapping:
		#genStoreInstVarAllowForIvarsRemapping:
		#genStorePopInstVarAllowForIvarsRemapping:
		#returnField and #primitive

	Warning: After calling on a CompiledMethod, you must do either
		aBoolean ifTrue: [aCompiledMethodOrMethodSpec voidCogVMState].
	or
		aBoolean ifTrue: [Smalltalk voidCogVMState].
	This is not done in this method, because when processing many, it is best to do
		Smalltalk voidCogVMState
	after all of them. See senders."
	
	"This method can not handle remapping of 1-byte ivar access bytecodes: [0..15], [96..103]
	This method can not convert two byte bytecodes #[128 129 130] to or from three byte 132 + #[64 160 192].
	However the Sista Bytecode version of this method can.
	It is therefore recommended to use the Sista Bytecode Set."

	| pc end bytecode followingByte thirdByte oldIvar newIvar bytecodeSize |
	"Just in case..."
	delta = 0 ifTrue: [ ^true ].
	
	(self includesBehavior: aCompiledMethodOrMethodSpec encoderClass) ifFalse: [
		aBoolean ifTrue: [self error: 'BytecodeEncoder inconsistency'].
		^false ]	.

	pc := aCompiledMethodOrMethodSpec initialPC.
	end := aCompiledMethodOrMethodSpec endPC.

	"Quick return"
	aCompiledMethodOrMethodSpec isReturnField ifTrue: [
		aBoolean ifTrue: [
			oldIvar := aCompiledMethodOrMethodSpec returnField+1.
			oldIvar >= firstIvarToDisplace ifTrue: [
				| primitiveCode |
				newIvar := oldIvar+delta.
				primitiveCode := newIvar-1 + 264.
				aCompiledMethodOrMethodSpec at: pc + 1 put: primitiveCode \\ 256.
				aCompiledMethodOrMethodSpec at: pc + 2 put: primitiveCode // 256 ]].
		^true ].

	[pc <= end] whileTrue: [
		bytecode := aCompiledMethodOrMethodSpec at: pc.
		
		"Copied from EncoderForV3PlusClosures class >> bytecodeSize:"
		bytecodeSize := true caseOf: {
			[bytecode <= 125] -> [1].
			[bytecode >= 176] -> [1].
			[bytecode >= 160] -> [2].						"long jumps"
			[bytecode >= 144] -> [1].						"short jumps"
			[bytecode >= 128] -> [ 							"extensions"
				#(2 2 2 2 3 2 2 1 1 1 2 3 3 3 3 4) at: bytecode - 127] }.
		
		"Can not handle short (single byte) instance variable access bytecodes at all"
		oldIvar := -1.
		bytecode < 16 ifTrue: [ oldIvar := bytecode + 1 ].
		(bytecode between: 96 and: 103) ifTrue: [ oldIvar := (bytecode bitAnd: 2r111) + 1 ].
		oldIvar >= firstIvarToDisplace ifTrue: [
			aBoolean ifTrue: [self error: 'Can not handle short instance variable access bytecodes'].
			^false ]	.
		
		"Two byte instance variable access bytecodes"
		followingByte := pc+1 <= end ifTrue: [ aCompiledMethodOrMethodSpec at: pc+1 ].
		((#[128 129 130] includes: bytecode) and: [followingByte < 64]) ifTrue: [
			followingByte+1 >= firstIvarToDisplace ifTrue: [
				followingByte+delta < 64 ifFalse: [
					aBoolean ifTrue: [self error: 'Can not convert 2 byte instance variable access bytecodes to 3 byte'].
					^false].
				aBoolean ifTrue: [ aCompiledMethodOrMethodSpec at: pc+1 put: followingByte+delta ]]].

		"Three byte instance variable access bytecodes"
		(bytecode = 132 and: [#[64 160 192] includes: followingByte]) ifTrue: [
			thirdByte := pc+2 <= end ifTrue: [ aCompiledMethodOrMethodSpec at: pc+2 ].
			thirdByte+1 >= firstIvarToDisplace ifTrue: [
				aBoolean ifTrue: [ aCompiledMethodOrMethodSpec at: pc+2 put: thirdByte+delta ]]].

		pc := pc + bytecodeSize ].

	^true
]

{ #category : 'bytecode decoding' }
EncoderForV3PlusClosures class >> emptyClosurePattern [
	"Answer the byte pattern for an empty closure. For instance (verbatim from the bytecode view):
		33 <8F 00 00 02> closureNumCopied: 0 numArgs: 0 bytes 37 to 38
		37 	<73> pushConstant: nil
		38 	<7D> blockReturn"
	"ByteArray readHexFrom: '8F000002737D'"
	^ #[143 0 0 2 115 125]
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> extensionsAt: pc in: aCompiledMethod into: trinaryBlock [
	"If the bytecode at pc is an extension then evaluate aTrinaryBlock
	 with the values of extA and extB and number of extension *bytes*.
	 If the bytecode at pc is not an extension then evaluate with 0, 0, 0.
	 There are no extensions in the SqueakV3/Smalltalk-80 bytecode set, so..." 
	^trinaryBlock value: 0 value: 0 value: 0
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> extensionsFor: pc in: aCompiledMethod into: trinaryBlock [
	"If the bytecode at pc is an extension, or if the bytecode at pc is preceeded by extensions,
	 then evaluate aTrinaryBlock with the values of extA and extB and number of extension *bytes*.
	 If the bytecode at pc is neither an extension or extended then evaluate with 0, 0, 0.
	 There are no extensions in the SqueakV3/Smalltalk-80 bytecode set, so..." 
	^trinaryBlock value: 0 value: 0 value: 0
]

{ #category : 'compiled method support' }
EncoderForV3PlusClosures class >> instVarReadScanBlockFor: varIndexCode using: scanner [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^[:b|
	   b < 16
		ifTrue: [b = varIndexCode]
		ifFalse:
			[b = 128
				ifTrue: [scanner followingByte = varIndexCode and: [varIndexCode <= 63]]
				ifFalse:
					[b = 132
					 and: [(scanner followingByte between: 64 and: 95)
					 and: [scanner thirdByte = varIndexCode]]]]]
]

{ #category : 'compiled method support' }
EncoderForV3PlusClosures class >> instVarReadScanBlockUsing: scanner [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of any inst var.
	See #genPushInstVar:
	See #genPushInstVarLong:"

	^[:b|
		(b < 16)
		or: [(b = 16r80 and: [scanner followingByte <= 16r3F])
		or:  [(b = 16r84 and: [scanner followingByte = 16r40])]]]
]

{ #category : 'compiled method support' }
EncoderForV3PlusClosures class >> instVarWriteScanBlockFor: varIndexCode using: scanner [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for writes of the inst var with zero-relative index varIndexCode.
	 N.B. Don't assume the compiler uses the most compact encoding available."
	^[:b|
	   b >= 96
	   and: [b <= 103
			ifTrue: [b - 96 = varIndexCode]
			ifFalse:
				[(b = 129 or: [b = 130])
					ifTrue: [scanner followingByte = varIndexCode and: [varIndexCode <= 63]]
					ifFalse:
						[b = 132
						 and: [(scanner followingByte between: 160 and: 223)
						 and: [scanner thirdByte = varIndexCode]]]]]]
]

{ #category : 'compiled method support' }
EncoderForV3PlusClosures class >> instVarWriteScanBlockUsing: scanner [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for reads of any inst var.
	See #genStoreInstVar:
	See #genStoreInstVarLong:
	See #genStorePopInstVar:
	See #genStorePopInstVarLong:"

	^[:b|
		(b between: 16r60 and: 16r60 + 7)
		or: [(b = 16r81 and: [scanner followingByte <= 16r3F])
		or: [(b = 16r82 and: [scanner followingByte <= 16r3F])
		or: [(b = 16r84 and: [scanner followingByte = 160])
		or: [(b = 16r84 and: [scanner followingByte = 192])]]]]]
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> interpretJumpIfCondIn: anInstructionStream [
	"Double-dispatch through the encoder to select the correct conditional jump decoder for the instruction set."
	^anInstructionStream interpretV3JumpIfCond
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> interpretJumpIn: anInstructionStream [
	"Double-dispatch through the encoder to select the correct jump interpreter for the instruction set."
	^anInstructionStream interpretV3Jump
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> interpretNextInstructionFor: aClient in: anInstructionStream [
	"Double-dispatch through the encoder to select the correct instruction set decoder."
	^anInstructionStream interpretNextV3ClosuresInstructionFor: aClient
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> isBlockReturnAt: pc in: method [
	"Answer whether the bytecode at pc is a return from block."

	^(method at: pc) = 125
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> isBranchIfFalseAt: pc in: method [
	"Answer whether the bytecode at pc is a conditional branch-if-false."
	| bytecode |
	bytecode := method at: pc.
	^(bytecode between: 152 and: 159) or: [bytecode between: 172 and: 175]
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> isBranchIfTrueAt: pc in: method [
	"Answer whether the bytecode at pc is a conditional branch-if-true."
	^(method at: pc) between: 168 and: 171
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> isExtension: bytecode [
	"Answer if the bytecode is an extension bytecode, i.e. one that extends the
	 range of the following bytecode.  The Smalltalk-80/V3 sets don't use extensions."
	^false
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> isJumpAt: pc in: method [
	"Answer whether the bytecode at pc is an (unconditional) jump."
	| bytecode |
	bytecode := method at: pc.
	^(bytecode between: 144 and: 151) or: [bytecode between: 160 and: 167]
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> isJustPopAt: pc in: method [
	"Answer whether the bytecode at pc is a pop."

	^(method at: pc) = 135	"135 		10000111 	Pop Stack Top"
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> isReturnAt: pc in: method [
	"Answer whether the bytecode at pc is a return."

	^(method at: pc) between: 120 and: 125
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> isReturnSelfFromMethodAt: pc in: method [
	"Answer whether the bytecode at pc is a return self from method."

	^(method at: pc) = 120
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> isReturnTopFromMethodAt: pc in: method [
	"Answer whether the bytecode at pc is a return stack top from method."

	^(method at: pc) = 124
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> isSendAt: pc in: method [
	"Answer whether the bytecode at pc is a message-send."

	| byte |
	byte := method at: pc.
	^byte >= 176			"special send or short send"
	 or: [byte >= 131
		 and: [byte <= 134	"long sends"	
		 and: [byte ~= 132	"double extended do anything"
			or: [(method at: pc + 1) // 32 <= 1]]]]
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> isStoreAt: pc in: method [
	"Answer whether the bytecode at pc is a store or store-pop."

	| byte |
	byte := method at: pc.
	^(byte between: 96 and: 142)
		and: [byte <= 111				"96 103	storeAndPopReceiverVariableBytecode"
										"104 111	storeAndPopTemporaryVariableBytecode"
			or: [byte >= 129			"129		extendedStoreBytecode"
				and: [byte <= 130		"130		extendedStoreAndPopBytecode"
					or: [(byte = 132	"132		doubleExtendedDoAnythingBytecode"
						and: [(method at: pc+1) >= 160])
					or: [byte = 141		"141		storeRemoteTempLongBytecode"
					or: [byte = 142		"142		storeAndPopRemoteTempLongBytecode"]]]]]]
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> isStorePopAt: pc in: method [
	"Answer whether the bytecode at pc is a store-pop."

	| byte |
	byte := method at: pc.
	^(byte between: 96 and: 111)	"96 103	storeAndPopReceiverVariableBytecode"
									"104 111	storeAndPopTemporaryVariableBytecode"
	  or: [byte = 130				"130		extendedStoreAndPopBytecode"
	  or: [byte = 142]]				"142		storeAndPopRemoteTempLongBytecode"
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> isSuperSendAt: pc in: method [
	"Answer whether the bytecode at pc is a message-send to super."

	"132 		10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj (for sends jjjjj = numArgs)
	 133 		10000101 jjjkkkkk 	Send Literal Selector #kkkkk To Superclass With jjj Arguments"

	| byte |
	byte := method at: pc.
	^byte = 133	"double extended do anything"
	  or: [byte = 132 and: [(method at: pc + 1) // 32 = 1]]
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> isSyntheticStoreAt: pc in: method for: anInstructionStream [
	"Answer whether the bytecode at pc is a store or store-pop of an indirect temp vector,
	 which implement mutable closed-over variables in the the closure implementation.
	 Stores into temp vectors are not real stores.  N.B. pcPreviousTo:in:for: is slow, so filter
	 out any preceding bytecodes other than what looks like a pushNewArrayCode.  But the
	 pcPreviousTo:in:for: is still necessary, since the presence of a pcPreviousTo:in:for: in the
	 right place is potentially ambiguous, possibly part of a different bytecode seqence."

	^(self isTempStoreAt: pc in: method)
	  and: [pc - 2 >= method initialPC
	  and: [(method at: pc - 2) = self pushNewArrayCode
	  and: [(method at: pc - 1) <= 127
	  and: [pc - 2 = (self pcPreviousTo: pc in: method for: anInstructionStream)]]]]
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> isTempStoreAt: pc in: method [
	"Answer whether the bytecode at pc is a store or store-pop into a temporary variable.
	 104-111 	01101iii 	Pop and Store Temporary Location #iii
	 129 		10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk
	 130 		10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"

	| byte |
	byte := method at: pc.
	^byte >= 104
	  and: [byte <= 111
			or: [byte <= 130 and: [byte >= 129 and: [(method at: pc + 1) >> 6 = 1]]]]
]

{ #category : 'compiled method support' }
EncoderForV3PlusClosures class >> markerOrNilFor: aMethod [
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
	| expectedHeaderPlusLliteralSize e |
	expectedHeaderPlusLliteralSize := Smalltalk wordSize * 4.
	^(((e := aMethod endPC - expectedHeaderPlusLliteralSize) = 3 or: [e = 4]) 
	  and: [aMethod numLiterals = 3
	  and: [(aMethod at:  expectedHeaderPlusLliteralSize + 1) = 16r70	"push self"
	  and: [(aMethod at: expectedHeaderPlusLliteralSize + 2) = 16rD0]]])	"send <literal 1>"
		ifTrue: [aMethod literalAt: 1]
]

{ #category : 'behavior support' }
EncoderForV3PlusClosures class >> maxAccessibleInstSize [
	"Answer the maximum number of accessible named instance variables supported by the bytecode set.
	C.f. genPushInstVarLong:, genStoreInstVarLong:, and genStorePopInstVarLong:"
	^256
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> nonExtensionBytecodeAt: pc in: method [
	"Answer the actual bytecode at pc in method, skipping past any preceeding extensions."
	^method at: pc
]

{ #category : 'compiled method support' }
EncoderForV3PlusClosures class >> nonLocalReturnScanBlockUsing: scanner [
	"Answer a block argument for InstructionStream>>scanFor:
	 that answers true for non-local returns."

	^[ :instr |
		instr between: 120 and: 124 ]
]

{ #category : 'bytecode decoding' }
EncoderForV3PlusClosures class >> pcFollowingBlockAt: pc in: method [
	"Assuming the pc is that of a block creation bytecode, answer the pc immediately following the block,
	 i.e. the next pc after the block creation."
	self assert: (method at: pc) = self createClosureCode.
	^(method at: pc + 2) * 256 + (method at: pc + 3) + pc + 4
]

{ #category : 'bytecode decoding' }
EncoderForV3PlusClosures class >> pcOfBlockCreationBytecodeForBlockStartingAt: startpc in: method [
	"Answer the pc of the push closure bytecode whose block starts at startpc in method.
	 143   10001111 llllkkkk jjjjjjjj iiiiiiii	Push Closure Num Copied llll Num Args kkkk BlockSize jjjjjjjjiiiiiiii"
	^startpc - 4
]

{ #category : 'bytecode decoding' }
EncoderForV3PlusClosures class >> pushClosureBytecodeSize [
	"Answer the size of the push closure bytecode.
	 143   10001111 llllkkkk jjjjjjjj iiiiiiii	Push Closure Num Copied llll Num Args kkkk BlockSize jjjjjjjjiiiiiiii"
	^4
]

{ #category : 'bytecode decoding' }
EncoderForV3PlusClosures class >> pushNewArrayCode [
	"138   10001010 jkkkkkkk		Push (Array new: kkkkkkk) (j = 0)
								or	Pop kkkkkkk elements into: (Array new: kkkkkkk) (j = 1)"
	^138
]

{ #category : 'bytecode decoding' }
EncoderForV3PlusClosures class >> pushNilCode [
	"Answer the pushNil bytecode.
	 112-119 	01110iii 	Push (receiver, true, false, nil, -1, 0, 1, 2) [iii]"
	^115
]

{ #category : 'compiled method support' }
EncoderForV3PlusClosures class >> pushReceiverScanBlock [
	"Answer a block argument for InstructionStream>>scanFor: that answers true
	 for push receiver bytecode."

	^[ :bc | bc = 112]
]

{ #category : 'services' }
EncoderForV3PlusClosures class >> remapInstVarAccessesIn: aCompiledMethodOrMethodSpec asPer: aDictionary doIt: aBoolean [
	"Remap all instance variable accesses according to aDictionary.
	aDictionary is of the form: {
		(oldIvarNumber -> newIvarNumber).
		...
		(oldIvarNumber -> newIvarNumber) }
	where instance variable numbers are 1-based.
	If aBoolean is false, just check if it is possible and answer true if so.
	If aBoolean is true, go ahead and do it. Raise an error on failure, because method(s) are now inconsistent.
	Note: If calling on a CompiledMethod, you must do
		Smalltalk voidCogVMState
	when done. But if you're processing a bunch of methods, do it after you're done with all of them.
	Similar to #displaceInstVarAccessesIn:starting:by:doIt:
	Note: Assumes the V3PlusClosures bytecode set is in use.
	Note: Based on InstructionStream>>scanFor:
	See also
		#genPushInstVarAllowForIvarsRemapping:
		#genStoreInstVarAllowForIvarsRemapping:
		#genStorePopInstVarAllowForIvarsRemapping:
		#returnField and #primitive"
	
	"This method can not handle remapping of 1-byte ivar access bytecodes: [0..15], [96..103]
	This method can not convert two byte bytecodes #[128 129 130] to or from three byte 132 + #[64 160 192].
	However the Sista Bytecode version of this method can.
	It is therefore recommended to use the Sista Bytecode Set."

	| pc end bytecode followingByte thirdByte oldIvar bytecodeSize |
	
	(self includesBehavior: aCompiledMethodOrMethodSpec encoderClass) ifFalse: [
		aBoolean ifTrue: [self error: 'BytecodeEncoder inconsistency'].
		^false ]	.

	pc := aCompiledMethodOrMethodSpec initialPC.
	end := aCompiledMethodOrMethodSpec endPC.

	"Quick return"
	aCompiledMethodOrMethodSpec isReturnField ifTrue: [
		aBoolean ifTrue: [
			aDictionary at: aCompiledMethodOrMethodSpec returnField+1 ifPresent: [ :newIvar |
				| primitiveCode |
				primitiveCode := newIvar-1 + 264.
				aCompiledMethodOrMethodSpec at: pc + 1 put: primitiveCode \\ 256.
				aCompiledMethodOrMethodSpec at: pc + 2 put: primitiveCode // 256 ]].
		^true ].

	[pc <= end] whileTrue: [
		bytecode := aCompiledMethodOrMethodSpec at: pc.
		
		"Copied from EncoderForV3PlusClosures class >> bytecodeSize:"
		bytecodeSize := true caseOf: {
			[bytecode <= 125] -> [1].
			[bytecode >= 176] -> [1].
			[bytecode >= 160] -> [2].						"long jumps"
			[bytecode >= 144] -> [1].						"short jumps"
			[bytecode >= 128] -> [ 							"extensions"
				#(2 2 2 2 3 2 2 1 1 1 2 3 3 3 3 4) at: bytecode - 127] }.
		
		"Can not handle short (single byte) instance variable access bytecodes at all"
		oldIvar := -1.
		bytecode < 16 ifTrue: [ oldIvar := bytecode + 1 ].
		(bytecode between: 96 and: 103) ifTrue: [ oldIvar := (bytecode bitAnd: 2r111) + 1 ].
		aDictionary at: oldIvar ifPresent: [ :newIvar |
			newIvar = oldIvar ifFalse: [
				aBoolean ifTrue: [self error: 'Can not handle short instance variable access bytecodes'].
				^false ]	].
		
		"Two byte instance variable access bytecodes"
		followingByte := pc+1 <= end ifTrue: [ aCompiledMethodOrMethodSpec at: pc+1 ].
		((#[128 129 130] includes: bytecode) and: [followingByte < 64]) ifTrue: [
			aDictionary at: followingByte+1 ifPresent: [ :newIvar |
				newIvar <= 64 ifFalse: [
					aBoolean ifTrue: [self error: 'Can not convert 2 byte instance variable access bytecodes to 3 byte'].
					^false].
				aBoolean ifTrue: [ aCompiledMethodOrMethodSpec at: pc+1 put: newIvar-1 ]]].

		"Three byte instance variable access bytecodes"
		(bytecode = 132 and: [#[64 160 192] includes: followingByte]) ifTrue: [
			thirdByte := pc+2 <= end ifTrue: [ aCompiledMethodOrMethodSpec at: pc+2 ].
			aDictionary at: thirdByte+1 ifPresent: [ :newIvar |
				aBoolean ifTrue: [ aCompiledMethodOrMethodSpec at: pc+2 put: newIvar-1 ]]].

		pc := pc + bytecodeSize ].

	^true
]

{ #category : 'scanning' }
EncoderForV3PlusClosures class >> scanBlockOrNilForLiteral: aLiteral [
	"Answer a block argument for CompiledMethod>>#scanFor: that answers
	 if the method refers to the literal implicitly via a special bytecode.
	 If the literal is not accessible via a special bytecode, answer nil."
	| value |
	"176-191 	1011iiii 	Send Arithmetic Message #iiii
	 192-207 	1100iiii 	Send Special Message #iiii"
	self flag: #deprecated. "mt: Use CompiledCode>>#implicitLiteralsDo:."
	(aLiteral isSymbol or: [aLiteral isInteger]) ifTrue:
		[value := aLiteral isSymbol
					ifTrue: [176 + ((Smalltalk specialSelectors indexOf: aLiteral ifAbsent: [^nil]) // 2)]
					ifFalse: [(aLiteral between: -1 and: 2) ifFalse: [^nil].
							aLiteral + 117].
		 ^[:byte| byte = value]].
	"112-119 	01110iii 	Push (receiver, true, false, nil, -1, 0, 1, 2) [iii]
	 120-123 	011110ii 	Return (receiver, true, false, nil) [ii] From Message"
	aLiteral == true ifTrue:
		[^[:byte| byte = 113 or: [byte = 121]]].
	aLiteral == false ifTrue:
		[^[:byte| byte = 114 or: [byte = 122]]].
	aLiteral == nil ifTrue:
		[^[:byte| byte = 115 or: [byte = 123]]].
	^nil
]

{ #category : 'instruction stream support' }
EncoderForV3PlusClosures class >> selectorToSendOrItselfFor: anInstructionStream in: method at: pc [
	"If anInstructionStream is at a send bytecode then answer the send's selector,
	 otherwise answer anInstructionStream itself.  The rationale for answering
	 anInstructionStream instead of, say, nil, is that potentially any existing object
	 can be used as a selector, but since anInstructionStream postdates the method,
	 it can't be one of them."

	| byte byte2 |
	byte := method at: pc.
	byte < 131 ifTrue: [^anInstructionStream].
	byte >= 176
		ifTrue: 
			["special byte or short send"
			byte >= 208
				ifTrue: [^method literalAt: (byte bitAnd: 15) + 1]
				ifFalse: [^Smalltalk specialSelectorAt: byte - 176 + 1]]
		ifFalse: 
			[byte <= 134 ifTrue: 
				[byte2 := method at: pc + 1.
				 byte = 131 ifTrue: [^method literalAt: byte2 \\ 32 + 1].
				 byte = 132 ifTrue: [byte2 < 64 ifTrue: [^method literalAt: (method at: pc + 2) + 1]].
				 byte = 133 ifTrue: [^method literalAt: byte2 \\ 32 + 1].
				 byte = 134 ifTrue: [^method literalAt: byte2 \\ 64 + 1]]].
	^anInstructionStream
]

{ #category : 'compiled method support' }
EncoderForV3PlusClosures class >> superSendScanBlockUsing: scanner [
	"Answer a block argument for InstructionStream>>scanFor:
	 that answers true for super sends."
	^[:instr |
	   instr = 16r85
	   or: [instr = 16r84
		and: [scanner followingByte between: 16r20 and: 16r3F]]]
]

{ #category : 'bytecode decoding' }
EncoderForV3PlusClosures class >> unusedBytecode [
	"Answer the opcode of a single-byte unused bytecode, if it exists in the encoder's bytecode set, or nil if not."
	^126
]

{ #category : 'services' }
EncoderForV3PlusClosures class >> withLongInstVarAccessBytecodes: aCompiledMethodOrMethodSpec [
	"This method is provided for compatibility with EncoderForSistaV1.
	However, the desired result can not be produced for the V3 Bytecode Set.
	The reason is the need for a space padding NOP bytecode, missing in V3.
	Answer nil: no conversion possible."

	^nil
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genBranchPopFalse: distance [
	"See BlueBook page 596"
	distance < 0 ifTrue:
		[^self outOfRangeError: 'distance' index: distance range: 0 to: 1023].
	(distance > 0 and: [distance < 9]) ifTrue:
		["152-159 	10011iii 	Pop and Jump 0n False iii +1 (i.e., 1 through 8)"
		 stream nextPut: 152 + distance - 1.
		 ^self].
	distance < 1024 ifTrue:
		["172-175 	101011ii jjjjjjjj 	Pop and Jump On False ii *256+jjjjjjjj"
		 stream
			nextPut: 172 + (distance bitShift: -8);
			nextPut: distance + 1024 \\ 256.
		 ^self].
	^self outOfRangeError: 'distance' index: distance range: 0 to: 1023
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genBranchPopTrue: distance [
	"See BlueBook page 596"
	distance < 0 ifTrue:
		[^self outOfRangeError: 'distance' index: distance range: 0 to: 1023].
	distance < 1024 ifTrue:
		["168-171 	101010ii jjjjjjjj 	Pop and Jump On True ii *256+jjjjjjjj"
		 stream
			nextPut: 168 + (distance bitShift: -8);
			nextPut: distance + 1024 \\ 256.
		 ^self].
	^self outOfRangeError: 'distance' index: distance range: 0 to: 1023
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genCallPrimitive: primitiveIndex [
	"139	11101111	iiiiiiii jjjjjjjj	Call Primitive #iiiiiiii + (jjjjjjjj * 256)"
	(primitiveIndex < 1 or: [primitiveIndex > 65535]) ifTrue: [
		self outOfRangeError: 'primitive index' index: primitiveIndex range: 1 to: 65535].
	stream
		nextPut: 139;
		nextPut: (primitiveIndex bitAnd: 255);
		nextPut: (primitiveIndex bitShift: -8)
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genDup [
	"See BlueBook page 596"
	"136 	10001000 	Duplicate Stack Top"
	stream nextPut: 136
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genJump: distance [
	"See BlueBook page 596"
	(distance > 0 and: [distance < 9]) ifTrue:
		["144-151 	10010iii 	Jump iii + 1 (i.e., 1 through 8)"
		 stream nextPut: 144 + distance - 1.
		 ^self].
	"160-167 	10100iii jjjjjjjj 	Jump(iii - 4) *256+jjjjjjjj"
	^self genJumpLong: distance
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genJumpLong: distance [
	"See BlueBook page 596"
	(distance >= -1024 and: [distance < 1024]) ifTrue:
		["160-167 	10100iii jjjjjjjj 	Jump(iii - 4) *256+jjjjjjjj"
		 stream
			nextPut: 160 + (distance + 1024 bitShift: -8);
			nextPut: distance + 1024 \\ 256.
		 ^self].
	^self outOfRangeError: 'distance' index: distance range: -1024 to: 1023
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genPop [
	"See BlueBook page 596"
	"135 	10000111 	Pop Stack Top"
	stream nextPut: 135
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genPushClosureCopyNumCopiedValues: numCopied numArgs: numArgs jumpSize: jumpSize [
	"143 	10001111 llllkkkk jjjjjjjj iiiiiiii	Push Closure Num Copied llll Num Args kkkk BlockSize jjjjjjjjiiiiiiii"
	(jumpSize < 0 or: [jumpSize > 65535]) ifTrue:
		[^self outOfRangeError: 'block size' index: jumpSize range: 0 to: 65535].
	(numCopied < 0 or: [numCopied > 15]) ifTrue:
		[^self outOfRangeError: 'num copied' index: numCopied range: 0 to: 15].
	(numArgs < 0 or: [numArgs > 15]) ifTrue:
		[^self outOfRangeError: 'num args' index: numArgs range: 0 to: 15].
	stream
		nextPut: 143;
		nextPut: numArgs + (numCopied bitShift: 4);
		nextPut: (jumpSize bitShift: -8);
		nextPut: (jumpSize bitAnd: 16rFF)
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genPushConsArray: size [
	(size < 0 or: [size > 127]) ifTrue:
		[^self outOfRangeError: 'numElements' index: size range: 0 to: 127].
	"138 	10001010 1kkkkkkk 	Push (Array new: kkkkkkk)"
	stream
		nextPut: 138;
		nextPut: size + 128
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genPushInstVar: instVarIndex [
	"Cuis Specific.
	In order of allowing the quick remapping of instance variables, leave some 'slack'.
	See
		#displaceInstVarAccessesIn:starting:by:doIt:
		#remapInstVarAccessesIn:asPer:doIt:
	"
	true ifTrue: [
		^self genPushInstVarAllowForIvarsRemapping: instVarIndex ].

	"See BlueBook page 596"
	instVarIndex >= 0 ifTrue:
		[instVarIndex < 16 ifTrue:
			["0-15 	0000iiii 	Push Receiver Variable #iiii"
			 stream nextPut: 0 + instVarIndex.
			 ^self].
		instVarIndex < 64 ifTrue:
			["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
			 stream
				nextPut: 128;
				nextPut: instVarIndex.
			 ^self]].
	self genPushInstVarLong: instVarIndex.
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genPushInstVarAllowForIvarsRemapping: instVarIndex [
	"Cuis Specific.
	In order of allowing the quick remapping of instance variables, leave some 'slack'.
	See
		#displaceInstVarAccessesIn:starting:by:doIt:
		#remapInstVarAccessesIn:asPer:doIt:
	The strategy for the V3 Bytecode Set is as follows:
		- never use the single byte bytecodes in [0..15] and [96..103]
		- Use 2 byte bytecodes 128, 129, 130 for ivar accesses up to 9 (even if they can handle up to 63)
		- Use 3 byte bytecode 132 for all ivar accesses, if ivar index >= 10
	This means that the first 10 ivars can be shifted by adding up to 54 ivars in superclasses, and
	other ivars can be shifted by adding any number of ivars in superclasses (up to the 255 ivars limit).
	This is needed for binding class shape modifying DynamicCuisLibraries, or 
	DynamicCuisLibraries built with a different class shape than the image loading them.
	The reason for using 128, 129 and 130 bytecodes for the first ivars is because the VM uses bytecode 132
	with small ivar index to indicate an access to ivars of MethodContext or ContextPart that require a more
	expensive processing. Using 128, 129 or 130 avoids this extra cost.
	This applies both to Spur Interpreter and Cog jitter.
	In the VMMaker package see #isReadMediatedContextInstVarIndex: and #isWriteMediatedContextInstVarIndex:
	The space cost of the use of these longer bytecodes is very small: about 34kb in the official Cuis image, or
	2 bytes per CompiledMethod in average.
	"

	"See BlueBook page 596"
	(instVarIndex >= 0 and: [instVarIndex < 10]) ifTrue: [
		"128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: instVarIndex.
		 ^self ].
	self genPushInstVarLong: instVarIndex.
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genPushInstVarLong: instVarIndex [
	"See BlueBook page 596"
	"See also MaybeContextInstanceVariableNode"
	(instVarIndex >= 0 and: [instVarIndex < 256]) ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 64;
			nextPut: instVarIndex.
		 ^self].
	^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 255
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genPushLiteral: literalIndex [
	"See BlueBook page 596"
	literalIndex < 0 ifTrue: 
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255].
	literalIndex < 32 ifTrue: 
		["32-63 	001iiiii 	Push Literal Constant #iiiii"
		 stream nextPut: 32 + literalIndex.
		 ^self].
	literalIndex < 64 ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: 128 + literalIndex.
		 ^self].
	literalIndex < 256 ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 96;
			nextPut: literalIndex.
		 ^self].
	^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genPushLiteralVar: literalIndex [
	"See BlueBook page 596"
	literalIndex < 0 ifTrue: 
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255].
	literalIndex < 32 ifTrue: 
		["64-95 	010iiiii 	Push Literal Variable #iiiii"
		 stream nextPut: 64 + literalIndex.
		 ^self].
	literalIndex < 64 ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: 192 + literalIndex.
		 ^self].
	literalIndex < 256 ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 128;
			nextPut: literalIndex.
		 ^self].
	^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genPushNewArray: size [
	(size < 0 or: [size > 127]) ifTrue:
		[^self outOfRangeError: 'numElements' index: size range: 0 to: 127].
	"138 	10001010 0kkkkkkk 	Pop kkkkkkk into: (Array new: kkkkkkk)"
	stream
		nextPut: 138;
		nextPut: size
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genPushReceiver [
	"See BlueBook page 596"
	"112-119 	01110iii 	Push (receiver, true, false, nil, -1, 0, 1, 2) [iii]"
	stream nextPut: 112
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genPushRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	(tempIndex >= 0 and: [tempIndex < 256
	 and: [tempVectorIndex >= 0 and: [tempVectorIndex < 256]]]) ifTrue:
		["140 	10001100 kkkkkkkk jjjjjjjj 	Push Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"
		 stream
			nextPut: 140;
			nextPut: tempIndex;
			nextPut: tempVectorIndex.
		 ^self].
	tempIndex >= 256 ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	tempVectorIndex >= 256 ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 255]
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genPushSpecialLiteral: aLiteral [
	"112-119 	01110iii 	Push (receiver, true, false, nil, -1, 0, 1, 2) [iii]"
	| index |
	index := #(true false nil -1 0 1 2) indexOf: aLiteral ifAbsent: 0.
	index = 0 ifTrue:
		[^self error: 'push special literal: ', aLiteral printString,  ' is not one of true false nil -1 0 1 2'].
	stream nextPut: index + 112
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genPushTemp: tempIndex [
	"See BlueBook page 596"
	tempIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	tempIndex < 16 ifTrue: 
		["16-31 	0001iiii 	Push Temporary Location #iiii"
		 stream nextPut: 16 + tempIndex.
		 ^self].
	tempIndex < 64 ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: 64 + tempIndex.
		 ^self].
	^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genPushTempLong: tempIndex [
	"See BlueBook page 596"
	(tempIndex >= 0 and: [tempIndex < 64]) ifTrue: 
		["128 	10000000 jjkkkkkk 	Push (Receiver Variable, Temporary Location, Literal Constant, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 128;
			nextPut: 64 + tempIndex.
		 ^self].
	^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genPushThisContext [
	"See BlueBook page 596"
	"137 	10001001 	Push Active Context"
	stream nextPut: 137
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genReturnReceiver [
	"See BlueBook page 596"
	"120-123 	011110ii 	Return (receiver, true, false, nil) [ii] From Message"
	stream nextPut: 120
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genReturnSpecialLiteral: aLiteral [
	"120-123 	011110ii 	Return (receiver, true, false, nil) [ii] From Message"
	| index |
	index := #(true false nil) indexOf: aLiteral ifAbsent: 0.
	index = 0 ifTrue:
		[^self error: 'return special literal: ', aLiteral printString,  ' is not one of true false nil'].
	stream nextPut: 120 + index
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genReturnTop [
	"See BlueBook page 596"
	"124-125 	0111110i 	Return Stack Top From (Message, Block) [i]"
	stream nextPut: 124
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genReturnTopToCaller [
	"See BlueBook page 596"
	"124-125 	0111110i 	Return Stack Top From (Message, Block) [i]"
	stream nextPut: 125
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genSend: selectorLiteralIndex numArgs: nArgs [
	"See BlueBook page 596 (with exceptions for 132 & 134)"
	nArgs < 0 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31 "!!"].
	selectorLiteralIndex < 0 ifTrue:
		["Special selector sends.
			176-191 	1011iiii 	Send Arithmetic Message #iiii
			192-207 	1100iiii 	Send Special Message #iiii"
		self flag: #yuck.
		 (selectorLiteralIndex negated between: 176 and: 207) ifFalse:
			[^self outOfRangeError: 'special selector code' index: selectorLiteralIndex negated range: 176 to: 207].
		 stream nextPut: selectorLiteralIndex negated.
		 ^self].
	(selectorLiteralIndex < 16 and: [nArgs < 3]) ifTrue:
		["	208-223 	1101iiii 	Send Literal Selector #iiii With No Arguments
			224-239 	1110iiii 	Send Literal Selector #iiii With 1 Argument
			240-255 	1111iiii 	Send Literal Selector #iiii With 2 Arguments"
		 stream nextPut: 208 + (nArgs * 16) + selectorLiteralIndex.
		 ^self].
	(selectorLiteralIndex < 32 and: [nArgs < 8]) ifTrue: 
		["	131 	10000011 jjjkkkkk 	Send Literal Selector #kkkkk With jjj Arguments"
		 stream
			nextPut: 131;
			nextPut: ((nArgs bitShift: 5) + selectorLiteralIndex).
		 ^self].
	(selectorLiteralIndex < 64 and: [nArgs < 4]) ifTrue: 
	 	["In Squeak V3
			134 	10000110 jjjjjjjj kkkkkkkk 	Send Literal Selector #kkkkkkkk To Superclass With jjjjjjjj Arguments
		 is replaced by
			134 	10000110 jjkkkkkk 	Send Literal Selector #kkkkkk With jj Arguments"
		 stream
			nextPut: 134;
			nextPut: ((nArgs bitShift: 6) + selectorLiteralIndex).
		 ^self].
	(selectorLiteralIndex < 256 and: [nArgs < 32]) ifTrue: 
		["In Squeak V3
			132 	10000100 jjjjjjjj kkkkkkkk 	Send Literal Selector #kkkkkkkk With jjjjjjjj Arguments
		  is replaced by
			132 	10000100 ooojjjjj kkkkkkkk
				ooo = 0 => Send Literal Selector #kkkkkkkk With jjjjj Arguments
				ooo = 1 => Send Literal Selector #kkkkkkkk To Superclass With jjjjj Arguments"
		stream
			nextPut: 132;
			nextPut: nArgs;
			nextPut: selectorLiteralIndex.
		 ^self].
	nArgs >= 32 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31].
	selectorLiteralIndex >= 256 ifTrue: 
		[^self outOfRangeError: 'selector literal index' index: selectorLiteralIndex range: 0 to: 255]
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genSendSpecial: specialSelectorIndex numArgs: nArgs [
	"See BlueBook page 596"
	self assert: (specialSelectorIndex between: 1 and: Smalltalk specialSelectorSize).
	self assert: nArgs = (Smalltalk specialNargsAt: specialSelectorIndex).
	"Special selector sends.
		176-191 	1011iiii 	Send Arithmetic Message #iiii
		192-207 	1100iiii 	Send Special Message #iiii"
	stream nextPut: specialSelectorIndex + 175
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genSendSuper: selectorLiteralIndex numArgs: nArgs [
	"See BlueBook page 596 (with exceptions for 132 & 134)"
	nArgs < 0 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31 "!!"].
	selectorLiteralIndex < 0 ifTrue:
		[^self outOfRangeError: 'selector literal index' index: selectorLiteralIndex range: 0 to: 255].
	(selectorLiteralIndex < 32 and: [nArgs < 8]) ifTrue: 
		["	133 	10000011 jjjkkkkk 	Send Literal Selector #kkkkk To Superclass With jjj Arguments"
		 stream
			nextPut: 133;
			nextPut: ((nArgs bitShift: 5) + selectorLiteralIndex).
		 ^self].
	(selectorLiteralIndex < 256 and: [nArgs < 32]) ifTrue: 
		["In Squeak V3
			132 	10000100 jjjjjjjj kkkkkkkk 	Send Literal Selector #kkkkkkkk With jjjjjjjj Arguments
		  is replaced by
			132 	10000100 ooojjjjj kkkkkkkk
				ooo = 0 => Send Literal Selector #kkkkkkkk With jjjjj Arguments
				ooo = 1 => Send Literal Selector #kkkkkkkk To Superclass With jjjjj Arguments"
		stream
			nextPut: 132;
			nextPut: 32 + nArgs;
			nextPut: selectorLiteralIndex.
		 ^self].
	nArgs >= 32 ifTrue:
		[^self outOfRangeError: 'numArgs' index: nArgs range: 0 to: 31].
	selectorLiteralIndex >= 256 ifTrue: 
		[^self outOfRangeError: 'selector literal index' index: selectorLiteralIndex range: 0 to: 255]
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genStoreInstVar: instVarIndex [
	"Cuis Specific.
	In order of allowing the quick remapping of instance variables, leave some 'slack'.
	See
		#displaceInstVarAccessesIn:starting:by:doIt:
		#remapInstVarAccessesIn:asPer:doIt:
	"
	true ifTrue: [
		^self genStoreInstVarAllowForIvarsRemapping: instVarIndex ].

	"See BlueBook page 596"
	(instVarIndex >= 0 and: [instVarIndex < 64]) ifTrue: 
		["129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 129;
			nextPut: instVarIndex.
		 ^self].
	self genStoreInstVarLong: instVarIndex
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genStoreInstVarAllowForIvarsRemapping: instVarIndex [
	"Cuis Specific.
	In order of allowing the quick remapping of instance variables, leave some 'slack'.
	See
		#displaceInstVarAccessesIn:starting:by:doIt:
		#remapInstVarAccessesIn:asPer:doIt:
	The strategy for the V3 Bytecode Set is as follows:
		- never use the single byte bytecodes in [0..15] and [96..103]
		- Use 2 byte bytecodes 128, 129, 130 for ivar accesses up to 9 (even if they can handle up to 63)
		- Use 3 byte bytecode 132 for all ivar accesses, if ivar index >= 10
	This means that the first 10 ivars can be shifted by adding up to 54 ivars in superclasses, and
	other ivars can be shifted by adding any number of ivars in superclasses (up to the 255 ivars limit).
	This is needed for binding class shape modifying DynamicCuisLibraries, or 
	DynamicCuisLibraries built with a different class shape than the image loading them.
	The reason for using 128, 129 and 130 bytecodes for the first ivars is because the VM uses bytecode 132
	with small ivar index to indicate an access to ivars of MethodContext or ContextPart that require a more
	expensive processing. Using 128, 129 or 130 avoids this extra cost.
	This applies both to Spur Interpreter and Cog jitter.
	In the VMMaker package see #isReadMediatedContextInstVarIndex: and #isWriteMediatedContextInstVarIndex:
	The space cost of the use of these longer bytecodes is very small: about 34kb in the official Cuis image, or
	2 bytes per CompiledMethod in average.
	"

	"See BlueBook page 596"
	(instVarIndex >= 0 and: [instVarIndex < 10]) ifTrue: [
		"129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 129;
			nextPut: instVarIndex.
		 ^self ].
	self genStoreInstVarLong: instVarIndex.
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genStoreInstVarLong: instVarIndex [
	"See BlueBook page 596"
	"See also MaybeContextInstanceVariableNode"
	(instVarIndex >= 0 and: [instVarIndex < 256]) ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 160;
			nextPut: instVarIndex.
		 ^self].
	^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 255
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genStoreLiteralVar: literalIndex [
	"See BlueBook page 596"
	literalIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255].
	literalIndex < 64 ifTrue: 
		["129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 129;
			nextPut: 192 + literalIndex.
		 ^self].
	literalIndex < 256 ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 224;
			nextPut: literalIndex.
		 ^self].
	^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genStorePopInstVar: instVarIndex [
	"Cuis Specific.
	In order of allowing the quick remapping of instance variables, leave some 'slack'.
	See
		#displaceInstVarAccessesIn:starting:by:doIt:
		#remapInstVarAccessesIn:asPer:doIt:
	"
	true ifTrue: [
		^self genStorePopInstVarAllowForIvarsRemapping: instVarIndex ].

	"See BlueBook page 596"
	instVarIndex >= 0 ifTrue:
		[instVarIndex < 8 ifTrue:
			["96-103 	01100iii 	Pop and Store Receiver Variable #iii"
			 stream nextPut: 96 + instVarIndex.
			 ^self].
		instVarIndex < 64 ifTrue:
			["130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
			 stream
				nextPut: 130;
				nextPut: instVarIndex.
			 ^self]].
	self genStorePopInstVarLong: instVarIndex
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genStorePopInstVarAllowForIvarsRemapping: instVarIndex [
	"Cuis Specific.
	In order of allowing the quick remapping of instance variables, leave some 'slack'.
	See
		#displaceInstVarAccessesIn:starting:by:doIt:
		#remapInstVarAccessesIn:asPer:doIt:
	The strategy for the V3 Bytecode Set is as follows:
		- never use the single byte bytecodes in [0..15] and [96..103]
		- Use 2 byte bytecodes 128, 129, 130 for ivar accesses up to 9 (even if they can handle up to 63)
		- Use 3 byte bytecode 132 for all ivar accesses, if ivar index >= 10
	This means that the first 10 ivars can be shifted by adding up to 54 ivars in superclasses, and
	other ivars can be shifted by adding any number of ivars in superclasses (up to the 255 ivars limit).
	This is needed for binding class shape modifying DynamicCuisLibraries, or 
	DynamicCuisLibraries built with a different class shape than the image loading them.
	The reason for using 128, 129 and 130 bytecodes for the first ivars is because the VM uses bytecode 132
	with small ivar index to indicate an access to ivars of MethodContext or ContextPart that require a more
	expensive processing. Using 128, 129 or 130 avoids this extra cost.
	This applies both to Spur Interpreter and Cog jitter.
	In the VMMaker package see #isReadMediatedContextInstVarIndex: and #isWriteMediatedContextInstVarIndex:
	The space cost of the use of these longer bytecodes is very small: about 34kb in the official Cuis image, or
	2 bytes per CompiledMethod in average.
	"

	"See BlueBook page 596"
	(instVarIndex >= 0 and: [instVarIndex < 10]) ifTrue: [
		"130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 130;
			nextPut: instVarIndex.
		 ^self ].
	self genStorePopInstVarLong: instVarIndex.
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genStorePopInstVarLong: instVarIndex [
	"See BlueBook page 596"
	"See also MaybeContextInstanceVariableNode"
	(instVarIndex >= 0 and: [instVarIndex < 256]) ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 192;
			nextPut: instVarIndex.
		 ^self].
	^self outOfRangeError: 'index' index: instVarIndex range: 0 to: 255
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genStorePopLiteralVar: literalIndex [
	"See BlueBook page 596"
	literalIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255].
	literalIndex < 64 ifTrue: 
		["130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 130;
			nextPut: 192 + literalIndex.
		 ^self].
	literalIndex < 256 ifTrue: 
		["132 	10000100 iiijjjjj kkkkkkkk 	(Send, Send Super, Push Receiver Variable, Push Literal Constant, Push Literal Variable, Store Receiver Variable, Store-Pop Receiver Variable, Store Literal Variable)[iii] #kkkkkkkk jjjjj"
		 stream
			nextPut: 132;
			nextPut: 224;
			nextPut: literalIndex.
		 self genPop.
		 ^self].
	^self outOfRangeError: 'index' index: literalIndex range: 0 to: 255
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genStorePopRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	"142 	10001110 kkkkkkkk jjjjjjjj 	Pop and Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"
	(tempIndex >= 0 and: [tempIndex < 256
	 and: [tempVectorIndex >= 0 and: [tempVectorIndex < 256]]]) ifTrue:
		[stream
			nextPut: 142;
			nextPut: tempIndex;
			nextPut: tempVectorIndex.
		 ^self].
	tempIndex >= 256 ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	tempVectorIndex >= 256 ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 255]
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genStorePopTemp: tempIndex [
	"See BlueBook page 596"
	tempIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	tempIndex < 8 ifTrue: 
		["104-111 	01101iii 	Pop and Store Temporary Location #iii"
		 stream nextPut: 104 + tempIndex.
		 ^self].
	tempIndex < 64 ifTrue: 
		["130 	10000010 jjkkkkkk 	Pop and Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 130;
			nextPut: 64 + tempIndex.
		 ^self].
	^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genStoreRemoteTemp: tempIndex inVectorAt: tempVectorIndex [
	"141 	10001101 kkkkkkkk jjjjjjjj 	Store Temp At kkkkkkkk In Temp Vector At: jjjjjjjj"
	(tempIndex >= 0 and: [tempIndex < 256
	 and: [tempVectorIndex >= 0 and: [tempVectorIndex < 256]]]) ifTrue:
		[stream
			nextPut: 141;
			nextPut: tempIndex;
			nextPut: tempVectorIndex.
		 ^self].
	tempIndex >= 256 ifTrue:
		[^self outOfRangeError: 'remoteTempIndex' index: tempIndex range: 0 to: 255].
	tempVectorIndex >= 256 ifTrue:
		[^self outOfRangeError: 'tempVectorIndex' index: tempVectorIndex range: 0 to: 255]
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> genStoreTemp: tempIndex [
	"See BlueBook page 596"
	tempIndex < 0 ifTrue:
		[^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63].
	tempIndex < 64 ifTrue: 
		["129 	10000001 jjkkkkkk 	Store (Receiver Variable, Temporary Location, Illegal, Literal Variable) [jj] #kkkkkk"
		 stream
			nextPut: 129;
			nextPut: 64 + tempIndex.
		 ^self].
	^self outOfRangeError: 'index' index: tempIndex range: 0 to: 63
]

{ #category : 'special literal encodings' }
EncoderForV3PlusClosures >> isSpecialLiteralForPush: literal [
	^literal isInteger
		ifFalse:
			[false == literal
			 or: [true == literal
			 or: [nil == literal]]]
		ifTrue: [literal between: -1 and: 2]
]

{ #category : 'bytecode generation' }
EncoderForV3PlusClosures >> maxIndexableLiterals [
	"This bytecode set can index up to 256 literals."
	^256
]

{ #category : 'testing' }
EncoderForV3PlusClosures >> supportsFullBlocks [
	"Answer if the instruction set supports full closures (closure creation from
	 specfic methods instead of bytecodes embedded in an outer home method)."
	
	^false
]
