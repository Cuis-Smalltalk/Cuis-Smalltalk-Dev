'From Cuis7.3 [latest update: #7008] on 25 January 2025 at 2:37:36 pm'!

!Float64Array methodsFor: 'printing' stamp: 'eem 1/25/2025 14:36:19'!
printAsLiteralOn: aStream

	aStream nextPut: $#; nextPut: $[.
	self
		do: [ :each | each storeOn: aStream ]
		separatedBy: [ aStream nextPut: $ ].
	aStream nextPut: $]! !


!Float64Array methodsFor: 'testing' stamp: 'eem 1/25/2025 14:33:47'!
isLiteral
	"so that 
	#(1 #[1.0 2 3] 5)
	prints itself"
	^self class == Float64Array and: [self allSatisfy: [:ea| ea isFinite]]! !

!Float64Array methodsFor: 'printing' stamp: 'eem 1/25/2025 14:37:02'!
storeOn: aStream

	self isLiteral
		ifTrue: [self printAsLiteralOn: aStream]
		ifFalse: [super storeOn: aStream]! !


!LiteralNode methodsFor: 'printing' stamp: 'eem 1/25/2025 14:31:06'!
printOn: aStream indent: level
	key isVariableBinding
		ifTrue: [
			key key isNil
				ifTrue: [
					aStream
						nextPutAll: '###';
						nextPutAll: key value soleInstance name ]
				ifFalse: [
					aStream
						nextPutAll: '##';
						nextPutAll: key key ]]
		ifFalse: [
			key isLiteral
				ifTrue: [ | isComplex |
					isComplex := false.
					key isArray ifTrue: [
						isComplex := key anySatisfy: [ :ea |
							ea isArray ]].
					"Is it complex? (i.e. array of arrays)"
					isComplex
						ifTrue: [
							aStream
								nextPut: $#;
								nextPut: $(.
							key do: [ :ea |
								aStream newLineTab: (1 max: level + 1).
								ea printAsLiteralOn: aStream ].
							aStream newLineTab: (1 max: level).
							aStream nextPut: $) ]
						ifFalse: [ key printAsLiteralOn: aStream ]]
				ifFalse: [
					"Need to generate code for stuff that is in a CompiledMethod literal
					but is not understood as a literal by the Compiler.
					Well, then it is because it was generated using backticks!!"
					aStream nextPut: $`.
					key storeOn: aStream.
					aStream nextPut: $` ]]! !


!InstructionStream methodsFor: 'decoding - private - sista v1' stamp: 'eem 1/25/2025 14:19:21'!
interpretNext2ByteSistaV1Instruction: bytecode for: client extA: extA extB: extB startPC: startPC
	"Send to the argument, client, a message that specifies the next instruction.
	 This method handles the two-byte codes.
	 For a table of the bytecode set, see EncoderForV1's class comment."

	| byte method |
	method := self method.
	byte := self method at: pc.
	pc := pc + 1.
	"We do an inline quasi-binary search on bytecode"
	bytecode < 234 ifTrue: "pushes"
		[bytecode < 231 ifTrue:
			[bytecode < 229 ifTrue:
				[| literal |
				 bytecode = 226 ifTrue:
					[^client pushReceiverVariable: (extA bitShift: 8) + byte].
				 literal := method literalAt: (extA bitShift: 8) + byte + 1.
				 bytecode = 227 ifTrue:
					[^client pushLiteralVariable: literal].
				 ^client pushConstant: literal].
			bytecode = 229 ifTrue:
				[^client pushTemporaryVariable: byte].
			^self unusedBytecode: client at: startPC].
		bytecode = 231 ifTrue:
			[^byte < 128
				ifTrue: [client pushNewArrayOfSize: byte]
				ifFalse: [client pushConsArrayWithElements: byte - 128]].
		bytecode = 232 ifTrue:
			[^client pushSpecialConstant: ((extB < 128 ifTrue: [extB] ifFalse: [extB - 256]) bitShift: 8) + byte].
		^client pushSpecialConstant: (Character value: ((extA bitAnd: 16rFF) bitShift: 8) + byte)].
	bytecode < 240 ifTrue: "sends, trap and jump"
		[bytecode < 236 ifTrue: "sends"
			[(bytecode = 235 and: [extB >= 64]) ifTrue:
				[^client
					directedSuperSend: (method literalAt: (extA bitShift: 5) + (byte // 8) + 1)
					numArgs: (extB - 64 bitShift: 3) + (byte \\ 8)].
			 ^client
				send: (method literalAt: (extA bitShift: 5) + (byte // 8) + 1)
				super: bytecode = 235
				numArgs: (extB bitShift: 3) + (byte \\ 8)].
		 bytecode = 236 ifTrue:
			[^client callMappedInlinedPrimitive: byte].
		bytecode = 237 ifTrue:
			[^client jump: (extB bitShift: 8) + byte].
		 ^client jump: (extB bitShift: 8) + byte if: bytecode = 238].
	bytecode < 243 ifTrue:
		[bytecode = 240 ifTrue:
			[^client popIntoReceiverVariable: (extA bitShift: 8) + byte].
		 bytecode = 241 ifTrue:
			[^client popIntoLiteralVariable: (method literalAt: (extA bitShift: 8) + byte + 1)].
		 ^client popIntoTemporaryVariable: byte].
	bytecode = 243 ifTrue:
		[^client storeIntoReceiverVariable: (extA bitShift: 8) + byte].
	bytecode = 244 ifTrue:
		[^client storeIntoLiteralVariable: (method literalAt: (extA bitShift: 8) + byte + 1)].
	bytecode = 245 ifTrue:
		[^client storeIntoTemporaryVariable: byte].
	"246-247	1111011 i	xxxxxxxx	UNASSIGNED"
	^self unusedBytecode: client at: startPC! !

!EncoderForSistaV1 class methodsFor: 'testing' stamp: 'eem 1/25/2025 18:20:09'!
canBeSpecialLiteral: aLiteral
	"This check can be used to prevent unnecessary use of #scanBlockOrNilForLiteral:."

	aLiteral isVariableBinding ifTrue: [^false]. "a common case; don't waste time analysing..."

	aLiteral isSymbol ifTrue: [^ Smalltalk specialSelectors identityIncludes: aLiteral].
	aLiteral isCharacter ifTrue: [^ aLiteral codePoint <= 65535].
	aLiteral isInteger ifTrue: [^ aLiteral between: -32768 and: 32767].

	aLiteral == true ifTrue: [^ true].
	aLiteral == false ifTrue: [^ true].
	aLiteral == nil ifTrue: [^ true].

	^ false! !
