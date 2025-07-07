'From Cuis7.5 [latest update: #7278] on 19 June 2025 at 10:13:46 am'!

!CompiledMethod class methodsFor: 'method encoding' stamp: 'jmv 6/19/2025 10:12:44'!
headerFlagForEncoderClass: anEncoderClass
	
	(anEncoderClass includesBehavior: PrimaryBytecodeSetEncoderClass) ifTrue: [^0].
	(anEncoderClass includesBehavior: SecondaryBytecodeSetEncoderClass) ifTrue: [^SmallInteger minVal].
	
	self error: 'The encoder is not one of the two installed bytecode sets'! !


!BytecodeEncoder methodsFor: 'method generation' stamp: 'jmv 6/19/2025 10:12:56'!
computeMethodHeaderForNumArgs: numArgs numTemps: numTemps numLits: numLits primitive: primitiveIndex
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
		+ numLits! !


!CompiledMethod class methodsFor: 'instance creation' stamp: 'jmv 6/19/2025 10:13:30'!
newBytes: numberOfBytes trailerBytes: trailer nArgs: nArgs nTemps: nTemps nStack: stackSize nLits: nLits primitive: primitiveIndex
	"Answer an instance of me. The header is specified by the message 
	 arguments. The remaining parts are not as yet determined."
	| method pc |
"15 | 63 | 32767"
	nArgs > 15
		ifTrue: [^self error: 'Cannot compile -- too many arguments'].
	nTemps > 63
		ifTrue: [^self error: 'Cannot compile -- too many temporary variables'].	
	nLits > 32767
		ifTrue: [^self error: 'Cannot compile -- too many literals'].

	method := self
				createMethod: numberOfBytes
				trailer: trailer
				header:  (CompiledMethod headerFlagForEncoderClass: PreferredBytecodeSetEncoderClass)
						+ (nArgs bitShift: 24)
						+ (nTemps bitShift: 18)
						+ ((nTemps + stackSize) > SmallFrame ifTrue: [1 bitShift: 17] ifFalse: [0])
						+ nLits
						+ (primitiveIndex > 0 ifTrue: [1 bitShift: 16] ifFalse: [0]).
	primitiveIndex > 0 ifTrue: [
		pc := method initialPC.
		method
			at: pc + 0 put: method encoderClass callPrimitiveCode;
			at: pc + 1 put: (primitiveIndex bitAnd: 16rFF);
			at: pc + 2 put: (primitiveIndex bitShift: -8)].
	^method! !

!methodRemoval: CompiledMethod class #headerFlagForEncoder: stamp: 'jmv 6/19/2025 10:13:02'!
CompiledMethod class removeSelector: #headerFlagForEncoder:!
