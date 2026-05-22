"
ImplicitLiteralFinder is a means to extract special literals from an instruction stream.  Instances of InstructionClient answer self in response to all bytecode decode messages.  Instances of this class answer the literal itself for implicit literal bytecode messages.

Instance Variables

"
Class {
	#name : 'ImplicitLiteralFinder',
	#superclass : 'InstructionClient',
	#category : 'Kernel-Methods',
	#package : 'Kernel',
	#tag : 'Methods'
}

{ #category : 'instruction decoding - implicit literals' }
ImplicitLiteralFinder >> methodReturnSpecialConstant: value [
	^value
]

{ #category : 'instruction decoding - implicit literals' }
ImplicitLiteralFinder >> pushSpecialConstant: value [
	^value
]

{ #category : 'instruction decoding - implicit literals' }
ImplicitLiteralFinder >> sendSpecial: selector numArgs: numArgs [
	^selector
]
