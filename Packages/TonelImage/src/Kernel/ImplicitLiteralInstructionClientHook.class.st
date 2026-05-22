"
ImplicitLiteralInstructionClientHook is a hook to allow clients to intercept bytecodes referencing implicit literals, such as special selector sends which have an implicit selector in Smalltalk specialSelectors.  It implements the messages sent by InstructionStream's interpretNextXXXInstructionFor: methods for bytecodes that reference implicit literals as sends of the relevant explciit literal methods.  This allows clients that want to to intercept implicit literal messages while leaving existing clients unchanged.

The class also supports CompiledCode>>abstractBytecodeMessagesFrom:to:do: et al which depend on MessageNotUnderstood to collect bytecode messages.  Use of instances of this class hides the existence of pushSpecialConstant: and sendSpecial:numArgs: since these are understood, but their sends of pushConstant: and send:super:numArgs: are not.

Instance Variables
"
Class {
	#name : 'ImplicitLiteralInstructionClientHook',
	#superclass : 'Object',
	#category : 'Kernel-Methods',
	#package : 'Kernel',
	#tag : 'Methods'
}

{ #category : 'instruction decoding - implicit literals' }
ImplicitLiteralInstructionClientHook >> methodReturnSpecialConstant: value [
	"This is a hook to allow clients to intercept implicit literal bytecodes"
	^self methodReturnConstant: value
]

{ #category : 'instruction decoding - implicit literals' }
ImplicitLiteralInstructionClientHook >> pushSpecialConstant: value [
	"This is a hook to allow clients to intercept implicit literal bytecodes"
	^self pushConstant: value
]

{ #category : 'instruction decoding - implicit literals' }
ImplicitLiteralInstructionClientHook >> sendSpecial: selector numArgs: numArgs [
	"This is a hook to allow clients to intercept special selector sends (which have an implicit selector)"
	^self send: selector super: false numArgs: numArgs
]
