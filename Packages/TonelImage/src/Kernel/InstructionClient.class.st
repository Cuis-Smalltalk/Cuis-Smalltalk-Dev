"
My job is to make it easier to implement clients for InstructionStream. See InstVarRefLocator
as an example. 
"
Class {
	#name : 'InstructionClient',
	#superclass : 'ImplicitLiteralInstructionClientHook',
	#category : 'Kernel-Methods',
	#package : 'Kernel',
	#tag : 'Methods'
}

{ #category : 'instruction decoding' }
InstructionClient >> blockReturnTop [
	"Return Top Of Stack bytecode."


]

{ #category : 'instruction decoding' }
InstructionClient >> callPrimitive: pimIndex [
	"V3PlusClosures:	139 10001011	iiiiiiii   jjjjjjjj  Call Primitive #iiiiiiii + (jjjjjjjj * 256)
	 NewsqueakV4:		249 11111001	iiiiiiii   jjjjjjjj  Call Primitive #iiiiiiii + (jjjjjjjj * 256)
	 SistaV1:			248 11111000 iiiiiiii mjjjjjjj  Call Primitive #iiiiiiii + ( jjjjjjj * 256)
							m=1 means inlined primitive, no hard return after execution."
]

{ #category : 'instruction decoding' }
InstructionClient >> doDup [
	"Duplicate Top Of Stack bytecode."


]

{ #category : 'instruction decoding' }
InstructionClient >> doNop [
	"Do Nothing Operation bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> doPop [
	"Remove Top Of Stack bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> jump: offset [
	"Unconditional Jump bytecode."


]

{ #category : 'instruction decoding' }
InstructionClient >> jump: offset if: condition  [
	"Conditional Jump bytecode."


]

{ #category : 'instruction decoding' }
InstructionClient >> methodReturnConstant: value  [
	"Return Constant bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> methodReturnReceiver [
	"Return Self bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> methodReturnTop [
	"Return Top Of Stack bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> popIntoLiteralVariable: anAssociation  [
	"Remove Top Of Stack And Store Into Literal Variable bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> popIntoReceiverVariable: offset  [
	"Remove Top Of Stack And Store Into Instance Variable bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> popIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex [
	"Remove Top Of Stack And Store Into Offset of Temp Vector bytecode."
]

{ #category : 'instruction decoding' }
InstructionClient >> popIntoTemporaryVariable: offset  [
	"Remove Top Of Stack And Store Into Temporary Variable bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> pushActiveContext [
	"Push Active Context On Top Of Its Own Stack bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize [
	"Push Closure bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> pushConsArrayWithElements: numElements [
	"Push Cons Array of size numElements popping numElements items from the stack into the array bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> pushConstant: value [
	"Push Constant, value, on Top Of Stack bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> pushLiteralVariable: anAssociation [
	"Push Contents Of anAssociation On Top Of Stack bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> pushNewArrayOfSize: numElements  [
	"Push New Array of size numElements bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> pushReceiver [
	"Push Active Context's Receiver on Top Of Stack bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> pushReceiverVariable: offset [
	"Push Contents Of the Receiver's Instance Variable Whose Index 
	is the argument, offset, On Top Of Stack bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> pushRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex [
	"Push Contents at Offset in Temp Vector bytecode."
]

{ #category : 'instruction decoding' }
InstructionClient >> pushTemporaryVariable: offset [
	"Push Contents Of Temporary Variable Whose Index Is the 
	argument, offset, On Top Of Stack bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> send: selector super: supered numArgs: numberArguments [
	"Send Message With Selector, selector, bytecode. The argument, 
	supered, indicates whether the receiver of the message is specified with 
	'super' in the source method. The arguments of the message are found in 
	the top numArguments locations on the stack and the receiver just 
	below them."

]

{ #category : 'instruction decoding' }
InstructionClient >> storeIntoLiteralVariable: anAssociation  [
	"Store Top Of Stack Into Literal Variable Of Method bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> storeIntoReceiverVariable: offset  [
	"Store Top Of Stack Into Instance Variable Of Method bytecode."

]

{ #category : 'instruction decoding' }
InstructionClient >> storeIntoRemoteTemp: remoteTempIndex inVectorAt: tempVectorIndex [
	"Store Top Of Stack And Store Into Offset of Temp Vector bytecode."
]

{ #category : 'instruction decoding' }
InstructionClient >> storeIntoTemporaryVariable: offset  [
	"Store Top Of Stack Into Temporary Variable Of Method bytecode."

]
