Class {
	#name : 'BlockStartLocator',
	#superclass : 'InstructionClient',
	#instVars : [
		'nextJumpIsAroundBlock'
	],
	#category : 'Kernel-Methods',
	#package : 'Kernel',
	#tag : 'Methods'
}

{ #category : 'initialization' }
BlockStartLocator >> initialize [
	nextJumpIsAroundBlock := false
]

{ #category : 'instruction decoding' }
BlockStartLocator >> jump: offset [
	"If this jump is around a block answer the size of that block."

	nextJumpIsAroundBlock ifTrue:
		[nextJumpIsAroundBlock := false.
		 ^offset]
]

{ #category : 'instruction decoding' }
BlockStartLocator >> pushClosureCopyNumCopiedValues: numCopied numArgs: numArgs blockSize: blockSize [
	"Answer the size of the block"
	^blockSize
]

{ #category : 'instruction decoding' }
BlockStartLocator >> send: selector super: supered numArgs: numberArguments [
	nextJumpIsAroundBlock := #closureCopy:copiedValues: == selector
]
