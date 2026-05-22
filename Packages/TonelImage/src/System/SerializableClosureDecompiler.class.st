Class {
	#name : 'SerializableClosureDecompiler',
	#superclass : 'Decompiler',
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'private' }
SerializableClosureDecompiler >> constructorForMethod: aMethod [

	^SerializableClosureDecompilerConstructor new
]

{ #category : 'public access' }
SerializableClosureDecompiler >> decompileBlockAndMethod: aBlock [
	"Decompile aBlock, returning the result as a BlockNode, and the corresponding MethodNode."
	"Decompiler new decompileBlockAndMethod: [3 + 4]"

	| homeMethod methodNode home methodClass methodSelector |
	(home := aBlock home) ifNil: [^ nil].
	homeMethod := home method.
	(home methodClass) == #unknown ifTrue: [^ nil].
	aBlock isClosure ifTrue: [
		methodClass := homeMethod methodClass ifNil: [Object].
		methodSelector := homeMethod selector ifNil: [homeMethod defaultSelector].
		methodNode := self decompile: methodSelector in: methodClass method: homeMethod.
		methodNode
			ifNil: [^nil]
			ifNotNil: [methodNode nodesDo: [:node| node pc = aBlock startpc ifTrue: [^{node. methodNode}]]].
		 ^self error: 'cannot find block node matching aBlock'].
	^self error: 'can only decompile BlockClosures'
]

{ #category : 'instruction decoding' }
SerializableClosureDecompiler >> pushReceiver [

	stack addLast: (InstanceVariableNode new
		name: 'theSelf'
		index: 1)
]
