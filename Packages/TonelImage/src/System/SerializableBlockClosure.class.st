"
A SerializableBlockClosure is a regular Smalltalk object (and hence serializable by ReferenceStream and SmartRefStream), that hold the source code for a BlockClosure, together with the state (originally in outer temps) that the BlockClosure might access.

When sent #asEvaluable (even after materializing in the same or different Smalltalk image / machine), the result is a BlockClosure that can be evaluated with identical result as the original.

By making BlockClosure>>objectForDataStream: call #asSerializable, we enable serialization and #veryDeepCopy of BlockClosures as if they were regular Smalltalk objects.

(The only limitation is that we can't meaningfully handle non-local returns. This limitation also applies to regular BlockClosures if evaluation is attempted when there's nowhere to return to.)
"
Class {
	#name : 'SerializableBlockClosure',
	#superclass : 'Object',
	#instVars : [
		'theSelf',
		'sourceCode',
		'capturedValues'
	],
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'instance creation' }
SerializableBlockClosure class >> onBlockClosure: aBlockClosure [
	^self new onBlockClosure: aBlockClosure
]

{ #category : 'evaluating' }
SerializableBlockClosure >> asEvaluable [

	^Compiler evaluate: sourceCode for: self logged: false
]

{ #category : 'objects from disk' }
SerializableBlockClosure >> comeFullyUpOnReload: smartRefStream [

	^ self asEvaluable
]

{ #category : 'initialization' }
SerializableBlockClosure >> onBlockClosure: aBlockClosure [

	| both blockNode methodNode indirectTempNames sortedOuterTemps ownNames usedOuterNames sortedUsedOuterNames |
	aBlockClosure hasNonLocalReturn ifTrue: [
		self error: 'Can not serialize closures with non-local returns.' ].
	aBlockClosure sendsToSuper ifTrue: [
		self error: 'Can not currently serialize closures with super sends.' ].
	both := SerializableClosureDecompiler new  decompileBlockAndMethod: aBlockClosure.
	blockNode := both first.
	methodNode := both second.

	indirectTempNames := methodNode temporaries
		detect: [ :node | node isIndirectTempVector ]
		ifFound: [ :node | node remoteTemps collect: [ :n | n name ]]
		ifNone: [#()].
	sortedOuterTemps := OrderedCollection new.
	aBlockClosure outerContextsDo: [ :c | c closure ifNotNil: [ :cc |
		| ccn |
		ccn := cc decompile.
		sortedOuterTemps addAll: ccn arguments; addAll: ccn temporaries ]].
	sortedOuterTemps addAll: methodNode temporaries; addAll: methodNode arguments.

	ownNames := ((blockNode arguments, blockNode temporaries)
		collect: [ :node | node name ]) asSet.
	usedOuterNames := Set new.
	blockNode nodesDo: [ :node | node isTemp ifTrue: [
		(ownNames includes: node name) | (indirectTempNames includes: node name) ifFalse: [
			usedOuterNames add: node name]]].

	sortedUsedOuterNames := sortedOuterTemps select: [ :node |
		usedOuterNames includes: node name ]. "sort them"
	sortedUsedOuterNames := sortedUsedOuterNames collect: [ :node | node name ].

	blockNode nodesDo: [ :node | node isTemp ifTrue: [ 
		node isRemote
			ifTrue: [node capturedIndex: (indirectTempNames indexOf: node name) ]
			ifFalse: [
				(sortedUsedOuterNames includes: node name)
					ifTrue: [node capturedIndex: (sortedUsedOuterNames indexOf: node name)]]]].

	theSelf := aBlockClosure receiver.
	capturedValues := aBlockClosure capturedValues.
	sourceCode := blockNode decompileString.
]
