'From Cuis7.5 [latest update: #7357] on 15 July 2025 at 10:47:33 am'!

!SerializableBlockClosure methodsFor: 'initialization' stamp: 'jmv 7/15/2025 10:41:23'!
onBlockClosure: aBlockClosure

	| both blockNode methodNode indirectTempNames sortedOuterTemps ownNames usedOuterNames sortedUsedOuterNames |
	aBlockClosure hasNonLocalReturn ifTrue: [
		self error: 'Can not serialize closures with non-local returns.' ].
	aBlockClosure sendsToSuper ifTrue: [
		self error: 'Can not currently serialize closures with super sends.' ].
'tota!!' print.
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
	sourceCode := blockNode decompileString.! !

!methodRemoval: SystemDictionary #unusedBlocks stamp: 'jmv 7/15/2025 10:47:11'!
SystemDictionary removeSelector: #unusedBlocks!
