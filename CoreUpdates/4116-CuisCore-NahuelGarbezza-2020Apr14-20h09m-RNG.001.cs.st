'From Cuis 5.0 [latest update: #4113] on 14 April 2020 at 8:53:11 pm'!

!Parser methodsFor: 'expression types' stamp: 'RNG 4/14/2020 20:50:40'!
createCascadeNodeWith: receiverNode and: messageNodes

	| sourceRangeOfFirstMessage |

	parseNode := CascadeNode new receiver: receiverNode messages: messageNodes.
	sourceRangeOfFirstMessage := encoder rawSourceRanges at: messageNodes first.
	sourceRangeOfFirstMessage ifNotNil: [
		| cascadeSourceRangeStart |
		cascadeSourceRangeStart := sourceRangeOfFirstMessage first.
		encoder noteSourceRange: (cascadeSourceRangeStart to: hereMark + 1) forNode: parseNode ]! !


!CascadeNode methodsFor: 'source ranges' stamp: 'RNG 4/14/2020 20:45:23'!
expandRanges: aSourceRange basedOn: sourceRanges using: sourceCode

	| receiverExpandedRanges expandedRangeWithReceiver |
	receiverExpandedRanges _ receiver expandRanges: (sourceRanges at: receiver) basedOn: sourceRanges using: sourceCode.
	expandedRangeWithReceiver _ self
		expandRange: (aSourceRange isInterval ifTrue: [ aSourceRange ] ifFalse: [ aSourceRange first ])
		basedOn: receiverExpandedRanges.
	^ super
		expandRanges: expandedRangeWithReceiver
		basedOn: expandedRangeWithReceiver
		using: sourceCode! !


!Parser methodsFor: 'expression types' stamp: 'RNG 4/14/2020 20:50:21'!
cascade
	" {; message} => CascadeNode."

	| receiverNode messageNodes sourceRangeOfFirstMessage |
	parseNode canCascade ifFalse:
		[^self expected: 'Cascading not'].
	parseNode ensureCanCascade: encoder.
	receiverNode := parseNode cascadeReceiver.
	messageNodes := OrderedCollection with: parseNode.
	[self match: #semicolon]
		whileTrue: 
			[parseNode := receiverNode.
			(self messagePart: 3 repeat: false)
				ifFalse: [^self expected: 'Cascade'].
			parseNode canCascade ifFalse:
				[^self expected: '<- No special messages'].
			parseNode ensureCanCascade: encoder.
				parseNode cascadeReceiver.
			messageNodes addLast: parseNode].
	self flag: #RNG. "to be replaced by self createCascadeNodeWith: receiverNode and: messageNodes in next changeset"
	parseNode := CascadeNode new receiver: receiverNode messages: messageNodes.
	sourceRangeOfFirstMessage := encoder rawSourceRanges at: messageNodes first.
	sourceRangeOfFirstMessage ifNotNil: [
		| cascadeSourceRangeStart |
		cascadeSourceRangeStart := sourceRangeOfFirstMessage first.
		encoder noteSourceRange: (cascadeSourceRangeStart to: hereMark + 1) forNode: parseNode ]! !


!MessageNode methodsFor: 'source ranges' stamp: 'RNG 4/14/2020 20:42:46'!
expandRanges: aSourceRange basedOn: sourceRanges using: sourceCode

	| receiverExpandedRanges expandedRangeWithReceiver |
	receiverExpandedRanges _ self isCascade
		ifTrue: [ aSourceRange ] "not expanded because expansion is handled in CascadeNode"
		ifFalse: [ receiver expandRanges: (self receiverSourceRangesFrom: sourceRanges) basedOn: sourceRanges using: sourceCode ].
	expandedRangeWithReceiver _ self
		expandRange: (aSourceRange isInterval ifTrue: [ aSourceRange ] ifFalse: [ aSourceRange first ])
		basedOn: receiverExpandedRanges.
	^ super
		expandRanges: expandedRangeWithReceiver
		basedOn: expandedRangeWithReceiver
		using: sourceCode! !


!CascadeNode reorganize!
('visiting' accept:)
('code generation (closures)' analyseTempsWithin:rootNode:assignmentPools:)
('code generation (new scheme)' emitCodeForValue:encoder:)
('code generation' sizeCodeForValue:)
('accessing' messages receiver)
('printing' printOn:indent: printOn:indent:precedence: printWithClosureAnalysisOn:indent: printWithClosureAnalysisOn:indent:precedence:)
('testing' isComplex)
('initialization' receiver:messages:)
('source ranges' expandRanges:basedOn:using:)
!

