'From Cuis7.9 [latest update: #8340] on 17 November 2026 at 10:15:42 pm'!

!ExtractMethodParameterizableReplacement methodsFor: 'private' stamp: 'HAW 17/Nov/2026, 6:05:31 pm (UTC)'!
isInCodeToExtractOutsideParameterizableNodes: aRange

	^(nodeInterval includesAllOf: aRange)
		and: [ parameterizableNodes noneSatisfy: [ :parameterizableNode |
			parameterizableNode value intersectsRange: aRange ]]! !

!ExtractMethodParameterizableReplacement methodsFor: 'private' stamp: 'HAW 17/Nov/2026, 6:05:31 pm (UTC)'!
occurrencesToParameterizeOfAll: parseNodesToParameterize

	| methodNode occurrences |

	methodNode := methodToExtractFrom notOptimizedMethodNode.
	occurrences := OrderedCollection new.
	parseNodesToParameterize do: [ :parseNode |
		(methodNode completeSourceRangesOf: parseNode ifAbsent: [ #() ]) do: [ :range |
			(self isInCodeToExtractOutsideParameterizableNodes: range)
				ifTrue: [ occurrences add: parseNode -> range ]]].

	^occurrences! !

!ExtractMethodParameterizableReplacement methodsFor: 'private' stamp: 'HAW 17/Nov/2026, 6:05:31 pm (UTC)'!
sourceCodeAt: aRange

	^methodToExtractFrom sourceCode copyFrom: aRange first to: aRange last! !


!ExtractMethodParameterizableReplacement methodsFor: 'printing' stamp: 'HAW 17/Nov/2026, 6:05:31 pm (UTC)'!
sourceCode

	"This method is needed for highlighting in the ExtractParameterizedCodeWindow"

	^ self sourceCodeAt: nodeInterval! !

!ExtractMethodParameterizableReplacement methodsFor: 'accessing' stamp: 'HAW 17/Nov/2026, 6:05:31 pm (UTC)'!
allArgumentsToParameterizeInOrderUsing: parseNodesToParameterize 

	| argumentsToParameterize |
	
	argumentsToParameterize := SortedCollection sortBlock: self sortBlockByRange.
	argumentsToParameterize
		addAll: parameterizableNodes;
		addAll: (self occurrencesToParameterizeOfAll: parseNodesToParameterize).
	
	^ argumentsToParameterize collect: [ :argument |
		argument key -> (self sourceCodeAt: argument value) ]! !

!methodRemoval: ExtractMethodParameterizableReplacement #occurrencesOutsideParameterizableNodesOfAll: stamp: 'HAW 17/Nov/2026, 6:05:31 pm (UTC)'!
ExtractMethodParameterizableReplacement removeSelector: #occurrencesOutsideParameterizableNodesOfAll:!
