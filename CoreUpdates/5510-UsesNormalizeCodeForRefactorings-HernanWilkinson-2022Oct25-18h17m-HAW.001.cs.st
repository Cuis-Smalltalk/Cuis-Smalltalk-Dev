'From Cuis 6.0 [latest update: #5509] on 25 October 2022 at 6:18:17 pm'!

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'jmv 10/25/2022 18:05:58'!
withClassDefinitionNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

	| selectedClass methodNode |

	selectedClass := self codeProvider selectedClassOrMetaClass.
	methodNode := [ selectedClass methodNodeFor: (CodeProvider normalizeCode: model actualContents) noPattern: true ] on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: selectedClass.! !

!SmalltalkEditor methodsFor: 'contextual rename' stamp: 'jmv 10/25/2022 18:04:46'!
withMethodNodeAndClassDo: aBlock ifErrorsParsing: anErrorBlock

	| selectedClass methodNode |

	selectedClass := self selectedClassOrMetaClassOrUndefinedObject.
	[ 
		[ methodNode := selectedClass methodNodeFor: (CodeProvider normalizeCode: model actualContents) ]
			on: UndeclaredVariableWarning do: [ :ex | ex resume ]
	] on: Error do: [ :anError |  ^ anErrorBlock value: anError ].

	^aBlock value: methodNode value: selectedClass.! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 10/25/2022 18:04:13'!
doItSourceCodeFor: aSourceCode in: anEvaluationContext

	^self stringClassToUse streamContents: [ :stream |
		Scanner doItSourceCodeHeaderWithContext: anEvaluationContext notNil into: stream.
		stream nextPutAll: (CodeProvider normalizeCode: aSourceCode)		]! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 10/25/2022 18:05:17'!
withSelectorAt: aPosition do: aBlock ifBehavior: aBehaviorBlock otherwise: failBlock

	| methodNode nodeAtRange nodeUnderCursor  |
	
	methodNode := self codeProvider
		methodNodeOf: (CodeProvider normalizeCode: model actualContents)
		ifErrorsParsing: [ :anError | ^ failBlock value ].
	
	aPosition < methodNode selectorLastPosition ifTrue: [ ^aBlock value: methodNode selector ].
	nodeAtRange := methodNode parseNodeIncluding: aPosition ifAbsent: [ ^ failBlock value ].
	nodeUnderCursor := nodeAtRange key.
	nodeUnderCursor isMessageNode ifTrue: [ ^aBlock value: nodeAtRange key selector key ].
	(nodeUnderCursor isLiteralNode and: [ nodeUnderCursor literalValue isSymbol ]) ifTrue: [ ^aBlock value: nodeUnderCursor literalValue ].
	(nodeUnderCursor isLiteralVariableNode and: [ nodeUnderCursor isReferencingBehavior ]) ifTrue: [ ^aBehaviorBlock value: nodeUnderCursor key value ].
	
	^ failBlock value ! !


!SmalltalkCompleter methodsFor: 'accessing' stamp: 'jmv 10/25/2022 18:07:03'!
allSource
	^CodeProvider normalizeCode: model actualContents! !

