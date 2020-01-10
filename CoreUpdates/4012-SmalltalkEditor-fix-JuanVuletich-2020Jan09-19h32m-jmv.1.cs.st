'From Cuis 5.0 [latest update: #4010] on 9 January 2020 at 7:49:25 pm'!

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 1/9/2020 19:47:47'!
withSelectorUnderCursorDo: aBlock ifErrorsParsing: aParsingErrorBlock ifNoSelector: aNoSelectorBlock

	| methodNode nodeAtRange nodeUnderCursor alternativeAnswer failed |
	failed _ false. 
	methodNode := self codeProvider
		methodNodeOf: model actualContents
		ifErrorsParsing: [ :anError |
			alternativeAnswer _ aParsingErrorBlock valueWithPossibleArgument: anError.
			failed _ true ].
	failed ifTrue: [ ^alternativeAnswer ].

	self startIndex < methodNode selectorLastPosition ifTrue: [ ^aBlock value: methodNode selector ].
	nodeAtRange := methodNode parseNodeIncluding: self startIndex ifAbsent: [ ^ aNoSelectorBlock value ].
	nodeUnderCursor := nodeAtRange key.
	nodeUnderCursor isMessageNode ifTrue: [ ^aBlock value: nodeAtRange key selector key ].
	(nodeUnderCursor isLiteralNode and: [ nodeUnderCursor literalValue isSymbol ]) ifTrue: [ ^aBlock value: nodeUnderCursor literalValue ].

	^ aNoSelectorBlock value ! !

