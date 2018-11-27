'From Cuis 5.0 of 7 November 2016 [latest update: #3504] on 27 November 2018 at 2:34:48 pm'!

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 11/27/2018 14:28:37'!
withSelectorUnderCursorDo: aBlock otherwise: failBlock

	self withSelectorUnderCursorDo: aBlock ifErrorsParsing: failBlock ifNoSelector: failBlock! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 11/27/2018 14:29:28'!
implementorsOfIt

	"Open an implementors browser on the  selector under cursor. If it can not parse the source code it uses
	old implementation that looks for the selected selector if any - Hernan"

	self
		withSelectorUnderCursorDo: [ :aSelector | Smalltalk browseAllImplementorsOf: aSelector ]
		otherwise: [ self implementorsOfItWhenErrorsParsing ]! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 11/27/2018 14:29:23'!
sendersOfIt

	"Open a senders browser on the  selector under cursor. If it can not parse the source code it uses
	old implementation that looks for the selected selector if any - Hernan"

	self
		withSelectorUnderCursorDo: [ :aSelector | Smalltalk browseAllCallsOn: aSelector ]
		otherwise: [ self sendersOfItWhenErrorsParsing ]! !

!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 11/27/2018 14:33:01'!
withSelectorUnderCursorDo: aBlock ifErrorsParsing: aParsingErrorBlock ifNoSelector: aNoSelectorBlock

	| methodNode nodeAtRange nodeUnderCursor |

	methodNode := self codeProvider methodNodeOf: model actualContents ifErrorsParsing: [ :anError | ^ aParsingErrorBlock valueWithPossibleArgument: anError ].

	self startIndex < methodNode selectorLastPosition ifTrue: [ ^aBlock value: methodNode selector ].
	nodeAtRange := methodNode parseNodeIncluding: self startIndex ifAbsent: [ ^ aNoSelectorBlock value ].
	nodeUnderCursor := nodeAtRange key.
	nodeUnderCursor isMessageNode ifTrue: [ ^aBlock value: nodeAtRange key selector key ].
	(nodeUnderCursor isLiteralNode and: [ nodeUnderCursor literalValue isSymbol ]) ifTrue: [ ^aBlock value: nodeUnderCursor literalValue ].

	^ aNoSelectorBlock value ! !

