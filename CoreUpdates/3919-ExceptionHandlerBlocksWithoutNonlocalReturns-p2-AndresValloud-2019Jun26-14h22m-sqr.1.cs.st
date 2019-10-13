'From Cuis 5.0 of 7 November 2016 [latest update: #3813] on 26 June 2019 at 2:23:06 pm'!

!Exception methodsFor: 'priv handling' stamp: 'sqr 6/26/2019 13:07:12'!
evaluateHandlerBlock: aBlock

	| handlerEx |
	handlerBlockNotCurtailed := false.
	^[
			| answer |
			answer _ [aBlock valueWithPossibleArgument: self] on: Exception do: [:ex | handlerEx _ ex.  ex pass].
			handlerBlockNotCurtailed _ true.
			answer
	] ifCurtailed:
		[
			(handlerBlockNotCurtailed not and: [handlerEx isNil or: [handlerEx handlerBlockNotCurtailed not]])
				ifTrue: [self error: 'Exception handler blocks must not do non local returns']
				ifFalse: [handlerBlockNotCurtailed _ true]
		]! !

!Exception methodsFor: 'priv handling' stamp: 'sqr 6/26/2019 12:29:35'!
handlerBlockNotCurtailed

	^handlerBlockNotCurtailed! !


!ContextPart methodsFor: 'private-exceptions' stamp: 'sqr 6/26/2019 10:30:43'!
evaluateSignal: exception
	"The following primitive is just a marker used to find the evaluation context. 
	See MethodContext>>#isHandlerOrSignalingContext. "

	<primitive: 199>
	| value |
	exception pushHandlerContext: self contextTag.
	value := exception evaluateHandlerBlock: self exceptionHandlerBlock.
	"return from self if not otherwise directed in handle block"
	self return: value! !

