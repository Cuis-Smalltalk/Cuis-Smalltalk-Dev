'From Cuis 4.2 of 25 July 2013 [latest update: #2079] on 28 August 2014 at 11:01:14.620957 pm'!

'Phase 1'!

!Exception methodsFor: 'priv handling' stamp: 'SqR 8/28/2014 22:49'!
privHandlerContext

	^handlerContext! !

!ContextPart methodsFor: 'private-exceptions' stamp: 'SqR 8/28/2014 22:55'!
exceptionClass

	^self tempAt: 1! !

!ContextPart methodsFor: 'private-exceptions' stamp: 'SqR 8/28/2014 22:58'!
exceptionHandlerBlock
	"handler context only. access temporaries from BlockClosure>>#on:do:"

	^self tempAt: 2! !


'Phase 2'!

!ContextPart methodsFor: 'private-exceptions' stamp: 'SqR 8/28/2014 22:57'!
evaluateSignal: exception
	"The following primitive is just a marker used to find the evaluation context. 
	See MethodContext>>#isHandlerOrSignalingContext. "

	<primitive: 199>
	| value |
	exception privHandlerContext: self contextTag.
	value := self exceptionHandlerBlock valueWithPossibleArgument: exception.	
	"return from self if not otherwise directed in handle block"
	self return: value! !

!ContextPart methodsFor: 'private-exceptions' stamp: 'SqR 8/28/2014 22:59'!
findNextHandlerContext
	"Return the next handler marked context, returning nil if there is none.  Search starts with self and proceeds up to nil."

	| context |
	context := self findNextHandlerOrSignalingContext.
	context isNil
		ifTrue: [ ^ nil ].
	context isHandlerContext
		ifTrue: [ ^ context ].	"If it isn't a handler context, it must be a signaling context.
	When we reach a signaling context we must skip over any handlers
	that might be on the stack between the signaling context and the handler
	context for that signal."
	^ context exceptionClass privHandlerContext nextHandlerContext! !

!ContextPart methodsFor: 'private-exceptions' stamp: 'SqR 8/28/2014 22:59'!
findNextHandlerOrSignalingContext
	"Return the next handler/signaling marked context, answering nil if there is none. 
	Search starts with self and proceeds up to nil."

	<primitive: 197>
	| context |
	context := self.
	[ 
	context isHandlerOrSignalingContext
		ifTrue: [ ^ context ].
	(context := context sender) == nil ] whileFalse.
	^ nil! !

!ContextPart methodsFor: 'private-exceptions' stamp: 'SqR 8/28/2014 22:59'!
isHandlerOrSignalingContext
	"Both BlockClosure>>on:do: (handler) and ContextPart>>evaluateSignal: (signaling) 
	are marked with primitive 199."

	^false! !

!MethodContext methodsFor: 'private-exceptions' stamp: 'SqR 8/28/2014 23:00'!
isHandlerOrSignalingContext
	"Both BlockClosure>>on:do: (handler) and ContextPart>>evaluateSignal: (signaling) 
	are marked with primitive 199."

	^method primitive = 199! !


'Phase 3'!

!ContextPart methodsFor: 'private-exceptions' stamp: 'SqR 8/28/2014 23:02'!
isHandlerContext
	"is this context for #on:do:?"
	^self isHandlerOrSignalingContext and: [ self selector == #on:do: ]! !

!methodRemoval: MethodContext #isHandlerContext!
MethodContext removeSelector: #isHandlerContext!


'Phase 4'!

!ContextPart methodsFor: 'private-exceptions' stamp: 'SqR 8/28/2014 23:03'!
nextHandlerContext

	^ self sender findNextHandlerContext! !


'Phase 5'!

!ContextPart methodsFor: 'private-exceptions' stamp: 'SqR 8/28/2014 23:07'!
handleSignal: exception
	"Sent to handler (on:do:) contexts only.  If my exception class (first arg) handles exception then execute my handle block (second arg), otherwise forward this message to the next handler context.  If none left, execute exception's defaultAction (see nil>>handleSignal:)."

	(self exceptionClass handles: exception)
		ifFalse: [ ^ self nextHandlerContext handleSignal: exception ].
	self evaluateSignal: exception! !


'Phase 6'!

!BlockClosure methodsFor: 'exceptions' stamp: 'SqR 8/28/2014 23:07'!
on: exception do: handlerAction
	"Evaluate the receiver in the scope of an exception handler.
	The following primitive is just a marker used to find the error handling context. 
	See MethodContext>>#isHandlerOrSignalingContext. "
	<primitive: 199>  
	^ self value! !

!ContextPart methodsFor: 'private-exceptions' stamp: 'SqR 8/28/2014 23:08'!
canHandleSignal: exception
	"Sent to handler (on:do:) contexts only.  If my exception class (first arg) handles exception then return true, otherwise forward this message to the next handler context.  If none left, return false (see nil>>canHandleSignal:)"

	^ (self exceptionClass handles: exception)
		or: [ self nextHandlerContext canHandleSignal: exception ].! !

!BlockContext methodsFor: 'exceptions' stamp: 'SqR 8/28/2014 23:08'!
on: exception do: handlerAction
	"Evaluate the receiver in the scope of an exception handler.
	The following primitive is just a marker used to find the error handling context. 
	See MethodContext>>#isHandlerOrSignalingContext. "
	<primitive: 199>  
	^ self value! !


'Phase 7'!

!methodRemoval: ContextPart #findNextHandlerContextStarting!
ContextPart removeSelector: #findNextHandlerContextStarting!
!methodRemoval: ContextPart #rearmHandlerDuring:!
ContextPart removeSelector: #rearmHandlerDuring:!
!methodRemoval: Exception #rearmHandlerDuring:!
Exception removeSelector: #rearmHandlerDuring:!
