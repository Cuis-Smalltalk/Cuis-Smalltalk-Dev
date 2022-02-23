'From Cuis 6.0 [latest update: #5083] on 23 February 2022 at 12:48:38 pm'!

!Message methodsFor: 'comparing' stamp: 'eem 11/27/2021 12:27'!
hasIdenticalContentsAs: aMessage
	"Answer if the argument's selector and arguments are identically equal to those of the receiver.
	 It is assumed that the argument aMessage is, in fact, a message."
       selector ~~ aMessage selector ifTrue:
		[^false].
	1 to: args size do:
		[:i| (args at: i) ~~ (aMessage arguments at: i) ifTrue: [^false]].
	^true! !


!Object methodsFor: 'error handling' stamp: 'eem 11/27/2021 12:55'!
doesNotUnderstand: aMessage 
	 "Handle the fact that there was an attempt to send the given
	  message to the receiver but the receiver does not understand
	  this message (typically sent from the machine when a message
	  is sent to the receiver and no method is defined for that selector).

	 Raise the MessageNotUnderstood signal.  If it is caught, answer
	 the result supplied by the exception handler.  If it is not caught,
	 answer the result of resending the message within a guard for
	 infinite recursion. This allows, for example, the programmer to
	 implement the method and continue."

	"Testing: (3 activeProcess)"

	| exception resumeValue |
	(exception := MessageNotUnderstood new)
		message: aMessage;
		receiver: self.
	resumeValue := exception signal.
	^exception reachedDefaultHandler "i.e. exception was not caught..."
		ifTrue:
			[[aMessage sentTo: self]
				on: MessageNotUnderstood
				do: [:ex|
					(self == ex receiver
					and: [aMessage hasIdenticalContentsAs: ex message]) ifFalse:
						[ex pass].
					self error: 'infinite recursion in doesNotUnderstand:']]
		ifFalse: [resumeValue]! !


!MethodContext methodsFor: 'private' stamp: 'jmv 3/2/2010 17:09'!
cannotReturn: result
	closureOrNil ifNotNil: [
		^self cannotReturn: result to: sender].
	Debugger
		openContext: thisContext
		label: 'computation has been terminated'
		contents: nil! !


!Process methodsFor: 'private' stamp: 'jmv 2/23/2022 12:39:44'!
complete: topContext to: aContext 
	"Run topContext on behalf of self on topContext's stack until aContext is popped or an unhandled 
	error is raised. Return self's new top context. Note: topContext must be a stack top context.
	Note: This method is meant to be called primarily by Process>>#terminate."

	| pair top error |
	pair _ topContext runUnwindUntilErrorOrReturnFrom: aContext.
	top _ pair first.
	error _ pair second.
	"If an error was detected jump back to the debugged process and re-signal the error;
	some errors may require a special care - see notes below."
	error ifNotNil: [
		error class == MessageNotUnderstood ifTrue: [error initialize]. "reset reachedDefaultHandler"
		top jump].
	^top

"Note: To prevent an infinite recursion of the MessageNotUnderstood error, reset reachedDefaultHandler before jumping back; this will prevent #doesNotUnderstand: from resending the unknown message."! !

!classRemoval: #ProceedBlockCannotReturn stamp: 'jmv 2/23/2022 12:39:58'!
Smalltalk removeClassNamed: #ProceedBlockCannotReturn!

