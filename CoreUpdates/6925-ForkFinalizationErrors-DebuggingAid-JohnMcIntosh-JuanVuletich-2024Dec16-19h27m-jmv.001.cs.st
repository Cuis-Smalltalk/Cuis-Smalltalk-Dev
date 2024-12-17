'From Cuis7.3 [latest update: #6925] on 16 December 2024 at 7:33:40 pm'!

!BlockClosure methodsFor: 'exceptions' stamp: 'jmv 12/16/2024 19:28:54'!
on: exception fork: handlerAction
	"Activate the receiver. In case of exception, fork a new process, which will handle an error.
	An original process will continue running as if receiver evaluation finished and answered nil,
	i.e., an expression like:

	[ self error: 'some error'] on: Error fork: [ :ex |  ex pass ]

	will always answer nil for original process, regardless of the forked block.

	The context stack , starting from context which sent this message to receiver and
	up to the top of the stack will be transferred to forked process, with handlerAction on top.
	(so when the forked process will be resuming, it will enter the handlerAction)
	 "

	^ self
		  on: exception
		  do: [ :ex |
			  | onDoCtx handler bottom thisCtx |
			  onDoCtx := thisContext.
			  thisCtx := onDoCtx home.

			  "find the context on stack for which this method's is sender"

			  [ onDoCtx sender == thisCtx ] whileFalse: [
				  onDoCtx := onDoCtx sender.
				  onDoCtx ifNil: [ "Can't find our home context. seems like we're already forked
				and handling another exception in new thread. In this case, just pass it through handler."
					  ^ handlerAction valueWithPossibleArgument: ex ] ].

			  bottom := [ Processor activeProcess terminate ] asContext.
			  onDoCtx privSender: bottom.

			  handler := [ handlerAction valueWithPossibleArgument: ex ] asContext.
			  handler privSender: thisContext sender.

			  (Process forContext: handler priority: Processor activePriority) resume.

			  "cut the stack of current process"
			  thisContext privSender: thisCtx.
			  nil ]! !


!FinalizationRegistry methodsFor: 'private - synchronization' stamp: 'jmv 12/16/2024 19:33:35'!
handleErrorsDuring: aBlock

	"The following code would silently swallow any errors during finalization.
	This was the behavior in Cuis WeakRegistry prior to December 2024.
	Such errors would likely remain unnoticed.
	At least, printing some warning to Transcript would be better."
	"aBlock on: Error do:[:ex| ex return]."

	"The following code will resume the finalization process, but would a debugger will open
	in a forked process, so finalization is not paused or stopped."
	aBlock on: Error fork: [ :e | e pass ].! !

