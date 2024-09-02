'From Cuis7.1 [latest update: #6665] on 2 September 2024 at 2:37:39 pm'!

!Debugger methodsFor: 'initialization' stamp: 'jmv 9/2/2024 14:37:13'!
windowIsClosing
	"My window is being closed.
	Terminate the process being debugged.
	Clean up. Restart the low space watcher."

	interruptedProcess
		ifNil: [^ self].
	interruptedProcess terminate.
	interruptedProcess := nil.
	interruptedProcessUI := nil.
	contextStackIndex := 0.
	contextStack := nil.
	contextStackTop := nil.
	receiverInspector := nil.
	contextVariablesInspector := nil.
	Smalltalk installLowSpaceWatcher.  "restart low space handler"
! !

