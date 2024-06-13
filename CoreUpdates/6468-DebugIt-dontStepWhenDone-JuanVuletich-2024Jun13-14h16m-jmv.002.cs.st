'From Cuis7.1 [latest update: #6467] on 13 June 2024 at 2:19:38 pm'!

!Debugger methodsFor: 'context stack menu' stamp: 'jmv 6/13/2024 14:18:45'!
doStep
	"Send the selected message in the accessed method, and regain control 
	after the invoked method returns."
	
	| currentContext newContext |

	interruptedProcess isTerminated ifTrue: [^self inform: 'Debugging terminated'].

	currentContext := self selectedContext.
	newContext := self handleLabelUpdatesIn: [interruptedProcess completeStep: currentContext]
						whenExecuting: currentContext.
	newContext == currentContext ifTrue:
		[newContext := interruptedProcess stepToSendOrReturn].
	self contextStackIndex > 1
		ifTrue: [self resetContext: newContext]
		ifFalse:
			[newContext == currentContext
				ifTrue: [self changed: #contentsSelection.
						self updateInspectors]
				ifFalse: [self resetContext: newContext]].
! !

!Debugger methodsFor: 'context stack menu' stamp: 'jmv 6/13/2024 14:18:48'!
send
	"Send the selected message in the accessed method, and take control in 
	the method invoked to allow further step or send."

	interruptedProcess isTerminated ifTrue: [^self inform: 'Debugging terminated'].

	self checkContextSelection.
	interruptedProcess step: self selectedContext.
	self resetContext: interruptedProcess stepToSendOrReturn.
! !

!Debugger methodsFor: 'context stack menu' stamp: 'jmv 6/13/2024 14:18:52'!
stepIntoBlock
	"Send messages until you return to the present method context.
	 Used to step into a block in the method."

	interruptedProcess isTerminated ifTrue: [^self inform: 'Debugging terminated'].

	self 
		handleLabelUpdatesIn: [interruptedProcess stepToHome: self selectedContext]
		whenExecuting: self selectedContext.
	self resetContext: interruptedProcess stepToSendOrReturn! !


!Debugger class methodsFor: 'instance creation' stamp: 'jmv 6/13/2024 13:41:12'!
newDebugging: aBlock
	"Create a new process to debug aBlock"

	| processToDebug debugger |
	debugger := self new.
	processToDebug := aBlock newProcessForDebugging name: 'debugIt'.
	debugger process: processToDebug context: processToDebug suspendedContext.
		
	^ debugger! !


!BlockClosure methodsFor: 'scheduling' stamp: 'jmv 6/13/2024 13:56:47'!
newProcessForDebugging
	"Answer a Process running the code in the receiver. The process is not 
	scheduled."
	<primitive: 19> "Simulation guard"
	^Process
		forContext: self asContext
		priority: Processor activePriority! !

