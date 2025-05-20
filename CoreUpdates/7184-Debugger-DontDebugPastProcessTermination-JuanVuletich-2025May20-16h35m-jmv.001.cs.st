'From Cuis7.3 [latest update: #7182] on 20 May 2025 at 4:35:46 pm'!

!Debugger class methodsFor: 'constants' stamp: 'jmv 5/20/2025 14:24:05'!
debuggingTerminatedProcessName
	^'debugging terminated'! !


!Debugger methodsFor: 'context stack menu' stamp: 'jmv 5/20/2025 13:16:27'!
doStep
	"Send the selected message in the accessed method, and regain control 
	after the invoked method returns.
	Button [Over] in the Debugger."
	
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
				ifFalse: [self resetContext: newContext]].! !

!Debugger methodsFor: 'context stack menu' stamp: 'jmv 5/20/2025 13:16:35'!
send
	"Send the selected message in the accessed method, and take control in 
	the method invoked to allow further step or send.
	Button [Into] in the Debugger."

	interruptedProcess isTerminated ifTrue: [^self inform: 'Debugging terminated'].

	self checkContextSelection.
	interruptedProcess step: self selectedContext.
	self resetContext: interruptedProcess stepToSendOrReturn.
! !

!Debugger methodsFor: 'context stack menu' stamp: 'jmv 5/20/2025 14:24:21'!
stepIntoBlock
	"Send messages until you return to the present method context.
	 Used to step into a block in the method.
	Button [Through] in the Debugger."

	interruptedProcess isTerminated ifTrue: [^self inform: 'Debugging terminated'].
	"It is actually suspended, not terminated!!"
	interruptedProcess name = Debugger debuggingTerminatedProcessName ifTrue: [
		^self inform: 'Debugging terminated'].

	self selectedContext method isQuick ifTrue: [ ^self doStep ].

	self 
		handleLabelUpdatesIn: [
			interruptedProcess stepToHome: self selectedContext ]
		whenExecuting: self selectedContext.
	self resetContext: interruptedProcess stepToSendOrReturn.! !


!BlockClosure methodsFor: 'scheduling' stamp: 'jmv 5/20/2025 14:24:28'!
newProcessForDebugging
	"Answer a Process running the code in the receiver. The process is not 
	scheduled."
	<primitive: 19> "Simulation guard"
	| p |
	p := Process
		forContext: [
			self value.
			p name: Debugger debuggingTerminatedProcessName.
			p terminate] asContext
		priority: Processor activePriority.
	^p! !

