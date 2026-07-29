'From Cuis7.9 [latest update: #8098] on 29 July 2026 at 2:18:53 pm'!

!Debugger methodsFor: 'context stack menu' stamp: 'HAW 26/Dec/2019, 1:19:57 pm (UTC)'!
handleLabelUpdatesIn: aBlock whileExecuting: aContext
	"Send the selected message in the accessed method, and regain control 
	after the invoked method returns."
	
	^aBlock
		on: Notification
		do: [:aNotification|
			aNotification 
				withNewDebuggerLabelOn: aContext
				do: [ :aNewLabel | 
					self labelString: aNewLabel.
					aNotification resume]
				ifNone: [aNotification pass]]! !


!Debugger methodsFor: 'context stack menu' stamp: 'jmv 29/Jul/2026, 5:18:37 pm (UTC)'!
doStep
	"Send the selected message in the accessed method, and regain control 
	after the invoked method returns.
	Button [Over] in the Debugger."
	
	| currentContext newContext |

	interruptedProcess isTerminated ifTrue: [^self inform: 'Debugging terminated'].

	currentContext := self selectedContext.
	newContext := self handleLabelUpdatesIn: [interruptedProcess completeStep: currentContext]
						whileExecuting: currentContext.
	newContext == currentContext ifTrue:
		[newContext := interruptedProcess stepToSendOrReturn].
	self contextStackIndex > 1
		ifTrue: [self resetContext: newContext]
		ifFalse:
			[newContext == currentContext
				ifTrue: [self changed: #contentsSelection.
						self updateInspectors]
				ifFalse: [self resetContext: newContext]].! !

!Debugger methodsFor: 'context stack menu' stamp: 'jmv 29/Jul/2026, 5:18:43 pm (UTC)'!
stepIntoBlock
	"Send messages until you return to the present method context.
	 Used to step into a block in the method.
	Button [Through] in the Debugger."

	interruptedProcess isTerminated ifTrue: [^self inform: 'Debugging terminated'].
	"It is actually suspended, not terminated!!"
	interruptedProcess name = Debugger debuggingTerminatedProcessName ifTrue: [
		^self inform: 'Debugging terminated'].

	self selectedContext method isQuick ifTrue: [ ^self doStep ].

	(self selectedContext = interruptedProcess suspendedContext and: [
			interruptedProcess suspendedContext willReturn and: [
				interruptedProcess suspendedContext closure isNil]])
		ifTrue: [
			^self doStep ].

	self 
		handleLabelUpdatesIn: [
			interruptedProcess stepToHome: self selectedContext ]
		whileExecuting: self selectedContext.
	self resetContext: interruptedProcess stepToSendOrReturn.! !

!methodRemoval: Debugger #handleLabelUpdatesIn:whenExecuting: stamp: 'jmv 29/Jul/2026, 5:18:46 pm (UTC)'!
Debugger removeSelector: #handleLabelUpdatesIn:whenExecuting:!
