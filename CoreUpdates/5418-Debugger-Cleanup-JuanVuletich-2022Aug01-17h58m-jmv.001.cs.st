'From Cuis 6.0 [latest update: #5415] on 1 August 2022 at 6:04:27 pm'!
!classDefinition: #Debugger category: #'Tools-Debugger'!
CodeProvider subclass: #Debugger
	instanceVariableNames: 'interruptedProcess contextStack contextStackTop contextStackIndex contextStackList receiverInspector contextVariablesInspector externalInterrupt proceedValue selectingPC savedCursor labelString interruptedProcessUI sendProceeds '
	classVariableNames: 'ErrorRecursion '
	poolDictionaries: ''
	category: 'Tools-Debugger'!

!Debugger methodsFor: 'code pane' stamp: 'jmv 8/1/2022 18:02:01'!
pcRange
	"Answer the indices in the source code for the method corresponding to 
	the selected context's program counter value."

	contextStackIndex = 0 ifTrue:
		[^1 to: 0].
	self selectedContext isDead ifTrue:
		[^1 to: 0].
	^self selectedContext debuggerMap
		rangeForPC: self selectedContext pc
		contextIsActiveContext: contextStackIndex = 1! !

!Debugger methodsFor: 'context stack (message list)' stamp: 'jmv 8/1/2022 18:00:02'!
expandStack
	"A Notifier is being turned into a full debugger.  Show a substantial amount of stack in the context pane."

	self newStack: (contextStackTop stackOfSize: Debugger defaultDebugStackSize).
	contextStackIndex _ 0.
	receiverInspector _ Inspector inspect: nil.
	contextVariablesInspector _ ContextVariablesInspector inspect: nil.! !

!Debugger methodsFor: 'context stack menu' stamp: 'jmv 8/1/2022 18:02:15'!
where
	"Select the expression whose evaluation was interrupted."

	self contextStackIndex: contextStackIndex oldContextWas: self selectedContext
! !

!Debugger methodsFor: 'private' stamp: 'jmv 8/1/2022 18:02:10'!
process: aProcess context: aContext

	interruptedProcess _ aProcess.
	contextStackTop _ aContext.
	self newStack: (contextStackTop stackOfSize: 1).
	contextStackIndex _ 1.! !


!Debugger class methodsFor: 'instance creation' stamp: 'jmv 8/1/2022 18:00:18'!
informExistingDebugger: aContext label: aString
	"Walking the context chain, we try to find out if we're in a debugger stepping situation.
	If we find the relevant contexts, we must rearrange them so they look just like they would
	if the methods were excuted outside of the debugger."
	| ctx quickStepMethod oldSender baseContext |
	ctx _ thisContext.
	quickStepMethod _ ContextPart compiledMethodAt: #quickSend:to:with:super:.
	[ctx sender == nil or: [ctx sender method == quickStepMethod]] whileFalse: [ctx _ ctx sender].
	ctx sender ifNil: [^self].
	baseContext _ ctx.
	"baseContext is now the context created by the #quickSend... method."
	oldSender _ ctx _ ctx sender home sender.
	"oldSender is the context which originally sent the #quickSend... method"
	[ctx == nil or: [ctx receiver isKindOf: self]] whileFalse: [ctx _ ctx sender].
	ctx == nil ifTrue: [^self].
	"ctx is the context of the Debugger method #doStep"
	ctx receiver labelString: aString.
	baseContext swapSender: baseContext sender sender sender.	"remove intervening contexts"
	thisContext swapSender: oldSender.	"make myself return to debugger"
	ErrorRecursion _ false.
	^aContext! !

!Debugger class methodsFor: 'opening' stamp: 'jmv 8/1/2022 17:58:40'!
openInterrupt: aString onProcess: interruptedProcess
	"Open a notifier in response to an interrupt. An interrupt occurs when the user types the interrupt key (cmd-. on Macs, ctrl-c or alt-. on other systems) or when the low-space watcher detects that memory is low."
	| debugger |
	<primitive: 19> "Simulation guard"
	debugger _ self new.
	debugger 	process: interruptedProcess context: interruptedProcess suspendedContext.

	(Preferences at: #logDebuggerStackToFile) ifTrue:
		[(aString includesSubString: 'Space') & 
			(aString includesSubString: 'low') ifTrue: [
				Smalltalk logError: aString inContext: debugger interruptedContext to: 'LowSpaceDebug']].

	^ debugger
		openNotifierContents: nil
		label: aString
! !

!methodRemoval: Debugger #proceedValue stamp: 'jmv 8/1/2022 17:59:54'!
Debugger removeSelector: #proceedValue!
!methodRemoval: Debugger #externalInterrupt: stamp: 'jmv 8/1/2022 17:58:47'!
Debugger removeSelector: #externalInterrupt:!
!methodRemoval: Debugger #proceedValue: stamp: 'jmv 8/1/2022 18:00:30'!
Debugger removeSelector: #proceedValue:!
!methodRemoval: Debugger #selectPC stamp: 'jmv 8/1/2022 18:00:59'!
Debugger removeSelector: #selectPC!
!classDefinition: #Debugger category: #'Tools-Debugger'!
CodeProvider subclass: #Debugger
	instanceVariableNames: 'interruptedProcess contextStack contextStackTop contextStackIndex contextStackList receiverInspector contextVariablesInspector savedCursor labelString interruptedProcessUI sendProceeds'
	classVariableNames: 'ErrorRecursion'
	poolDictionaries: ''
	category: 'Tools-Debugger'!
