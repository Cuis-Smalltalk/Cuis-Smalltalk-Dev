'From Cuis 6.0 [latest update: #5415] on 1 August 2022 at 6:46:19 pm'!
!classDefinition: #Debugger category: #'Tools-Debugger'!
CodeProvider subclass: #Debugger
	instanceVariableNames: 'interruptedProcess contextStack contextStackTop contextStackIndex contextStackList receiverInspector contextVariablesInspector savedCursor labelString interruptedProcessUI '
	classVariableNames: 'ErrorRecursion '
	poolDictionaries: ''
	category: 'Tools-Debugger'!

!Debugger commentStamp: 'jmv 8/1/2022 18:43:33' prior: 0!
I represent the machine state at the time of an interrupted process. I also represent a query path into the state of the process. The debugger is typically viewed through a window that views the stack of suspended contexts, the code for, and execution point in, the currently selected message, and inspectors on both the receiver of the currently selected message, and the variables in the current context.!


!Debugger class methodsFor: 'instance creation' stamp: 'jmv 8/1/2022 18:44:15'!
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
	^aContext! !

!Debugger class methodsFor: 'opening' stamp: 'jmv 8/1/2022 18:45:10'!
openContext: aContext label: aString contents: contentsStringOrNil
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

	<primitive: 19> "Simulation guard"

	(Preferences at: #logDebuggerStackToFile) ifTrue:
		[Smalltalk logError: aString inContext: aContext to: 'CuisDebug'].
	self informExistingDebugger: aContext label: aString.
	(Debugger new
		process: Processor activeProcess
		context: aContext)
			openNotifierContents: contentsStringOrNil
			label: aString.
	Processor activeProcess suspend.! !

!methodRemoval: Debugger class #errorRecursion stamp: 'jmv 8/1/2022 18:44:10'!
Debugger class removeSelector: #errorRecursion!
!methodRemoval: Debugger class #releaseClassCachedState stamp: 'jmv 8/1/2022 18:44:43'!
Debugger class removeSelector: #releaseClassCachedState!
!classDefinition: #Debugger category: #'Tools-Debugger'!
CodeProvider subclass: #Debugger
	instanceVariableNames: 'interruptedProcess contextStack contextStackTop contextStackIndex contextStackList receiverInspector contextVariablesInspector savedCursor labelString interruptedProcessUI'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Debugger'!
