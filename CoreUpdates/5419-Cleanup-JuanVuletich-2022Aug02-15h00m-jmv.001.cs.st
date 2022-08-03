'From Cuis 6.0 [latest update: #5418] on 2 August 2022 at 3:05:52 pm'!
!classDefinition: #Debugger category: #'Tools-Debugger'!
CodeProvider subclass: #Debugger
	instanceVariableNames: 'interruptedProcess contextStack contextStackTop contextStackIndex contextStackList receiverInspector contextVariablesInspector savedCursor labelString interruptedProcessUI sendProceeds '
	classVariableNames: 'ErrorRecursion '
	poolDictionaries: ''
	category: 'Tools-Debugger'!
!classDefinition: #Inspector category: #'Tools-Inspector'!
TextProvider subclass: #Inspector
	instanceVariableNames: 'acceptedContentsCache object selectionIndex context bindings '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Inspector'!

!Debugger class methodsFor: 'opening' stamp: 'jmv 8/1/2022 18:17:35'!
openDebugging: aBlock to: aCompiledMethod label: aString
	| processToDebug debugger |
	debugger _ Debugger new.
	processToDebug _ aBlock newProcess name: 'debugIt'.
	debugger process: processToDebug context: processToDebug suspendedContext.
	debugger openFullMorphicLabel: aString.
	[debugger interruptedContext method == aCompiledMethod]
		whileFalse: [debugger send]! !


!Debugger methodsFor: 'context stack menu' stamp: 'jmv 8/1/2022 18:28:00'!
send
	"Send the selected message in the accessed method, and take control in 
	the method invoked to allow further step or send."

	self checkContextSelection.
	interruptedProcess step: self selectedContext.
	self resetContext: interruptedProcess stepToSendOrReturn.
! !


!Debugger class methodsFor: 'opening' stamp: 'jmv 8/1/2022 18:13:25'!
openContext: aContext label: aString contents: contentsStringOrNil
	"Open a notifier in response to an error, halt, or notify. A notifier view just shows a short view of the sender stack and provides a menu that lets the user open a full debugger."

	<primitive: 19> "Simulation guard"

	(self errorRecursion not and: [Preferences at: #logDebuggerStackToFile]) ifTrue:
		[Smalltalk logError: aString inContext: aContext to: 'CuisDebug'].
	ErrorRecursion ifTrue: [
		ErrorRecursion _ false.
		contentsStringOrNil
			ifNil: [
				self primitiveError: 'Can not open debugger due to recursion error.', 
					String newLineString, aString]
			ifNotNil: [
				self primitiveError: 'Can not open debugger due to recursion error.', 
					String newLineString, aString, String newLineString, contentsStringOrNil ]].
	ErrorRecursion _ true.
	[self informExistingDebugger: aContext label: aString.
	(Debugger new
		process: Processor activeProcess
		context: aContext)
			openNotifierContents: contentsStringOrNil
			label: aString ] ensure: [ ErrorRecursion _ false ].
	Processor activeProcess suspend.
! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 8/1/2022 18:27:40'!
debug: aCompiledMethod receiver: anObject in: evalContext

	Debugger
		openDebugging: [
			aCompiledMethod
				valueWithReceiver: anObject
				arguments: (evalContext ifNil: [ #() ] ifNotNil: [ { evalContext } ])]
		to: aCompiledMethod
		label: 'Debug it'.! !


!TestCase methodsFor: 'debugging - private' stamp: 'jmv 8/1/2022 18:30:15'!
openDebuggerOnFailingTestMethod

	Debugger
		openDebugging: [
			[ self performTest ] ensure: [
			self tearDown.
			self tearDownResources]]
		to: self methodForTest
		label: 'Debug failed test ', self printString.! !

!methodRemoval: Inspector #context: stamp: 'jmv 8/2/2022 15:03:17'!
Inspector removeSelector: #context:!
!methodRemoval: Debugger class #context: stamp: 'jmv 8/2/2022 15:03:14'!
Debugger class removeSelector: #context:!
!methodRemoval: Debugger #receiverInspectorObject:context: stamp: 'jmv 8/2/2022 15:01:58'!
Debugger removeSelector: #receiverInspectorObject:context:!
!methodRemoval: Debugger #initialize stamp: 'jmv 8/2/2022 15:04:22'!
Debugger removeSelector: #initialize!
!methodRemoval: Debugger #sendProceeds stamp: 'jmv 8/2/2022 15:05:05'!
Debugger removeSelector: #sendProceeds!
!classDefinition: #Debugger category: #'Tools-Debugger'!
CodeProvider subclass: #Debugger
	instanceVariableNames: 'interruptedProcess contextStack contextStackTop contextStackIndex contextStackList receiverInspector contextVariablesInspector savedCursor labelString interruptedProcessUI'
	classVariableNames: 'ErrorRecursion'
	poolDictionaries: ''
	category: 'Tools-Debugger'!
!classDefinition: #Inspector category: #'Tools-Inspector'!
TextProvider subclass: #Inspector
	instanceVariableNames: 'acceptedContentsCache object selectionIndex bindings'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Inspector'!
