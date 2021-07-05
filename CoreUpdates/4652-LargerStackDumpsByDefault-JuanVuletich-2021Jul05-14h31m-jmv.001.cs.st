'From Cuis 5.0 [latest update: #4651] on 5 July 2021 at 2:36:45 pm'!

!Debugger class methodsFor: 'constants' stamp: 'jmv 7/5/2021 14:31:30'!
defaultDebugStackSize
	^50! !


!Debugger methodsFor: 'context stack (message list)' stamp: 'jmv 7/5/2021 14:32:39'!
expandStack
	"A Notifier is being turned into a full debugger.  Show a substantial amount of stack in the context pane."

	self newStack: (contextStackTop stackOfSize: Debugger defaultDebugStackSize).
	contextStackIndex _ 0.
	receiverInspector _ Inspector inspect: nil.
	contextVariablesInspector _ ContextVariablesInspector inspect: nil.
	proceedValue _ nil! !


!ProcessBrowser methodsFor: 'stack list' stamp: 'jmv 7/5/2021 14:34:21'!
updateStackList
	self updateStackList: Debugger defaultDebugStackSize! !


!ProcessBrowser class methodsFor: 'CPU utilization' stamp: 'jmv 7/5/2021 14:32:32'!
dumpPigStackOn: aStream
	"Must run forked on its own process, so the monitored behavior is not affected too much"

	| promise tally process stack suspendedContext |
	promise := Processor tallyCPUUsageFor: 1 every: 10.
	tally := promise value.
	"UISupervisor whenUIinSafeState: [self dumpTallyOnTranscript: tally]."
	aStream nextPutAll: '====Al processes===='; newLine.
	self dumpTally: tally on: aStream.
	aStream newLine; nextPutAll: '====Process using most CPU===='; newLine.
	process _ tally sortedCounts first value.
	(100.0 * (tally occurrencesOf: process) / tally size) rounded printOn: aStream.
	aStream
		nextPutAll: ' % ';
		nextPutAll: (process browserPrintStringFull: false);
		newLine.
	stack _ process == Processor activeProcess
		ifTrue: [thisContext stackOfSize: Debugger defaultDebugStackSize]
		ifFalse: [
			suspendedContext _ process suspendedContext.
			suspendedContext
				ifNotNil: [suspendedContext stackOfSize: Debugger defaultDebugStackSize]].
	stack 
		ifNil: [ aStream nextPutAll: 'No context'; newLine]
		ifNotNil: [
			stack do: [ :c | 
				c printOn: aStream.
				aStream newLine ]].
	^process! !


!ClassDescription methodsFor: 'accessing method dictionary' stamp: 'jmv 7/5/2021 14:33:38'!
recoverFromMDFaultWithTrace
	"This method handles emthodDict faults to support, eg, discoverActiveClasses (qv)."
	self recoverFromMDFault.
	Smalltalk at: #MDFaultDict ifPresent:
		[:faultDict | faultDict at: self name put:
			(String streamContents: [ :strm |
				(thisContext stackOfSize: Debugger defaultDebugStackSize) do: [ :item |
					strm print: item; newLine]])]

"Execute the following statement to induce MD fault tracing.  This means that, not only will all active classes be recorded but, after a test run, MDFaultDict will contain, for every class used, a stack trace showing how it came to be used.  This statement should be executed just prior to any such text, in order to clear the traces.

	Smalltalk at: #MDFaultDict put: Dictionary new.

"! !


!ContextPart methodsFor: 'debugger access' stamp: 'jmv 7/5/2021 14:35:18'!
errorReportOn: strm
	"Write a detailed error report on the stack (above me) on a stream.  For both the error file, and emailing a bug report.  Suppress any errors while getting printStrings.  Limit the length."

	| cnt aContext startPos |
 	strm print: Date today; space; print: Time now; newLine.
	strm newLine.
	strm nextPutAll: 'VM: ';
		nextPutAll: Smalltalk platformName asString;
		nextPutAll: ' - ';
		nextPutAll: Smalltalk vmVersion asString;
		newLine.
	strm nextPutAll: 'Image: ';
		nextPutAll: Smalltalk version asString;
		nextPutAll: ' [';
		nextPutAll: Smalltalk lastUpdateString asString;
		nextPutAll: ']';
		newLine.
	strm newLine.
	
	"Note: The following is an open-coded version of ContextPart>>stackOfSize: since this method may be called during a low space condition and we might run out of space for allocating the full stack."
	cnt _ 0.  startPos _ strm position.
	aContext _ self.
	[aContext notNil and: [(cnt _ cnt + 1) < Debugger defaultDebugStackSize]] whileTrue: [
		aContext printDetails: strm.	"variable values"
		strm newLine.
		aContext _ aContext sender].

	strm newLine; nextPutAll: '--- The full stack ---'; newLine.
	aContext _ self.
	cnt _ 0.
	[aContext == nil] whileFalse: [
		cnt _ cnt + 1.
		cnt = Debugger defaultDebugStackSize ifTrue: [strm nextPutAll: ' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -'; newLine ].
		strm print: aContext; newLine.  "just class>>selector"	

		strm position > (startPos+40000) ifTrue: [strm nextPutAll: '...etc...'.
			^ self]. 	"exit early"
		cnt > 100 ifTrue: [strm nextPutAll: '-- and more not shown --'.  ^ self].
		aContext _ aContext sender].
! !

!ContextPart methodsFor: 'debugger access' stamp: 'jmv 7/5/2021 14:35:26'!
shortErrorReportOn: strm
	"Write a short error report on the stack (above me) on a stream.  For both the error file, and emailing a bug report. "

	| cnt aContext |
 	strm print: Date today; space; print: Time now; newLine.
	aContext _ self.
	cnt _ 0.
	[aContext notNil and: [(cnt _ cnt + 1) < Debugger defaultDebugStackSize]] whileTrue: [
		strm print: aContext; newLine.  "just class>>selector"	
		aContext _ aContext sender]! !

!ContextPart methodsFor: 'debugger access' stamp: 'jmv 7/5/2021 14:33:53'!
shortStack
	"Answer a String showing the top ten contexts on my sender chain."

	^ String streamContents: [ :strm |
		(self stackOfSize: Debugger defaultDebugStackSize)
			do: [:item | strm print: item; newLine]]! !

!methodRemoval: ContextPart #longStack stamp: 'jmv 7/5/2021 14:33:11'!
ContextPart removeSelector: #longStack!

!Debugger class reorganize!
('class initialization' openContext:label:contents: releaseClassCachedState)
('instance creation' context: informExistingDebugger:label:)
('opening' interruptProcess:label: openInterrupt:onProcess: openOn:context:label:fullView:)
('cached state access' errorRecursion)
('constants' defaultDebugStackSize)
!

