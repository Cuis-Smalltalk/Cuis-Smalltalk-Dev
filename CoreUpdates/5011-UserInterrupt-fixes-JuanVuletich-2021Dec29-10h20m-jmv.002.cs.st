'From Cuis 5.0 [latest update: #5009] on 29 December 2021 at 10:30:17 am'!

!ProcessBrowser class methodsFor: 'CPU utilization' stamp: 'jmv 12/29/2021 10:24:34'!
findPigProcess
	"Must run forked on its own process, so the monitored behavior is not affected too much"

	| promise tally process |
	promise _ Processor tallyCPUUsageFor: 1 every: 10.
	tally _ promise value.
	process _ tally sortedCounts first value.
	"Avoid, if possible, the background process."
	(process == Processor backgroundProcess and: [ tally sortedCounts size >= 2 ]) ifTrue: [
		process _ tally sortedCounts second value ].
	^process! !


!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 12/29/2021 10:25:30'!
processTakingMostCPU
	"Must run forked on its own process, so the monitored behavior is not affected too much"

	^ ProcessBrowser findPigProcess! !


!ProcessBrowser class methodsFor: 'CPU utilization' stamp: 'jmv 12/29/2021 10:24:49'!
dumpPigStackOn: aStream
	"Must run forked on its own process, so the monitored behavior is not affected too much"

	| promise tally process stack suspendedContext |
	promise _ Processor tallyCPUUsageFor: 1 every: 10.
	tally _ promise value.
	process _ tally sortedCounts first value.
	"Avoid, if possible, the background process."
	(process == Processor backgroundProcess and: [ tally sortedCounts size >= 2 ]) ifTrue: [
		process _ tally sortedCounts second value ].
	"UISupervisor whenUIinSafeState: [self dumpTallyOnTranscript: tally]."
	aStream nextPutAll: '====Al processes===='; newLine.
	self dumpTally: tally on: aStream.
	aStream newLine; nextPutAll: '====Process using most CPU===='; newLine.
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


!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 12/29/2021 10:27:31'!
handleUserInterrupt
	| p |
	"Shift can only be detected on the Mac, due to likely buggy keystroke reporting..."
	p _ Sensor shiftPressed | Preferences cmdDotInterruptTakesStatistics
		ifTrue: [Utilities reportCPUandRAM]
		ifFalse: [Utilities processTakingMostCPU].
	Preferences cmdDotEnabled ifTrue: [
		"The background process can't be interrupted, or Cuis will likely crash."
		p == Processor backgroundProcess
			ifTrue: [ 'Can not interrupt backgroundProcess' print ]
			ifFalse: [
					[
						EventSensor install.
						p isTerminated
							ifTrue: [ ('Process {', p printString, '} isTerminated. Can not interrupt it.') print ]
							ifFalse: [ UISupervisor userInterrupt: p ]] fork
				]
		]! !


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 12/29/2021 10:28:47'!
userInterrupt: aProcess
	"Open a debugger."

	Debugger interruptProcess: aProcess label: 'User Interrupt'.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

EventSensor install.!

