'From Cuis 5.0 [latest update: #5017] on 30 December 2021 at 11:16:02 am'!

!ProcessBrowser class methodsFor: 'CPU utilization' stamp: 'jmv 12/30/2021 11:15:50'!
dumpPigStackOn: aStream
	"Must run forked on its own process, so the monitored behavior is not affected too much"

	| promise tally process stack suspendedContext |
	promise _ Processor tallyCPUUsageFor: 1 every: 10.
	tally _ promise value.
	process _ tally sortedCounts first value.
	"Avoid, if possible, the background process, and terminated processes."
	tally sortedCounts do: [ :entry |
		(process == Processor backgroundProcess or: [process isTerminated])
			ifTrue: [ process _ entry value ]].
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

!ProcessBrowser class methodsFor: 'CPU utilization' stamp: 'jmv 12/30/2021 11:15:52'!
findPigProcess
	"Must run forked on its own process, so the monitored behavior is not affected too much"

	| promise tally process |
	promise _ Processor tallyCPUUsageFor: 0.1 every: 10.
	tally _ promise value.
	process _ tally sortedCounts first value.
	"Avoid, if possible, the background process, and terminated processes."
	tally sortedCounts do: [ :entry |
		(process == Processor backgroundProcess or: [process isTerminated])
			ifTrue: [ process _ entry value ]].
	^process! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
UISupervisor whenUIinSafeState: [
	EventSensor install.
	Delay startTimerEventLoop.
	DisplayScreen installScreenUpdater.
	WeakArray restartFinalizationProcess.
	Smalltalk installLowSpaceWatcher.
	'System processes restarted' print ].!