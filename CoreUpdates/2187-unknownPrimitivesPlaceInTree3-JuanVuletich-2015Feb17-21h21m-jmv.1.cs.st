'From Cuis 4.2 of 25 July 2013 [latest update: #2184] on 17 February 2015 at 11:05:05.522556 pm'!

!QFakeTally methodsFor: 'accessing' stamp: 'jmv 2/17/2015 22:48'!
actualReceiverClass
	^nil! !


!QSystemTally methodsFor: 'tallying' stamp: 'jmv 2/17/2015 23:02'!
tallyChild: context inProcess: aProcess by: count
	"Find a suitable child, or add a new one if needed."
	| aMethod node nesting |
	aMethod _ context method.
	nesting _ self blockNestingCountOf: context.
	node _ receivers.
	[ node == nil ] whileFalse: [
		(context receiver class == node actualReceiverClass and: [aMethod == node method and: [ node process == aProcess and: [ nesting = node blockNestingCount ]]]) ifTrue: [
			^node addTallies: count ].
		node _ node nextLink ].
	node _ QSystemTally new
				class: context receiver class
				method: aMethod
				nesting: nesting.
	node process: aProcess.
	node nextLink: receivers.
	receivers _ node.
	^node addTallies: count! !

!QSystemTally methodsFor: 'accessing' stamp: 'jmv 2/17/2015 22:48'!
actualReceiverClass
	^class! !


!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 2/17/2015 22:41'!
startProfiling
	"Start the profiler process taking samplesPerMsec samples per *milli* second"
	| t0 |
	semaphore := Semaphore new.
	"Run a 100 msec busy loop to figure out the ticks per msec"
	t0 := Time localMillisecondClock + 2.
	[Time localMillisecondClock >= t0] whileFalse.
	ticksPerMSec := Time primHighResClock.
	[Time localMillisecondClock >= (t0 + 100)] whileFalse.
	ticksPerMSec := (Time primHighResClock - ticksPerMSec) 
			// (Time localMillisecondClock - t0).
	"Try to get 10 samples per msec"
	ticks := ticksPerMSec // 10.
	startTime := Time localMillisecondClock.
	vmStats := Smalltalk getVMParameters.
	profilerProcess := [self runProfilerProcess] newProcess.
	tallyRoot process: nil.
	profilerProcess priority: Processor timingPriority-1.
	profilerProcess name: 'AndreasSystemProfiler'.
	profilerProcess resume! !


!QSystemTally methodsFor: 'report' stamp: 'jmv 2/17/2015 22:44'!
printOn: aStream
	blockNesting timesRepeat: [
		aStream nextPutAll: '[] in '].
	aStream print: class; nextPutAll: '>>'; print: (method ifNotNil:[method selector]).
	aStream nextPutAll: ' -- '; print: tally! !

!QSystemTally methodsFor: 'tallying' stamp: 'jmv 2/17/2015 23:02'!
tally: context inProcess: aProcess by: count
	"Explicitly tally the specified context and its stack."
	| sender senderNode |

	"Add to this node if appropriate"
	(context receiver class == class and: [ context method == method and: [ aProcess == process | process isNil and: [ (self blockNestingCountOf: context) = blockNesting ]]]) ifTrue: [
		self addTallies: count.
		^self ].

	sender _ context sender.
	"No sender means end of recursion. Add new root to the tree."
	sender ifNil: [
		self addTallies: count.
		^self tallyChild: context inProcess: aProcess by: count ].

	"Find the node for the sending context (or add it if necessary).
	Then, add us."
	senderNode _ self tally: sender inProcess: aProcess by: count.
	^senderNode tallyChild: context inProcess: aProcess by: count! !

!QSystemTally methodsFor: 'tallying' stamp: 'jmv 2/17/2015 22:51'!
tallyFakeBy: count
	| node |
	node := receivers.
	[node == nil] whileFalse:[
		node isFake ifTrue:[^node addTallies: count].
		node := node nextLink.
	].
	node := QFakeTally new.
	node process: process.
	node nextLink: receivers.
	receivers := node.
	^node addTallies: count! !

!methodRemoval: QSystemTally #tallyLeafChild:inProcess:by:!
QSystemTally removeSelector: #tallyLeafChild:inProcess:by:!
