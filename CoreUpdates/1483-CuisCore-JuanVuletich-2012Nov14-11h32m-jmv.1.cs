'From Cuis 4.0 of 21 April 2012 [latest update: #1482] on 14 November 2012 at 11:32:57 am'!

!MessageTally commentStamp: '<historical>' prior: 0!
My instances observe and report the amount of time spent in methods.

NOTE: a higher-level user interface (combining the MessageTally result tree with a method browser) is available from TimeProfileBrowser.

MessageTally provides two different strategies available for profiling:

* spyOn: and friends use a high-priority Process to interrupt the block or process being spied on at periodic intervals. The interrupted call stack is then examined for caller information.

* tallySends: and friends use the interpreter simulator to run the block, recording every method call.

The two give you different results:

* spyOn: gives you a view of where the time is being spent in your program, at least on a rough statistical level (assuming you've run the block for long enough and have a high enough poll rate). If you're trying to optimize your code, start here and optimize the methods where most of the time is being spent first.

* tallySends: gives you accurate counts of how many times methods get called, and by exactly which route. If you're debugging, or trying to figure out if a given method is getting called too many times, this is your tool.

You can change the printing format (that is, the whitespace and string compression) by using these instance methods: 
	maxClassNameSize:
	maxClassPlusSelectorSize:
	maxTabs:

You can change the default polling period (initially set to 1) by calling
	MessageTally defaultPollPeriod: numberOfMilliseconds

Q: How do you interpret MessageTally>>tallySends:
A: The methods #tallySends: and #spyOn: measure two very different quantities, but broken down in the same who-called-who format.  #spyOn: is approximate, but more indicative of real time spent, whereas #tallySends: is exact and a precise record of how many times each method got executed.!


!MessageTally methodsFor: 'initialize-release' stamp: 'jmv 11/12/2012 12:01'!
spyEvery: millisecs on: aBlock
	"Create a spy and spy on the given block at the specified rate."
	"Spy only on the active process (in which aBlock is run)"

	| myDelay time0 observedProcess |
	aBlock isBlock
		ifFalse: [ self error: 'spy needs a block here' ].
	self class: aBlock receiver class method: aBlock method.
		"set up the probe"
	observedProcess _ Processor activeProcess.
	myDelay := Delay forMilliseconds: millisecs.
	time0 := Time millisecondClockValue.
	gcStats _ Smalltalk getVMParameters.
	Timer ifNotNil: [ self error: 'it seems a tally is already running' ].
	Timer _ [
		[ true ] whileTrue: [
			| startTime |
			startTime := Time millisecondClockValue.
			myDelay wait.

			self
				tally: Processor preemptedProcess suspendedContext
				in: (observedProcess == Processor preemptedProcess ifTrue: [observedProcess])
				"tally can be > 1 if ran a long primitive"
				by: (Time millisecondClockValue - startTime) // millisecs].
		nil] newProcess.
	Timer priority: Processor timingPriority-1.
	Timer name: 'MessageTally'.
		"activate the probe and evaluate the block"
	Timer resume.
	^ aBlock ensure: [
		"cancel the probe and return the value"
		"Could have already been terminated. See #terminateTimerProcess"
		Timer ifNotNil: [
			Timer terminate.
			Timer _ nil ].
		"Collect gc statistics"
		Smalltalk getVMParameters keysAndValuesDo: [ :idx :gcVal |
			gcVal ifNotNil: [
				gcStats at: idx put: (gcVal - (gcStats at: idx))]].
		time := Time millisecondClockValue - time0]! !

!MessageTally methodsFor: 'printing' stamp: 'jmv 11/11/2012 22:38'!
printOn: aStream total: total totalTime: totalTime tallyExact: isExact 

	isExact 
		ifTrue: [
			| myTally |
			myTally := tally.
			receivers
				ifNotNil: [receivers do: [:r | myTally := myTally - r tally]].
			aStream
				print: myTally;
				space]
		ifFalse: [
			| percentage |
			percentage := tally asFloat / total * 100.0.
			aStream
				nextPutAll: (percentage printShowingDecimalPlaces: 1);
				nextPutAll: '% (';
				nextPutAll: (percentage * totalTime / 100) rounded asStringWithCommas;
				nextPutAll: 'ms) '].
	receivers
		ifNil: [
			aStream
				nextPutAll: 'primitives';
				newLine]
		ifNotNil: [
			| className aSelector aClass |
			aSelector := class selectorAtMethod: method setClass: [ :c | aClass := c].
			className := aClass name contractTo: self maxClassNameSize.
			aStream
				nextPutAll: class name;
				nextPutAll: (aClass = class 
							ifTrue: ['>>']
							ifFalse: ['(' , aClass name , ')>>']);
				nextPutAll: (aSelector 
							contractTo: self maxClassPlusSelectorSize - className size);
				newLine]! !

!MessageTally methodsFor: 'printing' stamp: 'jmv 11/12/2012 12:47'!
rootPrintOn: aStream total: total totalTime: totalTime threshold: threshold

	| groups |
	groups _ (self sonsOver: threshold) groupBy: [ :aTally | aTally process] having: [ :g | true ].
	groups do: [ :g |
		| sons p |
		sons _ g asArray sort.
		p _ g anyOne process.
		(reportOtherProcesses or: [ p notNil ]) ifTrue: [
			aStream nextPutAll: '--------------------------------'; newLine.
			aStream nextPutAll: 'Process: ',  (p ifNil: [ 'other processes'] ifNotNil: [ p browserPrintString]); newLine.
			aStream nextPutAll: '--------------------------------'; newLine.
			sons do: [ :son |
				son
					treePrintOn: aStream
					tabs: OrderedCollection new
					thisTab: ''
					total: total
					totalTime: totalTime
					tallyExact: false
					orThreshold: threshold].
			aStream newLine].
	]! !

!MessageTally methodsFor: 'tallying' stamp: 'jmv 11/12/2012 13:43'!
tally: context in: aProcess by: count
	"Explicitly tally the specified context and its stack."
	| sender |

	"Add to this node if appropriate"
	context method == method ifTrue: [^self bumpBy: count].
	
	"No sender? Add new branch to the tree."
	sender _ context home sender.
	sender ifNil: [
		^ (self bumpBy: count) tallyPath: context in: aProcess by: count].
	
	"Find the node for the sending context (or add it if necessary)"
	^ (self tally: sender in: aProcess by: count) tallyPath: context in: aProcess by: count! !


!MessageTally class methodsFor: 'spying' stamp: 'jmv 11/12/2012 15:20'!
spyAllOn: aBlock
	"Spy on all the processes in the system
	
	[1000 timesRepeat: [3.14159 printString. Processor yield]] forkNamed: 'p1'.
	[1000 timesRepeat: [30 factorial. Processor yield]] forkNamed: 'p2'.
	[1000 timesRepeat: [30 factorial. Processor yield]] forkNamed: 'p3'.
	MessageTally spyAllOn: [ (Delay forMilliseconds: 100) wait]
	
	"
	| node result |
	node _ self new.
	node reportOtherProcesses: true.	"Irrelevant in this case. All processes will be reported on their own."
	result _ node spyAllEvery: self defaultPollPeriod on: aBlock.
	SystemWindow
		editText: (Workspace withText: (String streamContents: [:s | node report: s]))
		label: 'Spy Results'
		wrap: false.
	^ result! !

!MessageTally class methodsFor: 'spying' stamp: 'jmv 11/12/2012 11:23'!
spyOn: aBlock
	"
	Spy on aBlock, in the current process. Do not include statistics on other processes in the report.
	[1000 timesRepeat: [
		100 timesRepeat: [120 factorial].
		(Delay forMilliseconds: 10) wait
		]] forkAt: 45 named: '45'.
	MessageTally spyOn: [10000 timesRepeat: [1.23 printString]]
	"
	^self spyOn: aBlock reportOtherProcesses: false! !


!Process methodsFor: 'printing' stamp: 'jmv 11/12/2012 12:40'!
browserPrintStringWith: anObject

	^String streamContents: [ :stream |
		stream nextPut: $(.
		priority printOn: stream.
		self isSuspended
			ifTrue: [ stream nextPut: $s ].
		stream nextPutAll: ') '.
		self hash printOn: stream.
		stream space.
		stream nextPut: $'.
		stream nextPutAll: self name.
		stream nextPut: $'.
		anObject ifNotNil: [
			stream
				nextPutAll: ': ';
				nextPutAll: anObject asString ]]! !

