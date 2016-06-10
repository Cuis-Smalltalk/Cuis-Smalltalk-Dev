'From Cuis 4.2 of 25 July 2013 [latest update: #2811] on 10 June 2016 at 12:38:01.570127 pm'!
!classDefinition: #QSystemTally category: #'Tools-Profiling'!
QAbstractTally subclass: #QSystemTally
	instanceVariableNames: 'class method blockNesting wasInPrimitive '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!

!QAbstractTally methodsFor: 'tallying' stamp: 'jmv 6/9/2016 16:11'!
tallyPrimInMethod: aMethod by: count
	| node |
	node := receivers.
	[node == nil] whileFalse:[
		(node method == aMethod and: [ node wasInPrimitive ]) ifTrue: [
			^node addTallies: count ].
		node := node nextLink.
	].
	node := QSystemTally new class: aMethod methodClass method: aMethod nesting: 0.
	node wasInPrimitive: true.
	node process: process.
	node nextLink: receivers.
	receivers := node.
	^node addTallies: count! !


!QSystemTally methodsFor: 'accessing' stamp: 'jmv 6/9/2016 15:41'!
wasInPrimitive
	^ wasInPrimitive! !

!QSystemTally methodsFor: 'initialization' stamp: 'jmv 6/9/2016 15:39'!
initialize
	super initialize.
	wasInPrimitive _ false! !

!QSystemTally methodsFor: 'initialization' stamp: 'jmv 6/9/2016 15:39'!
wasInPrimitive: aBoolean
	wasInPrimitive _ aBoolean! !


!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 6/9/2016 16:25'!
reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream
	"Print a report, with cutoff percentage of each element of the tree 
	(leaves, roots, tree)=2, on the stream, strm."

	self reportTextOn: textStream linesOn: linesStream talliesOn: talliesStream cutoff: 0.2! !

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 6/9/2016 16:12'!
runProfilerProcess
	"Run the profiler process"

	| process tallyStart tallyTicks tallyStop methodForPrimitiveWhileTakingSample parentNode |
	totalTally := 0.
	Smalltalk profileSemaphore: semaphore.
	totalTicks := 0.
	tallyStart := tallyStop := Time primHighResClock.
	[ true ]
		whileTrue: [ 
			tallyStart := tallyStop.
			tallyStart := Time primHighResClock.
			Smalltalk profileStart: ticks.	"run for n ticks"
			semaphore wait.
			tallyStop := Time primHighResClock.
			tallyTicks := tallyStop - tallyStart.
			totalTicks := totalTicks + tallyTicks.
			process := Smalltalk profileSample.
			methodForPrimitiveWhileTakingSample := Smalltalk profilePrimitive.
			totalTally := totalTally + 1.
			process
				ifNotNil: [
					methodForPrimitiveWhileTakingSample
						ifNil: [
							tallyRoot
								tally: (process suspendedContext ifNil: [ thisContext ])
								inProcess: process
								by: tallyTicks.
							]
						ifNotNil: [
							"The intention of this code is record which primitive was running when the VM took the sample."
							"In Eliot Miranda's words: 
								AndreasSystemProfiler is more accurate because it uses VM support to tell it which primtiive was running when it took a sample. 
								MessageTally simply ascribes a primitive's cost to the method at the next suspendsion point, which, in some contexts, 
								can yield wildly misleading results."
							"The problem is that knowing just the primitive and the process doesn't give us the complete call stack.
							So, this is, in a sense, approximate."
							"
								AndreasSystemProfiler spyOn: [
							 	       [ #((1 2 3)) do: [ :each |
 								               each findLast: [ :ea |
								                        ea squared = ea ] ] ] bench ].
							The older code, for this example
								AndreasSystemProfiler spyOn:[10000 timesRepeat: [3.14159 printString]]
							gave:
								  |  2.9% (7 ms) (Number>>#raisedToInteger:)
								  |    2.2% (5 ms) (Float>>#timesTwoPower: )
							but #raisedToInteger: does NOT send #timesTwoPower:
							Approach taken: Add to parent node, but print with a note that specifies this is primitives, and maybe parent node is missing"
							parentNode _ tallyRoot
								tally: (process suspendedContext ifNil: [ thisContext ]) sender
								inProcess: process
								by: tallyTicks.
							parentNode
								tallyPrimInMethod: methodForPrimitiveWhileTakingSample by: tallyTicks
							]]]! !


!QAbstractTally methodsFor: 'report' stamp: 'jmv 6/9/2016 16:26'!
sonsOver: threshold
	"Answer the sons with tallys over the given threshold"
	receivers ifNil:[^#()].
	^receivers asArray select:[:son | son tally >= threshold].! !


!QSystemTally methodsFor: 'report' stamp: 'jmv 6/9/2016 18:42'!
printOn: textStream linesOn: linesStream talliesOn: talliesStreams tabs: tabsAndTreeLines total: total totalTime: totalTime

	| aSelector aClass percentage line |
	line _ String streamContents: [ :lineStream |
		tabsAndTreeLines do: [ :tabOrLineChar | lineStream nextPutAll: tabOrLineChar ].
		percentage _ tally asFloat / total * 100.0.
		percentage printOn: lineStream fractionDigits: 2.
		lineStream
			nextPutAll: '% (';
			nextPutAll: (percentage * totalTime / 100) rounded printStringWithCommas;
			nextPutAll: ' ms) '.
		aSelector _ class selectorAtMethod: method setClass: [ :c | aClass _ c].
			blockNesting > 0 ifTrue: [
				lineStream
					next: blockNesting put: $[;
					next: blockNesting put: $];
					space ].
		lineStream
			nextPutAll: class name;
			nextPutAll: (aClass = class 
					ifTrue: ['>>']
					ifFalse: ['(' , aClass name , ')>>']);
			nextPutAll: aSelector.
		wasInPrimitive ifTrue: [ lineStream nextPutAll: '   -- primitive (real sender possibly omitted)' ].
		].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStreams nextPut: self! !

!QSystemTally methodsFor: 'tallying' stamp: 'jmv 6/9/2016 16:10'!
tally: context inProcess: aProcess by: count
	"Explicitly tally the specified context and its stack."
	| sender senderNode |

	"Add to this node if appropriate"
	(context receiver class == class and: [ context method == method and: [ aProcess == process | process isNil and: [ (self blockNestingCountOf: context) = blockNesting ]]]) ifTrue: [
		self wasInPrimitive ifFalse: [
			self addTallies: count.
			^self ]].

	sender _ context sender.
	"No sender means end of recursion. Add new root to the tree."
	sender ifNil: [
		self addTallies: count.
		^self tallyChild: context inProcess: aProcess by: count ].

	"Find the node for the sending context (or add it if necessary).
	Then, add us."
	senderNode _ self tally: sender inProcess: aProcess by: count.
	^senderNode tallyChild: context inProcess: aProcess by: count! !

!QSystemTally methodsFor: 'tallying' stamp: 'jmv 6/9/2016 16:10'!
tallyChild: context inProcess: aProcess by: count
	"Find a suitable child, or add a new one if needed."
	| aMethod node nesting |
	aMethod _ context method.
	nesting _ self blockNestingCountOf: context.
	node _ receivers.
	[ node == nil ] whileFalse: [
		(context receiver class == node actualReceiverClass and: [aMethod == node method and: [ node process == aProcess and: [ nesting = node blockNestingCount ]]]) ifTrue: [
			node wasInPrimitive ifFalse: [
				^node addTallies: count ]].
		node _ node nextLink ].
	node _ QSystemTally new
				class: context receiver class
				method: aMethod
				nesting: nesting.
	node process: aProcess.
	node nextLink: receivers.
	receivers _ node.
	^node addTallies: count! !

!methodRemoval: QSystemTally #tallyFakeBy:!
QSystemTally removeSelector: #tallyFakeBy:!
!methodRemoval: QAbstractTally #isFake!
QAbstractTally removeSelector: #isFake!
!methodRemoval: QAbstractTally #tallyMethod:by:!
QAbstractTally removeSelector: #tallyMethod:by:!
!classDefinition: #QSystemTally category: #'Tools-Profiling'!
QAbstractTally subclass: #QSystemTally
	instanceVariableNames: 'class method blockNesting wasInPrimitive'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!classRemoval: #QFakeTally!
Smalltalk removeClassNamed: #QFakeTally!
