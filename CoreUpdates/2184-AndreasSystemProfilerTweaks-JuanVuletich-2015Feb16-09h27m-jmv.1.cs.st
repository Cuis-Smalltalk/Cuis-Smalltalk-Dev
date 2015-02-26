'From Cuis 4.2 of 25 July 2013 [latest update: #2183] on 16 February 2015 at 10:01:40.94083 am'!

!BlockClosure methodsFor: 'evaluating' stamp: 'jmv 1/28/2015 19:54'!
bench
	"See how many times I can value in 5 seconds.  I'll answer a meaningful description."

	| startTime endTime count roundTo3Digits |
	roundTo3Digits := [:num |
		| rounded lowDigit |
		rounded := (num * 1000) rounded. "round to 1/1000"
		lowDigit := (rounded numberOfDigitsInBase: 10) - 3. "keep only first 3 digits"
		rounded := rounded roundTo:(10 raisedTo: lowDigit).
		(lowDigit >= 3 or: [rounded \\ 1000 = 0]) "display fractional part only when needed"
			ifTrue: [(rounded // 1000) printStringWithCommas]
			ifFalse: [(rounded / 1000.0) printString]].
	count := 0.
	endTime := Time millisecondClockValue + 5000.
	startTime := Time millisecondClockValue.
	[ Time millisecondClockValue > endTime ] whileFalse: [ self value.  count := count + 1 ].
	endTime := Time millisecondClockValue.
	^count = 1
		ifTrue: [ (roundTo3Digits value: (endTime - startTime) / 1000) , ' seconds.' ]
		ifFalse:
			[ (roundTo3Digits value: (count * 1000) / (endTime - startTime)) , ' per second.' ]! !


!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 2/16/2015 09:55'!
runProfilerProcess
	"Run the profiler process"

	| process tallyStart tallyTicks tallyStop methodForPrimitiveWhileTakingSample leaf |
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
					leaf := tallyRoot
						tally: (process suspendedContext ifNil: [ thisContext ])
						inProcess: process
						by: tallyTicks.
					false ifTrue: [
						"The intention of this code is record which primitive was running when the VM took the sample."
						"In Eliot Miranda's words: 
							AndreasSystemProfiler is more accurate because it uses VM support to tell it which primtiive was running when it took a sample. 
							MessageTally simply ascribes a primitive's cost to the method at the next suspendsion point, which, in some contexts, 
							can yield wildly misleading results.
						"
						"This works ok, for example for:
							AndreasSystemProfiler spyOn: [
							        [ #((1 2 3)) do: [ :each |
 							               each findLast: [ :ea |
							                        ea squared = ea ] ] ] bench ].
						But the place in the call tree is not always correct. For instance for:
							AndreasSystemProfiler spyOn:[10000 timesRepeat: [3.14159 printString]]
						we get
							  |  2.9% (7 ms) (Number>>#raisedToInteger:)
							  |    2.2% (5 ms) (Float>>#timesTwoPower: )
						but #raisedToInteger: does NOT send #timesTwoPower:
						
						Disable it at least for now."
						methodForPrimitiveWhileTakingSample ifNotNil: [
							leaf tallyMethod: methodForPrimitiveWhileTakingSample by: tallyTicks ]]
					]]! !

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 2/8/2015 06:18'!
spyOn: aBlock
	"Profile system activity during execution of aBlock."
"	tallyRoot := QSystemTally new class: aBlock receiver class method: aBlock method nesting: 1."
	tallyRoot := QSystemTally new class: thisContext receiver class method: thisContext method nesting: 0.
	self startProfiling.
	^aBlock ensure: [ self stopProfiling ]! !


!QSystemTally methodsFor: 'converting' stamp: 'jmv 2/16/2015 09:58'!
asArray
	"esto es feo. normalmente los link esta en un LinkedList.
	Estamos de acuerdo que usualmente es el pedo... pero, es el elemento una coleccion o no????"
	"(jmv) (style) Mhhh. Normally instances of Link are in a LinkedList, that provides collection protocol.
	Here, we allow a subInstance of Link to start behaving as a Collection.
	Not bad per se, but inconsistent with other uses of Link..."
	| link |
	^Array streamContents:[:s|
		link := self.
		[link == nil] whileFalse:[
			s nextPut: link.
			link := link nextLink.
		].
	].! !

!QSystemTally methodsFor: 'tallying' stamp: 'jmv 2/16/2015 09:56'!
tallyMethod: aMethod by: count
	"Called explicitly and needs to decrement receiver's tally count"
	"???"
	
	"Not really nice. Currently disabled. See comment at sender."
	| node |
	node := receivers.
	[node == nil] whileFalse:[
		node method == aMethod ifTrue:[^node addTallies: count].
		node := node nextLink.
	].
	node := QSystemTally new class: aMethod methodClass method: aMethod nesting: 0.
	node process: process.
	node nextLink: receivers.
	receivers := node.
	^node addTallies: count! !

