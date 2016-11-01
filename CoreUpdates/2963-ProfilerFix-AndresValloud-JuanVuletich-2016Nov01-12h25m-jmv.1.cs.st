'From Cuis 4.2 of 25 July 2013 [latest update: #2962] on 1 November 2016 at 12:45:27 pm'!

!Object methodsFor: 'profiler support' stamp: 'jmv 11/1/2016 12:44:53'!
profilerFriendlyCall: aBlock
	"In AndreasSystemProfiler, usually primitives are reported as children of the wrong node.
	The reason is that while the primitive is recorded, it is only taken into account at next suspension point.
	
	For long running primitives, and maybe only while profiling, this method provides a workaround.
	Evaluate and compare

		AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 timesTwoPower: 10000]].
		AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 profilerFriendlyTimesTwoPower: 10000]].

	Also see #runProfilerProcess

	Keep in mind there is a performance penaly each time this method is called.
	Consider doing it only for long-running primitives, of while profiling code.
	"

	"The apparently useless loop is to ensure we have a suspension point in this method, after block evaluation.
	Suspension points are activation of message sends and bytecode back jumps."
	| primResult primWasCalled |
	primWasCalled _ false.
	[ primWasCalled ] whileFalse: [
		primResult _ aBlock value.
		primWasCalled _ true ].
	^primResult! !


!BoxedFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 11/1/2016 12:17:15'!
profilerFriendlyPrimTimesTwoPower: anInteger 
	"
	This is an example on proper primitive reporting in AndreasSystemProfiler.
	See senders.
	"

	<primitive: 54>
	^nil! !

!BoxedFloat64 methodsFor: 'mathematical functions' stamp: 'jmv 11/1/2016 12:41:29'!
profilerFriendlyTimesTwoPower: anInteger 

	"This is an example on proper primitive reporting in AndreasSystemProfiler.
	It is a reimplementation of #timesTwoPower: in a Profiler friendly way.

	Compare the results of 
		AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 timesTwoPower: 10000]].
	and
		AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 profilerFriendlyTimesTwoPower: 10000]].

	See #profilerFriendlyCall:
	"
	
	| primResult |
	primResult _ self profilerFriendlyCall: [
		self profilerFriendlyPrimTimesTwoPower: anInteger ].
	primResult ifNotNil: [ :result | ^result ].
	
	"Make sure that (2.0 raisedTo: Integer) can be safely used without overflow
	For example:
		Float fminNormalized * (2.0 raisedTo: 2000) = Float infinity.
	while:
		(Float fminNormalized timesTwoPower: 2000) = (2.0 raisedTo: 2000+Float emin)."	
	anInteger > Float emax ifTrue: [^(self timesTwoPower: Float emax) timesTwoPower: anInteger - Float emax].
	
	"In case of gradual underflow, timesTwoPower: is not exact, so greatest care must be taken
	because two consecutive timesTwoPower: might differ from a single one"
	anInteger < Float emin
		ifTrue:
			[| deltaToUnderflow |
			deltaToUnderflow := Float emin - self exponent max: Float emin.
			deltaToUnderflow >= 0 ifTrue:
				["self is already near or past underflow, so don't care, result will be zero"
				deltaToUnderflow := Float emin].
			^(self timesTwoPower: deltaToUnderflow) timesTwoPower: anInteger - deltaToUnderflow].
	
	"If (2.0 raisedToInteger: anInteger) fit in a positive SmallInteger, then use faster SmallInteger conversion.
	Note that SmallInteger maxVal highBit = 30 in a 32 bits image, so 1 can be shifted 29 times."
	anInteger > -29 ifTrue: [
		anInteger < 0 ifTrue: [^ self / (1 bitShift: (0 - anInteger)) asFloat].
		anInteger < 30 ifTrue: [^ self * (1 bitShift: anInteger) asFloat]].
	
	^ self * (2.0 raisedToInteger: anInteger)! !


!QAbstractTally methodsFor: 'report' stamp: 'jmv 11/1/2016 12:28:18'!
printOn: textStream linesOn: linesStream talliesOn: talliesStream tabs: tabsAndTreeLines total: total totalTime: totalTime parent: parentTally
	self subclassResponsibility! !

!QAbstractTally methodsFor: 'report' stamp: 'jmv 11/1/2016 12:00:18'!
treeDFSDo: aBlock afterChildrenDo: anotherBlock threshold: threshold parent: parentTally
	"Deep First Search"
	| sons |
	aBlock value: self value: parentTally.
	sons _ self sonsOver: threshold.
	sons isEmpty ifFalse: [
		sons _ self sortCollectionFrom: sons.
		1 to: sons size do: [ :i |
			(sons at: i) treeDFSDo: aBlock afterChildrenDo: anotherBlock threshold: threshold parent: self ]].
	anotherBlock value: self! !


!QSystemTally methodsFor: 'report' stamp: 'jmv 11/1/2016 12:16:06'!
printOn: textStream linesOn: linesStream talliesOn: talliesStreams tabs: tabsAndTreeLines total: total totalTime: totalTime parent: parentTally

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
			nextPutAll: (aClass == class 
					ifTrue: ['>>']
					ifFalse: ['(' , aClass name , ')>>']);
			nextPutAll: aSelector.
		wasInPrimitive ifTrue: [
			self flag: #profilerFriendlyCall:.
			parentTally methodSymbol == #profilerFriendlyCall:
				ifTrue: [
					lineStream nextPutAll: '   -- primitive (reported properly)' ]
				ifFalse: [
					lineStream nextPutAll: '   -- primitive (real sender possibly omitted, see #profilerFriendlyCall:)' ]
			].
		].
	textStream nextPutAll: line; newLine.
	linesStream nextPut: line.
	talliesStreams nextPut: self! !


!QAbstractTally methodsFor: 'report' stamp: 'jmv 11/1/2016 12:00:48'!
rootPrintOn: textStream linesOn: linesStream talliesOn: talliesStream total: total totalTime: totalTime threshold: threshold reportOnly: aProcessOrNil

	| groups dashes line tabsAndTreeLines thisTab |
	groups _ (self sonsOver: threshold)
		groupBy: [ :aTally | aTally process ]
		having: [ :g | aProcessOrNil isNil or: [ g anyOne process == aProcessOrNil ]].
	groups do: [ :g | | sons |
		sons _ self sortCollectionFrom: g.
		dashes _ '--------------------------------'.
		line _ 'Process: ', (g anyOne process browserPrintStringFull: false).
		textStream
			nextPutAll: dashes; newLine;
			nextPutAll: line; newLine;
			nextPutAll: dashes; newLine.
		linesStream
			nextPut: dashes;
			nextPut: line;
			nextPut: dashes.
		talliesStream
			nextPut: nil; nextPut: nil; nextPut: nil.
		sons do: [ :son |
			tabsAndTreeLines _ OrderedCollection new.
			thisTab _ '  '.
			son
				treeDFSDo: [ :eachTally :parentTally |
					eachTally
						printOn: textStream
						linesOn: linesStream talliesOn: talliesStream
						tabs: tabsAndTreeLines total: total totalTime: totalTime parent: parentTally.
					tabsAndTreeLines addLast: thisTab.
					thisTab := (eachTally sonsOver: threshold) size > 1 ifTrue: ['  |'] ifFalse: ['  '].
				]
				afterChildrenDo: [ :eachTally | tabsAndTreeLines removeLast ]
				threshold: threshold
				parent: self.
			].
		textStream newLine ]! !


!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'jmv 11/1/2016 12:15:00'!
runProfilerProcess
	"Run the profiler process"

	| process tallyStart tallyTicks tallyStop methodForPrimitiveWhileTakingSample parentNode contextToTally |
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
								AndreasSystemProfiler is more accurate because it uses VM support to tell it which primitive was running when it took a sample. 
								MessageTally simply ascribes a primitive's cost to the method at the next suspension point, which, in some contexts, 
								can yield wildly misleading results."
							"The problem is that knowing just the primitive and the process doesn't give us the complete call stack.
							So, this is, in a sense, approximate."
							"
								AndreasSystemProfiler spyOn: [
							 	       [ #((1 2 3)) do: [ :each |
 								               each findLast: [ :ea |
								                        ea squared = ea ] ] ] bench ].
							Without asking #sender to the context, for this example
								AndreasSystemProfiler spyOn:[10000 timesRepeat: [3.14159 printString]]
							gave:
								  |  2.9% (7 ms) (Number>>#raisedToInteger:)
								  |    2.2% (5 ms) (Float>>#timesTwoPower: )
							but #raisedToInteger: does NOT send #timesTwoPower:
							Approach taken: Add to parent node, but print with a note that specifies this is primitives, and maybe parent node is missing.
							Additionally, add a note, suggesting #profilerFriendlyCall:
							
							For example
								AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 timesTwoPower: 10000]].
									Here, the real parent node is missing.

								AndreasSystemProfiler spyOn:[1000000 timesRepeat: [3.14159 profilerFriendlyTimesTwoPower: 1000]].
									Here, the proper tree is shown.
							
							See profilerFriendlyCall:
							"
							contextToTally _ process suspendedContext ifNil: [ thisContext ].
							contextToTally method selector == #profilerFriendlyCall: ifFalse: [
								contextToTally _ contextToTally sender ].
							parentNode _ tallyRoot
								tally: contextToTally
								inProcess: process
								by: tallyTicks.
							parentNode
								tallyPrimInMethod: methodForPrimitiveWhileTakingSample by: tallyTicks
							]]]! !

!AndreasSystemProfiler methodsFor: 'profiling' stamp: 'sqr 10/15/2016 21:32:10'!
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
	vmStats := Smalltalk getVMParameters.
	startTime := Time localMillisecondClock.
	profilerProcess := [self runProfilerProcess] newProcess.
	tallyRoot process: nil.
	profilerProcess priority: Processor timingPriority-1.
	profilerProcess name: 'AndreasSystemProfiler'.
	profilerProcess resume! !

!AndreasSystemProfiler methodsFor: 'accessing result' stamp: 'jmv 11/1/2016 12:00:38'!
totalMillisecondsFor: aSelector
	"Answer the sum of the time spent in all appareances in aSelector in the tree"
	| totalTallies |
	totalTallies _ 0.
	tallyRoot
		treeDFSDo: [ :eachTally :parentTally |
			(eachTally methodSymbol == aSelector and: [ eachTally blockNestingCount = 0]) ifTrue: [
				totalTallies _ eachTally tally + totalTallies ]]
		afterChildrenDo: [ :eachTally | ]
		threshold: 0.0
		parent: nil.
	^ totalTallies asFloat / tallyRoot tally * totalTime! !

!methodRemoval: QSystemTally #printOn:linesOn:talliesOn:tabs:total:totalTime:!
QSystemTally removeSelector: #printOn:linesOn:talliesOn:tabs:total:totalTime:!
!methodRemoval: QAbstractTally #printOn:linesOn:talliesOn:tabs:total:totalTime:!
QAbstractTally removeSelector: #printOn:linesOn:talliesOn:tabs:total:totalTime:!
!methodRemoval: QAbstractTally #treeDFSDo:afterChildrenDo:threshold:!
QAbstractTally removeSelector: #treeDFSDo:afterChildrenDo:threshold:!

!Object reorganize!
('Breakpoint' break)
('accessing' addInstanceVarNamed:withValue: at: at:put: basicAt: basicAt:put: basicSize customizeExplorerContents rawBasicAt: rawBasicAt:put: size yourself)
('as yet unclassified' revisar)
('associating' ->)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' class isKindOf: isMemberOf: respondsTo:)
('comparing' = closeTo: hash literalEqual: ~=)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asString complexContents copyAs: mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' copy copyForClipboard copyFrom: copySameFrom: postCopy shallowCopy veryDeepCopy)
('events-old protocol' addDependent: breakDependents changed changed: removeDependent: update:)
('error handling' assert: assert:description: caseError confirm: confirm:orCancel: doesNotUnderstand: error: halt halt: handles: notify:at: primitiveFail primitiveFailed primitiveFailed: shouldBeImplemented shouldNotImplement subclassResponsibility)
('evaluating' value valueWithArguments:)
('events-accessing' actionForEvent: actionMap actionSequenceForEvent: actionsDo: hasActionForEvent: setActionSequence:forEvent: updateableActionMap)
('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)
('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent:)
('events-triggering' triggerEvent: triggerEvent:with: triggerEvent:withArguments:)
('finalization' actAsExecutor executor finalizationRegistry finalize retryWithGC:until: toFinalizeSend:to:with:)
('inspecting' basicInspect inspect inspectWithLabel: inspectorClass)
('macpal' flash)
('message handling' disableCode: executeMethod: perform: perform:with: perform:with:with: perform:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: perform:withPossiblyWrongSizedArguments: with:executeMethod: with:with:executeMethod: with:with:with:executeMethod: with:with:with:with:executeMethod: withArgs:executeMethod:)
('object serialization' comeFullyUpOnReload: convertToCurrentVersion:refStream: objectForDataStream: readDataFrom:size: storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: nominallyUnsent: print printAs:limitedTo: printOn: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: storeOn: storeString)
('system primitives' becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarNamed: instVarNamed:put: isPinned pin primitiveChangeClassTo: setPinned: someObject unpin)
('stepping' stepAt: wantsSteps)
('testing' is: isArray isBehavior isBlock isClosure isCollection isComplex isFloat isFraction isInteger isInterval isMethodProperties isNumber isPoint isPseudoContext isSequenceableCollection isString isSymbol isVariableBinding name renameTo:)
('translation support' inline: success: var:declareC:)
('user interface' browseClassHierarchy explore hasContentsInExplorer inform: notYetImplemented notify:)
('private' errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('morphic' activeHand runningWorld)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('profiler support' profilerFriendlyCall:)
!

