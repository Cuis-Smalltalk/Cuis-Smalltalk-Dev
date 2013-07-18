'From Cuis 4.1 of 12 December 2012 [latest update: #1512] on 17 December 2012 at 11:25:25 am'!

!Object methodsFor: 'tracing' stamp: 'jmv 12/17/2012 10:17'!
inboundPointersExcluding: objectsToExclude
"Answer a list of all objects in the system that point to me, excluding those in the collection of objectsToExclude. I do my best to avoid creating any temporary objects that point to myself, especially method and block contexts. Adapted from PointerFinder class >> #pointersTo:except:"

	| object lastObject pointers objectsToAlwaysExclude |
	Smalltalk garbageCollect.
	"Do this to get rid of just created MethodContext instance."
	Smalltalk primitiveGarbageCollect.
	lastObject _ Object new.
	"big collection shouldn't grow, so it's contents array is always the same"
	pointers := OrderedCollection new: 1000.
	"#allObjectsDo: and #pointsTo: are expanded inline to keep spurious
	 method and block contexts out of the results"
	object := self someObject.
	[lastObject == object] whileFalse: [
		object isInMemory
			ifTrue: [((object statePointsTo: self)
				or: [object class == self])
					ifTrue: [pointers add: object]].
		object := object nextObject].

	objectsToAlwaysExclude := {
		pointers collector.
		thisContext.
		thisContext sender.
		thisContext sender sender.
		objectsToExclude.
	}.

	^ pointers removeAllSuchThat: [ :ea |
		(objectsToAlwaysExclude identityIncludes: ea)
			or: [ objectsToExclude identityIncludes: ea ]]! !


!ProcessorTest methodsFor: 'as yet unclassified' stamp: 'jmv 12/17/2012 11:15'!
testGrabProcessor
	"
	ProcessorTest new testGrabProcessor
	"
	| done consistentState tries updates observations timeForAThousand iterationsFor50MSecs semaphore waitStart waited totalWait |
	timeForAThousand _ [ self wasteTime: 1000 ] timeToRun.
	iterationsFor50MSecs _ 50 * 1000 // timeForAThousand.
	consistentState _ true.
	tries _ 50.
	updates _ 0.
	done _ false.
	semaphore _ Semaphore new.
	[
		tries timesRepeat: [
			semaphore wait.
			[
				consistentState _ false.
				self wasteTime: iterationsFor50MSecs atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] grabProcessor
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time millisecondClockValue.
		(Delay forMilliseconds: 25) wait.
		waited _ Time millisecondClockValue - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		self assert: consistentState description: 'The low priority process was preempted in inconsistent state!!'.
	].

	self
		assert: updates = tries
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: observations = tries
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	"Min observed value is 26. Mean seems to be around 32. Max observed value was 52"
	self
		assert: (1.0 * totalWait / observations) < 60
		description: 'The low priority process was not preempted at the end of #grabProcessor'! !

!ProcessorTest methodsFor: 'as yet unclassified' stamp: 'jmv 12/17/2012 11:21'!
testGrabProcessorOnlyForNoTimeout
	"
	ProcessorTest new testGrabProcessorOnlyForNoTimeout
	"
	| done consistentState tries updates observations timeForAThousand iterationsFor50MSecs semaphore waitStart waited totalWait |
	timeForAThousand _ [ self wasteTime: 1000 ] timeToRun.
	iterationsFor50MSecs _ 50 * 1000 // timeForAThousand.
	consistentState _ true.
	tries _ 50.
	updates _ 0.
	done _ false.
	semaphore _ Semaphore new.
	[
		tries timesRepeat: [
			semaphore wait.
			[
				consistentState _ false.
				self wasteTime: iterationsFor50MSecs atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] grabProcessorOnlyFor: 1000
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time millisecondClockValue.
		(Delay forMilliseconds: 25) wait.
		waited _ Time millisecondClockValue - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		self assert: consistentState description: 'The low priority process was preempted in inconsistent state!!'.
	].

	self
		assert: (updates = tries)
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: (observations = tries)
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	"Min observed value is 26. Mean seems to be around 32. Max observed value was 52"
	self
		assert: (1.0 * totalWait / observations) <60
		description: 'The low priority process was not preempted right after #grabProcessor'! !

!ProcessorTest methodsFor: 'as yet unclassified' stamp: 'jmv 12/17/2012 11:21'!
testGrabProcessorOnlyForTimeout
	"
	ProcessorTest new testGrabProcessorOnlyForTimeout
	"
	| done consistentState tries updates observations timeForAThousand iterationsFor50MSecs semaphore waitStart waited totalWait consistentCount inconsistentCount |
	timeForAThousand _ [ self wasteTime: 1000 ] timeToRun.
	iterationsFor50MSecs _ 50 * 1000 // timeForAThousand.
	consistentState _ true.
	tries _ 50.
	updates _ 0.
	consistentCount _ 0.
	inconsistentCount _ 0.
	done _ false.
	semaphore _ Semaphore new.
	[
		tries timesRepeat: [
			semaphore wait.
			[
				consistentState _ false.
				self wasteTime: iterationsFor50MSecs atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] grabProcessorOnlyFor: 25
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time millisecondClockValue.
		(Delay forMilliseconds: 15) wait.
		waited _ Time millisecondClockValue - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		consistentState
			ifTrue: [ consistentCount _ consistentCount + 1 ]
			ifFalse: [ inconsistentCount _ inconsistentCount + 1 ].
		"If it needs time to finish, at low priority, allow for it."
		[ consistentState ] whileFalse: [ (Delay forMilliseconds: 2) wait ].
	].

	self
		assert: updates = tries
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: observations = tries
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	self
		assert: (1.0 * totalWait / observations) < 40	"Mean value should be around 25"
		description: 'The low priority process was not preempted after the tiemout'.
	self
		assert: consistentCount > 10
		description: 'It seems the low priority process was preempted before the timeout'.
	self
		assert: inconsistentCount > 10
		description: 'It seems the low priority process was not preempted at the timeout'! !

!ProcessorTest methodsFor: 'as yet unclassified' stamp: 'jmv 12/17/2012 11:22'!
testValueUnpreemptively
	"
	ProcessorTest new testValueUnpreemptively
	"
	| done consistentState tries updates observations timeForAThousand iterationsFor50MSecs semaphore waitStart waited totalWait |
	timeForAThousand _ [ self wasteTime: 1000 ] timeToRun.
	iterationsFor50MSecs _ 50 * 1000 // timeForAThousand.
	consistentState _ true.
	tries _ 50.
	updates _ 0.
	done _ false.
	semaphore _ Semaphore new.
	[
		tries timesRepeat: [
			semaphore wait.
			[
				consistentState _ false.
				self wasteTime: iterationsFor50MSecs atRandom.	"process for some time between 0 and 50 mSecs"
				updates _ updates + 1.
				consistentState _ true
			] valueUnpreemptively
		].
		done _ true
	] forkAt: Processor activeProcess priority -1.

	observations _ 0.
	totalWait _ 0.
	[ done ] whileFalse: [
		semaphore signal.
		waitStart _ Time millisecondClockValue.
		(Delay forMilliseconds: 25) wait.
		waited _ Time millisecondClockValue - waitStart.
		totalWait _ totalWait + waited.
		observations _ observations + 1.
		self assert: consistentState description: 'The low priority process was preempted in inconsistent state!!'.
	].

	self
		assert: updates = tries
		description: 'Too few updates done. It seems as if the low priority process was not allowed to finish processing'.
	self
		assert: observations = tries
		description: 'Too few observations done. It seems as if the low priority process was was never suspended'.
	"Min observed value is 26. Mean seems to be around 32. Max observed value was 52"
	self
		assert: (1.0 * totalWait / observations) < 60
		description: 'The low priority process was not preempted at the end of #grabProcessor'! !


!StringTest methodsFor: 'testing' stamp: 'jmv 12/17/2012 10:51'!
testLineSeparators
	"
	Test that #newLineCharacter is considered a line separator and not a line terminator.
	This means that the last line never ends with a #newLineCharacter (although it might be empty!!)
	StringTest new testLineSeparators
	"
	| justAnLf linesBounds |
	linesBounds _ OrderedCollection new.
	justAnLf _ '
'.
	justAnLf lineIndicesDo: [ :start :endWithoutDelimiters :end |
		linesBounds add: { start . endWithoutDelimiters. end }.
		].

	self assert: linesBounds size = 2 description: 'There should be two lines.'.

	self assert: linesBounds first first = 1 description: 'First line starts at position 1'.
	self assert: linesBounds first second = (linesBounds first first-1) description: 'First line is empty'.
	self assert: linesBounds first third = (linesBounds first second+1) description: 'First line is terminated by ab Lf'.

	self assert: linesBounds second first = ( linesBounds first third+1) description: 'Second line starts after end of first line'.
	self assert: linesBounds second second = (linesBounds second first-1) description: 'Second line is empty'.
	self assert: linesBounds second third = (linesBounds second second+0) description: 'Second line is not terminated by ab Lf'.! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 12/17/2012 10:17'!
pointersToEachIn: anArray
	"Find all occurrences in the system of pointers to elements of the argument"
	| object lastObject pointers subject |
	Smalltalk garbageCollect.
	"Do this to get rid of just created MethodContext instance."
	Smalltalk primitiveGarbageCollect.
	lastObject _ Object new.
	"big collection shouldn't grow, so it's contents array is always the same"
	pointers _ anArray collect: [ :each | OrderedCollection new: 1000 ].
	"#allObjectsDo: and #pointsTo: are expanded inline to keep spurious
	 method and block contexts out of the results"
	object _ self someObject.
	[ lastObject == object ] whileFalse: [
		object isInMemory ifTrue: [
			1 to: anArray size do: [ :i |
				subject _ anArray at: i.
				((object statePointsTo: subject)
					or: [ object class == subject ])
						ifTrue: [ (pointers at: i) add: object ]]].
		object _ object nextObject].

	pointers do: [ :oc |
		oc
			remove: anArray;
			remove: thisContext ifAbsent: nil;
			remove: thisContext sender ifAbsent: nil;
			remove: thisContext sender sender ifAbsent: nil;
			remove: oc collector ifAbsent: nil ].
	^pointers! !


!SystemDictionaryTest methodsFor: 'testing' stamp: 'jmv 12/17/2012 10:44'!
testPointersToEachIn
	"
10 timesRepeat: [SystemDictionaryTest new testPointersToEachIn ]
	"
	| p1 p2 o |
	o _ Browser.
	p1 _ (Smalltalk pointersTo: o).
	p2 _ (Smalltalk pointersToEachIn: {o}) first.
	self assert: p1 = p2.

	o _ Float pi.
	p1 _ (Smalltalk pointersTo: o).
	p2 _ (Smalltalk pointersToEachIn: {o}) first.
	self assert: p1 = p2.

	o _ Processor activeProcess.
	p1 _ (Smalltalk pointersTo: o).
	p2 _ (Smalltalk pointersToEachIn: {o}) first.
	self assert: p1 = p2.

	o _ SystemVersion current.
	p1 _ (Smalltalk pointersTo: o).
	p2 _ (Smalltalk pointersToEachIn: {o}) first.
	self assert: p1 = p2.! !

