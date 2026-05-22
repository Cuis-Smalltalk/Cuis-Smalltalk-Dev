Class {
	#name : 'DelayTest',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Kernel-Processes',
	#package : 'BaseImageTests',
	#tag : 'Kernel-Processes'
}

{ #category : 'testing-limits' }
DelayTest >> testBounds [
	"self run: #testBounds"
	
	self should: [Delay forMilliseconds: -1] raise: Error.
	self shouldnt: [Delay forMilliseconds: SmallInteger maxVal + 1] raise: Error.
	self shouldnt: [(Delay forMilliseconds: Float pi) wait] raise: Error. "Wait 3ms"

]

{ #category : 'testing-limits' }
DelayTest >> testMultiProcessWaitOnSameDelay [
	"Ensure that waiting on the same delay from multiple processes raises an error"
	"
	self new testMultiProcessWaitOnSameDelay
	"
	| delay p1 p2 wasRun |
	delay := Delay forSeconds: 1.
	wasRun := false.
	p1 := [delay wait] newProcess.
	p1 priority: Processor activePriority+1.
	p1 name: 'testMultiProcessWaitOnSameDelay-1'.
	p1 resume.
	p2 := [
		self should:[delay wait] raise: Error.
		wasRun := true.
	] newProcess.
	p2 priority:  Processor activePriority+1.
	p2 name: 'testMultiProcessWaitOnSameDelay-2'.
	p2 resume.
	p1 terminate.
	p2 terminate.
	self assert: wasRun.


]

{ #category : 'testing-limits' }
DelayTest >> testMultiSchedule [
	"Ensure that scheduling the same delay twice raises an error"
	| delay |
	delay := Delay forSeconds: 1.
	delay schedule.
	self should:[delay schedule] raise: Error.

]
