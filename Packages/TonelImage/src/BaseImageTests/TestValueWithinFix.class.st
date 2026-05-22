Class {
	#name : 'TestValueWithinFix',
	#superclass : 'TestCase',
	#category : 'BaseImageTests-Fixed Bugs',
	#package : 'BaseImageTests',
	#tag : 'Fixed Bugs'
}

{ #category : 'tests' }
TestValueWithinFix >> testValueWithinNonLocalReturnFixReal [
	"self run: #testValueWithinNonLocalReturnFixReal"
	"The real test for the fix is just as obscure as the original problem"
	| startTime |
	self valueWithinNonLocalReturn.
	startTime := Time localMillisecondClock.
	[[] repeat] valueWithin: 100 milliSeconds onTimeout:[ | deltaTime |
		"This *should* timeout after 100 msecs but the pending process from
		the previous invokation will signal timeout after 20 msecs already
		which will in turn cut this invokation short."
		deltaTime := Time localMillisecondClock - startTime.
		self deny: deltaTime < 90 description: 'deltaTime was: ', deltaTime printString.
	].

]

{ #category : 'tests' }
TestValueWithinFix >> testValueWithinNonLocalReturnFixSimply [
	"self run: #testValueWithinNonLocalReturnFixSimply"
	"The simple version to test the fix"
	self valueWithinNonLocalReturn.
	self shouldnt:[(Delay forMilliseconds: 50) wait] raise: TimedOut.
]

{ #category : 'tests' }
TestValueWithinFix >> testValueWithinTimingBasic [
	"Test timing of valueWithin:onTimeout:"
	| time |
	[time := [
		[1000 milliSeconds asDelay wait]
			valueWithin: 100 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time < 200 milliSeconds.
	] forkAt: Processor userInterruptPriority.
]

{ #category : 'tests' }
TestValueWithinFix >> testValueWithinTimingNestedInner [
	"Test nested timing of valueWithin:onTimeout:"
	| time |
	[time := [
		[
			[5 seconds asDelay wait]
				valueWithin: 100 milliSeconds onTimeout: []
		] valueWithin: 500 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time < 200 milliSeconds.
	] forkAt: Processor userInterruptPriority.
]

{ #category : 'tests' }
TestValueWithinFix >> testValueWithinTimingNestedOuter [
	"Test nested timing of valueWithin:onTimeout:"
	| time |
	[time := [
		[
			3 timesRepeat: [
				[5 seconds asDelay wait]
					valueWithin: 100 milliSeconds onTimeout: []]
		] valueWithin: 150 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time > 100 milliSeconds.
	self assert: time < 200 milliSeconds.
	] forkAt: Processor userInterruptPriority.
	
]

{ #category : 'tests' }
TestValueWithinFix >> testValueWithinTimingRepeat [
	"Test timing of valueWithin:onTimeout:"
	| time |
	[time := [
		3 timesRepeat: [
			[500 milliSeconds asDelay wait]
				valueWithin: 100 milliSeconds onTimeout: []]
	] durationToRun.
	self assert: time < 400 milliSeconds.
	] forkAt: Processor userInterruptPriority.

]

{ #category : 'tests' }
TestValueWithinFix >> valueWithinNonLocalReturn [
	"Do a non-local return from a valueWithin: block"
	[^self] valueWithin: 20 milliSeconds onTimeout:[].

]
