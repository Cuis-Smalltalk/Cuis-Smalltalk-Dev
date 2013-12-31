'From Cuis 4.0 of 21 April 2012 [latest update: #1494] on 4 December 2012 at 10:06:51 pm'!
!classDefinition: #TestValueWithinFix category: #'Tests-Bugs'!
TestCase subclass: #TestValueWithinFix
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tests-Bugs'!

!TestValueWithinFix methodsFor: 'tests' stamp: 'nice 12/27/2009 03:11'!
testValueWithinNonLocalReturnFixReal
	"self run: #testValueWithinNonLocalReturnFixReal"
	"The real test for the fix is just as obscure as the original problem"
	| startTime |
	self valueWithinNonLocalReturn.
	startTime := Time millisecondClockValue.
	[[] repeat] valueWithin: 100 milliSeconds onTimeout:[ | deltaTime |
		"This *should* timeout after 100 msecs but the pending process from
		the previous invokation will signal timeout after 20 msecs already
		which will in turn cut this invokation short."
		deltaTime := Time millisecondClockValue - startTime.
		self deny: deltaTime < 90.
	].
! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'ar 8/17/2007 13:38'!
testValueWithinNonLocalReturnFixSimply
	"self run: #testValueWithinNonLocalReturnFixSimply"
	"The simple version to test the fix"
	self valueWithinNonLocalReturn.
	self shouldnt:[(Delay forMilliseconds: 50) wait] raise: TimedOut.! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'ar 12/4/2012 20:35'!
testValueWithinTimingBasic
	"Test timing of valueWithin:onTimeout:"
	| time |
	time := [
		[1000 milliSeconds asDelay wait]
			valueWithin: 100 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time < 150 milliSeconds.! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'ar 12/4/2012 20:35'!
testValueWithinTimingNestedInner
	"Test nested timing of valueWithin:onTimeout:"
	| time |
	time := [
		[
			[5 seconds asDelay wait]
				valueWithin: 100 milliSeconds onTimeout: []
		] valueWithin: 500 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time < 150 milliSeconds.
! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'ar 12/4/2012 20:35'!
testValueWithinTimingNestedOuter
	"Test nested timing of valueWithin:onTimeout:"
	| time |
	time := [
		[
			3 timesRepeat: [
				[5 seconds asDelay wait]
					valueWithin: 100 milliSeconds onTimeout: []]
		] valueWithin: 150 milliSeconds onTimeout: []
	] durationToRun.
	self assert: time > 100 milliSeconds.
	self assert: time < 200 milliSeconds.
	! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'ar 12/4/2012 20:35'!
testValueWithinTimingRepeat
	"Test timing of valueWithin:onTimeout:"
	| time |
	time := [
		3 timesRepeat: [
			[500 milliSeconds asDelay wait]
				valueWithin: 100 milliSeconds onTimeout: []]
	] durationToRun.
	self assert: time < 350 milliSeconds.
! !

!TestValueWithinFix methodsFor: 'tests' stamp: 'ar 8/17/2007 13:37'!
valueWithinNonLocalReturn
	"Do a non-local return from a valueWithin: block"
	[^self] valueWithin: 20 milliSeconds onTimeout:[].
! !


!BlockClosure methodsFor: 'evaluating' stamp: 'ar 12/4/2012 20:36'!
valueWithin: aDuration onTimeout: timeoutBlock
	"Evaluate the receiver.
	If the evaluation does not complete in less than aDuration evaluate the timeoutBlock instead"

	| theProcess delay watchdog tag |

	aDuration <= Duration zero ifTrue: [^ timeoutBlock value ].

	"the block will be executed in the current process"
	theProcess := Processor activeProcess.
	delay := aDuration asDelay.
	tag := self.

	"make a watchdog process"
	watchdog := [
		delay wait. 	"wait for timeout or completion"
		theProcess ifNotNil:[ theProcess signalException: (TimedOut new tag: tag)] 
	] newProcess.

	"Watchdog needs to run at high priority to do its job (but not at timing priority)"
	watchdog priority: Processor timingPriority-1.

	"catch the timeout signal"
	^ [	watchdog resume.				"start up the watchdog"
		self ensure:[						"evaluate the receiver"
			theProcess := nil.				"it has completed, so ..."
			delay delaySemaphore signal.	"arrange for the watchdog to exit"
		]] on: TimedOut do: [ :e | 
			e tag == tag 
				ifTrue:[ timeoutBlock value ]
				ifFalse:[ e pass]].! !


!BlockContext methodsFor: 'evaluating' stamp: 'ar 12/4/2012 20:37'!
valueWithin: aDuration onTimeout: timeoutBlock
	"Evaluate the receiver.
	If the evaluation does not complete in less than aDuration evaluate the timeoutBlock instead"

	| theProcess delay watchdog tag |

	aDuration <= Duration zero ifTrue: [^ timeoutBlock value ].

	"the block will be executed in the current process"
	theProcess := Processor activeProcess.
	delay := aDuration asDelay.
	tag := self.

	"make a watchdog process"
	watchdog := [
		delay wait. 	"wait for timeout or completion"
		theProcess ifNotNil:[ theProcess signalException: (TimedOut new tag: tag)] 
	] newProcess.

	"Watchdog needs to run at high priority to do its job (but not at timing priority)"
	watchdog priority: Processor timingPriority-1.

	"catch the timeout signal"
	^ [	watchdog resume.				"start up the watchdog"
		self ensure:[						"evaluate the receiver"
			theProcess := nil.				"it has completed, so ..."
			delay delaySemaphore signal.	"arrange for the watchdog to exit"
		]] on: TimedOut do: [ :e | 
			e tag == tag 
				ifTrue:[ timeoutBlock value ]
				ifFalse:[ e pass]].! !
