"
I am the main way that a process may pause for some amount of time.  The simplest usage is like this:

	(Delay forSeconds: 5) wait.

An instance of Delay responds to the message 'wait' by suspending the caller's process for a certain amount of time. The duration of the pause is specified when the Delay is created with the message forMilliseconds: or forSeconds:. A Delay can be used again when the current wait has finished. For example, a clock process might repeatedly wait on a one-second Delay.

A delay in progress when an image snapshot is saved is resumed when the snapshot is re-started. Delays work across millisecond clock roll-overs.


For a more complex example, see  #testDelayOf:for:rect: .

A word of advice:
This is THE highest priority code which is run in Squeak, in other words it is time-critical. The speed of this code is critical for accurate responses, it is critical for network services, it affects every last part of the system.

In short: Don't fix it if it ain't broken! This code isn't supposed to be beautiful, it's supposed to be fast! The reason for duplicating code is to make it fast. The reason for not using ifNil:[]ifNotNil:[] is that the compiler may not inline those. Since the effect of changes are VERY hard to predict it is best to leave things as they are for now unless there is an actual need to change anything
"
Class {
	#name : 'Delay',
	#superclass : 'Object',
	#instVars : [
		'delayDuration',
		'resumptionTime',
		'delaySemaphore',
		'beingWaitedOn'
	],
	#classVars : [
		'ActiveDelay',
		'ActiveDelayStartTime',
		'FinishedDelay',
		'FinishedDelayNilledSemaphore',
		'RunTimerEventLoop',
		'ScheduledDelay',
		'ScheduledDelayNilledSemaphore',
		'SuspendedDelays',
		'TimerEventLoop',
		'TimingSemaphore'
	],
	#category : 'Kernel-Processes',
	#package : 'Kernel',
	#tag : 'Processes'
}

{ #category : 'testing' }
Delay class >> anyActive [
	"Return true if there is any delay currently active"
	^ActiveDelay notNil
]

{ #category : 'instance creation' }
Delay class >> forDuration: aDuration [

	^ self forMilliseconds: aDuration totalMilliseconds
]

{ #category : 'instance creation' }
Delay class >> forMilliseconds: anInteger [
	"Return a new Delay for the given number of milliseconds. Sending 'wait' to this Delay will cause the sender's process to be suspended for approximately that length of time."

	anInteger < 0 ifTrue: [self error: 'delay times cannot be negative'].
	^ self new
		setDelay: anInteger asInteger
		forSemaphore: Semaphore new

]

{ #category : 'instance creation' }
Delay class >> forSeconds: aNumber [
	"Return a new Delay for the given number of seconds. Sending 'wait' to this Delay will cause the sender's process to be suspended for approximately that length of time."

	aNumber < 0 ifTrue: [self error: 'delay times cannot be negative'].
	^ self new
		setDelay: (aNumber * 1000) asInteger
		forSemaphore: Semaphore new

]

{ #category : 'timer process' }
Delay class >> handleTimerEvent [
	"Handle a timer event; which can be either:
		- a schedule request (ScheduledDelay notNil)
		- an unschedule request (FinishedDelay notNil)
		- a timer signal (not explicitly specified)
	We check for timer expiry every time we get a signal."
	| nowTick nextTick afterwardsTick |
	"Wait until there is work to do."
	TimingSemaphore wait.

	"Process any schedule requests"
	ScheduledDelay ifNotNil: [
		"Schedule the given delay"
		self scheduleDelay: ScheduledDelay.
		ScheduledDelay := nil.
		ScheduledDelayNilledSemaphore initSignals. 
		ScheduledDelayNilledSemaphore signal ].

	"Process any unschedule requests"
	FinishedDelay ifNotNil: [
		self unscheduleDelay: FinishedDelay.
		FinishedDelay := nil.
		FinishedDelayNilledSemaphore initSignals.
		FinishedDelayNilledSemaphore signal ].

	"Check for clock wrap-around."
	nowTick := Time millisecondClockValue.
	nowTick < ActiveDelayStartTime ifTrue: [
		"clock wrapped"
		self saveResumptionTimes.
		self restoreResumptionTimes ].
	ActiveDelayStartTime := nowTick.

	"Signal any expired delays"
	[ ActiveDelay notNil and:[ nowTick >= ActiveDelay resumptionTime ]] whileTrue: [
		ActiveDelay signalWaitingProcess.
		SuspendedDelays isEmpty 
			ifTrue: [ ActiveDelay := nil ] 
			ifFalse:[ ActiveDelay := SuspendedDelays removeFirst ]].

	"And signal when the next request is due. We sleep at most 1sec here
	as a soft busy-loop so that we don't accidentally miss signals."
	nextTick := nowTick + 1000.
	ActiveDelay ifNotNil: [
		nextTick := nextTick min: ActiveDelay resumptionTime ].
	nextTick := nextTick min: 16r1FFFFFFF.	"MillisecondClockMask"

	"Since we have processed all outstanding requests, reset the timing semaphore so
	that only new work will wake us up again. Do this RIGHT BEFORE setting the next
	wakeup call from the VM because it is only signaled once so we mustn't miss it."
	TimingSemaphore initSignals.
	Delay primSignal: TimingSemaphore atMilliseconds: nextTick.

	"This test is necessary for the obscure case that the msecs clock rolls over
	after nowTick has been computed (unlikely but not impossible). In this case we'd
	wait for MillisecondClockMask msecs (roughly six days) or until another delay gets
	scheduled (which may not be any time soon). In any case, since handling the
	condition is easy, let's just deal with it."
	afterwardsTick := Time millisecondClockValue.
	afterwardsTick < nowTick  						"Means clock rollover."
		ifTrue: [ TimingSemaphore signal ]. "retry"

	"It seems that #primSignal:atMilliseconds: might not signal the semaphore if the requested moment is in the past.
	This behavior was observed in Linux Spur64 VMs.
	If this happens, next delay never finishes. Usual symptom is frozen Morphic.
	If there is any risk of this happening, just do another iteration right now."
	nextTick < afterwardsTick  						"We're already late for next signal."
		ifTrue: [ TimingSemaphore signal ]. "retry"
]

{ #category : 'class initialization' }
Delay class >> initialize [
	"Delay initialize"
	self startTimerEventLoop.
]

{ #category : 'primitives' }
Delay class >> primSignal: aSemaphore atMilliseconds: aSmallInteger [
	"Signal the semaphore when the millisecond clock reaches the value of the second argument. Fail if the first argument is neither a Semaphore nor nil. Essential. See Object documentation whatIsAPrimitive."
	<primitive: 136>
	^self primitiveFailed
]

{ #category : 'primitives' }
Delay class >> primSignal: aSemaphore atUTCMicroseconds: anInteger [
	"Signal the semaphore when the UTC microsecond clock reaches the value of the second argument.
	 Fail if the first argument is neither a Semaphore nor nil.
	Fail if the second argument is not an integer (either SmallInteger or LargePositiveInteger).
	See #primUtcMicrosecondClock
	 Essential. See Object documentation whatIsAPrimitive."
	<primitive: 242>
	^self primitiveFailed
]

{ #category : 'snapshotting' }
Delay class >> restoreResumptionTimes [
	"Private! Restore the resumption times of all scheduled Delays after a snapshot or clock roll-over. This method should be called only while the AccessProtect semaphore is held."

	| newBaseTime |
	newBaseTime := Time millisecondClockValue.
	SuspendedDelays
		do: [ :d | d adjustResumptionTimeOldBase: 0 newBase: newBaseTime].
	ActiveDelay
		ifNotNil: [ActiveDelay adjustResumptionTimeOldBase: 0 newBase: newBaseTime].
	ActiveDelayStartTime := newBaseTime
]

{ #category : 'timer process' }
Delay class >> runTimerEventLoop [
	"Run the timer event loop."
	[RunTimerEventLoop] whileTrue: [self handleTimerEvent]
]

{ #category : 'snapshotting' }
Delay class >> saveResumptionTimes [
	"Private! Record the resumption times of all Delays relative to a base time of zero. This is done prior to snapshotting or adjusting the resumption times after a clock roll-over. This method should be called only while the AccessProtect semaphore is held."

	| oldBaseTime |
	oldBaseTime := Time millisecondClockValue.
	ActiveDelay
		ifNotNil: [
			oldBaseTime < ActiveDelayStartTime
				ifTrue: [oldBaseTime := ActiveDelayStartTime].  "clock rolled over"
			ActiveDelay adjustResumptionTimeOldBase: oldBaseTime newBase: 0].
	SuspendedDelays do:
		[:d | d adjustResumptionTimeOldBase: oldBaseTime newBase: 0].

]

{ #category : 'timer process' }
Delay class >> scheduleDelay: aDelay [
	"Private. Schedule this Delay."
	
	aDelay resumptionTime: Time millisecondClockValue + aDelay delayDuration. 
		"Gary Chambers:  do the above here, via the high priority timer process to avoid rollover bug due to process pre-emption
		since the caller cannot use the AccessProtect semaphore."
	aDelay beingWaitedOn: true.
	ActiveDelay 
		ifNil: [ActiveDelay := aDelay]
		ifNotNil: [aDelay resumptionTime < ActiveDelay resumptionTime
			ifTrue: [SuspendedDelays add: ActiveDelay.
					ActiveDelay := aDelay]
			ifFalse: [SuspendedDelays add: aDelay]]

]

{ #category : 'snapshotting' }
Delay class >> shutDown [
	"Suspend the active delay, if any, before snapshotting. It will be reactived when the snapshot is resumed."
	"Details: This prevents a timer interrupt from waking up the active delay in the midst snapshoting, since the active delay will be restarted when resuming the snapshot and we don't want to process the delay twice."

	self stopTimerEventLoop.
	self saveResumptionTimes
]

{ #category : 'timer process' }
Delay class >> startTimerEventLoop [
	"Start the timer event loop"
	"Delay startTimerEventLoop"
	self stopTimerEventLoop.
	ActiveDelayStartTime := Time millisecondClockValue.
	SuspendedDelays := 
		Heap withAll: (SuspendedDelays ifNil:[#()])
			sortBlock: [:d1 :d2 | d1 resumptionTime <= d2 resumptionTime].
	TimingSemaphore := Semaphore new.
	ScheduledDelayNilledSemaphore := Semaphore new.
	FinishedDelayNilledSemaphore := Semaphore new.
	RunTimerEventLoop := true.
	TimerEventLoop := [self runTimerEventLoop] newProcess.
	TimerEventLoop
		priority: Processor timingPriority;
		name: '[system] Delay Scheduling';
		resume.
	TimingSemaphore signal. "get going"

]

{ #category : 'snapshotting' }
Delay class >> startUp [
	"Restart active delay, if any, when resuming a snapshot."

	self restoreResumptionTimes.
	self startTimerEventLoop.
]

{ #category : 'timer process' }
Delay class >> stopTimerEventLoop [
	"Stop the timer event loop"
	RunTimerEventLoop := false.
	TimingSemaphore signal.
	TimerEventLoop := nil.
]

{ #category : 'example' }
Delay class >> testDelayOf: delay for: testCount rect: r [
	"Delay testDelayOf: 100 for: 20 rect: (10@10 extent: 30@30).
	 Delay testDelayOf: 400 for: 20 rect: (50@10 extent: 30@30)."

	| onDelay offDelay |
	onDelay := Delay forMilliseconds: 50.
	offDelay := Delay forMilliseconds: delay - 50.
	Display fillBlack: r.
	[1 to: testCount do: [:i |
		Display fillWhite: r.
		onDelay wait.
		Display reverse: r.
		offDelay wait].
	] forkAt: Processor userInterruptPriority.

]

{ #category : 'instance creation' }
Delay class >> timeoutSemaphore: aSemaphore afterMSecs: anInteger [
	"Create and schedule a Delay to signal the given semaphore when the given number of milliseconds has elapsed. Return the scheduled Delay. The timeout can be cancelled by sending 'unschedule' to this Delay."
	"Details: This mechanism is used to provide a timeout when waiting for an external event, such as arrival of data over a network connection, to signal a semaphore. The timeout ensures that the semaphore will be signalled within a reasonable period of time even if the event fails to occur. Typically, the waiting process cancels the timeout request when awoken, then determines if the awaited event has actually occurred."

	anInteger < 0 ifTrue: [self error: 'delay times cannot be negative'].
	^ (self new setDelay: anInteger forSemaphore: aSemaphore) schedule

]

{ #category : 'timer process' }
Delay class >> timerProcess [
	^TimerEventLoop
]

{ #category : 'timer process' }
Delay class >> unscheduleDelay: aDelay [
	"Private. Unschedule this Delay."
	aDelay beingWaitedOn ifFalse:[^self].
	ActiveDelay == aDelay ifTrue: [
		SuspendedDelays isEmpty ifTrue:[
			ActiveDelay := nil.
		] ifFalse: [
			ActiveDelay := SuspendedDelays removeFirst.
		]
	] ifFalse:[
		SuspendedDelays remove: aDelay ifAbsent: nil.
	].
	aDelay beingWaitedOn: false.
]

{ #category : 'private' }
Delay >> adjustResumptionTimeOldBase: oldBaseTime newBase: newBaseTime [
	"Private! Adjust the value of the system's millisecond clock at which this Delay will be awoken. Used to adjust resumption times after a snapshot or clock roll-over."

	resumptionTime := newBaseTime + (resumptionTime - oldBaseTime).

]

{ #category : 'public' }
Delay >> beingWaitedOn [
	"Answer whether this delay is currently scheduled, e.g., being waited on"
	^beingWaitedOn
]

{ #category : 'public' }
Delay >> beingWaitedOn: aBool [
	"Indicate whether this delay is currently scheduled, e.g., being waited on"
	beingWaitedOn := aBool
]

{ #category : 'public' }
Delay >> delayDuration [
	^delayDuration
]

{ #category : 'public' }
Delay >> delaySemaphore [

	^ delaySemaphore
]

{ #category : 'delaying' }
Delay >> isExpired [

	^delaySemaphore isSignaled.

]

{ #category : 'printing' }
Delay >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: '('; print: delayDuration; nextPutAll: ' msecs'.
	beingWaitedOn ifTrue:[
		aStream nextPutAll: '; '; print: resumptionTime - Time millisecondClockValue; nextPutAll: ' msecs remaining'.
	].
	aStream nextPutAll: ')'.
]

{ #category : 'accessing' }
Delay >> resumptionTime [
	"Answer the value of the system's millisecondClock at which the receiver's 
	suspended Process will resume."

	^resumptionTime
]

{ #category : 'private' }
Delay >> resumptionTime: anInteger [
	"Private! Set the value of the system's millisecondClock at which the receiver's suspended Process will resumed.
	Must only be called from the class-side #scheduleDelay:."
	
	resumptionTime := anInteger
]

{ #category : 'private' }
Delay >> schedule [
	"Schedule this delay."

	beingWaitedOn ifTrue: [ ^ self error: 'This Delay has already been scheduled.' ].
	"Assuming comparison, #ifTrue:ifFalse and assignment all inlined bytecodes that can't be interrupted."
	ScheduledDelay == nil
		ifTrue: [
			"No other process will overwrite "
			ScheduledDelay := self ]
		ifFalse: [
			"Assuming when semaphore is signalled to continue this process, it cannot be interrupted again before assignment"
			ScheduledDelayNilledSemaphore wait.
			ScheduledDelay := self ].
	"Signal semaphore in #handleTimerEvent (highest priority process) to action ScheduledDelay,
        set ScheduledDelay to nil and signal ScheduledDelayNilledSemaphore"
	TimingSemaphore signal

]

{ #category : 'private' }
Delay >> setDelay: millisecondCount  [
	"Private! Initialize this delay to signal the given semaphore after the given number of milliseconds."

	delayDuration := millisecondCount asInteger
]

{ #category : 'private' }
Delay >> setDelay: millisecondCount forSemaphore: aSemaphore [
	"Private! Initialize this delay to signal the given semaphore after the given number of milliseconds."

	delayDuration := millisecondCount asInteger.
	delaySemaphore := aSemaphore.
	beingWaitedOn := false.

]

{ #category : 'private' }
Delay >> signalWaitingProcess [
	"The delay time has elapsed; signal the waiting process."

	beingWaitedOn := false.
	delaySemaphore signal.

]

{ #category : 'private' }
Delay >> unschedule [

	"Assuming comparison, #ifTrue:ifFalse and assignment all inlined bytecodes that can't be interrupted."
	FinishedDelay == nil
		ifTrue: [ FinishedDelay := self ]
		ifFalse: [
			"Assuming when semaphore is signalled to continue this process, it cannot be interrupted again before assignment"
			FinishedDelayNilledSemaphore wait.
			FinishedDelay := self ].
	"Signal semaphore in #handleTimerEvent (highest priority process) to action FinishedDelay,
        set FinishedDelay to nil and signal FinishedDelayNilledSemaphore"
	TimingSemaphore signal

]

{ #category : 'delaying' }
Delay >> wait [
	"Schedule this Delay, then wait on its semaphore. The current process will be suspended for the amount of time specified when this Delay was created."

	self schedule.
	[delaySemaphore wait] ifCurtailed:[self unschedule].

]
