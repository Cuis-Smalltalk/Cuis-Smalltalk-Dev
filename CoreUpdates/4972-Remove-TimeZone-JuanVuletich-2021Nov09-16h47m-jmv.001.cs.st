'From Cuis 5.0 [latest update: #4969] on 9 November 2021 at 4:58:59 pm'!
!classDefinition: #DateAndTime category: #'Kernel-Chronology'!
Magnitude subclass: #DateAndTime
	instanceVariableNames: 'seconds offset jdn nanos '
	classVariableNames: 'LastTick LastTickSemaphore LocalTimeZone '
	poolDictionaries: ''
	category: 'Kernel-Chronology'!

!DateAndTime class methodsFor: 'squeak protocol' stamp: 'jmv 11/9/2021 16:56:54'!
localOffset
	"Answer the duration we are offset from UTC"

	^ self now offset! !

!DateAndTime class methodsFor: 'system startup & shutdown' stamp: 'jmv 11/9/2021 16:57:39'!
releaseClassCachedState

	LastTickSemaphore _ nil.
	LastTick _ nil.! !

!DateAndTime class methodsFor: 'constants' stamp: 'jmv 11/9/2021 16:54:14'!
unixEpoch
	"
	DateAndTime unixEpoch
	1970-01-01T00:00:00+00:00
	
	(DateAndTime now - DateAndTime unixEpoch) days / 365.25
	"
	^ self
		julianDayNumber: 2440588
		seconds: 0
		nanoseconds: 0
		offset: `Duration zero`.! !


!Timespan class methodsFor: 'instance creation' stamp: 'jmv 11/9/2021 16:54:23'!
includingDateAndTime: aDateAndTime

	^ self starting: aDateAndTime duration: `Duration zero`.! !


!BlockClosure methodsFor: 'evaluating' stamp: 'jmv 11/9/2021 16:54:33'!
valueWithin: aDuration onTimeout: timeoutBlock
	"Evaluate the receiver.
	If the evaluation does not complete in less than aDuration evaluate the timeoutBlock instead"

	| theProcess delay watchdog tag |

	aDuration <= `Duration zero` ifTrue: [^ timeoutBlock value ].

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

!methodRemoval: DateAndTime class #localTimeZone: stamp: 'jmv 11/9/2021 16:57:30'!
DateAndTime class removeSelector: #localTimeZone:!
!methodRemoval: DateAndTime class #localTimeZone stamp: 'jmv 11/9/2021 16:57:06'!
DateAndTime class removeSelector: #localTimeZone!
!methodRemoval: DateAndTime #asLocal stamp: 'jmv 11/9/2021 16:51:57'!
DateAndTime removeSelector: #asLocal!
!methodRemoval: DateAndTime #timeZoneAbbreviation stamp: 'jmv 11/9/2021 16:50:42'!
DateAndTime removeSelector: #timeZoneAbbreviation!
!methodRemoval: DateAndTime #timeZoneName stamp: 'jmv 11/9/2021 16:50:44'!
DateAndTime removeSelector: #timeZoneName!
!classDefinition: #DateAndTime category: #'Kernel-Chronology'!
Magnitude subclass: #DateAndTime
	instanceVariableNames: 'seconds offset jdn nanos'
	classVariableNames: 'LastTick LastTickSemaphore'
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
!classRemoval: #TimeZone stamp: 'jmv 11/9/2021 16:58:25'!
Smalltalk removeClassNamed: #TimeZone!

