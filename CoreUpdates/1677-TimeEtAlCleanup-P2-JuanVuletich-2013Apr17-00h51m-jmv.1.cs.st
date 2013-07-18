'From Cuis 4.1 of 12 December 2012 [latest update: #1676] on 17 April 2013 at 12:53:42 am'!
!classDefinition: #Date category: #'Kernel-Chronology'!
Timespan subclass: #Date
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
!classDefinition: #DateAndTime category: #'Kernel-Chronology'!
Magnitude subclass: #DateAndTime
	instanceVariableNames: 'seconds offset jdn nanos '
	classVariableNames: 'DaysSinceEpoch LastMilliSeconds LastTick LastTickSemaphore LocalTimeZone MilliSecondOffset OffsetsAreValid '
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
!classDefinition: #Duration category: #'Kernel-Chronology'!
Magnitude subclass: #Duration
	instanceVariableNames: 'nanos seconds '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
!classDefinition: #Month category: #'Kernel-Chronology'!
Timespan subclass: #Month
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
!classDefinition: #Time category: #'Kernel-Chronology'!
Magnitude subclass: #Time
	instanceVariableNames: 'seconds nanos '
	classVariableNames: 'MillisecondClockMask '
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
!classDefinition: #TimeZone category: #'Kernel-Chronology'!
Object subclass: #TimeZone
	instanceVariableNames: 'offset abbreviation name '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
!classDefinition: #Week category: #'Kernel-Chronology'!
Timespan subclass: #Week
	instanceVariableNames: ''
	classVariableNames: 'StartDay '
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
!classDefinition: #Week category: #'Kernel-Chronology'!
Timespan subclass: #Week
	instanceVariableNames: ''
	classVariableNames: 'StartDay'
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
!classDefinition: #TimeZone category: #'Kernel-Chronology'!
Object subclass: #TimeZone
	instanceVariableNames: 'offset abbreviation name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
!classDefinition: #Time category: #'Kernel-Chronology'!
Magnitude subclass: #Time
	instanceVariableNames: 'seconds nanos'
	classVariableNames: 'MillisecondClockMask'
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
!methodRemoval: SystemDictionary #highResClock!
SystemDictionary removeSelector: #highResClock!
!classDefinition: #Duration category: #'Kernel-Chronology'!
Magnitude subclass: #Duration
	instanceVariableNames: 'nanos seconds'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
!classDefinition: #DateAndTime category: #'Kernel-Chronology'!
Magnitude subclass: #DateAndTime
	instanceVariableNames: 'seconds offset jdn nanos'
	classVariableNames: 'DaysSinceEpoch LastMilliSeconds LastTick LastTickSemaphore LocalTimeZone MilliSecondOffset OffsetsAreValid'
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
!classRemoval: #ChronologyConstants!
Smalltalk removeClassNamed: #ChronologyConstants!
