'From Cuis 4.2 of 25 July 2013 [latest update: #2107] on 11 November 2014 at 9:37:25.268934 am'!
!classDefinition: #Week category: #'Kernel-Chronology'!
Timespan subclass: #Week
	instanceVariableNames: ''
	classVariableNames: 'StartDay '
	poolDictionaries: ''
	category: 'Kernel-Chronology'!


!Time class methodsFor: 'squeak protocol' stamp: 'jmv 11/10/2014 21:04'!
afterMidnight: aDuration
	"Answer a Time aDuration after midnight.
	If aDuration is longer than a day, take the day fraction."

	| nanoCount secondCount |
	secondCount _ aDuration totalSeconds \\ Time secondsInDay.
	nanoCount _ aDuration nanoSeconds.
	^ self basicNew seconds: secondCount nanoSeconds: nanoCount ! !


!Month class methodsFor: 'squeak protocol' stamp: 'jmv 11/11/2014 09:36'!
including: aDateAndTime
	"Months start at day 1"
	| monthStart days |
	monthStart _ DateAndTime
				year: aDateAndTime yearNumber
				month: aDateAndTime monthIndex
				day: 1.
	days _ self daysInMonth: monthStart monthIndex forYear: monthStart yearNumber.
	^ self basicNew
 		start: monthStart;
		duration: (Duration days: days);
		yourself! !


!Week class methodsFor: 'squeak protocol' stamp: 'jmv 11/11/2014 09:34'!
including: aDateAndTime
	"Week will start from the 
		Week startDay
	"

	| midnight weekStart |
	midnight _ aDateAndTime midnight.
	weekStart _ midnight - (midnight dayOfWeek - 1) days.

	^ self basicNew
 		start: weekStart;
		duration: (Duration weeks: 1);
		yourself! !

!methodRemoval: Week class #startDay!
Week class removeSelector: #startDay!
!methodRemoval: Week class #startDay:!
Week class removeSelector: #startDay:!
!classDefinition: #Week category: #'Kernel-Chronology'!
Timespan subclass: #Week
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Chronology'!
