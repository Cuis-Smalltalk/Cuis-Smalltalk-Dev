'From Cuis 5.0 of 7 November 2016 [latest update: #3076] on 13 May 2017 at 1:02:55 pm'!

!DateAndTime methodsFor: 'double dispatching' stamp: 'jmv 5/12/2017 17:17:21'!
includingTimespanOf: aTimespanClass

	^ aTimespanClass includingDateAndTime: self! !


!Timespan methodsFor: 'double displatching' stamp: 'jmv 5/12/2017 17:17:18'!
includingTimespanOf: aTimespanClass

	^ aTimespanClass includingTimespan: self! !


!Timespan class methodsFor: 'instance creation' stamp: 'jmv 5/12/2017 16:48:47'!
includingDateAndTime: aDateAndTime

	^ self starting: aDateAndTime duration: Duration zero! !

!Timespan class methodsFor: 'instance creation' stamp: 'jmv 5/12/2017 17:22:59'!
includingTimespan: aDateAndTime

	| ending starting |
	starting _ self includingDateAndTime: aDateAndTime start.
	ending _ self includingDateAndTime: aDateAndTime end.
	starting = ending ifTrue: [ ^ starting ].
	self error: aDateAndTime printString, ' can not be included in a ', self name! !


!Date class methodsFor: 'instance creation' stamp: 'jmv 5/12/2017 16:48:52'!
includingDateAndTime: aDateAndTime

	^self basicNew
 		start: aDateAndTime midnight;
		duration: (Duration days: 1);
		yourself! !


!Month class methodsFor: 'squeak protocol' stamp: 'jmv 5/12/2017 16:48:55'!
includingDateAndTime: aDateAndTime
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


!Week class methodsFor: 'squeak protocol' stamp: 'jmv 5/12/2017 18:05:57'!
includingDateAndTime: aDateAndTime
	"
	Week including: '12 May 2017 ' asDate start
	(Week including: '12 May 2017 ' asDate start) start dayOfWeekName = #Monday 
	"

	| midnight weekStart |
	midnight _ aDateAndTime midnight.
	weekStart _ midnight - (midnight dayOfWeek - 1) days.

	^ self basicNew
 		start: weekStart;
		duration: (Duration weeks: 1);
		yourself! !


!Year class methodsFor: 'squeak protocol' stamp: 'jmv 5/12/2017 18:00:49'!
includingDateAndTime: aDateAndTime
	"Answer a calendar year"

	^ self yearNumber: aDateAndTime yearNumber! !


!Timespan class methodsFor: 'instance creation' stamp: 'jmv 5/12/2017 17:18:23'!
including: aDateAndTime

	^ aDateAndTime includingTimespanOf: self! !


!Year class methodsFor: 'squeak protocol' stamp: 'jmv 5/12/2017 17:43:17'!
yearNumber: aYear

	| yearStart |
	yearStart _ DateAndTime year: aYear month: 1 day: 1.
	^ self basicNew
 		start: yearStart;
		duration: (Duration days: (self daysInYear: yearStart yearNumber));
		yourself! !

!methodRemoval: Year class #including:!
Year class removeSelector: #including:!
!methodRemoval: Week class #including:!
Week class removeSelector: #including:!
!methodRemoval: Month class #including:!
Month class removeSelector: #including:!
!methodRemoval: Date class #including:!
Date class removeSelector: #including:!
