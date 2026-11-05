'From Cuis7.9 [latest update: #8325] on 5 November 2026 at 5:54:50 pm'!

!Month class methodsFor: 'squeak protocol' stamp: 'jmv 5/Nov/2026, 8:53:22 pm (UTC)'!
month: month year: year
	"Create a Month for the given <year> and <month>.
	<month> may be a number or a String with the
	name of the month. <year> should be with 4 digits."

	^ self includingDateAndTime: (DateAndTime year: year month: month day: 1)
! !


!Week class methodsFor: 'squeak protocol' stamp: 'jmv 5/Nov/2026, 8:53:44 pm (UTC)'!
yearNumber: yearNumber weekNumber: weekNumber

	| firstOfJanuary firstThursday thisThursday |
	firstOfJanuary := DateAndTime year: yearNumber month: 1 day: 1.
	firstThursday := firstOfJanuary + (4 - firstOfJanuary dayOfWeek \\ 7) days.
	thisThursday := firstThursday + ((weekNumber-1) * 7) days.

	thisThursday yearNumber = yearNumber
		ifFalse: [ self error: 'Week does not exist' ].

	^ self includingDateAndTime: thisThursday! !


!DateAndTime methodsFor: 'smalltalk-80' stamp: 'jmv 5/Nov/2026, 8:52:54 pm (UTC)'!
daysInYear

	"Answer the number of days in the year represented by the receiver."

	^ (Year includingDateAndTime: self) daysInYear! !

!DateAndTime methodsFor: 'public protocol' stamp: 'jmv 5/Nov/2026, 8:52:32 pm (UTC)'!
date

	^Date includingDateAndTime: self! !

!DateAndTime methodsFor: 'public protocol' stamp: 'jmv 5/Nov/2026, 8:52:59 pm (UTC)'!
month

	^Month includingDateAndTime: self! !

!DateAndTime methodsFor: 'public protocol' stamp: 'jmv 5/Nov/2026, 8:53:02 pm (UTC)'!
week

	^Week includingDateAndTime: self! !

!DateAndTime methodsFor: 'public protocol' stamp: 'jmv 5/Nov/2026, 8:53:05 pm (UTC)'!
year

	^Year includingDateAndTime: self! !


!DateAndTime class methodsFor: 'squeak protocol' stamp: 'jmv 5/Nov/2026, 8:49:55 pm (UTC)'!
year: year month: month day: day
	"Return a DateAndTime, midnight local time"
	^ self
		year: year
		month: month
		day: day
		hour: 0
		minute: 0! !

!DateAndTime class methodsFor: 'squeak protocol' stamp: 'jmv 5/Nov/2026, 8:49:43 pm (UTC)'!
year: year month: month day: day hour: hour minute: minute
	"Return a DateAndTime" 
	^ self
		year: year
		month: month
		day: day
		hour: hour
		minute: minute
		second: 0! !

