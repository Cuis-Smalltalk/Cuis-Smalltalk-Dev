'From Cuis 5.0 of 7 November 2016 [latest update: #3183] on 18 September 2017 at 6:29:35 pm'!

!Timespan methodsFor: 'private' stamp: 'jmv 9/18/2017 18:27:11'!
subtractMonth: aYear

	^ self subtractTimespan: aYear! !

!Timespan methodsFor: 'private' stamp: 'jmv 9/18/2017 17:22:56'!
subtractYear: aYear

	^ self subtractTimespan: aYear! !


!Month methodsFor: 'double dispatching' stamp: 'jmv 9/18/2017 18:26:57'!
subtractFrom: aTimespan

	^ aTimespan subtractMonth: self! !

!Month methodsFor: 'double dispatching' stamp: 'jmv 9/18/2017 18:26:44'!
subtractMonth: aMonth
	"Months can be subtracted even they have different length."

	^self start subtractDateAndtime: aMonth start! !


!Year methodsFor: 'double displatching' stamp: 'jmv 9/18/2017 17:23:04'!
subtractFrom: aTimespan

	^ aTimespan subtractYear: self! !

!Year methodsFor: 'double displatching' stamp: 'jmv 9/18/2017 17:24:41'!
subtractYear: aYear
	"Years can be subtracted even if one of them is leap and the other isn't."

	^self start subtractDateAndtime: aYear start! !


!Timespan methodsFor: 'private' stamp: 'jmv 9/18/2017 18:29:20'!
subtractTimespan: aTimespan
	"
	(Month month: 'March' year: 2017) - (Month month: 'January' year: 2017)
	(Month month: 'February' year: 2017) - (Month month: 'January' year: 2017)

	(Year yearNumber: 2016) - (Year yearNumber: 2015).
	(Year yearNumber: 2017) - (Year yearNumber: 2016).
	(Year yearNumber: 2017) - (Year yearNumber: 2015).

	(Year yearNumber: 2018) - (Date today). 					'Error'.
	"
	aTimespan duration = self duration ifFalse: [
		self error: 'Can not subtract Timespans of different duration' ].
	
	^self start subtractDateAndtime: aTimespan start! !


!Month reorganize!
('squeak protocol' daysInMonth)
('inquiries' name)
('printing' printOn:)
('public protocol' daysInYear isLeapYear monthAbbreviation monthIndex monthName year yearNumber)
('double dispatching' subtractFrom: subtractMonth:)
!

