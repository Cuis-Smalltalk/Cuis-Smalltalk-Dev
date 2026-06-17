'From Cuis7.9 [latest update: #8004] on 17 June 2026 at 1:47:39 pm'!

!DateAndTime class methodsFor: 'squeak protocol' stamp: 'jmv 17/Jun/2026 13:16:55'!
date: aDate time: aTimeOrNil offset: anUTCoffset
	"If anOffset is nil, use local time zone.
	If UTC is desired, pass Duration zero as the offset."

	anUTCoffset ifNil: [
		^ self date: aDate time: aTimeOrNil ].

	aTimeOrNil ifNil: [
		^ self 
			year: aDate yearNumber
			day: aDate dayOfYear
			hour: 0
			minute: 0
			second: 0
			offset: anUTCoffset ].

	^ self 
		year: aDate yearNumber
		day: aDate dayOfYear
		hour: aTimeOrNil hour
		minute: aTimeOrNil minute
		second: aTimeOrNil second
		offset: anUTCoffset! !


!DateAndTime reorganize!
('ansi protocol' + - < = asUTC dayOfMonth dayOfWeek dayOfWeekAbbreviation dayOfWeekName dayOfYear hash hour hour12 hour24 isLeapYear meridianAbbreviation minute monthAbbreviation monthName offset printOn: second)
('squeak protocol' dayMonthYearDo: julianDayNumber midnight nanoSecond noon printHMSOn: printHMSOn:separator: printOn:includeOffset: printWithMsOn: printYMDOn: setJdn:seconds:nano:offset: to: to:by: to:by:do: utcOffset:)
('smalltalk-80' daysInMonth daysInYear daysLeftInYear firstDayOfMonth)
('private' julianDayNumber:seconds:nanoseconds:offset: printOffsetOn: secondsSinceMidnight subtractDateAndtime: subtractDuration:)
('public protocol' date filenamishPrintOn: month monthIndex printNiceOn: printStringWithoutOffset printWithoutOffsetOn: secondsSinceSqueakEpoch time truncateToSeconds unixTimeSeconds unixTimeSecondsFloat week year yearNumber)
('testing' is: isUTC)
('double dispatching' includingTimespanOf: subtractFrom:)
!

