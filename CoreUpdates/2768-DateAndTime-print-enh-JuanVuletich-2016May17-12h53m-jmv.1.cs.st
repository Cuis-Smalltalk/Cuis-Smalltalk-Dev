'From Cuis 4.2 of 25 July 2013 [latest update: #2766] on 17 May 2016 at 1:13:29.759569 pm'!

!DateAndTime methodsFor: 'public protocol' stamp: 'jmv 5/17/2016 13:02'!
printStringWithoutOffset
	"Print as per ISO 8601 section 5.4.2
	Prints either:
		'YYYY-MM-DDThh:mm:ss.s' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s' (for negative years)"

	^String streamContents: [ :strm | self printWithoutOffsetOn: strm ]! !

!DateAndTime methodsFor: 'public protocol' stamp: 'jmv 5/17/2016 13:02'!
printWithoutOffsetOn: aStream
	"Print as per ISO 8601 section 5.4.2
	Prints either:
		'YYYY-MM-DDThh:mm:ss.s' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s' (for negative years)"

	^self printOn: aStream withLeadingSpace: false includeOffset: false! !


!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 5/17/2016 12:54'!
printOn: aStream
	"Print as per ISO 8601 section 5.4.2
	Prints either:
		'YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)"

	^self printOn: aStream withLeadingSpace: false includeOffset: true! !

!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 5/17/2016 12:57'!
printOn: aStream withLeadingSpace: printLeadingSpaceToo includeOffset: aBoolean
	"Print as per ISO 8601 section 5.4.2
	If printLeadingSpaceToo is false, prints either:
		'YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	If printLeadingSpaceToo is true, prints either:
		' YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)
	"

	self printYMDOn: aStream withLeadingSpace: printLeadingSpaceToo.
	aStream nextPut: $T.
	self printHMSOn: aStream.
	self nanoSecond ~= 0 ifTrue:
		[ | z ps |
		ps := self nanoSecond printString padded: #left to: 9 with: $0.
		z := ps findLast: [ :c | c asciiValue > $0 asciiValue ].
		(z > 0) ifTrue: [aStream nextPut: $.].
		ps from: 1 to: z do: [ :c | aStream nextPut: c ] ].
	aBoolean ifTrue: [
		aStream
			nextPut: (offset positive ifTrue: [$+] ifFalse: [$-]);
			nextPutAll: (offset hours abs asString padded: #left to: 2 with: $0);
			nextPut: $:;
			nextPutAll: (offset minutes abs asString padded: #left to: 2 with: $0).
		"Note: We should check the ISO to see if the seconds part is supported by the standard
		(usually not an issue. offsets usually don't include seconds)"
		offset seconds = 0 ifFalse: [
			aStream
				nextPut: $:;
				nextPutAll: (offset seconds abs truncated asString) ]]! !


!DateAndTime reorganize!
('ansi protocol' + - < = asLocal asUTC dayOfMonth dayOfWeek dayOfWeekAbbreviation dayOfWeekName dayOfYear hash hour hour12 hour24 isLeapYear meridianAbbreviation minute monthAbbreviation monthName offset second timeZoneAbbreviation timeZoneName)
('squeak protocol' dayMonthYearDo: julianDayNumber midnight nanoSecond noon printHMSOn: printOn: printOn:withLeadingSpace:includeOffset: printWithMsOn: printYMDOn: printYMDOn:withLeadingSpace: setJdn:seconds:nano:offset: to: to:by: to:by:do: utcOffset:)
('smalltalk-80' daysInMonth daysInYear daysLeftInYear firstDayOfMonth)
('private' julianDayNumber:seconds:nanoseconds:offset: secondsSinceMidnight)
('public protocol' date filenamishPrintOn: month monthIndex printStringWithoutOffset printWithoutOffsetOn: secondsSinceSqueakEpoch time unixTimeSeconds unixTimeSecondsFloat week year yearNumber)
('testing' is:)
!

