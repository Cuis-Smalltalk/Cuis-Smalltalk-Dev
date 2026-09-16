'From Cuis7.9 [latest update: #8189] on 16 September 2026 at 3:46:57 pm'!

!Date class methodsFor: 'instance creation' stamp: 'jmv 16/Sep/2026, 6:43:28 pm (UTC)'!
readFrom: aStream allow90sStyleStamps: aBoolean
	"Read a Date from the stream in any of the forms:
		<year>-<month>-<day>			(1982-04-15) (ISO8601)
		<month>-<day>-<year>			(04-15-1982) (Only if third argument has 3 or more characters)
		<monthName> <day> <year>		(April 15, 1982)
		<year> <monthName> <day>		(1982 April 15)
		<day> <monthName> <year>		(15 April 1982) (Only if third argument has 3 or more characters)
		
		<month>-<day>-<year>			(04-15-82) (Only if third argument > 31 => can't be a day number)
												Only allowed if aBoolean is True.
												Note: This format is only to support 20th century legacy stuff!!
												Don't use this format for dates in the 21st or 1st centuries.
		<day> <monthName> <year>		(15 April 82) (Only if third argument > 31 => can't be a day number)
												Only allowed if aBoolean is True.
												Note: This format is only to support 20th century legacy stuff!!
												Don't use this format for dates in the 21st or 1st centuries."

	| firstAsNumber firstAsNumberSign second third thirdLength thirdStart |
	firstAsNumberSign := aStream peek == $- ifTrue: [-1] ifFalse: [1].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isDigit ifTrue: [
		firstAsNumber := (Integer readFrom: aStream) * firstAsNumberSign ].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isLetter
		ifTrue: [
			"MonthName-DD-YY or DD-MonthName-YY or YY-MonthName-DD"
			^self readWithNamedMonthFrom: aStream
				firstNumber: firstAsNumber allow90sStyleStamps: aBoolean ].
	
	"YY-MM-DD, YYYY-MM-DD, etc.  (ISO8601), MM-DD-YYYY"
	second := Integer readFrom: aStream.
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	thirdStart := aStream position.
	third := Integer readFrom: aStream.
	thirdLength := aStream position - thirdStart.

	thirdLength > 2 ifTrue: [
		"MM-DD-YYYY"
		^self year: third month: (Month nameOfMonth: firstAsNumber) day: second ].

	(aBoolean and: [third > 31]) ifTrue: [
		"MM-DD-YY - Only included to support the latter years of the 20th century."
		^self year: 1900 + third month: (Month nameOfMonth: firstAsNumber) day: second ].

	"YY-MM-DD, YYYY-MM-DD, etc.  (ISO8601)"
	^self year: firstAsNumber month: (Month nameOfMonth: second) day: third! !

!Date class methodsFor: 'instance creation' stamp: 'jmv 16/Sep/2026, 6:40:14 pm (UTC)'!
readWithNamedMonthFrom: aStream firstNumber: firstAsNumberOrNil allow90sStyleStamps: aBoolean
	"See sender. Handle these forms:
		<monthName> <day> <year>		(April 15, 1982)
		<year> <monthName> <day>		(1982 April 15)
		<day> <monthName> <year>		(15 April 1982) (Only if third argument has 3 or more characters)
		
		<day> <monthName> <year>		(15 April 82) (Only if third argument > 31 => can't be a day number)
												Only allowed if aBoolean is True.
												Note: This format is only to support 20th century legacy stuff!!
												Don't use this format for dates in the 21st or 1st centuries."
	| day month year |

	"MonthName-DD-YY or DD-MonthName-YY or YY-MonthName-DD"
	month := String streamContents: [ :strm |
		[ aStream peek isLetter ] whileTrue: [ strm nextPut: aStream next ]].
	firstAsNumberOrNil
		ifNil: [
			"MonthName DD YY"
			[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
			day := Integer readFrom: aStream.
			[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
			year := Integer readFrom: aStream ]
		ifNotNil: [ | third thirdLength thirdStart |
			"DD-MonthName-YYYY, DD-MonthName-YY or YY-MonthName-DD"
			[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
			thirdStart := aStream position.
			third := Integer readFrom: aStream.
			thirdLength := aStream position - thirdStart.
			thirdLength > 2
				ifTrue: [
					"DD MonthName YYYY"
					day := firstAsNumberOrNil.
					year := third ]
				ifFalse: [
					(aBoolean and: [third > 31])
						ifTrue: [
							"DD MonthName YY.  - Only included to support the latter years of the 20th century."
							day := firstAsNumberOrNil.
							year := 1900 + third ]
						ifFalse: [
							"YY-MonthName-DD"
							year := firstAsNumberOrNil.
							day := third ]]].

	^ self year: year month: month day: day! !


!Date class methodsFor: 'instance creation' stamp: 'jmv 16/Sep/2026, 6:39:36 pm (UTC)'!
readFrom: aStream
	"Read a Date from the stream in any of the forms:
		<year>-<month>-<day>			(1982-04-15) (ISO8601)
		<month>-<day>-<year>			(04-15-1982) (Only if third argument has 3 or more characters)
		<monthName> <day> <year>		(April 15, 1982)
		<year> <monthName> <day>		(1982 April 15)
		<day> <monthName> <year>		(15 April 1982) (Only if third argument has 3 or more characters)"

	^self readFrom: aStream allow90sStyleStamps: false! !


!CompiledMethod class methodsFor: 'services' stamp: 'jmv 16/Sep/2026, 6:44:18 pm (UTC)'!
authorAndStamp: authorAndStamp partsDo: aBlock
	"Given a method time stamp string (see senders, see #authorAndStampAsSaved)
	evaluate a block on its parts: author initials[String] and timestamp[DateAndTime].
	Parts may be nil if not present.
	Support the various formats saved by Smalltalk-80, Squeak and Cuis over the years.
(Examples follow)
	CompiledMethod authorAndStamp: 'Squeak1.2 29/Jun/1997 10:40:55' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: ' 9/Jun/1997 00:00:00' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 5/20/2011 14:37' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 10/Jan/2014 22:53:00' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25 am ' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25 pm ' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 23:02:25' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25 am (-03:00)' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25Z' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25 UTC' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25 (UTC)' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25UTC' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25(UTC)' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25 am UTC' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 17/Jun/2026, 11:02:25 am (UTC)' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 5/20/2011T14:37' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 5/20/2011X14:37' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 5/20/2011 14:37' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 5/20/2011T4:37' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 5/20/2011X4:37' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 5/20/2011 4:37' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
	CompiledMethod authorAndStamp: 'jmv 5-20-2011 4:37' partsDo: [ :initials :timestamp | {initials. timestamp} print ].
"
	| authorAndDateParts authorPart colonPosition authorDateSeparator timePart datePart utcOffsetPart utcOffset dateAndTime |
	authorAndStamp ifNil: [^aBlock value: nil value: nil].
	authorAndDateParts := authorAndStamp.

	"Look for a Time (perhaps with an UTC offset afterwards)"
	colonPosition := authorAndStamp indexOf: $:.
	colonPosition = 0 ifFalse: [ | dateEnd timeStart utcOffsetStart |
		timeStart := (authorAndStamp at: colonPosition-2) isDigit
			ifTrue: [colonPosition-2]
			ifFalse: [colonPosition-1].
		dateEnd := (authorAndStamp at: timeStart-2) isDigit
			ifTrue: [timeStart-2]
			ifFalse: [timeStart-3].
		timePart := authorAndStamp copyFrom: timeStart to: authorAndStamp size.
		authorAndDateParts := authorAndStamp copyFrom: 1 to: dateEnd.
		"Look for a final UTC offset"
		utcOffsetPart := nil.
		utcOffsetStart := timePart indexOf: $(.
		utcOffsetStart = 0
			ifFalse: [
				utcOffsetPart := timePart copyFrom: utcOffsetStart+1 to: timePart size-1.
				timePart := timePart copyFrom: 1 to: utcOffsetStart-1 ]
			ifTrue: [
				utcOffsetStart := timePart indexOf: $Z ifAbsent: [timePart indexOf: $U].
				utcOffsetStart = 0 ifFalse: [
					utcOffsetPart := timePart copyFrom: utcOffsetStart to: timePart size.
					timePart := timePart copyFrom: 1 to: utcOffsetStart-1 ]].
		utcOffset := utcOffsetPart ifNotNil: [
			(utcOffsetPart = 'UTC' or: [ utcOffsetPart = 'Z'])
				ifTrue: [Duration zero]
				ifFalse: [Duration fromString: utcOffsetPart ]]].

	"Look for a Date. Separate authorPart and datePart."
	authorDateSeparator := authorAndDateParts lastIndexOf: Character space.
	datePart := authorAndDateParts copyFrom: authorDateSeparator+1 to: authorAndDateParts size.
	((datePart occurrencesOf: $/) >=2 or: [ (datePart occurrencesOf: $-) >=2  ])
		ifTrue: [
			authorPart := authorDateSeparator > 0 ifTrue: [authorAndDateParts copyFrom: 1 to: authorDateSeparator] ifFalse: ['unknown']]
		ifFalse: [
			authorPart := authorAndDateParts.
			datePart := nil.].

	dateAndTime := datePart ifNotNil: [
		DateAndTime
			date: (Date readFrom: datePart readStream allow90sStyleStamps: true)
			time: (timePart ifNotNil: [Time fromString: timePart withBlanksTrimmed])
			offset: utcOffset ].
	^aBlock
		value: authorPart withBlanksTrimmed
		value: dateAndTime! !

!methodRemoval: Date class #readWithNamedMonthFrom:firstNumber: stamp: 'jmv 16/Sep/2026, 6:43:40 pm (UTC)'!
Date class removeSelector: #readWithNamedMonthFrom:firstNumber:!
