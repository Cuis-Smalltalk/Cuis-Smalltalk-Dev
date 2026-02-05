'From Cuis7.7 [latest update: #7827] on 5 February 2026 at 11:08:41 am'!

!Date class methodsFor: 'squeak protocol' stamp: 'jmv 26/May/2022 16:33:54'!
readFrom_old: aStream 
	"Read a Date from the stream in any of the forms:
		<day> <month> <year>		(15 April 1982; 15-APR-82; 15.4.82; 15APR82)  
		<month> <day> <year>		(April 15, 1982; 4/15/82)
		<year>-<month>-<day>		(1982-04-15) (ISO8601)"
	| day month year firstAsNumber firstAsNumberSign |
	self flag: #oldCode. "Replaced by a method with #newCode flag and _new suffix."

	firstAsNumberSign := aStream peek == $- ifTrue: [-1] ifFalse: [1].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isDigit ifTrue: [
		firstAsNumber := (Integer readFrom: aStream) * firstAsNumberSign.
		(firstAsNumber < 0 or: [firstAsNumber > 31])
			ifTrue: [year := firstAsNumber]].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isLetter
		ifTrue: ["MonthName-DD-YY or DD-MonthName-YY or YY-MonthName-DD"
			month := String streamContents: [ :strm |
				[ aStream peek isLetter ] whileTrue: [ strm nextPut: aStream next ]].
			[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
			firstAsNumber
				ifNil: ["MonthName DD YY"
					day := Integer readFrom: aStream]
				ifNotNil: [
					year ifNil: ["DD MonthName YY"
						day := firstAsNumber]]]
		ifFalse: ["MM-DD-YY or DD-MM-YY or YY-MM-DD"
			year 
				ifNil: ["MM-DD-YY or DD-MM-YY"
					firstAsNumber > 12
						ifTrue: ["DD-MM-YY"
							day := firstAsNumber.
							month := Month nameOfMonth: (Integer readFrom: aStream)]
						ifFalse: ["MM-DD-YY"
							month := Month nameOfMonth: firstAsNumber.
							day := Integer readFrom: aStream]]
				ifNotNil: ["YY-MM-DD"
					month := Month nameOfMonth: (Integer readFrom: aStream)]].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	year
		ifNil: [year := Integer readFrom: aStream]
		ifNotNil: [day := Integer readFrom: aStream].
	(year < 100 and: [year >= 0])  ifTrue: [
		year :=  year < 69 
			ifTrue: [2000 + year]
			ifFalse: [1900 + year]].

	^ self year: year month: month day: day! !

!Date class methodsFor: 'instance creation' stamp: 'jmv 5/Feb/2026 10:58:45'!
readFrom_new: aStream
	"Read a Date from the stream in any of the forms:
		<year>-<month>-<day>			(1982-04-15) (ISO8601)
		<month>-<day>-<year>			(04-15-1982) (Only if third argument has 3 or more characters)
		<monthName> <day> <year>		(April 15, 1982)
		<year> <monthName> <day>		(1982 April 15)
		<day> <monthName> <year>		(15 April 1982) (Only if third argument has 3 or more characters)"

	| firstAsNumber firstAsNumberSign second third thirdLength thirdStart |
	self flag: #newCode. "Replaces by a method with #oldCode flag and _old suffix."

	firstAsNumberSign := aStream peek == $- ifTrue: [-1] ifFalse: [1].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isDigit ifTrue: [
		firstAsNumber := (Integer readFrom: aStream) * firstAsNumberSign ].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isLetter
		ifTrue: [
			"MonthName-DD-YY or DD-MonthName-YY or YY-MonthName-DD"
			^self readWithNamedMonthFrom: aStream firstNumber: firstAsNumber ].
	
	"YY-MM-DD, YYYY-MM-DD, etc.  (ISO8601), MM-DD-YYYY"
	second := Integer readFrom: aStream.
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	thirdStart := aStream position.
	third := Integer readFrom: aStream.
	thirdLength := aStream position - thirdStart.

	^thirdLength > 2
		ifTrue: [
			"MM-DD-YYYY"
			self year: third month: (Month nameOfMonth: firstAsNumber) day: second ]
		ifFalse: [
			"YY-MM-DD, YYYY-MM-DD, etc.  (ISO8601)"
			self year: firstAsNumber month: (Month nameOfMonth: second) day: third ]! !

!Date class methodsFor: 'instance creation' stamp: 'jmv 5/Feb/2026 10:56:40'!
readWithNamedMonthFrom: aStream firstNumber: firstAsNumberOrNil
	"See sender. Handle these forms:
		<monthName> <day> <year>		(April 15, 1982)
		<year> <monthName> <day>		(1982 April 15)
		<day> <monthName> <year>		(15 April 1982) (Only if third argument has 3 or more characters)
		"
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
			"DD-MonthName-YY or YY-MonthName-DD"
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
					"YY-MonthName-DD"
					year := firstAsNumberOrNil.
					day := third ]].

	^ self year: year month: month day: day! !


!Month class methodsFor: 'squeak protocol' stamp: 'jmv 26/May/2022 16:33:59'!
readFrom_old: aStream 
	"Read a Month from the stream in any of the forms:  
		<month> <year>			(April 1982; APR-82; 4.82; APR82)  
		<month> <year>			(April, 1982; 4/82)
		<year>-<month>			(1982-04) (ISO8601)"
	"
	Month readFrom: 'July 1998' readStream
	"
	| month year firstAsNumber firstAsNumberSign |
	self flag: #oldCode. "Replaced by a method with #newCode flag and _new suffix."

	firstAsNumberSign := aStream peek == $- ifTrue: [-1] ifFalse: [1].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isDigit ifTrue: [
		firstAsNumber := (Integer readFrom: aStream) * firstAsNumberSign.
		(firstAsNumber < 0 or: [firstAsNumber > 12])
			ifTrue: [year := firstAsNumber]].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isLetter
		ifTrue: ["MonthName-YY or YY-MonthName"
			month := String streamContents: [ :strm |
				[ aStream peek isLetter ] whileTrue: [ strm nextPut: aStream next ]].
			[aStream peek isAlphaNumeric] whileFalse: [aStream skip]]
		ifFalse: ["MM-YY or YY-MM"
			month := year 
				ifNil: ["MM-YY"
					Month nameOfMonth: firstAsNumber ]
				ifNotNil: ["YY-MM"
					Month nameOfMonth: (Integer readFrom: aStream)]].

	year ifNil: [
		[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
		year := Integer readFrom: aStream].
	(year < 100 and: [year >= 0])  ifTrue: [
		year := year < 69 
			ifTrue: [2000 + year]
			ifFalse: [1900 + year]].

	^ self month: month year: year! !

!Month class methodsFor: 'instance creation' stamp: 'jmv 5/Feb/2026 10:58:34'!
readFrom_new: aStream
	"Read a Date from the stream in any of the forms:
		<year>-<month>			(1982-04) (ISO8601)
		<month>-<year>			(04-1982) (Only if second argument has 3 or more characters)
		<monthName> <year>		(April, 1982; April 1982; APR-82; APR82)
		<year> <monthName>		(1982, April; 1982 April; etc)"

	| firstAsNumber firstAsNumberSign second secondLength secondStart |
	self flag: #newCode. "Replaces by a method with #oldCode flag and _old suffix."

	firstAsNumberSign := aStream peek == $- ifTrue: [-1] ifFalse: [1].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isDigit ifTrue: [
		firstAsNumber := (Integer readFrom: aStream) * firstAsNumberSign ].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	aStream peek isLetter
		ifTrue: [
			^self readNamedMonthFrom: aStream firstNumber: firstAsNumber ].
	
	"YY-MM, YYYY-MM, etc.  (ISO8601), MM-YYYY"
	secondStart := aStream position.
	second := Integer readFrom: aStream.
	secondLength := aStream position - secondStart.

	^secondLength > 2
		ifTrue: [
			"MM-YYYY"
			^self month: (Month nameOfMonth: firstAsNumber) year: second ]
		ifFalse: [
			"YY-MM, YYYY-MM, etc.  (ISO8601)"
			^self month: (Month nameOfMonth: second) year: firstAsNumber ]! !

!Month class methodsFor: 'instance creation' stamp: 'jmv 5/Feb/2026 10:55:32'!
readNamedMonthFrom: aStream firstNumber: firstAsNumberOrNil
	"See sender. Handle these forms:
		<monthName> <year>		(April, 1982; April 1982; APR-82; APR82)
		<year> <monthName>		(1982, April; 1982 April; etc)
		"

	| month year |

	"MonthName-YY or YY-MonthName"
	month := String streamContents: [ :strm |
		[ aStream peek isLetter ] whileTrue: [ strm nextPut: aStream next ]].
	firstAsNumberOrNil
		ifNil: [
			"MonthName YY"
			[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
			year := Integer readFrom: aStream ]
		ifNotNil: [
			"YY-MonthName"
			year := firstAsNumberOrNil ].

	^ self month: month year: year! !


!Date class methodsFor: 'instance creation' stamp: 'jmv 5/Feb/2026 10:58:18'!
readFrom: aStream
	"Read a Date from the stream in any of the forms:
		<year>-<month>-<day>			(1982-04-15) (ISO8601)
		<month>-<day>-<year>			(04-15-1982) (Only if third argument has 3 or more characters)
		<monthName> <day> <year>		(April 15, 1982)
		<year> <monthName> <day>		(1982 April 15)
		<day> <monthName> <year>		(15 April 1982) (Only if third argument has 3 or more characters)"

	| newAnswer oldAnswer p |
	p := aStream position.
	oldAnswer := self readFrom_old: aStream.
	aStream position: p.
	newAnswer := self readFrom_new: aStream.
	BackCompatibilityAlert checkOldAnswer: oldAnswer newAnswer: newAnswer.
	^newAnswer! !


!Month class methodsFor: 'instance creation' stamp: 'jmv 5/Feb/2026 10:58:30'!
readFrom: aStream
	"Read a Date from the stream in any of the forms:
		<year>-<month>			(1982-04) (ISO8601)
		<month>-<year>			(04-1982) (Only if second argument has 3 or more characters)
		<monthName> <year>		(April, 1982; April 1982; APR-82; APR82)
		<year> <monthName>		(1982, April; 1982 April; etc)"

	| newAnswer oldAnswer p |
	p := aStream position.
	oldAnswer := self readFrom_old: aStream.
	aStream position: p.
	newAnswer := self readFrom_new: aStream.
	BackCompatibilityAlert checkOldAnswer: oldAnswer newAnswer: newAnswer.
	^newAnswer! !


!Date class reorganize!
('smalltalk-80' dayOfWeek: daysInMonth:forYear: daysInYear: firstWeekdayOfMonth:year: fromDays: fromSeconds: indexOfMonth: leapYear: nameOfDay: nameOfMonth: newDay:month:year: newDay:year: today)
('squeak protocol' julianDayNumber: readFrom_old: tomorrow year:day: year:month:day: yesterday)
('general inquiries' dateAndTimeNow easterDateFor: orthodoxEasterDateFor:)
('inquiries' classDefinesDuration mustEncompassWholeDates)
('instance creation' includingDateAndTime: readFrom: readFrom_new: readWithNamedMonthFrom:firstNumber:)
!


!Month class reorganize!
('squeak protocol' includingDateAndTime: month:year: readFrom_old:)
('smalltalk-80' daysInMonth:forYear: indexOfMonth: nameOfMonth:)
('inquiries' classDefinesDuration daysInMonth monthNames mustEncompassWholeDates)
('instance creation' readFrom: readFrom_new: readNamedMonthFrom:firstNumber:)
!

