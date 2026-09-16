'From Cuis7.9 [latest update: #8185] on 16 September 2026 at 2:48:36 pm'!

!Date class methodsFor: 'instance creation' stamp: 'jmv 16/Sep/2026, 4:21:26 pm (UTC)'!
readFrom: aStream
	"Read a Date from the stream in any of the forms:
		<year>-<month>-<day>			(1982-04-15) (ISO8601)
		<month>-<day>-<year>			(04-15-1982) (Only if third argument has 3 or more characters)
		<month>-<day>-<year>			(04-15-82) (Only if third argument > 31 => can't be a day number)
												Note: This format is only to support 20th century legacy stuff!!
												Don't use this format for dates in the 21st or 1st centuries.
		<monthName> <day> <year>		(April 15, 1982)
		<year> <monthName> <day>		(1982 April 15)
		<day> <monthName> <year>		(15 April 1982) (Only if third argument has 3 or more characters)
		<day> <monthName> <year>		(15 April 82) (Only if third argument > 31 => can't be a day number)
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
			^self readWithNamedMonthFrom: aStream firstNumber: firstAsNumber ].
	
	"YY-MM-DD, YYYY-MM-DD, etc.  (ISO8601), MM-DD-YYYY"
	second := Integer readFrom: aStream.
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	thirdStart := aStream position.
	third := Integer readFrom: aStream.
	thirdLength := aStream position - thirdStart.

	thirdLength > 2 ifTrue: [
		"MM-DD-YYYY"
		^self year: third month: (Month nameOfMonth: firstAsNumber) day: second ].

	third > 31 ifTrue: [
		"MM-DD-YY - Only included to support the latter years of the 20th century."
		^self year: 1900 + third month: (Month nameOfMonth: firstAsNumber) day: second ].

	"YY-MM-DD, YYYY-MM-DD, etc.  (ISO8601)"
	^self year: firstAsNumber month: (Month nameOfMonth: second) day: third! !

!Date class methodsFor: 'instance creation' stamp: 'jmv 16/Sep/2026, 4:21:36 pm (UTC)'!
readWithNamedMonthFrom: aStream firstNumber: firstAsNumberOrNil
	"See sender. Handle these forms:
		<monthName> <day> <year>		(April 15, 1982)
		<year> <monthName> <day>		(1982 April 15)
		<day> <monthName> <year>		(15 April 1982) (Only if third argument has 3 or more characters)
		<day> <monthName> <year>		(15 April 82) (Only if third argument > 31 => can't be a day number)
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
					third > 31
						ifTrue: [
							"DD MonthName YY.  - Only included to support the latter years of the 20th century."
							day := firstAsNumberOrNil.
							year := 1900 + third ]
						ifFalse: [
							"YY-MonthName-DD"
							year := firstAsNumberOrNil.
							day := third ]]].

	^ self year: year month: month day: day! !

