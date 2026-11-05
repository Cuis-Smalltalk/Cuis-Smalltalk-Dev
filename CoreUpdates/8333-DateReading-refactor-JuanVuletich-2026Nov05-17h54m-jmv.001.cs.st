'From Cuis7.9 [latest update: #8325] on 5 November 2026 at 6:08:32 pm'!

!Date class methodsFor: 'instance creation' stamp: 'jmv 5/Nov/2026, 9:02:07 pm (UTC)'!
readYMDFrom: aStream allow90sStyleStamps: aBoolean into: threeArgBlock
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
			^ self readYMDWithNamedMonthFrom: aStream firstNumber: firstAsNumber allow90sStyleStamps: aBoolean into: threeArgBlock ].
	
	"YY-MM-DD, YYYY-MM-DD, etc.  (ISO8601), MM-DD-YYYY"
	second := Integer readFrom: aStream.
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip].
	thirdStart := aStream position.
	third := Integer readFrom: aStream.
	thirdLength := aStream position - thirdStart.

	thirdLength > 2 ifTrue: [
		"MM-DD-YYYY"
		^ threeArgBlock value: third value: (Month nameOfMonth: firstAsNumber) value: second ].

	(aBoolean and: [third > 31]) ifTrue: [
		"MM-DD-YY - Only included to support the latter years of the 20th century."
		^ threeArgBlock value: 1900 + third value: (Month nameOfMonth: firstAsNumber) value: second ].

	"YY-MM-DD, YYYY-MM-DD, etc.  (ISO8601)"
	^ threeArgBlock value: firstAsNumber value: ((Month nameOfMonth: second)) value: third! !

!Date class methodsFor: 'instance creation' stamp: 'jmv 5/Nov/2026, 9:06:23 pm (UTC)'!
readYMDWithNamedMonthFrom: aStream firstNumber: firstAsNumberOrNil allow90sStyleStamps: aBoolean into: threeArgBlock
	"See sender. Handle these forms:
		<monthName> <day> <year>		(April 15, 1982)
		<year> <monthName> <day>		(1982 April 15)
		<day> <monthName> <year>		(15 April 1982) (Only if third argument has 3 or more characters)
		
		<day> <monthName> <year>		(15 April 82) (Only if third argument > 31 => can't be a day number)
												Only allowed if aBoolean is True.
												Note: This format is only to support 20th century legacy stuff!!
												Don't use this format for dates in the 21st or 1st centuries.
	Note: if first entry is not monthName, aStream is already past it, and it was read into firstAsNumberOrNil. See senders."
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

	^ threeArgBlock value: year value: month value: day! !


!Date class methodsFor: 'instance creation' stamp: 'jmv 5/Nov/2026, 9:03:19 pm (UTC)'!
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

	^self readYMDFrom: aStream allow90sStyleStamps: aBoolean
		into: [ :year :month :day | self year: year month: month day: day ]! !

!methodRemoval: Date class #readWithNamedMonthFrom:firstNumber:allow90sStyleStamps: stamp: 'jmv 5/Nov/2026, 9:03:22 pm (UTC)'!
Date class removeSelector: #readWithNamedMonthFrom:firstNumber:allow90sStyleStamps:!
