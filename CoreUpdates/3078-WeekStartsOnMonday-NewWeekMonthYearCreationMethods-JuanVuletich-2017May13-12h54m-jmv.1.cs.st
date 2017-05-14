'From Cuis 5.0 of 7 November 2016 [latest update: #3076] on 13 May 2017 at 1:02:33 pm'!

!Timespan class methodsFor: 'squeak protocol' stamp: 'jmv 5/9/2017 19:54:38'!
fromString: aString
	"Please call with specific subclass."

	^ self readFrom: aString readStream! !


!Week class methodsFor: 'squeak protocol' stamp: 'jmv 5/12/2017 17:48:37'!
readFrom: aStream 
	"Read a Week from the stream in any of the forms:  
		<year>-W<week>			(2009-W01) (ISO8601)"
	| weekNumber yearNumber firstAsNumber firstAsNumberSign |
	firstAsNumberSign := aStream peek = $- ifTrue: [-1] ifFalse: [1].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
	aStream peek isDigit ifTrue: [
		firstAsNumber := (Integer readFrom: aStream) * firstAsNumberSign.
		(firstAsNumber < 0 or: [firstAsNumber > 12])
			ifTrue: [yearNumber := firstAsNumber]].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
	aStream next = $W ifFalse: [
		self error: 'Invalid Format' ].

	weekNumber _ Integer readFrom: aStream.
	weekNumber < 1 ifTrue: [ self error: 'Invalid Format' ].
	(yearNumber < 100 and: [yearNumber >= 0])  ifTrue: [
		yearNumber _ yearNumber < 69 
			ifTrue: [2000 + yearNumber]
			ifFalse: [1900 + yearNumber]].

	^ self yearNumber: yearNumber weekNumber: weekNumber! !

!Week class methodsFor: 'squeak protocol' stamp: 'jmv 5/13/2017 10:54:55'!
yearNumber: yearNumber weekNumber: weekNumber

	| firstOfJanuary firstThursday thisThursday |
	firstOfJanuary _ DateAndTime year: yearNumber month: 1 day: 1.
	firstThursday _ firstOfJanuary + (4 - firstOfJanuary dayOfWeek \\ 7) days.
	thisThursday _ firstThursday + ((weekNumber-1) * 7) days.

	thisThursday yearNumber = yearNumber
		ifFalse: [ self error: 'Week does not exist' ].

	^ self including: thisThursday! !


!Year class methodsFor: 'squeak protocol' stamp: 'jmv 5/12/2017 16:41:15'!
readFrom: aStream 

	| year sign |
	sign := aStream peek = $- ifTrue: [-1] ifFalse: [1].
	[aStream peek isDigit] whileFalse: [aStream skip: 1].
	year := (Integer readFrom: aStream) * sign.
	^ self yearNumber: year! !


!String methodsFor: 'converting' stamp: 'jmv 5/12/2017 11:24:09'!
asMonth
	"Many allowed forms, see Month>>#readFrom:
		'July 1998' asMonth.
		'1998/7'asMonth.
	"

	^ Month fromString: self! !

!String methodsFor: 'converting' stamp: 'jmv 5/12/2017 16:39:13'!
asWeek
	"
		'2008-W52' asWeek.
		'2008-W53' asWeek. 'Invalid format!!'.
		'2009-W01' asWeek
		'2009-W02' asWeek
		'2009-W53' asWeek
		'2010-W01' asWeek
		'2010-W02' asWeek
		'2008-W52' asWeek start.
		'2009-W01' asWeek start
		'2009-W02' asWeek start
		'2009-W53' asWeek start
		'2010-W01' asWeek start
		'2010-W02' asWeek start
	"

	^ Week fromString: self! !

!String methodsFor: 'converting' stamp: 'jmv 5/12/2017 16:37:23'!
asYear
	"
		'2008' asYear.
		'2008' asYear start.
	"

	^ Year fromString: self! !


!DateAndTime methodsFor: 'ansi protocol' stamp: 'jmv 5/12/2017 10:55:06'!
dayOfWeek

	"
	Sunday=1, ... , Saturday=7
	Monday=1, ... , Sunday=7
	'12 May 2017 ' asDate dayOfWeek = 5
	"

	^ (jdn rem: 7) + 1! !

!DateAndTime methodsFor: 'ansi protocol' stamp: 'jmv 5/12/2017 10:53:00'!
dayOfWeekName
	"
	'12 May 2017 ' asDate dayOfWeek = 5 
	'12 May 2017 ' asDate dayOfWeekName = #Friday 
	"

	^ Week nameOfDay: self dayOfWeek
! !


!Duration methodsFor: 'squeak protocol' stamp: 'jmv 5/12/2017 10:59:51'!
printOn: aStream
	"Format as per ANSI 5.8.2.16: [-]D:HH:MM:SS[.S]
	(Duration days: 2 hours: 3 minutes: 16 seconds: 43) printString =  '2:03:16:43' 
	"
	| d h m s n |
	d _ self days abs.
	h _ self hours abs.
	m _ self minutes abs.
 	s _ self seconds abs truncated.
	n _ self nanoSeconds abs. 	self negative ifTrue: [ aStream nextPut: $- ].
	d printOn: aStream. aStream nextPut: $:.
	h printOn: aStream length: 2 zeroPadded: true.aStream nextPut: $:.
	m printOn: aStream length: 2 zeroPadded: true.aStream nextPut: $:.
	s printOn: aStream length: 2 zeroPadded: true.
	n = 0 ifFalse: [
		| z ps |
		aStream nextPut: $..
		ps _ n printString padded: #left to: 9 with: $0. 
		z _ ps findLast: [ :c | c digitValue > 0 ].
		ps from: 1 to: z do: [ :c | aStream nextPut: c ] ]! !


!Date methodsFor: 'smalltalk-80' stamp: 'jmv 5/12/2017 10:54:58'!
weekdayIndex
	"Sunday=1, ... , Saturday=7
	Monday=1, ... , Sunday=7
	'12 May 2017 ' asDate weekdayIndex = 5
	"

	^ self dayOfWeek! !


!Date class methodsFor: 'squeak protocol' stamp: 'jmv 5/10/2017 21:56:27'!
readFrom: aStream 
	"Read a Date from the stream in any of the forms:
		<day> <month> <year>		(15 April 1982; 15-APR-82; 15.4.82; 15APR82)  
		<month> <day> <year>		(April 15, 1982; 4/15/82)
		<year>-<month>-<day>		(1982-04-15) (ISO8601)"
	| day month year firstAsNumber firstAsNumberSign |
	firstAsNumberSign := aStream peek = $- ifTrue: [-1] ifFalse: [1].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
	aStream peek isDigit ifTrue: [
		firstAsNumber := (Integer readFrom: aStream) * firstAsNumberSign.
		(firstAsNumber < 0 or: [firstAsNumber > 31])
			ifTrue: [year := firstAsNumber]].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
	aStream peek isLetter
		ifTrue: ["MonthName-DD-YY or DD-MonthName-YY or YY-MonthName-DD"
			month _ String streamContents: [ :strm |
				[ aStream peek isLetter ] whileTrue: [ strm nextPut: aStream next ]].
			[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
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

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
	year
		ifNil: [year := Integer readFrom: aStream]
		ifNotNil: [day := Integer readFrom: aStream].
	(year < 100 and: [year >= 0])  ifTrue: [
		year _  year < 69 
			ifTrue: [2000 + year]
			ifFalse: [1900 + year]].

	^ self year: year month: month day: day! !


!Month class methodsFor: 'squeak protocol' stamp: 'jmv 5/12/2017 11:28:03'!
readFrom: aStream 
	"Read a Month from the stream in any of the forms:  
		<month> <year>			(April 1982; APR-82; 4.82; APR82)  
		<month> <year>			(April, 1982; 4/82)
		<year>-<month>			(1982-04) (ISO8601)"
	"
	Month readFrom: 'July 1998' readStream
	"
	| month year firstAsNumber firstAsNumberSign |
	firstAsNumberSign := aStream peek = $- ifTrue: [-1] ifFalse: [1].
	[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
	aStream peek isDigit ifTrue: [
		firstAsNumber := (Integer readFrom: aStream) * firstAsNumberSign.
		(firstAsNumber < 0 or: [firstAsNumber > 12])
			ifTrue: [year := firstAsNumber]].

	[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
	aStream peek isLetter
		ifTrue: ["MonthName-YY or YY-MonthName"
			month _ String streamContents: [ :strm |
				[ aStream peek isLetter ] whileTrue: [ strm nextPut: aStream next ]].
			[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1]]
		ifFalse: ["MM-YY or YY-MM"
			month _ year 
				ifNil: ["MM-YY"
					Month nameOfMonth: firstAsNumber ]
				ifNotNil: ["YY-MM"
					Month nameOfMonth: (Integer readFrom: aStream)]].

	year ifNil: [
		[aStream peek isAlphaNumeric] whileFalse: [aStream skip: 1].
		year := Integer readFrom: aStream].
	(year < 100 and: [year >= 0])  ifTrue: [
		year _ year < 69 
			ifTrue: [2000 + year]
			ifFalse: [1900 + year]].

	^ self month: month year: year! !


!Week methodsFor: 'squeak protocol' stamp: 'jmv 5/12/2017 11:53:32'!
printOn: aStream
	"
		'2008-W52' asWeek.
		'2009-W01' asWeek
		'2009-W02' asWeek
		'2009-W53' asWeek
		'2010-W01' asWeek
		'2010-W02' asWeek
	"
	| thursday |
	thursday _ self start + 3 days.
	thursday yearNumber printOn: aStream.
	aStream nextPutAll: '-W'.
	(thursday dayOfYear-1 // 7 + 1) printOn: aStream length: 2 zeroPadded: true! !


!Week class methodsFor: 'squeak protocol' stamp: 'jmv 5/12/2017 10:54:04'!
indexOfDay: aSymbol
	"
	Week indexOfDay: #Sunday = 7
	Week nameOfDay: 7 = #Sunday 
	"

	^ self dayNames indexOf: aSymbol! !

!Week class methodsFor: 'smalltalk-80' stamp: 'jmv 5/12/2017 10:53:55'!
nameOfDay: anIndex
	"
	Week indexOfDay: #Sunday = 7
	Week nameOfDay: 7 = #Sunday 
	"

	^ self dayNames at: anIndex! !

!Week class methodsFor: 'inquiries' stamp: 'jmv 5/10/2017 22:25:02'!
dayNames

	^ #(Monday Tuesday Wednesday Thursday Friday Saturday Sunday)! !


!String methodsFor: 'converting' stamp: 'jmv 5/12/2017 11:22:17'!
asDate
	"Many allowed forms, see Date>>#readFrom:
		'2014/6/30' asDate.
		'70/12/30' asDate.
		'12/30/70' asDate.
		'30/12/70' asDate.
		'4/5/6' asDate.
		'15 April 1982' asDate.
	"

	^ Date fromString: self! !

!methodRemoval: Date class #fromString:!
Date class removeSelector: #fromString:!
