"
Instances of Date are Timespans with duration of 1 day.
Their default creation assumes a start of midnight in the local time zone.
"
Class {
	#name : 'Date',
	#superclass : 'Timespan',
	#category : 'Kernel-Chronology',
	#package : 'Kernel',
	#tag : 'Chronology'
}

{ #category : 'inquiries' }
Date class >> classDefinesDuration [

	^true
]

{ #category : 'general inquiries' }
Date class >> dateAndTimeNow [
	"Answer an array with first element Date today and second element Time now."

	^Time dateAndTimeNow
]

{ #category : 'smalltalk-80' }
Date class >> dayOfWeek: dayName  [

	^ Week indexOfDay: dayName
]

{ #category : 'smalltalk-80' }
Date class >> daysInMonth: monthName forYear: yearInteger  [

	^ Month daysInMonth: monthName forYear: yearInteger.

]

{ #category : 'smalltalk-80' }
Date class >> daysInYear: yearInteger  [

	^ Year daysInYear: yearInteger.
]

{ #category : 'general inquiries' }
Date class >> easterDateFor: year [

 "  compute the easter date.
    source: Physikalisch-Technische Bundesanstalt Braunschweig.
    Lichtenberg, H.: Zur Interpretation der Gaussschen Osterformel
                     und ihrer Ausnahmeregeln,
                     Historia Mathematica 24 (1997), pp. 441-444
  
    http://www.ptb.de/de/org/4/44/441/oste.htm
  "

  | k m s a d r og sz oe day |

  k := year // 100.
  m := 15 + (3*k + 3//4) - (8*k + 13//25).
   s := 2 - (3*k + 3// 4).
  a := year \\ 19.
  d := 19*a + m \\ 30.
  r := d//29 + ((d//28) - (d//29)* (a// 11)).

  og := 21 + d - r.
  sz := 7 - (year//4 + year + s\\7).
  oe := 7 - (og - sz\\7).
  day := og + oe.
  ^day <= 31
    ifTrue: [Date newDay: day month: 3 year: year ]
    ifFalse: [Date newDay: day - 31 month: 4 year: year].
]

{ #category : 'smalltalk-80' }
Date class >> firstWeekdayOfMonth: month year: year [
	"Answer the weekday index of the first day in <month> in the <year>."

	^ (self newDay: 1 month: month year: year) weekdayIndex

]

{ #category : 'smalltalk-80' }
Date class >> fromDays: dayCount  [
	"Days since 1 January 1901"

	^ self julianDayNumber: dayCount + Time squeakEpoch
]

{ #category : 'smalltalk-80' }
Date class >> fromSeconds: seconds [
	"Answer an instance of me which is 'seconds' seconds after January 1, 1901."

	^ self fromDays: ((Duration seconds: seconds) days)
]

{ #category : 'instance creation' }
Date class >> includingDateAndTime: aDateAndTime [

	^self basicNew
 		start: aDateAndTime midnight;
		duration: (Duration days: 1);
		yourself
]

{ #category : 'smalltalk-80' }
Date class >> indexOfMonth: aMonthName  [

	^ Month indexOfMonth: aMonthName.

]

{ #category : 'squeak protocol' }
Date class >> julianDayNumber: aJulianDayNumber [

	^ (DateAndTime julianDayNumber: aJulianDayNumber) date
]

{ #category : 'smalltalk-80' }
Date class >> leapYear: yearInteger  [

	^ Year leapYear: yearInteger
]

{ #category : 'inquiries' }
Date class >> mustEncompassWholeDates [

	^true
]

{ #category : 'smalltalk-80' }
Date class >> nameOfDay: dayIndex  [

	^ Week nameOfDay: dayIndex 
]

{ #category : 'smalltalk-80' }
Date class >> nameOfMonth: anIndex  [

	^ Month nameOfMonth: anIndex.

]

{ #category : 'smalltalk-80' }
Date class >> newDay: day month: month year: year  [

	^ self year: year month: month day: day
]

{ #category : 'smalltalk-80' }
Date class >> newDay: dayCount year: yearInteger [

	^ self year: yearInteger day: dayCount
]

{ #category : 'general inquiries' }
Date class >> orthodoxEasterDateFor: year [

 "  compute the easter date according to the rules of the orthodox calendar.
    source: 
    http://www.smart.net/~mmontes/ortheast.html 
  "
     | r1 r2 r3 r4 ra rb r5 rc date |

    r1 := year \\ 19.
    r2 := year \\ 4.
    r3 := year \\ 7.
    ra := 19*r1 + 16.
    r4 := ra \\ 30.
    rb := r2 + r2 + (4*r3) + (6*r4).
    r5 := rb \\ 7.
    rc := r4 + r5.
    date := Date newDay: 3 month: 4 year: year.
    ^date + rc days
]

{ #category : 'squeak protocol' }
Date class >> readFrom: aStream  [
	"Read a Date from the stream in any of the forms:
		<day> <month> <year>		(15 April 1982; 15-APR-82; 15.4.82; 15APR82)  
		<month> <day> <year>		(April 15, 1982; 4/15/82)
		<year>-<month>-<day>		(1982-04-15) (ISO8601)"
	| day month year firstAsNumber firstAsNumberSign |
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

	^ self year: year month: month day: day
]

{ #category : 'smalltalk-80' }
Date class >> today [

	^ DateAndTime now date
]

{ #category : 'squeak protocol' }
Date class >> tomorrow [

	^ self today next
]

{ #category : 'squeak protocol' }
Date class >> year: year day: dayOfYear [

	^ (DateAndTime year: year day: dayOfYear) date
]

{ #category : 'squeak protocol' }
Date class >> year: year month: month day: day [

	^ (DateAndTime year: year month: month day: day) date
]

{ #category : 'squeak protocol' }
Date class >> yesterday [

	^ self today previous
]

{ #category : 'squeak protocol' }
Date >> < aDate [

	self assert: aDate class == Date.
	^ self start < aDate start
]

{ #category : 'squeak protocol' }
Date >> > aDate [

	self assert: aDate class == Date.
	^ self start > aDate start
]

{ #category : 'squeak protocol' }
Date >> dayMonthYearDo: aBlock  [
	"Supply integers for day, month and year to aBlock and return the result"

	^ start dayMonthYearDo: aBlock
]

{ #category : 'public protocol' }
Date >> dayOfMonth [
	"Answer the day of the month represented by the receiver."

	^ start dayOfMonth
]

{ #category : 'public protocol' }
Date >> dayOfWeek [
	"Answer the day of the week represented by the receiver."

	^ start dayOfWeek
]

{ #category : 'public protocol' }
Date >> dayOfWeekName [
	"Answer the day of the week represented by the receiver."

	^ start dayOfWeekName
]

{ #category : 'public protocol' }
Date >> dayOfYear [
	"Answer the day of the year represented by the receiver."

	^ start dayOfYear
]

{ #category : 'public protocol' }
Date >> daysInMonth [


	^ start daysInMonth
]

{ #category : 'public protocol' }
Date >> daysInYear [
	"Answer the number of days in the month represented by the receiver."

	^ start daysInYear
]

{ #category : 'public protocol' }
Date >> daysLeftInYear [
	^ start daysLeftInYear
]

{ #category : 'public protocol' }
Date >> firstDayOfMonth [

	^ start firstDayOfMonth
]

{ #category : 'public protocol' }
Date >> isLeapYear [

	^ start isLeapYear
]

{ #category : 'public protocol' }
Date >> julianDayNumber [

	^ start julianDayNumber
]

{ #category : 'printing' }
Date >> mmddyyyy [
	"Answer the receiver rendered in standard fmt mm/dd/yyyy.  Good for avoiding year 2000 bugs.  Note that the name here is slightly misleading -- the month and day numbers don't show leading zeros, so that for example feb 1 1996 is 2/1/96"

	"Date today mmddyyyy"

	^ self printFormat: #(2 1 3 $/ 1 1)
]

{ #category : 'public protocol' }
Date >> month [

	^start month
]

{ #category : 'public protocol' }
Date >> monthAbbreviation [

	^ start monthAbbreviation
]

{ #category : 'public protocol' }
Date >> monthIndex [

	^ start monthIndex
]

{ #category : 'public protocol' }
Date >> monthName [

	^ start monthName
]

{ #category : 'smalltalk-80' }
Date >> previous: dayName  [
	"Answer the previous date whose weekday name is dayName."

	| n |
	n := 7 + self weekdayIndex - (self class dayOfWeek: dayName) \\ 7.
	n = 0 ifTrue: [ n := 7 ].
	^ self - n days
]

{ #category : 'printing' }
Date >> printFormat: formatArray  [
	"Answer a String describing the receiver using the format denoted by the 
	argument, formatArray."

	^ String streamContents: [ :strm |
		self printOn: strm format: formatArray].
]

{ #category : 'printing' }
Date >> printOn: aStream [

	self printOn: aStream format: #(1 2 3 $  3 1 )
]

{ #category : 'printing' }
Date >> printOn: aStream format: formatArray  [
	"Print a description of the receiver on aStream using the format 
	denoted the argument, formatArray: 
	
		#(item item item sep monthfmt yearfmt twoDigits) 
	
		items: 1=day 2=month 3=year will appear in the order given, 
	
		separated by sep which is eaither an ascii code or character. 
	
		monthFmt: 1=09 2=Sep 3=September 
	
		yearFmt: 1=1996 2=96 
	
		digits: (missing or)1=9 2=09. 
	
	See the examples in printOn: and mmddyy"
	| gregorian twoDigits element monthFormat |
	gregorian := self dayMonthYearDo: [ :d :m :y | {d. m. y} ].
	twoDigits := formatArray size > 6 and: [(formatArray at: 7) > 1].
	1 to: 3 do: 
		[ :i | 
			element := formatArray at: i.
			element = 1
				ifTrue: [twoDigits
						ifTrue: [aStream
								nextPutAll: (gregorian first printString
										padded: #left
										to: 2
										with: $0)]
						ifFalse: [gregorian first printOn: aStream]].
			element = 2
				ifTrue: [monthFormat := formatArray at: 5.
					monthFormat = 1
						ifTrue: [twoDigits
								ifTrue: [aStream
										nextPutAll: (gregorian middle printString
												padded: #left
												to: 2
												with: $0)]
								ifFalse: [gregorian middle printOn: aStream]].
					monthFormat = 2
						ifTrue: [aStream
								nextPutAll: ((Month nameOfMonth: gregorian middle)
										copyFrom: 1
										to: 3)].
					monthFormat = 3
						ifTrue: [aStream
								nextPutAll: (Month nameOfMonth: gregorian middle)]].
			element = 3
				ifTrue: [(formatArray at: 6)
							= 1
						ifTrue: [gregorian last printOn: aStream]
						ifFalse: [aStream
								nextPutAll: ((gregorian last \\ 100) printString
										padded: #left
										to: 2
										with: $0)]].
			i < 3
				ifTrue: [(formatArray at: 4)
							~= 0
						ifTrue: [
							aStream nextPut: ((formatArray at: 4) isNumber
								ifTrue: [ Character codePoint: (formatArray at: 4) ]
								ifFalse: [ (formatArray at: 4) ])
							]]]

]

{ #category : 'public protocol' }
Date >> secondsSinceSqueakEpoch [
	"Answer the seconds since the Squeak epoch: 1 January 1901"

	^ start secondsSinceSqueakEpoch
]

{ #category : 'printing' }
Date >> storeOn: aStream [

	aStream print: self printString; nextPutAll: ' asDate'
]

{ #category : 'public protocol' }
Date >> week [

	^start week
]

{ #category : 'smalltalk-80' }
Date >> weekday [
	"Answer the name of the day of the week on which the receiver falls."

	^ self dayOfWeekName
]

{ #category : 'smalltalk-80' }
Date >> weekdayIndex [
	"Sunday=1, ... , Saturday=7
	Monday=1, ... , Sunday=7
	'12 May 2017 ' asDate weekdayIndex = 5
	"

	^ self dayOfWeek
]

{ #category : 'public protocol' }
Date >> year [

	^start year
]

{ #category : 'public protocol' }
Date >> yearNumber [

	^ start yearNumber
]

{ #category : 'printing' }
Date >> yyyymmdd [
	"Format the date in ISO 8601 standard like '2002-10-22'."

	^ self printFormat: #(3 2 1 $- 1 1 2)
]
