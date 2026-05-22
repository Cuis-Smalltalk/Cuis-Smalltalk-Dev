"
I represent a point in UTC time as defined by ISO 8601. I have zero duration.


My implementation uses three SmallIntegers
 and a Duration:
jdn		- julian day number.
seconds	- number of seconds since midnight.
nanos	- the number of nanoseconds since the second.

offset	- duration from UTC.

The nanosecond attribute is almost always zero but it defined for full ISO compliance and is suitable for timestamping.

"
Class {
	#name : 'DateAndTime',
	#superclass : 'Magnitude',
	#instVars : [
		'seconds',
		'offset',
		'jdn',
		'nanos'
	],
	#classVars : [
		'LastTick',
		'LastTickSemaphore',
		'UtcOffsetInSeconds'
	],
	#category : 'Kernel-Chronology',
	#package : 'Kernel',
	#tag : 'Chronology'
}

{ #category : 'ansi protocol' }
DateAndTime class >> clockPrecision [
	"One nanosecond precision"

	^ Duration seconds: 0 nanoSeconds: 1

]

{ #category : 'squeak protocol' }
DateAndTime class >> date: aDate time: aTimeOrNil [

	aTimeOrNil ifNil: [
		^ self 
			year: aDate yearNumber
			day: aDate dayOfYear
			hour: 0
			minute: 0
			second: 0 ].
		
	^ self 
		year: aDate yearNumber
		day: aDate dayOfYear
		hour: aTimeOrNil hour
		minute: aTimeOrNil minute
		second: aTimeOrNil second
]

{ #category : 'squeak protocol' }
DateAndTime class >> epoch [
	"Answer a DateAndTime representing the Squeak epoch: 1 January 1901"

	^ self julianDayNumber: Time squeakEpoch
]

{ #category : 'smalltalk-80' }
DateAndTime class >> fromSeconds: seconds  [
	"Answer a DateAndTime since the Squeak epoch: 1 January 1901"

	| integerSeconds nanos |
	integerSeconds := seconds truncated.
	nanos := integerSeconds = seconds
		ifTrue: [ 0 ]
		ifFalse: [ (seconds - integerSeconds * Time nanosInSecond) asInteger ].
	^ self
		julianDayNumber: Time squeakEpoch
		seconds: integerSeconds
		nanoseconds: nanos
		offset: self localOffset
]

{ #category : 'squeak protocol' }
DateAndTime class >> fromString: aString [


	^ self readFrom: aString readStream.
]

{ #category : 'system startup & shutdown' }
DateAndTime class >> initClassCachedState [

	LastTickSemaphore := Semaphore forMutualExclusion.
	LastTick := 0.
	UtcOffsetInSeconds := 0.
]

{ #category : 'initialization' }
DateAndTime class >> initialize [
	"
	self initialize
	"
	self initClassCachedState
]

{ #category : 'squeak protocol' }
DateAndTime class >> julianDayNumber: aJulianDayNumber [

	^ self
		julianDayNumber: aJulianDayNumber
		seconds: 0
		nanoseconds: 0
		offset: self localOffset
]

{ #category : 'instance creation' }
DateAndTime class >> julianDayNumber: day seconds: secs nanoseconds: nsecs offset: utcOffset [

	^self basicNew julianDayNumber: day seconds: secs nanoseconds: nsecs offset: utcOffset
]

{ #category : 'squeak protocol' }
DateAndTime class >> localOffset [
	"Answer the duration we are offset from UTC"

	^ self now offset
]

{ #category : 'squeak protocol' }
DateAndTime class >> midnight [

	^ self now midnight

]

{ #category : 'instance creation' }
DateAndTime class >> nanosecondsTick [
	"Nanoseconds since UTC Posix epoch. In UTC. Independent of local time.
	Use highest resolution possible.
	If called several times, always answer different, increasing values. This means that they can used as Numeric Identifiers.
	Answer is a LargePositiveInteger.
	
	DateAndTime nanosecondsTick
	"
	self updateLastTickAndOffset.
	^LastTick
]

{ #category : 'squeak protocol' }
DateAndTime class >> new [
	"Answer a DateAndTime representing the Squeak epoch: 1 January 1901"

	^ self epoch
	
]

{ #category : 'squeak protocol' }
DateAndTime class >> noon [

	^ self now noon
]

{ #category : 'instance creation' }
DateAndTime class >> now [
	"Use highest resolution possible.
	If called several times, always answer different, increasing values. This means that they can used as TimeStamps.
	If, instead, a numeric unique identifier is desired, call #nanosecondsTick
	DateAndTime now
	"

	| nanoseconds utcOffset days remainingNanoseconds remainingSeconds |
	self updateLastTickAndOffset.

	utcOffset := Duration seconds: UtcOffsetInSeconds.
	nanoseconds := UtcOffsetInSeconds * 1e9 + LastTick.
	days := nanoseconds // Time nanosecondsInDay.
	remainingNanoseconds := nanoseconds \\ Time nanosecondsInDay.
	remainingSeconds := remainingNanoseconds // 1e9.
	remainingNanoseconds := remainingNanoseconds \\ 1e9.

	^ self basicNew
		setJdn: `DateAndTime unixEpoch julianDayNumber` + days
		seconds: remainingSeconds 
		nano: remainingNanoseconds 
		offset: utcOffset
]

{ #category : 'instance creation' }
DateAndTime class >> nowUpToSeconds [
	"Resolution is up to one second. Don't use as a TimeStamp!
	DateAndTime nowUpToSeconds
	"

	^self now truncateToSeconds
]

{ #category : 'squeak protocol' }
DateAndTime class >> readFrom: aStream [

	| offset date time ch |

	date := Date readFrom: aStream.
	[aStream peek isDigit]
		whileFalse: [aStream next].
	time := Time readFrom: aStream.
	aStream atEnd
		ifTrue: [ offset := self localOffset ]
		ifFalse: [
			ch := aStream next.
			ch = $Z
				ifTrue: [ offset := Duration zero]
				ifFalse: [
					ch == $+ ifTrue: [ch := Character space].
					"Assume that offset is defined as (+/-)hh:mm
					Add (zero days): as prefix and :(0 seconds) as suffix."
					offset := Duration fromString: (String with: ch), '0:', aStream upToEnd, ':0' ]].
		
	^ self
		year: date yearNumber
		month: date monthIndex
		day: date dayOfMonth
		hour: time hour
		minute: time minute
		second: time second
		nanoSecond: time nanoSecond
		offset: offset


	"
		DateAndTime readFrom: ' 2002-05-16T17:20:45.1Z' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.1+00:00' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.1+02:00' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.1-03:00' readStream
		DateAndTime readFrom: '-1199-01-05T20:33:14.321-05:00' readStream

		DateAndTime readFrom: ' 2002-05-16T17:20:45.02+01:01' readStream

		DateAndTime readFrom: ' 2002-05-16T17:20:45.003+01:01' readStream

		DateAndTime readFrom: ' 2002-05-16T17:20:45.0004+01:01' readStream
  		DateAndTime readFrom: ' 2002-05-16T17:20:45.00005' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.000006+01:01' readStream

		DateAndTime readFrom: ' 2002-05-16T17:20:45.0000007+01:01' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.00000008-01:01' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.000000009+01:01' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.0000000001+01:01' readStream

 		DateAndTime readFrom: ' 2002-05-16T17:20' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45+01:57' readStream
 		DateAndTime readFrom: ' 2002-05-16T17:20:45-02:34' readStream
 		DateAndTime readFrom: ' 2002-05-16T17:20:45+00:00' readStream
		DateAndTime readFrom: ' 1997-04-26T01:02:03+01:02:3' readStream
 	"
]

{ #category : 'system startup & shutdown' }
DateAndTime class >> releaseClassCachedState [

	LastTickSemaphore := nil.
	LastTick := nil.
	UtcOffsetInSeconds := nil.
]

{ #category : 'squeak protocol' }
DateAndTime class >> today [

	^ self midnight

]

{ #category : 'squeak protocol' }
DateAndTime class >> tomorrow [

	^ self today date next start
]

{ #category : 'constants' }
DateAndTime class >> unixEpoch [
	"
	DateAndTime unixEpoch
	1970-01-01T00:00:00+00:00
	
	(DateAndTime now - DateAndTime unixEpoch) days / 365.25
	"
	^ self
		julianDayNumber: 2440588
		seconds: 0
		nanoseconds: 0
		offset: `Duration zero`.
]

{ #category : 'instance creation' }
DateAndTime class >> unixTimeSeconds: secsSinceUnixEpoch [
	"
	DateAndTime unixEpoch
	1970-01-01T00:00:00+00:00
	
	DateAndTime unixTimeSeconds: DateAndTime now unixTimeSeconds
	"
	^ self unixEpoch + secsSinceUnixEpoch seconds
]

{ #category : 'instance creation' }
DateAndTime class >> unixTimeSeconds: secsSinceUnixEpoch milliseconds: millisecs [
	"
	DateAndTime unixEpoch
	1970-01-01T00:00:00+00:00
	
	DateAndTime unixTimeSeconds: DateAndTime now unixTimeSeconds
	"
	^ self unixEpoch + secsSinceUnixEpoch seconds + millisecs milliSeconds
]

{ #category : 'primitive access' }
DateAndTime class >> updateLastTickAndOffset [
	"
	LastTick:
		number of microseconds since the UTC Posix (Unix) epoch, 
		i.e. 00:00 on the morning of January 1, 1970, in UTC time.
	UtcOffsetInSeconds:
		Local time offset from UTC, in seconds.
	"
	| nanoseconds utcMicroSsecondsAndOffset |
	utcMicroSsecondsAndOffset := Time primUtcWithOffset: (Array new: 2).
	nanoseconds := utcMicroSsecondsAndOffset first * 1000.
	LastTick = nanoseconds
		ifFalse: [
			LastTick := nanoseconds]
		ifTrue: [
			LastTickSemaphore critical: [
				LastTick :=  LastTick + 1.
				nanoseconds := LastTick ]].
	UtcOffsetInSeconds := utcMicroSsecondsAndOffset second.
]

{ #category : 'squeak protocol' }
DateAndTime class >> year: year day: dayOfYear [
	"Return a DateAndTime"

	^ self
		year: year
		day: dayOfYear
		hour: 0
		minute: 0
		second: 0
]

{ #category : 'ansi protocol' }
DateAndTime class >> year: year day: dayOfYear hour: hour minute: minute second: second [

	^ self
		year: year
		day: dayOfYear
		hour: hour
		minute: minute
		second: second
		offset: self localOffset.

]

{ #category : 'ansi protocol' }
DateAndTime class >> year: year day: dayOfYear hour: hour minute: minute second: second offset: offset  [
	"Return a DataAndTime"

	| y d |
	y := self
		year: year
		month: 1
		day: 1
		hour: hour
		minute: minute
		second: second
		nanoSecond: 0
		offset: offset.

	d := Duration days: (dayOfYear - 1).

	^ y + d
]

{ #category : 'squeak protocol' }
DateAndTime class >> year: year month: month day: day [
	"Return a DateAndTime, midnight local time" 	^ self
 		year: year
 		month: month
 		day: day
 		hour: 0
		minute: 0
]

{ #category : 'squeak protocol' }
DateAndTime class >> year: year month: month day: day hour: hour minute: minute [

	"Return a DateAndTime" 	^ self
 		year: year
 		month: month
 		day: day
 		hour: hour
		minute: minute
		second: 0
]

{ #category : 'ansi protocol' }
DateAndTime class >> year: year month: month day: day hour: hour minute: minute second: second [
	"Return a DateAndTime"

	^ self
		year: year
		month: month
		day: day
		hour: hour
		minute: minute
		second: second
		offset: self localOffset

]

{ #category : 'squeak protocol' }
DateAndTime class >> year: year month: month day: day hour: hour minute: minute second: second nanoSecond: nanoCount offset: offset [
	"Return a DateAndTime"

	| monthIndex daysInMonth p q r s julianDayNumber |

	monthIndex := month isInteger ifTrue: [month] ifFalse: [Month indexOfMonth: month].
	daysInMonth := Month
		daysInMonth: monthIndex
		forYear: year.
	day < 1 ifTrue: [self error: 'day may not be zero or negative'].
	day > daysInMonth ifTrue: [self error: 'day is after month ends']. 	
	
	p := (monthIndex - 14) quo: 12.
	q := year + 4800 + p.
	r := monthIndex - 2 - (12 * p).
	s := (year + 4900 + p) quo: 100.

	julianDayNumber :=
 		( (1461 * q) quo: 4 ) +
			( (367 * r) quo: 12 ) -
 				( (3 * s) quo: 4 ) +
 					( day - 32075 ).

	^self basicNew
		setJdn: julianDayNumber 
		seconds: hour * 60 + minute * 60 + second
		nano: nanoCount
		offset: offset;
		yourself
]

{ #category : 'ansi protocol' }
DateAndTime class >> year: year month: month day: day hour: hour minute: minute second: second offset: offset [

	^ self
		year: year
		month: month
		day: day
		hour: hour
		minute: minute
		second: second
		nanoSecond: 0
		offset: offset

]

{ #category : 'squeak protocol' }
DateAndTime class >> yesterday [

	^ self today date previous start
]

{ #category : 'ansi protocol' }
DateAndTime >> + operand [
	"operand conforms to protocol Duration"

	self assert: operand class == Duration.

	^ self class
		julianDayNumber: jdn
		seconds: seconds + operand totalSeconds
		nanoseconds: nanos + operand nanoSeconds
		offset: offset
]

{ #category : 'ansi protocol' }
DateAndTime >> - operand [
	"operand is a DateAndTime or a Duration.
	Double dispatch"

	^ operand subtractFrom: self
]

{ #category : 'ansi protocol' }
DateAndTime >> < comparand [
	"comparand is a DateAndTime."
	| lvalue rvalue |
	self assert: (comparand is: #dateAndTime).
	offset = comparand offset
		ifTrue: [
			lvalue := self.
			rvalue := comparand ]
		ifFalse: [
			lvalue := self asUTC.
			rvalue := comparand asUTC ].
	^ lvalue julianDayNumber < rvalue julianDayNumber
		or: [
			lvalue julianDayNumber > rvalue julianDayNumber
				ifTrue: [ false ]
				ifFalse: [
					lvalue secondsSinceMidnight < rvalue secondsSinceMidnight
						or: [
							lvalue secondsSinceMidnight > rvalue secondsSinceMidnight
								ifTrue: [ false ]
								ifFalse: [ lvalue nanoSecond < rvalue nanoSecond ]]]]
]

{ #category : 'ansi protocol' }
DateAndTime >> = comparand [
	"comparand is a DateAndTime."
	| lvalue rvalue |

	"Any object is equal to itself"
	self == comparand ifTrue: [ ^ true ].

	self class == comparand class ifFalse: [ ^false ].

	offset = comparand offset
		ifTrue: [
			lvalue := self.
			rvalue := comparand ]
		ifFalse: [
			lvalue := self asUTC.
			rvalue := comparand asUTC ].
	^ lvalue julianDayNumber = rvalue julianDayNumber
		and: [ lvalue secondsSinceMidnight = rvalue secondsSinceMidnight
			and: [ lvalue nanoSecond = rvalue nanoSecond ] ]
]

{ #category : 'ansi protocol' }
DateAndTime >> asUTC [

	^ offset isZero
		ifTrue: [self]
		ifFalse: [self utcOffset: 0 hours]

]

{ #category : 'public protocol' }
DateAndTime >> date [

	^Date including: self
]

{ #category : 'squeak protocol' }
DateAndTime >> dayMonthYearDo: aBlock [
	"Evaluation the block with three arguments: day month, year."

	| l n i j dd mm yyyy |
	l := jdn + 68569.
	n := 4 * l // 146097.
	l := l - (146097 * n + 3 // 4).
	i := 4000 * (l + 1) // 1461001.
	l := l - (1461 * i // 4) + 31.
	j := 80 * l // 2447.
	dd := l - (2447 * j // 80).
	l := j // 11.
	mm := j + 2 - (12 * l).
	yyyy := 100 * (n - 49) + i + l.

	^ aBlock
		value: dd
		value: mm
		value: yyyy.
]

{ #category : 'ansi protocol' }
DateAndTime >> dayOfMonth [
	"Answer which day of the month is represented by the receiver."

	^ self
		dayMonthYearDo: [ :d :m :y | d ]
]

{ #category : 'ansi protocol' }
DateAndTime >> dayOfWeek [

	"
	Sunday=1, ... , Saturday=7
	Monday=1, ... , Sunday=7
	'12 May 2017 ' asDate dayOfWeek = 5
	"

	^ (jdn rem: 7) + 1
]

{ #category : 'ansi protocol' }
DateAndTime >> dayOfWeekAbbreviation [

	^ self dayOfWeekName copyFrom: 1 to: 3
]

{ #category : 'ansi protocol' }
DateAndTime >> dayOfWeekName [
	"
	'12 May 2017 ' asDate dayOfWeek = 5 
	'12 May 2017 ' asDate dayOfWeekName = #Friday 
	"

	^ Week nameOfDay: self dayOfWeek

]

{ #category : 'ansi protocol' }
DateAndTime >> dayOfYear [
	"This code was contributed by Dan Ingalls. It is equivalent to the terser
		^ jdn - (Year year: self year) start julianDayNumber + 1 but much quicker."

	^ self dayMonthYearDo:
		[ :d :m :y | | monthStart |
			monthStart := #(1 32 60 91 121 152 182 213 244 274 305 335) at: m.
			(m > 2 and: [ Year isLeapYear: y ])
				ifTrue: [ monthStart + d ]
				ifFalse: [ monthStart + d - 1 ]]
]

{ #category : 'smalltalk-80' }
DateAndTime >> daysInMonth [
	"Answer the number of days in the month represented by the receiver."


	^ self month daysInMonth
]

{ #category : 'smalltalk-80' }
DateAndTime >> daysInYear [

	"Answer the number of days in the year represented by the receiver."

	^ (Year including: self) daysInYear
]

{ #category : 'smalltalk-80' }
DateAndTime >> daysLeftInYear [
	"Answer the number of days in the year after the date of the receiver."

	^ self daysInYear - self dayOfYear

]

{ #category : 'public protocol' }
DateAndTime >> filenamishPrintOn: aStream [
	"
	String streamContents: [ :strm | DateAndTime now filenamishPrintOn: strm ] '2025Aug18-16h46m' .
	"
	
	| year month day monthName |
	self dayMonthYearDo: [ :d :m :y | year := y. month := m. day := d ].
	year negative
		ifTrue: [ aStream nextPut: $- ].
	monthName := Month nameOfMonth: month.
	aStream
		nextPutAll: (year abs printString padded: #left to: 4 with: $0);
		nextPutAll: (monthName copyFrom: 1 to: 3);
		nextPutAll: (day printString padded: #left to: 2 with: $0);
		nextPut: $-;
		nextPutAll: (self hour printString padded: #left to: 2 with: $0);
		nextPut: $h;
		nextPutAll: (self minute printString padded: #left to: 2 with: $0);
		nextPut: $m
]

{ #category : 'smalltalk-80' }
DateAndTime >> firstDayOfMonth [

	^ self month start dayOfYear
]

{ #category : 'ansi protocol' }
DateAndTime >> hash [

	^(jdn bitXor: seconds) bitXor: nanos
]

{ #category : 'ansi protocol' }
DateAndTime >> hour [

	^ self hour24

]

{ #category : 'ansi protocol' }
DateAndTime >> hour12 [
	"Answer an <integer> between 1 and 12, inclusive, representing the hour 
	of the day in the 12-hour clock of the local time of the receiver."
	^ self hour24 - 1 \\ 12 + 1
]

{ #category : 'ansi protocol' }
DateAndTime >> hour24 [


	^ (Duration seconds: seconds) hours

]

{ #category : 'double dispatching' }
DateAndTime >> includingTimespanOf: aTimespanClass [

	^ aTimespanClass includingDateAndTime: self
]

{ #category : 'testing' }
DateAndTime >> is: aSymbol [
	"Allow #dateAndTime as argument (see below *)
	Also keep #DateAndTime as valid, as there may be code using it.
	* Using #DateAndTime gives problems in the unlikely case of renaming
	the DateAndTime class to something else. The reason is that this very
	method is heavily used during the rename, to timestamp updated methods.
	If the base image uses a different Symbol, like #dateAndTime, ir works OK.
	"
	^ aSymbol == #DateAndTime or: [
		aSymbol == #dateAndTime or: [
			super is: aSymbol ]].
]

{ #category : 'ansi protocol' }
DateAndTime >> isLeapYear [


	^ Year isLeapYear: self yearNumber
]

{ #category : 'testing' }
DateAndTime >> isUTC [
	^offset isZero
]

{ #category : 'squeak protocol' }
DateAndTime >> julianDayNumber [


	^ jdn

]

{ #category : 'private' }
DateAndTime >> julianDayNumber: day seconds: secs nanoseconds: nsecs offset: utcOffset [
	"secs could span more than one day. nsecs could span more than one second."
	
	| daysOverflow nanosSinceSec secsOverflow secsSinceMidnight totalSecs totalDays |
	"Add to totalSecs any whole seconds in nsecs"
	secsOverflow := nsecs // Time nanosInSecond.
	nanosSinceSec := nsecs - (secsOverflow * Time nanosInSecond).
	totalSecs := secs + secsOverflow.

	"Add to totalDays any whole days in totalSecs"
	daysOverflow := totalSecs // Time secondsInDay.
	secsSinceMidnight := totalSecs - (daysOverflow * Time secondsInDay).
	totalDays := day + daysOverflow.

	jdn := totalDays.
	seconds := secsSinceMidnight.
	nanos := nanosSinceSec.
	offset := utcOffset
]

{ #category : 'ansi protocol' }
DateAndTime >> meridianAbbreviation [

	^ self time meridianAbbreviation
]

{ #category : 'squeak protocol' }
DateAndTime >> midnight [
	"Answer a DateAndTime starting at midnight local time"

	^self class basicNew
		setJdn: jdn
		seconds: 0
		nano: 0
		offset: self class localOffset

]

{ #category : 'ansi protocol' }
DateAndTime >> minute [


	^ (Duration seconds: seconds) minutes

]

{ #category : 'public protocol' }
DateAndTime >> month [

	^Month including: self
]

{ #category : 'ansi protocol' }
DateAndTime >> monthAbbreviation [


	^ self monthName copyFrom: 1 to: 3

]

{ #category : 'public protocol' }
DateAndTime >> monthIndex [
	^ self dayMonthYearDo:
		[ : d : m : y |  m ]
]

{ #category : 'ansi protocol' }
DateAndTime >> monthName [


	^ Month nameOfMonth: self monthIndex

]

{ #category : 'squeak protocol' }
DateAndTime >> nanoSecond [


	^ nanos

]

{ #category : 'squeak protocol' }
DateAndTime >> noon [
	"Answer a DateAndTime starting at noon"

	^ self dayMonthYearDo: [ :d :m :y |
		self class year: y month: m day: d hour: 12 minute: 0 second: 0 ]
]

{ #category : 'ansi protocol' }
DateAndTime >> offset [

	^ offset

]

{ #category : 'squeak protocol' }
DateAndTime >> printHMSOn: aStream [
	"Print just hh:mm:ss"
	self printHMSOn: aStream separator: $:
]

{ #category : 'squeak protocol' }
DateAndTime >> printHMSOn: aStream separator: aChar [
	"Print just hh:mm:ss"
	aStream
		nextPutAll: (self hour printString padded: #left to: 2 with: $0);
		nextPut: aChar;
		nextPutAll: (self minute printString padded: #left to: 2 with: $0);
		nextPut: aChar;
		nextPutAll: (self second printString padded: #left to: 2 with: $0).

]

{ #category : 'squeak protocol' }
DateAndTime >> printOn: aStream [
	"Print as per ISO 8601 section 5.4.2
	Prints either:
		'YYYY-MM-DDThh:mm:ss.sZ' (for positive years, UTC, i.e. zero offset)
		'YYYY-MM-DDThh:mm:ss.s+OO:oo' (for positive years, positive offset)
		'YYYY-MM-DDThh:mm:ss.s-OO:oo' (for positive years, negative offset)
		'-YYYY-MM-DDThh:mm:ss.s+OO:oo' (for negative years, offset as above)
	"

	^self printOn: aStream includeOffset: true
]

{ #category : 'squeak protocol' }
DateAndTime >> printOn: aStream includeOffset: aBoolean [
	"Print as per ISO 8601 section 5.4.2
	Prints either:
		'YYYY-MM-DDThh:mm:ss.sZ' (for positive years, UTC, i.e. zero offset)
		'YYYY-MM-DDThh:mm:ss.s+OO:oo' (for positive years, positive offset)
		'YYYY-MM-DDThh:mm:ss.s-OO:oo' (for positive years, negative offset)
		'-YYYY-MM-DDThh:mm:ss.s+OO:oo' (for negative years, offset as above)
	Does not include the offset at all if aBoolean == false.
	"

	self printYMDOn: aStream.
	aStream nextPut: $T.
	self printHMSOn: aStream.
	self nanoSecond ~= 0 ifTrue:
		[ | z ps |
		ps := self nanoSecond printString padded: #left to: 9 with: $0.
		z := ps findLast: [ :c | c codePoint > `$0 codePoint` ].
		(z > 0) ifTrue: [aStream nextPut: $.].
		ps from: 1 to: z do: [ :c | aStream nextPut: c ] ].
	aBoolean ifTrue: [
		self isUTC
			ifTrue: [ aStream nextPut: $Z]
			ifFalse: [
				aStream
					nextPut: (offset positive ifTrue: [$+] ifFalse: [$-]);
					nextPutAll: (offset hours abs printString padded: #left to: 2 with: $0);
					nextPut: $:;
					nextPutAll: (offset minutes abs printString padded: #left to: 2 with: $0).
				"Note: We should check the ISO to see if the seconds part is supported by the standard
				(Usually not an issue. UTC Offsets are multiple of 15 minutes.)"
				offset seconds = 0 ifFalse: [
					aStream
						nextPut: $:;
						nextPutAll: (offset seconds abs truncated printString) ]]].
]

{ #category : 'public protocol' }
DateAndTime >> printStringWithoutOffset [
	"Print as per ISO 8601 section 5.4.2
	Prints either:
		'YYYY-MM-DDThh:mm:ss.s' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s' (for negative years)"

	^String streamContents: [ :strm | self printWithoutOffsetOn: strm ]
]

{ #category : 'squeak protocol' }
DateAndTime >> printWithMsOn: aStream [
	"Print with millisecond resolution, no offset."

	| ps |
	self printYMDOn: aStream.
	aStream nextPut: $T.
	self printHMSOn: aStream.
	ps := (self nanoSecond // 1000000) printString padded: #left to: 3 with: $0.
	aStream nextPut: $..
	aStream nextPutAll: ps
]

{ #category : 'public protocol' }
DateAndTime >> printWithoutOffsetOn: aStream [
	"Print as per ISO 8601 section 5.4.2
	Prints either:
		'YYYY-MM-DDThh:mm:ss.s' (for positive years)
		'-YYYY-MM-DDThh:mm:ss.s' (for negative years)
	"

	^self printOn: aStream includeOffset: false
]

{ #category : 'squeak protocol' }
DateAndTime >> printYMDOn: aStream [
	"Print just the year, month, and day on aStream.
	
	Print just YYYY-MM-DD part.
	If the year is negative, prints out '-YYYY-MM-DD'."

	| year month day |
	self dayMonthYearDo: [ :d :m :y | year := y. month := m. day := d ].
	year negative
		ifTrue: [ aStream nextPut: $- ].
	aStream
		nextPutAll: (year abs printString padded: #left to: 4 with: $0);
		nextPut: $-;
		nextPutAll: (month printString padded: #left to: 2 with: $0);
		nextPut: $-;
		nextPutAll: (day printString padded: #left to: 2 with: $0)

]

{ #category : 'ansi protocol' }
DateAndTime >> second [


	^ (Duration seconds: seconds) seconds

]

{ #category : 'private' }
DateAndTime >> secondsSinceMidnight [

	^ seconds
]

{ #category : 'public protocol' }
DateAndTime >> secondsSinceSqueakEpoch [
	"Return the number of seconds since the Squeak epoch"

	^ (self - (self class epoch)) totalSeconds
]

{ #category : 'squeak protocol' }
DateAndTime >> setJdn: j seconds: s nano: n offset: o [

jdn := j.
seconds := s.
nanos :=  n.
offset :=  o

]

{ #category : 'private' }
DateAndTime >> subtractDateAndtime: operand [
	"operand is a DateAndTime or a Duration"

	| lvalue rvalue |
	offset = operand offset
		ifTrue: [
			lvalue := self.
			rvalue := operand ]
		ifFalse: [
			lvalue := self asUTC.
			rvalue := operand asUTC ].
	^ Duration
		seconds: (Time secondsInDay *(lvalue julianDayNumber - rvalue julianDayNumber)) + 
					(lvalue secondsSinceMidnight - rvalue secondsSinceMidnight)
		nanoSeconds: lvalue nanoSecond - rvalue nanoSecond
]

{ #category : 'private' }
DateAndTime >> subtractDuration: operand [
	"operand is a DateAndTime or a Duration"

	^self + operand negated
]

{ #category : 'double dispatching' }
DateAndTime >> subtractFrom: aDateAndTime [

	^ aDateAndTime subtractDateAndtime: self
]

{ #category : 'public protocol' }
DateAndTime >> time [

	^Time seconds: seconds nanoSeconds: nanos
]

{ #category : 'squeak protocol' }
DateAndTime >> to: anEnd [
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	self assert: (anEnd is: #dateAndTime).
	^ Timespan starting: self ending: anEnd
]

{ #category : 'squeak protocol' }
DateAndTime >> to: anEnd by: aDuration [
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	self assert: (anEnd is: #dateAndTime).
	self assert: aDuration class == Duration.
	^ (Schedule starting: self ending: anEnd)
		schedule: (Array with: aDuration);
		yourself.

]

{ #category : 'squeak protocol' }
DateAndTime >> to: anEnd by: aDuration do: aBlock [
	"Answer a Timespan. anEnd conforms to protocol DateAndTime or protocol Timespan"

	| schedule |
	schedule := self to: anEnd by: aDuration.
	schedule scheduleDo: aBlock.
	"Timespan are open on the right side. But include requested end if appropriate, like implementation in Number
	See DateAndTimeTest>>#testToByDo
	"
	schedule end + DateAndTime clockPrecision = anEnd ifTrue: [
		aBlock value: anEnd ]
]

{ #category : 'public protocol' }
DateAndTime >> truncateToSeconds [
	nanos := 0
]

{ #category : 'public protocol' }
DateAndTime >> unixTimeSeconds [
	"Return the number of seconds since the Unix epoch as an integer number

	DateAndTime now  unixTimeSeconds
	
	To check consistency with https://en.wikipedia.org/wiki/Unix_time
	(DateAndTime fromString: ' 2016-05-01T16:07:40') unixTimeSeconds
	1462118860
	"

	| elapsed |
	elapsed := self - self class unixEpoch.
	^ elapsed totalSeconds
]

{ #category : 'public protocol' }
DateAndTime >> unixTimeSecondsFloat [
	"Return the number of seconds since the Unix epoch.
	Answer an instance of Float, including fraction of a second

	DateAndTime now  unixTimeSecondsFloat
	
	To check consistency with https://en.wikipedia.org/wiki/Unix_time
	(DateAndTime fromString: ' 2016-05-01T16:07:40') unixTimeSecondsFloat
	1.46211886e9
	"

	"We know that DateAndTime unixEpoch has nanos set to zero"
	^ self unixTimeSeconds + (nanos * 1.0e-9)
]

{ #category : 'squeak protocol' }
DateAndTime >> utcOffset: anOffset [

	"Answer a <DateAndTime> equivalent to the receiver but offset from UTC by anOffset"

	| diff |
	self assert: anOffset class == Duration.

	diff := anOffset - self offset.
	^ self class
		julianDayNumber: jdn
		seconds: seconds + diff totalSeconds
		nanoseconds: nanos + diff nanoSeconds
		offset: anOffset
]

{ #category : 'public protocol' }
DateAndTime >> week [

	^Week including: self
]

{ #category : 'public protocol' }
DateAndTime >> year [

	^Year including: self
]

{ #category : 'public protocol' }
DateAndTime >> yearNumber [
	^ self
		dayMonthYearDo: [ :d :m :y | y ]
]
