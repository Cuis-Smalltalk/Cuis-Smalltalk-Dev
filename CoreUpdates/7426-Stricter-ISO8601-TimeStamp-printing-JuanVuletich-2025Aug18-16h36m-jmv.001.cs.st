'From Cuis7.5 [latest update: #7418] on 18 August 2025 at 5:54:32 pm'!

!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 8/18/2025 16:58:57'!
printOn: aStream includeOffset: aBoolean
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
						nextPutAll: (offset seconds abs truncated printString) ]]].! !

!DateAndTime methodsFor: 'testing' stamp: 'jmv 8/18/2025 16:36:23'!
isUTC
	^offset isZero! !


!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 8/18/2025 16:57:59'!
printOn: aStream
	"Print as per ISO 8601 section 5.4.2
	Prints either:
		'YYYY-MM-DDThh:mm:ss.sZ' (for positive years, UTC, i.e. zero offset)
		'YYYY-MM-DDThh:mm:ss.s+OO:oo' (for positive years, positive offset)
		'YYYY-MM-DDThh:mm:ss.s-OO:oo' (for positive years, negative offset)
		'-YYYY-MM-DDThh:mm:ss.s+OO:oo' (for negative years, offset as above)
	"

	^self printOn: aStream includeOffset: true! !

!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 8/18/2025 16:44:58'!
printWithMsOn: aStream
	"Print with millisecond resolution, no offset."

	| ps |
	self printYMDOn: aStream.
	aStream nextPut: $T.
	self printHMSOn: aStream.
	ps := (self nanoSecond // 1000000) printString padded: #left to: 3 with: $0.
	aStream nextPut: $..
	aStream nextPutAll: ps! !

!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 8/18/2025 16:43:33'!
printYMDOn: aStream
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
! !

!DateAndTime methodsFor: 'public protocol' stamp: 'jmv 8/18/2025 16:47:52'!
filenamishPrintOn: aStream
	"
	See also #dateTimeSuffix
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
		nextPut: $m! !

!DateAndTime methodsFor: 'public protocol' stamp: 'jmv 8/18/2025 16:55:50'!
printWithoutOffsetOn: aStream
	"Print as per ISO 8601 section 5.4.2
	Prints either:
		'YYYY-MM-DDThh:mm:ss.s' (for positive years)
		'-YYYY-MM-DDThh:mm:ss.s' (for negative years)
	"

	^self printOn: aStream includeOffset: false! !


!DateAndTime class methodsFor: 'squeak protocol' stamp: 'jmv 8/18/2025 17:54:18'!
readFrom: aStream

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
 	"! !


!Duration class methodsFor: 'squeak protocol' stamp: 'jmv 5/22/2022 20:21:16'!
fromString: aString
	"
	Extended to accept non-ANSI formats, such as:
		Duration fromString: '1:50:00'
		Duration fromString: '5:30.5'
		Duration fromString: '1:6:50:00'
		Duration fromString: '3.5h'
		Duration fromString: '2.5min'
		Duration fromString: '1.5s'
		Duration fromString: '200s'
		Duration fromString: '200ms'
	"
	| colons s |

	"If aString includes at least one $:, complete ANSI format"
	colons := aString occurrencesOf: $:.
	colons > 0 ifTrue: [
		s := aString.
		[colons < 3] whileTrue: [
			s := '00:', s.
			colons := colons + 1 ].
		^ self readFrom: s readStream ].

	"'3.5h' means 3.5 hours"
	(aString endsWith: 'h') ifTrue: [
		^self hours: aString asNumber ].

	"'3.5min' means 3.5 minutes"
	(aString endsWith: 'min') ifTrue: [
		^self minutes: aString asNumber ].

	"'3ms' means 3 milliseconds"
	(aString endsWith: 'ms') ifTrue: [
		^self milliSeconds: aString asNumber ].

	"'3.5s' means 3.5 seconds"
	(aString endsWith: 's') ifTrue: [
		^self seconds: aString asNumber ].
	
	^nil! !


!Time class methodsFor: 'smalltalk-80' stamp: 'jmv 8/18/2025 17:08:35'!
readFrom: aStream
	"Read a Time from the stream in the form:
		<hour>:<minute>:<second> <am/pm>

	<minute>, <second> or <am/pm> may be omitted.  e.g. 1:59:30 pm; 8AM; 15:30"

	| hour minute second nanos ampm |
	hour := Integer readFrom: aStream.
	minute := 0.
	second := 0.
	nanos := 0.
	(aStream peekFor: $:) 
		ifTrue: [
			minute := Integer readFrom: aStream.
			(aStream peekFor: $:) ifTrue: [
				second := Integer readFrom: aStream.
				(aStream peekFor: $.)
					ifTrue: [ | nanosStart scale |
						nanosStart := aStream position.
						nanos := Integer readFrom: aStream.
						"As if there were trailing zeros to fill 9 positions"
						scale := 10 raisedToInteger: nanosStart + 9 - aStream position.
						nanos := nanos * scale ]]].
	aStream skipSeparators.
	(aStream atEnd not and: ['ap' includes: aStream peek]) ifTrue: [
		ampm := aStream next asLowercase.
		(ampm == $p and: [hour < 12]) ifTrue: [hour := hour + 12].
			(ampm == $a and: [hour = 12]) ifTrue: [hour := 0].
		(aStream peekFor: $m) ifFalse: [aStream peekFor: $M ]].
	^ self 
		hour: hour 
		minute: minute 
		second: second 
		nanoSecond: nanos.

	"
	Time readFrom: '2:23:09 pm' readStream
	Time readFrom: '2:23:09.0 pm' readStream
	Time readFrom: '2:23:09.123402 pm' readStream
	Time readFrom: '2:23:09.00123402 pm' readStream
	Time readFrom: '2:23:09.001234020 pm' readStream
	Time readFrom: '2:23:09.000123402 pm' readStream
	"! !


!Utilities class methodsFor: 'identification' stamp: 'jmv 8/18/2025 16:47:43'!
dateTimeSuffix
	"Answer a string which indicates the date and time, intended for use in building fileout filenames, etc.
	See also #filenamishPrintOn:.

	Utilities dateTimeSuffix
	"
	| n |
	n := DateAndTime now.
	^ String streamContents: [ :strm |
		n printYMDOn: strm.
		strm nextPut: $_.
		n printHMSOn: strm separator: $. ]! !

!methodRemoval: DateAndTime #printYMDOn:withLeadingSpace: stamp: 'jmv 8/18/2025 16:45:18'!
DateAndTime removeSelector: #printYMDOn:withLeadingSpace:!
!methodRemoval: DateAndTime #printOn:withLeadingSpace:includeOffset: stamp: 'jmv 8/18/2025 16:44:35'!
DateAndTime removeSelector: #printOn:withLeadingSpace:includeOffset:!
