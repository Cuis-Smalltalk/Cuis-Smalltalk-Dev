'From Cuis7.9 [latest update: #8001] on 16 June 2026 at 5:41:30 pm'!

!DateAndTime methodsFor: 'private' stamp: 'jmv 16/Jun/2026 17:10:19'!
printOffsetOn: aStream
	"Print UTC offset"
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
			nextPutAll: (offset seconds abs truncated printString) ]! !

!DateAndTime methodsFor: 'public protocol' stamp: 'jmv 16/Jun/2026 17:15:12'!
printNiceOn: aStream
	"Print a nice human readable format.
	String streamContents: [ :strm | DateAndTime now printNiceOn: strm ]
		'16/Jun/2026, 5:46:51 pm (-03:00)' .
	String streamContents: [ :strm | DateAndTime nowUTC printNiceOn: strm ]
		'16/Jun/2026, 8:46:54 pm (UTC)' .
	"
	self date printOn: aStream format: #(1 2 3 $/  2 1 ).
	aStream nextPutAll: ', '.
	self time print24: false on: aStream.
	self isUTC
		ifTrue: [ aStream nextPutAll: ' (UTC)' ]
		ifFalse: [
			aStream space.
			aStream nextPut: $(.
			self printOffsetOn: aStream.
			aStream nextPut: $). ].! !


!DateAndTime class methodsFor: 'instance creation' stamp: 'jmv 16/Jun/2026 16:36:43'!
nowUTC
	"Answer UTC time. Give the same answer in any time zone.
	Use highest resolution possible.
	If called several times, always answer different, increasing values. This means that they can used as TimeStamps.
	If, instead, a numeric unique identifier is desired, call #nanosecondsTick
	DateAndTime nowUTC
	"

	| nanoseconds utcOffset days remainingNanoseconds remainingSeconds |
	self updateLastTickAndOffset.

	utcOffset := Duration zero.
	nanoseconds := LastTick.
	days := nanoseconds // Time nanosecondsInDay.
	remainingNanoseconds := nanoseconds \\ Time nanosecondsInDay.
	remainingSeconds := remainingNanoseconds // 1e9.
	remainingNanoseconds := remainingNanoseconds \\ 1e9.

	^ self basicNew
		setJdn: `DateAndTime unixEpoch julianDayNumber` + days
		seconds: remainingSeconds 
		nano: remainingNanoseconds 
		offset: utcOffset! !


!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 16/Jun/2026 17:11:51'!
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
	self nanoSecond = 0 ifFalse: [
		| z ps |
		ps := self nanoSecond printString padded: #left to: 9 with: $0.
		z := ps findLast: [ :c | c codePoint > `$0 codePoint` ].
		(z > 0) ifTrue: [aStream nextPut: $.].
		ps from: 1 to: z do: [ :c | aStream nextPut: c ] ].
	aBoolean ifTrue: [
		self isUTC
			ifTrue: [ aStream nextPut: $Z]
			ifFalse: [ self printOffsetOn: aStream ]].! !

