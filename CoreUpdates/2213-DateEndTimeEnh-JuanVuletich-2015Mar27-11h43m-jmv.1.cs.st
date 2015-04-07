'From Cuis 4.2 of 25 July 2013 [latest update: #2212] on 27 March 2015 at 11:43:37.697343 am'!

!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 3/27/2015 10:11'!
printOn: aStream withLeadingSpace: printLeadingSpaceToo includeOffset: aBoolean
	"Print as per ISO 8601 sections 5.3.3 and 5.4.1.
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
		offset seconds = 0 ifFalse: [
			aStream
				nextPut: $:;
				nextPutAll: (offset seconds abs truncated asString) ]]! !


!DateAndTime class methodsFor: 'instance creation' stamp: 'jmv 3/27/2015 10:08'!
nowUpToSeconds
	"Resolution is up to one second. Don't use as a TimeStamp!!
	DateAndTime nowUpToSeconds
	"

	| days seconds remainingSeconds |
	seconds _ Time localMicrosecondClock // 1000000.

	days _ seconds // Time secondsInDay.
	remainingSeconds _ seconds \\ Time secondsInDay.
	^ self basicNew
		setJdn: Time squeakEpoch + days 
		seconds: remainingSeconds 
		nano: 0 
		offset: self localOffset! !


!DateAndTime methodsFor: 'squeak protocol' stamp: 'jmv 3/27/2015 10:09'!
printOn: aStream
	"Print as per ISO 8601 sections 5.3.3 and 5.4.1.
	Prints either:
		'YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for positive years) or '-YYYY-MM-DDThh:mm:ss.s+ZZ:zz:z' (for negative years)"

	^self printOn: aStream withLeadingSpace: false includeOffset: true
! !

!methodRemoval: DateAndTime #printOn:withLeadingSpace:!
DateAndTime removeSelector: #printOn:withLeadingSpace:!
