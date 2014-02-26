'From Cuis 4.2 of 25 July 2013 [latest update: #1962] on 19 February 2014 at 9:58:21.08692 am'!

!DateAndTime class methodsFor: 'squeak protocol' stamp: 'jmv 2/19/2014 09:57'!
readFrom: aStream

	| offset date time ch |

	date _ Date readFrom: aStream.
	[aStream peek isDigit]
		whileFalse: [aStream next].
	time _ Time readFrom: aStream.
	aStream atEnd
		ifTrue: [ offset _ self localOffset ]
		ifFalse: [
			ch _ aStream next.
			ch = $+ ifTrue: [ch _ Character space].
			offset _ Duration fromString: ch asString, '0:', aStream upToEnd, ':0'].
		
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
		DateAndTime readFrom: '-1199-01-05T20:33:14.321-05:00' readStream
		DateAndTime readFrom: ' 2002-05-16T17:20:45.1+01:01' readStream

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

