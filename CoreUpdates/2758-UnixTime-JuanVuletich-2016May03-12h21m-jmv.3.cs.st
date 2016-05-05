'From Cuis 4.2 of 25 July 2013 [latest update: #2755] on 3 May 2016 at 5:05:05.643983 pm'!

!DateAndTime methodsFor: 'public protocol' stamp: 'jmv 5/3/2016 12:54'!
unixTimeSeconds
	"Return the number of seconds since the Unix epoch as an integer number

	DateAndTime now  unixTimeSeconds
	
	To check consistency with https://en.wikipedia.org/wiki/Unix_time
	(DateAndTime fromString: ' 2016-05-01T16:07:40') unixTimeSeconds
	1462118860
	"

	| elapsed |
	elapsed _ self - self class unixEpoch.
	^ elapsed totalSeconds! !

!DateAndTime methodsFor: 'public protocol' stamp: 'jmv 5/3/2016 14:41'!
unixTimeSecondsFloat
	"Return the number of seconds since the Unix epoch.
	Answer an instance of Float, including fraction of a second

	DateAndTime now  unixTimeSecondsFloat
	
	To check consistency with https://en.wikipedia.org/wiki/Unix_time
	(DateAndTime fromString: ' 2016-05-01T16:07:40') unixTimeSecondsFloat
	1.46211886e9
	"

	"We know that DateAndTime unixEpoch has nanos set to zero"
	^ self unixTimeSeconds + (nanos * 1.0e-9)! !


!DateAndTime class methodsFor: 'constants' stamp: 'jmv 5/3/2016 12:46'!
unixEpoch
	"
	DateAndTime unixEpoch
	1970-01-01T00:00:00+00:00
	
	(DateAndTime now - DateAndTime unixEpoch) days / 365.25
	"
	^ self
		julianDayNumber: 2440588
		seconds: 0
		nanoseconds: 0
		offset: Duration zero! !


!TimeZone class methodsFor: 'accessing' stamp: 'jmv 5/3/2016 12:40'!
utc
	"Answer the default time zone - UTC"

	^ self timeZones detect: [ :tz | tz abbreviation = 'UTC' ]! !


!TimeZone class methodsFor: 'accessing' stamp: 'jmv 5/3/2016 12:39'!
default
	"Answer the default time zone - UTC"

	^ self utc! !

!methodRemoval: DateAndTime #unixTimeMicroSecondsPart!
DateAndTime removeSelector: #unixTimeMicroSecondsPart!
!methodRemoval: DateAndTime #unixTimeSecondsFractionPart!
DateAndTime removeSelector: #unixTimeSecondsFractionPart!

!DateAndTime class reorganize!
('ansi protocol' clockPrecision now year:day:hour:minute:second: year:day:hour:minute:second:offset: year:month:day:hour:minute:second: year:month:day:hour:minute:second:offset:)
('squeak protocol' date:time: epoch fromString: julianDayNumber: localOffset localTimeZone localTimeZone: midnight new noon readFrom: today tomorrow year:day: year:month:day: year:month:day:hour:minute: year:month:day:hour:minute:second:nanoSecond:offset: yesterday)
('smalltalk-80' fromSeconds:)
('initialization' initialize)
('instance creation' julianDayNumber:seconds:nanoseconds:offset: nowUpToSeconds)
('system startup & shutdown' initClassCachedState releaseClassCachedState)
('constants' unixEpoch)
!

