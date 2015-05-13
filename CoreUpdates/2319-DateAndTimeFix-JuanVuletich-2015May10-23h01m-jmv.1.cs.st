'From Cuis 4.2 of 25 July 2013 [latest update: #2318] on 10 May 2015 at 11:05:14.243784 pm'!

!DateAndTime class methodsFor: 'system startup & shutdown' stamp: 'jmv 5/10/2015 23:02'!
initClassCachedState

	LastTickSemaphore := Semaphore forMutualExclusion.
	LastTick := 0! !

!DateAndTime class methodsFor: 'system startup & shutdown' stamp: 'jmv 5/10/2015 23:03'!
releaseClassCachedState

	LastTickSemaphore _ nil.
	LastTick _ nil! !


!DateAndTime class methodsFor: 'initialization' stamp: 'jmv 5/10/2015 23:02'!
initialize
	"
	self initialize
	"
	self initClassCachedState! !

DateAndTime initialize!

!DateAndTime class reorganize!
('ansi protocol' clockPrecision now year:day:hour:minute:second: year:day:hour:minute:second:offset: year:month:day:hour:minute:second: year:month:day:hour:minute:second:offset:)
('squeak protocol' date:time: epoch fromString: julianDayNumber: localOffset localTimeZone localTimeZone: midnight new noon readFrom: today tomorrow year:day: year:month:day: year:month:day:hour:minute: year:month:day:hour:minute:second:nanoSecond:offset: yesterday)
('smalltalk-80' fromSeconds:)
('initialization' initialize)
('instance creation' julianDayNumber:seconds:nanoseconds:offset: nowUpToSeconds)
('system startup & shutdown' initClassCachedState releaseClassCachedState)
!

