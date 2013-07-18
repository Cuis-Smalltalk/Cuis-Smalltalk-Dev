'From Cuis 4.1 of 12 December 2012 [latest update: #1674] on 14 April 2013 at 11:58:20 pm'!

!DateAndTime class methodsFor: 'initialize-release' stamp: 'jmv 4/14/2013 23:58'!
initialize

	ClockProvider := Time.
	LastTickSemaphore := Semaphore forMutualExclusion.
	LastMilliSeconds := 0.
	LastTick := 0.
	Smalltalk addToStartUpList: self.
	self startUp: true! !


!DifferenceFinder class methodsFor: 'initialization' stamp: 'jmv 4/14/2013 23:58'!
initialize
	"
	DifferenceFinder initialize
	"
	EmptyLCS := Set with: #().
	D := 1.
	U := 2.
	L := 3.
	UL := 4
! !

DifferenceFinder initialize!
DateAndTime initialize!
