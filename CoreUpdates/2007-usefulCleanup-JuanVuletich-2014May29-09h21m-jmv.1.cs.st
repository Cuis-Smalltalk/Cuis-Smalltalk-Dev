'From Cuis 4.2 of 25 July 2013 [latest update: #2005] on 29 May 2014 at 9:21:28.331265 am'!

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 5/29/2014 08:50'!
someCleanup
	"
	Smalltalk someCleanup
	Sometimes ueful, for example, prior to saving an image.
	"
	WorldState allInstancesDo: [ :a | a convertStepList; convertAlarms ].
	Delay startTimerEventLoop.
	Smalltalk garbageCollect! !

