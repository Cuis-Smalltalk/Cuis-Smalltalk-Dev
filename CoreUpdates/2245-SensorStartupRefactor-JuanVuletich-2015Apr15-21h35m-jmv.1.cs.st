'From Cuis 4.2 of 25 July 2013 [latest update: #2241] on 15 April 2015 at 9:35:26.314929 pm'!

!InputSensor class methodsFor: 'system startup' stamp: 'jmv 4/15/2015 21:35'!
startUp
	
	self installMouseDecodeTable.
	EventSensor install! !

