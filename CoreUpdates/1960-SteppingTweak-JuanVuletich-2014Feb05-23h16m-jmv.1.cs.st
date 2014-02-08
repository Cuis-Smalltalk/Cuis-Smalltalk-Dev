'From Cuis 4.2 of 25 July 2013 [latest update: #1959] on 5 February 2014 at 11:30:58.803454 pm'!

!Morph methodsFor: 'stepping' stamp: 'jmv 2/5/2014 23:26'!
startStepping
	"Start getting sent the 'step' message."
	"The #stepAt: message, or any message requiring an argument will be 
	called with the current millisecond timer value if no argument value is specified"

	self startStepping: #stepAt:! !

