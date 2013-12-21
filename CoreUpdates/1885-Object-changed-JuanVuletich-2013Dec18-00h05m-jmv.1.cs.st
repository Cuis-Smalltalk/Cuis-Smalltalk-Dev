'From Cuis 4.2 of 25 July 2013 [latest update: #1880] on 18 December 2013 at 12:05:17.140712 am'!

!Object methodsFor: 'events-old protocol' stamp: 'jmv 12/18/2013 00:05'!
changed
	"Receiver changed in a general way; inform all the dependents by 
	sending each dependent an update: message."

	self changed: self! !

