'From Cuis 4.2 of 25 July 2013 [latest update: #2070] on 6 August 2014 at 3:05:23.555375 pm'!

!Morph methodsFor: 'debug and other' stamp: 'jmv 8/6/2014 14:59'!
resumeAfterDrawError

	self drawingFailsNot.
	self redrawNeeded! !

!methodRemoval: PluggableButtonMorph #update:!
PluggableButtonMorph removeSelector: #update:!
