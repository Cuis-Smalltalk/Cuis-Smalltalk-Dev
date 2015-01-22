'From Cuis 4.2 of 25 July 2013 [latest update: #2156] on 20 January 2015 at 9:13:05.993517 am'!

!EventSensor class methodsFor: 'class initialization' stamp: 'jmv 1/20/2015 09:10'!
install
	"
	EventSensor install
	"
	"Install an EventSensor in place of the current Sensor."

	Sensor shutDownSensor.
	Sensor _ self new! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	EventSensor install!

