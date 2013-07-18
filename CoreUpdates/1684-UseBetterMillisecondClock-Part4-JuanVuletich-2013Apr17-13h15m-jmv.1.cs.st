'From Cuis 4.1 of 12 December 2012 [latest update: #1683] on 17 April 2013 at 1:18:30 pm'!

!EventSensor methodsFor: 'private' stamp: 'jmv 4/17/2013 13:18'!
eventTickler
	"Poll infrequently to make sure that the UI process is not been stuck. 
	If it has been stuck, then spin the event loop so that I can detect the 
	interrupt key."
	"Move declaration of delta to the method start, to avoid a bug in the decompiler"
	| delay delta |
	delay := Delay forMilliseconds: self class eventPollPeriod.
	self lastEventPoll.	"ensure not nil."
	[
		[ 
			delay wait.
			delta := Time localMillisecondClock - lastEventPoll.
			delta > self class eventPollPeriod ifTrue: [
				self fetchMoreEvents ]
		] on: Error do: [:ex | ].
		true 
	] whileTrue! !

!EventSensor methodsFor: 'private' stamp: 'jmv 4/17/2013 13:18'!
installEventTickler
	"Initialize the interrupt watcher process. Terminate the old process if any."
	"Sensor installEventTickler"

	EventTicklerProcess ifNotNil: [ EventTicklerProcess terminate ].
	EventTicklerProcess _ [ self eventTickler ] newProcess.
	EventTicklerProcess priority: Processor lowIOPriority.
	EventTicklerProcess name: 'Event Tickler'.
	EventTicklerProcess resume! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Sensor installEventTickler!

