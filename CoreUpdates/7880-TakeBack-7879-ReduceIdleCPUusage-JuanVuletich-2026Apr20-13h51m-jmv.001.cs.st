'From Cuis7.7 [latest update: #7879] on 20 April 2026 at 1:52:45 pm'!

!ProcessorScheduler class methodsFor: 'background process' stamp: 'jm 11/Sep/1997 10:44:00'!
idleProcess
	"A default background process which is invisible."

	[true] whileTrue:
		[self relinquishProcessorForMicroseconds: 1000]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
ProcessorScheduler startUp. "Create a new #idleProcess (back to sleep 1mS, until a decision is made)"
Smalltalk vmParameterAt: 26 put: 2. "Run VM heartbeat every 2mS (as it has been until #7879, affects Delay resolution)"!

