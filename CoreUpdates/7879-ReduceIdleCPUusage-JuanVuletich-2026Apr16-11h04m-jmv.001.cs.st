'From Cuis7.7 [latest update: #7878] on 16 April 2026 at 11:07:16 am'!

!ProcessorScheduler class methodsFor: 'background process' stamp: 'jmv 16/Apr/2026 11:04:33'!
idleProcess
	"A default background process which is invisible."

	[true] whileTrue:
		[self relinquishProcessorForMicroseconds: 50000]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
ProcessorScheduler startUp. "Create a new #idleProcess (sleep 50mS, previously 1mS)"
Smalltalk vmParameterAt: 26 put: 10. "Run VM heartbeat every 10mS (previously 2mS)"!

