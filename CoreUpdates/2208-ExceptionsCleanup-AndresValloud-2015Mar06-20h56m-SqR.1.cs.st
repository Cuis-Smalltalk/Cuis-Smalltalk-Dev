'From Cuis 4.2 of 25 July 2013 [latest update: #2169] on 6 March 2015 at 8:59:41.732496 pm'!

!Exception methodsFor: 'exceptionDescription' stamp: 'SqR 3/6/2015 20:56'!
noHandler
	"No one has handled this error, but now give them a chance to decide how to debug it.  If none handle this either then open debugger (see UnhandedError-defaultAction)"

	UnhandledError signalForException: self! !


!Exception methodsFor: 'exceptionDescription' stamp: 'SqR 3/6/2015 20:56'!
defaultAction
	"The default action taken if the exception is signaled."

	self subclassResponsibility! !


!Abort methodsFor: 'exceptionDescription' stamp: 'SqR 3/6/2015 20:58'!
defaultAction

	self noHandler! !


!Error methodsFor: 'exceptionDescription' stamp: 'SqR 3/6/2015 20:57'!
defaultAction

	self noHandler! !


!Halt methodsFor: 'priv handling' stamp: 'SqR 3/6/2015 20:57'!
defaultAction

	self noHandler! !


!IllegalResumeAttempt methodsFor: 'comment' stamp: 'SqR 3/6/2015 20:57'!
defaultAction

	self noHandler! !


!Abort reorganize!
('exceptionDescription' defaultAction)
!

