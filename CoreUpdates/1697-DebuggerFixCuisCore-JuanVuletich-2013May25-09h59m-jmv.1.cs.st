'From Cuis 4.1 of 12 December 2012 [latest update: #1696] on 25 May 2013 at 10:03:35.943393 am'!

!Process methodsFor: 'accessing' stamp: 'jmv 5/25/2013 10:01'!
basicSuspendedContext
	"Answer the context the receiver has suspended.
	The debugger needs access to this ivar even if the process appears to be terminated.
	To be better understood some day.
	This method was added when #suspendedContext was modified to answer nil for terminated processes."
	^suspendedContext! !


!Process methodsFor: 'changing suspended state' stamp: 'jmv 5/25/2013 09:57'!
step: aContext 
	"Resume self until aContext is on top, or if already on top, do next step"

	^ self basicSuspendedContext == aContext
		ifTrue: [self step]
		ifFalse: [self complete: (self calleeOf: aContext)]! !

