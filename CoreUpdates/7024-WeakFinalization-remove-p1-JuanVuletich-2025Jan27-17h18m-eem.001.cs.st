'From Cuis7.3 [latest update: #7022] on 27 January 2025 at 5:20:17 pm'!

!WeakKeyDictionary commentStamp: '<historical>' prior: 0!
I am a dictionary holding only weakly on my keys. This is a bit dangerous since at any time my keys can go away. Clients are responsible to register my instances by WeakArray such that the appropriate actions can be taken upon loss of any keys.
!


!EphemeronFinalizationProcess class methodsFor: 'accessing' stamp: 'eem 12/18/2024 09:52:15'!
finalizationProcess
	"The finalization process arranges to send mourn to each element of the VM's finalization queue,
	 which is accessed via primitiveFetchMourner.  The mourn queue contains Ephemerons whose
	 keys should be finalized. The VM signals FinalizationSemaphore whenever the queue is non-empty.
	 This process loops, waiting on the semaphore, fetches the first element of the queue and then
	 spawns a process at a higher priority to actually send the mourn messages. If an error occurs in
	 the higher priority mourn loop process then this process will simply spawn another process, hence
	 ensuring that errors in finalization methods don't break finalization."
	| throttle firstMourner |
	throttle := Semaphore new.
	[true] whileTrue:
		[FinalizationSemaphore wait; initSignals.
		
		 [firstMourner := self primitiveFetchMourner.
		  firstMourner notNil] whileTrue:
			[[throttle signal.
			  self mournLoopWith: firstMourner] forkAt: Processor activePriority + 1  named: '[system] Ephemeron Mourning'.
			  throttle wait]]! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
EphemeronFinalizationProcess restartFinalizationProcess!

