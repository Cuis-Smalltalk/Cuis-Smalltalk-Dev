'From Cuis7.3 [latest update: #6929] on 18 December 2024 at 8:29:42 pm'!

!Ephemeron methodsFor: 'mourning' stamp: 'eem 12/18/2024 11:54:12'!
mourn
	"Triggered ephemerons get sent mourn by EphemeronsFinalizationProcess class's finalizationProcess
	 (via mournLoopWith:) when the only reference(s) to an ephemeron's key is through
	 one or more ephemerons. i.e. the key is not reachable from the roots of the system,
	 only through ephemerons.  So ephemerons get sent mourn when their key would
	 otherwise have been garbage collected.  Respond to the information that the key
	 would have been garbage collected other than through ephemeron references by
	 sending finalize to the key.

	 N.B. read #finalizeEphemeron: to understand why #finalize is not sent here."
	container finalizeEphemeron: self.! !


!EphemeronFinalizationProcess class methodsFor: 'accessing' stamp: 'eem 12/18/2024 09:52:15'!
finalizationProcess
	"The finalization process arranges to send mourn to each element of the VM's finalization queue,
	 which is accessed via primitiveFetchMourner.  The mourn queue contains Ephemerons whose
	 keys should be finalized. The VM signals FinalizationSemaphore whenever the queue is non-empty.
	 This process loops, waiting on the semaphore, fetches the first element of the queue and then
	 spawns a process at a higher priority to actually send the mourn messages. If an error occurs in
	 the higher priority mourn loop process then this process will simply spawn another process, hence
	 ensuring that errors in finalization methods don't break finalization.

	 In addition this process also runs the old finalization scheme, supporting clients of the older,
	 WeakRegistry based scheme.  This may go away if and when all clients have moved over."
	| throttle firstMourner |
	throttle := Semaphore new.
	[true] whileTrue:
		[FinalizationSemaphore wait; initSignals.
		 "Support the old scheme until things have changed over..."
		 self doWeakFinalizationWork.
		
		 [firstMourner := self primitiveFetchMourner.
		  firstMourner notNil] whileTrue:
			[[throttle signal.
			  self mournLoopWith: firstMourner] forkAt: Processor activePriority + 1  named: '[system] Ephemeron Mourning'.
			  throttle wait]]! !

!EphemeronFinalizationProcess class methodsFor: 'class initialization' stamp: 'eem 12/18/2024 11:08:48'!
initialize
	"
	SystemDictionary initialize.
	EphemeronFinalizationProcess restartFinalizationProcess.
	EphemeronFinalizationProcess initialize.
	"

	FinalizationSemaphore := Smalltalk specialObjectsArray at: 42.! !

EphemeronFinalizationProcess initialize.
EphemeronFinalizationProcess restartFinalizationProcess!
