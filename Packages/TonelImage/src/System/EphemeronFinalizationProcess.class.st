"
I represent the finalization process that waits for finalization of objects and sends the #mourn message to then. I am registered to the startup to stop/start myself on each start.
"
Class {
	#name : 'EphemeronFinalizationProcess',
	#superclass : 'Object',
	#classVars : [
		'FinalizationSemaphore',
		'TheFinalizationProcess'
	],
	#category : 'System-Finalization',
	#package : 'System',
	#tag : 'Finalization'
}

{ #category : 'finalization' }
EphemeronFinalizationProcess class >> ensureFinalizationComplete [

	Smalltalk garbageCollect.
	Processor activePriority >= self runningFinalizationProcess priority ifTrue: [
		"Not needed if the default priorities are used. Just in case."
		Processor activeProcess priority: self runningFinalizationProcess priority-1.
		Processor yield ].
]

{ #category : 'accessing' }
EphemeronFinalizationProcess class >> finalizationProcess [
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
			  throttle wait]]
]

{ #category : 'class initialization' }
EphemeronFinalizationProcess class >> initialize [
	"
	SystemDictionary initialize.
	EphemeronFinalizationProcess restartFinalizationProcess.
	EphemeronFinalizationProcess initialize.
	"

	FinalizationSemaphore := Smalltalk specialObjectsArray at: 42.
]

{ #category : 'accessing' }
EphemeronFinalizationProcess class >> mournAllPending [
	"See #finalizationProcess.
	This additional implementation is only to be called one last time before snapshot / quit.
	Warning: Unlike #finalizationProcess, this run is not protected against failure in #mourn.
		Failure in #mourn will halt snapshot / quit"

	self primitiveFetchMourner ifNotNil: [ :firstMourner |
		self mournLoopWith: firstMourner ].
]

{ #category : 'private - mourning' }
EphemeronFinalizationProcess class >> mournLoopWith: firstMourner [
	"Send mourn to all the objects available in the mourn queue, starting
	 with firstMourner which the sender has already extracted for us.  If
	 an error occurs here, it will break this loop but the sender will spawn
	 another mournLoopWith: so that finalization is not broken by errors in
	 individual cases."
	| mourner |
	mourner := firstMourner.
	[mourner mourn.
	 (mourner := self primitiveFetchMourner) notNil] whileTrue
]

{ #category : 'private - mourning' }
EphemeronFinalizationProcess class >> primitiveFetchMourner [
	"Answer the next mourner in the VM's queue of objects to be finalized.
	 The queue contains weak arrays and ephemerons.  If the primitive is
	 not implemented, raise an error telling people to upgrade the VM.  If
	 implemented, the primitive fails if the queue is empty, with the error
	 code #'not found'. Primitive.  Essential."

	<primitive: 172 error: ec>
	ec ifNil: [^self error: 'The primitiveFetchMourner primitive is missing.\Please upgrade your virtual machine to one that has the primitive.' withNewLines].
	^nil
]

{ #category : 'mourning' }
EphemeronFinalizationProcess class >> restartFinalizationProcess [
	"kill any old process, just in case"
	TheFinalizationProcess
		ifNotNil: [TheFinalizationProcess terminate.
			TheFinalizationProcess := nil].

	FinalizationSemaphore := Smalltalk specialObjectsArray at: 42.
	TheFinalizationProcess := [ self finalizationProcess ] newProcess.
	TheFinalizationProcess priority: Processor userInterruptPriority.
	TheFinalizationProcess name: '[system] Ephemeron finalization'.
	TheFinalizationProcess resume
]

{ #category : 'accessing' }
EphemeronFinalizationProcess class >> runningFinalizationProcess [
	"Answer the FinalizationProcessNewEra I am running, if any"
	^TheFinalizationProcess
]

{ #category : 'system startup' }
EphemeronFinalizationProcess class >> startUp: resuming [
	"resuming ifFalse: [ ^self ]."
	"Must restart even if just continuing after a snapshot"
	self restartFinalizationProcess
]

{ #category : 'mourning' }
EphemeronFinalizationProcess class >> stopFinalizationProcess [
	"kill any old process, just in case"
	TheFinalizationProcess
		ifNotNil: [TheFinalizationProcess terminate.
			TheFinalizationProcess := nil].
]
