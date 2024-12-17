'From Cuis7.3 [latest update: #6913] on 12 December 2024 at 12:43:01 pm'!
!classDefinition: #EphemeronFinalizationProcess category: #'System-Finalization'!
Object subclass: #EphemeronFinalizationProcess
	instanceVariableNames: ''
	classVariableNames: 'FinalizationSemaphore TheFinalizationProcess'
	poolDictionaries: ''
	category: 'System-Finalization'!

!EphemeronFinalizationProcess commentStamp: 'jmv 12/12/2024 12:21:40' prior: 0!
I represent the finalization process that waits for finalization of objects and sends the #mourn message to then. I am registered to the startup to stop/start myself on each start.!


!WeakArray class methodsFor: 'finalization' stamp: 'JMM 12/6/2024 12:16:29'!
doWeakFinalizationWork
	"Process the weak registries, in the old finalization style.  Hopefully this will
	 eventually go away when all clients have adopted the new finalization scheme."
	FinalizationLock
		critical:
			[FinalizationDependents do:
				[ :weakDependent |
				weakDependent ifNotNil:
					[weakDependent finalizeValues]]]
		ifError:
			[:msg :rcvr | rcvr error: msg]! !


!EphemeronFinalizationProcess class methodsFor: 'accessing' stamp: 'jmv 12/12/2024 12:25:03'!
finalizationProcess
	"The finalization process arranges to send mourn to each element of the VM's finalization queue,
	 which is accessed via primitiveFetchMourner.  The VM signals FinalizationSemaphore whenever
	 the queue is non-empty.  This process loops, waiting on the semaphore, fetches the first element
	 of the queue and then spawns a process at a higher priority to actually send the mourn messages.
	 If an error occurs in the higher priority mourn loop process then this process will simply spawn
	 another process, hence ensuring that errors in finalization methods don't break finalization.

	 In addition this process also runs the old finalization scheme, supporting clients of the older,
	 WeakRegistry based scheme.  Hopefully this will go away when all clients have moved over."
	| throttle firstMourner |
	throttle := Semaphore new.
	[true] whileTrue:
		[FinalizationSemaphore wait; initSignals.
		 "Support the old scheme until things have changed over..."
		 self doWeakFinalizationWork.
		
		 [firstMourner := self primitiveFetchMourner.
		  firstMourner notNil] whileTrue:
			[[throttle signal.
			  self mournLoopWith: firstMourner] forkAt: Processor activePriority + 1  named: '[system] Ephemeron Mourn processing'.
			  throttle wait]]! !

!EphemeronFinalizationProcess class methodsFor: 'accessing' stamp: '9/24/2024 06:44'!
runningFinalizationProcess
	"Answer the FinalizationProcessNewEra I am running, if any"
	^TheFinalizationProcess! !

!EphemeronFinalizationProcess class methodsFor: 'class initialization' stamp: 'jmv 12/12/2024 12:38:36'!
initialize
	"
	SystemDictionary initialize.
	WeakArray restartFinalizationProcess.
	EphemeronFinalizationProcess initialize.
	"

	FinalizationSemaphore := Smalltalk specialObjectsArray at: 42.! !

!EphemeronFinalizationProcess class methodsFor: 'private - mourning' stamp: 'JMM 12/12/2024 10:46:41'!
mournLoopWith: firstMourner
	"Send mourn to all the objects available in the mourn queue, starting
	 with firstMourner which the sender has already extracted for us.  If
	 an error occurs here, it will break this loop but the sender will spawn
	 another mournLoopWith: so that finalization is not broken by errors in
	 individual cases."
	| mourner |
	mourner := firstMourner.
	[mourner mourn.
	 (mourner := self primitiveFetchMourner) notNil] whileTrue! !

!EphemeronFinalizationProcess class methodsFor: 'private - mourning' stamp: '9/24/2024 06:44'!
primitiveFetchMourner
	"Answer the next mourner in the VM's queue of objects to be finalized.
	 The queue contains weak arrays and ephemerons.  If the primitive is
	 not implemented, raise an error telling people to upgrade the VM.  If
	 implemented, the primitive fails if the queue is empty, with the error
	 code #'not found'. Primitive.  Essential."

	<primitive: 172 error: ec>
	ec ifNil: [^self error: 'The primitiveFetchMourner primitive is missing.\Please upgrade your virtual machine to one that has the primitive.' withCRs].
	^nil! !

!EphemeronFinalizationProcess class methodsFor: 'system startup' stamp: '9/24/2024 06:44'!
startUp: resuming
	resuming ifFalse: [ ^self ].
	self restartFinalizationProcess! !

!EphemeronFinalizationProcess class methodsFor: 'mourning' stamp: 'JMM 12/11/2024 12:38:56'!
doWeakFinalizationWork
	" Fork off the old weak array processing logic" 
	
	[WeakArray doWeakFinalizationWork] forkAt: Processor activePriority + 2 named: '[system] Ephemeron WeakArray processing'! !

!EphemeronFinalizationProcess class methodsFor: 'mourning' stamp: 'JMM 11/27/2024 09:48:11'!
restartFinalizationProcess
	"kill any old process, just in case"
	TheFinalizationProcess
		ifNotNil: [TheFinalizationProcess terminate.
			TheFinalizationProcess := nil].

	FinalizationSemaphore := Smalltalk specialObjectsArray at: 42.
	TheFinalizationProcess := [ self finalizationProcess ] newProcess.
	TheFinalizationProcess priority: Processor userInterruptPriority.
	TheFinalizationProcess name: '[system] Ephemeron finalization'.
	TheFinalizationProcess resume! !

!EphemeronFinalizationProcess class methodsFor: 'mourning' stamp: 'JMM 11/20/2024 17:21:58'!
stopFinalizationProcess
	"kill any old process, just in case"
	TheFinalizationProcess
		ifNotNil: [TheFinalizationProcess terminate.
			TheFinalizationProcess := nil].! !

!EphemeronFinalizationProcess class methodsFor: 'mourning' stamp: 'JMM 11/20/2024 17:24:21'!
theFinalizationProcess
	^TheFinalizationProcess! !


!WeakArray class methodsFor: 'services' stamp: 'JMM 11/29/2024 14:30:48'!
ensureFinalizationComplete

	Smalltalk garbageCollect.
	Processor activePriority >= self finalizationProcess priority ifTrue: [
		"Not needed if the default priorities are used. Just in case."
		Processor activeProcess priority: self finalizationProcess priority-1.
		Processor yield ].! !

!WeakArray class methodsFor: 'finalization' stamp: 'JMM 11/29/2024 14:30:21'!
finalizationProcess
	"nothing here, moved to EphemeronFinalizationProcess class"
	^EphemeronFinalizationProcess theFinalizationProcess! !

!WeakArray class methodsFor: 'finalization' stamp: 'jmv 12/12/2024 12:39:21'!
restartFinalizationProcess

	FinalizationDependents ifNil: [FinalizationDependents := WeakArray new: 10].
	FinalizationLock := Semaphore forMutualExclusion.
	FinalizationProcess
		ifNotNil: [ 
			FinalizationProcess terminate.
			FinalizationProcess := nil ].
	FinalizationSemaphore := nil.
	EphemeronFinalizationProcess restartFinalizationProcess! !

!WeakArray class methodsFor: 'finalization' stamp: 'JMM 11/27/2024 09:22:05'!
runningFinalizationProcess
	"Answer the FinalizationProcess I am running, if any"
	^EphemeronFinalizationProcess theFinalizationProcess! !

!WeakArray class methodsFor: 'finalization' stamp: 'JMM 11/27/2024 09:22:05'!
stopFinalizationProcess
	"kill old process"
	EphemeronFinalizationProcess stopFinalizationProcess! !


!SystemDictionary class methodsFor: 'initialization' stamp: 'jmv 12/12/2024 12:36:48'!
initialize
	"
	SystemDictionary initialize
	"

	StartUpList := OrderedCollection new.
	"These get processed from the top down..."
	#(
		#Delay
		#DisplayScreen
		#Cursor
		#InputSensor
		#ProcessorScheduler 	"Starts low space watcher and bkground."
		#EphemeronFinalizationProcess)
			do: [ :clsName | 	
				Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToStartUpList: cls]].
			
	ShutDownList := OrderedCollection new.
	"These get processed from the bottom up..."
	#(
		#Delay
		#DisplayScreen
		#InputSensor
		#ProcessBrowser
		#CPUWatcher)
			do: [ :clsName | 
				Smalltalk at: clsName ifPresent: [:cls | Smalltalk addToShutDownList: cls]]! !

!methodRemoval: WeakArray class #cleanupForEphemeronFinalizationProcess stamp: 'jmv 12/12/2024 12:38:42'!
WeakArray class removeSelector: #cleanupForEphemeronFinalizationProcess!

!WeakArray class reorganize!
('accessing' addWeakDependent: isFinalizationSupported removeWeakDependent:)
('class initialization' initialize)
('system startup' startUp)
('private' pvtCreateTemporaryObjectIn:)
('services' ensureFinalizationComplete)
!

SystemDictionary initialize!
EphemeronFinalizationProcess initialize!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

	SystemDictionary initialize.
	WeakArray restartFinalizationProcess.
	EphemeronFinalizationProcess initialize.!

