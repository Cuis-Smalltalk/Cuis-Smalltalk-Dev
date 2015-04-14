'From Cuis 4.2 of 25 July 2013 [latest update: #2236] on 13 April 2015 at 11:31:56.581091 am'!

!WeakArray class methodsFor: 'private' stamp: 'jmv 4/13/2015 11:27'!
stopFinalizationProcess
	"kill old process"
	FinalizationProcess
		ifNotNil: [ 
			FinalizationProcess terminate.
			FinalizationProcess := nil ]! !


!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 4/12/2015 19:39'!
stopLowSpaceWatcher
	"Stop the process to watch for low-space conditions."
	"
	Smalltalk
		stopLowSpaceWatcher;
		installLowSpaceWatcher
	"

	self primSignalAtBytesLeft: 0.  "disable low-space interrupts"
	LowSpaceProcess ifNotNil: [LowSpaceProcess terminate].
	LowSpaceProcess _ nil! !

!SystemDictionary methodsFor: 'ui' stamp: 'jmv 4/12/2015 20:13'!
globals
	"
	Smalltalk  globals
	"
	
	| associations globals |
	associations _ ((self  keys select: [:aKey | ((self  at: aKey) isKindOf: Class) not]) collect:[:aKey | self associationAt: aKey]).
	globals _ IdentityDictionary new.
	associations do: [:as | globals add: as].
	^globals! !


!ReadOnlyVariableBinding methodsFor: 'printing' stamp: 'jmv 4/12/2015 20:20'!
printOn: aStream

	aStream
		print: self key;
		nextPutAll: ' ->(ReadOnlyVariableBinding) ';
		print: self value.! !


!ProcessorScheduler class methodsFor: 'background process' stamp: 'jmv 4/12/2015 19:45'!
stopBackgroundProcess
	"kill the background process. Only call if you know what you are doing, or are happy to try dangerous behavior"

	BackgroundProcess ifNotNil: [BackgroundProcess terminate].
	BackgroundProcess _ nil! !


!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 4/12/2015 19:50'!
stopUIProcess
	UIProcess ifNotNil: [
		UIProcess animatedUI: nil.
		UIProcess terminate ].
	UIProcess _ nil! !


!WeakArray class methodsFor: 'private' stamp: 'jmv 4/13/2015 11:27'!
restartFinalizationProcess
	"kill any old process, just in case"
	self stopFinalizationProcess.

	FinalizationSemaphore := Smalltalk specialObjectsArray at: 42.
	FinalizationDependents ifNil: [FinalizationDependents := WeakArray new: 10].
	FinalizationLock := Semaphore forMutualExclusion.
	FinalizationProcess := [ self finalizationProcess ] newProcess.
	FinalizationProcess priority: Processor userInterruptPriority.
	FinalizationProcess name: 'WeakArray finalization'.
	FinalizationProcess resume! !


!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 4/13/2015 11:29'!
installLowSpaceWatcher
	"Start a process to watch for low-space conditions."
	"Smalltalk installLowSpaceWatcher"

	self stopLowSpaceWatcher.
	LowSpaceProcess _ [self lowSpaceWatcher] newProcess.
	LowSpaceProcess priority: Processor lowIOPriority.
	LowSpaceProcess name: 'Low Space Watcher'.
	LowSpaceProcess resume.

! !

!SystemDictionary methodsFor: 'ui' stamp: 'jmv 4/12/2015 20:13'!
inspectGlobals
	"
	Smalltalk  inspectGlobals
	"

	self globals inspectWithLabel: 'The Globals'! !


!ProcessorScheduler class methodsFor: 'background process' stamp: 'jmv 4/13/2015 11:30'!
startUp
	"Install a background process of the lowest possible priority that is always runnable."
	"Details: The virtual machine requires that there is aways some runnable process that can be scheduled; this background process ensures that this is the case."

	Smalltalk installLowSpaceWatcher.
	self stopBackgroundProcess.
	BackgroundProcess _ [self idleProcess] newProcess.
	BackgroundProcess priority: SystemRockBottomPriority.
	BackgroundProcess name: 'Idle Process'.
	BackgroundProcess resume.
! !

