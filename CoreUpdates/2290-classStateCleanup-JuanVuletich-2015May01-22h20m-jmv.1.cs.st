'From Cuis 4.2 of 25 July 2013 [latest update: #2289] on 1 May 2015 at 10:26:35.680699 pm'!
!classDefinition: #ProcessorScheduler category: #'Kernel-Processes'!
Object subclass: #ProcessorScheduler
	instanceVariableNames: 'quiescentProcessLists activeProcess '
	classVariableNames: 'BackgroundProcess HighIOPriority LowIOPriority SystemBackgroundPriority SystemRockBottomPriority TimingPriority UserBackgroundPriority UserInterruptPriority UserSchedulingPriority '
	poolDictionaries: ''
	category: 'Kernel-Processes'!

!ProcessorScheduler class methodsFor: 'priority names' stamp: 'jmv 5/1/2015 22:03'!
highIOPriority
	"Answer the priority at which the most time critical input/output 
	processes should run. An example is the process handling input from a 
	network."

	^70! !

!ProcessorScheduler class methodsFor: 'priority names' stamp: 'jmv 5/1/2015 22:03'!
lowIOPriority
	"Answer the priority at which most input/output processes should run. 
	Examples are the process handling input from the user (keyboard, 
	pointing device, etc.) and the process distributing input from a network."

	^60! !

!ProcessorScheduler class methodsFor: 'priority names' stamp: 'jmv 5/1/2015 22:03'!
lowestPriority
	"Return the lowest priority that is allowed with the scheduler"

	^10! !

!ProcessorScheduler class methodsFor: 'priority names' stamp: 'jmv 5/1/2015 22:03'!
systemBackgroundPriority
	"Answer the priority at which system background processes should run. 
	Examples are an incremental garbage collector or status checker."

	^20! !

!ProcessorScheduler class methodsFor: 'priority names' stamp: 'jmv 5/1/2015 22:03'!
timingPriority
	"Answer the priority at which the system processes keeping track of real 
	time should run."

	^80! !

!ProcessorScheduler class methodsFor: 'priority names' stamp: 'jmv 5/1/2015 22:03'!
userBackgroundPriority
	"Answer the priority at which user background processes should run."

	^30! !

!ProcessorScheduler class methodsFor: 'priority names' stamp: 'jmv 5/1/2015 22:03'!
userInterruptPriority
	"Answer the priority at which user processes desiring immediate service 
	should run. Processes run at this level will preempt the window 
	scheduler and should, therefore, not consume the processor forever."

	^50! !

!ProcessorScheduler class methodsFor: 'priority names' stamp: 'jmv 5/1/2015 22:03'!
userSchedulingPriority
	"Answer the priority at which the window scheduler should run."

	^40! !


!BlockClosure methodsFor: 'private' stamp: 'jmv 10/8/2012 22:20'!
grabProcessorFor: milliseconds onTimeout: timeoutBlock
	"Evaluate the receiver (block), without the possibility of preemption by regular priority processes.
	If not finished after milliseconds, restore original priority and evaluate timeoutBlock.
	Use with care!!"
	"Based on #valueUnpreemptively"
	
	| activeProcess oldPriority result done |
	activeProcess _ Processor activeProcess.
	oldPriority _ activeProcess priority.
	done _ false.
	
	activeProcess priority: Processor highIOPriority + Processor lowIOPriority // 2.
	milliseconds ifNotNil: [
		[
			(Delay forMilliseconds: milliseconds) wait.
			done ifFalse: [
				activeProcess
					suspend;
					priority: oldPriority;
					resume.
				timeoutBlock value ]
		] forkAt: Processor highIOPriority named: '#grabProcessorFor:onTimeout:' ].

	result _ self ensure: [
		done _ true.
		activeProcess priority: oldPriority].
	
	"Yield after restoring priority to give the preempted processes a chance to run"
	Processor yield.
	^result! !


!ProcessorScheduler methodsFor: 'priority names' stamp: 'jmv 5/1/2015 22:23'!
highIOPriority
	"Answer the priority at which the most time critical input/output 
	processes should run. An example is the process handling input from a 
	network."

	^self class highIOPriority! !

!ProcessorScheduler methodsFor: 'priority names' stamp: 'jmv 5/1/2015 22:23'!
lowIOPriority
	"Answer the priority at which most input/output processes should run. 
	Examples are the process handling input from the user (keyboard, 
	pointing device, etc.) and the process distributing input from a network."

	^self class lowIOPriority! !

!ProcessorScheduler methodsFor: 'priority names' stamp: 'jmv 5/1/2015 22:24'!
lowestPriority
	"Return the lowest priority that is allowed with the scheduler"
	^self class lowestPriority! !

!ProcessorScheduler methodsFor: 'priority names' stamp: 'jmv 5/1/2015 22:24'!
systemBackgroundPriority
	"Answer the priority at which system background processes should run. 
	Examples are an incremental garbage collector or status checker."

	^self class systemBackgroundPriority! !

!ProcessorScheduler methodsFor: 'priority names' stamp: 'jmv 5/1/2015 22:24'!
timingPriority
	"Answer the priority at which the system processes keeping track of real 
	time should run."

	^self class timingPriority! !

!ProcessorScheduler methodsFor: 'priority names' stamp: 'jmv 5/1/2015 22:24'!
userBackgroundPriority
	"Answer the priority at which user background processes should run."

	^self class userBackgroundPriority! !

!ProcessorScheduler methodsFor: 'priority names' stamp: 'jmv 5/1/2015 22:24'!
userInterruptPriority
	"Answer the priority at which user processes desiring immediate service 
	should run. Processes run at this level will preempt the window 
	scheduler and should, therefore, not consume the processor forever."

	^self class userInterruptPriority! !

!ProcessorScheduler methodsFor: 'priority names' stamp: 'jmv 5/1/2015 22:24'!
userSchedulingPriority
	"Answer the priority at which the window scheduler should run."

	^self class userSchedulingPriority! !


!ProcessorScheduler class methodsFor: 'background process' stamp: 'jmv 5/1/2015 22:26'!
startUp
	"Install a background process of the lowest possible priority that is always runnable."
	"Details: The virtual machine requires that there is aways some runnable process that can be scheduled; this background process ensures that this is the case."

	Smalltalk installLowSpaceWatcher.
	self stopBackgroundProcess.
	BackgroundProcess _ [self idleProcess] newProcess.
	BackgroundProcess priority: self lowestPriority.
	BackgroundProcess name: 'Idle Process'.
	BackgroundProcess resume.
! !

!methodRemoval: ProcessorScheduler class #initialize!
ProcessorScheduler class removeSelector: #initialize!
ProcessorScheduler initialize!

!ProcessorScheduler class reorganize!
('instance creation' new)
('background process' idleProcess relinquishProcessorForMicroseconds: startUp stopBackgroundProcess)
('priority names' highIOPriority lowIOPriority lowestPriority systemBackgroundPriority timingPriority userBackgroundPriority userInterruptPriority userSchedulingPriority)
!

!classDefinition: #ProcessorScheduler category: #'Kernel-Processes'!
Object subclass: #ProcessorScheduler
	instanceVariableNames: 'quiescentProcessLists activeProcess'
	classVariableNames: 'BackgroundProcess'
	poolDictionaries: ''
	category: 'Kernel-Processes'!
