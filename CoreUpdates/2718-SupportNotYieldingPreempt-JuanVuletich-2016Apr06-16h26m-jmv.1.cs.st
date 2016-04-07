'From Cuis 4.2 of 25 July 2013 [latest update: #2717] on 6 April 2016 at 4:31:43.317358 pm'!

!ProcessorScheduler methodsFor: 'accessing' stamp: 'jmv 4/6/2016 16:30'!
preemptedProcess
	"Return the process that the currently active process just preempted."
	
	self activeProcess priority to: 1 by: -1 do: [ :priority |
		(quiescentProcessLists at: priority) ifNotEmpty: [ :list |
			^ Smalltalk processPreemptionYields
				ifTrue: [ list last ]
				ifFalse: [ list first ]]].
	^ nil
	"
	Processor preemptedProcess
	"! !

