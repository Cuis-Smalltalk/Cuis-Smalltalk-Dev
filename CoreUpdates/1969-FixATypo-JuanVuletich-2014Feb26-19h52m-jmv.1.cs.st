'From Cuis 4.2 of 25 July 2013 [latest update: #1968] on 26 February 2014 at 7:52:25.930501 pm'!

!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 2/26/2014 19:52'!
interruptNameX: labelString
	"Create a Notifier on the active scheduling process with the given label."
	| preemptedProcess  label |
	preemptedProcess _ Processor preemptedProcess.

	"Only debug preempted process if its priority is >= UIProcess' priority"
	preemptedProcess priority < UIProcess priority ifTrue: [
		preemptedProcess _ UIProcess ].

	label _ labelString,
					' - Process: ', preemptedProcess name,
					' - Priority: ', preemptedProcess priority printString.
	preemptedProcess isTerminated
		ifTrue: [ self notify: 'Can not debug a terminated process: ', label ]
		ifFalse: [
			preemptedProcess suspend.
			Debugger
				openInterrupt: label 
				onProcess: preemptedProcess ]! !

