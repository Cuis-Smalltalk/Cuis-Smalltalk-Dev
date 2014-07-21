'From Cuis 4.2 of 25 July 2013 [latest update: #2032] on 9 July 2014 at 11:25:09.470607 am'!

!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 7/9/2014 11:25'!
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
		ifTrue: [
			ProjectX newProcessIfUIX: preemptedProcess.
			self notify: 'Can not debug a terminated process: ', label ]
		ifFalse: [
			preemptedProcess suspend.
			Debugger
				openInterrupt: label 
				onProcess: preemptedProcess ]! !

