'From Cuis 4.0 of 21 April 2012 [latest update: #1494] on 27 November 2012 at 11:17:23 pm'!

!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 11/27/2012 23:16'!
interruptNameX: labelString
	"Create a Notifier on the active scheduling process with the given label."
	| preemptedProcess  |
	preemptedProcess _ Processor preemptedProcess.

	"Only debug preempted process if its priority is >= UIProcess' priority"
	preemptedProcess priority < UIProcess priority ifTrue: [
		preemptedProcess _ UIProcess ].

	preemptedProcess suspend.
	Debugger
		openInterrupt: labelString, 
			' - Process: ', preemptedProcess name, 
			' - Priority: ', preemptedProcess priority printString 
		onProcess: preemptedProcess! !

