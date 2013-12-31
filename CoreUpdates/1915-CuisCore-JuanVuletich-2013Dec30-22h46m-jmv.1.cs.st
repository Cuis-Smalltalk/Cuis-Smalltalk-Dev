'From Cuis 4.2 of 25 July 2013 [latest update: #1915] on 30 December 2013 at 10:51:47.80881 pm'!

!Process methodsFor: 'printing' stamp: 'jmv 12/30/2013 22:51'!
printOn: aStream

	super printOn: aStream.
	aStream
		space;
		nextPutAll: (self browserPrintStringFull: false)! !


!ProjectX class methodsFor: 'as yet unclassified' stamp: 'jmv 12/30/2013 22:51'!
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
		ifTrue: [ self notify: 'Can not debut a terminated process: ', label ]
		ifFalse: [
			preemptedProcess suspend.
			Debugger
				openInterrupt: label 
				onProcess: preemptedProcess ]! !

