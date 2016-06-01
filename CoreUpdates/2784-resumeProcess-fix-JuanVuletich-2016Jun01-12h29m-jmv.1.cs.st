'From Cuis 4.2 of 25 July 2013 [latest update: #2783] on 1 June 2016 at 12:29:36.711832 pm'!

!ProcessBrowser class methodsFor: 'process control' stamp: 'jmv 5/26/2016 19:49'!
resumeProcess: aProcess
	| priority |
	priority _ self suspendedProcesses
				removeKey: aProcess
				ifAbsent: [aProcess priority].
	aProcess priority: priority.
	aProcess isSuspended ifTrue: [ aProcess resume ]
! !

