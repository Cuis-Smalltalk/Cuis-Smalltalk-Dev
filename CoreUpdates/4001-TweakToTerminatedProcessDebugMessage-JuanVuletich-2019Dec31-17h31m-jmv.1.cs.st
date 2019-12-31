'From Cuis 5.0 [latest update: #4000] on 31 December 2019 at 5:35:23 pm'!

!Process methodsFor: 'debugging' stamp: 'jmv 12/31/2019 17:34:38'!
debug: context title: title full: bool
	"Open debugger on self with context shown on top"

	| topCtxt |
	self isTerminated ifTrue: [^ self error: 'can not debug a terminated process'].
	topCtxt _ self isRunning ifTrue: [thisContext] ifFalse: [self suspendedContext].
	(topCtxt hasContext: context) ifFalse: [^ self error: 'context not in process'].
	Debugger openOn: self context: context label: title fullView: bool.
! !

