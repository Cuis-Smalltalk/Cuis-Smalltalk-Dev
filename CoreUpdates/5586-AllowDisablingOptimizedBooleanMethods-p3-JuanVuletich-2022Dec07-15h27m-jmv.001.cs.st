'From Cuis 6.0 [latest update: #5583] on 7 December 2022 at 3:27:53 pm'!

!Process methodsFor: 'debugging' stamp: 'jmv 12/7/2022 15:22:40'!
debugContext: context title: title
	"Open debugger on self with context shown on top"

	self isTerminated ifTrue: [^ self error: 'can not debug a terminated process'].
	Debugger openProcess: self context: context label: title.! !

