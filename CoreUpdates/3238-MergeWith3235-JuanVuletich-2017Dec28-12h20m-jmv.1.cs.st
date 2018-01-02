'From Cuis 5.0 of 7 November 2016 [latest update: #3237] on 28 December 2017 at 12:21:11 pm'!

!AndreasSystemProfiler methodsFor: 'reporting' stamp: 'jmv 12/28/2017 12:20:41'!
doReport
	"Report the results of this profiler run"
	| report |
	report _ self report.
	self runningWorld addDeferredUIMessage: [
		SystemWindow
			editText: (Workspace withText: report)
			label: 'Spy Results'
			wrap: false ]! !

