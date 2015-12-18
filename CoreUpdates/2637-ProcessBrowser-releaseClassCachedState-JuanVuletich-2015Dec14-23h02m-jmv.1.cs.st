'From Cuis 4.2 of 25 July 2013 [latest update: #2636] on 14 December 2015 at 11:03:50.217492 pm'!

!ProcessBrowser class methodsFor: 'cached state access' stamp: 'jmv 12/14/2015 23:03'!
releaseClassCachedState
	SuspendedProcesses ifNotNil: [
		SuspendedProcesses ifEmpty: [
			SuspendedProcesses _ nil ]]! !

