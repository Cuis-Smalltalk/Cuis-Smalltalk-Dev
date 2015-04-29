'From Cuis 4.2 of 25 July 2013 [latest update: #2245] on 16 April 2015 at 10:07:25.310002 am'!

!ProcessBrowser methodsFor: 'process list' stamp: 'jmv 4/16/2015 10:04'!
updateProcessList
	| oldSelectedProcess |
	oldSelectedProcess _ selectedProcess.
	processList _ selectedProcess _ selectedSelector _ nil.
	Smalltalk garbageCollectMost.
	"lose defunct processes"

	processList _ Process allSubInstances reject: [:each | each isTerminated ].
	processList _ processList sort: [ :a :b | a priority >= b priority ].
	processList _ WeakArray withAll: processList.
	self changed: #processNameList.
	self selectedProcess: ((processList includes: oldSelectedProcess)
		ifTrue: [oldSelectedProcess])! !


!ProcessBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 4/16/2015 10:07'!
startCPUWatcher

	model startCPUWatcher! !

!methodRemoval: ProcessBrowserWindow #setUpdateCallbackAfter:!
ProcessBrowserWindow removeSelector: #setUpdateCallbackAfter:!
