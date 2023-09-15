'From Cuis 6.0 [latest update: #6030] on 14 September 2023 at 3:44:15 pm'!

!ProcessorScheduler methodsFor: 'accessing' stamp: 'jmv 9/14/2023 15:38:42'!
processes
	"Answer all non terminated Processes"
	^Process allSubInstances reject: [:each | each isTerminated ]! !


!ProcessBrowser methodsFor: 'process list' stamp: 'jmv 9/14/2023 15:40:03'!
updateProcessList
	| oldSelectedProcess |
	oldSelectedProcess := selectedProcess.
	processList := selectedProcess := selectedSelector := nil.
	Smalltalk garbageCollectMost.
	"lose defunct processes"

	processList := Processor processes.
	processList := processList sort: [ :a :b | a priority >= b priority ].
	processList := WeakArray withAll: processList.
	self changed: #processNameList.
	self selectedProcess: ((processList includes: oldSelectedProcess)
		ifTrue: [oldSelectedProcess])! !


!ProcessorScheduler methodsFor: 'private' stamp: 'jmv 9/14/2023 15:37:21'!
anyProcessesAbove: highestPriority 
	"Do any instances of Process exist with higher priorities?"
	self processesDo: [ :p |
		p priority > highestPriority ifTrue: [ ^true ]].
	^false.! !


!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 9/14/2023 15:39:26'!
printStuffToCleanOnImageSave
	"So far, to serve experiments.
	Some day, when this is empty, we'll be closer to bootstrap from sources."
	| n nonNilVars m classInstVars v|
	n := 0.
	""
	'--------' print.
	'Proceeses: ' print.
	(Processor processes
		sort: [ :a :b | a priority >= b priority ]) do: [ :p | p print ].
	'======' print.
	'Globals: ' print.
	Smalltalk globals associationsDo: [ :g | " {g key. g value class }" g  print ].
	'======' print.
	'ClassVars (notNil):  ' print.
	n := 0.
	Smalltalk hierachySortedAllClassesDo: [ :cls |
		"Cleared, but come again for lazy init during this report.
		therefore, not required for bootstrap from sources"
		({ Scanner } includes: cls) ifFalse: [
			nonNilVars := cls classPool select: [ :var | var notNil ].
			nonNilVars notEmpty ifTrue: [
				n := n + 1.
				{ cls. nonNilVars keys} print ]]].
	'======' print.
	'InstClassVars (notNil): ' print.
	m := 0.
	Smalltalk hierachySortedAllClassesDo: [ :cls |
		classInstVars := Dictionary new.
		cls class allRegularInstVarNames do: [ :nam |
			v := cls instVarNamed: nam.
			v ifNotNil: [
				classInstVars at: nam put: v ]].
		classInstVars notEmpty ifTrue: [
			m := m + 1.
			{ cls class. classInstVars keys }  print ]].
	'======' print.
	
	{ n. m}  print! !

