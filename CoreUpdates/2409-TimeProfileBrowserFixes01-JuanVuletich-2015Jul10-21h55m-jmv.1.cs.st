'From Cuis 4.2 of 25 July 2013 [latest update: #2408] on 10 July 2015 at 10:09:22.462167 pm'!

!ProcessBrowserWindow methodsFor: 'menu building' stamp: 'jmv 7/9/2015 14:45'!
processListMenu
	| menu rules |
	menu _ MenuMorph new defaultTarget: self.

	model selectedProcess
		ifNotNil: [ :selectedProcess |
			rules _ model class rulesFor: model selectedProcess.
			menu addList: #(
				('inspect (i)'					#inspectProcess)
				('explore (I)'				#exploreProcess)
				('references finder'			#openReferencesFinder)).
			rules first
				ifTrue: [
					menu add: 'terminate (t)' target: model action: #terminateProcess.
					selectedProcess isSuspended
						ifTrue: [menu add: 'resume (r)' target: model action: #resumeProcess]
						ifFalse: [menu add: 'suspend (s)' target: model action: #suspendProcess]].
			rules second
				ifTrue: [
					menu addList: #(
						('change priority (p)'		 	#changePriority)
						('debug (d)'						#debugProcess))].
			(selectedProcess suspendingList isKindOf: Semaphore)
				ifTrue: [menu add: 'signal Semaphore (S)' target: model action: #signalSemaphore].
			menu add: 'full stack (k)' target: model action: #moreStack.
			menu addLine].

	menu addList: #(
		('find context... (f)'		#findContext)
		('find again (g)'			#nextContext		''		model)).
	menu addLine.

	menu
		add: (isStepping
				ifTrue: ['turn off auto-update (a)']
				ifFalse: ['turn on auto-update (a)'])
		action: #toggleAutoUpdate.
	menu add: 'update list (u)' target: model action: #updateProcessList.

	menu addLine.
	CPUWatcher isMonitoring
			ifTrue: [ menu add: 'stop CPUWatcher' action: #stopCPUWatcher ]
			ifFalse: [ menu add: 'start CPUWatcher' action: #startCPUWatcher  ].

	^ menu! !

!ProcessBrowserWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/9/2015 14:45'!
processListKey: aKey from: aView 
	^ aKey caseOf: {
		[$i] -> [self inspectProcess].
		[$I] -> [self exploreProcess].
		[$P] -> [self openReferencesFinder].
		[$t] -> [model terminateProcess].
		[$r] -> [model resumeProcess].
		[$s] -> [model suspendProcess].
		[$d] -> [self debugProcess].
		[$p] -> [self changePriority].
		[$f] -> [self findContext].
		[$g] -> [model nextContext].
		[$a] -> [self toggleAutoUpdate].
		[$u] -> [model updateProcessList].
		[$S] -> [model signalSemaphore].
		[$k] -> [model moreStack]}
		 otherwise: []! !

!methodRemoval: ProcessBrowserWindow #messageTally!
ProcessBrowserWindow removeSelector: #messageTally!

!ProcessBrowserWindow reorganize!
('initialization' initialize)
('GUI building' buildMorphicWindow)
('menu building' processListMenu stackListMenu toggleAutoUpdate)
('menu commands' browseContext changePriority debugProcess exploreContext exploreProcess exploreReceiver findContext inspectContext inspectProcess inspectReceiver openReferencesFinder startAutoUpdate startCPUWatcher stopAutoUpdate stopCPUWatcher)
('keyboard shortcuts' processListKey:from: stackListKey:from:)
('stepping' stepTime)
('open/close' delete openInWorld)
!

!methodRemoval: MessageTally class #defaultPollPeriod:!
MessageTally class removeSelector: #defaultPollPeriod:!
!methodRemoval: MessageTally class #spyForMilliseconds:!
MessageTally class removeSelector: #spyForMilliseconds:!
!methodRemoval: MessageTally class #spyOn:toFileNamed:reportOtherProcesses:!
MessageTally class removeSelector: #spyOn:toFileNamed:reportOtherProcesses:!
!methodRemoval: MessageTally class #spyOnProcess:forMilliseconds:!
MessageTally class removeSelector: #spyOnProcess:forMilliseconds:!
!methodRemoval: MessageTally class #spyOnProcess:forMilliseconds:reportOtherProcesses:!
MessageTally class removeSelector: #spyOnProcess:forMilliseconds:reportOtherProcesses:!
!methodRemoval: MessageTally class #spyOnProcess:forMilliseconds:toFileNamed:reportOtherProcesses:!
MessageTally class removeSelector: #spyOnProcess:forMilliseconds:toFileNamed:reportOtherProcesses:!
!methodRemoval: MessageTally class #time:!
MessageTally class removeSelector: #time:!
!methodRemoval: MessageTally #spyEvery:onProcess:forMilliseconds:!
MessageTally removeSelector: #spyEvery:onProcess:forMilliseconds:!

!MessageTally reorganize!
('collecting leaves' bump: bump:fromSender: into:fromSender: leavesInto:fromSender:)
('comparing' < = > hash isPrimitives sonsOver:)
('printing' fullPrintExactOn: fullPrintOn:threshold: leavesPrintExactOn: leavesPrintOn:threshold: printOn: printOn:total:totalTime:tallyExact: printSenderCountsOn: rootPrintOn:total:totalTime:threshold: treePrintOn:tabs:thisTab:total:totalTime:tallyExact:orThreshold:)
('reporting' report: report:cutoff: reportGCStatsOn: tally time)
('tallying' bumpBy: tally:by: tally:in:by: tallyPath:by: tallyPath:in:by:)
('private' class:method: copyWithTally: method primitives: process process:)
('accessing' reportOtherProcesses:)
('initialization' spyAllEvery:on: spyEvery:on:)
!

!methodRemoval: TimeProfileBrowser class #spyOnProcess:forMilliseconds:!
TimeProfileBrowser class removeSelector: #spyOnProcess:forMilliseconds:!
!methodRemoval: TimeProfileBrowser #runProcess:forMilliseconds:pollingEvery:!
TimeProfileBrowser removeSelector: #runProcess:forMilliseconds:pollingEvery:!
