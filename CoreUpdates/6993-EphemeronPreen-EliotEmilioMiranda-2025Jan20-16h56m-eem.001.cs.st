'From Cuis7.3 [latest update: #6992] on 20 January 2025 at 5:28:36 pm'!

!String methodsFor: 'formatting' stamp: 'eem 1/20/2025 17:30:25'!
withCRs
	"Answer a copy of the receiver in which backslash (\) characters have been replaced with carriage returns."

	^ self collect: [ :c | c == $\ ifTrue: [ Character cr ] ifFalse: [ c ]]! !


!WeakArray class methodsFor: 'finalization' stamp: 'eem 1/20/2025 16:59:03'!
finalizationProcess
	"nothing here, moved to EphemeronFinalizationProcess class"
	^EphemeronFinalizationProcess runningFinalizationProcess! !

!WeakArray class methodsFor: 'finalization' stamp: 'eem 1/20/2025 16:58:57'!
runningFinalizationProcess
	"Answer the FinalizationProcess I am running, if any"
	^EphemeronFinalizationProcess runningFinalizationProcess! !


!FinalizationRegistry methodsFor: 'finalization' stamp: 'eem 1/20/2025 17:02:57'!
finalizeEphemeron: anEphemeron
	"Finalize the ephemeron's key and remove the ephemeron from myself.
	 N.B. Here we send #finalize since finalization is called for by placing
	 objects in a FinalizationRegistry. i.e. the response to the ephemeron
	 being triggered is a property of the container of the ephemeron, not
	 the ephemeron itself."

	self removeKey: anEphemeron key ifAbsent: [].
	
	"Usually no executor is needed. The object that is about to be collected, the key needs to be finalized.
	 Some users may prefer to include an executor (an instance of ObjectFinalizer) as the value.
	 N.B. errors here will not stall the finalization process."
	
	(anEphemeron value ifNil: [anEphemeron key]) finalize! !


!ProcessBrowser class methodsFor: 'process control' stamp: 'eem 1/20/2025 16:58:24'!
rulesFor: aProcess
	"Answer two flags: allow-stop, and allow-debug"

	"Don't mess with the process running the ProcessBrowser.
	If we support several active UIs, we'd detect the UI process running us"
	aProcess == Processor activeProcess
		ifTrue: [^{false. false}].

	aProcess name = 'Sound Player'
		ifTrue: [ ^{false. false}].

	^ [aProcess caseOf: {
		[] -> [{false. false}].
		[Smalltalk lowSpaceWatcherProcess] -> [{false. false}].
		[WeakArray runningFinalizationProcess] -> [{false. false}].
		[EphemeronFinalizationProcess runningFinalizationProcess] -> [{false. false}].
		[Processor activeProcess] -> [{false. true}].
		[Processor backgroundProcess] -> [{false. false}].
		[Sensor interruptWatcherProcess] -> [{false. false}].
		[Sensor eventTicklerProcess] -> [{false. false}].
		[CPUWatcher currentWatcherProcess] -> [{false. false}].
		[Delay timerProcess] -> [{false. false}]}
		otherwise: [ {true. true}]]
		ifError: [ :err :rcvr | {true. true}]! !

!methodRemoval: EphemeronFinalizationProcess class #theFinalizationProcess stamp: 'eem 1/20/2025 16:59:06'!
EphemeronFinalizationProcess class removeSelector: #theFinalizationProcess!
