'From Cuis 6.0 [latest update: #5389] on 12 July 2022 at 2:29:56 pm'!

!UISupervisor class methodsFor: 'accessing' stamp: 'jmv 7/12/2022 14:20:06'!
animatedUIOf: aProcess
	"
	UISupervisor animatedUIOf: UIProcess
	[ (UISupervisor animatedUIOf: Processor activeProcess) print ] fork
	"
	^aProcess == UIProcess ifTrue: [ ^UI ]! !

!UISupervisor class methodsFor: 'accessing' stamp: 'jmv 7/12/2022 14:22:22'!
isUIProcess: aProcess
	^UIProcess == aProcess! !

!UISupervisor class methodsFor: 'accessing' stamp: 'jmv 7/12/2022 14:19:26'!
runningWorld
	"
	UISupervisor runningWorld
	[ UISupervisor runningWorld print ] fork
	"
	^Processor activeProcess == UIProcess ifTrue: [ ^UI ]! !


!Object methodsFor: 'morphic' stamp: 'jmv 7/12/2022 14:17:36'!
runningWorld
	"Answer a morphic world that is the current UI focus.
	This is the UI root animated by the active Process.
	This method could answer nil, if not in an UI process!!"

	^UISupervisor runningWorld! !


!Debugger methodsFor: 'private' stamp: 'jmv 7/12/2022 14:23:47'!
resumeProcess
	| mustTerminateActive mustRedisplay |
	mustRedisplay _ self runningWorld.
	savedCursor
		ifNotNil: [savedCursor activateCursor].
	mustTerminateActive _ false.
	interruptedProcess isTerminated ifFalse: [
		self runningWorld = interruptedProcessUI ifTrue: [
			mustTerminateActive _ true ].
		interruptedProcess resume ].
	"if old process was terminated, just terminate current one"
	interruptedProcess _ nil.
	contextStackIndex _ 0.
	contextStack _ nil.
	contextStackTop _ nil.
	receiverInspector _ nil.
	contextVariablesInspector _ nil.
	mustRedisplay ifNotNil: [ :w | UISupervisor whenUIinSafeState: [ w displayWorld ]].
	"restart low space handler"
	Smalltalk installLowSpaceWatcher.
	"If this process was the UI process, then it will terminate and never return to caller."
	mustTerminateActive
		ifTrue: [ Processor terminateActive ]! !


!CPUWatcher methodsFor: 'porcine capture' stamp: 'jmv 7/12/2022 14:22:41'!
catchThePig: aProcess
	| rule |
	"nickname, allow-stop, allow-debug"
	rule _ (ProcessBrowser rulesFor: aProcess) first.

	(UISupervisor isUIProcess: aProcess)
		ifTrue: [ "aProcess debugWithTitle: 'Interrupted from the CPUWatcher'." ]
		ifFalse: [
			rule ifFalse: [ ^self ].
			ProcessBrowser suspendProcess: aProcess.
			self openWindowForSuspendedProcess: aProcess ]! !


!Process methodsFor: 'printing' stamp: 'jmv 7/12/2022 14:28:05'!
browserPrintStringFull: aBoolean

	^String streamContents: [ :stream |
		aBoolean ifTrue: [
			stream
				nextPutAll: self statusString;
				space ].
		priority printOn: stream.
		stream nextPutAll: ' ('.
		self hash printOn: stream.
		stream
			nextPutAll: ') ';
			nextPutAll: self name.
		(UISupervisor animatedUIOf: self) ifNotNil: [ :ui |
			stream nextPutAll: ' - '.
			ui printOn: stream ].
		aBoolean ifTrue: [
			stream
				nextPutAll: ': ';
				nextPutAll: suspendedContext printString ]]! !


!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 7/12/2022 14:26:09'!
newProcessIfUI: processBeingSuspended
	"processBeingSuspended has been suspended or is about to be.
	Answer the UI we created a new process for.
	If not an UI process, still guarantee that the UI process is runnable."

	processBeingSuspended == UIProcess ifTrue: [
		self spawnNewMorphicProcessFor: UI.
		^UI ].
	^nil
	"
	|s|
	s := Semaphore new.
	[1/0. s signal] fork.
	s wait
	"! !

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 7/12/2022 14:26:58'!
newUIProcessIfNeeded
	"If the system needs a UIProcess (we know because UIProcess is not nil),
	then ensure that the UIProcess is ready to run, in order to hava a responsive UI.
	If we needed to create a new UI process, answer the old one, as it is most likely the
	process the user is interested in debugging. See senders."

	UIProcess ifNotNil: [ :oldUIProcess |
		oldUIProcess isBlocked | oldUIProcess isSuspended | oldUIProcess isTerminated ifTrue: [
			oldUIProcess == UIProcess ifTrue: [
				self spawnNewMorphicProcessFor: UI.
				^oldUIProcess ]]].
	^nil! !

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 7/12/2022 14:24:22'!
spawnNewMorphicProcessFor: guiRootObject
	"
	Safe to run anytime, as the old Morphic process will end by itself.
	UISupervisor spawnNewMorphicProcessFor: UISupervisor ui.
	"
	UI _ guiRootObject.
	UIProcess _ guiRootObject runProcess.
	UIProcess resume! !

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 7/12/2022 14:24:30'!
stopUIProcess
	UIProcess ifNotNil: [
		UIProcess terminate ].
	UI _ nil.
	UIProcess _ nil.! !

!UISupervisor class methodsFor: 'gui process' stamp: 'jmv 7/12/2022 14:27:39'!
ui
	^UI! !


!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 7/12/2022 14:24:45'!
runProcess
	
	| process |
	process _ [ self mainLoop ] newProcess.
	process
		priority: Processor userSchedulingPriority;
		name: 'Morphic UI'.
	^ process! !

!methodRemoval: Process #animatedUI stamp: 'jmv 7/12/2022 14:28:14'!
Process removeSelector: #animatedUI!
!methodRemoval: Process #animatedUI: stamp: 'jmv 7/12/2022 14:24:50'!
Process removeSelector: #animatedUI:!

!UISupervisor class reorganize!
('gui process' newProcessIfUI: newUIProcessIfNeeded spawnNewMorphicProcessFor: stopUIProcess ui userInterrupt:)
('services' isUIProcessRunning whenUIinSafeState:)
('accessing' animatedUIOf: isUIProcess: runningWorld)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Process allSubInstancesDo: [ :p | p removeActionsForEvent: #animatedUI ]!

