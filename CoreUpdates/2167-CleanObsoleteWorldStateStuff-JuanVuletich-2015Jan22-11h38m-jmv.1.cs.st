'From Cuis 4.2 of 25 July 2013 [latest update: #2166] on 22 January 2015 at 11:39:53.786444 am'!

!SystemDictionary methodsFor: 'housekeeping' stamp: 'jmv 1/22/2015 11:39'!
someCleanup
	"
	Smalltalk someCleanup
	Sometimes ueful, for example, prior to saving an image.
	"
	Delay startTimerEventLoop.
	Smalltalk garbageCollect! !


!Utilities class methodsFor: 'closure support' stamp: 'jmv 1/22/2015 11:39'!
postRecompileCleanup	"Utilities postRecompileCleanup"
	"Cleanup after loading closure bootstrap"
	"Before doing this, please start a new UI process (for example, by hitting alt-period and closing the debugger)."
	| unboundMethods contexts |
	self runningWorld ifNotNil: [ :w | w removeAllKnownFailing ].
	ProcessorScheduler startUp.
	WeakArray restartFinalizationProcess.
	MethodChangeRecord allInstancesDo:[:x| x noteNewMethod: nil].
	Smalltalk cleanOutUndeclared.
	Delay startTimerEventLoop.
	EventSensor install.
	Workspace allInstancesDo:[:ws| ws initializeBindings].
	Smalltalk garbageCollect.
	Smalltalk
		at: #DebuggerMethodMap
		ifPresent: [ :dmm | dmm voidMapCache ].
	Smalltalk garbageCollect.
	unboundMethods _ CompiledMethod unboundMethods.
	unboundMethods notEmpty ifTrue: [
		unboundMethods inspectWithLabel: 'Unbound Methods'].
	Smalltalk at: #BlockContext ifPresent: [ :bc |
		contexts _ bc allInstances.
		contexts ifNotEmpty: [
			contexts inspect. 
			self inform: 'There are left-over BlockContexts'.
			^self ]].
	unboundMethods isEmpty ifTrue:[
		self inform:'Congratulations - The bootstrap is now complete.'.
	]! !

!methodRemoval: WorldState #convertAlarms!
WorldState removeSelector: #convertAlarms!
!methodRemoval: WorldState #convertStepList!
WorldState removeSelector: #convertStepList!

!WorldState reorganize!
('alarms' addAlarm:withArguments:for:at: adjustAlarmTimes: alarmSortBlock alarms removeAlarm:for: triggerAlarmsBefore:)
('canvas' canvas clearCanvas doFullRepaint ensureNonDisplayCanvas recordDamagedRect: setCanvas: viewBox viewBox:)
('hands' activeHand hands handsDo: handsReverseDo: selectHandsToDrawForDamage:)
('initialization' clearWaitDelay initialize stepListSortBlock world:)
('stepping' cleanseStepListForWorld: runLocalStepMethodsIn: runStepMethodsIn: startStepping:at:selector:stepTime: stopStepping:selector: stopSteppingMorph:)
('update cycle' checkIfUpdateNeeded displayWorldSafely: doOneCycleFor: doOneCycleNowFor: handleFatalDrawingError: tryDeferredUpdatingAndSetCanvasFor:)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('drawing' displayWorld:submorphs: drawInvalidAreasWorld:submorphs: simpleDrawInvalidAreasWorld:submorphs:)
!

