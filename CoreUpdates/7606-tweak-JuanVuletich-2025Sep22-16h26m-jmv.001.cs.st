'From Cuis7.5 [latest update: #7602] on 22 September 2025 at 4:26:49 pm'!

!CompiledMethod class methodsFor: 'preferences' stamp: 'jmv 9/22/2025 16:26:29'!
useSistaBytecodes
	"
	CompiledMethod useSistaBytecodes.

		| nPrimary nSecondary |
		Smalltalk garbageCollect.
		nPrimary := nSecondary := 0.
		CompiledMethod allSubInstancesDo:
			[:cm|
			cm header >= 0
				ifTrue: [nPrimary := nPrimary + 1]
				ifFalse: [nSecondary := nSecondary + 1]].
		{nPrimary. nSecondary} print
	"
	self preferredBytecodeSetEncoderClass: EncoderForSistaV1.
	Compiler recompileAll.
	ProcessorScheduler startUp.
	Delay startTimerEventLoop.
	EphemeronFinalizationProcess restartFinalizationProcess.
	Sensor installInterruptWatcher.
	Sensor installEventTickler.
	UISupervisor ui ifNotNil: [ :w |
		[
			w showTaskbar.
			UISupervisor newUIProcess.
			Smalltalk garbageCollect ] fork.
		w
			initSortBlocks;
			hideTaskbar ].
	PreferenceSet installMiscPreferences.
	Smalltalk garbageCollect.! !

