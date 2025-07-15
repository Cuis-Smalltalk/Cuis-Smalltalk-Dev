'From Cuis7.5 [latest update: #7353] on 15 July 2025 at 10:14:53 am'!

!CompiledMethod class methodsFor: 'preferences' stamp: 'jmv 7/15/2025 09:50:34'!
useSistaBytecodes
	"
	CompiledMethod useSistaBytecodes.

		| nPrimary nSecondary |
		nPrimary := nSecondary := 0.
		self allSubInstancesDo:
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

!CompiledMethod class methodsFor: 'preferences' stamp: 'jmv 7/15/2025 09:55:43'!
useV3Bytecodes
	"
	CompiledMethod useV3Bytecodes.

		| nPrimary nSecondary |
		nPrimary := nSecondary := 0.
		self allSubInstancesDo:
			[:cm|
			cm header >= 0
				ifTrue: [nPrimary := nPrimary + 1]
				ifFalse: [nSecondary := nSecondary + 1]].
		{nPrimary. nSecondary} print
	"
	self preferredBytecodeSetEncoderClass: EncoderForV3PlusClosures.
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


!CompiledMethod class methodsFor: 'preferences' stamp: 'jmv 7/15/2025 08:49:52'!
preferredBytecodeSetEncoderClass: aBytecodeEncoderSubclass
	"Set the class that determines the bytecode set used to compile methods with.

		| nPrimary nSecondary |
		nPrimary := nSecondary := 0.
		self allSubInstancesDo:
			[:cm|
			cm header >= 0
				ifTrue: [nPrimary := nPrimary + 1]
				ifFalse: [nSecondary := nSecondary + 1]].
		{nPrimary. nSecondary} print
	"
	| nPrimary nSecondary |
	self assert: (aBytecodeEncoderSubclass includesBehavior: BytecodeEncoder).
	(aBytecodeEncoderSubclass == PrimaryBytecodeSetEncoderClass
	 or: [aBytecodeEncoderSubclass == SecondaryBytecodeSetEncoderClass]) ifTrue:
		[PreferredBytecodeSetEncoderClass := aBytecodeEncoderSubclass.
		 ^self].
	nPrimary := nSecondary := 0.
	self allSubInstancesDo:
		[:cm|
		 cm header >= 0
			ifTrue: [nPrimary := nPrimary + 1]
			ifFalse: [nSecondary := nSecondary + 1]].
	nPrimary = 0 ifTrue:
		[self installPrimaryBytecodeSet: aBytecodeEncoderSubclass.
		 ^self preferredBytecodeSetEncoderClass: aBytecodeEncoderSubclass].
	nSecondary = 0 ifTrue:
		[self installSecondaryBytecodeSet: aBytecodeEncoderSubclass.
		 ^self preferredBytecodeSetEncoderClass: aBytecodeEncoderSubclass].
	self error: 'Cannot set preferred bytecode set.  Both of the current sets appear to be in use.'! !

