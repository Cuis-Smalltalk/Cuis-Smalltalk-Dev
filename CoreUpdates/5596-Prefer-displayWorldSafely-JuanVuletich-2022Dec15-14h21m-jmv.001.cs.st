'From Cuis 6.0 [latest update: #5595] on 15 December 2022 at 2:22:01 pm'!

!Debugger methodsFor: 'private' stamp: 'jmv 12/15/2022 12:50:55'!
resumeProcess
	| mustTerminateActive mustRedisplay |
	mustRedisplay _ self runningWorld.
	savedCursor
		ifNotNil: [savedCursor activateCursor].
	mustTerminateActive _ false.
	interruptedProcess isTerminated ifFalse: [
		(UISupervisor isUIProcessRunning and: [ UISupervisor ui == interruptedProcessUI]) ifTrue: [
			UISupervisor uiProcess: interruptedProcess.
			mustTerminateActive _ true ].
		interruptedProcess resume ].
	interruptedProcess _ nil.
	contextStackIndex _ 0.
	contextStack _ nil.
	contextStackTop _ nil.
	receiverInspector _ nil.
	contextVariablesInspector _ nil.
	mustRedisplay ifNotNil: [ :w | UISupervisor whenUIinSafeState: [ w displayWorldSafely ]].
	"restart low space handler"
	Smalltalk installLowSpaceWatcher.
	"If this process was the UI process, and we just resumed the old one,
	then terminate us and never return to caller."
	mustTerminateActive
		ifTrue: [ Processor terminateActive ]! !


!WorldMorph methodsFor: 'interaction loop' stamp: 'jmv 12/15/2022 12:50:38'!
mainLoop
	| d |

	"Needed if for some reason Cuis is started with an earlier DateTime than the image was saved.
	Might happen, especially on RasPi or other systems without an RTC"
	lastCycleTime _ Time localMillisecondClock.
	lastAlarmTime _ 0.

	canvas isNil ifTrue: [
		self setMainCanvas ].
	self displayWorldSafely.
	d _ Delay forMilliseconds: 50.
	[
		self doOneCycle: d.
		UISupervisor isUIProcessRunning ]
			whileTrue: []! !

!WorldMorph methodsFor: 'world menu' stamp: 'jmv 12/15/2022 12:50:49'!
collapseAll
	"Collapse all windows"
	(SystemWindow windowsIn: self satisfying: [ :w | w isCollapsed not ])
		reverseDo: [ :w | w collapse.  self displayWorldSafely].
	self collapseNonWindows! !


!MenuMorph methodsFor: 'modal progress dialog' stamp: 'jmv 12/15/2022 12:50:30'!
displayAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	self runningWorld ifNotNil: [ :w |
		w addMorph: self centeredNear: aPoint.
		self world ifNotNil: [ w displayWorldSafely ].  "show myself"
		].
	aBlock value.
	self delete! !

!MenuMorph methodsFor: 'modal progress dialog' stamp: 'jmv 12/15/2022 12:51:04'!
informUserAt: aPoint during: aBlock
	"Add this menu to the Morphic world during the execution of the given block."

	| w titleString |

	titleString _ titleMorph submorphs first.
	self visible: false.
	w _ self world ifNil: [ self runningWorld ].
	aBlock value: [ :string |
		self visible ifFalse: [
			w addMorph: self centeredNear: aPoint.
			self visible: true].
		titleString contents: string.
		titleMorph morphWidth: titleString morphWidth + 8.
		self morphPosition: w activeHand morphPosition.
		self adjustSubmorphsLayout.
		self redrawNeeded.
		w ifNotNil: [
			w displayWorldSafely	].	 "show myself"
	]. 
	self delete.
	w ifNotNil: [
		w displayWorldSafely ]! !

