'From Cuis 5.0 [latest update: #5007] on 29 December 2021 at 10:14:33 am'!

!CPUWatcher methodsFor: 'startup-shutdown' stamp: 'jmv 12/29/2021 10:07:04'!
monitorProcessPeriod: secs sampleRate: msecs suspendPorcine: aBoolean
	| delay |
	self stopMonitoring.
	watcher _ [
		delay _ Delay forMilliseconds: msecs truncated.
		[
			| thisTally |
			thisTally _ IdentityBag new: 20.
			secs * 1000 // msecs timesRepeat: [
				"Could happen if process is halted and resumed."
				delay beingWaitedOn ifTrue: [ delay _ Delay forMilliseconds: delay delayDuration ].
				delay wait.
				thisTally add: Processor nextReadyProcess ].
			tally _ thisTally.
			aBoolean ifTrue: [ self findThePig ]
		] repeat
	] newProcess.
	watcher
		priority: Processor highestPriority;
		name: 'CPUWatcher monitor';
		resume.
	Processor yield.! !


!EventSensor methodsFor: 'private' stamp: 'sqr 12/29/2021 10:01:18'!
eventTickler
	"If the UI process hasn't processed events in a while, do it here.
	This is needed to detect the interrupt key."

	| delay |
	delay _ Delay forMilliseconds: self class eventPollPeriod.
	[
		[
			| delta |
			"Could happen if process is halted and resumed."
			delay beingWaitedOn ifTrue: [ delay _ Delay forMilliseconds: delay delayDuration ].
			delay wait.
			delta _ Time localMillisecondClock - self lastEventPoll.
			delta >= self class eventPollPeriod or: [delta <= 0] :: ifTrue: [
				"See #doOneCycleNowFor:"
				Cursor currentCursor = Cursor defaultCursor
					ifTrue: [ (Cursor cursorAt: #waitCursor) activateCursor ].
				"Discard any mouse events. This code is run when the UI is slow, essentially to have a working
				interrupt key. Processing mouse events is pointless: the UI will not handle them anyway.
				In addition, at least on Windows 7, when the machine is suspended and resumed with Cuis
				running, a lot of meaningless mouseMove events with the same coordinates are sent, maing
				Cuis extremely slow and CPU hungry for a few minutes without reason. Discarding mouse
				events makes the 'processing' of those very quick."
				self fetchMoreEventsDiscardingMouseEvents: true ]
		] on: UnhandledError do: [ :ex | ex return ]
	] repeat! !


!DisplayScreen class methodsFor: 'screen update' stamp: 'jmv 12/29/2021 10:08:50'!
screenUpdater
	| delay |
	delay _ Delay forMilliseconds: 50.
	ScreenUpdaterSemaphore _ Semaphore new.
	Damage _ nil.
	[
		"Could happen if process is halted and resumed."
		delay beingWaitedOn ifTrue: [ delay _ Delay forMilliseconds: delay delayDuration ].
		delay wait.
		ScreenUpdaterSemaphore wait.
		DisplayScreen isDisplayExtentOk ifTrue: [
			Display forceToScreen: Damage.
			ScreenUpdaterSemaphore initSignals.
			Damage _ nil ].
	] repeat! !


!StackSizeWatcher methodsFor: 'private' stamp: 'jmv 12/29/2021 10:07:47'!
watch

	| processToWatch |

	"Could happen if process is halted and resumed."
	delayBetweenChecks beingWaitedOn ifTrue: [
		delayBetweenChecks _ Delay forMilliseconds: delayBetweenChecks delayDuration ].
	delayBetweenChecks wait.
	processToWatch := Processor nextReadyProcess.
	(self shouldStopAndDebug: processToWatch) ifTrue: [ self debug: processToWatch ]
! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
EventSensor install.
DisplayScreen installScreenUpdater.
CPUWatcher stopMonitoring; startMonitoring.
Delay startTimerEventLoop.!

