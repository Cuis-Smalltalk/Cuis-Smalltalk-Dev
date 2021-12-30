'From Cuis 5.0 [latest update: #5018] on 30 December 2021 at 11:33:24 am'!

!EventSensor methodsFor: 'private' stamp: 'sqr 12/30/2021 05:37:56'!
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
			0 < delta and: [delta < self class eventPollPeriod] :: ifFalse: [
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
		] on: UnhandledError 	do: [ :ex | 			ex return ]
	] repeat! !

