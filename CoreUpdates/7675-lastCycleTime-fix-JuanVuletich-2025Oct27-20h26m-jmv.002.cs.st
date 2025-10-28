'From Cuis7.5 [latest update: #7673] on 27 October 2025 at 8:26:29 pm'!

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 10/27/2025 09:30:12'!
doOneCycleNowOn: aMorphicCanvas
	"Immediately do one cycle of the interaction loop.
	Only used for a few tests."
	"See #eventTickler"
	| thisCycleTime hadAny |
	thisCycleTime := Time localMillisecondClock.
	Cursor currentCursor = (Cursor cursorAt: #waitCursor) ifTrue: [ Cursor defaultCursor activateCursor ].
	"Repair visual damage."
	self checkForNewScreenSize.
	self displayWorldOn: aMorphicCanvas.
	"Run steps, alarms and deferred UI messages."
	self runDeferredUIMessages.
	self triggerAlarmsBefore: thisCycleTime.
	self runLocalStepMethods: thisCycleTime.
	"Ensure that any affected morph is redrawn ASAP."
	self displayWorldOn: aMorphicCanvas.
	"Process user input events. Run all event triggered code."
	hadAny := activeHand processEventQueue.
	"Run any deferred UI messages that were enqueued during this cycle, perhaps
	as consequence of event handling or step methods, or sent from other processes,
	before the inter cycle pause."
	self runDeferredUIMessages.
	"Ensure that any affected morph is redrawn ASAP, before the inter cycle pause."
	self displayWorldOn: aMorphicCanvas.
	lastCycleTime := thisCycleTime.
	lastCycleHadAnyEvent := hadAny.! !

