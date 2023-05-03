'From Cuis 6.0 [latest update: #5783] on 3 May 2023 at 5:11:44 pm'!

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 5/3/2023 17:09:26'!
doOneCycleNow
	"Immediately do one cycle of the interaction loop.
	Only used for a few tests."
	"See #eventTickler"
	| hadAny |
	lastCycleTime := Time localMillisecondClock.
	Cursor currentCursor = (Cursor cursorAt: #waitCursor) ifTrue: [ Cursor defaultCursor activateCursor ].
	"Repair visual damage."
	self checkForNewScreenSize.
	canvas ensureCurrentMorphIsWorld.
	self displayWorld.
	"Run steps, alarms and deferred UI messages."
	self runDeferredUIMessages.
	self triggerAlarmsBefore: lastCycleTime.
	self runLocalStepMethods: lastCycleTime.
	"Ensure that any affected morph is redrawn ASAP."
	self displayWorld.
	"Process user input events. Run all event triggered code."
	hadAny := false.
	self handsDo: [ :h |
		activeHand := h.
		hadAny := hadAny | h processEventQueue ].
	"The default is the primary hand"
	activeHand := self hands first.
	"Run any deferred UI messages that were enqueued during this cycle, perhaps
	as consequence of event handling or step methods, or sent from other processes,
	before the inter cycle pause."
	self runDeferredUIMessages.
	"Ensure that any affected morph is redrawn ASAP, before the inter cycle pause."
	self displayWorld.
	lastCycleHadAnyEvent := hadAny.! !

