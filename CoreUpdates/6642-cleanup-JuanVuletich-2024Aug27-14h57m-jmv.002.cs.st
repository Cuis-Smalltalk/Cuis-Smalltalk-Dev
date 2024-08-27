'From Cuis7.1 [latest update: #6643] on 27 August 2024 at 2:57:48 pm'!

!HandMorph methodsFor: 'halo handling' stamp: 'jmv 8/27/2024 14:20:28'!
obtainHalo: aHalo
	"Used for transfering halos between hands"

	self halo == aHalo ifTrue: [^self].
	"Find former owner"
	self world handsDo: [ :h |
		h halo == aHalo ifTrue: [h releaseHalo: aHalo]].
	self halo: aHalo.! !


!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 8/27/2024 14:17:56'!
doOneCycleNowOn: aMorphicCanvas
	"Immediately do one cycle of the interaction loop.
	Only used for a few tests."
	"See #eventTickler"
	| hadAny |
	lastCycleTime := Time localMillisecondClock.
	Cursor currentCursor = (Cursor cursorAt: #waitCursor) ifTrue: [ Cursor defaultCursor activateCursor ].
	"Repair visual damage."
	self checkForNewScreenSize.
	self displayWorldOn: aMorphicCanvas.
	"Run steps, alarms and deferred UI messages."
	self runDeferredUIMessages.
	self triggerAlarmsBefore: lastCycleTime.
	self runLocalStepMethods: lastCycleTime.
	"Ensure that any affected morph is redrawn ASAP."
	self displayWorldOn: aMorphicCanvas.
	"Process user input events. Run all event triggered code."
	hadAny := false.
	"The default is the first hand"
	self handsReverseDo: [ :h |
		activeHand := h.
		hadAny := hadAny | h processEventQueue ].
	"Run any deferred UI messages that were enqueued during this cycle, perhaps
	as consequence of event handling or step methods, or sent from other processes,
	before the inter cycle pause."
	self runDeferredUIMessages.
	"Ensure that any affected morph is redrawn ASAP, before the inter cycle pause."
	self displayWorldOn: aMorphicCanvas.
	lastCycleHadAnyEvent := hadAny.! !

