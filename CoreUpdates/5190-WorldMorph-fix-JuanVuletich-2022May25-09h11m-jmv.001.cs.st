'From Cuis 6.0 [latest update: #5189] on 25 May 2022 at 9:11:24 am'!

!WorldMorph methodsFor: 'update cycle' stamp: 'jmv 5/25/2022 09:11:08'!
doOneCycleNow
	"Immediately do one cycle of the interaction loop.
	Only used for a few tests."
	"See #eventTickler"
	| hadAny |
	Cursor currentCursor = (Cursor cursorAt: #waitCursor) ifTrue: [ Cursor defaultCursor activateCursor ].
	"Repair visual damage."
	self checkForNewScreenSize.
	self displayWorldSafely.
	"Run steps, alarms and deferred UI messages"
	self runStepMethods.
	"Process user input events. Run all event triggered code."
	hadAny _ false.
	self handsDo: [ :h |
		activeHand _ h.
		hadAny _ hadAny | h processEventQueue ].
	"The default is the primary hand"
	activeHand _ self hands first.
	^ hadAny.! !

