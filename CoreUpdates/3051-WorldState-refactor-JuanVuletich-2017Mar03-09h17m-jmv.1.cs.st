'From Cuis 5.0 of 7 November 2016 [latest update: #3049] on 3 March 2017 at 9:22:19 am'!

!PasteUpMorph methodsFor: 'interaction loop' stamp: 'jmv 3/2/2017 19:58:01'!
doOneCycleNow
	"see the comment in WorldState >> doOneCycleNow
	Only used for a few tests."
	worldState doOneCycleNow! !


!WorldState methodsFor: 'update cycle' stamp: 'jmv 3/3/2017 09:21:41'!
doOneCycleNow
	"Immediately do one cycle of the interaction loop."

	| hadAnyEvent |
	"See #eventTickler"
	Cursor currentCursor = Cursor wait ifTrue: [
		Cursor normal activateCursor ].

	"Repair visual damage."
	DisplayScreen checkForNewScreenSize.
	self displayWorldSafely.

	"Run steps, alarms and deferred UI messages"
	self runStepMethods.

	"Process user input events. Run all event triggered code."
	hadAnyEvent _ false.
	self handsDo: [ :h |
		activeHand _ h.
		hadAnyEvent _ hadAnyEvent | h processEventQueue.
		activeHand _ nil ].

	"The default is the primary hand"
	activeHand _ self hands first.


	^hadAnyEvent! !

!methodRemoval: PasteUpMorph #runStepMethods!
PasteUpMorph removeSelector: #runStepMethods!
