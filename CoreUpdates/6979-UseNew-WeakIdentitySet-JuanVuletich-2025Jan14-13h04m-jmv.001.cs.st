'From Cuis7.3 [latest update: #6978] on 14 January 2025 at 1:07:30 pm'!

!WorldMorph methodsFor: 'errors on draw' stamp: 'jmv 1/13/2025 17:03:05'!
removeAllKnownFailing
	drawingFailingMorphs do: [ :m | m redrawNeeded ].
	drawingFailingMorphs := WeakIdentitySet new.
! !

!WorldMorph methodsFor: 'initialization' stamp: 'jmv 1/13/2025 17:02:57'!
initialize

	super initialize.
	activeHand := HandMorph new.
	damageRecorder := DamageRecorder new.
	stepList := Heap sortBlock: self stepListSortBlock.
	alarms := Heap sortBlock: self alarmSortBlock.
	lastAlarmTime := 0.
	deferredUIMessages := SharedQueue new.
	drawingFailingMorphs := WeakIdentitySet new.
	pause := 20.
	lastCycleTime := Time localMillisecondClock.
	lastCycleHadAnyEvent := false! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
WorldMorph allInstancesDo: [ :w | w removeAllKnownFailing ].
Smalltalk garbageCollect.!

