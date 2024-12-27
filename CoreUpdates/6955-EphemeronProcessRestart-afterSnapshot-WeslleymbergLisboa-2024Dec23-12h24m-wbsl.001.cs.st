'From Cuis7.3 [latest update: #6947] on 23 December 2024 at 6:47:53 pm'!

!EphemeronFinalizationProcess class methodsFor: 'system startup' stamp: 'wbsl 12/23/2024 18:10:14'!
startUp: resuming
	"resuming ifFalse: [ ^self ]."
	"Must restart even if just continuing after a snapshot"
	self restartFinalizationProcess! !

