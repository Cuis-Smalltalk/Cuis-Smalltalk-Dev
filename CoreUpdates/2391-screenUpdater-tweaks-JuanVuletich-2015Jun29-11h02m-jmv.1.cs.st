'From Cuis 4.2 of 25 July 2013 [latest update: #2390] on 29 June 2015 at 11:03:27.783265 am'!

!DisplayScreen class methodsFor: 'screen update' stamp: 'jmv 6/29/2015 11:02'!
screenUpdateRequired: aRectangle
	
	Damage _ aRectangle quickMerge: Damage.
	ScreenUpdaterSemaphore signal! !

!DisplayScreen class methodsFor: 'screen update' stamp: 'jmv 6/29/2015 11:02'!
screenUpdater
	| delay |
	delay _ Delay forMilliseconds: 50.
	ScreenUpdaterSemaphore _ Semaphore new.
	Damage _ nil.
	[
		ScreenUpdaterSemaphore initSignals; wait.
		Display forceToScreen: Damage.
		Damage _ nil.
		delay wait.
	] repeat! !


!Transcript class methodsFor: 'preferred protocol' stamp: 'jmv 6/29/2015 08:38'!
bounds: aRectangle

	innerRectangle _ aRectangle rounded insetBy: self padding! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	DisplayScreen installScreenUpdater!

