'From Cuis 4.2 of 25 July 2013 [latest update: #2433] on 27 July 2015 at 5:01:17.098738 pm'!

!DisplayScreen class methodsFor: 'screen update' stamp: 'jmv 7/27/2015 16:57'!
screenUpdater
	| delay |
	delay _ Delay forMilliseconds: 50.
	ScreenUpdaterSemaphore _ Semaphore new.
	Damage _ nil.
	[
		ScreenUpdaterSemaphore initSignals; wait.
		delay wait.
		Display forceToScreen: Damage.
		Damage _ nil.
	] repeat! !


!DisplayScreen class methodsFor: 'screen update' stamp: 'jmv 7/27/2015 17:07'!
screenUpdateRequired: aRectangle
	
	Damage _ aRectangle
		ifNotNil: [ aRectangle quickMerge: Damage ]
		ifNil: [ Display boundingBox ].
	ScreenUpdaterSemaphore signal! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
DisplayScreen installScreenUpdater!

