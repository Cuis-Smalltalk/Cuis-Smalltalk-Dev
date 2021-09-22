'From Cuis 5.0 [latest update: #4871] on 22 September 2021 at 9:35:25 am'!

!DisplayScreen methodsFor: 'other' stamp: 'jmv 4/1/2013 20:12'!
forceToScreen: aRectangle
	"Force the given rectangular section of the Display to be copied to the screen. The primitive call does nothing if the primitive is not implemented. Used when the deferUpdates flag in the virtual machine is on; see #deferUpdates:."

	self primShowRectLeft: aRectangle left
		right: aRectangle right
		top: aRectangle top
		bottom: aRectangle bottom.
! !


!DisplayScreen class methodsFor: 'screen update' stamp: 'jmv 9/22/2021 09:31:18'!
screenUpdater
	| delay |
	delay _ Delay forMilliseconds: 50.
	ScreenUpdaterSemaphore _ Semaphore new.
	Damage _ nil.
	[
		delay wait.
		ScreenUpdaterSemaphore wait.
		DisplayScreen isDisplayExtentOk ifTrue: [
			Display forceToScreen: Damage.
			ScreenUpdaterSemaphore initSignals.
			Damage _ nil ].
	] repeat! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	DisplayScreen installScreenUpdater!

