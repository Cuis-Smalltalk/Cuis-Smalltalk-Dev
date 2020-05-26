'From Cuis 5.0 [latest update: #4188] on 26 May 2020 at 11:42:00 am'!

!StringRequestMorph class methodsFor: 'private' stamp: 'KenD 5/25/2020 12:37:58'!
deltaToTextPane
	"Answer a distance to translate an instance of the receiver by when it is opened in the world in order to have the hand be over the text pane (so the text pane has focus).
	Distance is relative to font size"
	| e |
	e _ Preferences windowTitleFont lineSpacing.
	^ (2 * e)@(1.5 * e)! !

