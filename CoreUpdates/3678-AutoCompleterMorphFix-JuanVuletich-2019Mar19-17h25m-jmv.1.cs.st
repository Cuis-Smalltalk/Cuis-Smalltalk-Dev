'From Cuis 5.0 of 7 November 2016 [latest update: #3677] on 19 March 2019 at 5:25:59 pm'!

!AutoCompleterMorph methodsFor: 'events' stamp: 'jmv 3/19/2019 17:25:05'!
mouseEnter: evt
	self activeHand newKeyboardFocus: completer textMorph.
	^ super mouseEnter: evt! !

!AutoCompleterMorph methodsFor: 'event handling testing' stamp: 'jmv 3/19/2019 17:25:18'!
handlesMouseOver: evt
	"Do I want to receive mouseEnter: and mouseLeave: when the button is up and the hand is empty?" 
	^true! !

