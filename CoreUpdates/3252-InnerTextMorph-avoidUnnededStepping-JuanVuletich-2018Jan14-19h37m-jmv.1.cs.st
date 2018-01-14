'From Cuis 5.0 of 7 November 2016 [latest update: #3251] on 14 January 2018 at 7:37:37 pm'!

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 1/14/2018 19:19:58'!
keyboardFocusChange: aBoolean
	
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw border feedback"
	aBoolean
		ifTrue: [
			"A hand is wanting to send us characters..."
			editor ifNil: [ self editor storeSelectionInComposition ].	"Forces install"
			self showsBlinkingCursor ifTrue: [
				editor hasSelection ifFalse: [
					self startBlinking ]]]
		ifFalse: [ self stopBlinking ].
	"Selection might be shown differently when focused"
	owner
		ifNotNil: [ owner redrawNeeded ]
		ifNil: [ self redrawNeeded ]		"Or at least redraw us"! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 1/14/2018 19:14:30'!
selectionChanged

	self textComposition selectionRects do: [ :r | self invalidateLocalRect: r ].
	editor hasSelection
		ifTrue: [ self stopBlinking ]
		ifFalse: [ self startBlinking ]! !

