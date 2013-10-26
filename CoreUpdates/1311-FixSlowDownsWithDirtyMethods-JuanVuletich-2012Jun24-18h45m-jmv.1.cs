'From Cuis 4.0 of 21 April 2012 [latest update: #1310] on 24 June 2012 at 6:47:53 pm'!

!CodeProvider methodsFor: 'self-updating' stamp: 'jmv 6/24/2012 18:35'!
updateListsAndCodeIn: aWindow
	"This is done because we are not the real model (i.e. Smalltalk). Some other browser might change our contents, without our model knowing it."
	aWindow canDiscardEdits ifTrue: [
		aWindow updatablePanes do: [ :aPane | aPane verifyContents ].
		self updatePaneIfNeeded ]! !

