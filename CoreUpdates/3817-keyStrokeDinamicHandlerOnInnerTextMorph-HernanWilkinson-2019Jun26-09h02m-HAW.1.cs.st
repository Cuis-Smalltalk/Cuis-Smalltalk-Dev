'From Cuis 5.0 of 7 November 2016 [latest update: #3805] on 26 June 2019 at 9:02:51 am'!

!InnerTextMorph methodsFor: 'events' stamp: 'HAW 6/26/2019 09:02:18'!
keyStroke: aKeyboardEvent

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].

	autoCompleter 
		ifNil: [ self processKeyStroke: aKeyboardEvent ]
		ifNotNil: [
			autoCompleter
				autoCompletionAround: [ self processKeyStroke: aKeyboardEvent ]
				keyStroke: aKeyboardEvent ].
			
	super keyStroke: aKeyboardEvent! !

