'From Cuis7.1 [latest update: #6820] on 19 November 2024 at 5:02:09 pm'!

!TextEditor methodsFor: 'menu messages' stamp: 'jfr 8/19/2024 10:00:53'!
find: aString
	"Prompt the user for a string to search for, and search the receiver from the current selection onward for it.  1/26/96 sw"

	aString isEmpty ifFalse: [
		"Set focus on our text morph, so that cmd-g does the search again"
		morph world activeHand newKeyboardFocus: morph.
		self setSearch: aString.
		ChangeText := self class findText.  "Implies no replacement to againOnce: method"
		morph handleInteraction: [
			(self findAndReplaceMany: false)
				ifFalse: [ self flash ]].
		morph scrollSelectionIntoView ].
! !


!TextEditor methodsFor: 'menu messages' stamp: 'jmv 11/19/2024 15:46:03'!
find
	"Prompt the user for a string to search for, and search the receiver from the current selection onward for it.  1/26/96 sw"

	self
		request: 'Find what?'
		initialAnswer: self class findText
		do: [ :aString | self find: aString ].
! !

