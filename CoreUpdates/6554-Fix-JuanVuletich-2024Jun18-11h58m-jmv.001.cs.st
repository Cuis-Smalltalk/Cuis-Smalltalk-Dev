'From Cuis7.1 [latest update: #6483] on 18 June 2024 at 12:01:35 pm'!

!TextEditor methodsFor: 'menu messages' stamp: 'jmv 6/18/2024 12:00:50'!
find
	"Prompt the user for a string to search for, and search the receiver from the current selection onward for it.  1/26/96 sw"

	self
		request: 'Find what?'
		initialAnswer: self class findText
		do: [:aString|
			aString isEmpty ifFalse: [
				"Set focus on our text morph, so that cmd-g does the search again"
				morph world activeHand newKeyboardFocus: morph.
				self setSearch: aString.
				ChangeText := self class findText.  "Implies no replacement to againOnce: method"
				morph handleInteraction: [
					(self findAndReplaceMany: false)
						ifFalse: [ self flash ]].
				morph scrollSelectionIntoView ]].
! !

!TextEditor methodsFor: 'menu messages' stamp: 'jmv 6/18/2024 12:00:47'!
findAgain
	"Find the text-to-find again.  1/24/96 sw"

	morph handleInteraction: [
		(self findAndReplaceMany: false)
			ifFalse: [ self flash ]].
	morph scrollSelectionIntoView.
! !

