'From Cuis 5.0 [latest update: #4476] on 24 November 2020 at 3:39:52 pm'!

!TextEditor methodsFor: 'menu messages' stamp: 'jmv 11/24/2020 15:39:08'!
find
	"Prompt the user for a string to search for, and search the receiver from the current selection onward for it.  1/26/96 sw"

	self
		request: 'Find what?'
		initialAnswer: self class findText
		do: [:aString|
			aString isEmpty ifFalse:
				["Set focus on our text morph, so that cmd-g does the search again"
				morph world activeHand newKeyboardFocus: morph.
				self setSearch: aString.
				ChangeText _ self class findText.  "Implies no replacement to againOnce: method"
				(self findAndReplaceMany: false)
					ifFalse: [ self flash ].
				morph scrollSelectionIntoView ]].

"	morph installEditorToReplace: self"! !

