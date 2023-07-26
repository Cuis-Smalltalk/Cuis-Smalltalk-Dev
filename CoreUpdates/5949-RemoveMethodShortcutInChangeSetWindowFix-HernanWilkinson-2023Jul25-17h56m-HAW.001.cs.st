'From Cuis 6.0 [latest update: #5948] on 25 July 2023 at 7:32:07 pm'!

!ChangeSorterWindow methodsFor: 'keyboard shortcuts' stamp: 'jmv 7/3/2023 12:11:32'!
messageListKey: aChar from: view
	"Respond to a Command key in the message-list pane."

	self flag: #definesKeyboardShortcuts.
	aChar = $d ifTrue: [^ model forget].
	super messageListKey: aChar from: view! !

