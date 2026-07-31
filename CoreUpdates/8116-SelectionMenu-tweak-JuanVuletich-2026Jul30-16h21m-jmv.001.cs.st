'From Cuis7.9 [latest update: #8115] on 30 July 2026 at 4:22:06 pm'!

!SelectionMenu class methodsFor: 'instance creation' stamp: 'jmv 30/Jul/2026, 6:54:00 pm (UTC)'!
selections: aList
	"Answer an instance of me whose labels and selections are identical.  "
	"
	(SelectionMenu selections: { '<on>just'. '<on>a'. '<off>test' }) startUpMenu print
	"
	^ self selections: aList lines: nil! !

