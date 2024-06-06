'From Cuis7.1 [latest update: #6452] on 6 June 2024 at 5:23:08 pm'!

!Character methodsFor: 'testing' stamp: 'jmv 6/6/2024 16:04:41'!
isSeparator
	"Answer whether the receiver is one of the separator characters"
	"Also see #separators"

	^ #(32 "space"
		9 "tab"
		10 "line feed"
		13 "cr"
		12 "form feed"
		160 "non-breaking space"
		8203 "Character zeroWidthSpace")
			statePointsTo: self codePoint! !


!Character class methodsFor: 'instance creation' stamp: 'jmv 6/6/2024 17:22:40'!
separators
	"Answer the separator characters"
	"Also see #isSeparator"

	^ #(32 "space"
		9 "tab"
		10 "line feed"
		13 "cr"
		12 "form feed"
		160 "non-breaking space"
		8203 "Character zeroWidthSpace")
			collect: [ :v | Character codePoint: v ]! !

