'From Cuis7.5 [latest update: #7554] on 11 September 2025 at 12:41:51 pm'!

!TextModelMorph methodsFor: 'focus handling' stamp: 'jmv 9/11/2025 12:40:20'!
hasKeyboardFocus
	"For all practical purposes, our inner text having focus means us having focus. No two separated concepts."
	^super hasKeyboardFocus or: [ scroller hasKeyboardFocus ]! !

