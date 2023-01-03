'From Cuis 6.0 [latest update: #5618] on 3 January 2023 at 4:19:42 pm'!

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 1/3/2023 16:16:27'!
newKeyboardFocus: aMorphOrNil
	"Make the given morph the new keyboard focus, canceling the previous keyboard focus if any. If the argument is nil, the current keyboard focus is cancelled."
	| oldFocus |
	keyboardFocus == aMorphOrNil ifFalse: [
		oldFocus _ keyboardFocus.
		keyboardFocus _ aMorphOrNil.
		aMorphOrNil ifNotNil: [
			oldFocus ifNotNil: [oldFocus keyboardFocusChange: false].
			aMorphOrNil keyboardFocusChange: true]].
! !

