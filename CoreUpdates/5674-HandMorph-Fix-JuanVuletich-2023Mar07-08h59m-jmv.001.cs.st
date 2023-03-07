'From Cuis 6.0 [latest update: #5673] on 7 March 2023 at 9:02:16 am'!

!HandMorph methodsFor: 'focus handling' stamp: 'jmv 3/7/2023 09:01:21'!
newKeyboardFocus: aMorphOrNil
	"Make the given morph the new keyboard focus, canceling the previous keyboard focus if any. If the argument is nil, the current keyboard focus is cancelled."
	| oldFocus |
	keyboardFocus == aMorphOrNil ifFalse: [
		oldFocus _ keyboardFocus.
		keyboardFocus _ aMorphOrNil.
		oldFocus ifNotNil: [oldFocus keyboardFocusChange: false].
		aMorphOrNil ifNotNil: [aMorphOrNil keyboardFocusChange: true]].! !

