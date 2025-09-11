'From Cuis7.5 [latest update: #7547] on 11 September 2025 at 9:12:13 am'!

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 9/9/2025 17:26:13'!
keyboardFocusChange: aBoolean
	"Notify change due to green border for keyboard focus"

	"We are losing focus"
	| newKeyboardFocus |
	aBoolean ifFalse: [
		newKeyboardFocus := self world activeHand keyboardFocus.
		(newKeyboardFocus ~~ prevKbdFocus and: [
			newKeyboardFocus notNil and: [
				(newKeyboardFocus hasOwner: self) not ]])
		ifTrue: [
			self deleteIfPopUp: nil.
			self redrawNeeded ]].! !

