'From Cuis7.5 [latest update: #7566] on 13 September 2025 at 5:29:11 pm'!

!MenuMorph methodsFor: 'keyboard control' stamp: 'jmv 9/13/2025 15:58:25'!
keyboardFocusChange: aBoolean
	"Notify change due to green border for keyboard focus"

	"We are losing focus"
	| newKeyboardFocus |
	aBoolean ifFalse: [
		newKeyboardFocus := self world activeHand keyboardFocus.
		(newKeyboardFocus ~~ prevKbdFocus and: [
			newKeyboardFocus notNil and: [
				(newKeyboardFocus hasOwner: self) not and: [
					newKeyboardFocus owner ~~ activeSubMenu ]]])
		ifTrue: [
			self deleteIfPopUp: nil.
			self redrawNeeded ]].! !

