'From Cuis7.1 [latest update: #6610] on 20 August 2024 at 5:12:22 pm'!

!MenuMorph methodsFor: 'control' stamp: 'jmv 8/20/2024 17:10:51'!
deleteIfPopUp: evt
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu."

	| h |
	h := evt ifNotNil: [ evt hand ].
	h ifNil: [
		self world ifNotNil: [ :w | h := h activeHand ]].
	h ifNil: [ h := self activeHand ].
	h ifNotNil: [
		h keyboardFocus == self ifTrue: [
			h newKeyboardFocus: prevKbdFocus ].
		h mouseFocus == self ifTrue: [
			"h newMouseFocus: prevMouseFocus."
			h newMouseFocus: nil ]].
	stayUp ifFalse: [
		self delete.
		popUpOwner ifNotNil: [
			popUpOwner isSelected: false.
			popUpOwner deleteIfPopUp: evt ]].! !

!MenuMorph methodsFor: 'halos and balloon help' stamp: 'jmv 8/20/2024 17:02:27'!
addHalo: evt
	self stayUp.
	self world ifNotNil: [ :w | w activeHand ifNotNil: [ :h | h releaseMouseFocus: self ]].
	super addHalo: evt.! !

