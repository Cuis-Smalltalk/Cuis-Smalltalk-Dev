'From Cuis7.1 [latest update: #6455] on 6 June 2024 at 6:56:22 pm'!

!MenuMorph methodsFor: 'control' stamp: 'jmv 6/6/2024 18:53:31'!
deleteIfPopUp: evt
	"Remove this menu from the screen if stayUp is not true. If it is a submenu, also remove its owning menu."

	| h |
	stayUp ifFalse: [ 
		h := evt ifNotNil: [ evt hand ].
		h ifNil: [
			self world ifNotNil: [ :w | h := h activeHand ]].
		h ifNil: [ h := self activeHand ].
		self delete.
		h ifNotNil: [
			h newKeyboardFocus: prevKbdFocus.
			"h newMouseFocus: prevMouseFocus."
			h newMouseFocus: nil.
			].
		popUpOwner ifNotNil: [
			popUpOwner isSelected: false.
			popUpOwner deleteIfPopUp: evt ]].! !

