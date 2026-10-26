'From Cuis7.9 [latest update: #8279] on 26 October 2026 at 10:54:33 am'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 26/Oct/2026, 1:53:23 pm (UTC)'!
delete
	"Remove the receiver as a submorph of its owner and make its 
	new owner be nil."

	| activeHand |
	self world ifNotNil: [ :w |
		w activeHand ifNotNil: [ :h |
			activeHand := h.
			h
				releaseKeyboardFocusIfOrOwnedBy: self;
				releaseMouseFocus: self ]].
	self clearDependencyAndEvents.
	self allSubmorphsDo: [ :each |
		each clearDependencyAndEvents ].
	self privateDelete.
	
	activeHand ifNotNil: [
		activeHand dispatchMouseOverEvent ].! !

