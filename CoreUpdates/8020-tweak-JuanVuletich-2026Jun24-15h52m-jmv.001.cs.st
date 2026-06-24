'From Cuis7.9 [latest update: #8017] on 24 June 2026 at 3:53:38 pm'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 24/Jun/2026, 2:30:58 pm (UTC)'!
delete
	"Remove the receiver as a submorph of its owner and make its 
	new owner be nil."

	self world ifNotNil: [ :w |
		w activeHand ifNotNil: [ :h | h
			releaseKeyboardFocusIfOrOwnedBy: self;
			releaseMouseFocus: self ]].
	self clearDependencyAndEvents.
	self allSubmorphsDo: [ :each |
		each clearDependencyAndEvents ].
	self privateDelete.! !

