'From Cuis7.5 [latest update: #7336] on 8 July 2025 at 11:40:14 am'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 7/8/2025 09:33:40'!
delete
	"Remove the receiver as a submorph of its owner and make its 
	new owner be nil."

	| aWorld |
	aWorld := self world ifNil: [ self runningWorld ].
	aWorld ifNotNil: [
		aWorld activeHand ifNotNil: [ :h | h
			releaseKeyboardFocus: self;
			releaseMouseFocus: self ]].
	self clearDependencyAndEvents.
	self allSubmorphsDo: [ :each |
		each clearDependencyAndEvents ].
	self privateDelete.! !

