'From Cuis7.3 [latest update: #7062] on 5 March 2025 at 10:55:00 am'!

!Morph methodsFor: 'submorphs-add/remove' stamp: 'hlsf 3/1/2025 10:17:12'!
delete
	"Remove the receiver as a submorph of its owner and make its 
	new owner be nil."

	| aWorld |
	aWorld := self world ifNil: [ self runningWorld ].
	aWorld ifNotNil: [
		aWorld activeHand ifNotNil: [ :h | h
			releaseKeyboardFocus: self;
			releaseMouseFocus: self ]].
	self privateDelete.
	self clearDependencyAndEvents.
	self allSubmorphsDo: [ :each |
		each clearDependencyAndEvents ].! !

