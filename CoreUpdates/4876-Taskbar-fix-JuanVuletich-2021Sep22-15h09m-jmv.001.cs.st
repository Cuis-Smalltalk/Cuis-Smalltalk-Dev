'From Cuis 5.0 [latest update: #4875] on 22 September 2021 at 3:09:47 pm'!

!TaskbarMorph methodsFor: 'submorphs-add/remove' stamp: 'jmv 9/22/2021 15:09:15'!
noteNewOwner: aMorph
	"I have just been added as a submorph of aMorph"

	super noteNewOwner: aMorph.
	aMorph submorphsDo: [ :subMorph |
		self refreshTaskbarFor: subMorph ].
	self screenSizeChanged.! !

