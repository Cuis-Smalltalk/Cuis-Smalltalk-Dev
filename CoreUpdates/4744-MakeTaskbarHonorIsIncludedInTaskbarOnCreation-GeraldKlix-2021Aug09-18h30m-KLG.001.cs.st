'From Haver 5.0 [latest update: #4743] on 10 August 2021 at 10:32:48 am'!

!TaskbarMorph methodsFor: 'submorphs-add/remove' stamp: 'KLG 8/10/2021 10:29:46'!
noteNewOwner: aMorph
	"I have just been added as a submorph of aMorph"

	super noteNewOwner: aMorph.
	aMorph submorphsDo: [ :subMorph |
		subMorph isIncludedInTaskbar ifTrue: [
			self addButtonFor: subMorph ] ].
	self notifyDisplayResize! !

