'From Cuis7.9 [latest update: #8395] on 24 November 2026 at 2:28:22 pm'!

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 24/Nov/2026, 4:42:54 pm (UTC)'!
noteNewOwner: aMorph
	"I have just been added as a submorph of aMorph"

	self taskbar ifNotNil: [ :tb |
		tb refreshTaskbarFor: self ].! !


!TaskbarMorph methodsFor: 'services' stamp: 'jmv 24/Nov/2026, 5:27:52 pm (UTC)'!
refreshTaskbarFor: aMorph

	(aMorph isIncludedInTaskbar and: [ aMorph owner == self world ])
		ifTrue: [
			(self buttonFor: aMorph) ifNil: [self addButtonFor: aMorph ]]
		ifFalse: [ self removeButtonFor: aMorph ].! !

