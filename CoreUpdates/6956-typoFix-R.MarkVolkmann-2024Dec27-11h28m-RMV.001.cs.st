'From Cuis7.3 [latest update: #6955] on 27 December 2024 at 11:28:57 am'!

!Morph methodsFor: 'geometry testing' stamp: 'RMV 12/27/2024 11:28:39'!
clipsSubmorphsReally
	"Currently only one level of clipping is supported.
	This means that if a morph clipsSubmorphs, then no submorph in its tree can do it.
	This is a current limitation of VectorCanvas"

	self clipsSubmorphs ifFalse: [ ^false ].
	self allOwnersDo: [ :o | o clipsSubmorphs ifTrue: [ ^false ]].
	^true! !

