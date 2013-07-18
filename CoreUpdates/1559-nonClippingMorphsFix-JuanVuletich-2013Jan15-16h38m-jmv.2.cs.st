'From Cuis 4.1 of 12 December 2012 [latest update: #1558] on 15 January 2013 at 5:45:39 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 1/15/2013 13:05'!
clipSubmorphs: aBoolean
	"Drawing specific. If this property is set, clip the receiver's submorphs to the receiver's clipping bounds."
	self clipsSubmorphs = aBoolean
		ifFalse: [
			self redrawNeeded.
			"Adjust property properly regardless of the default value"
			self removeProperty: #clipSubmorphs.
			self clipsSubmorphs = aBoolean
				ifFalse: [ self setProperty: #clipSubmorphs toValue: aBoolean ].
			self redrawNeeded ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 1/15/2013 17:44'!
morphFullBoundsInWorld
	"Morphs should know nothing about absolute coordinates..."
	"Should implement in some reasonable way... including submorphs?"

	| r |
	self flag: #jmvVer2.
	"IF I remove this, then layout of buttons in FileList breaks when selecting / deselecting code files. Besides, ProgressMorph example breaks too"
	self layoutSubmorphsIfNeeded.

	self flag: #jmvVer2.	"consider submorphs!!!!!!!!!!"
"	^self morphBoundsInWorld"

	self flag: #jmvVer2.
	^(submorphs isEmpty or: [ self clipsSubmorphs ])
		ifTrue: [ self morphBoundsInWorld ]
		ifFalse: [
			r _ self morphBoundsInWorld.
			submorphs do: [ :m |
				r _ r merge: m morphFullBoundsInWorld ].
			r]! !


!HandMorph methodsFor: 'layout' stamp: 'jmv 1/15/2013 16:35'!
morphFullBoundsInWorld
	"Extend my bounds by the shadow offset when carrying morphs."
"
	self layoutSubmorphsAndComputeFullBounds.
	^submorphs isEmpty
		ifTrue: [ fullBounds ]
	
		ifFalse: [ fullBounds topLeft corner: fullBounds bottomRight + self shadowOffset ].
"

	"Morphs should know nothing about absolute coordinates..."
	"In addition, considering just first submorph... should include all of them"
	| r |
	self flag: #jmvVer2.
	r _ super morphFullBoundsInWorld.
	^submorphs isEmpty
		ifTrue: [ r ]
		ifFalse: [ r origin corner: r corner + self shadowOffset ]! !

!methodRemoval: HaloMorph #morphFullBoundsInWorld!
HaloMorph removeSelector: #morphFullBoundsInWorld!
