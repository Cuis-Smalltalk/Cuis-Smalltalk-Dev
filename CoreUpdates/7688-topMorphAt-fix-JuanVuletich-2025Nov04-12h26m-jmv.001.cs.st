'From Cuis7.5 [latest update: #7687] on 4 November 2025 at 12:28:28 pm'!

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 11/4/2025 12:26:45'!
topMorphAt: worldPoint
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint,
	and is not covered by any other morph. Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	"Not visible implies #includesPixel: is false. But still, don't waste time going into submorphs."
	self visible ifFalse: [ ^nil ].

	(self includesPixel: worldPoint) ifTrue: [ ^ self ].
	submorphs do: [ :m |
		(m topMorphAt: worldPoint) ifNotNil: [ :sm | ^sm ]].
	^nil! !

!Morph methodsFor: 'submorphs-accessing' stamp: 'jmv 11/4/2025 12:27:43'!
topUnlockedMorphAt: worldPoint
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint,
	and is not covered by any other morph. Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	self isLocked ifTrue: [ ^nil ].
	
	"Not visible implies #includesPixel: is false. But still, don't waste time going into submorphs."
	self visible ifFalse: [ ^nil ].
	
	(self includesPixel: worldPoint) ifTrue: [ ^ self ].
	submorphs do: [ :m |
		(m topUnlockedMorphAt: worldPoint) ifNotNil: [ :sm | ^sm ]].
	^nil! !


!BoxMorph methodsFor: 'submorphs-accessing' stamp: 'jmv 11/4/2025 12:26:54'!
topMorphAt: worldPoint
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint,
	and is not covered by any other morph. Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	| doWeIncludePixel |

	"Not visible implies #includesPixel: is false. But still, don't waste time going into submorphs."
	self visible ifFalse: [ ^nil ].

	doWeIncludePixel := self includesPixel: worldPoint.
	(doWeIncludePixel or: [ self submorphsMightProtrude and: [self clipsSubmorphs not]]) ifTrue: [
		submorphs do: [ :m |
			(m topMorphAt: worldPoint) ifNotNil: [ :sm | ^sm ]]].
	doWeIncludePixel ifTrue: [ ^self ].
	^nil! !

!BoxMorph methodsFor: 'submorphs-accessing' stamp: 'jmv 11/4/2025 12:27:47'!
topUnlockedMorphAt: worldPoint
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint,
	and is not covered by any other morph. Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	| doWeIncludePixel |
	self isLocked ifTrue: [ ^nil ].
	
	"Not visible implies #includesPixel: is false. But still, don't waste time going into submorphs."
	self visible ifFalse: [ ^nil ].
	
	doWeIncludePixel := self includesPixel: worldPoint.
	(doWeIncludePixel or: [ self submorphsMightProtrude and: [self clipsSubmorphs not]]) ifTrue: [
		submorphs do: [ :m |
			(m topUnlockedMorphAt: worldPoint) ifNotNil: [ :sm | ^sm ]]].
	doWeIncludePixel ifTrue: [ ^self ].
	^nil! !

