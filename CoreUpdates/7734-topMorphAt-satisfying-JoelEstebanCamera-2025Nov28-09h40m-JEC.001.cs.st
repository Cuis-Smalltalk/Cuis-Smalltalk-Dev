'From Cuis7.5 [latest update: #7720] on 28 November 2025 at 4:39:32 pm'!

!Morph methodsFor: 'submorphs-accessing' stamp: 'JEC 28/Nov/2025 16:36:10'!
topMorphAt: worldPoint satisfying: aCondition
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint, 
	satisfies the condition block, and is not covered by any other morph. 
	Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	"Not visible implies #includesPixel: is false. But still, don't waste time going into submorphs."
	(self visible and: [ aCondition value: self ]) ifFalse: [ ^ nil ].
	
	(self includesPixel: worldPoint) ifTrue: [ ^ self ].

	submorphs do: [ :m |
		(m topMorphAt: worldPoint satisfying: aCondition) ifNotNil: [ :foundMorph | ^foundMorph ] ]

	^ nil! !


!BoxMorph methodsFor: 'submorphs-accessing' stamp: 'JEC 28/Nov/2025 16:35:50'!
topMorphAt: worldPoint satisfying: aCondition
	"Find, if any a (possibly nested) submorph that #includesPixel: worldPoint, 
	satisfies the condition block, and is not covered by any other morph. 
	Answer could also be self.
	Answer nil if neither us nor any submorph #includesPixel: worldPoint.
	Note: do not include not #visible morphs (see #includesPixel:)."

	| doWeIncludePixel |

	"Not visible implies #includesPixel: is false. But still, don't waste time going into submorphs."
	(self visible and: [ aCondition value: self ]) ifFalse: [ ^ nil ].

	doWeIncludePixel := self includesPixel: worldPoint.
	(doWeIncludePixel or: [ self submorphsMightProtrude and: [ self clipsSubmorphs not ] ]) ifTrue: [
		submorphs do: [ :submorph |
			(submorph topMorphAt: worldPoint satisfying: aCondition)
				ifNotNil: [ :foundMorph | ^ foundMorph ] ] ].
	doWeIncludePixel ifTrue: [ ^ self ].

	^nil! !


!Morph methodsFor: 'submorphs-accessing' stamp: 'JEC 28/Nov/2025 16:33:58'!
topMorphAt: worldPoint

	^ self
		topMorphAt: worldPoint
		satisfying: [ :aMorph | true ]! !

!methodRemoval: BoxMorph #topMorphAt: stamp: 'JEC 28/Nov/2025 16:04:20'!
BoxMorph removeSelector: #topMorphAt:!
