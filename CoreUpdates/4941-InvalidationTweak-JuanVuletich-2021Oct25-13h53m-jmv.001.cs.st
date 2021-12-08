'From Cuis 5.0 [latest update: #4940] on 25 October 2021 at 1:54:20 pm'!

!MorphicCanvas methodsFor: 'morphic world' stamp: 'jmv 10/25/2021 13:53:34'!
drawWorld: aWorldMorph rootMorphs: rootMorphs rootMorphsDamage: rootMorphsDamage backgroundDamage: newDamageFromMorphsBelow
	"Redraw the damaged areas.
	Draw each morph just once, merging rectanges to be repaired as needed."

	| morph morphDamage allDamage |
	"Iterate from back to front."
	allDamage _ Rectangle merging: newDamageFromMorphsBelow.
	rootMorphs size to: 1 by: -1 do: [ :i |
		morph _ rootMorphs at: i.
		morphDamage _ rootMorphsDamage at: i.
		morph displayFullBounds ifNotNil: [ :morphFullBounds |
			newDamageFromMorphsBelow do: [ :r | | intersection |
				intersection _ r intersect: morphFullBounds.
				intersection hasPositiveExtent ifTrue: [
					morphDamage
						ifNil: [ morphDamage _  intersection ]
						ifNotNil: [ morphDamage updateMerging: intersection ]]]].
		morphDamage ifNotNil: [
			self newClipRect: morphDamage.
			self fullDraw: morph.
			newDamageFromMorphsBelow add: morphDamage.
			allDamage
				ifNil: [ allDamage _ morphDamage copy ]
				ifNotNil: [ allDamage updateMerging: morphDamage ]]].
	^allDamage! !

