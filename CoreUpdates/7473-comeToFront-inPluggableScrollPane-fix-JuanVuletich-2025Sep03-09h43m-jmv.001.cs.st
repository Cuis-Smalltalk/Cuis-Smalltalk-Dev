'From Cuis7.5 [latest update: #7490] on 3 September 2025 at 9:44:37 am'!

!Morph methodsFor: 'private' stamp: 'jmv 9/3/2025 09:39:05'!
privateMove: aMorph toIndex: index
	"aMorph's position changes within in the submorph chain
	Move aMorph to the requested index."

	| oldIndex |
	oldIndex := submorphs indexOf: aMorph.
	index = oldIndex ifTrue: [ ^self ].
	index < oldIndex
		ifTrue: [
			oldIndex-1 to: index by: -1 do: [ :i |
				submorphs at: i+1 put: (submorphs at: i)]]
		ifFalse: [
			submorphs replaceFrom: oldIndex to: index-1 with: submorphs startingAt: oldIndex+1 ].
	submorphs at: index put: aMorph.
	self world ifNotNil: [aMorph redrawNeeded].
	self privateFixLayerOrder.
	self someSubmorphPositionOrExtentChanged.! !


!PluggableScrollPane methodsFor: 'private' stamp: 'jmv 9/3/2025 09:41:22'!
privateMoveFrontMorph: aMorph
	"Position 1 and 2 in our submorphs are taken by our scrollbars"
	
	self privateMove: aMorph toIndex: 3! !


!Morph methodsFor: 'submorphs-add/remove' stamp: 'jmv 9/3/2025 09:40:32'!
comeToFront

	"Restore default #layerNumber for us.
	We may still end behind other 'more topMost' morphs."
	self removeProperty: #layerNumber.
	owner privateMoveFrontMorph: self! !

!Morph methodsFor: 'private' stamp: 'jmv 9/3/2025 09:39:27'!
privateMoveBackMorph: aMorph
	"aMorph's position changes within in the submorph chain
	Move aMorph behind all siblings"
	
	self privateMove: aMorph toIndex: submorphs size! !

!Morph methodsFor: 'private' stamp: 'jmv 9/3/2025 09:39:37'!
privateMoveFrontMorph: aMorph
	"aMorph's position changes within in the submorph chain
	Move aMorph to be above all siblings"
	
	self privateMove: aMorph toIndex: 1! !

