'From Cuis 5.0 [latest update: #4276] on 22 July 2020 at 8:45:12 pm'!

!HandMorph methodsFor: 'geometry' stamp: 'jmv 7/22/2020 20:41:16'!
displayBounds
	^self morphPosition extent: self morphExtent ! !

!HandMorph methodsFor: 'geometry' stamp: 'jmv 7/22/2020 20:12:48'!
submorphsDisplayFullBounds
"Usando los nombres viejos que estoy por hacer pelota. Repasar."
	"Current full bounds of submorphs carried by the hand.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around)."
	| fullBounds |
	submorphs isEmpty ifTrue: [ ^nil ].
	fullBounds _ nil.
	(self valueOfProperty: #lastPos)
		ifNil: [						"Just once when a new morph (not in the world) is attached to the hand."
			self submorphsDo: [ :m |
				m displayFullBounds ifNotNil: [ :b |
					fullBounds _ fullBounds ifNil: [b] ifNotNil: [ fullBounds merge: b ]]]]
		ifNotNil: [ :lastPos |						"When carrying morphs around."
			self submorphsDo: [ :m |
				m displayFullBounds ifNotNil: [ :b |
					fullBounds _ fullBounds ifNil: [b] ifNotNil: [ fullBounds merge: b ]]].
			fullBounds ifNil: [ ^nil ].						"bueeeenoooo"
			fullBounds _ fullBounds translatedBy: self morphPosition-lastPos ].
	^fullBounds! !


!HandMorph methodsFor: 'drawing' stamp: 'jmv 7/22/2020 20:16:07'!
savePatchFrom: aCanvas appendDamageTo: aStream
	"Save the part of the given canvas under this hand as a Form and return its bounding rectangle."

	"Details: The previously used patch Form is recycled when possible to reduce the burden on storage management."

	| fullBounds |
	fullBounds _ self displayFullBounds.
	fullBounds ifNil: [ ^self ].

	fullBounds _ fullBounds intersect: (`0@0` extent: aCanvas extent).
	(savedPatch isNil or: [savedPatch extent ~= fullBounds extent]) 
		ifTrue: [
			"allocate new patch form if needed"
			savedPatch _ Form extent: fullBounds extent depth: aCanvas depth ].
	aCanvas
		contentsOfArea: fullBounds
		into: savedPatch.
	savedPatch offset: fullBounds topLeft.
	prevFullBounds
		ifNil: [ aStream nextPut: fullBounds ]
		ifNotNil: [ aStream nextPut: (fullBounds quickMerge: prevFullBounds)].
	prevFullBounds _ fullBounds! !

!HandMorph methodsFor: 'geometry' stamp: 'jmv 7/22/2020 20:13:51'!
displayFullBounds
	| fullBounds |
	fullBounds _ self displayBounds.
	self submorphsDisplayFullBounds ifNotNil: [ :b |
		fullBounds _ b quickMerge: fullBounds ].
	^fullBounds! !


!WorldState methodsFor: 'hands' stamp: 'jmv 7/22/2020 20:42:49'!
selectHandsToDrawForDamage: damageList
	"Select the set of hands that must be redrawn because either (a) the hand itself has changed or (b) the hand intersects some damage rectangle."

	| result |
	result _ OrderedCollection new.
	hands do: [:hand |
		hand needsToBeDrawn ifTrue: [
			hand hasChanged
				ifTrue: [result add: hand]
				ifFalse: [
					hand displayFullBounds ifNotNil: [ :handBounds |
						(damageList anySatisfy: [ :r | r intersects: handBounds]) ifTrue: [
							result add: hand]]]]].
	^ result! !

!methodRemoval: HandMorph #morphFullBoundsInWorld stamp: 'jmv 7/22/2020 20:44:54'!
HandMorph removeSelector: #morphFullBoundsInWorld!
!methodRemoval: HandMorph #submorphsFullBoundsInWorld stamp: 'jmv 7/22/2020 20:44:49'!
HandMorph removeSelector: #submorphsFullBoundsInWorld!
!methodRemoval: Morph #morphFullBoundsInWorld stamp: 'jmv 7/22/2020 20:44:55'!
Morph removeSelector: #morphFullBoundsInWorld!
