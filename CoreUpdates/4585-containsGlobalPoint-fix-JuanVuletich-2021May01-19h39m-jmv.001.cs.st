'From Cuis 5.0 [latest update: #4581] on 1 May 2021 at 7:42:39 pm'!

!Morph methodsFor: 'structure' stamp: 'jmv 5/1/2021 19:34:50'!
topmostWorld
	"Answer the last morph in the owner chain (i.e. the morph without owner) if it is a WorldMorph, or nil."
	^owner
		ifNotNil: [ owner topmostWorld ]! !


!WorldMorph methodsFor: 'structure' stamp: 'jmv 5/1/2021 19:34:53'!
topmostWorld
	"Answer the last morph in the owner chain (i.e. the morph without owner) if it is a WorldMorph, or nil."
	owner
		ifNotNil: [ ^owner topmostWorld ].
	^self! !


!Morph methodsFor: 'geometry testing' stamp: 'jmv 5/1/2021 19:41:56'!
containsGlobalPoint: worldPoint

	self visible ifTrue: [
		self topmostWorld ifNotNil: [ :w |
			w canvas ifNotNil: [ :canvas |
				^ canvas morph: self isAtPoint: worldPoint ]]].
	^ false! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 5/1/2021 19:40:01'!
fullContainsGlobalPoint: worldPoint
	"Answer true if worldPoint is in some submorph, even if not inside our shape."

	self visible ifTrue: [
		self topmostWorld ifNotNil: [ :w |
			w canvas ifNotNil: [ :canvas |
				(canvas morph: self isAtPoint: worldPoint) ifTrue: [ ^ true ].
				self submorphsDo: [ :m |
					(m fullContainsGlobalPoint: worldPoint) ifTrue: [ ^ true ]]]]].
	^ false! !

