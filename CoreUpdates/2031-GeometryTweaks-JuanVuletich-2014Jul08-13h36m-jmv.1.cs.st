'From Cuis 4.2 of 25 July 2013 [latest update: #2030] on 8 July 2014 at 1:39:29.103286 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 7/8/2014 13:38'!
internalizeDistanceFromWorld: aPoint
	"aPoint is a delta in World coordinates. Answer is in own coordinates."
	| inOwners |
	self flag: #jmvVer2.
	inOwners _ owner
		ifNotNil: [ owner internalizeDistanceFromWorld: aPoint ]
		ifNil: [ aPoint ].
	^self internalizeDistance: inOwners! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/8/2014 11:41'!
morphExtentInWorld: newExtent
	"world coordinates
	Ignored by morphs that are not resizeable"! !


!RectangleLikeMorph methodsFor: 'geometry' stamp: 'jmv 7/8/2014 11:41'!
morphExtentInWorld: newExtent
	"world coordinates"
	self flag: #jmvVer2.
	self morphExtent: (self internalizeDistanceFromWorld: newExtent)! !


!Morph methodsFor: 'geometry' stamp: 'jmv 7/8/2014 11:37'!
externalizeDistanceToWorld: aPoint
	"aPoint is in own coordinates. Answer is in world coordinates.
	BUT there is no well defined World!!"
	| inOwners |
	self flag: #jmvVer2.

	inOwners _ self externalizeDistance: aPoint.
	^owner
		ifNotNil: [ owner externalizeDistanceToWorld: inOwners ]
		ifNil: [ inOwners ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/8/2014 11:28'!
morphExtentInWorld
	"eventually, remove."
	self flag: #jmvVer2.
	^self externalizeDistanceToWorld: self morphExtent! !


!HaloMorph methodsFor: 'private' stamp: 'jmv 7/8/2014 11:41'!
doGrow: evt with: growHandle
	"Called while the mouse is down in the grow handle"

	| newExtent |
self revisar.
	self flag: #jmvVer2.
	evt hand obtainHalo: self.
"Como podria andar el grow de un morph embebido en otro? andara ahora?"
newExtent _ evt eventPosition - positionOffset - target morphPositionInWorld.
	evt shiftPressed ifTrue: [newExtent _ (newExtent x max: newExtent y) asPoint].
	(newExtent x = 0 or: [newExtent y = 0]) ifTrue: [^ self].
	target morphExtentInWorld: newExtent.
	growHandle morphPositionInWorld: evt eventPosition - (growHandle morphExtent // 2)! !

