'From Cuis 5.0 [latest update: #4263] on 19 July 2020 at 5:22:10 pm'!

!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/13/2020 16:59:51'!
fullContainsGlobalPoint: worldPoint
"Answer true even if aLocalPoint is in some unclipped submorph, but outside us "

	"If not visible, won't contain any point at all."
	| canvas |
	self visible ifFalse: [ ^false ].

	canvas _ self world canvas.
	canvas isNil ifTrue: [ ^false ].
	(canvas morph: self isAtPoint: worldPoint) ifTrue: [ ^ true ]. 
	self submorphsDrawingOutsideReverseDo: [ :m |
		(m fullContainsGlobalPoint: worldPoint) ifTrue: [ ^ true ]].
	^ false! !


!RectangleLikeMorph methodsFor: 'geometry testing' stamp: 'jmv 7/13/2020 16:37:51'!
morphContainsPoint: aLocalPoint
"Answer true even if aLocalPoint is in asubmorph, as long as it is in our shape"

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	"Most morphs answer true to to #isOrthoRectangularMorph, or redefine this method..."
	self isOrthoRectangularMorph ifTrue: [
		^ self morphLocalBounds containsPoint: aLocalPoint ].
	
	"...But for those who not, provide correct albeit expensive behavior."
	"Can't do better. Please redefine in subclasses as appropriate!! (or finish Morphic 3!!)"
	"Mhhh. Podria usar el #imageForm: y ver si es transparente... deberia andar"
	^ self morphLocalBounds containsPoint: aLocalPoint! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 7/13/2020 16:43:16'!
morph: aMorph isAtPoint: aPoint

	^aMorph morphContainsPoint:
		(aMorph internalizeFromWorld: aPoint)! !


!Morph methodsFor: 'geometry testing' stamp: 'jmv 7/13/2020 17:00:03'!
fullContainsPoint: aLocalPoint
"Answer true even if aLocalPoint is in some unclipped submorph, but outside us "

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	^self fullContainsGlobalPoint: (self externalizeToWorld: aLocalPoint).! !

!methodRemoval: Morph #morphContainsPoint: stamp: 'jmv 7/19/2020 17:21:25'!
Morph removeSelector: #morphContainsPoint:!
