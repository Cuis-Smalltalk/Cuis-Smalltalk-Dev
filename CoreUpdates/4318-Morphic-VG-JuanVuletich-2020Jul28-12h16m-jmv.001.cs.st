'From Cuis 5.0 [latest update: #4316] on 28 July 2020 at 12:19:29 pm'!

!RectangleLikeMorph methodsFor: 'geometry testing' stamp: 'jmv 7/28/2020 12:16:53'!
morphContainsPoint: aLocalPoint
	"Answer true even if aLocalPoint is in a submorph, as long as it is in our shape"

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	"Most morphs answer true to to #isOrthoRectangularMorph, or redefine this method..."
	self isOrthoRectangularMorph ifTrue: [
		^ self morphLocalBounds containsPoint: aLocalPoint ].
	
	"...But for those who not, provide correct albeit expensive behavior."
	"Can't do better. Please redefine in subclasses as appropriate!! (or finish Morphic 3!!)"
	"Mhhh. Podria usar el #imageForm: y ver si es transparente... deberia andar"
	^ self morphLocalBounds containsPoint: aLocalPoint! !


!BitBltCanvas methodsFor: 'morphic' stamp: 'jmv 7/28/2020 12:18:56'!
morph: aMorph isAtPoint: aPoint
	aMorph displayBounds ifNotNil: [ :r |
		(r containsPoint: aPoint) ifFalse: [
			^false ]].
	^aMorph morphContainsPoint:
		(aMorph internalizeFromWorld: aPoint)! !

