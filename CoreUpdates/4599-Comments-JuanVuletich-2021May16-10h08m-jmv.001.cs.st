'From Cuis 5.0 [latest update: #4597] on 16 May 2021 at 10:17:29 am'!

!Morph methodsFor: 'geometry' stamp: 'jmv 5/16/2021 10:16:34'!
adjustDisplayBoundsBy: delta
	"Private for framework use.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Rectangle is clipped appropiately if some owner clips us."

	privateDisplayBounds ifNotNil: [
		privateDisplayBounds _ privateDisplayBounds translatedBy: delta ].
	self allMorphsDo: [ :m | m adjustDisplayBoundsBy: delta ].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 5/16/2021 10:16:38'!
displayBounds
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Rectangle is clipped appropiately if some owner clips us."

	self visible ifFalse: [ ^nil ].
	^ privateDisplayBounds! !

!Morph methodsFor: 'geometry' stamp: 'jmv 5/16/2021 10:16:42'!
displayBoundsSetFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Rectangle is clipped appropiately if some owner clips us."

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			c canvasToUse == aCanvas ifTrue: [
				privateDisplayBounds _ aCanvas boundingRectOfCurrentMorphAfterDraw ]]].! !

!Morph methodsFor: 'geometry' stamp: 'jmv 5/16/2021 10:16:46'!
displayBoundsUpdateFrom: aCanvas
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Rectangle is clipped appropiately if some owner clips us."

	self world ifNotNil: [ :w |
		w canvas ifNotNil: [ :c |
			c canvasToUse == aCanvas ifTrue: [
				privateDisplayBounds _ privateDisplayBounds
					quickMerge: aCanvas boundingRectOfCurrentMorphAfterDraw ]]].! !


!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 5/14/2021 11:44:08'!
textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc
	| displayScanner leftInRun line possibleVisibleLocalRect textTopLeft |

	textTopLeft _ boundsRect topLeft.
	possibleVisibleLocalRect _ currentTransformation boundsOfInverseTransformOf: self clipRect.

	displayScanner _ MorphicScanner new
		defaultFont: aTextComposition defaultFont;
		text: aTextComposition textComposed
		foreground: c.
	displayScanner canvas: self.

	leftInRun _ 0.
	"Take clipRect into account. Extrememly fast scrolls and redraws of huge files (like .sources)"
	(aTextComposition lineIndexForPoint: (possibleVisibleLocalRect topLeft - textTopLeft max: `0@0`) )
		to: (aTextComposition lineIndexForPoint: possibleVisibleLocalRect bottomRight - textTopLeft)
		do: [ :i |
			line _ aTextComposition lines at: i.
			aTextComposition
				displaySelectionInLine: line
				on: self
				textTopLeft: textTopLeft
				selectionColor: sc.
			leftInRun _ displayScanner displayLine: line textTopLeft: textTopLeft leftInRun: leftInRun ]! !

