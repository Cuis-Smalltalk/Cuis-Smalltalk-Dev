'From Cuis 5.0 [latest update: #4069] on 20 March 2020 at 12:56:57 pm'!
!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'form clipLeft clipTop clipRight clipBottom transformations currentTransformation cti currentMorph drawingMorphStack engine clipRect '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!

!MorphicCanvas methodsFor: 'accessing' stamp: 'pb 3/20/2020 12:49:43'!
clipRect
	"Return the currently active clipping rectangle"
	"In targetForm coordinates"
	^ clipRect ifNil: [clipRect := clipLeft@clipTop corner: clipRight@clipBottom+1]! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'pb 3/20/2020 12:44:46'!
textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc
	| displayScanner leftInRun line boundsInWorld tl |

	tl _ boundsRect topLeft.
	boundsInWorld _ currentTransformation displayBoundsOfTransformOf: boundsRect.

	displayScanner _ MorphicScanner new
		defaultFont: aTextComposition defaultFont;
		text: aTextComposition textComposed
		foreground: c.
	displayScanner canvas: self.

	leftInRun _ 0.
	"Take clipRect into account. Extrememly fast scrolls and redraws of huge files (like .sources)"
	(aTextComposition lineIndexForPoint: (`0@0` max: self clipRect origin - boundsInWorld origin))
		to: (aTextComposition lineIndexForPoint: (boundsInWorld extent min: self clipRect corner - boundsInWorld origin))
		do: [ :i |
			line _ aTextComposition lines at: i.
			aTextComposition
				displaySelectionInLine: line
				on: self
				textTopLeft: tl
				selectionColor: sc.
			leftInRun _ displayScanner displayLine: line textTopLeft: tl leftInRun: leftInRun ]! !

!MorphicCanvas methodsFor: 'private' stamp: 'pb 3/20/2020 12:49:28'!
setClipRect: aRectangle
	"by convention, aRectangle includes left and top but does not include right and bottom.
	We do draw clipRight and clipBottom but not beyond.
	"
	"In targetForm coordinates"
	clipRect := aRectangle! !


!BitBltCanvas methodsFor: 'testing' stamp: 'pb 3/20/2020 12:48:36'!
isCurrentMorphVisible
	| aRectangle myClipRect |
	currentMorph visible ifFalse: [ ^false ].
	"#clippingRectForCurrentMorph is valid even before drawing currentMorph, only in BitBltCanvas!!"
	aRectangle := self clippingRectForCurrentMorph.
	myClipRect := self clipRect.
	aRectangle right < myClipRect left	ifTrue: [^ false].
	aRectangle left > myClipRect right	ifTrue: [^ false].
	aRectangle bottom < myClipRect top	ifTrue: [^ false].
	aRectangle top > myClipRect bottom	ifTrue: [^ false].
	^ true
! !

!classDefinition: #MorphicCanvas category: #'Morphic-Support'!
Object subclass: #MorphicCanvas
	instanceVariableNames: 'form clipRect clipLeft clipTop clipRight clipBottom transformations currentTransformation cti currentMorph drawingMorphStack engine'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Support'!
