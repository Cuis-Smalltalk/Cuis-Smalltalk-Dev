'From Cuis 4.2 of 25 July 2013 [latest update: #2136] on 20 December 2014 at 4:07:04.623871 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 12/20/2014 14:51'!
morphLocalBounds

	^self morphTopLeft extent: self morphExtent! !

!Morph methodsFor: 'geometry' stamp: 'jmv 12/20/2014 14:50'!
morphTopLeft
	"By default, morphs occupy a rectangle specified by #morphTopLef and #morphExtent"
	^0@0! !


!Morph methodsFor: 'drawing' stamp: 'jmv 12/20/2014 16:01'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	aCanvas
		fillRectangle: self morphLocalBounds
		color: self color! !

!Morph methodsFor: 'geometry testing' stamp: 'jmv 12/20/2014 14:52'!
morphContainsPoint: aLocalPoint
	| shadow |

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	"Most morphs answer true to to #isOrthoRectangularMorph, or redefine this method..."
	self isOrthoRectangularMorph ifTrue: [
		^ self morphLocalBounds containsPoint: aLocalPoint ].
	
	"...But for those who not, provide correct albeit expensive behavior."
	shadow _ self ownShadowForm.
	^(shadow pixelValueAt: aLocalPoint) > 0! !


!BorderedRectMorph methodsFor: 'drawing' stamp: 'jmv 12/20/2014 14:58'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	"If you redefine this method in a subclass, please take a look at the comment at #isOpaqueMorph"
	aCanvas
		fillRectangle: self morphLocalBounds
		color: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor! !


!MenuMorph methodsFor: 'drawing' stamp: 'jmv 12/20/2014 16:03'!
drawOn: aCanvas
	
	| roundCorners |
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			aCanvas roundRect: self morphLocalBounds color: color radius: Theme current roundedWindowRadius ]
		ifFalse: [
			aCanvas fillRectangle: self morphLocalBounds color: color borderWidth: borderWidth borderStyleSymbol: #raised baseColorForBorder: color ]! !


!HaloMorph methodsFor: 'events-processing' stamp: 'jmv 12/20/2014 15:07'!
containsPoint: aLocalPoint event: aMorphicEvent

	self visible ifFalse: [ ^false ].

	"mouseButton3 events are handled by the halo"
	(aMorphicEvent isMouse and: [
		aMorphicEvent isMouseDown and: [ aMorphicEvent mouseButton3Pressed ]])
	ifTrue: [
		^ self morphLocalBounds containsPoint: aLocalPoint ].

	^false! !

!HaloMorph methodsFor: 'geometry testing' stamp: 'jmv 12/20/2014 14:54'!
morphContainsPoint: aLocalPoint

	"If not visible, won't contain any point at all."
	self visible ifFalse: [ ^false ].

	"We behave as if we were a rectangle. I.e., we want (specifically mouse button) events that happen inside our bounds"
	^ self morphLocalBounds containsPoint: aLocalPoint! !


!HoverHelpMorph methodsFor: 'drawing' stamp: 'jmv 12/20/2014 15:09'!
drawOn: aCanvas

	| r |
	r _ self morphLocalBounds.
	aCanvas roundRect: r color: self color radius: 4.
	aCanvas
		textComposition: textComposition
		bounds: (r insetBy: 4)
		color: Color black
		selectionColor: (Theme current textHighlightFocused: false)! !


!WindowEdgeAdjustingMorph methodsFor: 'geometry testing' stamp: 'jmv 12/20/2014 14:57'!
morphContainsPoint: aLocalPoint
	| sensitiveBorder |
	( self morphLocalBounds containsPoint: aLocalPoint) ifFalse: [ ^false ].
	sensitiveBorder _ 4.
	selector caseOf: {
		[ #windowTopLeft: ] -> [ ^ aLocalPoint x < sensitiveBorder or: [ aLocalPoint y < sensitiveBorder ]].
		[ #windowTopRight: ] -> [ ^ extent x - aLocalPoint x <= sensitiveBorder or: [ aLocalPoint y < sensitiveBorder ]].
		[ #windowBottomLeft: ] -> [ ^ aLocalPoint x < sensitiveBorder or: [ extent y- aLocalPoint y <= sensitiveBorder ]].
		[ #windowBottomRight: ] -> [ ^ extent x - aLocalPoint x <= sensitiveBorder or: [ extent y - aLocalPoint y <= sensitiveBorder ]].
	}
	otherwise: [
		"all the morph is sensitive for horizontal and vertical (i.e. non corner) instances."
		^true ]! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 12/20/2014 15:10'!
displayBoundsOf: aMorph
	"Answer a rectangle that completely bounds aMorph when drawn on our form.
	Might be larger than strictly required.
		- In Morphic 2, this could cause clipping artifacts. This doesn't usually happen because:
			a) Morphic 2 doesn't use scaling and rotation
			b) Most Morphic 2 morphs have rectangular shape.
		- In Morphic 3, clipping also considers the real shape of the owner morph. This avoids those artifacts."

	"Think about doing a BoundsFinderCanvas even for Morphic 2"
	self flag: #jmvVer2.

	^self externalizeDisplayBounds: aMorph morphLocalBounds from: aMorph! !


!BitBltCanvas methodsFor: 'drawing' stamp: 'jmv 11/21/2014 11:12'!
line: pt1 to: pt2 width: wp color: c
	|  p1 p2 w |
	p1 _ currentTransformation intTransform: pt1.
	p2 _ currentTransformation intTransform: pt2.
	w _ currentTransformation externalizeScalar: wp.
	self setPaintColor: c.
	port
		width: w;
		height: w;
		drawFrom: p1 to: p2! !

