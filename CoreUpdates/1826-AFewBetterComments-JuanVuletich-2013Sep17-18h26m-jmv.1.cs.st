'From Cuis 4.2 of 25 July 2013 [latest update: #1825] on 17 September 2013 at 6:32:17.904266 pm'!

!Morph methodsFor: 'geometry testing' stamp: 'jmv 9/17/2013 18:28'!
isOrthoRectangularMorph
	"Answer true if I fill my bounds. I.e. I am a rectangle aligned with Display borders and
	specified by my #morphExtent.
	If true, #morphContainsPoint: can simply check #morphExtent."
	^false! !


!RectangleLikeMorph methodsFor: 'geometry testing' stamp: 'jmv 9/17/2013 18:28'!
isOrthoRectangularMorph
	"Answer true if I fill my bounds. I.e. I am a rectangle aligned with Display borders and
	specified by my #morphExtent.
	If true, #morphContainsPoint: can simply check #morphExtent."
	^true! !


!BorderedRectMorph methodsFor: 'drawing' stamp: 'jmv 9/17/2013 18:31'!
drawOn: aCanvas
	"A canvas is already set with a proper transformation from our coordinates to those of the Canvas target."
	"If you redefine this method in a subclass, please take a look at the comment at #isOpaqueMorph"
	aCanvas
		fillRectangle: (0@0 extent: extent)
		color: color
		borderWidth: borderWidth
		borderStyleSymbol: #simple
		baseColorForBorder: borderColor! !

!BorderedRectMorph methodsFor: 'testing' stamp: 'jmv 9/17/2013 18:29'!
isOpaqueMorph
	"Any submorph that answers true to #isOrthoRectangularMorph (to optimize #morphContainsPoint:)
	but is not an opaque rectangle covering bounds MUST answer false to this message"
	color mightBeTranslucent ifTrue: [
		^false ].
	borderWidth > 0 ifTrue: [
		borderColor mightBeTranslucent ifTrue: [
			^false ]].
	^true! !


!PluggableButtonMorph methodsFor: 'geometry testing' stamp: 'jmv 9/17/2013 18:29'!
isOrthoRectangularMorph
	"Answer true if I fill my bounds. I.e. I am a rectangle aligned with Display borders and
	specified by my #morphExtent.
	If true, #morphContainsPoint: can simply check #morphExtent."
	^self isRoundButton not! !


!LayoutAdjustingMorph methodsFor: 'testing' stamp: 'jmv 9/17/2013 18:30'!
isOpaqueMorph
	"Any submorph that answers true to #isOrthoRectangularMorph (to optimize #morphContainsPoint:)
	but is not an opaque rectangle covering bounds MUST answer false to this message"
	color mightBeTranslucent ifTrue: [
		^false ].
	^true! !

