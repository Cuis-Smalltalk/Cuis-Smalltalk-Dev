'From Cuis 4.0 of 21 April 2012 [latest update: #2310] on 12 June 2012 at 5:14:20 pm'!

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 5/23/2012 19:31'!
draw3DLookOn: aCanvas

	| borderStyleSymbol c |
	borderStyleSymbol _ self isPressed ifFalse: [ #raised ] ifTrue: [ #inset ].
	c _ color.
	self mouseIsOver ifTrue: [ c _ c  lighter ].
	aCanvas
		fillRectangle: bounds
		colorOrInfiniteForm: c
		borderWidth: borderWidth
		borderStyleSymbol: borderStyleSymbol.

	self drawRegularLabelOn: aCanvas! !

