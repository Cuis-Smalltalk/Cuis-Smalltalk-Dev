'From Cuis 4.2 of 25 July 2013 [latest update: #2583] on 18 November 2015 at 7:59:07.495031 am'!

!FunctionGraphMorph methodsFor: 'drawing' stamp: 'len 11/18/2015 07:57'!
drawOn: aCanvas
	"
	| g |
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x mod: 1 ] color: Color green.
	g addFunction: [ :x | x div: 1 ] color: Color red.
	g openInWorld
	"
	| r y |
	r _ self morphLocalBounds.
	aCanvas setClipRect: self morphBoundsInWorld.
	aCanvas
		fillRectangle: r
		color: (Color gray: 0.9).
	aCanvas
		frameAndFillRectangle: r fillColor: (Color gray: 0.9)
		borderWidth: 0.05 borderColor: Color white.
	aCanvas line: (self toMorphic:0@yMin) to: (self toMorphic: 0 @ yMax) width: 2 color: Color lightGray.
	aCanvas line: (self toMorphic: xMin@0) to: (self toMorphic: xMax@0) width: 2 color: Color lightGray.

	xMin*1.0 to: xMax count: 600 do: [ :x |
		functions with: colors do: [ :f :c |
			y _ f value: x.
			(y between: yMin and: yMax) ifTrue: [
				aCanvas
					fillRectangle: ((self xToMorphic: x)@(self yToMorphic: y) extent: 5)
					color: c ]]]! !

