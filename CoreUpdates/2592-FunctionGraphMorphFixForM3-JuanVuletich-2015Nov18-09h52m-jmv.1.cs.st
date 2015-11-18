'From Cuis 4.2 of 25 July 2013 [latest update: #2590] on 18 November 2015 at 10:00:28.408657 am'!

!FunctionGraphMorph methodsFor: 'drawing' stamp: 'jmv 11/18/2015 09:59'!
drawOn: aCanvas
	"
	| g |
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x mod: 1 ] color: Color green.
	g addFunction: [ :x | x div: 1 ] color: Color red.
	g openInWorld
	"
	| r y r2 |
	r _ self morphLocalBounds.
	aCanvas
		fillRectangle: r
		color: (Color gray: 0.9).
	aCanvas
		frameAndFillRectangle: r fillColor: (Color gray: 0.9)
		borderWidth: 0.05 borderColor: Color white.
	aCanvas line: (self toMorphic:0@yMin)-(0@2) to: (self toMorphic: 0 @ yMax)+(0@2) width: 2 color: Color lightGray.
	aCanvas line: (self toMorphic: xMin@0)+(2@0) to: (self toMorphic: xMax@0)-(2@0) width: 2 color: Color lightGray.

	xMin*1.0 to: xMax count: 600 do: [ :x |
		functions with: colors do: [ :f :c |
			y _ f value: x.
			(y between: yMin and: yMax) ifTrue: [
				r2 _ (self xToMorphic: x)@(self yToMorphic: y) extent: 5.
				(r containsRect: r2) ifTrue: [
					aCanvas
						fillRectangle: r2
						color: c ]]]]! !

