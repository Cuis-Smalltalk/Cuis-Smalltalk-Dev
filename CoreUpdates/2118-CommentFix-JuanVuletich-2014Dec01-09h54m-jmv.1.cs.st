'From Cuis 4.2 of 25 July 2013 [latest update: #2117] on 1 December 2014 at 9:54:37.764475 am'!

!Number methodsFor: 'arithmetic' stamp: 'jmv 12/1/2014 09:54'!
\\ divisor
	"Modulo operation. Remainder of the integer division #// (Floored division, truncated to minus infinity, a.k.a Knuth's division)
	Answer a Number with the same sign as divisor. 
	9\\4 = 1
	-9\\4 = 3
	9\\-4 = -3
	0.9\\0.4 = 0.1
	See http://en.wikipedia.org/wiki/Modulo_operation
	See #\\, #rem: and #mod:
	See detailed discussion at http://mathforum.org/library/drmath/view/52343.html
	"

	^self - (self // divisor * divisor)

	"Evaluate the following:"
"
| c divisor topLeft |
divisor _ 1.
topLeft _ 40@40.
c _ Display getCanvas.
c frameAndFillRectangle: (topLeft-5 extent: 320@320+10) fillColor: Color white borderWidth: 2 borderColor: Color black.
c line: 160@0 + topLeft to: 160@320 + topLeft width: 2 color: Color lightGray.
c line: 0@160 + topLeft to: 320@160 + topLeft width: 2 color: Color lightGray.
-4.0 to: 4 count: 400 do: [ :x |
	c fillRectangle: ((x@(x/divisor negated)) * 40 + 160 + topLeft extent: 3) color: Color blue.
	c fillRectangle: ((x@(x // divisor) negated) * 40 + 160 + topLeft extent: 4) color: Color red.
	c fillRectangle: ((x@(x \\ divisor) negated) * 40 + 160 + topLeft extent: 4) color: Color green ].
divisor _ -1.
topLeft _ 400@40.
c _ Display getCanvas.
c frameAndFillRectangle: (topLeft-5 extent: 320@320+10) fillColor: Color white borderWidth: 2 borderColor: Color black.
c line: 160@0 + topLeft to: 160@320 + topLeft width: 2 color: Color lightGray.
c line: 0@160 + topLeft to: 320@160 + topLeft width: 2 color: Color lightGray.
-4.0 to: 4 count: 400 do: [ :x |
	c fillRectangle: ((x@(x/divisor negated)) * 40 + 160 + topLeft extent: 3) color: Color blue.
	c fillRectangle: ((x@(x // divisor) negated) * 40 + 160 + topLeft extent: 4) color: Color red.
	c fillRectangle: ((x@(x \\ divisor) negated) * 40 + 160 + topLeft extent: 4) color: Color green ].
"! !

