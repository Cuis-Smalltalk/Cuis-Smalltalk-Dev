'From Cuis 4.2 of 25 July 2013 [latest update: #2110] on 12 November 2014 at 5:24:46.265221 pm'!

!Number methodsFor: 'arithmetic' stamp: 'jmv 11/11/2014 17:37'!
div: aNumber
	"Integer division with non-negative remainder. Euclidean division.
	(9 div:4) = 2
	(-9 div: 4) = -3
	(-0.9 div: 0.4) = -3
	#mod: answers the remainder from this division.
	See #//, #quo:, #div:"
	"Answer an integer q such that: 
		for some r, aNumber * q + r = self
		with 0 <= r < | aNumber |"

	aNumber positive ifTrue: [ ^self // aNumber ].
	^ (self // aNumber abs) negated! !

!Number methodsFor: 'arithmetic' stamp: 'jmv 11/11/2014 10:27'!
mod: divisor
	"Modulo operation. Remainder of the integer division #div: (Euclidean division)
	Answer a Number that is never negative (it is positive or zero). 
	(9 mod: 4) = 1
	(-9 mod: 4) = 3
	(9 mod: -4) = 1
	(0.9 mod: 0.4) = 0.1
	See http://en.wikipedia.org/wiki/Modulo_operation
	See #\\, #rem: and #mod:
	See detailed discussion at http://mathforum.org/library/drmath/view/52343.html
	"
	"Answer r such that: 
		for some integer q, aNumber * q + r = self
		with 0 <= r < | aNumber |"

	^self \\ divisor abs

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
	c fillRectangle: ((x@(x div: divisor) negated) * 40 + 160 + topLeft extent: 4) color: Color red.
	c fillRectangle: ((x@(x mod: divisor) negated) * 40 + 160 + topLeft extent: 4) color: Color green ].
divisor _ -1.
topLeft _ 400@40.
c _ Display getCanvas.
c frameAndFillRectangle: (topLeft-5 extent: 320@320+10) fillColor: Color white borderWidth: 2 borderColor: Color black.
c line: 160@0 + topLeft to: 160@320 + topLeft width: 2 color: Color lightGray.
c line: 0@160 + topLeft to: 320@160 + topLeft width: 2 color: Color lightGray.
-4.0 to: 4 count: 400 do: [ :x |
	c fillRectangle: ((x@(x/divisor negated)) * 40 + 160 + topLeft extent: 3) color: Color blue.
	c fillRectangle: ((x@(x div: divisor) negated) * 40 + 160 + topLeft extent: 4) color: Color red.
	c fillRectangle: ((x@(x mod: divisor) negated) * 40 + 160 + topLeft extent: 4) color: Color green ].
"! !


!Number methodsFor: 'arithmetic' stamp: 'jmv 11/11/2014 10:23'!
// aNumber
	"Integer division with truncation toward negative infinity. 
	9//4 = 2
	-9//4 = -3
	-0.9//0.4 = -3
	#\\ answers the remainder from this division.
	See #//, #quo:, #div:"

	^ (self / aNumber) floor! !

!Number methodsFor: 'arithmetic' stamp: 'jmv 11/11/2014 17:37'!
quo: aNumber
	"Integer division with truncation toward zero.
	(-9 quo: 4) = -2
	(-0.9 quo: 0.4) = -2
	#rem: answers the remainder from this division.
	See #//, #quo:, #div:"

	^ (self / aNumber) truncated! !

!Number methodsFor: 'arithmetic' stamp: 'jmv 11/11/2014 17:37'!
rem:  divisor
	"Modulo operation. Remainder of the integer division #quo: (division truncated to zero).
	Answer a Number with the same  sign as dividend (i.e.,  self).
	(9 rem: 4) = 1.
	(-9 rem: 4) = -1.
	(0.9 rem: 0.4) = 0.1.
	See http://en.wikipedia.org/wiki/Modulo_operation
	See #\\, #rem: and #mod:
	See detailed discussion at http://mathforum.org/library/drmath/view/52343.html
	"

	^self - ((self quo: divisor) * divisor)

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
	c fillRectangle: ((x@(x quo: divisor) negated) * 40 + 160 + topLeft extent: 4) color: Color red.
	c fillRectangle: ((x@(x rem: divisor) negated) * 40 + 160 + topLeft extent: 4) color: Color green ].
divisor _ -1.
topLeft _ 400@40.
c _ Display getCanvas.
c frameAndFillRectangle: (topLeft-5 extent: 320@320+10) fillColor: Color white borderWidth: 2 borderColor: Color black.
c line: 160@0 + topLeft to: 160@320 + topLeft width: 2 color: Color lightGray.
c line: 0@160 + topLeft to: 320@160 + topLeft width: 2 color: Color lightGray.
-4.0 to: 4 count: 400 do: [ :x |
	c fillRectangle: ((x@(x/divisor negated)) * 40 + 160 + topLeft extent: 3) color: Color blue.
	c fillRectangle: ((x@(x quo: divisor) negated) * 40 + 160 + topLeft extent: 4) color: Color red.
	c fillRectangle: ((x@(x rem: divisor) negated) * 40 + 160 + topLeft extent: 4) color: Color green ].
"! !

!Number methodsFor: 'arithmetic' stamp: 'jmv 11/11/2014 10:27'!
\\ divisor
	"Modulo operation. Remainder of the integer division #// (Floored division, truncated to minus infiniti, a.k.a Knuth's division)
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


!Integer methodsFor: 'arithmetic' stamp: 'jmv 11/10/2014 23:22'!
// aNumber 
	| q |
	aNumber = 0 ifTrue: [^ (ZeroDivide dividend: self) signal].
	self = 0 ifTrue: [^ 0].
	q _ self quo: aNumber.
	"Refer to the comment in Number>>#//."
	^(q negative
		ifTrue: [q * aNumber ~= self]
		ifFalse: [q = 0 and: [self negative ~= aNumber negative]])
			ifTrue: [q - 1"Truncate towards minus infinity."]
			ifFalse: [q]! !

