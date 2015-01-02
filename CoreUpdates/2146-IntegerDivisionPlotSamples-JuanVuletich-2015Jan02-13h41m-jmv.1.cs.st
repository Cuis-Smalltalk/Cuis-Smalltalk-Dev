'From Cuis 4.2 of 25 July 2013 [latest update: #2145] on 2 January 2015 at 1:45:36.673611 pm'!

!Number methodsFor: 'arithmetic' stamp: 'jmv 1/2/2015 13:44'!
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
| g d |
d _ 1.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x mod: d ] color: Color green.
g addFunction: [ :x | x div: d ] color: Color red.
g openInWorld
"
"
| g d |
d _ -1.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x mod: d ] color: Color green.
g addFunction: [ :x | x div: d ] color: Color red.
g openInWorld
"
! !

!Number methodsFor: 'arithmetic' stamp: 'jmv 1/2/2015 13:43'!
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
| g d |
d _ 1.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x rem: d ] color: Color green.
g addFunction: [ :x | x quo: d ] color: Color red.
g openInWorld
"
"
| g d |
d _ -1.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x rem: d ] color: Color green.
g addFunction: [ :x | x quo: d ] color: Color red.
g openInWorld
"! !

!Number methodsFor: 'arithmetic' stamp: 'jmv 1/2/2015 13:41'!
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
| g d |
d _ 1.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x \\ d ] color: Color green.
g addFunction: [ :x | x // d ] color: Color red.
g openInWorld
"
"
| g d |
d _ -1.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x \\ d ] color: Color green.
g addFunction: [ :x | x // d ] color: Color red.
g openInWorld
"! !

