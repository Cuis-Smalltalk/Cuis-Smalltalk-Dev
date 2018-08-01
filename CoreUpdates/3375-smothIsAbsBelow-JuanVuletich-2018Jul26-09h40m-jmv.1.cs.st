'From Cuis 5.0 of 7 November 2016 [latest update: #3374] on 26 July 2018 at 11:48:37 am'!

!Float methodsFor: 'mathematical functions' stamp: 'jmv 7/26/2018 09:42:55'!
smoothIsAbsBelow: threshold
	"A Function that is 1 at zero, 0 outside a bounded interval of size 4*threshold, and is continuous and differentiable.
	
	It is a 'smooth' version of the #isAbsBelow: function.
	Useful when we need to 'count' stuff, but still want continuous and differentiable stuff."
	"
	| g |
	Feature require: 'Morphic-Widgets-Extras'.
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x abs < Float halfPi ifTrue: [1] ifFalse:[0] ] color: Color brown.
	g addFunction: [ :x | x smoothIsAbsBelow: Float halfPi ] color: Color red.
	(g embeddedInMorphicWindowLabeled: 'graph') openInWorld
	"
	| scaled |
	scaled _ self / threshold.
	^ scaled abs < 2 ifTrue: [ (scaled * Float halfPi) cos + 1.0 * 0.5 ] ifFalse: [ 0 ]! !


!Number methodsFor: 'arithmetic' stamp: 'jmv 7/26/2018 09:41:17'!
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
Feature require: 'Morphic-Widgets-Extras'.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x \\ d ] color: Color green.
g addFunction: [ :x | x // d ] color: Color red.
g openInWorld
"
"
| g d |
d _ -1.
Feature require: 'Morphic-Widgets-Extras'.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x \\ d ] color: Color green.
g addFunction: [ :x | x // d ] color: Color red.
g openInWorld
"! !

!Number methodsFor: 'arithmetic' stamp: 'jmv 7/26/2018 09:41:05'!
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
	And https://biblio.ugent.be/input/download?func=downloadFile&recordOId=314490&fileOId=452146
	"
	"Answer r such that: 
		for some integer q, aNumber * q + r = self
		with 0 <= r < | aNumber |"

	^self \\ divisor abs

	"Evaluate the following:"
"
| g d |
d _ 1.
Feature require: 'Morphic-Widgets-Extras'.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x mod: d ] color: Color green.
g addFunction: [ :x | x div: d ] color: Color red.
g openInWorld
"
"
| g d |
d _ -1.
Feature require: 'Morphic-Widgets-Extras'.
g _ FunctionGraphMorph new.
g domain: (-4 to: 4).
g addFunction: [ :x | x mod: d ] color: Color green.
g addFunction: [ :x | x div: d ] color: Color red.
g openInWorld
"
! !

