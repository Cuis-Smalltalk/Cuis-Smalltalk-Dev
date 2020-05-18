'From Cuis 5.0 [latest update: #4147] on 18 May 2020 at 3:09:14 pm'!

!Number methodsFor: 'arithmetic' stamp: 'jmv 5/18/2020 15:07:11'!
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
Compiler evaluate: '
	| g d |
	d _ 1.
	Feature require: ''Morphic-Widgets-Extras''.
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x \\ d ] color: Color green.
	g addFunction: [ :x | x // d ] color: Color red.
	g openInWorld'
"
"
Compiler evaluate: '
	| g d |
	d _ -1.
	Feature require: ''Morphic-Widgets-Extras''.
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x \\ d ] color: Color green.
	g addFunction: [ :x | x // d ] color: Color red.
	g openInWorld'
"! !

!Number methodsFor: 'arithmetic' stamp: 'jmv 5/18/2020 15:07:30'!
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
	And 'The Euclidean Definition of the Functions div and mod' by Raymond T. Boute, https://core.ac.uk/download/pdf/55698442.pdf
	"
	"Answer r such that: 
		for some integer q, aNumber * q + r = self
		with 0 <= r < | aNumber |"

	^self \\ divisor abs

	"Evaluate the following:"
"
Compiler evaluate: '
	| g d |
	d _ 1.
	Feature require: ''Morphic-Widgets-Extras''.
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x mod: d ] color: Color green.
	g addFunction: [ :x | x div: d ] color: Color red.
	g openInWorld'
"
"
Compiler evaluate: '
	| g d |
	d _ -1.
	Feature require: ''Morphic-Widgets-Extras''.
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x mod: d ] color: Color green.
	g addFunction: [ :x | x div: d ] color: Color red.
	g openInWorld'
"
! !

!Number methodsFor: 'arithmetic' stamp: 'jmv 5/18/2020 15:08:09'!
rem:  divisor
	"Modulo operation. Remainder of the integer division #quo: (division with truncation towards zero).
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
Compiler evaluate: '
	| g d |
	d _ 1.
	Feature require: ''Morphic-Widgets-Extras''.
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x rem: d ] color: Color green.
	g addFunction: [ :x | x quo: d ] color: Color red.
	g openInWorld'
"
"
Compiler evaluate: '
	| g d |
	d _ -1.
	Feature require: ''Morphic-Widgets-Extras''.
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x rem: d ] color: Color green.
	g addFunction: [ :x | x quo: d ] color: Color red.
	g openInWorld'
"! !


!Float methodsFor: 'mathematical functions' stamp: 'jmv 5/18/2020 15:08:33'!
smoothIsAbsBelow: threshold
	"A Function that is 1 at zero, 0 outside a bounded interval of size 4*threshold, and is continuous and differentiable.
	
	It is a 'smooth' version of an #isAbsBelow: function: x abs < threshold.
	Useful when we need to 'count' stuff, but still want continuous and differentiable stuff."
	"
Compiler evaluate: '
	| g |
	Feature require: ''Morphic-Widgets-Extras''.
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x abs < Float halfPi ifTrue: [1] ifFalse:[0] ] color: Color brown.
	g addFunction: [ :x | x smoothIsAbsBelow: Float halfPi ] color: Color red.
	(g embeddedInMorphicWindowLabeled: ''graph'') openInWorld'
	"
	| scaled |
	scaled _ self / threshold.
	^ scaled abs < 2 ifTrue: [ (scaled * Float halfPi) cos + 1.0 * 0.5 ] ifFalse: [ 0 ]! !

!Float methodsFor: 'mathematical functions' stamp: 'jmv 5/18/2020 15:08:52'!
smoothStep
	"A Function that is
		x <= -1 		-> 		1
		-1 < x < 1 	->			smooth differentiable transition from 1 to 0
		1 <= x 		-> 		0
	
	It is a 'smooth' version of an #negative
	Has countless applications. For example in image and signal processing, but also in other fields."
	"
Compiler evaluate: '
	| g |
	Feature require: ''Morphic-Widgets-Extras''.
	g _ FunctionGraphMorph new.
	g domain: (-4 to: 4).
	g addFunction: [ :x | x abs < 1 ifTrue: [1] ifFalse:[0] ] color: Color brown.
	g addFunction: [ :x | x smoothStep ] color: Color red.
	(g embeddedInMorphicWindowLabeled: ''graph'') openInWorld'
	"
	self isNaN ifTrue: [ ^self ].
	self > -1 ifFalse: [ ^ 1 ].
	1 > self ifFalse: [ ^ 0 ].
	^ (self +2 * Float halfPi) sin * 0.5 + 0.5! !

