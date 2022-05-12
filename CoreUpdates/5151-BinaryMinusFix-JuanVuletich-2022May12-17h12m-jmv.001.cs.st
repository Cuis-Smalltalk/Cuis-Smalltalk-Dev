'From Cuis 6.0 [latest update: #5150] on 12 May 2022 at 5:13:14 pm'!

!Number methodsFor: 'arithmetic' stamp: 'jmv 5/12/2022 16:59:55'!
\\ divisor
	"Modulo operation. Remainder of the integer division #// (Floored division, truncated to minus infinity, a.k.a Knuth's division)
	Answer a Number with the same sign as divisor. 
	9\\4 = 1
	-9\\4 = 3
	9\\ -4 = -3
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

