'From Cuis 4.2 of 25 July 2013 [latest update: #2367] on 15 June 2015 at 5:22:19.349582 pm'!

!Number methodsFor: 'arithmetic' stamp: 'jmv 6/15/2015 16:34'!
div: aNumber
	"Integer division with non-negative remainder. Euclidean division.
	(9 div:4) = 2
	(-9 div: 4) = -3
	(-0.9 div: 0.4) = -3
	#mod: answers the remainder from this division. See comments and examples there.
	See #//, #quo:, #div:"
	"Answer an integer q such that: 
		for some r, aNumber * q + r = self
		with 0 <= r < | aNumber |"

	aNumber positive ifTrue: [ ^self // aNumber ].
	^ (self // aNumber abs) negated! !

!Number methodsFor: 'arithmetic' stamp: 'jmv 6/15/2015 16:35'!
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


!Float methodsFor: 'mathematical functions' stamp: 'nice 2/12/2015 02:48'!
timesTwoPower: anInteger 
	"Primitive. Answer with the receiver multiplied by 2.0 raised
	to the power of the argument.
	Optional. See Object documentation whatIsAPrimitive."

	<primitive: 54>
	self isFinite ifFalse: [^self].
	self isZero ifTrue: [^self].
	
	"Make sure that (2.0 raisedTo: Integer) can be safely used without overflow
	For example:
		Float fminNormalized * (2.0 raisedTo: 2000) = Float infinity.
	while:
		(Float fminNormalized timesTwoPower: 2000) = (2.0 raisedTo: 2000+Float emin)."	
	anInteger > Float emax ifTrue: [^(self timesTwoPower: Float emax) timesTwoPower: anInteger - Float emax].
	
	"In case of gradual underflow, timesTwoPower: is not exact, so greatest care must be taken
	because two consecutive timesTwoPower: might differ from a single one"
	anInteger < Float emin
		ifTrue:
			[| deltaToUnderflow |
			deltaToUnderflow := Float emin - self exponent max: Float emin.
			deltaToUnderflow >= 0 ifTrue:
				["self is already near or past underflow, so don't care, result will be zero"
				deltaToUnderflow := Float emin].
			^(self timesTwoPower: deltaToUnderflow) timesTwoPower: anInteger - deltaToUnderflow].
	
	"If (2.0 raisedToInteger: anInteger) fit in a positive SmallInteger, then use faster SmallInteger conversion.
	Note that SmallInteger maxVal highBit = 30 in a 32 bits image, so 1 can be shifted 29 times."
	anInteger > -29 ifTrue: [
		anInteger < 0 ifTrue: [^ self / (1 bitShift: (0 - anInteger)) asFloat].
		anInteger < 30 ifTrue: [^ self * (1 bitShift: anInteger) asFloat]].
	
	^ self * (2.0 raisedToInteger: anInteger)! !

