'From Cuis 5.0 [latest update: #4146] on 17 May 2020 at 5:49:20 am'!

!Fraction commentStamp: '<historical>' prior: 0!
Fraction provides methods for dealing with rational numbers like 1/3 as true fractions (not approximations as a Float 0.33333...). All public arithmetic operations answer reduced fractions, or Integers when the denominator is 1.

Examples: (note the parentheses required to get the right answers in Smalltalk):

(2/3) + (2/3)
(6/4)		 		"Fractions are reduced to the smallest numerator and denominator possible"
(1/-3)			"Denominator is kept positive, the sign is always in the numerator"
(2/3) + (1/3)	"When the denominator reduces to 1, the answer is an Integer"
!


!Integer commentStamp: '<historical>' prior: 0!
I am a common abstract superclass for all Integer implementations. My implementation subclasses are SmallInteger, LargePositiveInteger, and LargeNegativeInteger.

There are three implementation of Euclidean division of 'a' by 'b', where 'a' and 'b' are Integers considered as elements of the rational integers ring Z:
	- Euclidean division with the quotient rounded towards negative infinity: // and \\ answer 'q' and 'r' such that 'a = bq + r' with 'r abs < b abs', where 'r' is 0 or has the same sign as 'b'. This is sometimes called Knuth's division, and it matches the division commonly implemented in CPUs.
	- Euclidean division with the quotient rounded towards zero: #quo: and #rem: answer 'q' and 'r' such that 'a = bq + r' with 'r abs < b abs', where 'r' is 0 or has the same sign as 'a'.
	- Euclidean division with non-negative remainder: #div: and #mod: answer 'q' and 'r' such that 'a = bq + r' with '0 <= r < b abs'.
Note that, taking the absolute value as Euclidean function, all of these divisions comply with the definition of Euclidean division. However requiring only 'r abs < b abs' is not enough for producing a unique quotient and remainder, and the additional requirements for the sign of 'r' (different in each of the three kinds of division) guarantee a unique choice of quotient and remainder.

Additionally, the division in the rational field is implemented with the message / that answers a Fraction 'a/b' if the result is not a whole integer. Note that in the current design of the Number hierarchy, because Fractions reduce to Integers when the denominator is 1, an Integer per-se doesn't know if it is an element of the rational integers ring Z or a member of the rational field Q. In the rational field Q, the quotient of the Euclidean division is / and the remainder is always 0, and the other three divisions are not Euclidean divisions.!


!Number methodsFor: 'arithmetic' stamp: 'len 5/17/2020 05:40:28'!
div: aNumber
	"Integer division with non-negative remainder.
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

!Number methodsFor: 'arithmetic' stamp: 'len 5/17/2020 05:39:46'!
mod: divisor
	"Modulo operation. Remainder of the integer division #div:.
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

!Number methodsFor: 'arithmetic' stamp: 'len 5/17/2020 05:35:19'!
quo: aNumber
	"Integer division with truncation towards zero.
	(-9 quo: 4) = -2
	(-0.9 quo: 0.4) = -2
	#rem: answers the remainder from this division.
	See #//, #quo:, #div:"

	^ (self / aNumber) truncated! !

!Number methodsFor: 'arithmetic' stamp: 'len 5/17/2020 05:41:49'!
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

