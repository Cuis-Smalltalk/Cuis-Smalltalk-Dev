'From Cuis6.3 [latest update: #6349] on 7 May 2024 at 3:44:59 pm'!

!Number methodsFor: 'converting' stamp: 'jmv 5/7/2024 13:23:11'!
moduloTwoPiAsFloat
	"Answer the remainder of the division by 2 * pi, as a Floating Point number.
	Provides full Float precision of the remainder result, even for huge arguments.
	See comment at Fraction.
	See senders.
	Note: Not to be implemented in the Float hierarchy. Only for exact number representations.
	If receiver is ever a Float, sender should be using a Float specific operation."

	^self subclassResponsibility! !


!Fraction methodsFor: 'converting' stamp: 'jmv 5/7/2024 15:40:28'!
moduloTwoPiAsFloat
	"Answer the remainder of the division by 2 * pi, as a Floating Point number.
	Useful for periodic functions definded on Floats.
	Provides full Float precision of the remainder result, even for huge arguments.
	Evaluate comments below and compare results.
	See senders."
	"
	| bigNum |
	bigNum := 1e22 + (10202/10000).
	{'wrong!!'. bigNum asFloat sin. bigNum asFloat cos} print.
	{'wrong!!'. (bigNum+1) asFloat sin. (bigNum+1) asFloat cos} print.
	{'correct!!'. bigNum moduloTwoPiAsFloat sin. bigNum moduloTwoPiAsFloat cos} print.
	{'correct!!'. (bigNum+1) moduloTwoPiAsFloat sin. (bigNum+1) moduloTwoPiAsFloat cos} print.
	{'Reference:'.  0 sin. 0 cos} print.
	{'Reference:'.  1 sin. 1 cos} print.
	"
	"
	| bigNum |
	bigNum := 1e500 - (282532 / 100000).
	{'wrong!!'. bigNum asFloat sin. bigNum asFloat cos} print.
	{'wrong!!'. (bigNum+1) asFloat sin. (bigNum+1) asFloat cos} print.
	{'correct!!'. bigNum moduloTwoPiAsFloat sin. bigNum moduloTwoPiAsFloat cos} print.
	{'correct!!'. (bigNum+1) moduloTwoPiAsFloat sin. (bigNum+1) moduloTwoPiAsFloat cos} print.
	{'Reference:'.  0 sin. 0 cos} print.
	{'Reference:'.  1 sin. 1 cos} print.
	"
	"
	(1e22 \\ Float twoPi) print.
	(1e22+1 \\ Float twoPi) print.
	1e22 moduloTwoPiAsFloat print.
	(1e22+1) moduloTwoPiAsFloat print.
	(1e500 \\ Float twoPi) print.
	(1e500+1 \\ Float twoPi) print.
	1e500 moduloTwoPiAsFloat print.
	(1e500+1) moduloTwoPiAsFloat print.
	"

	| numberOfPiDecimals twoPiFraction |
	(self positive and: [ self <= (628/100) ]) ifTrue: [
		^self asFloat ].
	numberOfPiDecimals := ((numerator digitLength - denominator digitLength) * 256 log) ceiling + 21.
	twoPiFraction := (Fraction piAsFraction: numberOfPiDecimals) * 2.
	^ (self \\ twoPiFraction) asFloat.! !

!Fraction methodsFor: 'testing' stamp: 'jmv 5/7/2024 13:16:23'!
positive

	^numerator positive! !


!Fraction class methodsFor: 'approximations' stamp: 'jmv 5/7/2024 11:21:52'!
pi1000Digits
	"
	
	Integer pi1000Digits asNumber
	Integer pi1000Digits asNumber class
	Integer pi1000Digits asNumber / (10 raisedToInteger: Integer pi1000Digits size - 1) :: asFloat
	Integer pi1000Digits asNumber+1 / (10 raisedToInteger: Integer pi1000Digits size - 1) :: asFloat
	"

	^'3141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093844609550582231725359408128481117450284102701938521105559644622948954930381964428810975665933446128475648233786783165271201909145648566923460348610454326648213393607260249141273724587006606315588174881520920962829254091715364367892590360011330530548820466521384146951941511609433057270365759591953092186117381932611793105118548074462379962749567351885752724891227938183011949129833673362440656643086021394946395224737190702179860943702770539217176293176752384674818467669405132000568127145263560827785771342757789609173637178721468440901224953430146549585371050792279689258923542019956112129021960864034418159813629774771309960518707211349999998372978049951059731732816096318595024459455346908302642522308253344685035261931188171010003137838752886587533208381420617177669147303598253490428755468731159562863882353787593751957781857780532171226806613001927876611195909216420198' .! !

!Fraction class methodsFor: 'approximations' stamp: 'jmv 5/7/2024 11:45:21'!
piAsFraction: numberOfDecimals
	"Answer a Fraction built with the first numberOfDigits decimal digits of Pi.
	Note: It will always be less than the Real Pi. It is not a rounded version of Pi, but a truncated version of pi.
	Fraction piAsFraction: 5
	"
	^(Fraction piDigitsAsInteger: numberOfDecimals) / (10 raisedToInteger: numberOfDecimals)! !

!Fraction class methodsFor: 'approximations' stamp: 'jmv 5/7/2024 11:43:11'!
piDigitsAsInteger: numberOfDecimals
	"Answer an integer number formed by the first numberOfDecimals+1 digits of pi.
	If numberOfDecimals=2, answer 314.
	If numberOfDecimals=4, answer 31415

	Fraction piDigitsAsInteger: 2
	Fraction piDigitsAsInteger: 4
	"
	| answer characterStream manyDigits |
	manyDigits := self pi1000Digits.
	numberOfDecimals < manyDigits size ifFalse: [
		^self error: 'Can not approximate pi with so many decimals' ].
	characterStream := manyDigits readStream.
	answer := characterStream next digitValue.
	numberOfDecimals timesRepeat: [
		answer := 10 * answer + characterStream next digitValue ].
	^answer! !


!Integer methodsFor: 'converting' stamp: 'jmv 5/7/2024 13:23:26'!
moduloTwoPiAsFloat
	"Answer the remainder of the division by 2 * pi, as a Floating Point number.
	Provides full Float precision of the remainder result, even for huge arguments.
	See comment at Fraction.
	See senders."

	^self asFraction moduloTwoPiAsFloat! !


!Number methodsFor: 'converting' stamp: 'jmv 5/7/2024 13:29:13'!
inMinusPiToPiRange
	"Avoid conversion to Float if already ok"
	(self > Float pi negated and: [self <= Float pi]) ifTrue: [
		^ self ].
	^ self moduloTwoPiAsFloat inMinusPiToPiRange! !

!Number methodsFor: 'mathematical functions' stamp: 'jmv 5/7/2024 15:23:50'!
cos
	"The receiver represents an angle measured in radians. Answer its cosine.
	See comment at Fraction >> #moduloTwoPiAsFloat"

	^self moduloTwoPiAsFloat cos! !

!Number methodsFor: 'mathematical functions' stamp: 'jmv 5/7/2024 15:23:46'!
sin
	"The receiver represents an angle measured in radians. Answer its sine.
	See comment at Fraction >> #moduloTwoPiAsFloat"

	^self moduloTwoPiAsFloat sin! !

!Number methodsFor: 'mathematical functions' stamp: 'jmv 5/7/2024 13:30:39'!
tan
	"The receiver represents an angle measured in radians. Answer its 
	tangent."

	^self moduloTwoPiAsFloat tan! !

