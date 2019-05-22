'From Cuis 5.0 of 7 November 2016 [latest update: #3741] on 12 May 2019 at 10:33:47 pm'!

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:42:20'!
FF
	"
	Character FF
	"
	^ $! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:34:09'!
circ
	"
	Character circ
	"
	^ $•! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:39:22'!
div
	"
	Character div
	"
	^ $÷! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:33:06'!
epsilon
	"
	Character epsilon
	"
	^ $„! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:39:44'!
pi
	"
	Character pi
	"
	^ $ƒ! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:40:22'!
sqrt
	"
	Character sqrt
	"
	^ $Ÿ! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:44:27'!
zeta
	"
	Character zeta
	"
	^ $…! !


!Character class methodsFor: 'class initialization' stamp: 'len 5/12/2019 22:31:40'!
initializeUnicodeCodePoints
	"
	Character initializeUnicodeCodePoints
	String streamContents: [ :strm | 28 to: 255 do: [ :i | strm nextPut: (Character numericValue: i) ]]
	"

	"Initialize the table of Unicode code points"
	UnicodeCodePoints _ Array new: 256.
	0 to: 255 do: [ :code |
		UnicodeCodePoints at: code + 1 put: code ].

	"Arrows"
	UnicodeCodePoints at: 28+1 put: 8592.		"left arrow"
	UnicodeCodePoints at: 29+1 put: 8594.		"right arrow"
	UnicodeCodePoints at: 30+1 put: 8593.		"up arrow"
	UnicodeCodePoints at: 31+1 put: 8595.		"down arrow"
	
	"The following codes are different in ISO 8859-15 from those in ISO 8859-1,
	so the character code is not equal to the Unicode code point"
	UnicodeCodePoints at: 16rA4+1 put: 16r20AC.		"euro sign"
	UnicodeCodePoints at: 16rA6+1 put: 16r160.		"latin capital letter S with caron"
	UnicodeCodePoints at: 16rA8+1 put: 16r161.		"latin small letter s with caron"
	UnicodeCodePoints at: 16rB4+1 put: 16r17D.		"latin capital letter Z with caron"
	UnicodeCodePoints at: 16rB8+1 put: 16r17E.		"latin small letter z with caron"
	UnicodeCodePoints at: 16rBC+1 put: 16r152.		"latin capital ligature OE"
	UnicodeCodePoints at: 16rBD+1 put: 16r153.		"latin small ligature oe"
	UnicodeCodePoints at: 16rBE+1 put: 16r178.		"latin capital letter Y with diaeresis"

	UnicodeCodePoints at: 16r80+1 put: 16r2205.		"EMPTY SET"
	UnicodeCodePoints at: 16r81+1 put: 16r221E.		"INFINITY"
	UnicodeCodePoints at: 16r82+1 put: 16r2135.		"ALEF SYMBOL"
	UnicodeCodePoints at: 16r83+1 put: 16r03C0.		"pi"
	UnicodeCodePoints at: 16r84+1 put: 16r03B5.		"epsilon"
	UnicodeCodePoints at: 16r85+1 put: 16r03B6.		"zeta"
	UnicodeCodePoints at: 16r86+1 put: 16r0394.		"Delta"
	UnicodeCodePoints at: 16r87+1 put: 16r03A9.		"Omega"
	UnicodeCodePoints at: 16r88+1 put: 16r2102.		"DOUBLE-STRUCK CAPITAL C"
	UnicodeCodePoints at: 16r89+1 put: 16r210D.		"DOUBLE-STRUCK CAPITAL H"
	UnicodeCodePoints at: 16r8A+1 put: 16r2115.		"DOUBLE-STRUCK CAPITAL N"
	UnicodeCodePoints at: 16r8B+1 put: 16r2119.		"DOUBLE-STRUCK CAPITAL P"
	UnicodeCodePoints at: 16r8C+1 put: 16r211A.		"DOUBLE-STRUCK CAPITAL Q"
	UnicodeCodePoints at: 16r8D+1 put: 16r211D.		"DOUBLE-STRUCK CAPITAL R"
	UnicodeCodePoints at: 16r8E+1 put: 16r2124.		"DOUBLE-STRUCK CAPITAL Z"
	UnicodeCodePoints at: 16r8F+1 put: 16r0046.		"DOUBLE-STRUCK CAPITAL F" "should be 1D53D, index out of bounds"
	UnicodeCodePoints at: 16r90+1 put: 16r2295.		"OPLUS"
	UnicodeCodePoints at: 16r91+1 put: 16r2297.		"OTIMES"
	UnicodeCodePoints at: 16r92+1 put: 16r2299.		"ODOT"
	UnicodeCodePoints at: 16r93+1 put: 16r2296.		"OMINUS"
	UnicodeCodePoints at: 16r94+1 put: 16r2217.		"ASTERISK OPERATOR"
	UnicodeCodePoints at: 16r95+1 put: 16r2218.		"RING OPERATOR (circ)"
	UnicodeCodePoints at: 16r96+1 put: 16r2219.		"BULLET OPERATOR (filled circ)"
	UnicodeCodePoints at: 16r97+1 put: 16r22C5.		"DOT OPERATOR" "(already at B7)"
	UnicodeCodePoints at: 16r98+1 put: 16r22C6.		"STAR OPERATOR"
	UnicodeCodePoints at: 16r99+1 put: 16r21A0.		"SURJECTIVE ARROW"
	UnicodeCodePoints at: 16r9A+1 put: 16r21A3.		"INJECTIVE ARROW"
	UnicodeCodePoints at: 16r9B+1 put: 16r2194.		"BIJECTIVE ARROW"
	UnicodeCodePoints at: 16r9C+1 put: 16r21AA.		"INCLUSION ARROW"
	UnicodeCodePoints at: 16r9D+1 put: 16r21A6.		"MAPPING ARROW"
	UnicodeCodePoints at: 16r9E+1 put: 16r2202.		"PARTIAL DIFFERENTIAL"
	UnicodeCodePoints at: 16r9F+1 put: 16r221A.		"SQUARE ROOT"
	
	UnicodeCodePoints at: 16r2D+1 put: 16r2212.		"WIDE MINUS"
	UnicodeCodePoints at: 16r2A+1 put: 16r2217.		"CENTERED ASTERISK"! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:41:10'!
CC
	"
	Character CC
	"
	^ $ˆ! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:41:20'!
HH
	"
	Character HH
	"
	^ $‰! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:41:29'!
NN
	"
	Character NN
	"
	^ $Š! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:41:38'!
PP
	"
	Character PP
	"
	^ $‹! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:41:48'!
QQ
	"
	Character QQ
	"
	^ $Œ! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:41:55'!
RR
	"
	Character RR
	"
	^ $! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:42:11'!
ZZ
	"
	Character ZZ
	"
	^ $Ž! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:43:23'!
aleph
	"
	Character aleph
	"
	^ $‚! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:35:59'!
bullet
	"
	Character bullet
	"
	^ $–! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:33:17'!
emptySet
	"
	Character emptySet
	"
	^ $€! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:36:47'!
infinity
	"
	Character infinity
	"
	^ $! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:36:35'!
oplus
	"
	Character oplus
	"
	^ $! !

!Character class methodsFor: 'accessing mathematical symbols' stamp: 'len 5/12/2019 21:37:52'!
otimes
	"
	Character otimes
	"
	^ $‘! !


!String class methodsFor: 'initialization' stamp: 'len 5/12/2019 21:51:05'!
initialize
	"
	String initialize
	"

	| order newOrder lowercase |

	"Case insensitive compare sorts null, space, digits, letters, all the rest..."
	newOrder _ Array new: 256.
	order _ -1.
	newOrder at: 0+1 put:  (order _ order+1).
	32 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order _ order+1)].
	#(92 94 124 126) do: [ :c | "\^|~"
		newOrder at: c + 1 put: (order _ order+1)].
	16r90 to: 16r9F do: [:c| "special math operators"
		newOrder at: c + 1 put: (order _ order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		order _ order+1.
		newOrder at: upperAndLowercase first numericValue + 1 put: order.
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase second numericValue + 1 put: order ]].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order _ order+1)]].
	CaseInsensitiveOrder _ newOrder asByteArray.
	
	"Case sensitive compare sorts null, space, digits, letters, all the rest..."
	newOrder _ Array new: 256.
	order _ -1.
	newOrder at: 0+1 put:  (order _ order+1).
	32 to: 64 do: [ :c |
		newOrder at: c + 1 put: (order _ order+1)].
	#(92 94 124 126) do: [ :c | "\^|~"
		newOrder at: c + 1 put: (order _ order+1)].
	16r90 to: 16r9F do: [:c| "special math operators"
		newOrder at: c + 1 put: (order _ order+1)].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		upperAndLowercase size > 1 ifTrue: [
			newOrder at: upperAndLowercase first numericValue + 1 put: (order _ order+1) ]].
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :upperAndLowercase |
		lowercase _ upperAndLowercase size = 1
			ifTrue: [ upperAndLowercase first ]
			ifFalse: [ upperAndLowercase second ].
		newOrder at: lowercase numericValue + 1 put: (order _ order+1) ].
	1 to: newOrder size do: [ :i |
		(newOrder at: i) ifNil: [
			newOrder at: i put: (order _ order+1)]].
	order = 255 ifFalse: [self error: 'order problem'].
	CaseSensitiveOrder _ newOrder asByteArray.

	"a table for translating to lower case"
	LowercasingTable _ String withAll: (Character characterTable collect: [:c | c asLowercase]).

	"a table for translating to upper case"
	UppercasingTable _ String withAll: (Character characterTable collect: [:c | c asUppercase]).

	"a table for testing tokenish (for fast numArgs)"
	Tokenish _ String withAll: (Character characterTable collect:
									[:c | c tokenish ifTrue: [c] ifFalse: [$~]]).

	"CR and LF--characters that terminate a line"
	CSLineEnders _ CharacterSet new.
	CSLineEnders add: Character cr.
	CSLineEnders add: Character lf.

 	"separators and non-separators"
	CSSeparators _ CharacterSet separators.
	CSNonSeparators _ CSSeparators complement! !

!methodRemoval: Character class #circle!
Character class removeSelector: #circle!
!methodRemoval: Character class #contourIntegral!
Character class removeSelector: #contourIntegral!
!methodRemoval: Character class #doesNotExist!
Character class removeSelector: #doesNotExist!
!methodRemoval: Character class #exists!
Character class removeSelector: #exists!
!methodRemoval: Character class #forAll!
Character class removeSelector: #forAll!
!methodRemoval: Character class #greaterNotEqual!
Character class removeSelector: #greaterNotEqual!
!methodRemoval: Character class #greaterOrEqual!
Character class removeSelector: #greaterOrEqual!
!methodRemoval: Character class #greaterOverEqual!
Character class removeSelector: #greaterOverEqual!
!methodRemoval: Character class #identical!
Character class removeSelector: #identical!
!methodRemoval: Character class #integral!
Character class removeSelector: #integral!
!methodRemoval: Character class #lessNotEqual!
Character class removeSelector: #lessNotEqual!
!methodRemoval: Character class #lessOrEqual!
Character class removeSelector: #lessOrEqual!
!methodRemoval: Character class #lessOverEqual!
Character class removeSelector: #lessOverEqual!
!methodRemoval: Character class #notEqual!
Character class removeSelector: #notEqual!
!methodRemoval: Character class #notIdentical!
Character class removeSelector: #notIdentical!
!methodRemoval: Character class #odot!
Character class removeSelector: #odot!
!methodRemoval: Character class #partial!
Character class removeSelector: #partial!
!methodRemoval: Character class #strictlyEquivalent!
Character class removeSelector: #strictlyEquivalent!
!methodRemoval: Character class #summation!
Character class removeSelector: #summation!
Character initialize!
String initialize!

