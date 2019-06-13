'From Cuis 5.0 of 7 November 2016 [latest update: #3791] on 11 June 2019 at 11:07:45 am'!

!Character methodsFor: 'accessing' stamp: 'jmv 6/11/2019 11:04:59'!
codePointOfGlyphToUse
	"
	For certain ASCII characters, we prefer a non ASCII Unicode glyph if available (i.e. with TrueType fonts).
	$* codePoint hex
	$* codePointOfGlyphToUse hex
	"
	self = $- ifTrue: [ ^16r2212 ].	"WIDE MINUS"
	self = $* ifTrue: [ ^16r2217 ].	"CENTERED ASTERISK"
	^ self codePoint! !


!Character class methodsFor: 'class initialization' stamp: 'jmv 6/11/2019 11:05:22'!
initialize
	"Create the table of unique Characters.
	Character initialize
	"
	self initializeLookupTables.
	self initializeUnicodeCodePoints.! !

!Character class methodsFor: 'class initialization' stamp: 'jmv 6/11/2019 11:05:16'!
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
	
	"Additionally, see #codePointOfGlyphToUse"! !

Character initialize!
