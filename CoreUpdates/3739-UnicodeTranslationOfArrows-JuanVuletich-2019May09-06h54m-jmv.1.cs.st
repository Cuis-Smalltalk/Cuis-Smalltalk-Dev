'From Cuis 5.0 of 7 November 2016 [latest update: #3738] on 9 May 2019 at 7:00:28 am'!

!Character class methodsFor: 'class initialization' stamp: 'jmv 5/9/2019 07:00:25'!
initialize
	"Create the table of unique Characters.
	Character initialize
	"
	self initializeLookupTables.
	self initializeUnicodeCodePoints.! !

!Character class methodsFor: 'class initialization' stamp: 'jmv 5/7/2019 19:52:45'!
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

	UnicodeCodePoints at: 16r80+1 put: 16r2200.		"FOR ALL"
	UnicodeCodePoints at: 16r81+1 put: 16r2202.		"PARTIAL DIFFERENTIAL"
	UnicodeCodePoints at: 16r82+1 put: 16r2203.		"THERE EXISTS"
	UnicodeCodePoints at: 16r83+1 put: 16r2204.		"THERE DOES NOT EXIST"
	UnicodeCodePoints at: 16r84+1 put: 16r2205.		"EMPTY SET"
	UnicodeCodePoints at: 16r85+1 put: 16r221E.		"INFINITY"
	UnicodeCodePoints at: 16r86+1 put: 16r2102.		"DOUBLE-STRUCK CAPITAL C"
	UnicodeCodePoints at: 16r87+1 put: 16r210D.		"DOUBLE-STRUCK CAPITAL H"
	UnicodeCodePoints at: 16r88+1 put: 16r2115.		"DOUBLE-STRUCK CAPITAL N"
	UnicodeCodePoints at: 16r89+1 put: 16r2119.		"DOUBLE-STRUCK CAPITAL P"
	UnicodeCodePoints at: 16r8A+1 put: 16r211A.		"DOUBLE-STRUCK CAPITAL Q"
	UnicodeCodePoints at: 16r8B+1 put: 16r211D.		"DOUBLE-STRUCK CAPITAL R"
	UnicodeCodePoints at: 16r8C+1 put: 16r2124.		"DOUBLE-STRUCK CAPITAL Z"
	UnicodeCodePoints at: 16r8D+1 put: 16r2135.		"ALEF SYMBOL"
	UnicodeCodePoints at: 16r8E+1 put: 16r2A00.		"N-ARY CIRCLED DOT OPERATOR"
	UnicodeCodePoints at: 16r8F+1 put: 16r2A01.		"N-ARY CIRCLED PLUS OPERATOR"
	UnicodeCodePoints at: 16r90+1 put: 16r2A02.		"N-ARY CIRCLED TIMES OPERATOR"
	UnicodeCodePoints at: 16r91+1 put: 16r2211.		"N-ARY SUMMATION"
	UnicodeCodePoints at: 16r92+1 put: 16r222B.		"INTEGRAL"
	UnicodeCodePoints at: 16r93+1 put: 16r2A15.		"INTEGRAL AROUND A POINT OPERATOR"
	UnicodeCodePoints at: 16r94+1 put: 16r2260.		"NOT EQUAL TO"
	UnicodeCodePoints at: 16r95+1 put: 16r2261.		"IDENTICAL TO"
	UnicodeCodePoints at: 16r96+1 put: 16r2262.		"NOT IDENTICAL TO"
	UnicodeCodePoints at: 16r97+1 put: 16r2263.		"STRICTLY EQUIVALENT TO"
	UnicodeCodePoints at: 16r98+1 put: 16r2264.		"LESS-THAN OR EQUAL TO"
	UnicodeCodePoints at: 16r99+1 put: 16r2265.		"GREATER-THAN OR EQUAL TO"
	UnicodeCodePoints at: 16r9A+1 put: 16r2266.		"LESS-THAN OVER EQUAL TO"
	UnicodeCodePoints at: 16r9B+1 put: 16r2267.		"GREATER-THAN OVER EQUAL TO"
	UnicodeCodePoints at: 16r9C+1 put: 16r2268.		"LESS-THAN BUT NOT EQUAL TO"
	UnicodeCodePoints at: 16r9D+1 put: 16r2269.		"GREATER-THAN BUT NOT EQUAL TO"
	UnicodeCodePoints at: 16r9E+1 put: 16r2218.		"RING OPERATOR"
	UnicodeCodePoints at: 16r9F+1 put: 16r2219.		"BULLET OPERATOR"! !

Character initialize!
