'From Cuis 6.0 [latest update: #5531] on 26 October 2022 at 4:42:38 pm'!

!Character class methodsFor: 'class initialization' stamp: 'jmv 10/26/2022 16:32:44'!
initializeUnicodeCodePoints
	"
	Character initializeUnicodeCodePoints
	String streamContents: [ :strm | 32 to: 255 do: [ :i | strm nextPut: (Character numericValue: i) ]]
	"

	"Initialize the table of Unicode code points"
	UnicodeCodePoints _ Array new: 256.
	0 to: 255 do: [ :code |
		UnicodeCodePoints at: code + 1 put: code ].
	
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

	"Mathematical symbols (see Character>>#namedCharactersTable)"
	UnicodeCodePoints at: 16r80+1 put: 16r221E.		"infinity"
	UnicodeCodePoints at: 16r81+1 put: 16r2135.		"aleph"
	UnicodeCodePoints at: 16r82+1 put: 16r2115.		"blackboard bold N"
	UnicodeCodePoints at: 16r83+1 put: 16r2124.		"blackboard bold Z"
	UnicodeCodePoints at: 16r84+1 put: 16r211A.		"blackboard bold Q"
	UnicodeCodePoints at: 16r85+1 put: 16r211D.		"blackboard bold R"
	UnicodeCodePoints at: 16r86+1 put: 16r2102.		"blackboard bold C"
	UnicodeCodePoints at: 16r87+1 put: 16r2119.		"blackboard bold P"
	UnicodeCodePoints at: 16r88+1 put: 16r03B1.		"alpha"
	UnicodeCodePoints at: 16r89+1 put: 16r03B2.		"beta"
	UnicodeCodePoints at: 16r8A+1 put: 16r03B3.		"gamma"
	UnicodeCodePoints at: 16r8B+1 put: 16r03B4.		"delta"
	UnicodeCodePoints at: 16r8C+1 put: 16r03B5.		"epsilon"
	UnicodeCodePoints at: 16r8D+1 put: 16r03BB.		"lambda"
	UnicodeCodePoints at: 16r8E+1 put: 16r03C0.		"pi"
	UnicodeCodePoints at: 16r8F+1 put: 16r03B6.		"zeta"
	UnicodeCodePoints at: 16r90+1 put: 16r2295.		"oplus"
	UnicodeCodePoints at: 16r91+1 put: 16r2297.		"otimes"
	UnicodeCodePoints at: 16r92+1 put: 16r2218.		"circ"
	UnicodeCodePoints at: 16r93+1 put: 16r2293.		"prod"
	UnicodeCodePoints at: 16r94+1 put: 16r2294.		"coprod"
	UnicodeCodePoints at: 16r95+1 put: 16r22A5.		"perp"
	UnicodeCodePoints at: 16r96+1 put: 16r0394.		"Delta"
	UnicodeCodePoints at: 16r97+1 put: 16r0393.		"Gamma"
	UnicodeCodePoints at: 16r98+1 put: 16r03C9.		"omega"
	UnicodeCodePoints at: 16r99+1 put: 16r21A0.		"SURJECTIVE ARROW"
	UnicodeCodePoints at: 16r9A+1 put: 16r21A3.		"INJECTIVE ARROW"
	UnicodeCodePoints at: 16r9B+1 put: 16r2194.		"BIJECTIVE ARROW"
	UnicodeCodePoints at: 16r9C+1 put: 16r21AA.		"INCLUSION ARROW"
	UnicodeCodePoints at: 16r9D+1 put: 16r21A6.		"MAPPING ARROW"
	UnicodeCodePoints at: 16r9E+1 put: 16r2202.		"partial differential"
	UnicodeCodePoints at: 16r9F+1 put: 16r221A.		"sqrt"! !


!String class methodsFor: 'services' stamp: 'jmv 10/26/2022 16:31:26'!
isAscii: aStringOrByteArray
	"Return true if all bytes in argument are less than 128.
	This means it is a pure 7-bit ASCII.
	Valid both for instances of String (possibly with accented latin letters or other Character)
	and for utf-8 encoded ByteArrays.
	In these cases, the bytes represent the same ascii string both as String and as Utf8.
	See also #nextUtf8BytesAndCodePointInto:into:, #ascii and #nonAscii"

	^ (String
		findFirstInString: aStringOrByteArray
		inSet: `(CharacterSet nonAscii) byteArrayMap`
		startingAt: 1)
			= 0! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
	Character initializeUnicodeCodePoints!

