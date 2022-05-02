'From Cuis 6.0 [latest update: #5045] on 25 January 2022 at 9:34:42 am'!

!Character class methodsFor: 'class initialization' stamp: 'len 12/28/2021 12:09:45'!
greekLowercaseLetters
	"Answer the lowercase greek letters included in our font within the ASCII range.
	These are considered lowercase letters and can be used as identifiers (variable names, keyword or unary messages, etc)."
	^ 'àâäãåçéèµò'! !

!Character class methodsFor: 'class initialization' stamp: 'len 12/28/2021 12:09:40'!
greekUppercaseLetters
	"Answer the uppercase greek letters included in our font within the ASCII range.
	These are considered uppercase letters and can be used as identifiers (variable names, keyword or unary messages, etc)."
	^ 'ñó'! !

!Character class methodsFor: 'class initialization' stamp: 'len 1/9/2022 07:55:43'!
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

!Character class methodsFor: 'constants' stamp: 'len 1/8/2022 10:53:19'!
namedCharactersTable
	"Table of named characters that we can enter in an editor using \name.
	Please keep the names short and try to follow the naming convention used in LaTeX."
	^ #((left $) (right $) (up $) (down $) (oplus $ê) (otimes $ë) (times $◊) (div $˜) #(circ $í) (cdot $∑) (prod $ì) (coprod $î) (perp $ï) (alpha $à) (beta $â) (gamma $ä) (delta $ã) (epsilon $å) (lambda $ç) (mu $µ) (pi $é) (zeta $è) (Delta $ñ) (Gamma $ó) (omega $ò) (N $Ç) (Z $É) (Q $Ñ) (R $Ö) (C $Ü) (P $á) (infty $Ä) (aleph $Å) (sqrt $ü) (partial $û) (deg $∞))! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'len 8/20/2021 16:00:03'!
printIt
	"Treat the current text selection as an expression; evaluate it. Insert the 
	description of the result of evaluation after the selection and then make 
	this description the new text selection."
	| rpt |
	self
		evaluateSelectionAndDo: [ :result |
			rpt _ model fullPrintIt
				ifTrue: [result printText]
				ifFalse: [result printTextLimitedTo: 10000].
			rpt _ rpt copyReplaceAll: Character lf asString with: Character lf asString, ' '.
			self afterSelectionInsertAndSelect: 
				((' ', rpt, ' ') initialFontFrom: emphasisHere)]
		ifFail: [ morph flash ]
		profiled: false.! !

Character initializeUnicodeCodePoints!

