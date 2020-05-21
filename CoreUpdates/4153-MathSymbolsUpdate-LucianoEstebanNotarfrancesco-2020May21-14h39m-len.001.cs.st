'From Cuis 5.0 [latest update: #4152] on 21 May 2020 at 2:41:16 pm'!

!Character class methodsFor: 'class initialization' stamp: 'len 5/21/2020 08:23:36'!
blackboardLetters
	"Answer the 'blackboard bold' or 'double struck' letters included in our font within the ASCII range.
	These are considered uppercase letters and can be used as identifiers (variable names, keyword or unary messages, etc)."
	^ 'ÇÉÑÖÜá'! !

!Character class methodsFor: 'class initialization' stamp: 'len 5/21/2020 08:24:19'!
greekLowercaseLetters
	"Answer the lowercase greek letters included in our font within the ASCII range.
	These are considered lowercase letters and can be used as identifiers (variable names, keyword or unary messages, etc)."
	^ 'àâäãåçéèµ'! !

!Character class methodsFor: 'class initialization' stamp: 'len 5/21/2020 08:24:46'!
greekUppercaseLetters
	"Answer the uppercase greek letters included in our font within the ASCII range.
	These are considered uppercase letters and can be used as identifiers (variable names, keyword or unary messages, etc)."
	^ 'ñóò'! !

!Character class methodsFor: 'constants' stamp: 'len 5/21/2020 08:26:58'!
namedCharactersTable
	"Table of named characters that we can enter in an editor using \name.
	Please keep the names short and try to follow the naming convention used in LaTeX."
	^ #((left $) (right $) (up $) (down $) (oplus $ê) (otimes $ë) (times $◊) (div $˜) #(circ $í) (dot $∑) (bullet $ì) (diamond $î) (star $ï) (alpha $à) (beta $â) (gamma $ä) (delta $ã) (epsilon $å) (lambda $ç) (mu $µ) (pi $é) (zeta $è) (Delta $ñ) (Gamma $ó) (Omega $ò) (N $Ç) (Z $É) (Q $Ñ) (R $Ö) (C $Ü) (P $á) (infty $Ä) (aleph $Å) (sqrt $ü) (partial $û) (degree $∞))! !


!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'len 5/21/2020 06:41:21'!
normalCharacter: aKeyboardEvent 
	"A nonspecial character is to be added to the stream of characters."

	| stopIndex startIndex string key |
	aKeyboardEvent keyCharacter isLetter ifTrue: [^ super normalCharacter: aKeyboardEvent].
	"Allow to enter named (otherwise untypable) characters like the alpha greek letter as \alpha."
	string _ self privateCurrentString.
	stopIndex _ self pointIndex - 1.
	(stopIndex between: 2 and: string size)
		ifFalse: [^ super normalCharacter: aKeyboardEvent].
	startIndex _ stopIndex.
	"Look backwards and find a character that is not a letter (we want to find '\' just a few characters behind):"
	[startIndex > 0 and: [stopIndex - startIndex < 7 and: [(string at: startIndex) isLetter]]] whileTrue: [startIndex _ startIndex - 1].
	(startIndex > 0 and: [(string at: startIndex) = $\])
		ifFalse: [^ super normalCharacter: aKeyboardEvent].
	key _ string copyFrom: startIndex+1 to: stopIndex.
	(Character namedCharactersTable detect: [:one| key = one first] ifNone: [])
		ifNotNil: [:aPair| self selectFrom: startIndex to: stopIndex; replaceSelectionWith: aPair second asString].
	^ super normalCharacter: aKeyboardEvent! !


!Character class methodsFor: 'class initialization' stamp: 'len 5/21/2020 06:35:42'!
initializeLookupTables
	LowercaseMappingTable _ Array new: 256.
	LowercaseTruthTable _ Array new: 256.
	UppercaseMappingTable _ Array new: 256.
	UppercaseTruthTable _ Array new: 256.
	LetterTruthTable _ Array new: 256.
	UnaccentedTable _ ByteArray new: 256.
	0
		to: 255
		do: [ :idx | | char |
			"Default to an identity mapping with a false truth mapping"
			char _ self numericValue: idx.
			LowercaseMappingTable
				at: idx + 1
				put: char.
			LowercaseTruthTable
				at: idx + 1
				put: false.
			UppercaseMappingTable
				at: idx + 1
				put: char.
			UppercaseTruthTable
				at: idx + 1
				put: false.
			LetterTruthTable
				at: idx + 1
				put: false.
			UnaccentedTable at: idx + 1 put: idx].
	"Now override as needed"
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :group | | uppercase lowercase |
		group size > 1
			ifTrue: [ | lowercaseChar uppercaseChar |
				uppercase _ group first numericValue.
				lowercase _ group second numericValue.
				lowercaseChar _ self numericValue: lowercase.
				uppercaseChar _ self numericValue: uppercase.
				LowercaseMappingTable
					at: lowercase + 1
					put: lowercaseChar.
				LowercaseMappingTable
					at: uppercase + 1
					put: lowercaseChar.
				LowercaseTruthTable
					at: lowercase + 1
					put: true.
				LetterTruthTable
					at: lowercase + 1
					put: true.
				UppercaseMappingTable
					at: lowercase + 1
					put: uppercaseChar.
				UppercaseMappingTable
					at: uppercase + 1
					put: uppercaseChar.
				UppercaseTruthTable
					at: uppercase + 1
					put: true.
				LetterTruthTable
					at: uppercase + 1
					put: true.
				group size > 2
					ifTrue: [|unaccentedUppercase unaccentedLowercase|
						unaccentedUppercase _ group third numericValue.
						unaccentedLowercase _ group fourth numericValue.
						UnaccentedTable at: uppercase+1 put: unaccentedUppercase.
						UnaccentedTable at: lowercase+1 put: unaccentedLowercase]]
			ifFalse: [ | lowercaseChar |
				lowercase _ group first numericValue.
				lowercaseChar _ self numericValue: lowercase.
				LowercaseMappingTable
					at: lowercase + 1
					put: lowercaseChar.
				UppercaseMappingTable
					at: lowercase + 1
					put: lowercaseChar.
				LowercaseTruthTable
					at: lowercase + 1
					put: true.
				UppercaseTruthTable
					at: lowercase + 1
					put: false.
				LetterTruthTable
					at: lowercase + 1
					put: true ]].
		Character greekLowercaseLetters do: [:each|
			LowercaseTruthTable at: each numericValue + 1 put: true.
			LetterTruthTable at: each numericValue + 1 put: true].
		Character greekUppercaseLetters do: [:each|
			LowercaseTruthTable at: each numericValue + 1 put: true.
			LetterTruthTable at: each numericValue + 1 put: true].
		Character blackboardLetters do: [:each|
			UppercaseTruthTable at: each numericValue + 1 put: true.
			LetterTruthTable at: each numericValue + 1 put: true]! !

!Character class methodsFor: 'class initialization' stamp: 'len 5/20/2020 17:34:43'!
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


	UnicodeCodePoints at: 16r80+1 put: 16r221E.		"INFINITY"
	UnicodeCodePoints at: 16r81+1 put: 16r2135.		"ALEF SYMBOL"
	UnicodeCodePoints at: 16r82+1 put: 16r2115.		"DOUBLE-STRUCK CAPITAL N"
	UnicodeCodePoints at: 16r83+1 put: 16r2124.		"DOUBLE-STRUCK CAPITAL Z"
	UnicodeCodePoints at: 16r84+1 put: 16r211A.		"DOUBLE-STRUCK CAPITAL Q"
	UnicodeCodePoints at: 16r85+1 put: 16r211D.		"DOUBLE-STRUCK CAPITAL R"
	UnicodeCodePoints at: 16r86+1 put: 16r2102.		"DOUBLE-STRUCK CAPITAL C"
	UnicodeCodePoints at: 16r87+1 put: 16r2119.		"DOUBLE-STRUCK CAPITAL P"
	UnicodeCodePoints at: 16r88+1 put: 16r03B1.		"alpha"
	UnicodeCodePoints at: 16r89+1 put: 16r03B2.		"beta"
	UnicodeCodePoints at: 16r8A+1 put: 16r03B3.		"gamma"
	UnicodeCodePoints at: 16r8B+1 put: 16r03B4.		"delta"
	UnicodeCodePoints at: 16r8C+1 put: 16r03B5.		"epsilon"
	UnicodeCodePoints at: 16r8D+1 put: 16r03BB.		"lambda"
	UnicodeCodePoints at: 16r8E+1 put: 16r03C0.		"pi"
	UnicodeCodePoints at: 16r8F+1 put: 16r03B6.		"zeta"
	UnicodeCodePoints at: 16r90+1 put: 16r2295.		"OPLUS"
	UnicodeCodePoints at: 16r91+1 put: 16r2297.		"OTIMES"
	UnicodeCodePoints at: 16r92+1 put: 16r2218.		"RING OPERATOR (circ)"
	UnicodeCodePoints at: 16r93+1 put: 16r2219.		"BULLET OPERATOR"
	UnicodeCodePoints at: 16r94+1 put: 16r22C4.		"DIAMOND OPERATOR"
	UnicodeCodePoints at: 16r95+1 put: 16r22C6.		"STAR OPERATOR"
	UnicodeCodePoints at: 16r96+1 put: 16r0394.		"Delta"
	UnicodeCodePoints at: 16r97+1 put: 16r0393.		"Gamma"
	UnicodeCodePoints at: 16r98+1 put: 16r03A9.		"Omega"
	UnicodeCodePoints at: 16r99+1 put: 16r21A0.		"SURJECTIVE ARROW"
	UnicodeCodePoints at: 16r9A+1 put: 16r21A3.		"INJECTIVE ARROW"
	UnicodeCodePoints at: 16r9B+1 put: 16r2194.		"BIJECTIVE ARROW"
	UnicodeCodePoints at: 16r9C+1 put: 16r21AA.		"INCLUSION ARROW"
	UnicodeCodePoints at: 16r9D+1 put: 16r21A6.		"MAPPING ARROW"
	UnicodeCodePoints at: 16r9E+1 put: 16r2202.		"PARTIAL DIFFERENTIAL"
	UnicodeCodePoints at: 16r9F+1 put: 16r221A.		"SQUARE ROOT"
! !


!Scanner class methodsFor: 'cached class state' stamp: 'len 5/21/2020 08:07:57'!
initTypeTable
	| newTable |
	newTable := Array new: 256 withAll: #xIllegal. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"

	1 to: 255
		do: [:index |
			(Character numericValue: index) isValidInIdentifiers
				ifTrue: [
					"Digits and $_ are overwritten later"
					newTable at: index put: #xLetter ]].

	newTable atAll: ($0 numericValue to: $9 numericValue) put: #xDigit.
	newTable atAll:  '°!!%&+-*/\∑◊˜¨≠Ø,<=>´ªø?@~êëíìîïûüôöõúù' asByteArray put: #xBinary.
	newTable at: self doItCharacterValue put: #doIt.
	newTable at: $" numericValue put: #xDoubleQuote.
	newTable at: $# numericValue put: #xLitQuote.
	newTable at: $$ numericValue put: #xDollar.
	newTable at: $' numericValue put: #xSingleQuote.
	newTable at: $` numericValue put: #xBacktick.
	newTable at: $: numericValue put: #xColon.
	newTable at: $( numericValue put: #leftParenthesis.
	newTable at: $) numericValue put: #rightParenthesis.
	newTable at: $. numericValue put: #period.
	newTable at: $; numericValue put: #semicolon.
	newTable at: $[ numericValue put: #leftBracket.
	newTable at: $] numericValue put: #rightBracket.
	newTable at: ${ numericValue put: #leftBrace.
	newTable at: $} numericValue put: #rightBrace.
	newTable at: $^ numericValue put: #upArrow.
	newTable at: $_ numericValue put: #xUnderscore.
	newTable at: $| numericValue put: #verticalBar.
	TypeTable := newTable "bon voyage!!"

	"
	Scanner initTypeTable
	"! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Character initialize.
Scanner initialize.!

