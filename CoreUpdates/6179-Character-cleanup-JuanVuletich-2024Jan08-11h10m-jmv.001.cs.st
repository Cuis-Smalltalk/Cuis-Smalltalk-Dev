'From Cuis6.3 [latest update: #6171] on 8 January 2024 at 11:21:58 am'!

!Character methodsFor: 'testing' stamp: 'jmv 1/8/2024 11:13:00'!
isVowel
	"Answer whether the receiver is one of the vowels, AEIOU, in upper or 
	lower case, and with various diacritical marks."

self flag: #unicodeNeedsImprovement.
	^Character vowels includes: self! !


!Character class methodsFor: 'class initialization' stamp: 'jmv 1/8/2024 11:13:46'!
accentedLetters
	"Answer all letters with all possible accents (combining diacritical marks).
	Order of this array is:
		unaccented
		U+0300 	COMBINING GRAVE ACCENT
		U+0301 	COMBINING ACUTE ACCENT
		U+0302 	COMBINING CIRCUMFLEX ACCENT
		U+0303 	COMBINING TILDE
		U+0308 	COMBINING DIAERESIS
		U+030A 	COMBINING RING ABOVE
		U+030C 	COMBINING CARON
	Do not include letters that can't have any accent in ISO 8859-15
	Denote unsupported combinations (for example A with caron, or N with accute accent) by a $-"
self flag: #unicodeNeedsImprovement.
	^#(
		'AÀÁÂÃÄÅ-'
		'aàáâãäå-'
		'EÈÉÊ-Ë--'
		'eèéê-ë--'
		'IÌÍÎ-Ï--'
		'iìíî-ï--'
		'OÒÓÔÕÖ--'
		'oòóôõö--'
		'UÙÚÛ-Ü--'
		'uùúû-ü--'
		'N---Ñ---'
		'n---ñ---'
		'S------Š'
		's------š'
		'Y-Ý--Ÿ--'
		'y-ý--ÿ--'
		'Z------Ž'
		'z------ž'
	).! !

!Character class methodsFor: 'class initialization' stamp: 'jmv 1/8/2024 11:14:27'!
uppercaseLowercaseAndUnaccentedLetters
	"Answer all uppercase and lowercase letters in standard collation order.
	For accented characters, include the non-accented ones.
	Note that $ß is 'LATIN SMALL LETTER SHARP S' and ISO 8859-15 doesn't have an uppercase version."
self flag: #unicodeNeedsImprovement.
	^#(
		'Aa' 'ÀàAa' 'ÁáAa' 'ÂâAa' 'ÃãAa' 'ÄäAa' 'ÅåAa'
		'Bb' 'Cc' 'Çç' 'Dd'
			'Ee' 'ÈèEe' 'ÉéEe' 'ÊêEe' 'ËëEe'
		'Ff' 'Gg' 'Hh'
			'Ii' 'ÌìIi' 'ÍíIi' 'ÎîIi' 'ÏïIi'
		'Jj' 'Kk' 'Ll' 'Mm' 'Nn' 'Ññ'
			'Oo' 'ÒòOo' 'ÓóOo' 'ÔôOo' 'ÕõOo' 'ÖöOo'
		'Pp' 'Qq' 'Rr'
			'Ss' 'ŠšSs'
		'Tt'
			'Uu' 'ÙùUu' 'ÚúUu' 'ÛûUu' 'ÜüUu'
		'Vv' 'Ww' 'Xx'
			'Yy' 'ÝýYy' 'ŸÿYy'
			'Zz' 'ŽžZz' 
		'Ææ' 'Ðð' 'Øø' 'Þþ' 'ß' 'Œœ'
	).! !

!Character class methodsFor: 'instance creation' stamp: 'jmv 1/8/2024 11:15:30'!
separators
self flag: #unicodeNeedsImprovement.
	"Also see #isSeparator"
	^ #(32 "space"
		13 "cr"
		9 "tab"
		10 "line feed"
		12 "form feed")
		collect: [:v | Character numericValue: v]

	
! !

!methodRemoval: Character class #greekUppercaseLetters stamp: 'jmv 1/8/2024 11:13:27'!
Character class removeSelector: #greekUppercaseLetters!
!methodRemoval: Character class #lfCharacter stamp: 'jmv 1/8/2024 11:17:45'!
Character class removeSelector: #lfCharacter!
!methodRemoval: Character class #greekLowercaseLetters stamp: 'jmv 1/8/2024 11:13:25'!
Character class removeSelector: #greekLowercaseLetters!
!methodRemoval: Character class #crCharacter stamp: 'jmv 1/8/2024 11:16:28'!
Character class removeSelector: #crCharacter!
!methodRemoval: Character class #blackboardLetters stamp: 'jmv 1/8/2024 11:14:06'!
Character class removeSelector: #blackboardLetters!

!Character class reorganize!
('class initialization' accentedLetters uppercaseLowercaseAndUnaccentedLetters vowels)
('instance creation' asciiValue: codePoint: digitValue: new numericValue: separators)
('accessing untypeable characters' backspace cr delete end enter escape home insert lf newLineCharacter newPage null pageDown pageUp shortUnderscore smalltalkDownArrow smalltalkLeftArrow smalltalkRightArrow smalltalkUpArrow space tab)
('converting' evaluate:withUtf16WordsOfUnicodeCodePoint: evaluate:withUtf8BytesOfUnicodeCodePoint: evaluateOnce:withUtf8BytesOfUnicodeCodePoint: macRomanToLatin1: utf8BytesAndCodePointAt:in:into:into: utf8BytesAndCodePointFor:byte2:byte3:byte4:into:into: utf8BytesAndCodePointForIso8859s1:into:into: utf8BytesOfUnicodeCodePoint:)
('static state' basicLookupTables lowercaseMappingTable unaccentedTable uppercaseMappingTable)
!

