'From Cuis 6.0 [latest update: #5559] on 28 November 2022 at 12:07:00 pm'!
!classDefinition: #UnicodeCodePoint category: #'Kernel-Text'!
Object subclass: #UnicodeCodePoint
	instanceVariableNames: 'value '
	classVariableNames: 'BinarySelectorsTable LowercaseLettersTable UppercaseLettersTable SymbolsLikeTable DiacriticalMarksTable '
	poolDictionaries: ''
	category: 'Kernel-Text'!

!Character methodsFor: 'testing' stamp: 'jmv 11/28/2022 10:12:17'!
isDiacriticalMark
	"No diacritical marks are instances of Character.
	See other implementors."

	^ false! !

!Character methodsFor: 'testing' stamp: 'jmv 11/28/2022 10:32:22'!
isSymbolLike
	^self asUnicodeCodePoint isSymbolLike! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 11/28/2022 10:19:15'!
isDiacriticalMark
	"
	(UnicodeCodePoint codePoint: 16r0041) isDiacriticalMark
	(UnicodeCodePoint codePoint: 16r20D7) isDiacriticalMark
	"
	| i |
	i _ value + 1.
	DiacriticalMarksTable bitSize < i ifTrue: [ ^false ].
	^ DiacriticalMarksTable bitBooleanAt: i.! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 11/28/2022 10:30:07'!
isSymbolLike
	"
	(UnicodeCodePoint codePoint: 16r0041) isSymbolLike
	(UnicodeCodePoint codePoint: 16r002B) isSymbolLike
	(UnicodeCodePoint codePoint: 16r03F6) isSymbolLike
	(UnicodeCodePoint codePoint: 16r2140) isSymbolLike
	"
	| i |
	i _ value + 1.
	SymbolsLikeTable bitSize < i ifTrue: [ ^false ].
	^ SymbolsLikeTable bitBooleanAt: i.! !

!UnicodeCodePoint methodsFor: 'converting' stamp: 'jmv 11/28/2022 10:26:54'!
asUnicodeCodePoint
	^self! !


!UnicodeCodePoint class methodsFor: 'class initialization' stamp: 'jmv 11/28/2022 12:06:35'!
initialize
	"https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
	http://www.unicode.org/L2/L1999/UnicodeData.html
	UnicodeCodePoint initialize
	"

	| fields codePoint codePointName generalCategory |
	UppercaseLettersTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	LowercaseLettersTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	SymbolsLikeTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	DiacriticalMarksTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	(DirectoryEntry smalltalkImageDirectory // 'UnicodeData.txt') readStreamDo: [ :strm |
		[strm atEnd] whileFalse: [
			fields _ strm nextLine substringsSeparatedBy: $;.
			(#(4 5 6) includes: fields first size) ifFalse: [ fields first print. self halt ].
			codePoint _ ('16r', fields first) asNumber.
			codePointName _ fields at: 2.
			generalCategory _ fields at: 3.
			codePoint <= 16r1FFFF ifTrue: [
				generalCategory = 'Lu' ifTrue: [ UppercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
				generalCategory = 'Ll' ifTrue: [ LowercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Pc'. 'Po'. 'Sm'. 'Sc'. 'Sk'. 'So' } includes: generalCategory) ifTrue: [ SymbolsLikeTable bitBooleanAt: codePoint+1 put: true ].
				({ 'Mn'. 'Mc'. 'Me' } includes: generalCategory) ifTrue: [ DiacriticalMarksTable bitBooleanAt: codePoint+1 put: true ]].
			"
			cannonicalCombiningClasses _ fields at: 4.
			characterDecompositionMapping _ fields at: 6.
			uppercaseMapping _ fields at: 13. lowercaseMapping _ fields at: 14. titlecaseMapping _ fields at: 15.
			Random next > 10.999 | (generalCategory = 'So') ifTrue: [
				{codePoint hex. codePointName. generalCategory} print ].
			decimalDigitValue _ fields at: 7. digitValue _ fields at: 8. numericValue _ fields at: 9.
			decimalDigitValue isEmpty & digitValue isEmpty & numericValue isEmpty ifFalse: [
				{'numeric'. codePoint. codePointName. generalCategory. decimalDigitValue. digitValue. numericValue} print].
			"
			]].! !

!classDefinition: #UnicodeCodePoint category: #'Kernel-Text'!
Object subclass: #UnicodeCodePoint
	instanceVariableNames: 'value'
	classVariableNames: 'BinarySelectorsTable DiacriticalMarksTable LowercaseLettersTable SymbolsLikeTable UppercaseLettersTable'
	poolDictionaries: ''
	category: 'Kernel-Text'!
UnicodeCodePoint initialize!
