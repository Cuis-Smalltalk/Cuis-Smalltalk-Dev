'From Cuis 6.0 [latest update: #5319] on 25 June 2022 at 4:57:45 pm'!

!ByteArray methodsFor: 'accessing' stamp: 'jmv 6/25/2022 16:53:20'!
bitSize
	"Note:
	(ByteArray newBits: 9) bitSize = 16 
	"
	^self size * 8! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/25/2022 16:54:29'!
isLowercase
	"Answer whether the receiver is a letter."
	"
	(UnicodeCodePoint codePoint: 16r22C7) isLowercase
	(UnicodeCodePoint codePoint: 16r042F) isLowercase
	(UnicodeCodePoint codePoint: 16r0431) isLowercase
	"
	| i |
	self asCharacter ifNotNil: [ :ch | ^ch isLowercase ].
	i _ value + 1.
	LowercaseLettersTable bitSize < i ifTrue: [ ^false ].
	^LowercaseLettersTable bitBooleanAt: i.! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/25/2022 16:54:09'!
isUppercase
	"Answer whether the receiver is an uppercase letter."
	"
	(UnicodeCodePoint codePoint: 16r22C7) isUppercase
	(UnicodeCodePoint codePoint: 16r042F) isUppercase
	(UnicodeCodePoint codePoint: 16r0431) isUppercase
	"
	| i |
	self asCharacter ifNotNil: [ :ch | ^ch isUppercase ].
	i _ value + 1.
	UppercaseLettersTable bitSize < i ifTrue: [ ^false ].
	^UppercaseLettersTable bitBooleanAt: i.! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/25/2022 16:54:52'!
isValidInBinarySelectors
	"
	(UnicodeCodePoint codePoint: 16r22C7) isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r042F) isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r0431) isValidInBinarySelectors
	"
	| i |
	(Character codePoint: value) ifNotNil: [ :ch | ^ch isValidInBinarySelectors ].
	i _ value + 1.
	BinarySelectorsTable bitSize < i ifTrue: [ ^false ].
	^BinarySelectorsTable bitBooleanAt: i.! !


!UnicodeCodePoint class methodsFor: 'class initialization' stamp: 'jmv 6/25/2022 16:57:06'!
initialize
	"https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
	http://www.unicode.org/L2/L1999/UnicodeData.html
	UnicodeCodePoint initialize
	"

	| fields codePoint codePointName generalCategory |
	UppercaseLettersTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	LowercaseLettersTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	BinarySelectorsTable _ ByteArray newBits: 16r1FFFF. "Basic Multilingual Plane 	+ Supplementary Multilingual Plane"
	(DirectoryEntry smalltalkImageDirectory // 'UnicodeData.txt') readStreamDo: [ :strm |
		[strm atEnd] whileFalse: [
			fields _ strm nextLine substringsSeparatedBy: $;.
			(#(4 5 6) includes: fields first size) ifFalse: [ fields first print. self halt ].
			codePoint _ ('16r', fields first) asNumber.
			codePointName _ fields at: 2.
			generalCategory _ fields at: 3.
			codePoint <= 16r1FFFF ifTrue: [
				(Character codePoint: codePoint)
					ifNotNil: [ :char |	"Char definition trumps Unicode definition"
						char isUppercase ifTrue: [ UppercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
						char isLowercase ifTrue: [ LowercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
						char isValidInBinarySelectors ifTrue: [ BinarySelectorsTable bitBooleanAt: codePoint+1 put: true ].
						]
					ifNil: [
						generalCategory = 'Lu' ifTrue: [ UppercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
						generalCategory = 'Ll' ifTrue: [ LowercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
						({ 'Po'. 'Sm'. 'Sc'. 'So' } includes: generalCategory) ifTrue: [ BinarySelectorsTable bitBooleanAt: codePoint+1 put: true ]]].
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

UnicodeCodePoint initialize!
