'From Cuis 6.0 [latest update: #5315] on 24 June 2022 at 5:29:36 pm'!
!classDefinition: #UnicodeCodePoint category: #'Kernel-Text'!
Object subclass: #UnicodeCodePoint
	instanceVariableNames: 'value '
	classVariableNames: 'BinarySelectorsTable LowercaseLettersTable UppercaseLettersTable '
	poolDictionaries: ''
	category: 'Kernel-Text'!

!UnicodeCodePoint class methodsFor: 'class initialization' stamp: 'jmv 6/24/2022 15:43:16'!
initialize
	"https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
	http://www.unicode.org/L2/L1999/UnicodeData.html
	UnicodeCodePoint initialize
	"

	| fields codePoint codePointName generalCategory |
	UppercaseLettersTable _ ByteArray newBits: 16r10FFFF.
	LowercaseLettersTable _ ByteArray newBits: 16r10FFFF.
	BinarySelectorsTable _ ByteArray newBits: 16r10FFFF.
	(DirectoryEntry smalltalkImageDirectory // 'UnicodeData.txt') readStreamDo: [ :strm |
		[strm atEnd] whileFalse: [
			fields _ strm nextLine substringsSeparatedBy: $;.
			(#(4 5 6) includes: fields first size) ifFalse: [ fields first print. self halt ].
			codePoint _ ('16r', fields first) asNumber.
			codePointName _ fields at: 2.
			generalCategory _ fields at: 3.
			(Character codePoint: codePoint)
				ifNotNil: [ :char |	"Char definition trumps Unicode definition"
					char isUppercase ifTrue: [ UppercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
					char isLowercase ifTrue: [ LowercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
					char isValidInBinarySelectors ifTrue: [ BinarySelectorsTable bitBooleanAt: codePoint+1 put: true ].
					]
				ifNil: [
					generalCategory = 'Lu' ifTrue: [ UppercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
					generalCategory = 'Ll' ifTrue: [ LowercaseLettersTable bitBooleanAt: codePoint+1 put: true ].
					({ 'Po'. 'Sm'. 'Sc'. 'So' } includes: generalCategory) ifTrue: [ BinarySelectorsTable bitBooleanAt: codePoint+1 put: true ]].
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
			]].
! !


!CharacterSequence methodsFor: 'accessing' stamp: 'jmv 6/24/2022 13:57:36'!
substringsSeparatedBy: delimiterCharacter
	"Find substrings delimited by aCharacter.
	Answer may include empty strings
	'0063;LATIN SMALL LETTER C;Ll;0;L;;;;;N;;;0043;;0043' substringsSeparatedBy: $;
	"

	| char in out |
	^Array streamContents: [ :answer |
		in _ self readStream.
		out _ self species writeStream.
		[ in atEnd ] whileFalse: [
			char _ in next.
			char = delimiterCharacter
				ifFalse: [ out nextPut: char ]
				ifTrue: [
					answer nextPut: out contents.
					out _ self species writeStream ]].
		answer nextPut: out contents.
		out _ nil ]! !


!Character methodsFor: 'testing' stamp: 'jmv 6/24/2022 15:16:31'!
isValidInBinarySelectors
	"Can be part of a binary selector?
		$< isValidInBinarySelectors
		$| isValidInBinarySelectors
		$^ isValidInBinarySelectors
		$: isValidInBinarySelectors
	"
	| v |
	v _ self numericValue.
	v = 0 ifTrue: [ ^false ].
	^#(verticalBar upArrow xColon xBinary) statePointsTo: (Scanner typeTable at: v)! !


!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/24/2022 15:26:02'!
isLetter
	"Answer whether the receiver is a letter."
	"
	(UnicodeCodePoint codePoint: 16r22C7) isLetter
	(UnicodeCodePoint codePoint: 16r042F) isLetter
	(UnicodeCodePoint codePoint: 16r0431) isLetter
	"
	^self isLowercase or: [ self isUppercase ]! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/24/2022 15:25:44'!
isLowercase
	"Answer whether the receiver is a letter."
	"
	(UnicodeCodePoint codePoint: 16r22C7) isLowercase
	(UnicodeCodePoint codePoint: 16r042F) isLowercase
	(UnicodeCodePoint codePoint: 16r0431) isLowercase
	"
	self asCharacter ifNotNil: [ :ch | ^ch isLowercase ].
	^LowercaseLettersTable bitBooleanAt: value + 1! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/24/2022 15:25:29'!
isUppercase
	"Answer whether the receiver is an uppercase letter."
	"
	(UnicodeCodePoint codePoint: 16r22C7) isUppercase
	(UnicodeCodePoint codePoint: 16r042F) isUppercase
	(UnicodeCodePoint codePoint: 16r0431) isUppercase
	"
	self asCharacter ifNotNil: [ :ch | ^ch isUppercase ].
	^UppercaseLettersTable bitBooleanAt: value + 1! !

!UnicodeCodePoint methodsFor: 'testing' stamp: 'jmv 6/24/2022 15:25:09'!
isValidInBinarySelectors
	"
	(UnicodeCodePoint codePoint: 16r22C7) isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r042F) isValidInBinarySelectors
	(UnicodeCodePoint codePoint: 16r0431) isValidInBinarySelectors
	"
	(Character codePoint: value) ifNotNil: [ :ch | ^ch isValidInBinarySelectors ].
	^BinarySelectorsTable bitBooleanAt: value + 1! !

!classDefinition: #UnicodeCodePoint category: #'Kernel-Text'!
Object subclass: #UnicodeCodePoint
	instanceVariableNames: 'value'
	classVariableNames: 'BinarySelectorsTable LowercaseLettersTable UppercaseLettersTable'
	poolDictionaries: ''
	category: 'Kernel-Text'!
UnicodeCodePoint initialize!
