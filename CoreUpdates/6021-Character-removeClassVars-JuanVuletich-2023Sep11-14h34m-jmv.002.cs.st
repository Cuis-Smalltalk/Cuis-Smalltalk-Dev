'From Cuis 6.0 [latest update: #6018] on 11 September 2023 at 2:53:25 pm'!
!classDefinition: #Character category: #'Kernel-Text'!
Object immediateSubclass: #Character
	instanceVariableNames: 'value '
	classVariableNames: 'CharacterTable UnaccentedTable UnicodeCodePoints LowercaseMappingTable UppercaseMappingTable IrregularIso8859s15Codes '
	poolDictionaries: ''
	category: 'Kernel-Text'!

!Character class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:51:11'!
basicIrregularIso8859s15Codes
	
	| unicodeCodePoints irregularIso8859s15Codes |
	unicodeCodePoints := Character unicodeCodePoints.
	irregularIso8859s15Codes := Dictionary new.
	0 to: 255 do: [ :iso |
		(unicodeCodePoints at: iso+1) = iso ifFalse: [
			irregularIso8859s15Codes at: (unicodeCodePoints at: iso+1) put: iso ]].
	^irregularIso8859s15Codes! !

!Character class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:40:24'!
basicLookupTables
	| lowercaseMappingTable uppercaseMappingTable unaccentedTable |
	lowercaseMappingTable := Array new: 256.
	uppercaseMappingTable := Array new: 256.
	unaccentedTable := ByteArray new: 256.
	0
		to: 255
		do: [ :idx | | char |
			"Default to an identity mapping with a false truth mapping"
			char := self numericValue: idx.
			lowercaseMappingTable
				at: idx + 1 put: char.
			uppercaseMappingTable
				at: idx + 1 put: char.
			unaccentedTable at: idx + 1 put: idx].
	"Now override as needed"
	Character uppercaseLowercaseAndUnaccentedLetters do: [ :group | | uppercase lowercase |
		group size > 1
			ifTrue: [ | lowercaseChar uppercaseChar |
				uppercase := group first numericValue.
				lowercase := group second numericValue.
				lowercaseChar := self numericValue: lowercase.
				uppercaseChar := self numericValue: uppercase.
				lowercaseMappingTable
					at: lowercase + 1 put: lowercaseChar;
					at: uppercase + 1 put: lowercaseChar.
				uppercaseMappingTable
					at: lowercase + 1 put: uppercaseChar;
					at: uppercase + 1 put: uppercaseChar.
				group size > 2
					ifTrue: [|unaccentedUppercase unaccentedLowercase|
						unaccentedUppercase := group third numericValue.
						unaccentedLowercase := group fourth numericValue.
						unaccentedTable
							at: uppercase+1 put: unaccentedUppercase;
							at: lowercase+1 put: unaccentedLowercase]]
			ifFalse: [ | lowercaseChar |
				lowercase := group first numericValue.
				lowercaseChar := self numericValue: lowercase.
				lowercaseMappingTable
					at: lowercase + 1 put: lowercaseChar.
				uppercaseMappingTable
					at: lowercase + 1 put: lowercaseChar ]].
	^{ lowercaseMappingTable. uppercaseMappingTable. unaccentedTable}! !

!Character class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:48:46'!
basicUnicodeCodePoints
	"Initialize the table of Unicode code points"
	| unicodeCodePoints |
	unicodeCodePoints := Array new: 256.
	0 to: 255 do: [ :code |
		unicodeCodePoints at: code + 1 put: code ].
	
	"The following codes are different in ISO 8859-15 from those in ISO 8859-1,
	so the character code is not equal to the Unicode code point"
	unicodeCodePoints at: 16rA4+1 put: 16r20AC.		"euro sign"
	unicodeCodePoints at: 16rA6+1 put: 16r160.		"latin capital letter S with caron"
	unicodeCodePoints at: 16rA8+1 put: 16r161.		"latin small letter s with caron"
	unicodeCodePoints at: 16rB4+1 put: 16r17D.		"latin capital letter Z with caron"
	unicodeCodePoints at: 16rB8+1 put: 16r17E.		"latin small letter z with caron"
	unicodeCodePoints at: 16rBC+1 put: 16r152.		"latin capital ligature OE"
	unicodeCodePoints at: 16rBD+1 put: 16r153.		"latin small ligature oe"
	unicodeCodePoints at: 16rBE+1 put: 16r178.		"latin capital letter Y with diaeresis"

	"Mathematical symbols (see Character>>#namedCharactersTable)"
	unicodeCodePoints at: 16r80+1 put: 16r221E.		"infinity"
	unicodeCodePoints at: 16r81+1 put: 16r2135.		"aleph"
	unicodeCodePoints at: 16r82+1 put: 16r2115.		"blackboard bold N"
	unicodeCodePoints at: 16r83+1 put: 16r2124.		"blackboard bold Z"
	unicodeCodePoints at: 16r84+1 put: 16r211A.		"blackboard bold Q"
	unicodeCodePoints at: 16r85+1 put: 16r211D.		"blackboard bold R"
	unicodeCodePoints at: 16r86+1 put: 16r2102.		"blackboard bold C"
	unicodeCodePoints at: 16r87+1 put: 16r2119.		"blackboard bold P"
	unicodeCodePoints at: 16r88+1 put: 16r03B1.		"alpha"
	unicodeCodePoints at: 16r89+1 put: 16r03B2.		"beta"
	unicodeCodePoints at: 16r8A+1 put: 16r03B3.		"gamma"
	unicodeCodePoints at: 16r8B+1 put: 16r03B4.		"delta"
	unicodeCodePoints at: 16r8C+1 put: 16r03B5.		"epsilon"
	unicodeCodePoints at: 16r8D+1 put: 16r03BB.		"lambda"
	unicodeCodePoints at: 16r8E+1 put: 16r03C0.		"pi"
	unicodeCodePoints at: 16r8F+1 put: 16r03B6.		"zeta"
	unicodeCodePoints at: 16r90+1 put: 16r2295.		"oplus"
	unicodeCodePoints at: 16r91+1 put: 16r2297.		"otimes"
	unicodeCodePoints at: 16r92+1 put: 16r2218.		"circ"
	unicodeCodePoints at: 16r93+1 put: 16r2293.		"prod"
	unicodeCodePoints at: 16r94+1 put: 16r2294.		"coprod"
	unicodeCodePoints at: 16r95+1 put: 16r22A5.		"perp"
	unicodeCodePoints at: 16r96+1 put: 16r0394.		"Delta"
	unicodeCodePoints at: 16r97+1 put: 16r0393.		"Gamma"
	unicodeCodePoints at: 16r98+1 put: 16r03C9.		"omega"
	unicodeCodePoints at: 16r99+1 put: 16r21A0.		"SURJECTIVE ARROW"
	unicodeCodePoints at: 16r9A+1 put: 16r21A3.		"INJECTIVE ARROW"
	unicodeCodePoints at: 16r9B+1 put: 16r2194.		"BIJECTIVE ARROW"
	unicodeCodePoints at: 16r9C+1 put: 16r21AA.		"INCLUSION ARROW"
	unicodeCodePoints at: 16r9D+1 put: 16r21A6.		"MAPPING ARROW"
	unicodeCodePoints at: 16r9E+1 put: 16r2202.		"partial differential"
	unicodeCodePoints at: 16r9F+1 put: 16r221A.		"sqrt"
	^unicodeCodePoints! !

!Character class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:41:06'!
lowercaseMappingTable
	^`Character basicLookupTables at: 1`! !

!Character class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:41:12'!
uppercaseMappingTable
	^`Character basicLookupTables at: 2`! !


!Character methodsFor: 'converting' stamp: 'jmv 9/11/2023 14:42:59'!
asLowercase
	^ Character lowercaseMappingTable at: self numericValue + 1.! !

!Character methodsFor: 'converting' stamp: 'jmv 9/11/2023 14:43:17'!
asUppercase
	"If the receiver is lowercase, answer its matching uppercase Character."
	^ Character uppercaseMappingTable at: self numericValue + 1.! !


!Character class methodsFor: 'class initialization' stamp: 'jmv 9/11/2023 14:53:15'!
initialize
	"
	Character initialize
	"
	Character isImmediateClass ifTrue: [
		CharacterTable := nil ].! !

!Character class methodsFor: 'fileIn/Out' stamp: 'jmv 9/11/2023 14:53:02'!
definition
	"Special class definition for Character, SmallInteger and SmallFloat64
	Required to have the same definition both for preSpur and Spur, and to be compatible with both."
	^
'Object immediateSubclass: #Character
	instanceVariableNames: ''value''
	classVariableNames: ''CharacterTable''
	poolDictionaries: ''''
	category: ''', self category, ''''
! !

!Character class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:51:48'!
irregularIso8859s15Codes
	^`Character basicIrregularIso8859s15Codes`! !

!Character class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:41:18'!
unaccentedTable
	^`Character basicLookupTables at: 3`! !

!Character class methodsFor: 'static state' stamp: 'jmv 9/11/2023 14:49:18'!
unicodeCodePoints
	^`Character basicUnicodeCodePoints`! !

!methodRemoval: Character class #releaseClassState stamp: 'jmv 9/11/2023 14:52:15'!
Character class removeSelector: #releaseClassState!
!methodRemoval: Character class #initializeUnicodeCodePoints stamp: 'jmv 9/11/2023 14:52:32'!
Character class removeSelector: #initializeUnicodeCodePoints!
!methodRemoval: Character class #initializeLookupTables stamp: 'jmv 9/11/2023 14:42:13'!
Character class removeSelector: #initializeLookupTables!
!classDefinition: #Character category: #'Kernel-Text'!
Object immediateSubclass: #Character
	instanceVariableNames: 'value'
	classVariableNames: 'CharacterTable'
	poolDictionaries: ''
	category: 'Kernel-Text'!
Character initialize!

!Character class reorganize!
('class initialization' accentedLetters blackboardLetters greekLowercaseLetters greekUppercaseLetters initialize uppercaseLowercaseAndUnaccentedLetters vowels)
('instance creation' asciiValue: codePoint: digitValue: immediateNumericValue: new nonImmediateNumericValue: numericValue: separators)
('accessing untypeable characters' backspace cr crCharacter delete end enter escape home insert lf lfCharacter newLineCharacter newPage null pageDown pageUp shortUnderscore smalltalkDownArrow smalltalkLeftArrow smalltalkRightArrow smalltalkUpArrow space tab)
('constants')
('converting' evaluate:withUtf8BytesOfUnicodeCodePoint: evaluateOnce:withUtf8BytesOfUnicodeCodePoint: iso8859s15CodeForUnicodeCodePoint: macRomanToLatin1: utf8BytesAndCodePointAt:in:into:into: utf8BytesAndCodePointFor:byte2:byte3:byte4:into:into: utf8BytesAndCodePointForIso8859s15:into:into: utf8BytesOfUnicodeCodePoint:)
('fileIn/Out' definition)
('static state' basicIrregularIso8859s15Codes basicLookupTables basicUnicodeCodePoints irregularIso8859s15Codes lowercaseMappingTable unaccentedTable unicodeCodePoints uppercaseMappingTable)
!

