'From Cuis 6.0 [latest update: #5308] on 23 June 2022 at 5:06:59 pm'!
!methodRemoval: Utf8String #doesNotUnderstand: stamp: 'jmv 6/23/2022 17:05:06'!
Utf8String removeSelector: #doesNotUnderstand:!
!methodRemoval: Utf8String #is: stamp: 'jmv 6/23/2022 17:06:37'!
Utf8String removeSelector: #is:!
!methodRemoval: Utf32String #doesNotUnderstand: stamp: 'jmv 6/23/2022 17:05:04'!
Utf32String removeSelector: #doesNotUnderstand:!
!methodRemoval: Utf32String #is: stamp: 'jmv 6/23/2022 17:06:19'!
Utf32String removeSelector: #is:!
!methodRemoval: UnicodeCodePoint #doesNotUnderstand: stamp: 'jmv 6/23/2022 17:05:02'!
UnicodeCodePoint removeSelector: #doesNotUnderstand:!
!methodRemoval: UnicodeCodePoint #is: stamp: 'jmv 6/23/2022 17:05:39'!
UnicodeCodePoint removeSelector: #is:!

!UnicodeCodePoint reorganize!
('accessing' asciiValue codePoint digitValue numericValue)
('testing' canBeGlobalVarInitial isAccented isAlphaNumeric isCharacter isDigit isLetter isLineSeparator isLowercase isSeparator isUnaccented isUnicodeCodePoint isUppercase isValidInBinarySelectors isValidInIdentifiers isValidStartOfIdentifiers isVowel iso8859s15Code tokenish)
('copying' shallowCopy)
('comparing' < <= = > >= hash)
('converting' asCharacter asLowercase asString asSymbol asText asUnaccented asUppercase to: withDiacriticalMark:)
('printing' hex isLiteral printOn: storeOn:)
('private' privateValue:)
('fileman-testing' isDriveLetter isDriveSeparator isPathSeparator)
!


!Utf32String reorganize!
('accessing' at: isLastLineSeparator isLastSeparator size words words:)
('copying' , appendToString: copyFrom:to: copyReplaceFrom:to:with:)
('comparing' = hash sameAs:)
('converting' asUtf32String asUtf32Words asUtf8Bytes)
('testing' isAscii isEmpty isUtf32String)
('paragraph support' encompassLine: encompassParagraph:)
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
('initialization' initialize)
!

