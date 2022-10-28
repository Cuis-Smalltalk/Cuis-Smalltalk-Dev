'From Cuis 6.0 [latest update: #5538] on 28 October 2022 at 4:55:26 pm'!

!Character reorganize!
('accessing' asciiValue codePoint digitValue immediateNumericValue iso8859s15Code nonImmediateNumericValue numericValue)
('comparing' < <= = > >= hash identityHash)
('testing' canBeGlobalVarInitial is: isAccented isAlphaNumeric isCharacter isDigit isLetter isLineSeparator isLowercase isSeparator isSpecial isUnaccented isUppercase isValidInBinarySelectors isValidInIdentifiers isValidStartOfIdentifiers isVowel tokenish)
('copying' shallowCopy)
('printing' hex isLiteral printOn: storeOn:)
('converting' asCharacter asLowercase asString asSymbol asText asUnaccented asUnicodeCodePoint asUppercase to: withDiacriticalMark:)
('object fileIn' comeFullyUpOnReload:)
('fileman-testing' isDriveLetter isDriveSeparator isPathSeparator)
!


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


!AbstractFont class reorganize!
('instance accessing' fromUser fromUser:)
('constants' boldCode italicCode normalCode struckThroughCode subscriptCode superscriptCode underlinedCode)
!

