'From Cuis 5.0 of 7 November 2016 [latest update: #3083] on 25 May 2017 at 8:23:29 pm'!

!Character reorganize!
('accessing' asciiValue codePoint digitValue immediateNumericValue iso8859s15Code nonImmediateNumericValue numericValue)
('comparing' < <= = > >= hash identityHash)
('testing' canBeGlobalVarInitial isAccented isAlphaNumeric isDigit isLetter isLineSeparator isLowercase isSeparator isSpecial isUnaccented isUppercase isValidInIdentifiers isValidStartOfIdentifiers isVowel tokenish)
('copying' shallowCopy)
('printing' hex isLiteral printOn: storeOn:)
('converting' asCharacter asLowercase asString asSymbol asText asUnaccented asUppercase to: withDiacriticalMark:)
('object fileIn' comeFullyUpOnReload:)
('fileman-testing' isDriveLetter isDriveSeparator isPathSeparator)
!


!String reorganize!
('accessing' at: at:put: byteAt: byteAt:put: byteSize findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findLastOccurrenceOfString:startingAt: findString: findString:startingAt: findString:startingAt:caseSensitive: findTokens: findTokens:keep: includesSubString: includesSubstring:caseSensitive: indexOf: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: indexOfAnyOf:startingAt:ifAbsent: indexOfSubCollection:startingAt:ifAbsent: lineCount lineIndicesDo: lineNumber: lines linesDo: skipAnySubStr:startingAt: skipDelimiters:startingAt: string)
('comparing' < <= = > >= alike: beginsWith: caseInsensitiveLessOrEqual: caseSensitiveLessOrEqual: commonPartWith:startAt:stopAt:applying: commonPrefixWith: compare: compare:caseSensitive: crc16 endsWith: hash is:substringAt: match: sameAs: startingAt:match:startingAt:)
('copying' , append: appendToString: appendToText: copyReplaceTokens:with: join: padded:to:with:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend: asCamelCase asCharacter asDate asFileName asHex asIdentifier: asInteger asLegalSelector asLowercase asMonth asNumber asSmalltalkComment asString asSymbol asText asUnHtml asUnaccented asUppercase asUtf8 asUtf8: asWeek asYear base64Decoded capitalized contractTo: correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: displayStringOrText findSelector initialIntegerOrNil keywords prefixAndSuffix: romanNumber squeezedTo: substrings surroundedBySingleQuotes translateFrom:to:table: translateToLowercase translateToUppercase translateWith: truncateTo: truncateWithElipsisTo: withBlanksCondensed withBlanksTrimmed withCuisLineEndings withDescriptiveLineEndings withFirstCharacterDownshifted withLineEndings: withNewLines withoutEnclosing: withoutPrefix: withoutSuffix: withoutTrailingBlanks)
('displaying' displayAt: displayOn: displayOn:at: displayOn:at:textColor: displayProgressAt:from:to:during:)
('printing' isLiteral print printOn: storeOn:)
('private' correctAgainstEnumerator:continuedFrom: evaluateExpression:parameters: getEnclosedExpressionFrom: replaceFrom:to:with:startingAt:)
('system primitives' compare:with:collated: findSubstring:in:startingAt:matchTable: numArgs)
('testing' hasContentsInExplorer isAlphaNumeric isRemote isString)
('paragraph support' encompassLine: encompassParagraph: endOfParagraphBefore: indentationIfBlank:)
('arithmetic' * + - / // \\)
('user interface' edit editLabel:)
('formatting' format:)
('uCompletion' separateKeywords)
('fileman-converting' asAbsolutePathName asDirectoryEntry asDriveName asFileEntry asPathComponents asPathTokens withoutWindowsDriveName)
('fileman-testing' beginsWithPathSeparator beginsWithWindowsDriveName isAbsolutePathName isDriveName isPathSeparator isRelativeMark isRelativePathName)
('fileman-actions' fileContents fileContents:)
('fileman-accessing' indexOfFirstPathSeparator indexOfLastPathSeparator pathAndLocalName upToFirstPathSeparator upToLastPathSeparator)
('text conversion helpers' bold italic struck sub super under)
!


!String class reorganize!
('instance creation' addUnicodeCodePoint:to:hex: crString crlfString fromString: fromUtf8: fromUtf8:hex:trimLastNull: lfString newLineString readFrom: tab)
('initialization' initialize)
('primitives' findFirstInString:inSet:startingAt: indexOfByte:inString:startingAt: stringHash:initialHash: translate:from:to:table:)
('character collation' does:caseInsensitiveCollateBefore: does:collateAfter: does:collateBefore:)
('fileman-constants' pathSeparators)
!

