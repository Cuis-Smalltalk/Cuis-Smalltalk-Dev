'From Cuis 6.0 [latest update: #5238] on 8 June 2022 at 12:04:50 pm'!

!String reorganize!
('accessing' at: at:put: byteAt: byteAt:put: byteSize findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findLastOccurrenceOfString:startingAt: findString: findString:in:startingAt: findString:startingAt: findString:startingAt:caseSensitive: findTokens: findTokens:keep: includesSubString: includesSubstring:caseSensitive: indexOf: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf: indexOfAnyOf:startingAt:ifAbsent: indexOfSubCollection:startingAt:ifAbsent: isLastLineSeparator isLastSeparator lineCount lineIndicesDo: lineNumber: lines linesDo: size skipAnySubStr:startingAt: skipDelimiters:startingAt:)
('comparing' < <= = > >= alike: beginsWith: caseInsensitiveLessOrEqual: caseSensitiveLessOrEqual: commonPrefixWith: compare: compare:caseSensitive: crc16 endsWith: hash is:sameAs: is:substringAt: match: sameAs: startingAt:match:startingAt:)
('copying' , append: appendToString: copyReplaceTokens:with: copyReplacing: copyReplacing:into: copyReplacing:with: copyReplacing:with:into: flattenTo: join: newFromAfterAppending:into:keepingFrom:skipping: padded:to:with:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend: afterBlanksEndsWith: asByteString asCamelCase asDate asFileName asHex asIdentifier: asLowercase asMonth asNumber asSmalltalkComment asUnHtml asUnaccented asUppercase asUtf32Words asUtf32Words: asUtf8Bytes asUtf8Bytes: asValidInstanceVariableName asValidSelector asWeek asYear base64Decoded capitalized correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: findPositiveInteger findSelector firstNonSeparator fixLineEndings initialIntegerOrNil keywords lastNonSeparator prefixAndSuffix: romanNumber squeezedTo: substrings surroundedBySingleQuotes thatCanHoldCodePoints translateFrom:to:table: translateToLowercase translateToUppercase translateWith: truncateTo: truncateWithElipsisTo: uncapitalized withBlanksCondensed withBlanksTrimmed withCuisLineEndings withDescriptiveLineEndings withLineEndings: withNewLines withoutEnclosing: withoutLeadingBlanks withoutPrefix: withoutSeparators withoutSuffix: withoutTrailingBlanks)
('enumerating' collect:)
('displaying' displayAt: displayOn: displayOn:at: displayOn:at:textColor: displayProgressAt:from:to:during:)
('printing' isLiteral)
('private' correctAgainstEnumerator:continuedFrom: evaluateExpression:parameters: getEnclosedExpressionFrom: replaceFrom:to:with:startingAt:)
('system primitives' compare:with: compare:with:collated: findSubstring:in:startingAt:matchTable: numArgs)
('testing' canHoldCodePoints hasContentsInExplorer is: isAlphaNumeric isByteString isEmpty isRemote)
('paragraph support' encompassLine: encompassParagraph: endOfParagraphBefore: indentationIfBlank:)
('arithmetic' * + - / // \\)
('formatting' format:)
('uCompletion' separateKeywords)
('fileman-converting' asAbsolutePathName asDirectoryEntry asDriveName asFileEntry asPathComponents asPathTokens withoutWindowsDriveName)
('fileman-testing' beginsWithPathSeparator beginsWithWindowsDriveName isAbsolutePathName isDriveName isPathSeparator isRelativeMark isRelativePathName)
('fileman-actions' fileContents fileContents:)
('fileman-accessing' indexOfFirstPathSeparator indexOfLastPathSeparator pathAndLocalName upToFirstPathSeparator upToLastPathSeparator)
('text conversion emphasis' bold italic pointSize: struck sub super under)
('text conversion attributes' centered justified leftFlush rightFlush)
('text conversion color' black blue cyan gray green magenta red yellow)
('grammar' article asPlural exceptionalPlural isUninflictedNoun lowercasePlural pluralize: withArticle)
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
!


!Utf8String reorganize!
('accessing' at: bytes bytes: isLastLineSeparator isLastSeparator size)
('copying' , appendToString: copyFrom:to: copyReplaceFrom:to:with:)
('comparing' < <= = > >= basicHash computeHash hash)
('converting' asUtf32Words asUtf8Bytes asUtf8String)
('enumerating' do: from:to:do: from:to:doWithUtf8Bytes:)
('testing' is: isAscii isEmpty isUtf8String)
('paragraph support' encompassLine: encompassParagraph:)
('character scanner support' scanCharactersFrom:to:with:rightX:stopConditions:)
('private' beAscii boundsCheck: byteIndexAt: codePointIndexAt: computeCountAndIndex computeCountAndIndexFull computeCountNoIndex encodedByteCountAt: stride)
('as yet unclassified' doesNotUnderstand:)
('initialization' initialize)
!

