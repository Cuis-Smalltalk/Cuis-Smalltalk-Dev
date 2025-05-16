'From Cuis7.3 [latest update: #7160] on 14 May 2025 at 3:44:23 pm'!

!Collection reorganize!
('[mk] accessing' size)
('[mk] enumerating' allSatisfy: anySatisfy: count: reject: select:)
('[mk] testing' includes: is: isSequenceable)
('[mk] comparing' = emptyCollectionHash hash)
('[mk] private' species)
('accessing' anyOne atRandom atRandom:)
('adapting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend:)
('adding' add: add:withOccurrences: addAll:)
('arithmetic' * + - / // \\ raisedTo:)
('converting' as:on:delimiter:last: asArray asBag asByteArray asCommaSeparated:on: asCommaStringAnd asDictionary asFloat32Array asFloat64Array asIdentitySet asIntegerArray asNewArray asOrderedCollection asPointArray asSet asSortedCollection asSortedCollection: asStringOn:delimiter:last: asWordArray)
('copying' , copyWith: copyWithout: copyWithoutAll: flatten flattenTo:)
('enumerating' associationsDo: associationsDo:separatedBy: collect: collect:andFold: collect:andFold:ifEmpty: collect:thenSelect: detect: detect:ifFound:ifNone: detect:ifNone: detectMax: detectMin: difference: do: do:separatedBy: do:without: fold: fold:ifEmpty: groupBy: groupBy:having: inject:into: intersection: noneSatisfy: reduce: reject:thenCollect: reject:thenDo: select:thenCollect: select:thenDo: union:)
('filter streaming' contents)
('math functions' abs arcCos arcSin arcTan ceiling cos degreeCos degreeSin exp floor ln log log2 negated reciprocal roundTo: rounded sign sin sqrt squared tan truncateTo: truncated)
('printing' printElementsOn: printNameOn: printOn: storeOn:)
('private' emptyCheck errorCollectionTooSmall errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed)
('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)
('testing' canHoldAnyCodePoint identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifNotEmpty: ifNotEmpty:ifEmpty: includesAllOf: includesAnyOf: includesSubstringAnywhere: isCollection isEmpty isEmptyOrNil notEmpty occurrencesOf:)
('sorting' sorted sorted:)
('statistics' average average: average:ifEmpty: max max: mean min min: product product: range sum sum: sum:ifEmpty:)
('inspecting' hasContentsInExplorer)
!


!SequenceableCollection reorganize!
('[mk] accessing' indexOf: indexOf:ifAbsent: indexOf:startingAt: indexOf:startingAt:ifAbsent: replaceFrom:to:with:)
('[mk] comparing' = hasEqualElements: hash hashQuick hashQuick:)
('[mk] converting' asByteArray)
('[mk] copying' , copyBlank: copyFrom:to: copyReplaceFrom:to:with: grownTo:)
('[mk] enumerating' do: select:)
('[mk] testing' includes: isSequenceable)
('[mk] stream creation' asStreamResult readStream thatCanBeModified writeStream)
('accessing' after: after:ifNone: allButFirst allButFirst: allButLast allButLast: antepenultimate antepenultimateIfAbsent: anyOne at:ifAbsent: atAll: atAll:put: atAll:putAll: atAllPut: atLast: atLast:ifAbsent: atLast:put: atPin: atRandom: atWrap: atWrap:put: before: before:ifNone: eighth fifth fillWith: first first: firstAvailable: fourth from:to:put: head: identityIndexOf: identityIndexOf:ifAbsent: indexOfMax indexOfSubCollection:startingAt: indexOfSubCollection:startingAt:ifAbsent: last last: lastAvailable: lastIfEmpty: lastIndexOf: lastIndexOf:ifAbsent: lastIndexOf:startingAt:endingAt:do: lastIndexOf:startingAt:endingAt:ifAbsent: lastIndexOf:startingAt:ifAbsent: middle ninth penultimate penultimateIfAbsent: quickIndexOf: replaceAll:with: replaceFrom:to:with:startingAt: second seventh sixth swap:with: tail: third)
('converting' @ asColorArray asNewArray concatenation printStringWithNewline readStreamFrom:to: reverse reversed)
('copying' copyAfter: copyAfterLast: copyFrom:count: copyReplaceAll:with: copyUpThrough: copyUpTo: copyUpToLast: copyWith: shuffleBy: shuffled shuffledBy:)
('enumerating' allButFirstDo: allButLastDo: asDigitsToPower:do: collect: collect:from:to: combinations:atATimeDo: do:separatedBy: do:without: findBinary: findBinary:do:ifNone: findBinaryIndex: findBinaryIndex:do:ifNone: findFirst: findFirst:startingAt: findFirstBasicAt:startingAt: findLast: from:to:do: groupsOf:atATimeDo: intervalOfSubCollection: keysAndValuesDo: pairsCollect: pairsDo: permutationsDo: polynomialEval: quickFindFirst: quickFindLast: replace: reverseDo: reverseWith:do: upTo: with:allSatisfy: with:anySatisfy: with:collect: with:do: with:do:separatedBy: with:reverseDo: with:with:collect: with:with:do: withIndexCollect: withIndexDo: withIndexDo:separatedBy: withNextDo: withPreviousCyclicDo: withPreviousDo:)
('inspecting' customizeExplorerContents explorerContents)
('private' asDigitsAt:in:do: combinationsAt:upTo:in:after:upTo:do: copyReplaceAll:with:asTokens: errorOutOfBounds permutationsStartingAt:do:)
('removing' remove:ifAbsent:)
('testing' beginsWith: endsWith: ifInBounds:ifNot: isInBounds:)
('gui' do:displayingProgress:)
('math functions' += -=)
('assertions' assertIsInBounds:)
('sorting' sorted:)
!


!ArrayedCollection class reorganize!
('[mk] instance creation' new new: numElements withAll: withAll:with:)
('instance creation' instancesMustBeOfSpecificSize new:from: new:withAll: newFrom: newFromStream: with: with:with: with:with:with: with:with:with:with: with:with:with:with:with: with:with:with:with:with:with: with:withAll:)
('stream creation' writeStream)
('aux' bytesPerBasicElement)
!


!ByteArray reorganize!
('[mk] access - Numeric Types' bitAt: bitAt:put: bitBooleanAt: bitBooleanAt:put: float32At:bigEndian: float32At:put:bigEndian: float64At:bigEndian: float64At:put:bigEndian: int16At:bigEndian: int16At:put:bigEndian: int32At:bigEndian: int32At:put:bigEndian: int64At:bigEndian: int64At:put:bigEndian: int8At:bigEndian: int8At:put:bigEndian: uint16At:bigEndian: uint16At:put:bigEndian: uint32At:bigEndian: uint32At:put:bigEndian: uint64At:bigEndian: uint64At:put:bigEndian: uint8At:bigEndian: uint8At:put:bigEndian:)
('[mk] accessing' indexOf:startingAt:ifAbsent:)
('[mk] converting' asByteArray asString)
('[mk] testing' is:)
('[mk] comparing' = hash)
('[mk] system primitives' replaceFrom:to:with:startingAt:)
('accessing' atAllPut: bitSize indexOfAnyIn:startingAt:ifAbsent:)
('converting' asHex asUtf8BytesOrByteStringOrByteArray base64Encoded hex)
('private' bitAt:bitPosition: bitAt:bitPosition:put: defaultElement)
('printing' printAsLiteralOn: printOn: storeOn:)
('testing' isLiteral)
('initialization' readHexFrom:)
!


!CharacterSequence reorganize!
('[mk] accessing' indexOf: string)
('[mk] comparing' = hash)
('[mk] printing' printOn: storeOn:)
('[mk] testing' isString)
('[mkSymbol] converting' asSymbol)
('accessing' findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findLastOccurrenceOfString:startingAt: findString: findString:startingAt:caseSensitive: findTokens: findTokens:keep: includesSubString: includesSubstring:caseSensitive: indexOfSubCollection:startingAt:ifAbsent: lineCount lineNumber: lines linesDo: skipAnySubStr:startingAt: skipDelimiters:startingAt: subStrings: substringsSeparatedBy: utf8BytesSize)
('adding' add:)
('copying' append: appendToText: copyReplaceTokens:with: copyReplacing: copyReplacing:into: copyReplacing:with: copyReplacing:with:into: copyReplacing:withAll: copyReplacing:withAll:into: flattenTo: join: newFromAfterAppending:into:keepingFrom:skipping: padded:to:with:)
('comparing' < <= > >= alike: beginsWith: commonPartWith:startAt:stopAt:applying: commonPrefixWith: endsWith: is:substringAt: match: startingAt:match:startingAt:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend: asByteString asCamelCase asDate asFileName asIdentifier: asLowercase asMonth asNumber asPlainString asSmalltalkComment asString asText asUnHtml asUnaccented asUnicodeString asUnicodeStringOrText asUppercase asUtf8Bytes asUtf8BytesOrByteString asUtf8BytesOrByteStringOrByteArray asValidInstanceVariableName asValidSelector asWeek asYear base64Decoded capitalized contractTo: correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: displayStringOrText findPositiveInteger findSelector firstNonSeparator fixLineEndings initialIntegerOrNil integerOrNilAt: keywords lastNonSeparator prefixAndSuffix: romanNumber squeezedTo: translated truncateTo: truncateWithElipsisTo: uncapitalized withBlanksCondensed withBlanksTrimmed withCuisLineEndings withDescriptiveLineEndings withDoubleColonsDoubled withDoubleColonsUndoubled withNewLines withoutEnclosing: withoutLeadingBlanks withoutPossiblePrefix: withoutPossibleSuffix: withoutPrefix: withoutSeparators withoutSuffix: withoutTrailingBlanks)
('enumerating' collect: collectInArray:)
('printing' print printAsLiteralOn:)
('testing' isAlphaNumeric isLineEndAt: isRemote isSeparators)
('user interface' edit editLabel:)
('fileman-converting' asDirectoryEntry asDriveName asFileEntry asFullDirectoryEntry asFullFileEntry asPathComponents asPathTokens withoutWindowsDriveName)
('fileman-testing' beginsWithPathSeparator beginsWithWindowsDriveName isAbsolutePathName isDriveName isPathSeparator isRelativeMark isRelativePathName)
('fileman-accessing' indexOfFirstPathSeparator indexOfLastPathSeparator pathAndLocalName upToFirstPathSeparator upToLastPathSeparator)
('arithmetic' * + - / // \\)
('text conversion emphasis' bold italic pointSize: struck sub super under)
('text conversion attributes' centered justified leftFlush rightFlush)
('text conversion color' black blue cyan gray green magenta red yellow)
('displaying' displayAt: displayOn: displayOn:at: displayOn:at:textColor: displayProgressAt:from:to:during:)
('private' correctAgainstEnumerator:continuedFrom: evaluateExpression:parameters: getEnclosedExpressionFrom:)
('formatting' format:)
('uCompletion' separateKeywords)
('paragraph support' endOfParagraphBefore: indentationIfBlank:)
('grammar' article asPlural exceptionalPlural isUninflictedNoun lowercasePlural pluralize: withArticle)
('fileman-actions' fileContents fileContents:)
('Unicode normalization' asNFC asNFD canonicalComposition canonicalDecomposition)
!


!Stream reorganize!
('[mk] reading' next)
('[mk] writing' nextPutAll: print:)
('[mk] accessing' binary)
('[mk] normalized access' nextFloat32BigEndian: nextFloat32Put:bigEndian: nextFloat64BigEndian: nextFloat64Put:bigEndian: nextInt16BigEndian: nextInt16Put:bigEndian: nextInt32BigEndian: nextInt32Put:bigEndian: nextInt64BigEndian: nextInt64Put:bigEndian: nextUint16BigEndian: nextUint16Put:bigEndian: nextUint32BigEndian: nextUint32Put:bigEndian: nextUint64BigEndian: nextUint64Put:bigEndian:)
('[mk] nonhomogeneous accessing' nextString)
('[mk] testing' is:)
('reading' basicNext contents next: readOnly)
('writing' flush next:put: nextPut: nextPut:when: nextPutAll:asCommaSeparated: nextPutAll:when:)
('accessing' localName nextMatchAll: openReadOnly)
('testing' atEnd closed isReadOnly isWriteOnly nextUint32WordsPutAll:)
('enumerating' do:)
('printing' print:fractionDigits: print:when: printOn:)
('FileStream compatibility' close)
('nonhomogeneous accessing' nextNumber nextStringPut: nextUnicodeString nextUnicodeStringPut:)
!


!PositionableStream reorganize!
('[mk] accessing' next: next:into:startingAt: nextInto: originalContents)
('[mk] positioning' position position: reset)
('[mk] private' on:)
('[mk] mode' useBytes)
('reading' back contents contentsOfEntireFile crLfNextLine next:into: nextAvailable: nextInto:startingAt: nextKeyword nextLine nextWordsInto: peek peekBack peekFor: untilAnySatisfying: upTo: upTo:delimiterIsTerminator: upToAll: upToAny:)
('writing' next:putAll: next:putAll:startingAt:)
('testing' atEnd atStart isBinary isEmpty isFileStream isText notEmpty)
('positioning' match: matchStart: padTo:put: padToNextLongPut: resetContents setToEnd skip skip: skipBack skipTo:)
('fileIn/Out' backChunk checkForPreamble: compileNextChunk compileNextChunkHandlingExceptions compileNextChunkWhenDoesNotStartWithExclamationMark compileNextChunkWhenStartsWithExclamationMark evaluate:printingErrorWith: fileIn fileInAnnouncing: fileInInformingTo: nextChunk skipSeparators unCommand)
('private' collectionSpecies positionError setFrom:to:)
('filein/out' copyPreamble:oldPreamble:)
('gui' untilEnd:displayingProgress:)
!


!ReadStream reorganize!
('[mk] reading' next next: next:into:startingAt:)
('[mk] writing' nextPutAll:)
('reading' readInto:startingAt:count: readStream size upTo:delimiterIsTerminator: upToEnd)
('writing' next:putAll:startingAt: nextPut:)
('accessing')
('private' on:from:to:)
('file stream compatibility' localName)
('testing' isReadOnly)
!


!WriteStream reorganize!
('[mk] reading' contents next next:)
('[mk] writing' next:into:startingAt: nextPut: nextPutAll: space)
('[mk] positioning' position: reset)
('[mk] private' isCompatibleWithContents: on:)
('accessing' size)
('reading' nextWordsInto: peek upTo:delimiterIsTerminator:)
('writing' next:putAll:startingAt: nextPutAllString:withAttributes:)
('positioning' resetToStart setToEnd)
('character writing' cr ensureASpace ensureNoSpace lf newLine newLineTab newLineTab: newLineTab:when: newLineWhen: nextPutKeyword:withArg: peekLast space: space:when: spaceWhen: tab tab: tab:when: tabWhen:)
('fileIn/Out' nextChunkPut: timeStamp)
('printing' store:)
('private' braceArray braceArray: growTo: pastEndPut: with: withAttribute:do: withAttributes:do:)
('testing' isEmpty isWriteOnly)
('services' padToEndIfCantTruncate)
!


!Number reorganize!
('[mk] arithmetic' // \\ abs negated)
('[mk] comparing' = hash)
('[mk] mathematical functions' raisedToInteger:)
('[mk] truncation and round off' ceiling floor)
('[mk] converting' adaptToInteger:andSend: asInteger)
('[mk] testing' is: isNumber isZero negative)
('[mk] intervals' to:do:)
('[mk] printing' printString)
('[mkFloat] converting' adaptToFloat:andSend:)
('arithmetic' * + - / ^ div: mod: quo: reciprocal rem:)
('comparing' %)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh cubed degreeCos degreeSin exp floorLog: interpolateTo:at: lg ln log log: log2 magnitude nthRoot: raisedTo: raisedToFraction: raisedToNegativeInteger: sign: sin sinh sqrt squared tan tanh)
('truncation and round off' detentBy:atMultiplesOf:snap: fractionPart integerPart round4perMagnitudeOrder roundDownTo: roundTo: roundUpTo: rounded roundedHAFZ roundedHTE roundedToEven truncateTo: truncated)
('converting' @ adaptToCollection:andSend: adaptToFraction:andSend: adaptToPoint:andSend: asFloat asIntegerOrFloat asNumber asPoint asSmallAngleDegrees asSmallPositiveDegrees days degreesToRadians degreesToRadiansMinutes:seconds: hours inMinusPiToPiRange milliSeconds minutes moduloTwoPiAsFloat nanoSeconds radiansToDegrees seconds weeks withNegativeSign)
('testing' even ifNotZero: isDivisibleBy: isFinite isInfinite isNaN odd positive sign strictlyPositive)
('intervals' to: to:by: to:by:do: to:count: to:do:separatedBy: toSelfPlus:)
('printing' isOrAreStringWith: printOn: printOn:base: printOn:fractionDigits: printOn:integerDigits:fractionDigits: printOn:integerDigits:padWith:fractionDigits:positiveIndicator: printStringBase: printStringFractionDigits: storeOn: storeOn:base: storeStringBase: withBinaryUnitPrefixAndValue: withDecimalUnitPrefixAndValue:)
!


!Float class reorganize!
('[mkFloat] instance creation' signPart:mantissaPart:exponentPart:)
('[mkFloat] constants' fmin fminDenormalized maxExactInteger one pi precision)
('[mkFloat] class initialization' initClassCachedState initialize)
('[mkFloat] testing' denormalized)
('instance creation' basicNew basicNew: fromIEEE32Bit: new new: readFrom: signBit:mantissaBits:exponentBits:)
('constants' e emax emin epsilon fmax fminNormalized halfPi infinity ln10 ln2 nan negativeInfinity negativeZero radix twoPi zero)
('class initialization' releaseClassCachedState)
('testing' nativeWordOrdering)
!


!Integer reorganize!
('[mk] arithmetic' * + - //)
('[mk] comparing' < <= = > >= hash hashMultiply)
('[mk] truncation and round off' ceiling floor)
('[mk] converting' asInteger)
('[mk] bit manipulation' highBit highBitOfMagnitude)
('[mk] testing' is: isInteger)
('arithmetic' / alignedTo: crossSumBase: gcd: quo: reciprocalModulo: reciprocalModulo2: xgcd:)
('benchmarks' benchFib benchmark tinyBenchmarks)
('bit manipulation' << >> allMask: anyBitOfMagnitudeFrom:to: anyMask: bitAnd: bitAt: bitAt:put: bitClear: bitInvert bitInvert16 bitInvert32 bitOr: bitReverse: bitShift: bitShiftMagnitude: bitXor: lowBit noMask:)
('comparing')
('converting' adaptToFraction:andSend: asColorOfDepth: asFloat asFraction asHexDigit asIntegerOrFloat moduloTwoPiAsFloat)
('enumerating' timesRepeat:)
('inspecting' customizeExplorerContents explorerContents hasContentsInExplorer)
('intervals' integersTo:count:)
('mathematical functions' factorial floatLog floorLog: ifMultipleOf2And5Do:otherwise: lcm: log log: log2 nthRoot: nthRootRounded: nthRootTruncated: productTo: raisedTo:modulo: raisedToFraction: raisedToInteger:modulo: sqrt sqrtFloor sqrtRounded sumTo: take:)
('printing' hex hex8 isLiteral numberOfDigitsInBase: printAsLiteralOn: printOn:base:length:padded: printOn:base:nDigits: printOn:fractionDigits: printOn:length:zeroPadded: printOn:thousandSeparator:includePlusSign: printStringAsBytes printStringAsBytesDecimal printStringBase:length:padded: printStringHex printStringRadix: printStringRoman printStringWithCommas printStringWords storeOn:base: storeStringHex)
('system primitives' lastDigit replaceFrom:to:with:startingAt:)
('testing' even isPowerOfTwo isPrime isProbablyPrime nextPrime)
('truncation and round off' atRandom atRandom: normalize roundedHAFZ roundedHTE truncated)
('private' copyto: digitAdd: digitCompare: digitDiv:neg: digitLogic:op:length: digitLshift: digitMultiply:neg: digitRshift:bytes:lookfirst: digitSubtract: growby: growto: isProbablyPrimeWithK:andQ: montgomeryDigitBase montgomeryDigitLength montgomeryDigitMax montgomeryNumberOfDigits montgomeryRaisedTo:times:modulo:mInvModB: montgomeryTimes:modulo:mInvModB: print:on:prefix:length:padded: romanDigits:for:on: slidingLeftRightRaisedTo:modulo:)
!


!SmallInteger reorganize!
('[mk] arithmetic' * + - // \\ quo:)
('[mk] comparing' < <= = > >= ~= hashMultiply identityHash)
('[mk] bit manipulation' bitAnd: bitOr: bitShift: bitXor: lowBit)
('[mk] testing' even)
('[mk] printing' decimalDigitLength printString)
('[mk] system primitives' nextInstance)
('[mkObjectCopying] copying' shallowCopy)
('[mkFloat] converting' asFloat)
('arithmetic' / gcd:)
('bit manipulation' byteReversed)
('testing' isLarge odd)
('printing' numberOfDigitsInBase: printOn:base: printOn:base:length:padded: printOn:base:nDigits: printStringBase: printStringBase:nDigits: threeDigitName)
('system primitives' digitAt: digitAt:put: digitLength nextObject)
('mathematical functions' sqrt)
!


!Character reorganize!
('[mk] accesing' codePoint)
('[mk] comparing' hash identityHash)
('[mk] testing' isCharacter isDigit isLetter isLowercase isUppercase)
('[mkObjectCopying] copying' shallowCopy)
('accessing' asciiValue digitValue)
('comparing' < <= > >= collatesBefore: collatingCategory)
('testing' canBeGlobalVarInitial isAccented isAlphaNumeric isAscii isDiacriticalMark isLineSeparator isNullCharacter isSeparator isSymbolLike isUnaccented isValidInBinarySelectors isValidInFilenames isValidInIdentifiers isValidStartOfIdentifiers isVowel shouldBePrintedAsLiteral shouldBePrintedAsLiteralVisiting: tokenish)
('printing' hex isLiteral printAsLiteralOn: printOn: storeOn:)
('converting' asLowercase asString asSymbol asText asUnaccented asUppercase to: withDiacriticalMark:)
('fileman-testing' isDriveLetter isDriveSeparator isPathSeparator)
!

