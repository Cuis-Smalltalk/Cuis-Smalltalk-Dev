'From Cuis7.7 [latest update: #7937] on 15 May 2026 at 9:28:08 am'!

!Collection reorganize!
('accessing' anyOne atRandom atRandom:)
('adapting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend:)
('adding' add: add:withOccurrences: addAll:)
('arithmetic' * + - / // \\ raisedTo:)
('comparing' = emptyCollectionHash hash)
('converting' as:on:delimiter:last: asArray asBag asByteArray asCommaSeparated:on: asCommaStringAnd asDictionary asFloat32Array asFloat64Array asIdentitySet asIntegerArray asNewArray asOrderedCollection asPointArray asSet asSortedCollection asSortedCollection: asStringOn:delimiter:last: asWordArray)
('copying' , copyWith: copyWithout: copyWithoutAll: flatten flattenTo:)
('enumerating' allSatisfy: anySatisfy: associationsDo: associationsDo:separatedBy: collect: collect:andFold: collect:andFold:ifEmpty: collect:thenSelect: count: difference: do: do:separatedBy: do:without: fold: fold:ifEmpty: groupBy: groupBy:having: inject:into: intersection: noneSatisfy: reduce: reject:thenCollect: reject:thenDo: select:thenCollect: select:thenDo: union:)
('find an element' detect: detect:ifFound:ifNone: detect:ifNone: detectMax: detectMin:)
('filter streaming' contents)
('inspecting' hasContentsInExplorer)
('math functions' abs arcCos arcSin arcTan ceiling cos degreeCos degreeSin exp floor ln log log2 negated reciprocal roundTo: rounded sign sin sqrt squared tan truncateTo: truncated)
('printing' printElementsOn: printNameOn: printOn: storeOn:)
('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)
('sorting' sorted sorted:)
('statistics' average average: average:ifEmpty: max max: mean min min: product product: range sum sum: sum:ifEmpty:)
('testing' canHoldAnyCodePoint identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifNotEmpty: ifNotEmpty:ifEmpty: includes: includesAllOf: includesAnyOf: includesSubstringAnywhere: is: isCollection isEmpty isEmptyOrNil isSequenceable notEmpty occurrencesOf:)
('private' emptyCheck errorCollectionTooSmall errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed species)
('[mk] accessing' size)
('[mk] enumerating' reject: select:)
!


!SequenceableCollection reorganize!
('accessing' after: after:ifNone: allButFirst allButFirst: allButLast allButLast: antepenultimate antepenultimateIfAbsent: anyOne at:ifAbsent: at:ifPresent: at:ifPresent:ifAbsent: atAll: atAll:put: atAll:putAll: atAllPut: atLast: atLast:ifAbsent: atLast:put: atPin: atRandom: atWrap: atWrap:put: before: before:ifNone: eighth fifth fillWith: first first: firstAvailable: fourth from:to:put: head: identityIndexOf: identityIndexOf:ifAbsent: indexOf:ifAbsent: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfMax indexOfSubCollection:startingAt: indexOfSubCollection:startingAt:ifAbsent: last last: lastAvailable: lastIfEmpty: lastIndexOf: lastIndexOf:ifAbsent: lastIndexOf:startingAt:endingAt:do: lastIndexOf:startingAt:endingAt:ifAbsent: lastIndexOf:startingAt:ifAbsent: middle ninth penultimate penultimateIfAbsent: quickIndexOf: replaceAll:with: replaceFrom:to:with:startingAt: second seventh sixth swap:with: tail: third)
('assertions' assertIsInBounds:)
('comparing' = hasEqualElements: hash hashQuick hashQuick: is:subcollectionAt:)
('converting' @ asByteArray asColorArray asNewArray concatenation printStringWithNewline readStreamFrom:to: reverse reversed)
('copying' , copyAfter: copyAfterLast: copyBlank: copyFrom:count: copyFrom:to: copyReplaceAll:with: copyReplaceFrom:to:with: copyUpThrough: copyUpTo: copyUpToLast: copyWith: grownTo: shuffleBy: shuffled shuffledBy:)
('enumerating' allButFirstDo: allButLastDo: asDigitsToPower:do: collect: collect:from:to: do: do:separatedBy: do:without: from:to:do: intervalOfSubCollection: keysAndValuesDo: polynomialEval: replace: reverseDo: reverseWith:do: select: upTo: with:allSatisfy: with:anySatisfy: with:collect: with:do: with:do:separatedBy: with:reverseDo: with:with:collect: with:with:do: withIndexCollect: withIndexDo: withIndexDo:separatedBy:)
('enum. subcollections' combinations:atATimeDo: groupsOf:atATimeDo: pairsCollect: pairsDo: permutationsDo: withNextDo: withPreviousCyclicDo: withPreviousDo:)
('find an element' findFirst: findFirst:startingAt: findFirstBasicAt:startingAt: findLast:)
('find in sorted receiver' findBinary: findBinary:do:ifNone: findBinaryIndex: findBinaryIndex:do:ifNone: quickFindFirst: quickFindLast:)
('gui' do:displayingProgress:)
('inspecting' customizeExplorerContents explorerContents)
('math functions' += -=)
('removing' remove:ifAbsent:)
('sorting' sorted:)
('stream creation' asStreamResult readStream thatCanBeModified writeStream)
('testing' beginsWith: endsWith: ifInBounds:ifNot: includes: isInBounds: isSequenceable)
('private' asDigitsAt:in:do: combinationsAt:upTo:in:after:upTo:do: copyReplaceAll:with:asTokens: errorOutOfBounds permutationsStartingAt:do:)
('[mk] accessing' indexOf: replaceFrom:to:with:)
!

