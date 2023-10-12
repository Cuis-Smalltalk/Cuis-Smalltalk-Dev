'From Cuis 6.0 [latest update: #6042] on 11 October 2023 at 11:54:11 am'!

!Integer methodsFor: 'inspecting' stamp: 'jmv 10/11/2023 11:45:37'!
customizeExplorerContents

	^ true! !

!Integer methodsFor: 'inspecting' stamp: 'jmv 10/11/2023 10:23:55'!
hasContentsInExplorer

	^true! !


!SequenceableCollection methodsFor: 'inspecting' stamp: 'jmv 10/11/2023 11:18:44'!
explorerContents

	^self asArray withIndexCollect: [:value :index |
		ObjectExplorerWrapper
			with: value
			name: index printString
			model: self]! !


!ArrayedCollection methodsFor: 'inspecting' stamp: 'jmv 10/11/2023 11:41:27'!
explorerContents
	"See #indexHead and #indexTail"

	| answer indexHead indexTail total shown elementIndex |
	indexHead := 500.
	indexTail := 30.
	shown := indexHead + indexTail.
	total := self size.

	total <= shown ifTrue: [
		^super explorerContents ].

	answer := Array new: shown.
	1 to: indexHead do: [ :index |
		answer at: index put: (
			ObjectExplorerWrapper
				with: (self at: index)
				name: index printString
				model: self) ].

	elementIndex := total-indexTail+1.
	indexHead+1 to: shown do: [ :index |
		answer at: index put: (
			ObjectExplorerWrapper
				with: (self at: elementIndex)
				name: elementIndex printString
				model: self).
		elementIndex := elementIndex + 1 ].

	^answer! !


!OrderedCollection methodsFor: 'inspecting' stamp: 'jmv 10/11/2023 11:41:31'!
explorerContents
	"See #indexHead and #indexTail"

	| answer indexHead indexTail total shown elementIndex |
	indexHead := 500.
	indexTail := 30.
	shown := indexHead + indexTail.
	total := self size.

	total <= shown ifTrue: [
		^super explorerContents ].

	answer := Array new: shown.
	1 to: indexHead do: [ :index |
		answer at: index put: (
			ObjectExplorerWrapper
				with: (self at: index)
				name: index printString
				model: self) ].

	elementIndex := total-indexTail+1.
	indexHead+1 to: shown do: [ :index |
		answer at: index put: (
			ObjectExplorerWrapper
				with: (self at: elementIndex)
				name: elementIndex printString
				model: self).
		elementIndex := elementIndex + 1 ].

	^answer! !


!Dictionary methodsFor: 'inspecting' stamp: 'jmv 10/11/2023 10:40:54'!
explorerContents

	^self explorerContentsWithIndexCollect: [:value :index |
		ObjectExplorerWrapper
			with: value
			name: index printString
			model: self]! !


!Collection methodsFor: 'inspecting' stamp: 'jmv 10/11/2023 11:46:44'!
hasContentsInExplorer

	^ true! !


!ObjectExplorerWrapper methodsFor: 'as yet unclassified' stamp: 'jmv 10/11/2023 10:30:32'!
contents
	"For objects with specific ways to show contents in explorers."
	(item customizeExplorerContents) ifTrue: [
		^ item explorerContents].

	"For all others, show named vars first, then indexed vars"
	^(item class allInstVarNames asOrderedCollection withIndexCollect: [:each :index |
		self class
			with: (item instVarAt: index)
			name: each
			model: item
			parent: self]) ,
	((1 to: item basicSize) collect: [:index |
		self class
			with: (item basicAt: index)
			name: index printString
			model: item
			parent: self])! !

!methodRemoval: String #hasContentsInExplorer stamp: 'jmv 10/11/2023 11:46:52'!
String removeSelector: #hasContentsInExplorer!
!methodRemoval: Collection #explorerContentsWithIndexCollect: stamp: 'jmv 10/11/2023 11:49:10'!
Collection removeSelector: #explorerContentsWithIndexCollect:!
!methodRemoval: Collection #explorerContents stamp: 'jmv 10/11/2023 11:49:10'!
Collection removeSelector: #explorerContents!

!Object reorganize!
('accessing' addInstanceVarNamed:withValue: at: at:put: basicAt: basicAt:put: basicSize rawBasicAt: rawBasicAt:put: size yourself)
('associating' ->)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' class isKindOf: isMemberOf: respondsTo:)
('comparing' = ~= hash literalEqual:)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asString complexContents copyAs: mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' copy copyForClipboard copyFrom: copySameFrom: postCopy shallowCopy veryDeepCopy)
('events-old protocol' addDependent: breakDependents changed: removeDependent:)
('error handling' assert: assert:description: caseError deny: doesNotUnderstand: error: handles: notYetImplemented notify: notify:at: primitiveFail primitiveFailed primitiveFailed: shouldBeImplemented shouldNotHappen shouldNotHappenBecause: shouldNotHappenBecauseErrorMessage shouldNotHappenErrorMessage shouldNotImplement subclassResponsibility)
('evaluating' value valueWithArguments: valueWithPossibleArgument:)
('events-accessing' actionForEvent: actionMap actionSequenceForEvent: actionsDo: hasActionForEvent: setActionSequence:forEvent: updateableActionMap)
('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)
('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent:)
('events-triggering' triggerEvent: triggerEvent:with: triggerEvent:withArguments:)
('finalization' actAsExecutor executor finalizationRegistry finalize retryWithGC:until: toFinalizeSend:to:with:)
('inspecting' basicInspect copyToClipboard customizeExplorerContents explore hasContentsInExplorer inspect inspectWithLabel: inspectorClass)
('macpal' flash)
('message handling' argumentName argumentNameSufix disableCode: executeMethod: perform: perform:with: perform:with:with: perform:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: perform:withPossiblyWrongSizedArguments: with:executeMethod: with:with:executeMethod: with:with:with:executeMethod: with:with:with:with:executeMethod: withArgs:executeMethod:)
('object serialization' comeFullyUpOnReload: convertToCurrentVersion:refStream: objectForDataStream: readDataFrom:size: releaseCachedState storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: longPrintText longPrintTextLimitedTo: nominallyUnsent: print printAs:limitedTo: printOn: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: shortPrintString storeOn: storeString)
('system primitives' becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarAtPrim73: instVarAtPrim74:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: someObject)
('stepping' stepAt: wantsSteps)
('testing' is: isArray isBehavior isBlock isByteString isCharacter isClosure isCollection isComplex isContext isFloat isFloatOrFloatComplex isFraction isInteger isInterval isMethodProperties isNumber isObject isPoint isPseudoContext isString isSymbol isUnicodeCodePoint isUnicodeString isVariableBinding name renameTo:)
('translation support' inline: success: var:declareC:)
('user interface' browseClassHierarchy confirm: confirm:orCancel: inform: request:do: request:initialAnswer: request:initialAnswer:do: request:initialAnswer:do:orCancel: request:initialAnswer:orCancel: request:initialAnswer:verifying:do: request:initialAnswer:verifying:do:orCancel:)
('private' deprecatedMethod errorDescriptionForSubcriptBounds: errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError: setPinned: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('morphic' activeHand runningWorld)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('profiler support' profilerFriendlyCall:)
('pinning' isPinned pin unpin)
('updating' changed update:)
('debugging-halt' clearHaltOnce halt halt: haltOnce haltOnceEnabled setHaltOnce toggleHaltOnce)
!


!Float reorganize!
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan: arcTanNonPrimitive copySignTo: cos cosh degreeCos degreeSin expNonPrimitive floorLog: lnNonPrimitive log log2 log: nthRoot: reciprocal reciprocalFloorLog: reciprocalLogBase2 safeArcCos sinNonPrimitive sinh smoothIsAbsBelow: smoothStep sqrt sqrtNonPrimitive tan tanh)
('converting' adaptToFraction:andSend: adaptToInteger:andSend: asFloat asFraction asIEEE32BitPrecisionFloat asIEEE32BitWord asTrueFraction degreesToRadians inMinusPiToPiRange partBits: partValues: partValues:ifInfinite:ifNaN: radiansToDegrees withNegativeSign)
('truncation and round off' exponent exponentBits exponentPart floatsAwayFrom: integerPart mantissaBits mantissaPart nextAwayFromZero nextTowardsZero predecessor rounded roundedHAFZ signBit signPart significand significandAsInteger successor ulp)
('testing' isDenormalized isFinite isFloat isFloatOrFloatComplex isInfinite isLiteral isNaN isZero sign)
('arithmetic' abs negated)
('accessing' at: at:put: basicAt: basicAt:put: byteSize)
('printing' absPrintExactlyOn:base: absPrintOn:base: absPrintOn:base:mantissaSignificantBits: hex printAsIEEE32BitPrecisionFloatOn:base: printOn:base: printOn:fractionDigits: storeOn:base:)
('intervals' to:count:do: to:hops:do:)
('private' absPrintOn:base:digitCount: replaceWordsFrom:to:with:startingAt:)
('comparing' hash isWithin:floatsFrom: literalEqual:)
('inspecting' hasContentsInExplorer)
!


!LargePositiveInteger reorganize!
('arithmetic' * + - / // \\ abs negated quo:)
('bit manipulation' bitAt: bitReverse: bitXor: hashMultiply highBit highBitOfMagnitude)
('comparing' < <= > >= hash)
('converting' asFloat normalize withAtLeastNDigits:)
('mathematical functions' mightBeASquare sqrt)
('printing' printOn:base: printOn:base:nDigits:)
('system primitives' digitAt: digitAt:put: digitLength replaceFrom:to:with:startingAt:)
('testing' isLarge negative positive sign strictlyPositive)
('inspecting' hasContentsInExplorer)
!


!Collection reorganize!
('accessing' anyOne atRandom atRandom: size)
('adapting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend:)
('adding' add: add:withOccurrences: addAll:)
('arithmetic' * + - / // \\ raisedTo:)
('comparing' = emptyCollectionHash hash)
('converting' as:on:delimiter:last: asArray asBag asByteArray asCharacterSet asCommaSeparated:on: asCommaStringAnd asDictionary asFloat32Array asFloat64Array asIdentitySet asIntegerArray asNewArray asOrderedCollection asPointArray asSet asSortedCollection asSortedCollection: asStringOn:delimiter:last: asWordArray)
('copying' , copyWith: copyWithout: copyWithoutAll: flatten flattenTo:)
('enumerating' allSatisfy: anySatisfy: associationsDo: collect: collect:andFold: collect:andFold:ifEmpty: collect:thenSelect: count: detect: detect:ifFound:ifNone: detect:ifNone: detectMax: detectMin: difference: do: do:separatedBy: do:without: fold: fold:ifEmpty: groupBy: groupBy:having: inject:into: intersection: noneSatisfy: reduce: reject: select: select:thenCollect: select:thenDo: union:)
('filter streaming' contents)
('math functions' abs arcCos arcSin arcTan ceiling cos degreeCos degreeSin exp floor ln log log2 negated reciprocal roundTo: rounded sign sin sqrt squared tan truncateTo: truncated)
('printing' printElementsOn: printNameOn: printOn: storeOn:)
('private' emptyCheck errorCollectionTooSmall errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed species)
('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)
('testing' canHoldCodePoints identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifNotEmpty: ifNotEmpty:ifEmpty: includes: includesAllOf: includesAnyOf: includesSubstringAnywhere: is: isCollection isEmpty isEmptyOrNil isSequenceable notEmpty occurrencesOf:)
('sorting' sorted sorted:)
('statistics' average average: average:ifEmpty: max max: mean min min: product product: range sum sum: sum:ifEmpty:)
('inspecting' hasContentsInExplorer)
!


!SequenceableCollection reorganize!
('accessing' after: after:ifNone: allButFirst allButFirst: allButLast allButLast: antepenultimate antepenultimateIfAbsent: anyOne at:ifAbsent: atAll: atAll:put: atAll:putAll: atAllPut: atLast: atLast:ifAbsent: atLast:put: atPin: atRandom: atWrap: atWrap:put: before: before:ifNone: eighth fifth fillWith: first first: firstAvailable: fourth from:to:put: head: identityIndexOf: identityIndexOf:ifAbsent: indexOf: indexOf:ifAbsent: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfMax indexOfSubCollection:startingAt: indexOfSubCollection:startingAt:ifAbsent: last last: lastAvailable: lastIfEmpty: lastIndexOf: lastIndexOf:ifAbsent: lastIndexOf:startingAt:endingAt:do: lastIndexOf:startingAt:endingAt:ifAbsent: lastIndexOf:startingAt:ifAbsent: middle ninth penultimate penultimateIfAbsent: quickIndexOf: replaceAll:with: replaceFrom:to:with: replaceFrom:to:with:startingAt: second seventh sixth swap:with: tail: third)
('comparing' = hasEqualElements: hash hashQuick hashQuick:)
('converting' @ asByteArray asColorArray asNewArray asStreamResult concatenation isSequenceable printStringWithNewline readStream readStreamFrom:to: reverse reversed thatCanBeModified writeStream)
('copying' , copyAfter: copyAfterLast: copyFrom:count: copyFrom:to: copyReplaceAll:with: copyReplaceFrom:to:with: copyUpThrough: copyUpTo: copyUpToLast: copyWith: grownTo: shuffleBy: shuffled shuffledBy:)
('enumerating' allButFirstDo: allButLastDo: asDigitsToPower:do: collect: collect:from:to: combinations:atATimeDo: do: do:separatedBy: do:without: findBinary: findBinary:do:ifNone: findBinaryIndex: findBinaryIndex:do:ifNone: findFirst: findFirst:startingAt: findLast: from:to:do: groupsOf:atATimeDo: intervalOfSubCollection: keysAndValuesDo: pairsCollect: pairsDo: permutationsDo: polynomialEval: quickFindFirst: quickFindLast: replace: reverseDo: reverseWith:do: select: upTo: with:collect: with:do: with:do:separatedBy: with:reverseDo: with:with:collect: with:with:do: withIndexCollect: withIndexDo: withIndexDo:separatedBy: withNextDo: withPreviousCyclicDo: withPreviousDo:)
('inspecting' customizeExplorerContents explorerContents)
('private' asDigitsAt:in:do: combinationsAt:upTo:in:after:upTo:do: copyReplaceAll:with:asTokens: errorOutOfBounds permutationsStartingAt:do:)
('removing' remove:ifAbsent:)
('testing' beginsWith: endsWith: ifInBounds:ifNot: includes: isInBounds:)
('gui' do:displayingProgress:)
('math functions' += -= derivative incrementFraction integral)
('assertions' assertIsInBounds:)
('sorting' sorted:)
!


!String reorganize!
('accessing' at: at:put: byteSize findString:startingAt: findStringCaseInsensitive:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyOf:startingAt:ifAbsent: isLastLineSeparator isLastSeparator lineIndicesDo: replaceFrom:to:with:startingAt: size)
('comparing' < <= = > >= caseInsensitiveLessOrEqual: crc16 hash is:substringAt: sameAs:)
('copying' , appendToString: copyReplaceFrom:to:with: copyWith:)
('converting' asByteString asByteStringIfAscii asCodePoints asHex asLowercase asUppercase asUtf8Bytes asUtf8BytesOrByteString asUtf8BytesOrByteStringOrByteArray capitalized fixLineEndings substrings surroundedBySingleQuotes thatCanHoldCodePoints translateToLowercase translateToUppercase uncapitalized unescapePercents withLineEndings:)
('enumerating' collect:)
('printing' isLiteral)
('services' numArgs)
('testing' canHoldCodePoints is: isAscii isByteString isEmpty isInCanonicalCompositionForm)
('paragraph support' encompassLine: encompassParagraph:)
!


!Text reorganize!
('accessing' append: asUtf8Bytes at: at:put: commandForReplaceFrom:to:with: embeddedMorphs embeddedMorphsFrom:to: findString:startingAt: findString:startingAt:caseSensitive: findStringCaseInsensitive:startingAt: isLastLineSeparator isLastSeparator lineCount prepend: rangeOf:startingAt: replaceFrom:to:with: size string)
('comparing' = hash)
('copying' , appendToString: appendToText: copyFrom:to: copyReplaceFrom:to:with: grownTo: postCopy)
('converting' asNumber asPlainString asStreamResult asString asText asUnicodeStringOrText displayStringOrText optimizedForMutationSpeed replaceFrom:to:with:startingAt: replaceFrom:to:withString:attributes:startingAt: reversed thatCanHoldCodePoints)
('emphasis' addAttribute: addAttribute:from:to: alignmentAt: attributesAt: attributesFrom:to:do: characterStyleOrNilAt: characterStyleOrNilIfApplying: colorAt: commandForAddAttribute:from:to: commandForRemoveAttributes:from:to: emphasisAt: find: font: fontAt:default: fontIfApplying:default: initialFont: initialFontFrom: paragraphStyleOrNilAt: paragraphStyleOrNilIfApplying: removeAttributes:from:to: removeAttributesThat: runLengthFor: withAttributeValues:do:)
('testing' canHoldCodePoints is: isEmpty isLineEndAt: isRemote isSeparators)
('printing' printOn: storeOn:)
('private' basicReplaceAttributesFrom:to:with: fixParagraphAttributesFrom:to: privateSetParagraphAttributes:from:to: privateSetRuns: runs setString:setRuns:)
('attributes' canJoin:and: hasAnyAttribute hasAnyParagraphAttribute)
('user interface' edit editLabel:)
('paragraph support' encompassLine: encompassParagraph: paragraphStyleChunksDo:)
('adding emphasis' allBold bold italic pointSize: shoutDisableEmphasis struck sub super under)
('adding attributes' centered justified leftFlush rightFlush)
('adding color' black blue color: cyan gray green magenta red yellow)
('adding' add:)
('inspecting' customizeExplorerContents)
!


!Dictionary reorganize!
('accessing' associationAt: associationAt:ifAbsent: associations at: at:ifAbsent: at:ifAbsentPut: at:ifPresent: at:ifPresent:ifAbsent: at:ifPresentAndInMemory: at:put: keyAtIdentityValue: keyAtIdentityValue:ifAbsent: keyAtValue: keyAtValue:ifAbsent: keys keysSortedSafely values)
('testing' hasBindingThatBeginsWith: includes: includesKey: is: keyForIdentity: occurrencesOf:)
('adding' add: addAll: declare:from:)
('removing' keysAndValuesRemove: remove: remove:ifAbsent: removeKey: removeKey:ifAbsent: removeUnreferencedKeys unreferencedKeys)
('enumerating' associationsDo: collect: do: keysAndValuesDo: keysDo: select: valuesDo:)
('printing' printElementsOn: storeOn:)
('private' errorKeyNotFound: errorValueNotFound keyAt: noCheckAdd: postCopy rehash scanFor: valueAtNewKey:put:atIndex:declareFrom:)
('comparing' = emptyCollectionHash hash)
('inspecting' customizeExplorerContents explorerContents explorerContentsWithIndexCollect: inspectorClass)
('compiling' bindingOf: bindingsDo:)
!


!OrderedDictionary reorganize!
('removing' removeKey:ifAbsent:)
('enumerating' associationsDo: do: keysDo:)
('accessing' at:replacingKey:put: keysSortedSafely)
('inspecting' explorerContentsWithIndexCollect:)
('private' atNewIndex:put: init:)
!

