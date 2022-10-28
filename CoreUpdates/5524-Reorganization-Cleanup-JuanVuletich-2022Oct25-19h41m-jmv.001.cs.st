'From Cuis 6.0 [latest update: #5515] on 25 October 2022 at 7:53:18 pm'!
!classDefinition: #Float32PointArray category: #'Collections-Arrayed'!
Float32Array variableWordSubclass: #Float32PointArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!
!classDefinition: #PointArray category: #'Collections-Arrayed'!
IntegerArray variableWordSubclass: #PointArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!
!classDefinition: #ShortIntegerArray category: #'Collections-Arrayed'!
ArrayedCollection variableWordSubclass: #ShortIntegerArray
	instanceVariableNames: ''
	classVariableNames: 'LastSaveOrder'
	poolDictionaries: ''
	category: 'Collections-Arrayed'!
!classDefinition: #ShortPointArray category: #'Collections-Arrayed'!
ShortIntegerArray variableWordSubclass: #ShortPointArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!
!classDefinition: #ShortWordArray category: #'Collections-Arrayed'!
ShortIntegerArray variableWordSubclass: #ShortWordArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Arrayed'!
!classDefinition: #ShortRunArray category: #'Collections-Arrayed'!
ArrayedCollection variableWordSubclass: #ShortRunArray
	instanceVariableNames: ''
	classVariableNames: 'LastSaveOrder'
	poolDictionaries: ''
	category: 'Collections-Arrayed'!

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/25/2022 19:45:39'!
coversPixel: worldPoint
	"Answer true if pixel worldPoint is covered by us (or a submorph of ours), even if we are not visible a it
	because of some other morph above us also covers it.

	WARNING: Might answer nil when we simply don't know. It is generally advisable to call this method only for
	morphs where the answer is not nil:
		- BoxedMorphs. No sepecial care needed. (Senders in the base Cuis image are of this kind).
		- Morphs drawn by VectorCanvas (i.e. they #requiresVectorCanvas). See notes below.
	
	Note: If your morph #requiresVectorCanvas, and depends on this general implementation, remember to 
	`removeProperty: #bitMask` whenever it becomes invalid due to changes in your morphs. You may consider
	using #ownsPixel: if appropriate, that doesn't require any maintenance and is cheaper (in cpu and memory).
	See #bitMask.

	See #ownsPixel:
	See #includesPixel:"

	self visible ifTrue: [
		"Expensive way for morphs with arbitrary shape in VectorGraphics.
		The base Cuis System doesn't use this.
		Use this in applications if the expense of maintaining #bitMask is worth it."
		self canvas ifNotNil: [ :canvas |
			canvas canDoVectorGraphics ifTrue: [
				^(self bitMask pixelValueAt: worldPoint - self displayFullBounds topLeft) = 1 ]]].

	"If we don't know."
	^ nil.! !

!Morph methodsFor: 'geometry services' stamp: 'jmv 10/25/2022 19:46:51'!
includesPixel: worldPoint
	"Answer true if the grabbing from the pixel argument pixel with the mouse should grab us:

	- Answer true if we own the pixel, i.e. we are the last morph drawn at worldPoint.

	- Some implementations (BoxedMorph) may also answer true if we cover but don't own the pixel,
	meaning that some other morph was drawn later, covering us.

	- For other morphs, not BoxedMorph, (i.e. they run this implementation), if VectorGraphics is not active,
	answer true for any point that lies within our bounds. If this is the case, consider using VectorGraphics: In addition to
	many other advantages, this method will always answer a strictly correct answer.

	Therefore, senders need to be aware that the answer is meaningful only when it is known that no other morph
	covers us. A common case is to find the topmost morph at some position. In that case, iterating front to back ensures
	that if any other morph covers us, it is found first.

	Note: This method is redefined by some subclasses. This implementation is only used for morphs drawn by VectorCanvas,
	either because the main canvas is a VectorCanvas, or because it is a HybridCanvas and we answer true to #requiresVectorCanvas.
	(See other implementors)

	Note: Also see #ownsPixel: and #coversPixel:"

	"Check if #ownsPixel:, if possible."
	self canvas ifNotNil: [ :canvas |
		canvas canDoVectorGraphics ifTrue: [
			^ (canvas morphIdAt: worldPoint) = self morphId ]].

	^ false.! !

!methodRemoval: HaloMorph #morphLocalBounds stamp: 'jmv 10/25/2022 19:43:42'!
HaloMorph removeSelector: #morphLocalBounds!
!methodRemoval: AbstractFont #baseKern stamp: 'jmv 10/25/2022 19:48:19'!
AbstractFont removeSelector: #baseKern!
!methodRemoval: AbstractFont #characterToGlyphMap stamp: 'jmv 10/25/2022 19:48:19'!
AbstractFont removeSelector: #characterToGlyphMap!
!methodRemoval: AbstractFont #xTable stamp: 'jmv 10/25/2022 19:48:19'!
AbstractFont removeSelector: #xTable!
!methodRemoval: BitBlt #primDisplayString:from:to:map:xTable:kern: stamp: 'jmv 10/25/2022 19:48:19'!
BitBlt removeSelector: #primDisplayString:from:to:map:xTable:kern:!

!Character reorganize!
('accessing' asciiValue codePoint digitValue immediateNumericValue iso8859s15Code nonImmediateNumericValue numericValue)
('comparing' < <= = > >= hash identityHash)
('testing' canBeGlobalVarInitial is: isAccented isAlphaNumeric isCharacter isDigit isLetter isLineSeparator isLowercase isSeparator isSpecial isUnaccented isUppercase isValidInBinarySelectors isValidInIdentifiers isValidStartOfIdentifiers isVowel tokenish)
('copying' shallowCopy)
('printing' hex isLiteral printOn: storeOn:)
('converting' asCharacter asLowercase asString asSymbol asText asUnaccented asUnicodeCodePoint asUppercase to: withDiacriticalMark:)
('object fileIn' comeFullyUpOnReload:)
('fileman-testing' isDriveLetter isDriveSeparator isPathSeparator)
('double dispatch' widthIn:)
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
('double dispatch' widthIn:)
!


!Collection reorganize!
('accessing' anyOne atRandom atRandom: size)
('adapting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend:)
('adding' add: add:withOccurrences: addAll:)
('arithmetic' * + - / // \\ raisedTo:)
('comparing' = emptyCollectionHash hash)
('converting' as:on:delimiter:last: asArray asBag asByteArray asCharacterSet asCommaSeparated:on: asCommaStringAnd asDictionary asFloat32Array asFloat64Array asIdentitySet asIntegerArray asNewArray asOrderedCollection asPointArray asSet asSortedCollection asSortedCollection: asStringOn:delimiter:last: asWordArray)
('copying' , copyWith: copyWithout: copyWithoutAll: flatten flattenTo:)
('enumerating' allSatisfy: anySatisfy: associationsDo: collect: collect:andFold: collect:andFold:ifEmpty: collect:thenSelect: count: detect: detect:ifFound:ifNone: detect:ifNone: detectMax: detectMin: difference: do: do:separatedBy: do:without: explorerContents explorerContentsWithIndexCollect: fold: fold:ifEmpty: groupBy: groupBy:having: inject:into: intersection: noneSatisfy: reduce: reject: select: select:thenCollect: select:thenDo: union:)
('filter streaming' contents)
('math functions' abs arcCos arcSin arcTan ceiling cos degreeCos degreeSin exp floor ln log log2 negated reciprocal roundTo: rounded sign sin sqrt squared tan truncateTo: truncated)
('printing' printElementsOn: printNameOn: printOn: storeOn:)
('private' emptyCheck errorCollectionTooSmall errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed species)
('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)
('testing' canHoldCodePoints hasContentsInExplorer identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifNotEmpty: ifNotEmpty:ifEmpty: includes: includesAllOf: includesAnyOf: includesSubstringAnywhere: is: isCollection isEmpty isEmptyOrNil isSequenceable notEmpty occurrencesOf:)
('sorting' sorted sorted:)
('statistics' average average: average:ifEmpty: max max: mean min min: product product: range sum sum: sum:ifEmpty:)
!


!TrueTypeFont reorganize!
('printing' printOn:)
('objects from disk' objectForDataStream:)
('initialization' addDerivativeFont: baseFont: family:ttFontDescription:pointSize:)
('accessing' ascent baseFont cachedFormDepth descent familyName fontDescription lineGap lineSpacing pointSize widthOf: widthOfCodePoint: widthOfUtf8Byte1:b2:b3:b4:)
('testing' isBaseFont isBold isItalic isStruckThrough isSubscript isSuperscript isUnderlined)
('glyph access' glyphAt: glyphAtUtf8Byte1:byte2:byte3:byte4:)
('emphasis' addEmphasis: emphasis emphasized:)
('synthetic derivatives' makeStruckThroughGlyph: makeUnderlinedGlyph: syntheticSubscript syntheticSuperscript)
('swapping glyphs' useShortUnderscore)
('caching' releaseCachedState)
!


!LayoutMorph class reorganize!
('instance creation' new newColumn newRow)
('examples' example1 example10 example101VG example11 example13 example1b example2 example20 example3 example6 example7 launcherExample)
('new-morph participation' categoryInNewMorphMenu)
!

