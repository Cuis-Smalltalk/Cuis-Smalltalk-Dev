'From Cuis 5.0 [latest update: #4214] on 11 June 2020 at 10:25:09 am'!
!methodRemoval: AbstractFont class #familyName:pointSize: stamp: 'jmv 6/11/2020 10:24:21'!
AbstractFont class removeSelector: #familyName:pointSize:!
!methodRemoval: AbstractFont class #default stamp: 'jmv 6/11/2020 10:24:21'!
AbstractFont class removeSelector: #default!
!methodRemoval: AbstractFont class #familyName:aroundPointSize: stamp: 'jmv 6/11/2020 10:24:21'!
AbstractFont class removeSelector: #familyName:aroundPointSize:!
!methodRemoval: AbstractFont class #pointSizesFor: stamp: 'jmv 6/11/2020 10:24:21'!
AbstractFont class removeSelector: #pointSizesFor:!
!methodRemoval: AbstractFont class #familyNames stamp: 'jmv 6/11/2020 10:24:21'!
AbstractFont class removeSelector: #familyNames!

!StrikeFont reorganize!
('accessing' ascent baseFont baseFont: baseKern characterToGlyphMap descent familyName glyphs lineSpacing name name: pointSize subscript superscript widthOf: xTable)
('testing' isBaseFont isBold isItalic isStruckThrough isSubscript isSuperscript isUnderlined)
('emphasis' bonk:with: emphasis emphasis: emphasized: isSynthetic isSynthetic: makeBoldGlyphs makeItalicGlyphs makeStruckThroughGlyphs makeUnderlinedGlyphs releaseCachedState reset)
('file in/out' printOn:)
('glyphs' ensureCleanBold extendMaxAsciiTo: fillZeroWidthSlots glyphAt: glyphAt:put: makeControlCharsVisible makeCrInvisible makeCrVisible makeLfInvisible makeLfVisible makeTabInvisible makeTabVisible useAlwaysLeftArrow useLeftArrow useRightArrow useShortUnderscore useUnderscore)
('private' createCharacterToGlyphMap)
('building' setGlyphsDepthAtMost: stripHighGlyphs)
('objects from disk' objectForDataStream:)
('synthetic derivatives' syntheticBold syntheticItalic syntheticST80Glyphs syntheticStruckThrough syntheticSubscript syntheticSuperscript syntheticUnderlined)
('copying' postCopy)
('BitBltCanvas support' onBitBltCanvasEngine:displayString:from:to:at:color:)
!


!StrikeFont class reorganize!
('removing' limitTo16Bits saveSpace)
('character shapes' makeControlCharsVisible makeCrInvisible makeCrVisible makeLfInvisible makeLfVisible makeTabInvisible makeTabVisible useUnderscore)
('class cached access' releaseClassState)
('initialization' assignmentGlyphSelectorPreferenceChanged)
!

