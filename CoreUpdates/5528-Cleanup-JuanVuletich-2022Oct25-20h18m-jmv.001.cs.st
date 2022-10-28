'From Cuis 6.0 [latest update: #5515] on 25 October 2022 at 8:20:23 pm'!
!methodRemoval: MorphicCanvas #textComposition:bounds:color:selectionColor:avoidOverhang: stamp: 'jmv 10/25/2022 20:19:40'!
MorphicCanvas removeSelector: #textComposition:bounds:color:selectionColor:avoidOverhang:!
!methodRemoval: MorphicCanvas #ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor: stamp: 'jmv 10/25/2022 20:19:42'!
MorphicCanvas removeSelector: #ellipseCenterX:y:rx:ry:borderWidth:borderColor:fillColor:!
!methodRemoval: MorphicCanvas #textComposition:bounds:color:selectionColor: stamp: 'jmv 10/25/2022 20:19:43'!
MorphicCanvas removeSelector: #textComposition:bounds:color:selectionColor:!
!methodRemoval: TrueTypeFont #useShortUnderscore stamp: 'jmv 10/25/2022 20:19:33'!
TrueTypeFont removeSelector: #useShortUnderscore!

!TrueTypeFont reorganize!
('printing' printOn:)
('objects from disk' objectForDataStream:)
('initialization' addDerivativeFont: baseFont: family:ttFontDescription:pointSize:)
('accessing' ascent baseFont cachedFormDepth descent familyName fontDescription lineGap lineSpacing pointSize widthOf: widthOfCodePoint: widthOfUtf8Byte1:b2:b3:b4:)
('testing' isBaseFont isBold isItalic isStruckThrough isSubscript isSuperscript isUnderlined)
('glyph access' glyphAt: glyphAtUtf8Byte1:byte2:byte3:byte4:)
('emphasis' addEmphasis: emphasis emphasized:)
('synthetic derivatives' makeStruckThroughGlyph: makeUnderlinedGlyph: syntheticSubscript syntheticSuperscript)
('caching' releaseCachedState)
!

