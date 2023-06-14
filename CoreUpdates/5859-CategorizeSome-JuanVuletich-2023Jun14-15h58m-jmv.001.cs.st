'From Cuis 6.0 [latest update: #5857] on 14 June 2023 at 4:10:14 pm'!

!TextAttribute reorganize!
('accessing' anchoredFormOrMorph emphasisCode)
('comparing' dominates:)
('iterating' forBaseFontDo: forCharacterStyleReferenceDo: forFontFamilyAndSizeDo: forParagraphStyleReferenceDo: forTextActionInfoDo: forTextAlignmentDo: forTextBackgroundColorDo: forTextColorDo: forTextEmphasisDo:)
('testing' canBeJoinedWith: dominatedByNormalEmphasis isBackgroundColor isFont isForFormatting isForShout isParagraphAttribute isTextColor mayActOnClick mayBeExtended)
('text action' actOnClickFor: actOnClickFor:in: actOnClickFor:in:at: actOnClickFor:in:at:editor:)
!


!TextAction reorganize!
('as yet unclassified' analyze: info validate:)
('iterating' forTextActionInfoDo:)
('testing' dominatedByNormalEmphasis isForFormatting mayActOnClick)
!


!TextDoIt reorganize!
('as yet unclassified' analyze: evalString: info)
('accessing' evalString)
('comparing' = hash)
('text action' actOnClickFor:)
!


!TextURL reorganize!
('as yet unclassified' analyze: info url:)
('accessing' url)
('comparing' = hash)
('text action' actOnClickFor:)
!


!TextAlignment reorganize!
('accessing' alignment alignment:)
('comparing' = dominates: hash)
('iterating' forTextAlignmentDo:)
('testing' isParagraphAttribute)
!


!TextBackgroundColor reorganize!
('accessing' color color:)
('comparing' = dominates: hash)
('iterating' forTextBackgroundColorDo:)
('printing' printOn:)
('testing' isBackgroundColor)
!


!TextColor reorganize!
('accessing' color color:)
('comparing' = dominates: hash)
('iterating' forTextColorDo:)
('printing' printOn:)
('testing' isTextColor)
!


!TextEmphasis reorganize!
('accessing' emphasisCode emphasisCode:)
('comparing' = dominates: hash)
('iterating' forTextEmphasisDo:)
('printing' printOn:)
('testing' dominatedByNormalEmphasis)
!


!TextAnchor reorganize!
('accessing' anchoredFormOrMorph anchoredFormOrMorph:)
('comparing' = hash)
('copying' postCopy)
('testing' canBeJoinedWith: isForFormatting mayBeExtended)
!

