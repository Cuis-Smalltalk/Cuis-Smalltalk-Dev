"
Tells a piece of text to be a certain way.

"
Class {
	#name : 'TextAttribute',
	#superclass : 'Object',
	#category : 'System-Text-Attributes',
	#package : 'System',
	#tag : 'Text-Attributes'
}

{ #category : 'text action' }
TextAttribute >> actOnClickFor: model [
	"Subclasses may override to provide, eg, hot-spot actions"
	^ false
]

{ #category : 'text action' }
TextAttribute >> actOnClickFor: model in: aTextComposition [
	^self actOnClickFor: model
]

{ #category : 'text action' }
TextAttribute >> actOnClickFor: model in: aTextComposition at: clickPoint [
	^self actOnClickFor: model in: aTextComposition
]

{ #category : 'text action' }
TextAttribute >> actOnClickFor: model in: aTextComposition at: clickPoint editor: editor [
	^self actOnClickFor: model in: aTextComposition at: clickPoint
]

{ #category : 'accessing' }
TextAttribute >> anchoredFormOrMorph [
	"If one hides here, return it"
	^nil
]

{ #category : 'testing' }
TextAttribute >> canBeJoinedWith: aTextAttribute [
	"To be used for RunArray compaction"
	^self = aTextAttribute
]

{ #category : 'testing' }
TextAttribute >> dominatedByNormalEmphasis [
	"Subclasses may override if TextEmphasis normal (i.e. cmd-0) should turn them off"
	^ false
]

{ #category : 'comparing' }
TextAttribute >> dominates: another [
	"Subclasses may override condense multiple attributes"
	^ false
]

{ #category : 'accessing' }
TextAttribute >> emphasisCode [
	"Subclasses may override to add bold, italic, etc"
	^ 0
]

{ #category : 'iterating' }
TextAttribute >> forCharacterStyleReferenceDo: aBlock [
	"No action is the default"
]

{ #category : 'iterating' }
TextAttribute >> forFontFamilyDo: aBlock [
	"No action is the default"
]

{ #category : 'iterating' }
TextAttribute >> forFontSizeDo: aBlock [
	"No action is the default"
]

{ #category : 'iterating' }
TextAttribute >> forLineSpacingDo: aBlock [
	"No action is the default"
]

{ #category : 'iterating' }
TextAttribute >> forParagraphStyleReferenceDo: aBlock [
	"No action is the default"
]

{ #category : 'iterating' }
TextAttribute >> forTextActionInfoDo: aBlock [
	"No action is the default"
]

{ #category : 'iterating' }
TextAttribute >> forTextAlignmentDo: aBlock [
	"No action is the default"
]

{ #category : 'iterating' }
TextAttribute >> forTextBackgroundColorDo: aBlock [
	"No action is the default"
]

{ #category : 'iterating' }
TextAttribute >> forTextColorDo: aBlock [
	"No action is the default"
]

{ #category : 'iterating' }
TextAttribute >> forTextEmphasisDo: aBlock [
	"No action is the default"
]

{ #category : 'testing' }
TextAttribute >> isBackgroundColor [
	^false
]

{ #category : 'testing' }
TextAttribute >> isCenteredOrRightFlush [
	^false
]

{ #category : 'testing' }
TextAttribute >> isFont [
	^false
]

{ #category : 'testing' }
TextAttribute >> isForFormatting [
	"Answer false for those attributes that are not for text formatting.
	This is, those that are not considered in #withFormattingAttributeValues:do:"
	^true
]

{ #category : 'testing' }
TextAttribute >> isForShout [
	"True if to be removed from code before styling"
	^false
]

{ #category : 'testing' }
TextAttribute >> isParagraphAttribute [
	"Attributes that answer true will always be applied to whole paragraphs, i.e. starting at the position after a newLine (or 1) and ending at a newLine (or text size)
	"
	^false
]

{ #category : 'testing' }
TextAttribute >> isTextAlignment [
	^false
]

{ #category : 'testing' }
TextAttribute >> isTextColor [
	^false
]

{ #category : 'testing' }
TextAttribute >> mayActOnClick [
	"Subclasses may override to provide, eg, hot-spot actions"
	^ false
]

{ #category : 'testing' }
TextAttribute >> mayBeExtended [
	"A quality that may be overridden by subclasses, such as TextAnchors, that really only apply to a single character"
	^ true
]
