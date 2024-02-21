'From Cuis6.3 [latest update: #6229] on 21 February 2024 at 10:49:12 am'!

!TextEditor methodsFor: 'typing support' stamp: 'jmv 2/21/2024 10:43:48'!
setEmphasisHereFromTextForward: f

	| i forward attributeIndex prevIsLineSeparator nextIsLineSeparator prevIsSeparator nextIsSeparator nextChar prevChar |
	i := self pointIndex.
	"Try to set emphasisHere correctly after whitespace.
	Most important after a cr, i.e. at the start of a new line"
	prevIsLineSeparator :=  i > 1 and: [ (prevChar := model actualContents at: i-1) isLineSeparator ].
	nextIsLineSeparator := i <= model textSize and: [ (nextChar := model actualContents at: i) isLineSeparator ].
	prevIsSeparator :=  i > 1 and: [ prevChar isSeparator ].
	nextIsSeparator := i <= model textSize and: [ nextChar isSeparator ].
	prevIsLineSeparator & nextIsLineSeparator
		ifTrue: [
			"Empty paragraph: take emphasis from the newLine character"
			forward := false ]
		ifFalse: [
			prevIsSeparator == nextIsSeparator
				ifTrue: [
					"Space at both sides, or non-space at both sides, take emphasis used where the cursor comes from"
					forward := f ]
				ifFalse: [
					"Space at one side and non-space at the other, take emphasis from the non-space character"
					forward := nextIsSeparator ]].
	attributeIndex := i - (forward ifTrue: [ 1 ] ifFalse: [ 0 ]).
	attributeIndex > 0 
		ifTrue: [
			emphasisHere := (model actualContents attributesAt: attributeIndex)
						select: [ :att | att mayBeExtended ].
			morph possiblyChanged ].! !

!TextEditor methodsFor: 'initialization' stamp: 'jmv 2/21/2024 10:43:11'!
initialize
	emphasisHere := #().
	initialSelectionStart := nil.
	initialSelectionStop := nil.
	doWordSelection := false.
	doParagraphSelection := false.
	defaultFont := FontFamily defaultFamilyAndPointSize.! !

