'From Cuis 5.0 [latest update: #4995] on 2 December 2021 at 5:35:23 pm'!

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 12/2/2021 17:00:09'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent newHeight newWidth oldExtent |
	(owner is: #ScrollPane) ifFalse: [
		^self ].

	newWidth _ extent x.
	"Adjust width only if we don't wrap text to own width!!"
	wrapFlag ifFalse: [
		newWidth _ self textComposition usedWidth max: 9 ].
	newHeight _ self textComposition usedHeight max: FontFamily defaultLineSpacing + 2.
	newExtent _ newWidth @ newHeight.
	extent = newExtent 
		ifTrue: [
			"Too conservative: only text composition (because of changes in text or styles, etc)
			should cause invalidation.
			Try to avoid calling #fit unless needed."
			self redrawNeeded ]
		ifFalse: [
			oldExtent _ extent.
			extent _ newExtent.
			self extentChanged: oldExtent.
			owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
			self redrawNeeded ].

	owner innerHeight: newExtent y.! !


!TextParagraphMorph methodsFor: 'private' stamp: 'jmv 12/2/2021 17:32:15'!
fit
	"Adjust my bounds to fit the text."

	| newExtent oldExtent |
	(owner is: #ScrollPane) ifFalse: [
		^self ].
	newExtent _ extent x
		@ (textComposition usedHeight + (4 * borderWidth)
			max: FontFamily defaultLineSpacing + 2).
	extent = newExtent ifFalse: [
		oldExtent _ extent.
		extent _ newExtent.
		self extentChanged: oldExtent.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ].! !

