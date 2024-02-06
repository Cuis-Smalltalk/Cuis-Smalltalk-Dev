'From Cuis6.3 [latest update: #6215] on 6 February 2024 at 12:59:05 pm'!

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 2/6/2024 12:55:22'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent newHeight newWidth oldExtent |
	(owner is: #ScrollPane) ifFalse: [
		^self ].

	newWidth := extent x.
	"Adjust width only if we don't wrap text to own width!!"
	wrapFlag ifFalse: [
		newWidth := self textComposition usedWidth + 3 max: 9 ].
	newHeight := self textComposition usedHeight max: FontFamily defaultLineSpacing + 2.
	newExtent := newWidth @ newHeight.
	extent = newExtent 
		ifTrue: [
			"Too conservative: only text composition (because of changes in text or styles, etc)
			should cause invalidation.
			Try to avoid calling #fit unless needed."
			self redrawNeeded ]
		ifFalse: [
			oldExtent := extent.
			extent := newExtent.
			self extentChanged: oldExtent.
			owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
			self redrawNeeded ].

	owner innerHeight: newExtent y.! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 2/6/2024 12:58:46'!
updateFromTextComposition
	"A change has taken place in my textComposition, as a result of editing and I must be updated. "

	textComposition ifNotNil: [
		wrapFlag ifFalse: [
			textComposition
				initialize;
				extentForComposing: self extentForComposing;
				composeAll ].
		editor storeSelectionInComposition.
		needsFit ifTrue: [
			self fit.
			needsFit := false ]].

	(owner is: #ScrollPane) ifTrue: [
		owner
			updateScrollBarsBounds;
			setScrollDeltas ].! !

