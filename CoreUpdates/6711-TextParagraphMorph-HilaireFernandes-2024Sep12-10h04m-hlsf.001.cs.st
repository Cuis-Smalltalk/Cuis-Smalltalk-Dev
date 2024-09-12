'From Cuis7.1 [latest update: #6709] on 12 September 2024 at 10:09:05 am'!

!TextParagraphMorph methodsFor: 'initialization' stamp: 'hlsf 9/12/2024 10:06:26'!
defaultPadding
	^ `0@0`! !


!TextParagraphMorph methodsFor: 'geometry' stamp: 'hlsf 9/12/2024 10:04:01'!
reflowAndMaybeResize
	"Reflow text. If appropriate, adjust height or extent."

	| newHeight |
	textComposition
		extentForComposing: self extentForComposing;
		composeAll.
	setsHeightOnContent ifTrue: [
		newHeight := textComposition usedHeight + (2 * self interiorOrigin y)
				max: self minimumExtent y.
		extent y = newHeight ifFalse: [
			extent := extent x @ newHeight.
			self redrawNeeded.
			owner ifNotNil: [
				owner someSubmorphPositionOrExtentChanged ]].
		(owner is: #ScrollPane) ifTrue: [
			owner
				updateScrollBarsBounds;
				setScrollDeltas ].
		^self ].
	shrinkFromExtent ifNotNil: [
		extent := textComposition usedExtent + (2 * self interiorOrigin) ].
	self redrawNeeded.! !


!TextParagraphMorph reorganize!
('accessing' contents: extentForComposing setHeightOnContent shrinkFromExtent:)
('initialization' defaultPadding initialize)
('drawing' drawOn:)
('geometry' adjustExtent borderWidth: extentChanged: minimumExtent privateExtent: reflowAndMaybeResize)
!

