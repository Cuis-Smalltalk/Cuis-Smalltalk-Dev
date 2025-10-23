'From Cuis7.5 [latest update: #7668] on 22 October 2025 at 10:02:04 am'!

!Text methodsFor: 'attributes' stamp: 'jmv 10/21/2025 18:19:23'!
hasAnyCenteredOrRightFlush
	"Return false if there are no Text Alignment attributes for center or right flush"

	^runs values anySatisfy: [ :emphArray | emphArray anySatisfy: [ :attr | attr isCenteredOrRightFlush ]]! !


!TextAttribute methodsFor: 'testing' stamp: 'jmv 10/21/2025 18:18:43'!
isCenteredOrRightFlush
	^false! !


!TextAlignment methodsFor: 'testing' stamp: 'jmv 10/21/2025 18:18:35'!
isCenteredOrRightFlush
	^alignment = 2 or: [ alignment = 1 ]! !


!TextParagraphMorph methodsFor: 'accessing' stamp: 'jmv 10/21/2025 17:14:57'!
contents: aStringOrText

	| current |
	current := textComposition textComposed.
	current = aStringOrText ifTrue: [
		((current is: #Text) and: [aStringOrText is: #Text])
		ifTrue: [
			current runs = aStringOrText runs ifTrue: [
				^self ]]
		ifFalse: [
			^self ]].
	textComposition setModel: (TextModel withText: aStringOrText).
	self reflowAndMaybeResize.! !

!TextParagraphMorph methodsFor: 'geometry' stamp: 'jmv 10/21/2025 18:20:48'!
reflowAndMaybeResize
	"Reflow text. If appropriate, adjust height or extent."

	| extentForComposing usedExtent newHeight newExtent |
	extentForComposing := self extentForComposing.
	"If we must shrink, but have centered or right aligned text"
	(shrinkFromExtent notNil and: [textComposition textComposed hasAnyCenteredOrRightFlush]) ifTrue: [
		| leftAlignedText auxTC |
		leftAlignedText := textComposition textComposed copy removeAttributesThat: [ :attribute | attribute isCenteredOrRightFlush ].
		auxTC := TextComposition new.
		auxTC setModel: (TextModel withText: leftAlignedText).
		auxTC
			extentForComposing: extentForComposing;
			composeAll.
		extentForComposing := auxTC usedExtent ].

	textComposition
		extentForComposing: extentForComposing;
		composeAll.
	usedExtent := textComposition usedExtent.

	newExtent := extent.
	setsHeightOnContent ifTrue: [
		newHeight := usedExtent y + (2 * self interiorOrigin y)
				max: self minimumExtent y.
		newExtent := extent x @ newHeight ].
	shrinkFromExtent ifNotNil: [
		newExtent := usedExtent + (2 * self interiorOrigin) ].

	newExtent = extent ifFalse: [
		extent := newExtent.
		owner ifNotNil: [
			owner someSubmorphPositionOrExtentChanged ].
		(owner is: #ScrollPane) ifTrue: [
			owner
				updateScrollBarsBounds;
				setScrollDeltas ]].

	self redrawNeeded.! !

