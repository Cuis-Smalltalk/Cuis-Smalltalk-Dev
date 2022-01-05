'From Cuis 6.0 [latest update: #5040] on 5 January 2022 at 4:26:32 pm'!

!Text methodsFor: 'attributes' stamp: 'jmv 1/5/2022 16:13:27'!
hasAnyParagraphAttribute
	"Return false if there are no paragraph attributes"

	^runs values anySatisfy: [ :emphArray | emphArray anySatisfy: [ :attr | attr isParagraphAttribute ]]! !


!TextParagraphMorph methodsFor: 'accessing' stamp: 'jmv 1/5/2022 14:07:55'!
contents: aStringOrText
	textComposition textComposed ~= aStringOrText ifTrue: [
		textComposition	
			initialize;
			setModel: (TextModel withText: aStringOrText);
			extentForComposing: self extentForComposing;
			composeAll.
		extent _ textComposition usedExtent + (4 * borderWidth).
		self redrawNeeded ]! !

!TextParagraphMorph methodsFor: 'initialization' stamp: 'jmv 1/5/2022 14:10:00'!
initialize
	super initialize.
	extent _ `1250@1140`.
	textComposition _ TextComposition new.
	textComposition
		setModel: (TextModel withText: '');
		extentForComposing: self extentForComposing;
		composeAll.! !

!TextParagraphMorph methodsFor: 'private' stamp: 'jmv 1/5/2022 16:19:56'!
fit
	"Adjust my height to fit the text."

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


!TextComposition methodsFor: 'access' stamp: 'jmv 1/5/2022 16:19:07'!
usedExtent
	"If there is any justified, rightFlush or centered text, assume all available width has been used."

	^ self usedWidth @ self usedHeight! !

!TextComposition methodsFor: 'access' stamp: 'jmv 1/5/2022 16:18:54'!
usedWidth
	"If there is any justified, rightFlush or centered text, assume all available width has been used."

	^ maxRightX! !

!TextComposition methodsFor: 'composition' stamp: 'jmv 1/5/2022 16:18:59'!
composeLinesFrom: start to: stop delta: delta into: lineColl priorLines: priorLines atY: startingY 
	"While the section from start to stop has changed, composition may ripple all the way to the end of the text.  However in a rectangular container, if we ever find a line beginning with the same character as before (ie corresponding to delta in the old lines), then we can just copy the old lines from there to the end of the container, with adjusted indices and y-values"

	| newResult composer |
	composer _ TextComposer new.
	editor ifNotNil: [
		composer editor: editor ].
	newResult _ composer
				composeLinesFrom: start
				to: stop
				delta: delta
				into: lineColl
				priorLines: priorLines
				atY: startingY
				text: model actualContents
				extentForComposing: extentForComposing.
	lines _ newResult first asArray.

	"If there is any justified, rightFlush or centered text, assume all available width has been used."
	model actualContents hasAnyParagraphAttribute ifTrue: [ ^extentForComposing x ].

	^newResult second "right border of recomposed lines (bounded by extentForComposing x)"! !

