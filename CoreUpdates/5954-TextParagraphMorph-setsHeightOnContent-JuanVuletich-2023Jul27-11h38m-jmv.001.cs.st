'From Cuis 6.0 [latest update: #5950] on 27 July 2023 at 12:07:31 pm'!
!classDefinition: #TextParagraphMorph category: 'Morphic-Widgets'!
BoxedMorph subclass: #TextParagraphMorph
	instanceVariableNames: 'textComposition setsHeightOnContent '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!TextParagraphMorph commentStamp: 'jmv 7/27/2023 12:02:52' prior: 0!
TextParagraphMorph support display of text with emphasis. It can be used on its own, no need to include them in some kind of TextModelMorph / ScrollPane.

WordWrap is done and clipping is done to morph height. If the `owner is: #ScrollPane`, then height is automatically adjusted.
Support all features of Text, including fonts, sizes, emphasis and embedded morphs.

(TextParagraphMorph contents: 'Hello
World!!') openInWorld.


(TextParagraphMorph contents: Utilities defaultTextEditorContents) openInWorld.

"Make all contents always visible."
(TextParagraphMorph contents: Utilities defaultTextEditorContents) setHeightOnContent openInWorld.


"No automatic height adjust => no scrollbar!!"
| t s |
t := TextParagraphMorph contents: Utilities defaultTextEditorContents.
s := PluggableScrollPane new.
s scroller: t.
s openInWorld.

"Do automatic height adjust => include scrollbar as needed!!"
| t s |
t := TextParagraphMorph contents: Utilities defaultTextEditorContents.
s := PluggableScrollPane new.
s scroller: t setHeightOnContent.
s openInWorld.!


!TextParagraphMorph methodsFor: 'accessing' stamp: 'jmv 7/27/2023 12:03:29'!
setHeightOnContent
	"So we always adjust our own height to match contents.
	Usually when used in a PluggableScrollPane.
	Also useful for elements in a list."

	setsHeightOnContent := true! !


!TextParagraphMorph methodsFor: 'initialization' stamp: 'jmv 7/27/2023 12:04:42'!
initialize
	super initialize.
	extent := `1250@1140`.
	setsHeightOnContent := false.
	textComposition := TextComposition new.
	textComposition
		setModel: (TextModel withText: '');
		extentForComposing: self extentForComposing;
		composeAll.! !

!TextParagraphMorph methodsFor: 'geometry' stamp: 'jmv 7/27/2023 12:01:11'!
extentChanged: oldExtent
	"Our extent changed. Reflow text."

	| newHeight |
	super extentChanged: oldExtent.
	extent x = oldExtent x ifFalse: [
		textComposition
			initialize;
			extentForComposing: self extentForComposing;
			composeAll.
		setsHeightOnContent ifTrue: [
			newHeight := textComposition usedHeight + (4 * borderWidth)
					max: self minimumExtent y.
			extent y = newHeight ifFalse: [
				extent := extent x @ newHeight.
				self redrawNeeded.
				owner someSubmorphPositionOrExtentChanged ].
			(owner is: #ScrollPane) ifTrue: [
				owner
					updateScrollBarsBounds;
					setScrollDeltas ]]].! !

!TextParagraphMorph methodsFor: 'geometry' stamp: 'jmv 7/27/2023 12:01:23'!
privateExtent: aPoint
	| newExtent |

	newExtent := aPoint.
	setsHeightOnContent ifTrue: [
		"We decide our own height"
		newExtent := aPoint x truncated @ extent y ].
	^ super privateExtent: newExtent! !

!classDefinition: #TextParagraphMorph category: 'Morphic-Widgets'!
BoxedMorph subclass: #TextParagraphMorph
	instanceVariableNames: 'textComposition setsHeightOnContent'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
TextParagraphMorph allInstancesDo: [ :tpm | tpm instVarNamed: 'setsHeightOnContent' put: ( tpm owner is: #ScrollPane) ]!

