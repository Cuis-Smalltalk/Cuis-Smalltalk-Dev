'From Cuis 6.0 [latest update: #5950] on 27 July 2023 at 12:18:59 pm'!

!TextParagraphMorph commentStamp: 'jmv 7/27/2023 12:09:43' prior: 0!
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


!TextParagraphMorph methodsFor: 'geometry' stamp: 'jmv 7/27/2023 12:17:37'!
reflowAndMaybeResize
	"Reflow text. If appropriate, adjust height or extent."

	| newHeight |
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
				setScrollDeltas ]].! !


!TextParagraphMorph methodsFor: 'geometry' stamp: 'jmv 7/27/2023 12:17:58'!
extentChanged: oldExtent
	"Our extent changed. Reflow text."

	super extentChanged: oldExtent.
	extent x = oldExtent x ifFalse: [
		self reflowAndMaybeResize ].! !

