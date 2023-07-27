'From Cuis 6.0 [latest update: #5950] on 27 July 2023 at 11:37:10 am'!

!InnerTextMorph commentStamp: 'jmv 7/26/2023 19:20:15' prior: 0!
InnerTextMorphs support display of text with emphasis.  They also support reasonable text-editing capabilities, as well as embedded hot links, and the ability to embed submorphs in the text. They are 'bare' in the sense that they can not clip contents to some window, or scroll it by themselves.

Text display is composed and clipped to the innerBounds implied by borderWidth.

Comment about Shout specifics:
-----------------------------------------

In order to colour the text, I use an instance of SHTextStylerST80, which I store in my 'styler' instance variable.

When my setText: method is called, I use my styler to ...
	a) optionally set all assignments to ansi or leftArrow. 
	b) Colour my text (immediately, if the text is less than 4096 chars in length, or in a backgroundProcess otherwise)

	When my text is changed, my hasUnacceptedEdits: method is called with true, and I ask my styler to re-colour my text. This is performed in a background process so that typing remains responsive regardless of the length of the text.

	Just before my styler is about to format/style the text, I send #stylerAboutToStyle:  to my model. This gives my model a chance to veto the styling (by answering false), or to initialize the styler with information it needs in order to parse the text correctly (e.g. the class to which a method belongs, or the workspace in which I am contained).

	My styler informs me that it has finished styling by triggering the #shoutStyled event which I handle. I then update the textAttributes of my text and refresh the display.
 
	My 'unstyledAcceptText' instance variable is used in conjunction with my #acceptTextInModel and #correctFrom:to:with: methods to ensure that when my text is modified during a method compilation (removing unused vars etc), I do not lose those changes.!


!TextParagraphMorph commentStamp: 'jmv 7/26/2023 19:18:01' prior: 0!
TextParagraphMorph support display of text with emphasis. It can be used on its own, no need to include them in some kind of TextModelMorph / ScrollPane.

WordWrap is done and clipping is done to morph height. If the `owner is: #ScrollPane`, then height is automatically adjusted.
Support all features of Text, including fonts, sizes, emphasis and embedded morphs.

(TextParagraphMorph contents: 'Hello
World!!') openInWorld.


(TextParagraphMorph contents: Utilities defaultTextEditorContents) openInWorld.


| t s |
t := TextParagraphMorph contents: Utilities defaultTextEditorContents.
s := PluggableScrollPane new.
s scroller: t.
s openInWorld.!


!TextParagraphMorph methodsFor: 'geometry' stamp: 'jmv 7/26/2023 19:35:10'!
adjustExtent
	"Sent by owner if kind of PluggableScrollPane.
	This is just a suggestion. If we do wordwrap, the width will be honored.
	But the height is whatever is appropriate for the contents!!"

	self morphExtent: owner viewableExtent! !

!TextParagraphMorph methodsFor: 'geometry' stamp: 'jmv 7/27/2023 11:33:29'!
extentChanged: oldExtent
	"Our extent changed. Reflow text."

	| newHeight |
	super extentChanged: oldExtent.
	extent x = oldExtent x ifFalse: [
		textComposition
			initialize;
			extentForComposing: self extentForComposing;
			composeAll.
		(owner is: #ScrollPane) ifTrue: [
			newHeight := textComposition usedHeight + (4 * borderWidth)
					max: self minimumExtent y.
			extent y = newHeight ifFalse: [
				extent := extent x @ newHeight.
				self redrawNeeded.
				owner someSubmorphPositionOrExtentChanged ].
			owner
				updateScrollBarsBounds;
				setScrollDeltas ]].! !

!TextParagraphMorph methodsFor: 'geometry' stamp: 'jmv 7/27/2023 11:33:15'!
minimumExtent

	^3@1 * FontFamily defaultLineSpacing ceiling + 2! !

!methodRemoval: TextParagraphMorph #fit stamp: 'jmv 7/27/2023 11:31:04'!
TextParagraphMorph removeSelector: #fit!
