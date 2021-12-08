'From Cuis 5.0 [latest update: #4962] on 1 November 2021 at 11:42:37 am'!
!classDefinition: #TextParagraphMorph category: 'Morphic-Widgets'!
WidgetMorph subclass: #TextParagraphMorph
	instanceVariableNames: 'textComposition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!TextParagraphMorph commentStamp: 'jmv 11/1/2021 09:58:10' prior: 0!
TextParagraphMorph support display of text with emphasis. It can be used on its own, no need to include them in some kind of TextModelMorph / ScrollPane.

Clipping to extent is done, and word wrap is optional. Support all features of Text, including fonts, sizes, emphasis and embedded morphs.

Text display is clipped to the innerBounds of the rectangle, and text composition is normally performed within a rectangle which is innerBounds inset by the margins parameter.


(TextParagraphMorph contents: 'Hello
World!!') openInWorld.


(TextParagraphMorph contents: Utilities defaultTextEditorContents) openInWorld.


| t s |
t := TextParagraphMorph contents: Utilities defaultTextEditorContents.
s := PluggableScrollPane new.
s scroller: t.
s openInWorld.!


!TextParagraphMorph methodsFor: 'accessing' stamp: 'hlsf 10/31/2021 19:52:05'!
contents: aStringOrText
	textComposition textComposed ~= aStringOrText ifTrue: [
		textComposition		
			setModel: (TextModel withText: aStringOrText);
			composeAll.
		extent _ textComposition usedExtent + 8]! !

!TextParagraphMorph methodsFor: 'initialization' stamp: 'jmv 11/1/2021 10:08:39'!
initialize
	super initialize.
	textComposition _ TextComposition new.
	textComposition
		setModel: (TextModel withText: '');
		extentForComposing: extent x @ 9999999;
		composeAll.! !

!TextParagraphMorph methodsFor: 'drawing' stamp: 'hlsf 10/31/2021 20:06:11'!
drawOn: aCanvas
	aCanvas
		textComposition: textComposition
		bounds: self morphLocalBounds 
		color: Theme current text
		selectionColor: `Color red`.! !

!TextParagraphMorph methodsFor: 'geometry' stamp: 'hlsf 11/1/2021 11:25:27'!
adjustExtent
	"This is just a suggestion. If we do wordwrap, the width will be honored.
	But the height is whatever is appropriate for the contents!! See #fit"
	self morphExtent: owner viewableExtent! !

!TextParagraphMorph methodsFor: 'geometry' stamp: 'hlsf 11/1/2021 11:29:52'!
extentChanged: oldExtent
	"Our extent changed. Reflow text."
	super extentChanged: oldExtent.
	extent x = oldExtent x ifFalse: [
	textComposition
		initialize;
		extentForComposing: extent x @ 9999999;
		composeAll.
	self fit.
	(owner is: #ScrollPane) ifTrue: [
		owner
			updateScrollBarsBounds;
			setScrollDeltas ]].! !

!TextParagraphMorph methodsFor: 'geometry' stamp: 'hlsf 11/1/2021 11:33:19'!
privateExtent: aPoint
	| newExtent |

	newExtent _ aPoint.
	(owner is: #ScrollPane) ifTrue: [
		"We decide our own height"
		newExtent _ aPoint x truncated @ extent y ].
	^ super privateExtent: newExtent! !

!TextParagraphMorph methodsFor: 'private' stamp: 'hlsf 11/1/2021 11:29:40'!
fit
	"Adjust my bounds to fit the text."

	| newExtent oldExtent |
	(owner is: #ScrollPane) ifFalse: [
		^self ].

	newExtent _ extent x @ (textComposition usedHeight max: FontFamily defaultLineSpacing + 2).
	extent = newExtent ifFalse: [
		oldExtent _ extent.
		extent _ newExtent.
		self extentChanged: oldExtent.
		owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
		self redrawNeeded ].! !


!TextParagraphMorph class methodsFor: 'instance creation' stamp: 'hlsf 10/31/2021 19:58:58'!
contents: aStringOrText
	^ self new 
		contents: aStringOrText ;
		yourself ! !


!InnerTextMorph methodsFor: 'private' stamp: 'jmv 11/1/2021 11:39:40'!
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

	(owner is: #ScrollPane) ifTrue: [
		owner innerHeight: newExtent y ].! !

!methodRemoval: Text #textSize stamp: 'jmv 11/1/2021 11:41:44'!
Text removeSelector: #textSize!
!methodRemoval: Text #actualContents stamp: 'jmv 11/1/2021 11:41:42'!
Text removeSelector: #actualContents!

!Text reorganize!
('accessing' append: at: at:put: commandForReplaceFrom:to:with: customizeExplorerContents embeddedMorphs embeddedMorphsFrom:to: findString:startingAt: findString:startingAt:caseSensitive: lineCount prepend: rangeOf:startingAt: replaceFrom:to:with: size string)
('comparing' = hash)
('copying' , appendToString: appendToText: copyFrom:to: copyReplaceFrom:to:with: copyReplaceTokens:with: postCopy)
('converting' asNumber asString asText displayStringOrText optimizedForMutationSpeed replaceFrom:to:with:startingAt: replaceFrom:to:withString:attributes:startingAt: reversed withCuisLineEndings)
('emphasis' addAttribute: addAttribute:from:to: alignmentAt: attributesAt: attributesFrom:to:do: characterStyleOrNilAt: characterStyleOrNilIfApplying: colorAt: commandForAddAttribute:from:to: commandForRemoveAttributes:from:to: emphasisAt: find: font: fontAt:default: fontIfApplying:default: initialFont: initialFontFrom: paragraphStyleOrNilAt: paragraphStyleOrNilIfApplying: removeAttributes:from:to: removeAttributesThat: runLengthFor: withAttributeValues:do:)
('testing' is: isRemote)
('printing' printOn: storeOn:)
('private' basicReplaceAttributesFrom:to:with: fixParagraphAttributesFrom:to: privateSetParagraphAttributes:from:to: privateSetRuns: runs setString:setRuns:)
('attributes' canJoin:and: hasAnyAttribute hasAnyParagraphAttribute)
('user interface' edit editLabel:)
('paragraph support' encompassLine: encompassParagraph: paragraphStyleChunksDo:)
('adding emphasis' allBold bold italic pointSize: struck sub super under)
('adding attributes' centered justified leftFlush rightFlush)
('adding color' black blue color: cyan gray green magenta red yellow)
('adding' add:)
!

!classRemoval: #ReadOnlyTextMorph stamp: 'jmv 11/1/2021 11:40:49'!
Smalltalk removeClassNamed: #ReadOnlyTextMorph!

