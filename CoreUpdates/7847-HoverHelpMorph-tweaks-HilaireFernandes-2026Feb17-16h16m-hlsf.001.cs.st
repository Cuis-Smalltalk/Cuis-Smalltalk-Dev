'From Cuis7.7 [latest update: #7839] on 17 February 2026 at 4:19:07 pm'!

!HoverHelpMorph methodsFor: 'accessing' stamp: 'hlsf 17/Feb/2026 16:18:43'!
contents: aStringOrText wrappedTo: wordWrapWidthOrNil
	"Do wordwrap as set up.
	If requested, honor wordWrapWidthOrNil or find a good default if nil.
	If not requested, make the morph as wide as needed"

	| contentsToCompose lastIsLineSeparator width |
	contents := aStringOrText.
	contents isEmptyOrNil ifTrue: [
		textComposition := nil.
		^self ].
	contentsToCompose := aStringOrText.
	lastIsLineSeparator := aStringOrText last isLineSeparator.
	lastIsLineSeparator ifTrue: [
		contentsToCompose := aStringOrText allButLast ].
	width := (doAutoWordWrap ifNil: [lastIsLineSeparator not])
		ifTrue: [
			wordWrapWidthOrNil ifNil: [
				FontFamily defaultLineSpacing * (aStringOrText size sqrt * 2.0 max: 13)]]
		ifFalse: [9999999].
	textComposition := TextComposition new.
	textComposition
		setModel: (TextModel withText: contentsToCompose asText);
		extentForComposing: width@9999999.
	textComposition composeAll.
	self morphExtent: textComposition usedExtent + 8.! !

!HoverHelpMorph methodsFor: 'drawing' stamp: 'hlsf 17/Feb/2026 16:17:54'!
drawOn: aCanvas

	| r |
	
	contents isEmptyOrNil ifTrue: [ ^self ].
	
	r := self localBounds.
	aCanvas roundRect: r color: color radius: 4.
	aCanvas
		drawTextComposition: textComposition
		at: `4@4`
		extent: extent - 8
		color: `Color black`
		selectionColor: (Theme current textHighlightFocused: false).! !

