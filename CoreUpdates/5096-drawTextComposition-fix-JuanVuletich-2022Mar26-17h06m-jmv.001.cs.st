'From Cuis 6.0 [latest update: #5095] on 26 March 2022 at 5:15:50 pm'!

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/26/2022 17:12:35'!
drawTextComposition: aTextComposition at: drawingPosition extent: anExtent color: c selectionColor: sc
	"strict boolean controls whether to avoid or include the last line, if it doesn't fully fit in visibleTextBounds"

	self drawTextComposition: aTextComposition at: drawingPosition extent: anExtent color: c selectionColor: sc avoidOverhang: false! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/26/2022 17:12:41'!
drawTextComposition: aTextComposition at: drawingPosition extent: anExtent color: c selectionColor: sc avoidOverhang: strict
	"strict boolean controls whether to avoid or include the last line, if it doesn't fully fit in visibleTextBounds"

	| displayScanner leftInRun line possiblyVisible firstLineIndex lastLineIndex |

	possiblyVisible _ currentTransformation boundsOfInverseTransformOf: self clipRect.
	possiblyVisible _ (possiblyVisible translatedBy: drawingPosition negated) intersect: (0@0 extent: anExtent).

	displayScanner _ MorphicScanner new
		defaultFont: aTextComposition defaultFont;
		text: aTextComposition textComposed
		foreground: c.
	displayScanner canvas: self.

	leftInRun _ 0.
	"Take clipRect into account. Extrememly fast scrolls and redraws of huge files (like .sources)"
	firstLineIndex _ aTextComposition lineIndexForPoint: (possiblyVisible topLeft max: `0@0`).
	lastLineIndex _ aTextComposition lineIndexForPoint: (possiblyVisible bottomRight min: anExtent).
	firstLineIndex
		to: lastLineIndex
		do: [ :i |
			line _ aTextComposition lines at: i.
			(strict not or: [ line top + line baseline < possiblyVisible bottom ]) ifTrue: [
				aTextComposition
					displaySelectionInLine: line
					on: self
					textTopLeft: drawingPosition
					selectionColor: sc.
				leftInRun _ displayScanner displayLine: line textTopLeft: drawingPosition leftInRun: leftInRun ]]! !


!Transcripter methodsFor: 'accessing' stamp: 'jmv 3/26/2022 17:09:42'!
endEntry
	| c d cb |
	c _ self contents.
	DisplayScreen isDisplayExtentOk ifFalse: [
		"Handle case of user resizing physical window"
		DisplayScreen startUp.
		frame _ frame intersect: Display boundingBox.
		^ self clear; show: c].
	textComposition
		setModel: (TextModel withText: c asText);
		extentForComposing: frame width-8 @9999.
	textComposition composeAll.
	d _ textComposition usedHeight - frame height.
	d > 0 ifTrue: [
		"Scroll up to keep all contents visible"
		cb _ textComposition characterBlockAtPoint:
			`0@0` + (0@(d+FontFamily defaultLineSpacing)).
		self on: (c copyFrom: cb stringIndex to: c size).
		readLimit _ position _ collection size.
		^ self endEntry].
	Display fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	Display getCanvas
		drawTextComposition: textComposition
		at: `4@4` + frame topLeft
		extent: Display extent
		color: `Color black`
		selectionColor: `Color blue`.
	DisplayScreen screenUpdateRequired: nil! !


!InnerTextMorph methodsFor: 'drawing' stamp: 'jmv 3/26/2022 17:10:07'!
drawOn: aCanvas
	"Draw the receiver on a canvas"

	false ifTrue: [ self debugDrawLineRectsOn: aCanvas ].  "show line rects for debugging"

	aCanvas
		drawTextComposition: self textComposition
		at: `0@0`
		extent: extent
		color: color
		selectionColor: (Theme current textHighlightFocused: self hasKeyboardFocus).

	model actualContents isEmpty ifTrue: [
		owner
			valueOfProperty: #emptyTextDisplayMessage
			ifPresentDo: [ :msg |
				aCanvas
					drawString: msg
					at: `0@0`
					font: nil
					color: Theme current textEmptyDisplayMessage ]].! !


!TextParagraphMorph methodsFor: 'drawing' stamp: 'jmv 3/26/2022 17:08:48'!
drawOn: aCanvas
	| b |
	b _ self morphLocalBounds insetBy: borderWidth * 2.
	aCanvas
		fillRectangle: self morphLocalBounds color: color.
	aCanvas
		drawTextComposition: textComposition
		at: b topLeft
		extent: b extent
		color: Theme current text
		selectionColor: `Color red`
		avoidOverhang: true.
	aCanvas
		frameRectangle: self morphLocalBounds
		color: borderColor
		borderWidth: borderWidth
		borderStyleSymbol: nil! !


!HoverHelpMorph methodsFor: 'drawing' stamp: 'jmv 3/26/2022 17:11:33'!
drawOn: aCanvas

	| r |
	r _ self morphLocalBounds.
	aCanvas roundRect: r color: self color radius: 4.
	aCanvas
		drawTextComposition: textComposition
		at: `4@4`
		extent: extent - 8
		color: `Color black`
		selectionColor: (Theme current textHighlightFocused: false).! !


!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/26/2022 17:14:13'!
textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc
	"Prefer #drawTextComposition:at:extent:..."

	self drawTextComposition: aTextComposition at: boundsRect topLeft extent: boundsRect extent color: c selectionColor: sc avoidOverhang: false! !

!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 3/26/2022 17:14:36'!
textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc avoidOverhang: strict
	"Prefer #drawTextComposition:at:extent:..."

	self drawTextComposition: aTextComposition at: boundsRect topLeft extent: boundsRect extent color: c selectionColor: sc avoidOverhang: strict! !

!methodRemoval: MorphicCanvas #textComposition:at:extent:color:selectionColor: stamp: 'jmv 3/26/2022 17:07:45'!
MorphicCanvas removeSelector: #textComposition:at:extent:color:selectionColor:!
!methodRemoval: MorphicCanvas #textComposition:at:extent:color:selectionColor:avoidOverhang: stamp: 'jmv 3/26/2022 17:07:52'!
MorphicCanvas removeSelector: #textComposition:at:extent:color:selectionColor:avoidOverhang:!
