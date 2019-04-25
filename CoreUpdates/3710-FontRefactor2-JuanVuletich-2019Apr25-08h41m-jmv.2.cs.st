'From Cuis 5.0 of 7 November 2016 [latest update: #3708] on 25 April 2019 at 9:14:52 am'!

!Object methodsFor: 'private' stamp: 'jmv 4/24/2019 20:40:04'!
primitiveError: aString 
	"This method is called when the error handling results in a recursion in 
	calling on error: or halt or halt:."

	| context emergencyEvaluator lines r |
	r _ `10@10` extent: (Display extent -20 min: `700@1000`).
	lines _ r height // AbstractFont default lineSpacing.
	emergencyEvaluator _ Transcripter newInFrame: r.
	emergencyEvaluator
		nextPutAll: '***System error handling failed***'; newLine;
		nextPutAll: aString; newLine;
		nextPutAll: '-------------------------------'; newLine.
	context _ thisContext sender sender.
	(30 min: lines - 10) timesRepeat: [context ifNotNil: [emergencyEvaluator print: (context _ context sender); newLine]].
	emergencyEvaluator
		nextPutAll: '-------------------------------'; newLine;
		nextPutAll: 'Type ''revert'' to revert your last method change.'; newLine;
		nextPutAll: 'Type ''exit'' to exit the emergency evaluator.'; newLine.
	emergencyEvaluator readEvalPrint! !


!Transcripter methodsFor: 'accessing' stamp: 'jmv 4/24/2019 20:40:14'!
endEntry
	| c d cb |
	c _ self contents.
	Display extent ~= DisplayScreen actualScreenSize ifTrue: [
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
			`0@0` + (0@(d+AbstractFont default lineSpacing)).
		self on: (c copyFrom: cb stringIndex to: c size).
		readLimit _ position _ collection size.
		^ self endEntry].
	Display fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	Display getCanvas
		textComposition: textComposition
		bounds: (`4@4` + frame topLeft extent: Display extent)
		color: `Color black`
		selectionColor: `Color blue`.
	DisplayScreen screenUpdateRequired: nil! !


!ProgressInitiationException methodsFor: 'as yet unclassified' stamp: 'jmv 4/24/2019 20:59:12'!
defaultAction

	| delta textFrame barFrame outerFrame result range width filledWidth f h textWidth textForm innerBarFrame oldFilledWidth oldFilledWidth2 prevContents |
	f _ AbstractFont default.
	h _ f lineSpacing * 3//2.
	textWidth _ (f widthOfString: progressTitle) + h.
	width _ 150 max: textWidth.

	textForm _ Form extent: width@h depth: 32.
	textForm fillBlack.
	textForm fillWhite: (textForm boundingBox insetBy: 2).
	progressTitle displayOn: textForm at: (width-textWidth+h//2@4).
	barFrame _ Rectangle center: aPoint extent: width@h.
	textFrame _ `0@0` extent: width@h.
	textFrame _ textFrame
					aligned: textFrame bottomCenter
					with: barFrame topCenter + `0@2`.
	outerFrame _ barFrame merge: textFrame.
	delta _ outerFrame amountToTranslateWithin: Display boundingBox.
	barFrame _ barFrame translatedBy: delta.
	textFrame _ textFrame translatedBy: delta.
	outerFrame _ outerFrame translatedBy: delta.
	prevContents _ Form fromDisplay: outerFrame.
	range _ maxVal = minVal ifTrue: [1] ifFalse: [maxVal - minVal].  "Avoid div by 0"
	innerBarFrame _ barFrame insetBy: 2.
	textForm displayAt: textFrame topLeft.
	Display fillBlack: barFrame.
	Display fillWhite: innerBarFrame.
	oldFilledWidth _ 0.
	oldFilledWidth2 _ 0.
	result _ workBlock value:  "Supply the bar-update block for evaluation in the work block"
		[ :barVal |
		barVal
			ifNotNil: [ currentVal _ barVal ]
			ifNil: [
				currentVal _ currentVal + 1.
				currentVal >= maxVal
					ifTrue: [ currentVal _ minVal ]].
		filledWidth _ ((barFrame width-4) asFloat * ((currentVal-minVal) asFloat / range min: 1.0)) asInteger.
		filledWidth  > oldFilledWidth ifTrue: [
			textForm displayAt: textFrame topLeft.
			Display fillBlack: barFrame.
			Display fillWhite: innerBarFrame.
			Display fillGray: (barFrame topLeft + `2@2` extent: filledWidth@17).
			filledWidth -200 > oldFilledWidth2
				ifFalse: [
					"Usually just request an update, to be done asynchronously."
					DisplayScreen screenUpdateRequired: outerFrame ]
				ifTrue: [
					"Once in a while, force a real screen update (warning: really slow on MacOS if done too often)"
					Display forceToScreen: outerFrame. oldFilledWidth2 _ filledWidth ].
			oldFilledWidth _ filledWidth ]].
	prevContents displayAt: outerFrame topLeft.
	self resume: result! !


!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 4/24/2019 20:41:49'!
standardWindowExtent
	"Answer the standard default extent for new windows.  "

	| grid allowedArea maxLevel |
	"NOTE: following copied from strictlyStaggeredInitialFrameFor:"
	allowedArea _ self maximumUsableArea insetBy: (
		self scrollBarSetback @ self screenTopSetback extent: `0@0`
	).
	"Number to be staggered at each corner (less on small screens)"
	maxLevel _ allowedArea area > 700000 ifTrue: [3] ifFalse: [2].
	"Amount by which to stagger (less on small screens)"
	grid _ allowedArea area > 700000 ifTrue: [40] ifFalse: [20].
	^ (allowedArea extent - (grid*(maxLevel+1*2) + (grid//2))) min: `52@40` * Preferences standardCodeFont lineSpacing! !


!Transcript class methodsFor: 'displaying' stamp: 'jmv 4/24/2019 20:29:10'!
displayOn: aForm in: aRectangle
	"
	1 to: 2000 do: [ :i | i print ].
	1 to: 750 do: [ :i | i print ].
	Transcript displayOn: Display in: bounds
	"
	| font count string x y fh canvas innerR index |
	aForm fill: aRectangle fillColor: `Color white`.
	font _ AbstractFont default.

	innerR _ aRectangle insetBy: self padding.
	canvas _ aForm getCanvas.
	canvas newClipRect: innerR.
	x _ innerR left.
	y _ innerR top.
	fh _ font lineSpacing.
	count _ lastIndex - firstIndex \\ self maxEntries + 1 min: innerR height // fh - 1.
	index _ lastIndex - count \\ self maxEntries + 1.
	count timesRepeat: [
		string _ entries at: index.	
		canvas drawString: string at: x@y font: font color: `Color veryDarkGray`.
		y _ y + fh.
		index _ index \\ self maxEntries + 1 ].
	string _ unfinishedEntry contents.
	canvas drawString: string at: x@y font: font color: `Color veryDarkGray`! !

!Transcript class methodsFor: 'displaying' stamp: 'jmv 4/24/2019 20:42:17'!
displayUnfinishedEntryOn: aForm

	| font count string x y fh canvas r innerR |
	innerR _ bounds insetBy: self padding.
	lastDisplayPosition < innerR right ifTrue: [
		font _ AbstractFont default.
		fh _ font lineSpacing.
		count _ innerR height // fh-1.
		x _ innerR left.
		string _ unfinishedEntry contents.
		y _ ((lastIndex - firstIndex \\ self maxEntries) min: count-1) + 1 * font lineSpacing + innerR top.
		canvas _ aForm getCanvas.
		r _ innerR left: lastDisplayPosition.
		canvas newClipRect: r.
		(canvas drawString: string at: x@y font: font color: `Color veryDarkGray`) ifNotNil: [ :lastPoint |
			lastDisplayPosition _ lastPoint x.
			^r ]].
	^nil! !


!GrafPort methodsFor: 'text' stamp: 'jmv 4/24/2019 20:41:09'!
basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: font
	"Answer last affected pixel position"

	destY _ aPoint y.
	destX _ aPoint x.

	"the following are not really needed, but theBitBlt primitive will fail if not set"
	sourceX ifNil: [sourceX _ 100].
	width ifNil: [width _ 100].

	self primDisplayString: aString from: startIndex to: stopIndex
			map: font characterToGlyphMap xTable: font xTable
			kern: font baseKern.
	^ destX@(destY+font lineSpacing)! !

!GrafPort methodsFor: 'text' stamp: 'jmv 4/22/2019 13:06:08'!
displayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: aStrikeFont color: foregroundColor
	"If required, do a second pass with new rule and colorMap.
	Answer last affected pixel position
	Answer nil if nothing was done
	"

	| answer prevRule secondPassMap sourceDepth destDepth |

	"Slight optimization when there's nothing to do."
	clipHeight = 0 ifTrue: [^nil].
	clipWidth = 0 ifTrue: [^nil].

	self installStrikeFont: aStrikeFont foregroundColor: (foregroundColor alpha: 1).

	"If combinationRule is rgbMul, we might need the special two-pass technique for component alpha blending.
	If not, do it simply"
	combinationRule = 37 "rgbMul" ifFalse: [
		^self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: aStrikeFont ].
	
	"We need to do a second pass. The colormap set is for use in the second pass."
	secondPassMap _ colorMap.
	sourceDepth _ sourceForm depth.
	destDepth _ destForm depth.
	colorMap _ sourceDepth ~= destDepth
		ifTrue: [ Color cachedColormapFrom: sourceDepth to: destDepth ].
	answer := self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: aStrikeFont.
	colorMap := secondPassMap.
	secondPassMap ifNotNil: [
		prevRule := combinationRule.
		combinationRule := 20. "rgbAdd"
		self basicDisplayString: aString from: startIndex to: stopIndex at: aPoint strikeFont: aStrikeFont.
		combinationRule := prevRule ].
	^answer! !

!GrafPort methodsFor: 'private' stamp: 'jmv 4/25/2019 09:14:34'!
installStrikeFont: aStrikeFont foregroundColor: foregroundColor

	sourceForm _ aStrikeFont glyphs.
	sourceY _ 0.
	height _ sourceForm height.
	self setRuleAndMapFor: sourceForm depth foregroundColor: foregroundColor! !


!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 4/24/2019 21:03:12'!
label: aStringOrNil font: aFontOrNil
	"Label this button with the given string."
	label _ aStringOrNil.
	font _ aFontOrNil.
	(self fontToUse notNil and: [ label notNil ])
		ifTrue: [ "Add a bit of padding"
			extent := (self fontToUse widthOfString: label) + 10 @ (self fontToUse lineSpacing + 10) ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 4/24/2019 20:58:58'!
drawEmbossedLabelOn: aCanvas

	| availableW center colorForLabel f l labelMargin targetSize w x y |
	label ifNotNil: [
		colorForLabel _ Theme current buttonLabel.
		self isPressed
			ifFalse: [
				self mouseIsOver
					ifFalse: [ colorForLabel _ colorForLabel adjustSaturation: -0.10 brightness: 0.10 ]]
			ifTrue: [ colorForLabel _ colorForLabel adjustSaturation: 0.0 brightness: -0.07 ].
		f _ self fontToUse.
		center _ extent // 2.
		labelMargin _ 3.
		w _ f widthOfString: label.
		availableW _ extent x - labelMargin - labelMargin.
		availableW >= w
			ifTrue: [
				l _ label ]
			ifFalse: [
				x _ labelMargin.
				targetSize _ label size * availableW // w.
				l _ label squeezedTo: targetSize.
				(f widthOfString: l) > availableW ifTrue: [
					targetSize _ targetSize - 1.
					l _ label squeezedTo: targetSize ]].
		
		w _ f widthOfString: l.
		x _ center x - (w // 2).
		y _ center y - (f lineSpacing // 2).
		aCanvas
			drawString: l
			at: x@y
			font: f
			color: colorForLabel
			embossed: true ]! !

!PluggableButtonMorph methodsFor: 'drawing' stamp: 'jmv 4/24/2019 20:59:05'!
drawRegularLabelOn: aCanvas

	| w f center x y  availableW l labelMargin |

	f _ self fontToUse.
	center _ extent // 2.

	label ifNotNil: [
		labelMargin _ 4.
		w _ f widthOfString: label.
		availableW _ extent x - labelMargin - labelMargin - 1.
		availableW >= w
			ifTrue: [
				x _ center x - (w // 2).
				l _ label ]
			ifFalse: [
				x _ labelMargin.
				l _ label squeezedTo: (label size * availableW / w) rounded ].
		y _ center y - (f lineSpacing // 2).
		self isPressed ifTrue: [
			x _ x + 1.
			y _ y + 1 ].
		aCanvas
			drawString: l
			at: x@y
			font: f
			color: Theme current buttonLabel ]! !


!PluggableListMorph methodsFor: 'geometry' stamp: 'jmv 4/24/2019 20:41:36'!
scrollDeltaHeight
	"Return the increment in pixels which this pane should be scrolled."
	^ self font lineSpacing! !


!TextModelMorph methodsFor: 'geometry' stamp: 'jmv 4/24/2019 20:40:10'!
scrollDeltaHeight
	"Return the increment in pixels which this pane should be scrolled."

	^ AbstractFont default lineSpacing! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 4/24/2019 20:40:08'!
minPaneHeightForReframe
	^AbstractFont default lineSpacing + 10! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 4/24/2019 20:42:07'!
boxExtent
	"answer the extent to use in all the buttons. 
	 
	the label height is used to be proportional to the fonts preferences"
	| e |
	Theme current minimalWindows ifTrue: [^`0@0`].
	e _ Preferences windowTitleFont lineSpacing.
	^e@e! !


!CodePackageListWindow methodsFor: 'GUI building' stamp: 'jmv 4/24/2019 20:41:02'!
initialExtent

	^`540@400` * Preferences standardCodeFont lineSpacing // 14! !


!CodeWindow methodsFor: 'GUI building' stamp: 'jmv 4/24/2019 20:39:53'!
defaultAnnotationPaneHeight
	"Answer the receiver's preferred default height for new annotation panes."

	^ AbstractFont default lineSpacing * 2 + 8! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 4/24/2019 20:39:50'!
buildMorphicClassColumnWith: classList

	| column |
	
	column _ LayoutMorph newColumn.
	column
		addMorphUseAll: classList;
		addAdjusterAndMorph: self buildMorphicSwitches fixedHeight: (Theme current minimalWindows ifTrue: [AbstractFont default lineSpacing + 4] ifFalse: [AbstractFont default lineSpacing *2-4]).
		
	^column! !

!BrowserWindow methodsFor: 'GUI building' stamp: 'jmv 4/24/2019 20:40:51'!
buildNoSysCatMorphicWindow
	"A Browser without the class categories list"

	| mySingletonList upperPanes messageCatList messageList classColumn classList |
	mySingletonList _ PluggableListMorph
				model: model
				listGetter: #systemCategorySingleton
				indexGetter: #indexIsOne
				indexSetter: #indexIsOne:
				mainView: self
				menuGetter: #systemCatSingletonMenu
				keystrokeAction: #systemCatSingletonKey:from:.
	mySingletonList hideScrollBarsIndefinitely.

	classColumn _ self buildMorphicClassColumn.
	classList _ classColumn submorphs third.
	messageCatList _ self buildMorphicMessageCatList.
	messageList _ self buildMorphicMessageList.
	
	classList rightSibling: messageCatList.
	messageCatList leftSibling: classList rightSibling: messageList.
	messageList leftSibling: messageCatList.
	
	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: classColumn proportionalWidth: 0.3;
		addAdjusterAndMorph: messageCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: messageList proportionalWidth: 0.4.

	messageList makeItemsDraggable.
	messageCatList 
		acceptDropsFrom: messageList 
		performing: #categorizeUnderCategoryAt:selector: 
		whenOutsideList: #categorizeUnderNewCategorySelector:.

	self layoutMorph
		addMorph: mySingletonList fixedHeight: Preferences standardCodeFont lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.

	model changed: #editSelection! !


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 4/24/2019 20:40:59'!
buildMorphicWindow
	"Create a pluggable version of all the views for a Browser, using Morphic widgets."

	| list1 list2 upperPanes |
	model systemCategoryListIndex: 1.
	list1 _ PluggableListMorph
			model: model 
			listGetter: #systemCategorySingleton
			indexGetter: #indexIsOne 
			indexSetter: #indexIsOne:
			mainView: self
			menuGetter: #codeFileListMenu
			keystrokeAction: #codeFileListKey:from:.
	list1 hideScrollBarsIndefinitely.
	
	list2 _ PluggableListMorph
			model: model 
			listGetter: #messageCategoryList
			indexGetter: #messageCategoryListIndex 
			indexSetter: #messageCategoryListIndex:
			mainView: self
			menuGetter: #messageCategoryMenu
			keystrokeAction: nil.

	upperPanes _ LayoutMorph newRow.
	upperPanes
		addMorph: self buildMorphicClassColumn proportionalWidth: 0.3;
		addAdjusterAndMorph: list2 proportionalWidth: 0.3;
		addAdjusterAndMorph: self buildMorphicMessageList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: list1 fixedHeight: Preferences standardCodeFont lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.
	model changed: #editSelection! !


!ChangeSorterWindow methodsFor: 'GUI building' stamp: 'jmv 4/24/2019 20:40:56'!
initialExtent
	^`540@300` * Preferences standardCodeFont lineSpacing // 14! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 4/24/2019 20:41:45'!
initialExtent
	^ `640 @ 320` * Preferences standardCodeFont lineSpacing // 14! !


!InspectorWindow methodsFor: 'GUI building' stamp: 'jmv 4/24/2019 20:41:30'!
initialExtent

	^`600@325` * Preferences standardCodeFont lineSpacing // 14! !


!ObjectExplorerWindow methodsFor: 'GUI building' stamp: 'jmv 4/24/2019 20:41:33'!
initialExtent

	^`300@500` * Preferences standardCodeFont lineSpacing // 14! !


!AutoCompleterMorph class methodsFor: 'preferences' stamp: 'jmv 4/24/2019 20:40:46'!
itemHeight
	"height must be forced to be even to allow the detail arrow to be drawn correctly"	
	^ (self listFont lineSpacing + 2) roundUpTo: 2"14".! !


!StringMorph methodsFor: 'geometry' stamp: 'jmv 4/24/2019 20:45:28'!
measureContents
	| f |
	f _ self fontToUse.
	^((f widthOfString: contents) max: 3)  @ f lineSpacing! !


!LayoutMorph methodsFor: 'private' stamp: 'jmv 4/24/2019 20:40:01'!
minPaneHeightForReframe
	^AbstractFont default lineSpacing + 10! !


!TaskbarMorph methodsFor: 'initialization' stamp: 'jmv 4/24/2019 20:42:10'!
defaultHeight

	^ Preferences windowTitleFont lineSpacing * 2 * self scale! !


!InnerListMorph methodsFor: 'list management' stamp: 'jmv 4/24/2019 20:41:19'!
drawBoundsForRow: row
	"calculate the bounds that row should be drawn at.  This might be outside our bounds!!"

	self flag: #jmvVer2.
	"revisar senders"
	^ 0 @ (self drawYForRow: row) extent: extent x @ font lineSpacing! !

!InnerListMorph methodsFor: 'list management' stamp: 'jmv 4/24/2019 20:41:22'!
drawYForRow: row
	"calculate the vertical position that row should be drawn at.  This might be outside our bounds!!"
	^ row - 1 * font lineSpacing! !

!InnerListMorph methodsFor: 'list management' stamp: 'jmv 4/24/2019 20:41:25'!
rowAtLocation: aPoint
	"return the number of the row at aPoint"

	listItems isEmpty ifTrue: [ ^0 ].
	^aPoint y // font lineSpacing + 1 min: listItems size max: 1! !

!InnerListMorph methodsFor: 'list management' stamp: 'jmv 4/24/2019 20:41:27'!
rowAtLocation: aPoint ifNone: aNoneBlock

	| potentialRowNumber |
	
	potentialRowNumber := aPoint y // font lineSpacing + 1.
	
	^(listItems isInBounds: potentialRowNumber) 
		ifTrue: [ potentialRowNumber ]
		ifFalse: aNoneBlock! !

!InnerListMorph methodsFor: 'geometry' stamp: 'jmv 4/24/2019 20:41:16'!
adjustExtent
	"Adjust our height to match the underlying list,
	but make it wider if neccesary to fill the available width in our PluggableListMorph
	(this is needed to make the selection indicator no narrower than the list)"
	self morphExtent:
		self desiredWidth @ ((listItems size max: 1) * font lineSpacing)
! !


!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 4/24/2019 20:39:58'!
minimumExtent

	^(9@(AbstractFont default lineSpacing+2))! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 4/24/2019 20:38:42'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent newHeight newWidth |
	newWidth _ extent x.
	"Adjust width only if we don't wrap text to own width!!"
	wrapFlag ifFalse: [
		newWidth _ self textComposition usedWidth max: 9 ].
	newHeight _ self textComposition usedHeight max: AbstractFont default lineSpacing + 2.
	newExtent _ newWidth @ newHeight.
	extent = newExtent 
		ifTrue: [
			self redrawNeeded.
			"Too conservative: only text composition (because of changes in text or styles, etc)
			should cause invalidation.
			Try to avoid calling #fit unless needed."
		]
		ifFalse: [
			extent = newExtent ifFalse: [
				self redrawNeeded.
				extent _ newExtent.
				self someSubmorphPositionOrExtentChanged.
				owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
				self redrawNeeded ]].

	owner innerHeight: newExtent y! !


!FillInTheBlankMorph methodsFor: 'initialization' stamp: 'jmv 4/24/2019 20:39:55'!
sizeUnit
	^AbstractFont default lineSpacing! !


!TextComposer methodsFor: 'as yet unclassified' stamp: 'jmv 4/24/2019 20:59:37'!
addEmptyTrailingLine: isANewParagraph
	"The line to add is usually the first line of a new paragraph (if last char in text was newLine),
	but it can be a new line in same paragraph (if enough spaces ended last line)."
	| ts f h bs r lm rm w a leftMarginForAlignment s |
	s _ theText size+1.
	f _ editor
		ifNotNil: [ editor lastFont ]
		ifNil: [ theText fontAt: s default: self defaultFont ].
	ts _ editor
		ifNotNil: [ editor lastParagraphStyleOrNil ]
		ifNil: [ theText paragraphStyleOrNilAt: s].

	h _ f lineSpacing.
	bs _ f ascent.
	lm _ 0.
	rm _ 0.
	w _ extentForComposing x.
	a _ 0.
	ts ifNotNil: [
		isANewParagraph ifTrue: [
			h _ h + ts spaceBefore.
			bs _ bs + ts spaceBefore ].
		lm _ ((isANewParagraph and: [ ts isListStyle not ])
			ifTrue: [ ts firstIndent ]
			ifFalse: [ ts restIndent ]).
		rm _ ts rightIndent.
		a _ ts alignment ].
	
	leftMarginForAlignment _ a = CharacterScanner rightFlushCode
		ifTrue: [ w - rm]
		ifFalse: [
			a = CharacterScanner centeredCode
				ifTrue: [ (w - rm - lm) //2 + lm]
				ifFalse: [ lm ]].
	r _ leftMarginForAlignment @ currentY extent: 0@h.

	lines addLast: (
		EmptyLine new
			firstIndex: s lastIndex: s - 1;
			rectangle: r;
			lineHeight: h baseline: bs;
			paragraphStyle: ts)! !


!Theme methodsFor: 'other options' stamp: 'jmv 4/24/2019 20:42:14'!
buttonPaneHeight
	"Answer the user's preferred default height for button panes."

	^Preferences standardButtonFont lineSpacing * 14 // 8! !


!PopUpMenu methodsFor: 'accessing' stamp: 'jmv 4/24/2019 20:41:39'!
frameHeight
	"Designed to avoid the entire frame computation (includes MVC form),
	since the menu may well end up being displayed in Morphic anyway."
	| nItems |
	nItems _ 1 + labelString lineCount.
	^ (nItems * Preferences standardMenuFont lineSpacing) + 4 "border width"! !

!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 4/24/2019 20:41:42'!
startUpSegmented: segmentHeight withCaption: captionOrNil at: location allowKeyboard: aBoolean
	"This menu is too big to fit comfortably on the screen.
	Break it up into smaller chunks, and manage the relative indices.
	Inspired by a special-case solution by Reinier van Loon.  The boolean parameter indicates whether the menu should be given keyboard focus (if in morphic)"

"
(PopUpMenu labels: (String streamContents: [:s | 1 to: 100 do: [:i | s print: i; newLine]. s skip: -1])
		lines: (5 to: 100 by: 5)) startUpWithCaption: 'Give it a whirl...'.
"
	| nLines nLinesPer allLabels from to subset subLines index |
	allLabels := labelString lines.
	nLines _ allLabels size.
	lineArray ifNil: [lineArray _ Array new].
	nLinesPer _ segmentHeight // Preferences standardMenuFont lineSpacing - 5.
	from := 1.
	[ true ] whileTrue: [
		to := (from + nLinesPer) min: nLines.
		subset := (allLabels copyFrom: from to: to) asOrderedCollection.
		subset add: (to = nLines ifTrue: ['start over...'] ifFalse: ['more...'])
			before: subset first.
		subLines _ lineArray select: [:n | n >= from] thenCollect: [:n | n - (from-1) + 1].
		subLines _ (Array with: 1) , subLines.
		index := (PopUpMenu labels: subset printStringWithNewline lines: subLines)
					startUpWithCaption: captionOrNil at: location allowKeyboard: aBoolean.
		index = 1
			ifTrue: [from := to + 1.
					from > nLines ifTrue: [ from := 1 ]]
			ifFalse: [index = 0 ifTrue: [^ 0].
					^ from + index - 2]]! !

