'From Squeak3.7 of ''4 September 2004'' [latest update: #5989] on 6 November 2008 at 6:31:22 pm'!!classDefinition: #CharacterScanner category: #'Graphics-Text'!Object subclass: #CharacterScanner	instanceVariableNames: 'destX lastIndex xTable map destY stopConditions text textStyle alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth emphasisCode kern indentationLevel wantsColumnBreaks '	classVariableNames: 'DefaultStopConditions '	poolDictionaries: 'TextConstants'	category: 'Graphics-Text'!!classDefinition: #NewParagraph category: #'System-Text'!Object subclass: #NewParagraph	instanceVariableNames: 'text textStyle firstCharacterIndex container lines positionWhenComposed offsetToEnd maxRightX selectionStart selectionStop wantsColumnBreaks focused '	classVariableNames: ''	poolDictionaries: ''	category: 'System-Text'!!classDefinition: #OldTextComposer category: #'Morphic-OldText Support'!Object subclass: #OldTextComposer	instanceVariableNames: 'lines maxRightX currentY scanner possibleSlide nowSliding prevIndex prevLines currCharIndex startCharIndex stopCharIndex deltaCharIndex theText theContainer isFirstLine theTextStyle defaultLineHeight actualHeight wantsColumnBreaks '	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-OldText Support'!!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 11/6/2008 18:18'!setFont
	super setFont.
	stopConditions == DefaultStopConditions 
		ifTrue: [stopConditions := stopConditions copy].
	stopConditions at: Space asciiValue + 1 put: #space! !!NewParagraph methodsFor: 'composition' stamp: 'jmv 11/6/2008 18:27'!compose: t style: ts in: textContainer	text _ t.	textStyle _ ts.	offsetToEnd _ text size - 1.	container _ textContainer.	self composeAll! !!NewParagraph methodsFor: 'composition' stamp: 'jmv 11/6/2008 18:24'!composeAll	self composeLinesFrom: 1 to: text size delta: 0		into: OrderedCollection new priorLines: Array new atY: container top! !!NewParagraph methodsFor: 'composition' stamp: 'jmv 11/6/2008 18:16'!composeLinesFrom: start to: stop delta: delta into: lineColl priorLines: priorLines atY: startingY 
	"While the section from start to stop has changed, composition may ripple all the way to the end of the text.  However in a rectangular container, if we ever find a line beginning with the same character as before (ie corresponding to delta in the old lines), then we can just copy the old lines from there to the end of the container, with adjusted indices and y-values"

	| newResult |
	newResult := OldTextComposer new 
				composeLinesFrom: start
				to: stop
				delta: delta
				into: lineColl
				priorLines: priorLines
				atY: startingY
				textStyle: textStyle
				text: text
				container: container.
	lines := newResult first asArray.
	maxRightX := newResult second.
	^maxRightX! !!NewParagraph methodsFor: 'composition' stamp: 'jmv 11/6/2008 18:25'!testNewComposeAll
	| newResult |
	self 
		OLDcomposeLinesFrom: 1
		to: text size
		delta: 0
		into: OrderedCollection new
		priorLines: Array new
		atY: container top.
	newResult := OldTextComposer new 
				composeLinesFrom: 1
				to: text size
				delta: 0
				into: OrderedCollection new
				priorLines: Array new
				atY: container top
				textStyle: textStyle
				text: text
				container: container.
	newResult first with: lines
		do: [:e1 :e2 | e1 longPrintString = e2 longPrintString ifFalse: [self halt]].
	newResult second = maxRightX ifFalse: [self halt].
	^{ 
		newResult.
		{ 
			lines.
			maxRightX}}! !!NewParagraph methodsFor: 'selection' stamp: 'jmv 11/6/2008 18:25'!defaultCharacterBlock	^ (CharacterBlock new stringIndex: 1 text: text			topLeft: lines first topLeft extent: 0 @ 0)		textLine: lines first! !!OldTextComposer methodsFor: 'as yet unclassified' stamp: 'jmv 11/6/2008 18:15'!composeEachRectangleIn: rectangles 
	| myLine lastChar |
	1 to: rectangles size
		do: 
			[:i | 
			currCharIndex <= theText size ifFalse: [^false].
			myLine := scanner 
						composeFrom: currCharIndex
						inRectangle: (rectangles at: i)
						firstLine: isFirstLine
						leftSide: i = 1
						rightSide: i = rectangles size.
			lines addLast: myLine.
			actualHeight := actualHeight max: myLine lineHeight.	"includes font changes"
			currCharIndex := myLine last + 1.
			lastChar := theText at: myLine last.
			lastChar = Character cr ifTrue: [^#cr]].
	^false! !!OldTextComposer methodsFor: 'as yet unclassified' stamp: 'jmv 11/6/2008 18:17'!composeLinesFrom: argStart to: argStop delta: argDelta into: argLinesCollection priorLines: argPriorLines atY: argStartY textStyle: argTextStyle text: argText container: argContainer	lines _ argLinesCollection.	theTextStyle _ argTextStyle.	theText _ argText.	theContainer _ argContainer.	deltaCharIndex _ argDelta.	currCharIndex _ startCharIndex _ argStart.	stopCharIndex _ argStop.	prevLines _ argPriorLines.	currentY _ argStartY.	defaultLineHeight _ theTextStyle lineGrid.	maxRightX _ theContainer left.	possibleSlide _ stopCharIndex < theText size and: [theContainer isMemberOf: Rectangle].	nowSliding _ false.	prevIndex _ 1.	scanner _ CompositionScanner new text: theText textStyle: theTextStyle.	isFirstLine _ true.	self composeAllLines.	isFirstLine ifTrue: ["No space in container or empty text"		self 			addNullLineWithIndex: startCharIndex			andRectangle: (theContainer topLeft extent: 0@defaultLineHeight)	] ifFalse: [		self fixupLastLineIfCR	].	^{lines asArray. maxRightX}! !!OldTextComposer methodsFor: 'as yet unclassified' stamp: 'jmv 11/6/2008 18:15'!slideOneLineDown
	"Having detected the end of rippling recoposition, we are only sliding old lines"

	| priorLine |
	prevIndex < prevLines size 
		ifFalse: 
			["There are no more prevLines to slide."

			^nowSliding := possibleSlide := false].

	"Adjust and re-use previously composed line"
	prevIndex := prevIndex + 1.
	priorLine := (prevLines at: prevIndex) slideIndexBy: deltaCharIndex
				andMoveTopTo: currentY.
	lines addLast: priorLine.
	currentY := priorLine bottom.
	currCharIndex := priorLine last + 1! !!OldTextContainer methodsFor: 'private' stamp: 'jmv 11/6/2008 17:58'!computeShadow	| canvas back bounds |	bounds _ self bounds.	canvas _ (Display defaultCanvasClass extent: bounds extent depth: 1)			shadowColor: Color black.	canvas translateBy: bounds topLeft negated during:[:tempCanvas|		self fillsOwner			ifTrue: [tempCanvas fullDrawMorph: (textMorph owner copyWithoutSubmorph: textMorph)]			ifFalse: [tempCanvas fillRectangle: textMorph bounds color: Color black].		self avoidsOcclusions ifTrue:			[back _ tempCanvas form deepCopy.			tempCanvas form fillWhite.			textMorph owner submorphsInFrontOf: textMorph do:				[:m | tempCanvas fullDrawMorph: m].			back displayOn: tempCanvas form at: 0@0 rule: Form reverse].	].	shadowForm _ canvas form offset: bounds topLeft.	vertProfile _ shadowForm  yTallyPixelValue: 1 orNot: false.	rectangleCache _ Dictionary new.	^ shadowForm! !!OldTextMorph methodsFor: 'copying' stamp: 'jmv 11/6/2008 17:53'!veryDeepInner: deepCopier 	"Copy all of my instance variables. Some need to be not copied at all, but shared.	Warning!!!! Every instance variable defined in this class must be handled.	We must also implement veryDeepFixupWith:.  See DeepCopier class comment."	super veryDeepInner: deepCopier.	textStyle _ textStyle veryDeepCopyWith: deepCopier.	text _ text veryDeepCopyWith: deepCopier.	wrapFlag _ wrapFlag veryDeepCopyWith: deepCopier.	paragraph _ paragraph veryDeepCopyWith: deepCopier.	editor _ editor veryDeepCopyWith: deepCopier.	container _ container veryDeepCopyWith: deepCopier.	backgroundColor _ backgroundColor veryDeepCopyWith: deepCopier.	margins _ margins veryDeepCopyWith: deepCopier! !!OldTextMorph methodsFor: 'drawing' stamp: 'jmv 11/6/2008 18:09'!drawOn: aCanvas	"Draw the receiver on a canvas"	| fauxBounds |	self setDefaultContentsIfNil.	super drawOn: aCanvas.  "Border and background if any"	false ifTrue: [self debugDrawLineRectsOn: aCanvas].  "show line rects for debugging"	text size = 0		ifTrue: [self drawNullTextOn: aCanvas].	"Hack here:  The canvas expects bounds to carry the location of the text, but we also need to communicate clipping."	fauxBounds _ self bounds topLeft corner: self innerBounds bottomRight.	aCanvas paragraph: self paragraph bounds: fauxBounds color: color! !!OldTextMorph methodsFor: 'geometry' stamp: 'jmv 11/6/2008 18:10'!container	"Return the container for composing this text.  There are four cases:	1.  container is specified as, eg, an arbitrary shape,	2.  container is specified as the bound rectangle, because		this morph is linked to others,	3.  container is nil, and wrap is true -- grow downward as necessary,	4.  container is nil, and wrap is false -- grow in 2D as nexessary."	container ifNil: [		wrapFlag ifTrue: [^ self compositionRectangle withHeight: 9999999].		^ self compositionRectangle topLeft extent: 9999999@9999999].	^ container! !!OldTextMorph methodsFor: 'geometry testing' stamp: 'jmv 11/6/2008 18:09'!containsPoint: aPoint	(super containsPoint: aPoint) ifFalse: [^ false].  "Not in my bounds"	container ifNil: [^ true].  "In bounds of simple text"	text size = 0 ifTrue:		["make null text frame visible"		^ super containsPoint: aPoint].	"In complex text (non-rect container), test by line bounds"	^ self paragraph containsPoint: aPoint! !!OldTextMorph methodsFor: 'menu' stamp: 'jmv 11/6/2008 17:47'!addCustomMenuItems: aCustomMenu hand: aHandMorph 
	"Add text-related menu items to the menu"

	| outer |
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	aCustomMenu 
		addUpdating: #autoFitString
		target: self
		action: #autoFitOnOff.
	aCustomMenu 
		addUpdating: #wrapString
		target: self
		action: #wrapOnOff.
	aCustomMenu add: 'text margins...' translated action: #changeMargins:.
	(Preferences noviceMode or: [Preferences simpleMenus]) 
		ifFalse: 
			[aCustomMenu add: 'code pane menu...' translated
				action: #yellowButtonActivity.
			aCustomMenu add: 'code pane shift menu....' translated
				action: #shiftedYellowButtonActivity].
	outer := self owner.
	((outer isKindOf: OldPolygonMorph) and: [outer isOpen]) 
		ifTrue: 
			[container isNil 
				ifFalse: 
					[aCustomMenu add: 'reverse direction' translated
						action: #reverseCurveDirection.
					aCustomMenu add: 'set baseline' translated action: #setCurveBaseline:]]
		ifFalse: 
			[(container isNil or: [container fillsOwner not]) 
				ifTrue: 
					[aCustomMenu add: 'fill owner''s shape' translated action: #fillingOnOff]
				ifFalse: 
					[aCustomMenu add: 'rectangular bounds' translated action: #fillingOnOff].
			(container isNil or: [container avoidsOcclusions not]) 
				ifTrue: 
					[aCustomMenu add: 'avoid occlusions' translated action: #occlusionsOnOff]
				ifFalse: 
					[aCustomMenu add: 'ignore occlusions' translated action: #occlusionsOnOff]]! !!OldTextMorph methodsFor: 'private' stamp: 'jmv 11/6/2008 18:07'!composeToBounds	"Compose my text to fit my bounds.	If any text lies outside my bounds, it will be clipped, or	if I have successors, it will be shown in the successors."	self releaseParagraph; paragraph.	container ifNotNil:		[self privateBounds: container bounds truncated].	self paragraph positionWhenComposed: self position! !!OldTextMorph methodsFor: 'private' stamp: 'jmv 11/6/2008 18:11'!fit	"Adjust my bounds to fit the text.  Should be a no-op if autoFit is not specified.	Required after the text changes,	or if wrapFlag is true and the user attempts to change the extent."	| newExtent para cBounds lastOfLines heightOfLast |	self isAutoFit 		ifTrue: 			[newExtent := (self paragraph extent max: 9 @ textStyle lineGrid) + (0 @ 2).			newExtent := newExtent + (2 * borderWidth).			margins 				ifNotNil: [newExtent := ((0 @ 0 extent: newExtent) expandBy: margins) extent].			newExtent ~= bounds extent 				ifTrue: 					[container isNil 						ifTrue: 							[para := paragraph.	"Save para (layoutChanged smashes it)"							super extent: newExtent.							paragraph := para]].			container notNil				ifTrue: 					[cBounds := container bounds truncated.					"23 sept 2000 - try to allow vertical growth"					lastOfLines := self paragraph lines last.					heightOfLast := lastOfLines bottom - lastOfLines top.					(lastOfLines last < text size 						and: [lastOfLines bottom + heightOfLast >= self bottom]) 							ifTrue: 								[container releaseCachedState.								cBounds := cBounds origin corner: cBounds corner + (0 @ heightOfLast)].					self privateBounds: cBounds]].	"These statements should be pushed back into senders"	self paragraph positionWhenComposed: self position.	self changed	"Too conservative: only paragraph composition					should cause invalidation."! !!OldTextMorph methodsFor: 'private' stamp: 'jmv 11/6/2008 18:28'!paragraph	"Paragraph instantiation is lazy -- create it only when needed"	paragraph ifNotNil: [^ paragraph].self setProperty: #CreatingParagraph toValue: true.	self setDefaultContentsIfNil.	"...Code here to recreate the paragraph..."	paragraph _ (self paragraphClass new textOwner: self owner).	paragraph		compose: text		style: textStyle copy		in: self container.	wrapFlag ifFalse:		["Was given huge container at first... now adjust"		paragraph adjustRightX].	self fit.self removeProperty: #CreatingParagraph.	^ paragraph! !!OldTextMorph methodsFor: 'private' stamp: 'jmv 11/6/2008 17:53'!text: t textStyle: s wrap: wrap color: c	predecessor: pred successor: succ	"Private -- for use only in morphic duplication"	text _ t.	textStyle _ s.	wrapFlag _ wrap.	color _ c.	paragraph _ editor _ container _ nil! !!OldTextMorph methodsFor: 'private' stamp: 'jmv 11/6/2008 18:06'!updateFromParagraph	"A change has taken place in my paragraph, as a result of editing and I must be updated.  If a line break causes recomposition of the current paragraph, or it the selection has entered a different paragraph, then the current editor will be released, and must be reinstalled with the resulting new paragraph, while retaining any editor state, such as selection, undo state, and current typing emphasis."	"removed multiple lined paragraph support (predecessor and successor)"	| newStyle sel oldEditor |	paragraph ifNil: [^self].	wrapFlag ifNil: [wrapFlag := true].	editor ifNotNil: 			[oldEditor := editor.			sel := editor selectionInterval.			editor storeSelectionInParagraph].	text := paragraph text.	paragraph textStyle = textStyle 		ifTrue: [self fit]		ifFalse: [			newStyle := paragraph textStyle.			self 				text: text textStyle: newStyle;	"Set new style if any"				releaseParagraph;				"Force recomposition"				fit.								"and propagate the change"			editor ifNotNil: [self installEditorToReplace: editor]].	super layoutChanged.	sel ifNil: [^self].	editor ifNil: 			["Reinstate selection after, eg, style change"			self installEditorToReplace: oldEditor]! !OldTextMorph removeSelector: #addPredecessor:!OldTextMorph removeSelector: #addSuccessor:!OldTextMorph removeSelector: #adjustLineIndicesBy:!OldTextMorph removeSelector: #delete!OldTextMorph removeSelector: #firstCharacterIndex!OldTextMorph removeSelector: #firstInChain!OldTextMorph removeSelector: #isLinkedTo:!OldTextMorph removeSelector: #lastCharacterIndex!OldTextMorph removeSelector: #passKeyboardFocusTo:!OldTextMorph removeSelector: #predecessor!OldTextMorph removeSelector: #predecessor:successor:!OldTextMorph removeSelector: #predecessorChanged!OldTextMorph removeSelector: #recomposeChain!OldTextMorph removeSelector: #setPredecessor:!OldTextMorph removeSelector: #setSuccessor:!OldTextMorph removeSelector: #startingIndex!OldTextMorph removeSelector: #successor!OldTextMorph removeSelector: #veryDeepFixupWith:!OldTextMorph removeSelector: #withSuccessorsDo:!!OldTextMorph reorganize!('accessing' asText autoFit: backgroundColor backgroundColor: borderWidth: contents contents: contents:wrappedTo: contentsAsIs: contentsWrapped: crAction crAction: cursor editor isAutoFit isWrapped margins margins: newContents: text textColor textColor: textStyle userString wrapFlag:)('alignment' centered justified leftFlush rightFlush)('anchors' adjustTextAnchor: anchorMorph:at:type:)('caching' releaseCachedState)('change reporting' ownerChanged)('classification' isTextMorph)('containment' fillingOnOff occlusionsOnOff setContainer:)('copying' copy veryDeepInner:)('drawing' areasRemainingToFill: debugDrawLineRectsOn: drawNullTextOn: drawOn:)('editing' acceptContents acceptOnCR cancelEdits chooseAlignment chooseEmphasis chooseEmphasisOrAlignment chooseFont chooseStyle enterClickableRegion: handleEdit: handleInteraction:fromEvent: hasUnacceptedEdits: xeqLinkText:withParameter:)('event handling' handlesKeyboard: handlesMouseDown: hasFocus keyStroke: keyboardFocusChange: mouseDown: mouseMove: mouseUp: wouldAcceptKeyboardFocusUponTab)('events-processing' handleKeystroke: handleMouseMove:)('geometry' bounds container defaultLineHeight extent: minimumExtent privateMoveBy: textBounds)('geometry testing' containsPoint:)('initialization' beAllFont: defaultColor initialize setTextStyle: string:fontName:size: string:fontName:size:wrap:)('layout' acceptDroppingMorph:event:)('menu' addCustomMenuItems:hand: autoFitOnOff autoFitString changeMargins: reverseCurveDirection setCurveBaseline: shiftedYellowButtonActivity wrapOnOff wrapString yellowButtonActivity)('printing' fullPrintOn:)('scripting access' insertCharacters: insertContentsOf:)('submorphs-add/remove' addMorphFront:fromWorldPosition: goBehind)('testing' basicType)('visual properties' fillStyle fillStyle:)('private' clippingRectangle composeToBounds compositionRectangle fit installEditorToReplace: paragraph paragraphClass privateOwner: releaseEditor releaseParagraph releaseParagraphReally removedMorph: selectionChanged setDefaultContentsIfNil text:textStyle: text:textStyle:wrap:color:predecessor:successor: updateFromParagraph)('*connectors-accessing' fontName:size:)!OldTextComposer removeSelector: #composeLinesFrom:to:delta:into:priorLines:atY:textStyle:text:container:wantsColumnBreaks:!!classDefinition: #OldTextComposer category: #'Morphic-OldText Support'!Object subclass: #OldTextComposer	instanceVariableNames: 'lines maxRightX currentY scanner possibleSlide nowSliding prevIndex prevLines currCharIndex startCharIndex stopCharIndex deltaCharIndex theText theContainer isFirstLine theTextStyle defaultLineHeight actualHeight'	classVariableNames: ''	poolDictionaries: ''	category: 'Morphic-OldText Support'!NewParagraph removeSelector: #adjustLineIndicesBy:!NewParagraph removeSelector: #compose:style:from:in:!NewParagraph removeSelector: #composeAllStartingAt:!NewParagraph removeSelector: #firstCharacterIndex!NewParagraph removeSelector: #lastCharacterIndex!NewParagraph removeSelector: #wantsColumnBreaks!NewParagraph removeSelector: #wantsColumnBreaks:!!classDefinition: #NewParagraph category: #'System-Text'!Object subclass: #NewParagraph	instanceVariableNames: 'text textStyle container lines positionWhenComposed offsetToEnd maxRightX selectionStart selectionStop focused'	classVariableNames: ''	poolDictionaries: ''	category: 'System-Text'!CharacterScanner removeSelector: #wantsColumnBreaks:!!classDefinition: #CharacterScanner category: #'Graphics-Text'!Object subclass: #CharacterScanner	instanceVariableNames: 'destX lastIndex xTable map destY stopConditions text textStyle alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth emphasisCode kern indentationLevel'	classVariableNames: 'DefaultStopConditions'	poolDictionaries: 'TextConstants'	category: 'Graphics-Text'!