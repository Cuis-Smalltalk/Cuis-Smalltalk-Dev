'From Cuis 4.2 of 25 July 2013 [latest update: #1850] on 17 October 2013 at 5:10:44.707792 pm'!
!classDefinition: #MorphicScanner category: #'Graphics-Text'!
CharacterScanner subclass: #MorphicScanner
	instanceVariableNames: 'canvas lineY foregroundColor paragraphColor paraTopLeft ignoreColorChanges defaultColor topLeft '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!
!classDefinition: #TextEditor category: #'System-Text'!
Editor subclass: #TextEditor
	instanceVariableNames: 'model paragraph markBlock pointBlock emphasisHere lastParenLocation selectionStartBlocks selectionStopBlocks initialSelectionStart initialSelectionStop doWordSelection doParagraphSelection textComposition '
	classVariableNames: 'ChangeText FindText '
	poolDictionaries: ''
	category: 'System-Text'!
!classDefinition: #HoverHelpMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #HoverHelpMorph
	instanceVariableNames: 'contents paragraph textComposition '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!classDefinition: #InnerTextMorph category: #'Morphic-Views for Models'!
InnerPluggableMorph subclass: #InnerTextMorph
	instanceVariableNames: 'model wrapFlag paragraph editor pauseBlinking acceptOnCR hasUnacceptedEdits hasEditingConflicts askBeforeDiscardingEdits styler autoCompleter mutex textComposition '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
!classDefinition: #OneLineEditorMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #OneLineEditorMorph
	instanceVariableNames: 'font emphasis contents editor showCaret pauseBlinking caretRect keyboardFocusWatcher crAction showTextCursor textCursorRect '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!

!CharacterBlock commentStamp: 'jmv 10/16/2013 19:40' prior: 0!
My instances contain information about displayed characters. They are used to return the results of methods:
	TextComposition characterBlockAtPoint: aPoint and
	TextComposition characterBlockForIndex: stringIndex.
Any recomposition or movement of a TextComposition can make the instance obsolete.
My instances are effectively inmutable, as the only method settings instance variables is private and used just to build new instances.!

!classDefinition: #Transcripter category: #'Collections-Streams'!
ReadWriteStream subclass: #Transcripter
	instanceVariableNames: 'frame para textComposition '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!

!TextComposer commentStamp: 'jmv 10/16/2013 21:48' prior: 0!
Aux class for TextComposition. My instances are usually transient, and therefore don't need a TextModel, but just a Text with the current contents.!

!classDefinition: #TextComposition category: #'Morphic-Text Support'!
Object subclass: #TextComposition
	instanceVariableNames: 'model extentForComposing lines maxRightX selectionStartBlocks selectionStopBlocks editor showTextCursor lastTextCursorRect'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Text Support'!

!TextComposition commentStamp: '<historical>' prior: 0!
A TextComposition represents text that has been laid out, or composed, in some container.
(This class was formerly known as Paragraph).
	text 		A Text with encoded per-character emphasis.
	lines		An Array of TextLines comprising the final layout of the text
				after it has been composed within its container.
Lines are ordered vertically. However, for a given y, there may be several lines in left to right order. Lines must never be empty, even if text is empty.!


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 10/16/2013 21:02'!
displayBulletIfAppropriateFor: textLine textLeft: texLeft
	"texLeft is relative to the morph currently being drawn"

	| paragraphEnd count pattern |
	paragraphStyle ifNotNil: [
		(textLine isFirstLine and: [ paragraphStyle isListStyle ]) ifTrue: [
			pattern _ paragraphStyle listBulletPattern.
			"Count how many paragraphs before this one already used the pattern"
			count _ 0.
			paragraphEnd _ textLine first-1.
			[
			paragraphEnd > 0 and: [ ((text paragraphStyleOrNilAt: paragraphEnd) ifNotNil: [ :ps | ps listBulletPattern ]) = pattern ]] whileTrue: [
				count _ count + 1.
				paragraphEnd _ text string endOfParagraphBefore: paragraphEnd ].
			"Our number in the list, is one more than the count of previous contiguous paragraphs with this pattern"
			self
				displayBulletTextLeft: texLeft
				number: count + 1]]! !

!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 10/16/2013 21:02'!
displayBulletTextLeft: texLeft number: bulletNumber
	"texLeft is relative to the morph currently being drawn"

	| pattern i c j s bullet bulletPos bulletSize prefix |
	pattern _ paragraphStyle listBulletPattern.
	bullet _ pattern.
	(i _ pattern indexOf: $%) > 0
		ifTrue: [ bullet _ bulletNumber asString]
		ifFalse: [
			(i _ pattern indexOf: $z) > 0
				ifTrue: [ bullet _ (Character value: 96 + bulletNumber) asString ]
				ifFalse: [
					(i _ pattern indexOf: $Z) > 0
						ifTrue: [ bullet _ (Character value: 64 + bulletNumber) asString ]]].
	prefix _ 0.
	i > 0 ifTrue: [
		c _ pattern at: i.
		j _ i.
		s _ pattern size.
		[ j <= s and: [ (pattern at: j) = c ] ] whileTrue: [ j _ j + 1 ].
		j _ j - 1.
		bulletSize _ j-i+1.
		prefix _ bulletSize - bullet size max: 0.
		bullet size > bulletSize ifTrue: [
			bullet _ bullet copyFrom: bullet size - bulletSize + 1 to: bullet size ].
		bullet _ (pattern copyFrom: 1 to: i-1), bullet, (pattern copyFrom: j+1 to: pattern size) ].
	bulletPos _ paragraphStyle firstIndent + texLeft + ((font widthOf: $9) * prefix)@destY.
	canvas
		drawString: bullet
		from: 1
		to: bullet size
		at: bulletPos
		font: font
		color: foregroundColor
		kern: kern! !

!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 10/16/2013 21:02'!
displayLine: textLine textTopLeft: textTopLeft leftInRun: leftInRun
	"The call on the primitive (scanCharactersFrom:to:in:rightX:) will be interrupted according to an array of stop conditions passed to the scanner at which time the code to handle the stop condition is run and the call on the primitive continued until a stop condition returns true (which means the line has terminated).  leftInRun is the # of characters left to scan in the current run; when 0, it is time to call setStopConditions."

	"textTopLeft is relative to the morph currently being drawn"
	| stopCondition nowLeftInRun startIndex string lastPos priorFont |

	topLeft _ textTopLeft.
	line _ textLine.
	lineY _ line top + textTopLeft y.
	rightMargin _ line rightMargin + textTopLeft x.
	lastIndex _ line first.
	leftInRun <= 0 ifTrue: [
		self setFont.
		self setStopConditions ].
	leftMargin _ (line leftMarginForAlignment: alignment) + textTopLeft x.
	destX _ leftMargin.
	destY _ lineY + line baseline - font ascent.

	textLine isEmptyLine ifTrue: [
		textLine paragraphStyle ifNotNil: [ :ps |
			ps = paragraphStyle ifFalse: [
				foregroundColor _ defaultColor.
				priorFont _ font.
				self setActualFont: ps font.
				ps color ifNotNil: [ :color | self textColor: color ].
				alignment _ ps alignment.
				paragraphStyle _ ps.
				priorFont ifNotNil: [ destX _ destX + priorFont descentKern ].
				destX _ destX - font descentKern.
				kern _ 0 - font baseKern.
				spaceWidth _ font widthOf: Character space.
				xTable _ font xTable.
				map _ font characterToGlyphMap.
				self setStopConditions.
				text ifNotNil: [ destY _ lineY + line baseline - font ascent ]
			]
		].
		self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.
		^leftInRun ].

	self displayBulletIfAppropriateFor: textLine textLeft: textTopLeft x.

	lastIndex _ line first.
	leftInRun <= 0
		ifTrue: [nowLeftInRun _ text runLengthFor: lastIndex]
		ifFalse: [nowLeftInRun _ leftInRun].
	runStopIndex _ lastIndex + (nowLeftInRun - 1) min: line last.
	spaceCount _ 0.
	string _ text string.

	self placeEmbeddedObject.
	[
		startIndex _ lastIndex.
		lastPos _ destX@destY.
		stopCondition _ self
			scanCharactersFrom: lastIndex to: runStopIndex
			in: string rightX: rightMargin stopConditions: stopConditions
			kern: kern.
		lastIndex >= startIndex ifTrue: [
			canvas  
				drawString: string
				from: startIndex
				to: lastIndex
				at: lastPos
				font: font
				color: foregroundColor
				kern: kern ].
		"see setStopConditions for stopping conditions for displaying."
		(self perform: stopCondition) ifTrue: [
			"Number of characters remaining in the current run"
			^ runStopIndex - lastIndex ]
	] repeat.! !


!SimpleEditor methodsFor: 'accessing-selection' stamp: 'jmv 10/16/2013 22:17'!
hasSelection
	^markIndex ~= pointIndex! !


!TextEditor methodsFor: 'accessing-selection' stamp: 'jmv 10/16/2013 22:17'!
hasSelection
	^ markBlock ~= pointBlock! !

!TextEditor methodsFor: 'initialize-release' stamp: 'jmv 10/16/2013 20:10'!
textComposition: aTextComposition 
	"Install aTextComposition as the one to be edited by the receiver."

	textComposition _ aTextComposition! !

!TextEditor methodsFor: 'private' stamp: 'jmv 10/16/2013 20:10'!
storeSelectionInComposition
	"for proper display of selected text"
	textComposition
		selectionStartBlocks: (selectionStartBlocks copyWith: self startBlock)
		selectionStopBlocks: (selectionStopBlocks copyWith: self stopBlock)! !


!FormCanvas methodsFor: 'drawing-text' stamp: 'jmv 10/16/2013 20:40'!
textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc
	| displayScanner leftInRun line boundsInWorld tl |

	tl _ boundsRect topLeft.
	boundsInWorld _ currentTransformation displayBoundsOfTransformOf: boundsRect.
	self setPaintColor: c.

	displayScanner _ MorphicScanner new 
		text: aTextComposition textComposed
		foreground: (shadowColor ifNil: [ c ])
		ignoreColorChanges: self isShadowDrawing.
	displayScanner canvas: self.

	leftInRun _ 0.
	"Take clipRect into account. Extrememly fast scrolls and redraws of huge files (like .sources)"
	(aTextComposition lineIndexForPoint: (0@0 max: clipRect origin- boundsInWorld origin))
		to: (aTextComposition lineIndexForPoint: (boundsInWorld extent min: clipRect corner - boundsInWorld origin))
		do: [ :i |
			line _ aTextComposition lines at: i.
			aTextComposition
				displaySelectionInLine: line
				on: self
				textTopLeft: tl
				selectionColor: sc.
			leftInRun _ displayScanner displayLine: line textTopLeft: tl leftInRun: leftInRun  ]! !


!PasteUpMorph methodsFor: 'interaction loop' stamp: 'jmv 10/16/2013 22:00'!
doOneCycleNow
	"see the comment in doOneCycleNowFor:
	Only used for a few tests."
	worldState doOneCycleNowFor: self! !


!InnerTextMorph methodsFor: 'private' stamp: 'jmv 10/16/2013 20:04'!
installEditorAndTextComposition
	"Install an editor for my textComposition. Install also the textComposition."
	editor _ model editorClass new morph: self.
	editor model: model.

	"...Code here to recreate the TextComposition... We positively know it is nil."
	textComposition _ TextComposition new.
	textComposition
		setModel: model;
		extentForComposing: self extentForComposing.
	editor textComposition: textComposition.
	textComposition editor: editor.
	editor setEmphasisHereFromText.
	textComposition composeAll.
	editor resetState.
	self fit.
	self selectionChanged! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 10/16/2013 20:01'!
releaseEditorAndTextComposition
	"Editor and TextComposition instantiation is lazy -- they will be created only when needed"

	editor _ nil.
	textComposition _ nil! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 10/16/2013 20:02'!
resetTextComposition
	textComposition ifNotNil: [
		textComposition
			initialize;
			extentForComposing: self extentForComposing;
			composeAll.
		editor storeSelectionInComposition ].
	self fit.
	self selectionChanged.! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 10/16/2013 21:02'!
textComposition
	"textComposition instantiation is lazy -- create it only when needed"
	self mutex critical: [
		textComposition ifNil: [ self installEditorAndTextComposition ]].
	^textComposition! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 10/16/2013 20:02'!
updateFromTextComposition
	"A change has taken place in my textComposition, as a result of editing and I must be updated. "

	textComposition ifNotNil: [
		editor storeSelectionInComposition.
		self fit ].

	owner
		updateScrollBarsBounds;
		setScrollDeltas! !


!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 10/16/2013 22:37'!
displayTextCursorAtX: x top: top bottom: bottom emphasis: emphasis on: aCanvas
	| textCursorColor x1 isBold isItalic x0 h w halfW r d |
	isBold _ emphasis allMask: 1.
	isItalic _ emphasis allMask: 2.
	textCursorColor _ Theme current textCursor.
	h _ bottom - top.
	w _ isBold
		ifTrue: [ h // 25 + 2 ]
		ifFalse: [ h // 30 + 1 ].
	halfW _ w // 2.
	isItalic
		ifTrue: [	
			"Keep tweaking if needed!!"
			d _ isBold ifTrue: [ 3 ] ifFalse: [ h // 24].
			x0 _ x- (h*5//24) + d.
			x1 _ x + d ]
		ifFalse: [
			x0 _ x.
			x1 _ x].
	x0 < halfW ifTrue: [
		x1 _ x1 - x0 + halfW.
		x0 _ halfW ].
	r _ extent x-halfW-1.
	r < x1 ifTrue: [
		x0 _ x0 + r - x1.
		x1 _ r ].
	textCursorRect _ x0-halfW-1@ top corner: x1+halfW+1+1 @ bottom.
	aCanvas
		line: x0+halfW@bottom to: x1+halfW@(top+w)
		width: w color: textCursorColor! !

!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 10/16/2013 22:19'!
drawTextCursorOn: aCanvas
	|  bottom x |

	showTextCursor ifTrue: [
		bottom _ self baseFont height.
		x _ self fontToUse widthOfString: contents from: 1 to: editor startIndex-1.
		self displayTextCursorAtX: x top: 0 bottom: bottom emphasis: emphasis on: aCanvas ]! !

!OneLineEditorMorph methodsFor: 'testing' stamp: 'jmv 10/16/2013 22:18'!
hasTextCursor

	^ self hasKeyboardFocus and: [editor notNil and: [editor hasSelection not]]! !


!TextComposition methodsFor: 'access' stamp: 'jmv 10/16/2013 19:32'!
editor: anEditor
	"Can be nil.
	But if not nil, must be the same editor used in any TextMorph that references us."
	editor _ anEditor! !

!TextComposition methodsFor: 'access' stamp: 'jmv 10/16/2013 19:32'!
extent
	^ extentForComposing x @ (lines last bottom - lines first top)! !

!TextComposition methodsFor: 'access' stamp: 'jmv 10/16/2013 22:31'!
lastTextCursorRect
	"Warning: Could be not updated, it is the one for when the text cursor was last drawn."
	^lastTextCursorRect! !

!TextComposition methodsFor: 'access' stamp: 'jmv 10/16/2013 19:32'!
numberOfLines

	^lines size! !

!TextComposition methodsFor: 'access' stamp: 'jmv 10/16/2013 19:32'!
showTextCursor
	^showTextCursor! !

!TextComposition methodsFor: 'access' stamp: 'jmv 10/16/2013 22:31'!
showTextCursor: aBool
	showTextCursor _ aBool! !

!TextComposition methodsFor: 'access' stamp: 'jmv 10/16/2013 19:49'!
textComposed
	"Not named #text just to avoid false polyphormism"
	"The returned object should be treated as read-only, and never modified"
	^ model actualContents! !

!TextComposition methodsFor: 'access' stamp: 'jmv 10/16/2013 19:32'!
usedExtent
	^ maxRightX @ (lines last bottom - lines first top)! !

!TextComposition methodsFor: 'composition' stamp: 'jmv 10/16/2013 19:32'!
composeAll
	^self
		composeLinesFrom: 1
		to: model textSize
		delta: 0
		into: OrderedCollection new
		priorLines: Array new
		atY: 0! !

!TextComposition methodsFor: 'composition' stamp: 'jmv 10/16/2013 19:32'!
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
	maxRightX _ newResult second.
	^maxRightX! !

!TextComposition methodsFor: 'composition' stamp: 'jmv 10/16/2013 19:32'!
extentForComposing: aPoint
	extentForComposing _ aPoint! !

!TextComposition methodsFor: 'composition' stamp: 'jmv 10/16/2013 21:49'!
recomposeFrom: startArg to: stopArg delta: delta
	"Recompose this text.  The altered portion is between start and stop.
	Recomposition may continue to the end of the text, due to a ripple effect.
	Delta is the amount by which the current text is longer than it was
	when its current lines were composed.
	Expand the requested interval to include whole paragraphs because there could be paragraph attributes."
	| intervalToFix start stop startLine newLines |
	"Have to recompose line above in case a word-break was affected."
	intervalToFix _ model actualContents encompassParagraph: (startArg to: stopArg).
	start _ intervalToFix first.
	stop _ intervalToFix last.
	startLine _ (self lineIndexFor: start) - 1 max: 1.
	[startLine > 1 and: [(lines at: startLine-1) top = (lines at: startLine) top]]
		whileTrue: [startLine _ startLine - 1].  "Find leftmost of line pieces"
	newLines _ OrderedCollection new: lines size + 1.
	1 to: startLine-1 do: [:i | newLines addLast: (lines at: i)].
	self composeLinesFrom: (lines at: startLine) first to: stop delta: delta
			into: newLines priorLines: lines
			atY: (lines at: startLine) top! !

!TextComposition methodsFor: 'composition' stamp: 'jmv 10/16/2013 19:32'!
setModel: aTextModel
	model _ aTextModel! !

!TextComposition methodsFor: 'display' stamp: 'jmv 10/16/2013 21:49'!
displaySelectionInLine: line on: aCanvas textTopLeft: textTopLeft  selectionColor: sc

	"textTopLeft is relative to the morph currently being drawn"
	selectionStartBlocks with: selectionStopBlocks do: [ :startBlock :stopBlock |
		self
			displaySelectionStartBlock: startBlock
			stopBlock: stopBlock
			InLine: line
			on: aCanvas
			textTopLeft: textTopLeft
			selectionColor: sc ]! !

!TextComposition methodsFor: 'display' stamp: 'jmv 10/16/2013 22:33'!
displaySelectionStartBlock: startBlock stopBlock: stopBlock InLine: line on: aCanvas textTopLeft: textTopLeft  selectionColor: sc
	"textTopLeft is relative to the morph currently being drawn"

	| leftX rightX idx textCursorFont t b textCursorAttributes |

	startBlock ifNil: [^self].	"No selection"
	startBlock = stopBlock 
		ifTrue: [
			"Only show text cursor on line where clicked"
			startBlock textLine first = line first ifFalse: [
				^self ].
			showTextCursor ifTrue: [
				leftX _ textTopLeft x + startBlock left.
				idx _ startBlock stringIndex.
				textCursorAttributes _ editor ifNotNil: [ editor currentAttributes ].
				textCursorFont _ textCursorAttributes
					ifNil: [ model actualContents fontAt: idx ]
					ifNotNil: [ model actualContents fontIfApplying: textCursorAttributes ].
				b _ textTopLeft y + line top + line baseline + textCursorFont descent-1.
				t _ textTopLeft y + line top + line baseline - textCursorFont ascent.
				self
					displayTextCursorAtX: leftX
					top: t
					bottom: b
					emphasis: textCursorFont emphasis
					on: aCanvas
					textLeft: textTopLeft x ]]
		ifFalse: [
			"Test entire selection before or after here"
			(stopBlock stringIndex < line first 
				or: [startBlock stringIndex > (line last + 1)])
					ifTrue: [^self].	"No selection on this line"
			(stopBlock stringIndex = line first 
				and: [stopBlock textLine ~= line])
					ifTrue: [^self].	"Selection ends on line above"
			(startBlock stringIndex = (line last + 1) 
				and: [stopBlock textLine ~= line])
					ifTrue: [^self].
			lastTextCursorRect _ nil.
			leftX _  textTopLeft x + (startBlock stringIndex < line first 
				ifTrue: [ line ]
				ifFalse: [ startBlock ]) left.
			rightX _  textTopLeft x + ((stopBlock stringIndex > (line last + 1) or: [
					stopBlock stringIndex = (line last + 1) 
						and: [stopBlock textLine ~= line]]) 
				ifTrue: [line right]
				ifFalse: [stopBlock left]).
			aCanvas
				fillRectangle: (leftX @ (line top +  textTopLeft y) corner: rightX @ (line bottom +  textTopLeft y))
				color: sc ].	"Selection begins on line below"! !

!TextComposition methodsFor: 'display' stamp: 'jmv 10/16/2013 22:33'!
displayTextCursorAtX: x top: top bottom: bottom emphasis: emphasis on: aCanvas textLeft: textLeft
	"x, top, bottom, textLeft are relative to the morph currently being drawn."

	| textCursorColor x1 isBold isItalic x0 h w halfW r d |
	isBold _ emphasis allMask: 1.
	isItalic _ emphasis allMask: 2.
	textCursorColor _ Theme current textCursor.
	h _ bottom - top.
	w _ isBold
		ifTrue: [ h // 25 + 2 ]
		ifFalse: [ h // 30 + 1 ].
	halfW _ w // 2.
	isItalic
		ifTrue: [	
			"Keep tweaking if needed!! For italics with descenders (i.e. p), cursor shows a bit to the left..."
			d _ isBold ifTrue: [ h // 8 ] ifFalse: [ h // 9].
			x0 _ x- (h*5//24) + d.
			x1 _ x + d ]
		ifFalse: [
			x0 _ x - halfW.
			x1 _ x - halfW].
	x0-textLeft < -1 ifTrue: [
		x1 _ x1 - x0 + textLeft.
		x0 _ textLeft ].
	r _ extentForComposing x-halfW-1.
	r < (x1-textLeft) ifTrue: [
		x0 _ x0 + r - x1+textLeft.
		x1 _ r +textLeft].
	lastTextCursorRect _ isBold
		ifTrue: [ x0-halfW@ top corner: x1+halfW+3 @ (bottom+2) ]
		ifFalse: [ x0-halfW@ top corner: x1+halfW+2 @ (bottom+2) ].
	aCanvas
		line: x0+halfW@(bottom-w-w+3+halfW) to: x1+halfW@top
		width: w color: textCursorColor! !

!TextComposition methodsFor: 'editing' stamp: 'jmv 10/16/2013 19:32'!
clickAt: clickPoint
	"Give sensitive text a chance to fire.  Display flash: (100@100 extent: 100@100)."
	| startBlock action target range boxes box t |
	action _ false.
	startBlock _ self characterBlockAtPoint: clickPoint.
	t _ model actualContents.
	(t attributesAt: startBlock stringIndex) do: [ :att | 
		att mayActOnClick ifTrue:
				[(target _ model) ifNil: [ target _ editor morph].
				range _ t rangeOf: att startingAt: startBlock stringIndex.
				boxes _ self selectionRectsFrom: (self characterBlockForIndex: range first) 
							to: (self characterBlockForIndex: range last+1).
				box _ boxes detect: [ :each | each containsPoint: clickPoint] ifNone: nil.
				box ifNotNil: [
					box _ editor morph morphBoundsInWorld.
					editor morph allOwnersDo: [ :m | box _ box intersect: (m morphBoundsInWorld) ].
					Utilities
						awaitMouseUpIn: box
						repeating: nil
						ifSucceed: [(att actOnClickFor: target in: self at: clickPoint editor: editor) ifTrue: [action _ true]].
				]]].
	^ action! !

!TextComposition methodsFor: 'selection' stamp: 'jmv 10/16/2013 19:32'!
addSelectionRectsFrom: characterBlock1 to: characterBlock2 to: aStream
	"Return an array of rectangles representing the area between the two character blocks given as arguments."
	| line1 line2 rects cb1 cb2 w |
	characterBlock1 <= characterBlock2
		ifTrue: [cb1 _ characterBlock1.  cb2 _ characterBlock2]
		ifFalse: [cb2 _ characterBlock1.  cb1 _ characterBlock2].
	cb1 = cb2 ifTrue: [
		w _ 6.
		^ aStream nextPut: (cb1 topLeft - (w@0) corner: cb1 bottomLeft + ((w+1)@0))].
	line1 _ self lineIndexFor: cb1 stringIndex.
	line2 _ self lineIndexFor: cb2 stringIndex.
	line1 = line2 ifTrue: [
		^ aStream nextPut: (cb1 topLeft corner: cb2 bottomRight)].
	rects _ OrderedCollection new.
	rects addLast: (cb1 topLeft corner: (lines at: line1) bottomRight).
	line1+1 to: line2-1 do: [ :i |
		| line |
		line _ lines at: i.
		(line left = rects last left and: [ line right = rects last right ])
			ifTrue: [ "new line has same margins as old one -- merge them, so that the caller gets as few rectangles as possible"
					| lastRect |
					lastRect _ rects removeLast.
					rects add: (lastRect bottom: line bottom) ]
			ifFalse: [ "differing margins; cannot merge"
					rects add: line rectangle ] ].
	aStream nextPutAll: rects.
	aStream nextPut: ((lines at: line2) topLeft corner: cb2 bottomLeft)! !

!TextComposition methodsFor: 'selection' stamp: 'jmv 10/16/2013 19:32'!
characterBlockAtPoint: aPoint 
	"Answer a CharacterBlock for the character in the text at aPoint."
	| line |
	line _ lines at: (self lineIndexForPoint: aPoint).
	^ (CharacterBlockScanner new text: model actualContents)
		characterBlockAtPoint: aPoint index: nil
		in: line! !

!TextComposition methodsFor: 'selection' stamp: 'jmv 10/16/2013 19:32'!
characterBlockForIndex: index 
	"Answer a CharacterBlock for the character in text at index."
	| line t |
	line _ lines at: (self lineIndexFor: index).
	t _ model actualContents.
	^ (CharacterBlockScanner new text: t)
		characterBlockAtPoint: nil index: ((index max: line first) min: t size+1)
		in: line! !

!TextComposition methodsFor: 'selection' stamp: 'jmv 10/16/2013 19:32'!
defaultCharacterBlock
	^ CharacterBlock
		stringIndex: 1
		text: model actualContents
		topLeft: lines first topLeft
		extent: 0 @ 0
		textLine: lines first! !

!TextComposition methodsFor: 'selection' stamp: 'jmv 10/16/2013 19:32'!
selectionRects
	"Return an array of rectangles representing the selection regions."
	^ Array streamContents: [ :strm |
		selectionStartBlocks with: selectionStopBlocks do: [ :startBlock :stopBlock |
			self addSelectionRectsFrom: startBlock to: stopBlock to: strm ]]! !

!TextComposition methodsFor: 'selection' stamp: 'jmv 10/16/2013 19:32'!
selectionRectsFrom: characterBlock1 to: characterBlock2 
	"Return an array of rectangles representing the area between the two character blocks given as arguments."
	^ Array streamContents: [ :strm |
		self addSelectionRectsFrom: characterBlock1 to: characterBlock2 to: strm ]! !

!TextComposition methodsFor: 'selection' stamp: 'jmv 10/16/2013 19:32'!
selectionStartBlocks: startBlocks selectionStopBlocks: stopBlockBlocks
	selectionStartBlocks _ startBlocks.
	selectionStopBlocks _ stopBlockBlocks.! !

!TextComposition methodsFor: 'private' stamp: 'jmv 10/16/2013 19:32'!
fastFindFirstLineSuchThat: lineBlock
	"Perform a binary search of the lines array and return the index
	of the first element for which lineBlock evaluates as true.
	This assumes the condition is one that goes from false to true for
	increasing line numbers (as, eg, yval > somey or start char > somex).
	If lineBlock is not true for any element, return size+1."

	^lines
		findBinaryIndex: [ :each | 
			(lineBlock value: each)
				ifTrue: [ -1 ]
				ifFalse: [ 1 ] ]
		do: [ :found | found ]
		ifNone: [ :lower :upper | upper ]! !

!TextComposition methodsFor: 'private' stamp: 'jmv 10/16/2013 19:32'!
indentationOfLineIndex: lineIndex ifBlank: aBlock
	"Answer the number of leading tabs in the line at lineIndex.  If there are
	 no visible characters, pass the number of tabs to aBlock and return its value.
	 If the line is word-wrap overflow, back up a line and recur."

	| arrayIndex first last str |
	str _ model actualContents string.
	arrayIndex _ lineIndex.
	[
		first _ (lines at: arrayIndex) first.
		 first > 1 and: [(str at: first - 1) isLineSeparator not ] ] whileTrue: [ "word wrap"
			arrayIndex _ arrayIndex - 1].
	last _ (lines at: arrayIndex) last.
	
	^(str copyFrom: first to: last) indentationIfBlank: aBlock! !

!TextComposition methodsFor: 'private' stamp: 'jmv 10/16/2013 19:32'!
lastLine
	^lines last! !

!TextComposition methodsFor: 'private' stamp: 'jmv 10/16/2013 19:32'!
lineIndexFor: characterIndex
	"Answer the index of the line in which to select the character at index."
	^ (self fastFindFirstLineSuchThat: [:line | line first > characterIndex]) - 1 max: 1! !

!TextComposition methodsFor: 'private' stamp: 'jmv 10/16/2013 19:32'!
lineIndexForPoint: aPoint
	"Answer the index of the line in which to select the character nearest to aPoint."
	| i py |
	py _ aPoint y truncated.

	"Find the first line at this y-value"
	i _ (self fastFindFirstLineSuchThat: [ :line | line bottom > py]) min: lines size.

	"Now find the first line at this x-value"
	[ i < lines size and: [ (lines at: i+1) top = (lines at: i) top
				and: [ aPoint x >= (lines at: i+1) left ]]]
		whileTrue: [ i _ i + 1 ].
	^ i! !

!TextComposition methodsFor: 'private' stamp: 'jmv 10/16/2013 19:32'!
lines
	^ lines! !

!TextComposition methodsFor: 'initialization' stamp: 'jmv 10/16/2013 22:31'!
initialize
	showTextCursor _ false.
	selectionStartBlocks _ #().
	selectionStopBlocks _ #()! !


!Theme methodsFor: 'colors' stamp: 'jmv 10/16/2013 22:08'!
textCursor
	^ Display depth <= 2
		ifTrue: [ Color black ]
		ifFalse: [ self text ]! !


!Debugger methodsFor: 'private' stamp: 'jmv 10/16/2013 19:43'!
resetContext: aContext 
	"Used when a new context becomes top-of-stack, for instance when the
	method of the selected context is re-compiled, or the simulator steps or
	returns to a new method. There is room for much optimization here, first
	to save recomputing the whole stack list (and text), and secondly to avoid
	recomposing all that text (by editing the TextComposition instead of recreating it)."

	| oldContext |
	oldContext := self selectedContext.
	contextStackTop := aContext.
	self newStack: contextStackTop contextStack.
	self changed: #contextStackList.
	self contextStackIndex: 1 oldContextWas: oldContext.
	self acceptedContentsChanged.
! !


!CharacterBlockScanner methodsFor: 'stop conditions' stamp: 'jmv 10/16/2013 19:41'!
doNewLine
	"Answer a CharacterBlock that specifies the current location of the mouse 
	relative to a newLine stop condition that has just been 
	encountered. The convention is to denote selections by 
	CharacterBlocks, sometimes including the newLine character (cursor is at 
	the end) and sometimes not (cursor is in the middle of the text)."

	((characterIndex notNil
		and: [characterIndex > text size])
			or: [(line last = text size)
				and: [(destY + line lineHeight) < characterPoint y]])
		ifTrue: [
			"When off end of string, give data for next character"
			destY _ destY +  line lineHeight.
			characterPoint _ leftMargin @ destY.
			lastIndex _ lastIndex + 1.
			lastCharacterWidth _ 0.
			^ true ].
		characterPoint _ destX @ destY.
		lastCharacterWidth _ rightMargin - destX.
		^true! !


!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 10/16/2013 19:43'!
endOfRun
	"Answer true if scanning has reached the end of the TextComposition. 
	Otherwise step conditions (mostly install potential new font) and answer 
	false."

	| runLength |
	lastIndex = text size
		ifTrue:	[
			line stop: lastIndex.
			spaceX _ destX.
			line paddingWidth: rightMargin - destX - 1.
			^true ]
		ifFalse:	[
			runLength _ (text runLengthFor: (lastIndex _ lastIndex + 1)).
			runStopIndex _ lastIndex + (runLength - 1).
			self setFont.
			self setStopConditions.
			self placeEmbeddedObject.
			^false ]! !

!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 10/16/2013 19:43'!
space
	"Record left x and character index of the space character just encounted. 
	Used for wrap-around. Answer whether the character has crossed the 
	right edge of the composition rectangle of the TextComposition."
	spaceX _ destX.
	destX _ spaceX + spaceWidth.
	spaceIndex _ lastIndex.
	lineHeightAtSpace _ lineHeight.
	baselineAtSpace _ baseline.
	lastIndex _ lastIndex + 1.
	spaceCount _ spaceCount + 1.
	destX > rightMargin ifTrue: [ ^ self crossedX ].
	^ false! !

!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 10/16/2013 19:43'!
tab
	"Advance destination x according to tab settings in the current
	ParagraphStyle. Answer whether the character has crossed the right edge of 
	the composition rectangle of the TextComposition."

	destX _ self tabDestX.
	destX > rightMargin ifTrue:	[^self crossedX].
	lastIndex _ lastIndex + 1.
	^false
! !


!MorphicScanner methodsFor: 'scanning' stamp: 'jmv 10/16/2013 20:49'!
placeEmbeddedObject: anchoredFormOrMorph

	(super placeEmbeddedObject: anchoredFormOrMorph) ifFalse: [^ false].
	(anchoredFormOrMorph is: #Morph)
		ifTrue: [
			"This method should be redone calling reasonable protocol on the canvas.
			Also check use of global coordinates..."
			self flag: #jmvVer2.
			anchoredFormOrMorph morphPositionInWorld:
				((destX - anchoredFormOrMorph morphWidth)@
				(lineY+ line baseline - anchoredFormOrMorph morphHeight)) -
					topLeft ]
		ifFalse: [
			destY _ lineY.
			canvas
				image: anchoredFormOrMorph
				at: destX - anchoredFormOrMorph width @ (destY + line baseline - anchoredFormOrMorph height) ].
	^ true! !

!MorphicScanner methodsFor: 'private' stamp: 'jmv 10/16/2013 20:42'!
setFont 
	foregroundColor _ defaultColor.
	super setFont.  "Sets font and emphasis bits, and maybe foregroundColor"
	text ifNotNil: [ destY _ lineY + line baseline - font ascent ]! !

!MorphicScanner methodsFor: 'private' stamp: 'jmv 10/16/2013 20:42'!
text: t foreground: foreColor ignoreColorChanges: shadowMode
	text _ t.
	paragraphStyle _ nil.
	foregroundColor _ defaultColor _ foreColor.
	ignoreColorChanges _ shadowMode! !


!Text methodsFor: 'as yet unclassified' stamp: 'jmv 10/16/2013 21:46'!
paragraphStyleChunksDo: aBlock
	"Evaluate aBlock over each chunk (sequence of paragraphs) that have the same paragraphStyle"
	| start nextStart style |
	start _ 1.
	nextStart _ 1.
	[ start <= self size ] whileTrue: [
		style _ self paragraphStyleOrNilAt: start.
		[ nextStart <= self size and: [ (self paragraphStyleOrNilAt: nextStart) = style ]] whileTrue: [
			nextStart _ nextStart + 1 ].
		aBlock value: (start to: nextStart-1) value: style.
		start _ nextStart ]! !


!Editor methodsFor: 'menu messages' stamp: 'jmv 10/16/2013 22:10'!
lineSelectAndEmptyCheck: returnBlock
	"If the current selection is empty, expand it to be the entire current line; if after that's done the selection is still empty, then evaluate the returnBlock, which will typically consist of '[^ self]' in the caller -- check senders of this method to understand this."

	self selectLine.  "if current selection is empty, then first select the entire line in which occurs before proceeding"
	self hasSelection ifFalse: [morph flash.  ^ returnBlock value]! !

!Editor methodsFor: 'new selection' stamp: 'jmv 10/16/2013 22:37'!
deselectAndPlaceCursorAt: characterIndex 
	"Deselect, then place the text cursor before the character at characterIndex.
	 Be sure it is in view."

	self selectFrom: characterIndex to: characterIndex - 1! !

!Editor methodsFor: 'typing/selecting keys' stamp: 'jmv 10/16/2013 22:15'!
backspace: aKeyboardEvent
	"Backspace over the last character."
	"This is a user command, and generates undo"

	| startIndex |
	aKeyboardEvent shiftPressed ifTrue: [
		"^ self backWord: aKeyboardEvent keyCharacter"
		^self forwardDelete: aKeyboardEvent ].
	startIndex _ self markIndex + (self hasSelection ifTrue: [1] ifFalse: [0]).
	startIndex _ 1 max: startIndex - 1.
	self backTo: startIndex.
	^false! !

!Editor methodsFor: 'private' stamp: 'jmv 10/16/2013 19:46'!
beginningOfLine: position
	"Redefined in subclasses using TextComposition"
	^ self beginningOfParagraph: position! !

!Editor methodsFor: 'private' stamp: 'jmv 10/16/2013 19:47'!
endOfLine: position
	"Redefined in subclasses using TextComposition"
	^self endOfParagraph: position! !


!SimpleEditor methodsFor: 'initialize-release' stamp: 'jmv 10/16/2013 22:37'!
resetState 
	"Establish the initial conditions for editing the paragraph: place text cursor 
	before first character, set the emphasis to that of the first character,
	and save the paragraph for purposes of canceling."

	pointIndex _ 1.	"Like pointBlock in TextEditor"
	markIndex _ 1.	"Like markBlock in TextEditor"! !

!SimpleEditor methodsFor: 'menu messages' stamp: 'jmv 10/16/2013 22:37'!
copySelection
	"Copy the current selection and store it in the Clipboard, unless empty."

	self lineSelectAndEmptyCheck: [^ self].
	self clipboardTextPut: self selection! !

!SimpleEditor methodsFor: 'new selection' stamp: 'jmv 10/16/2013 22:10'!
selectLine
	"Make the receiver's selection, if currently empty, encompass the current line."
	self hasSelection ifTrue:[^self].
	self selectAll! !


!TextEditor methodsFor: 'accessing' stamp: 'jmv 10/16/2013 20:08'!
replaceSelectionWith: aTextOrString
	"Deselect, and replace the selection text by aText."
	"This is a user command, and generates undo"

	| start stop replacement |
	start _ self startIndex.
	stop _ self stopIndex.
	(aTextOrString isEmpty and: [stop > start]) ifTrue: [
		"If deleting, then set emphasisHere from 1st character of the deletion"
		emphasisHere _ (model actualContents attributesAt: start) select: [:att |
			att mayBeExtended]].

	(start = stop and: [ aTextOrString isEmpty ]) ifFalse: [
		replacement _ self addAttributesForPasting: aTextOrString.
		model logUndoAndReplaceFrom: start to: stop - 1 with: replacement.
		textComposition
			recomposeFrom: start
			to:  start + replacement size - 1
			delta: replacement size - (stop-start).
		self deselectAndPlaceCursorAt: start + replacement size.
		selectionStartBlocks _ #().
		selectionStopBlocks _ #().
		self userHasEdited  " -- note text now dirty" ].

	morph possiblyChanged! !

!TextEditor methodsFor: 'accessing-selection' stamp: 'jmv 10/16/2013 20:09'!
markIndex: anIndex
	"Called, for example, when selecting text with shift+arrow keys"
	markBlock _ (textComposition characterBlockForIndex: anIndex)
! !

!TextEditor methodsFor: 'accessing-selection' stamp: 'jmv 10/16/2013 20:10'!
markIndex: anIndex pointIndex: anotherIndex
	"Called, for example, when selecting text with shift+arrow keys"
	markBlock _ textComposition characterBlockForIndex: anIndex.
	pointBlock _ textComposition characterBlockForIndex: anotherIndex! !

!TextEditor methodsFor: 'accessing-selection' stamp: 'jmv 10/16/2013 20:09'!
pointIndex: anIndex
	"Called, for example, when selecting text with shift+arrow keys"
	pointBlock _ textComposition characterBlockForIndex: anIndex! !

!TextEditor methodsFor: 'accessing-selection' stamp: 'jmv 10/16/2013 21:50'!
selectionAsStream
	"Answer a ReadStream on the text that is currently  selected."

	^ReadWriteStream
		on: self privateCurrentString
		from: self startIndex
		to: self stopIndex - 1! !

!TextEditor methodsFor: 'as yet unclassified' stamp: 'jmv 10/16/2013 20:08'!
totalTextHeight

	^textComposition lines last bottom! !

!TextEditor methodsFor: 'attributes' stamp: 'jmv 10/16/2013 19:54'!
offerFontMenu
	"Present a menu of available fonts, and if one is chosen, apply it to the current selection.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	"This is a user command, and generates undo"

	| curFont newFont attr startIndex |
	startIndex _ self startIndex.
	curFont _ model actualContents fontAt: startIndex.
	newFont _ StrikeFont fromUser: curFont.
	newFont ifNil: [ ^self ].
	attr _ TextFontFamilyAndSize
			familyName: newFont familyName
			pointSize: newFont pointSize.
	self applyAttribute: attr.
	morph updateFromTextComposition.! !

!TextEditor methodsFor: 'current selection' stamp: 'jmv 10/16/2013 20:09'!
recomputeSelection
	"The same characters are selected but their coordinates may have changed."

	selectionStartBlocks _ selectionStartBlocks collect: [ :block | textComposition characterBlockForIndex: block stringIndex ].
	selectionStopBlocks _ selectionStopBlocks collect: [ :block | textComposition characterBlockForIndex: block stringIndex ].
	self markIndex: self markIndex pointIndex: self pointIndex! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 10/16/2013 22:38'!
inOutdent: aKeyboardEvent delta: delta
	"Add/remove a tab at the front of every line occupied by the selection. 
	Derived from work by Larry Tesler back in December 1985.  Now triggered by Cmd-L and Cmd-R.  2/29/96 sw"
	"This is a user command, and generates undo"

	| realStart realStop lines startLine stopLine start stop adjustStart indentation size numLines inStream newString outStream |

	"Operate on entire lines, but remember the real selection for re-highlighting later"
	realStart _ self startIndex.
	realStop _ self stopIndex - 1.

	"Special case: the text cursor on a line of its own, including weird case at end of paragraph"
	(realStart > realStop and: [
				realStart < 2 or: [(self privateCurrentString at: realStart - 1) isLineSeparator ]])
		ifTrue: [
			delta < 0
				ifTrue: [
					morph flash]
				ifFalse: [
					self replaceSelectionWith: Character tab asSymbol.
					self deselectAndPlaceCursorAt: realStart + 1].
			^true].

	lines _ textComposition lines.
	startLine _ textComposition lineIndexFor: realStart.
	stopLine _ textComposition lineIndexFor: (realStart max: realStop).
	start _ (lines at: startLine) first.
	stop _ (lines at: stopLine) last.
	
	"Pin the start of highlighting unless the selection starts a line"
	adjustStart _ realStart > start.

	"Find the indentation of the least-indented non-blank line; never outdent more"
	indentation _ (startLine to: stopLine) inject: 1000 into: [ :previousValue :each |
		previousValue min: (textComposition indentationOfLineIndex: each ifBlank: [ :tabs | 1000 ])].			

	size _  stop + 1 - start.
	numLines _ stopLine + 1 - startLine.
	inStream _ ReadStream on: self privateCurrentString from: start to: stop.

	newString _ String new: size + ((numLines * delta) max: 0).
	outStream _ ReadWriteStream on: newString.

	"This subroutine does the actual work"
	self indent: delta fromStream: inStream toStream: outStream.

	"Adjust the range that will be highlighted later"
	adjustStart ifTrue: [realStart _ (realStart + delta) max: start].
	realStop _ realStop + outStream position - size.

	"Prepare for another iteration"
	indentation _ indentation + delta.
	size _ outStream position.
	inStream _ outStream setFrom: 1 to: size.

	outStream
		ifNil: [ 	"tried to outdent but some line(s) were already left flush"
			morph flash]
		ifNotNil: [
			self selectInvisiblyFrom: start to: stop.
			size = newString size ifFalse: [newString _ outStream contents].
			self replaceSelectionWith: newString].
	self selectFrom: realStart to: realStop. 	"highlight only the original range"
	^ true! !

!TextEditor methodsFor: 'events' stamp: 'jmv 10/16/2013 20:08'!
mouseButton1Down: aMouseButtonEvent localPosition: localEventPosition
	| b |

	initialSelectionStart _ nil.
	initialSelectionStop _ nil.
	doWordSelection _ false.
	doParagraphSelection _ false.

	"Multiple selection of text.
	Windows uses Control, Mac uses Command (i.e. commandAlt)
	On the Mac, command-button1 is translated to command-button3 by the VM. do:
		Preferences disable: #commandClickOpensHalo
	to disable this behavior and make command-button1 work for multiple selection. "
	(aMouseButtonEvent controlKeyPressed or: [ aMouseButtonEvent commandAltKeyPressed ]) ifTrue: [
		self selectionInterval size > 0 ifTrue: [
			selectionStartBlocks _ selectionStartBlocks copyWith: self startBlock.
			selectionStopBlocks _ selectionStopBlocks copyWith: self stopBlock ]]
	ifFalse: [
		selectionStartBlocks _ #().
		selectionStopBlocks _ #() ].

	b _ textComposition characterBlockAtPoint: localEventPosition.

	(textComposition clickAt: localEventPosition) ifTrue: [
		markBlock _ b.
		pointBlock _ b.
		aMouseButtonEvent hand releaseKeyboardFocus: self.
		^ self ].
	
	aMouseButtonEvent shiftPressed
		ifFalse: [
			(self markIndex = b stringIndex and: [ self pointIndex = b stringIndex ])
				ifTrue: [
					markBlock _ b.
					pointBlock _ b ]
				ifFalse: [
					markBlock _ b.
					pointBlock _ b.	
					self setEmphasisHereFromText ]]! !

!TextEditor methodsFor: 'events' stamp: 'jmv 10/16/2013 20:10'!
mouseButton1Up: aMouseButtonEvent localPosition: localEventPosition
	| cursorBlock cursorIndex startBlock startIndex stopBlock stopIndex |

	aMouseButtonEvent shiftPressed
		ifTrue: [
			"Squeak classic behavior for click, move, shift-click sequence "
			"pointBlock _(textComposition characterBlockAtPoint: (evt eventPosition))."

			"Mac behavior"
			cursorBlock _ textComposition characterBlockAtPoint: localEventPosition.
			cursorIndex _ cursorBlock stringIndex.
			startBlock _ self startBlock min: cursorBlock.
			startIndex _ startBlock stringIndex.
			stopBlock _ self stopBlock max: cursorBlock.
			stopIndex _ stopBlock stringIndex.
			(stopIndex - cursorIndex) < (cursorIndex - startIndex)
				ifTrue: [
					markBlock _ startBlock.
					pointBlock _ cursorBlock ]
				ifFalse: [
					markBlock _ stopBlock.
					pointBlock _ cursorBlock ]].
	self storeSelectionInComposition! !

!TextEditor methodsFor: 'events' stamp: 'jmv 10/16/2013 20:08'!
mouseMove: aMouseMoveEvent localPosition: localEventPosition
	"Change the selection in response to mouse-down drag"

	| b interval i1 i2 |
	doWordSelection ifTrue: [
		pointBlock _ (textComposition characterBlockAtPoint: localEventPosition).
		self selectWordLeftDelimiters: '' rightDelimiters: ''.
		markBlock _ self startBlock min: initialSelectionStart.
		pointBlock _ self stopBlock max: initialSelectionStop.
		self storeSelectionInComposition.
		^self ].

	doParagraphSelection ifTrue: [
		b _ textComposition characterBlockAtPoint: localEventPosition.
		i1 _ b stringIndex min: initialSelectionStart stringIndex.
		i2 _ b stringIndex max: initialSelectionStop stringIndex-1.
		interval _ self privateCurrentString encompassParagraph: (i1 to: i2).
		self selectFrom: interval first to: interval last.
		markBlock _ self startBlock min: initialSelectionStart.
		pointBlock _ self stopBlock max: initialSelectionStop.
		self storeSelectionInComposition.
		^self ].

	pointBlock _ (textComposition characterBlockAtPoint: localEventPosition).
	self storeSelectionInComposition! !

!TextEditor methodsFor: 'initialize-release' stamp: 'jmv 10/16/2013 22:39'!
resetState 
	"Establish the initial conditions for editing the paragraph: place text cursor 
	before first character and set the emphasis to that of the first character"

	markBlock _ textComposition defaultCharacterBlock.
	pointBlock _ markBlock.
	self setEmphasisHereFromText.
	selectionStartBlocks _ #().
	selectionStopBlocks _ #()! !

!TextEditor methodsFor: 'menu messages' stamp: 'jmv 10/16/2013 19:54'!
chooseAlignment
	"This is a user command, and generates undo"

	| aList reply  |
	aList _ #(leftFlush centered justified rightFlush).
	reply _ (SelectionMenu labelList: aList selections: aList) startUp.
	reply ifNil: [ ^self ].
	self applyAttribute: (TextAlignment perform: reply).
	morph updateFromTextComposition.
	^ true! !

!TextEditor methodsFor: 'menu messages' stamp: 'jmv 10/16/2013 22:38'!
copySelection
	"Copy the current selection and store it in the Clipboard, unless empty."

	| multiSelection |
	self lineSelectAndEmptyCheck: [^ self].

	multiSelection _ self selection.
	self clipboardTextPut: multiSelection! !

!TextEditor methodsFor: 'menu messages' stamp: 'jmv 10/16/2013 21:50'!
cut
	"Cut out the current selection and redisplay if necessary."
	"This is a user command, and generates undo"

	| multiSelection |
	selectionStartBlocks ifEmpty: [
		self lineSelectAndEmptyCheck: [^ self]].

	multiSelection _ self selection.
	self zapMultiSelection.
	self replaceSelectionWith: self nullText.
	self clipboardTextPut: multiSelection.! !

!TextEditor methodsFor: 'menu messages' stamp: 'jmv 10/16/2013 22:16'!
setSearchString
	"Make the current selection, if any, be the current search string."
	self hasSelection ifFalse: [morph flash. ^ self].
	self setSearch: self selection string! !

!TextEditor methodsFor: 'new selection' stamp: 'jmv 10/16/2013 22:38'!
deselectAndPlaceCursorAt: characterIndex 
	"Deselect, then place the text cursor before the character at characterIndex.
	 Be sure it is in view."

	selectionStartBlocks _ #().
	selectionStopBlocks _ #().
	super deselectAndPlaceCursorAt: characterIndex! !

!TextEditor methodsFor: 'new selection' stamp: 'jmv 10/16/2013 22:12'!
lineSelectAndEmptyCheck: returnBlock
	"If the current selection is empty, expand it to be the entire current line; if after that's done the selection is still empty, then evaluate the returnBlock, which will typically consist of '[^ self]' in the caller -- check senders of this method to understand this."

	self selectLine.  "if current selection is empty, then first select the entire line in which occurs before proceeding"
	(self hasSelection or: [ selectionStartBlocks notEmpty]) ifFalse: [morph flash.  ^ returnBlock value]! !

!TextEditor methodsFor: 'new selection' stamp: 'jmv 10/16/2013 20:02'!
selectFrom: start to: stop
	"Select the specified characters inclusive.
	I.e. if contents is 'hello' and we want to select the $e, do 'self selectFrom: 2 to: 2'
	If we want to put cursor after e, but with no selection, do 'self selectFrom: 3 to: 2', or better yet, call #selectAt:"
	self selectInvisiblyFrom: start to: stop.
	self storeSelectionInComposition.
	morph redrawNeeded.
	"Preserve current emphasis if selection is empty"
	start > stop ifTrue: [
		self setEmphasisHereFromText ]! !

!TextEditor methodsFor: 'new selection' stamp: 'jmv 10/16/2013 22:10'!
selectLine
	"Make the receiver's selection, if currently empty, encompass the current line."
	(self hasSelection or: [ selectionStartBlocks notEmpty]) ifTrue:[^self].
	self selectInterval: (model actualContents encompassLine: self selectionInterval)! !

!TextEditor methodsFor: 'typing support' stamp: 'jmv 10/16/2013 20:02'!
processKeyStroke: aKeyboardEvent
	"Key struck on the keyboard. Find out which one and, if special, carry 
	out the associated special action. Otherwise, add the character to the 
	stream of characters."

	(self dispatchOn: aKeyboardEvent) ifTrue: [
		self storeSelectionInComposition.
		^self].

	markBlock _ pointBlock.
	self storeSelectionInComposition! !

!TextEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 10/16/2013 22:38'!
backWord: aKeyboardEvent
	"If the selection is not empty, delete it and leave it in the backspace buffer.
	 Else, delete the word before the text cursor."
	"This is a user command, and generates undo"

	| startIndex |
	self hasSelection ifFalse: [ "No selection, delete at least one character"
		startIndex _ 1 max: self markIndex - 1.
		[startIndex > 1 and:
			[(self privateCurrentString at: startIndex - 1) tokenish]]
				whileTrue: [
					startIndex _ startIndex - 1]]
	ifTrue: [ "some selection, just delete it"
		startIndex _ self markIndex].
	self backTo: startIndex.
	^false! !

!TextEditor methodsFor: 'private' stamp: 'jmv 10/16/2013 20:08'!
applyAttribute: aTextAttribute
	"The user selected aTextAttribute via shortcut, menu or other means.
	If there is a selection, apply the attribute to the selection.
	In any case use the attribute for the user input (emphasisHere)"
	"This generates undo"
	| anythingDone |

	anythingDone _ false.
	emphasisHere _ Text addAttribute: aTextAttribute toArray: emphasisHere.
	self selectionIntervalsDo: [ :interval |
		(interval notEmpty or: [ aTextAttribute isParagraphAttribute ])
			ifTrue: [
				anythingDone _ true.
				model logUndoAndAddAttribute: aTextAttribute from: interval first to: interval last.
				textComposition recomposeFrom: interval first to: interval last delta: 0 ]].
	anythingDone ifTrue: [
		self recomputeSelection.	"Needed so visible selection is updated to reflect new visual extent of selection"
		self userHasEdited ].

	"Even if nothing done, emphasisHere might have changed"
	morph possiblyChanged! !

!TextEditor methodsFor: 'private' stamp: 'jmv 10/16/2013 20:10'!
beginningOfLine: position
	"Redefined in subclasses using TextComposition"
	^ (textComposition lines at: (textComposition lineIndexFor: position)) first! !

!TextEditor methodsFor: 'private' stamp: 'jmv 10/16/2013 20:09'!
endOfLine: position
	"Redefined in subclasses using TextComposition"
	| targetLine |
	targetLine _ textComposition lines at: (textComposition lineIndexFor: position).
	^ targetLine = textComposition lastLine
		ifFalse: [ targetLine last ]
		ifTrue: [ targetLine last + 1 ]! !

!TextEditor methodsFor: 'private' stamp: 'jmv 10/16/2013 20:09'!
findAndReplaceOnce: indexStream
	"Find the next occurrence of FindText.  If none, answer false.
	Append the start index of the occurrence to the stream indices, and, if
	ChangeText is not the same object as FindText, replace the occurrence by it.
	Note that the search is case-sensitive for replacements, otherwise not."
	"This is a user command, and generates undo"

	| where |
	where _ model actualContents
		findString: FindText
		startingAt: self stopIndex
		caseSensitive: ((ChangeText ~~ FindText) or: [Preferences caseSensitiveFinds]).
	where = 0 ifTrue: [^ false].

	selectionStartBlocks add: (textComposition characterBlockForIndex: where).
	selectionStopBlocks add: (textComposition characterBlockForIndex: where + FindText size).
	self selectFrom: where to: where + FindText size - 1.	"Repeat it here. Senders beware: only one of these should last"

	ChangeText ~~ FindText ifTrue: [ self replaceSelectionWith: ChangeText ].
	indexStream nextPut: where.
	^ true! !

!TextEditor methodsFor: 'private' stamp: 'jmv 10/16/2013 22:39'!
isDisjointFrom: anInterval
	"Answer true if anInterval is empty and not touching or within the current
	 interval, or if anInterval is a not empty but it does not overlap the current
	 selection."

	| fudge |
	fudge _ anInterval size = 0 ifTrue: [1] ifFalse: [0].
	^(anInterval last + fudge < self startIndex or:
			[anInterval first - fudge >= self stopIndex])
! !

!TextEditor methodsFor: 'private' stamp: 'jmv 10/16/2013 20:09'!
pageHeight
	| howManyLines visibleHeight totalHeight ratio |
	howManyLines _ textComposition numberOfLines.
	visibleHeight _ self visibleHeight.
	totalHeight _ self totalTextHeight.
	ratio _ visibleHeight / totalHeight.
	^(ratio * howManyLines) rounded - 2! !

!TextEditor methodsFor: 'private' stamp: 'jmv 10/16/2013 20:09'!
sameColumn: start newLine: lineBlock forward: isForward
	"Private - Compute the index in my text
	with the line number derived from lineBlock,"
	" a one argument block accepting the old line number.
	The position inside the line will be preserved as good as possible"
	"The boolean isForward is used in the border case to determine if
	we should move to the beginning or the end of the line."
	| wordStyle column currentLine offsetAtTargetLine targetEOL lines numberOfLines currentLineNumber targetLineNumber |
	wordStyle _ Preferences wordStyleCursorMovement.
	wordStyle
		ifTrue: [
			lines _ textComposition lines.
			numberOfLines _ textComposition numberOfLines.
			currentLineNumber  _ textComposition lineIndexFor: start.
			currentLine _ lines at: currentLineNumber]
		ifFalse: [
			lines _ self lines.
			numberOfLines := lines size.
			currentLine _ lines
				detect:[:lineInterval | lineInterval last >= start]
				ifNone:[lines last].
			currentLineNumber _ currentLine second].
	column _ start - currentLine first.
	targetLineNumber _ ((lineBlock value: currentLineNumber) max: 1) min: numberOfLines.
	offsetAtTargetLine _ (lines at: targetLineNumber) first.
	targetEOL _ (lines at: targetLineNumber) last + (targetLineNumber = numberOfLines ifTrue:[1]ifFalse:[0]).
	targetLineNumber = currentLineNumber
	"No movement or movement failed. Move to beginning or end of line."
		ifTrue:[^isForward
			ifTrue:[targetEOL]
			ifFalse:[offsetAtTargetLine]].
	^offsetAtTargetLine + column min: targetEOL.! !

!TextEditor methodsFor: 'private' stamp: 'jmv 10/16/2013 20:08'!
unapplyAttribute: aTextAttribute
	"The user selected aTextAttribute to be removed.
	If there is a selection, unapply the attribute to the selection.
	In any case do not use the attribute for the user input (emphasisHere)"
	"This generates undo"

	| interval |
	emphasisHere _ emphasisHere copyWithout: aTextAttribute.
	interval _ self selectionInterval.
	(interval isEmpty and: [ aTextAttribute isParagraphAttribute not ])
		ifTrue: [ ^self ].
	model logUndoAndRemoveAttribute: aTextAttribute from: interval first to: interval last.
	textComposition recomposeFrom: interval first to: interval last delta: 0.
	self recomputeSelection.	"Needed so visible selection is updated to reflect new visual extent of selection"
	morph possiblyChanged! !

!TextEditor methodsFor: 'private' stamp: 'jmv 10/16/2013 20:09'!
zapMultiSelection
	"This generates undo"

	| delta intervals mi pi start stop miAndPiUpdated |
	miAndPiUpdated _ false.
	mi _ self markIndex.
	pi _ self pointIndex.
	delta _ 0.
	intervals _ (selectionStartBlocks with: selectionStopBlocks
		collect: [ :strt :stp | strt stringIndex to: stp stringIndex ])
			sorted: [ :a :b | a first < b first ].
	intervals do: [ :interval |
		miAndPiUpdated not ifTrue: [
			interval first > mi
				ifTrue: [
					miAndPiUpdated _ true.
					mi _ mi + delta.
					pi _ pi + delta ]
				ifFalse: [
					interval last > mi ifTrue: [
						miAndPiUpdated _ true.
						mi _ interval first + delta.
						pi _ mi ]]].
		start _  interval first+delta.
		stop _ interval last - 1 + delta.
		model logUndoAndReplaceFrom: start to: stop with: ''.
		delta _ delta - (interval size-1).
		textComposition
			recomposeFrom: start
			to:  start - 1
			delta: delta ].

	miAndPiUpdated not ifTrue: [
		mi _ mi + delta.
		pi _ pi + delta ].
	selectionStartBlocks _ #().
	selectionStopBlocks _ #().
	self markIndex: mi pointIndex: pi! !

!TextEditor methodsFor: 'commands' stamp: 'jmv 10/16/2013 20:08'!
insertMorph: aMorph at: relPt
	"This is a user command, and generates undo"
	| index newText |

	"Warning: As undo will only be done for text in the model, undoing this will neither
	kill the morph nor send it to another owner!! See implementors of #removeMorph: in TextMorphs.
	A specific undo for inserting / removing morphs might be in order. But I doubt the
	TextModel should be responsible for that."
	self flag: #jmv.

	index _ (textComposition characterBlockAtPoint: relPt) stringIndex.
	newText _ ' ', (Text string: '*' attribute: (TextAnchor new anchoredFormOrMorph: aMorph)), ' '.
	model logUndoAndReplaceFrom: index to: index-1 with: newText.
	textComposition recomposeFrom: index to: index + newText size -1 delta: newText size.! !

!TextEditor methodsFor: 'commands' stamp: 'jmv 10/16/2013 20:09'!
removeMorph: aMorph
	"This is a user command, and generates undo"

	| range |

	"Warning: As undo will only be done for text in the model, undoing this will neither
	recreate the morph nor add it to the TextMorph!! (See senders of #insertMorph:at:
	A specific undo for inserting / removing morphs might be in order. But I doubt the
	TextModel should be responsible for that."
	self flag: #jmv.

	range _ model actualContents find: (TextAnchor new anchoredFormOrMorph: aMorph).
	range ifNotNil: [
		model logUndoAndReplaceFrom: range first to: range last with: Text new.
		textComposition recomposeFrom: range first to: range first  -1 delta: range size negated ]! !

!TextEditor methodsFor: 'undo & redo' stamp: 'jmv 10/16/2013 20:07'!
redo
	model redoAndEvaluate: [ :modelUpdated :newCursorPos |
		newCursorPos
			ifNil: [ self recomputeSelection ]
			ifNotNil: [ self markIndex: newCursorPos pointIndex: newCursorPos ].
		modelUpdated ifTrue: [
			self userHasEdited.
			textComposition composeAll.			"this could be made more specific..."
			morph possiblyChanged ]]! !

!TextEditor methodsFor: 'undo & redo' stamp: 'jmv 10/16/2013 20:09'!
undo
	model undoAndEvaluate: [ :modelUpdated :newCursorPos |
		newCursorPos
			ifNil: [ self recomputeSelection ]
			ifNotNil: [ self markIndex: newCursorPos pointIndex: newCursorPos ].
		modelUpdated ifTrue: [
			self userHasEdited.
			textComposition composeAll.				"this could be made more specific..."
			morph possiblyChanged ]]! !


!SmalltalkEditor methodsFor: 'menu messages' stamp: 'jmv 10/16/2013 22:15'!
selectedSymbol
	"Return the currently selected symbol, or nil if none.  Spaces, tabs and returns are ignored"

	| aString |
	self hasSelection ifFalse: [^ nil].
	aString _ self selection string reject: [ :a | a isSeparator ].
	aString size = 0 ifTrue: [^ nil].
	Symbol hasInterned: aString  ifTrue: [:sym | ^ sym].

	^ nil! !

!SmalltalkEditor methodsFor: 'typing/selecting keys' stamp: 'jmv 10/16/2013 22:37'!
argAdvance: aKeyboardEvent
	"Invoked by Ctrl-a.  Useful after Ctrl-q.
	 Search forward from the end of the selection for a colon followed by
		a space.  Place the text cursor after the space.  If none are found, place the
		text cursor at the end of the text.."

	| start t |
	t _ model actualContents.
	start _ t findString: ': ' startingAt: self stopIndex.
	start = 0 ifTrue: [ start _ t size + 1].
	self deselectAndPlaceCursorAt: start + 2.
	^true! !


!TextModelMorph methodsFor: 'updating' stamp: 'jmv 10/16/2013 19:59'!
update: aSymbol
	super update: aSymbol.
	aSymbol ifNil: [^self].
	aSymbol == #flash ifTrue: [^self flash].
	aSymbol == #actualContents ifTrue: [
		"Some day, it would be nice to keep objects and update them
		instead of throwing them away all the time for no good reason..."
		self textMorph releaseEditorAndTextComposition.
		self textMorph formatAndStyleIfNeeded.
		self redrawNeeded.
		^self ].
	aSymbol == #acceptedContents ifTrue: [
		self textMorph hasUnacceptedEdits ifTrue: [
			self textMorph hasEditingConflicts: true.
			^self redrawNeeded ].
		model refetch.
		self setScrollDeltas.
		^self redrawNeeded ].
	aSymbol == #refetched ifTrue: [
		self setSelection: model getSelection.
		self hasUnacceptedEdits: false.
		^self ].
	aSymbol == #initialSelection ifTrue: [
		^self setSelection: model getSelection; redrawNeeded ].
	aSymbol == #autoSelect ifTrue: [
		TextEditor abandonChangeText.	"no replacement!!"
		self editor
			setSearch: model autoSelectString;
			findAndReplaceMany: true.
		self textMorph updateFromTextComposition.
		^self scrollSelectionIntoView ].
	"Quite ugly"
	aSymbol == #clearUserEdits ifTrue: [
		^self hasUnacceptedEdits: false].
	aSymbol == #shoutStyle ifTrue: [
		self textMorph stylerStyled.
		^self redrawNeeded ]! !


!HoverHelpMorph methodsFor: 'accessing' stamp: 'jmv 10/16/2013 19:50'!
contents: aString
	contents _ aString.
	textComposition _ TextComposition new.
	textComposition
		setModel: (TextModel withText: contents asText);
		extentForComposing: 9999999@9999999.
	textComposition composeAll.
	self morphExtent: textComposition usedExtent + 8! !

!HoverHelpMorph methodsFor: 'drawing' stamp: 'jmv 10/16/2013 19:50'!
drawOn: aCanvas

	| r |
	r _ 0@0 extent: extent.
	aCanvas roundRect: r color: self color radius: 4.
	aCanvas
		textComposition: textComposition
		bounds: (r insetBy: 4)
		color: Color black
		selectionColor: (Theme current textHighlightFocused: false)! !


!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 10/16/2013 20:00'!
editor
	"Return my current editor, or install a new one."
	self mutex critical: [
		editor ifNil: [ self installEditorAndTextComposition ]].
	^editor! !

!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 10/16/2013 19:59'!
model: aTextModel
	model _ aTextModel.
	styler ifNotNil: [ styler textModel: model ].
	self releaseEditorAndTextComposition.	"So the model is properly set on the editor and the text composition"! !

!InnerTextMorph methodsFor: 'accessing' stamp: 'jmv 10/16/2013 19:58'!
wrapFlag: aBoolean
	"Change whether contents are wrapped to the container."

	aBoolean == wrapFlag ifTrue: [^ self].
	wrapFlag _ aBoolean.

	"Compose my text to fit my bounds.
	If any text lies outside my bounds, it will be clipped,."
	self resetTextComposition! !

!InnerTextMorph methodsFor: 'caching' stamp: 'jmv 10/16/2013 19:59'!
releaseCachedState

	super releaseCachedState.
	self releaseEditorAndTextComposition.
! !

!InnerTextMorph methodsFor: 'drawing' stamp: 'jmv 10/16/2013 19:55'!
debugDrawLineRectsOn: aCanvas
	"Shows where text line rectangles are"

	self textComposition lines do: [ :line |
		aCanvas
			frameRectangle: line rectangle
			borderWidth: 1
			color: Color brown ]
! !

!InnerTextMorph methodsFor: 'drawing' stamp: 'jmv 10/16/2013 19:55'!
drawOn: aCanvas
	"Draw the receiver on a canvas"

	false ifTrue: [ self debugDrawLineRectsOn: aCanvas ].  "show line rects for debugging"

	aCanvas
		textComposition: self textComposition
		bounds: (0@0 extent: extent)
		color: color
		selectionColor: (Theme current textHighlightFocused: self hasKeyboardFocus)! !

!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 10/16/2013 19:59'!
cancelEdits
	"The message is sent when the user hits Cmd-L.
	Cancel the current contents and end editing."
	self releaseEditorAndTextComposition.
	model refetch! !

!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 10/16/2013 19:54'!
chooseEmphasisOrAlignment
	self editor changeEmphasisOrAlignment.
	self updateFromTextComposition! !

!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 10/16/2013 19:54'!
chooseFont
	self editor offerFontMenu.
	self updateFromTextComposition.! !

!InnerTextMorph methodsFor: 'editing' stamp: 'jmv 10/16/2013 21:01'!
handleInteraction: interactionBlock
	"Perform the changes in interactionBlock, noting any change in selection
	and possibly a change in the size of the composition"
	| oldSize |
	oldSize _ model textSize.
	self selectionChanged.  "Note old selection"

	interactionBlock value.

	self selectionChanged.  "Note new selection"
	oldSize = model textSize
		ifFalse: [ self updateFromTextComposition ]! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 10/16/2013 20:02'!
keyboardFocusChange: aBoolean
	
	"The message is sent to a morph when its keyboard focus changes.
	The given argument indicates that the receiver is gaining (versus losing) the keyboard focus.
	In this case, all we need to do is to redraw border feedback"
	aBoolean
		ifTrue: [
			"A hand is wanting to send us characters..."
			editor ifNil: [ self editor storeSelectionInComposition ].	"Forces install"
			self startBlinking ]
		ifFalse: [ self stopBlinking ].
	"Selection might be shown differently when focused"
	owner
		ifNotNil: [ owner redrawNeeded ]
		ifNil: [ self redrawNeeded ]		"Or at least redraw us"! !

!InnerTextMorph methodsFor: 'event handling' stamp: 'jmv 10/16/2013 19:54'!
processKeyStroke: evt
	| action |

	(acceptOnCR and: [evt isReturnKey])
		ifTrue: [^ self acceptContents].

	self pauseBlinking.
	evt isReturnKey ifTrue: [	"Return - check for special action"
		action _ self crAction.
		action ifNotNil: [
			^action value]].
	self handleInteraction: [ editor processKeyStroke: evt ].
	self updateFromTextComposition.

	self scrollSelectionIntoView! !

!InnerTextMorph methodsFor: 'geometry' stamp: 'jmv 10/16/2013 19:58'!
morphExtent: aPoint
	| newExtent |
	"Resist changing the extent if no wordwrap.. this should be checked."
	wrapFlag ifFalse: [ ^ self ].
	newExtent _ aPoint truncated max: self minimumExtent.
	
	"No change of wrap width"
	newExtent x = extent x ifTrue: [ ^ self ].

	super morphExtent: newExtent.
	
	self resetTextComposition.
	self editor recomputeSelection.	
	self updateFromTextComposition.! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 10/16/2013 19:55'!
fit
	"Adjust my bounds to fit the text.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent |
	newExtent _ (self textComposition extent max: 9 @ StrikeFont default height) + (0 @ 2).
	extent = newExtent ifFalse: [
		self basicExtent: newExtent ].

	self redrawNeeded.	"Too conservative: only text composition
							should cause invalidation."
	owner innerHeight: newExtent y! !

!InnerTextMorph methodsFor: 'private' stamp: 'jmv 10/16/2013 19:55'!
selectionChanged

	self textComposition selectionRects do: [:r | self invalidateRect: r ]! !

!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 10/16/2013 22:31'!
onBlinkCursor
	"Blink the cursor"
	textComposition ifNil: [ ^nil ].
	textComposition showTextCursor: textComposition showTextCursor not | pauseBlinking.
	pauseBlinking _ false.
	textComposition lastTextCursorRect ifNotNil: [ :r | self invalidateRect: r].! !

!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 10/16/2013 22:29'!
pauseBlinking
	"Show a solid cursor (non blinking) for a short while"
	pauseBlinking _ true.
	textComposition ifNotNil: [
		"Show cursor right now if needed"
		textComposition showTextCursor ifFalse: [
			textComposition showTextCursor: true ]]! !

!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 10/16/2013 22:29'!
startBlinking
	"And show the cursor"
	pauseBlinking _ true.
	"Start blinking in a short while"
	textComposition ifNotNil: [ textComposition showTextCursor: true ].
	self
		startStepping: #onBlinkCursor
		at: Time localMillisecondClock
		arguments: nil
		stepTime: 500! !

!InnerTextMorph methodsFor: 'blinking cursor' stamp: 'jmv 10/16/2013 22:29'!
stopBlinking
	"And do not show cursor anymore."
	self stopSteppingSelector: #onBlinkCursor.
	textComposition ifNotNil: [
		textComposition showTextCursor: false ]! !

!InnerTextMorph methodsFor: 'shout' stamp: 'jmv 10/16/2013 19:55'!
stylerStyled

	self textComposition composeAll.
	self editor recomputeSelection.	
	self updateFromTextComposition.
	self editor blinkParen.
	self scrollSelectionIntoView! !


!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 10/16/2013 22:20'!
onBlinkCursor
	"Blink the cursor"
	showTextCursor _ showTextCursor not | pauseBlinking.
	pauseBlinking _ false.
	textCursorRect ifNotNil: [ :r | self invalidateRect: r]! !

!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 10/16/2013 22:20'!
pauseBlinking
	"Show a solid cursor (non blinking) for a short while"
	pauseBlinking _ true.
	"Show cursor right now if needed"
	showTextCursor ifFalse: [
		showTextCursor _ true.
		textCursorRect ifNotNil: [ :r | self invalidateRect: r ]]! !

!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 10/16/2013 22:19'!
startBlinking
	"And show the cursor"
	pauseBlinking _ true.
	"Start blinking in a short while"
	showTextCursor _ true.
	self
		startStepping: #onBlinkCursor
		at: Time localMillisecondClock
		arguments: nil
		stepTime: 500! !

!OneLineEditorMorph methodsFor: 'blink cursor' stamp: 'jmv 10/16/2013 22:20'!
stopBlinking
	"And do not show cursor anymore."
	self stopSteppingSelector: #onBlinkCursor.
	"Hide cursor right now if needed"
	showTextCursor ifTrue: [
		showTextCursor _ false.
		textCursorRect ifNotNil: [ :r | self invalidateRect: r ]]! !

!OneLineEditorMorph methodsFor: 'drawing' stamp: 'jmv 10/16/2013 22:18'!
drawOn: aCanvas
	self hasSelection ifTrue: [ self drawSelectionOn: aCanvas ].
	self hasTextCursor ifTrue: [ self drawTextCursorOn: aCanvas ].
	aCanvas
		drawString: contents
		in: (0@0 extent: extent)
		font: self fontToUse
		color: color! !

!OneLineEditorMorph methodsFor: 'editing' stamp: 'jmv 10/16/2013 21:03'!
handleInteraction: interactionBlock
	"Perform the changes in interactionBlock, noting any change in selection
	and possibly a change in the size of the composition"

	"Also couple the editor to Morphic keyboard events"

	| oldEditor oldContents |
	oldEditor _ editor.
	oldContents _ contents.
	interactionBlock value.
	oldContents == contents 
		ifTrue: [ "this will not work if the composition changed"
			editor _ oldEditor	"since it may have been changed while in block"].
	self redrawNeeded! !

!OneLineEditorMorph methodsFor: 'initialization' stamp: 'jmv 10/16/2013 22:19'!
initialize
	super initialize.
	contents _ ''.
	font _ nil.
	emphasis _ 0.
	showTextCursor _ false. "Should never be nil"! !


!Transcripter methodsFor: 'initialization' stamp: 'jmv 10/16/2013 20:58'!
initInFrame: rect
	frame _ rect insetBy: 2.  "Leave room for border"
	textComposition _ TextComposition new.
	textComposition
		setModel: (TextModel withText: self contents asText);
		extentForComposing: frame width-8 @9999.
	textComposition composeAll! !

!Transcripter methodsFor: 'accessing' stamp: 'jmv 10/16/2013 20:59'!
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
	d _ textComposition extent y - frame height.
	d > 0 ifTrue: [
		"Scroll up to keep all contents visible"
		cb _ textComposition characterBlockAtPoint:
			0@0 + (0@(d+StrikeFont default height)).
		self on: (c copyFrom: cb stringIndex to: c size).
		readLimit _ position _ collection size.
		^ self endEntry].
	Display fill: (frame insetBy: -2) fillColor: self black;
			fill: frame fillColor: self white.
	Display getCanvas
		textComposition: textComposition
		bounds: (4@4 + frame topLeft extent: Display extent)
		color: Color black
		selectionColor: Color blue! !


!TextAttribute methodsFor: 'as yet unclassified' stamp: 'jmv 10/16/2013 21:48'!
actOnClickFor: model in: aTextComposition
	^self actOnClickFor: model! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: 'jmv 10/16/2013 21:47'!
actOnClickFor: model in: aTextComposition at: clickPoint
	^self actOnClickFor: model in: aTextComposition! !

!TextAttribute methodsFor: 'as yet unclassified' stamp: 'jmv 10/16/2013 21:47'!
actOnClickFor: model in: aTextComposition at: clickPoint editor: editor
	^self actOnClickFor: model in: aTextComposition at: clickPoint! !

!TextAttribute methodsFor: 'testing' stamp: 'jmv 10/16/2013 21:48'!
isParagraphAttribute
	"Attributes that answer true will always be applied to whole paragraphs, i.e. starting at the position after a newLine (or 1) and ending at a newLine (or text size)
	"
	^false! !


!TextAlignment methodsFor: 'testing' stamp: 'jmv 10/16/2013 21:47'!
isParagraphAttribute
	"Attributes that answer true will always be applied to whole paragraphs, i.e. starting at the position after a newLine (or 1) and ending at a newLine (or text size)
	"
	^true! !


!TextComposer methodsFor: 'as yet unclassified' stamp: 'jmv 10/16/2013 19:45'!
addEmptyTrailingLine: isANewParagraph
	"The line to add is usually the first line of a new paragraph (if last char in text was newLine),
	but it can be a new line in same paragraph (if enough spaces ended last line)."
	| ts f h bs r lm rm w a leftMarginForAlignment s |
	s _ theText size+1.
	f _ editor
		ifNotNil: [ editor lastFont ]
		ifNil: [ theText fontAt: s ].
	ts _ editor
		ifNotNil: [ editor lastParagraphStyleOrNil ]
		ifNil: [ theText paragraphStyleOrNilAt: s].

	h _ f height.
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

!methodRemoval: Theme #insertionPoint!
Theme removeSelector: #insertionPoint!
!classDefinition: #Transcripter category: #'Collections-Streams'!
ReadWriteStream subclass: #Transcripter
	instanceVariableNames: 'frame textComposition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Streams'!
!methodRemoval: OneLineEditorMorph #displayInsertionMarkAtX:top:bottom:emphasis:on:!
OneLineEditorMorph removeSelector: #displayInsertionMarkAtX:top:bottom:emphasis:on:!
!methodRemoval: OneLineEditorMorph #drawCaretOn:!
OneLineEditorMorph removeSelector: #drawCaretOn:!
!methodRemoval: OneLineEditorMorph #hasVisibleCaret!
OneLineEditorMorph removeSelector: #hasVisibleCaret!
!classDefinition: #OneLineEditorMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #OneLineEditorMorph
	instanceVariableNames: 'font emphasis contents editor showTextCursor pauseBlinking textCursorRect keyboardFocusWatcher crAction'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!methodRemoval: InnerTextMorph #installEditorAndParagraph!
InnerTextMorph removeSelector: #installEditorAndParagraph!
!methodRemoval: InnerTextMorph #paragraph!
InnerTextMorph removeSelector: #paragraph!
!methodRemoval: InnerTextMorph #releaseEditorAndParagraph!
InnerTextMorph removeSelector: #releaseEditorAndParagraph!
!methodRemoval: InnerTextMorph #resetParagraph!
InnerTextMorph removeSelector: #resetParagraph!
!methodRemoval: InnerTextMorph #updateFromParagraph!
InnerTextMorph removeSelector: #updateFromParagraph!
!classDefinition: #InnerTextMorph category: #'Morphic-Views for Models'!
InnerPluggableMorph subclass: #InnerTextMorph
	instanceVariableNames: 'model wrapFlag textComposition editor pauseBlinking acceptOnCR hasUnacceptedEdits hasEditingConflicts askBeforeDiscardingEdits styler autoCompleter mutex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Views for Models'!
!classDefinition: #HoverHelpMorph category: #'Morphic-Widgets'!
RectangleLikeMorph subclass: #HoverHelpMorph
	instanceVariableNames: 'contents textComposition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Widgets'!
!methodRemoval: FormCanvas #paragraph:bounds:color:selectionColor:!
FormCanvas removeSelector: #paragraph:bounds:color:selectionColor:!
!methodRemoval: TextEditor #hasCaret!
TextEditor removeSelector: #hasCaret!
!methodRemoval: TextEditor #paragraph:!
TextEditor removeSelector: #paragraph:!
!methodRemoval: TextEditor #storeSelectionInParagraph!
TextEditor removeSelector: #storeSelectionInParagraph!
!classDefinition: #TextEditor category: #'System-Text'!
Editor subclass: #TextEditor
	instanceVariableNames: 'model textComposition markBlock pointBlock emphasisHere lastParenLocation selectionStartBlocks selectionStopBlocks initialSelectionStart initialSelectionStop doWordSelection doParagraphSelection'
	classVariableNames: 'ChangeText FindText'
	poolDictionaries: ''
	category: 'System-Text'!
!methodRemoval: SimpleEditor #hasCaret!
SimpleEditor removeSelector: #hasCaret!
!methodRemoval: Editor #hasSelection!
Editor removeSelector: #hasSelection!
!methodRemoval: MorphicScanner #displayBulletIfAppropriateFor:paragraphTopLeft:!
MorphicScanner removeSelector: #displayBulletIfAppropriateFor:paragraphTopLeft:!
!methodRemoval: MorphicScanner #displayBulletParagraphTopLeft:number:!
MorphicScanner removeSelector: #displayBulletParagraphTopLeft:number:!
!methodRemoval: MorphicScanner #displayLine:paragraphTopLeft:leftInRun:!
MorphicScanner removeSelector: #displayLine:paragraphTopLeft:leftInRun:!
!classDefinition: #MorphicScanner category: #'Graphics-Text'!
CharacterScanner subclass: #MorphicScanner
	instanceVariableNames: 'canvas lineY foregroundColor defaultColor topLeft ignoreColorChanges'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Text'!
!classRemoval: #Paragraph!
Smalltalk removeClassNamed: #Paragraph!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

InnerTextMorph allInstancesDo: [ :each | each installEditorAndTextComposition] !

