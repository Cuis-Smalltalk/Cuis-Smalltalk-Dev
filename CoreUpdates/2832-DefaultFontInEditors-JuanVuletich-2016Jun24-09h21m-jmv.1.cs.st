'From Cuis 4.2 of 25 July 2013 [latest update: #2831] on 24 June 2016 at 9:48:52.473602 am'!
!classDefinition: #CharacterScanner category: #'Graphics-Text'!
Object subclass: #CharacterScanner
	instanceVariableNames: 'destX lastIndex xTable map destY stopConditions text alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth kern paragraphStyle tabWidth defaultFont '
	classVariableNames: 'DefaultStopConditions StopConditionsWithPaddedSpace StopConditionsWithSpace '
	poolDictionaries: ''
	category: 'Graphics-Text'!
!classDefinition: #TextEditor category: #'System-Text'!
Editor subclass: #TextEditor
	instanceVariableNames: 'model textComposition markBlock pointBlock emphasisHere lastParenLocation selectionStartBlocks selectionStopBlocks initialSelectionStart initialSelectionStop doWordSelection doParagraphSelection defaultFont '
	classVariableNames: 'ChangeText FindText '
	poolDictionaries: ''
	category: 'System-Text'!

!CharacterScanner methodsFor: 'initialization' stamp: 'jmv 6/23/2016 16:41'!
defaultFont: aFont
	defaultFont _ aFont! !


!Text methodsFor: 'emphasis' stamp: 'jmv 6/23/2016 16:22'!
fontAt: characterIndex default: defaultFont
	"Answer the font for characters in the run beginning at characterIndex."

	^self fontIfApplying: (self attributesAt: characterIndex) default: defaultFont! !

!Text methodsFor: 'emphasis' stamp: 'jmv 6/23/2016 16:22'!
fontIfApplying: textAttributes default: defaultFont
	"Answer the font for characters as specified by the argument."

	self withAttributeValues: textAttributes do: [ :familyName :pointSize :emphasis :color :alignment :characterStyle :paragraphStyle |
		^((AbstractFont familyName: familyName pointSize: pointSize) ifNil: [ defaultFont baseFont ])
			emphasized: emphasis ]! !


!TextEditor methodsFor: 'accessing' stamp: 'jmv 6/23/2016 16:41'!
defaultFont
	^defaultFont! !

!TextEditor methodsFor: 'accessing' stamp: 'jmv 6/23/2016 16:39'!
defaultFont: aFont
	defaultFont _ aFont! !

!TextEditor methodsFor: 'attributes' stamp: 'jmv 6/23/2016 17:05'!
offerDefaultFontMenu
	"Present a menu of available fonts, and if one is chosen, apply it to the current selection.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	"This is a user command, and generates undo"

	| newFont |
	newFont _ AbstractFont fromUser: defaultFont.
	newFont ifNil: [ ^self ].
	defaultFont _ newFont.
	morph resetTextComposition.
	self recomputeSelection.
	morph updateFromTextComposition.! !


!TextComposer methodsFor: 'accessing' stamp: 'jmv 6/23/2016 17:14'!
defaultFont
	^editor ifNil: [ AbstractFont default ] ifNotNil: [ editor defaultFont ]! !


!TextComposition methodsFor: 'access' stamp: 'jmv 6/23/2016 17:14'!
defaultFont
	^editor ifNil: [ AbstractFont default ] ifNotNil: [ editor defaultFont ]! !


!CharacterScanner methodsFor: 'private' stamp: 'jmv 6/23/2016 16:49'!
setFont
	| attributes |
	"Set the font and other emphasis.
	In fact set actual ParagraphStyle (or nil), alignment, font and emphasis"

	self setActualFont: (text fontAt: lastIndex default: defaultFont).
	attributes _ text attributesAt: lastIndex.
	text
		withAttributeValues: attributes
		do: [ :familyNameInText :pointSizeInText :emphasisInText :colorInText :alignmentInText :characterStyleInText :paragraphStyleInText |
			colorInText ifNotNil: [ self textColor: colorInText ].
			alignment _ alignmentInText.
			paragraphStyle _ paragraphStyleInText ].
	
	"Hardcoded color for TextAction"
	attributes do: [ :attribute |
		attribute forTextActionInfoDo: [ :info |
			self textColor: TextAction textActionColor ]].

	"Install various parameters from the font."
	spaceWidth _ font widthOf: Character space.
	tabWidth _ (font widthOf: $a) * 3.
	xTable _ font xTable.
	map _ font characterToGlyphMap! !


!CompositionScanner methodsFor: 'stop conditions' stamp: 'jmv 6/23/2016 16:49'!
crossedX
	"There is a word that has fallen across the right edge of the composition 
	rectangle. This signals the need for wrapping which is done to the last 
	space that was encountered, as recorded by the space stop condition."

	spaceCount >= 1 ifTrue: [
		"The common case. First back off to the space at which we wrap."
		line stop: spaceIndex.
		lastLineBreakingSpace _ spaceIndex.
		lineHeight _ lineHeightAtSpace.
		baseline _ baselineAtSpace.
		spaceCount _ spaceCount - 1.
		spaceIndex _ spaceIndex - 1.

		"Check to see if any spaces preceding the one at which we wrap.
			Double space after punctuation, most likely."
		[(spaceCount > 1 and: [(text at: spaceIndex) = Character space])]
			whileTrue: [
				spaceCount _ spaceCount - 1.
				"Account for backing over a run which might
					change width of space."
				font _ text fontAt: spaceIndex default: defaultFont.
				spaceIndex _ spaceIndex - 1.
				spaceX _ spaceX - (font widthOf: Character space)].
		line paddingWidth: rightMargin - spaceX - 1.
		line internalSpaces: spaceCount]
	ifFalse: [
		"Neither internal nor trailing spaces -- almost never happens."
		lastIndex _ lastIndex - 1.
		[ destX <= rightMargin or: [ lastIndex = 0]]
			whileFalse: [
				destX _ destX - (font widthOf: (text at: lastIndex)).
				lastIndex _ lastIndex - 1].
		lastLineBreakingSpace _ lastIndex.
		spaceX _ destX.
		line paddingWidth: rightMargin - destX - 1.
		line stop: (lastIndex max: line first)].
	^true! !


!TextEditor methodsFor: 'accessing' stamp: 'jmv 6/23/2016 16:51'!
lastFont
	"Answer the Font for to be used if positioned at the end of the text"

	^self startIndex > model textSize
		ifTrue: [ model actualContents fontIfApplying: emphasisHere default: defaultFont ]
		ifFalse: [ model actualContents fontAt: model textSize +1 default: defaultFont ]! !

!TextEditor methodsFor: 'attributes' stamp: 'jmv 6/24/2016 09:28'!
offerFontMenu
	"Present a menu of available fonts, and if one is chosen, apply it to the current selection.
	Note: use #baseFont. If emphasis is desired, add it separatedly."
	"This is a user command, and generates undo"

	| curFont newFont attr startIndex |
	startIndex _ self startIndex.
	curFont _ model actualContents fontAt: startIndex default: defaultFont.
	newFont _ AbstractFont fromUser: curFont.
	newFont ifNil: [ ^self ].
	attr _ TextFontFamilyAndSize
			familyName: newFont familyName
			pointSize: newFont pointSize.
	self applyAttribute: attr.
	morph updateFromTextComposition.! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/23/2016 16:56'!
initializeMenu
	"Initialize the mouseButton2 (right button) pop-up menu and corresponding messages."
	"
	Editor initialize
	"

	menu _ SelectionMenu fromArray: {
		{'Help...'.							#openHelp}.
		#-.
		{'Find...(f)'.						#find}.
		{'Find Again (g)'.					#findAgain}.
		{'Use Selection for Find (j)'.	#setSearchString}.
		#-.
		{'Undo - multiple (z)'.			#undo}.
		{'Redo - multiple (Z)'.			#redo}.
		{'Undo / Redo history'.			#offerUndoHistory}.
		#-.
		{'Copy (c)'.						#copySelection}.
		{'Cut (x)'.							#cut}.
		{'Paste (v)'.						#paste}.
		{'Paste without Format'.			#pasteString}.
		{'Paste...'.						#pasteRecent}.
		#-.
		{'Toggle WordWrap'.			#wrapOnOff}.
		{'Set Font... (k)'.					#offerFontMenu}.
		{'Set Default Font...'.			#offerDefaultFontMenu}.
		{'Set Alignment...'.				#chooseAlignment}.
	}! !


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/24/2016 09:31'!
initializeMenu
	"Initialize the mouseButton2 (right button) pop-up menu and corresponding messages."
	"
	Editor initialize
	"

	menu _ SelectionMenu fromArray: {
		{'Help...'.							#openHelp}.
		#-.
		{'Find...(f)'.						#find}.
		{'Find Again (g)'.					#findAgain}.
		{'Use Selection for Find (j)'.	#setSearchString}.
		#-.
		{'Undo - multiple (z)'.			#undo}.
		{'Redo - multiple (Z)'.			#redo}.
		{'Undo / Redo history'.			#offerUndoHistory}.
		#-.
		{'Copy (c)'.						#copySelection}.
		{'Cut (x)'.							#cut}.
		{'Paste (v)'.						#paste}.
		{'Paste without Format'.			#pasteString}.
		{'Paste...'.						#pasteRecent}.
		#-.
		{'Accept (s)'.						#acceptContents}.
		{'Cancel (l)'.						#cancelEdits}.
		#-.
		{'Toggle WordWrap'.			#wrapOnOff}.
		{'Set Font... (k)'.					#offerFontMenu}.
		{'Set Default Font...'.			#offerDefaultFontMenu}.
		{'Smalltalk Options'.				#getMenu2}.
	}.
	menu2 _ SelectionMenu fromArray: {
		{'Do it (d)'.						#doIt}.
		{'Print it (p)'.						#printIt}.
		{'Inspect it (i)'.					#inspectIt}.
		{'Explore it (I)'.					#exploreIt}.
		{'Debug it'.						#debugIt}.
		#-.
		{'Explain'.							#explain}.
		{'Browse it (b)'.					#browseIt}.
		{'Senders of it (n)'.				#sendersOfIt}.
		{'Implementors of it (m)'.		#implementorsOfIt}.
		{'References to it (N)'.			#referencesToIt}.
		#-.
		{'Method Strings with it (E)'.	#methodStringsContainingit}.
		{'Method Source with it'.		#methodSourceContainingIt}.
		{'Class Comments with it'.		#classCommentsContainingIt}.
	}! !


!InnerTextMorph methodsFor: 'private' stamp: 'jmv 6/23/2016 17:15'!
installEditorAndTextComposition
	"Install an editor for my textComposition. Install also the textComposition."
	| e tc |
	
	"Editor and TextComposition are assigned here atomically."
	e _ model editorClass new morph: self.
	e model: model.
	e defaultFont: AbstractFont default.
	tc _ TextComposition new.
	"Keep critical section short"
	self mutex critical: [
		editor _ e.
		textComposition _ tc ].
	tc
		setModel: model;
		extentForComposing: self extentForComposing.
	e textComposition: tc.
	tc editor: e.
	e setEmphasisHereFromText.
	tc composeAll.
	e resetState.
	self fit.
	self selectionChanged.

	"Add extras. Text Styler and Autocompleter"
	self stylerClass:
		(Preferences syntaxHighlightingAsYouType ifTrue: [
			model textStylerClass ]).
	self autoCompleterClass:
		model autoCompleterClass! !


!MorphicCanvas methodsFor: 'drawing-text' stamp: 'jmv 6/24/2016 09:27'!
textComposition: aTextComposition bounds: boundsRect color: c selectionColor: sc
	| displayScanner leftInRun line boundsInWorld tl |

	tl _ boundsRect topLeft.
	boundsInWorld _ currentTransformation displayBoundsOfTransformOf: boundsRect.

	displayScanner _ MorphicScanner new
		defaultFont: aTextComposition defaultFont;
		text: aTextComposition textComposed
		foreground: c.
	displayScanner canvas: self.

	leftInRun _ 0.
	"Take clipRect into account. Extrememly fast scrolls and redraws of huge files (like .sources)"
	(aTextComposition lineIndexForPoint: (0@0 max: (clipLeft@clipTop)- boundsInWorld origin))
		to: (aTextComposition lineIndexForPoint: (boundsInWorld extent min: (clipRight@clipBottom+1) - boundsInWorld origin))
		do: [ :i |
			line _ aTextComposition lines at: i.
			aTextComposition
				displaySelectionInLine: line
				on: self
				textTopLeft: tl
				selectionColor: sc.
			leftInRun _ displayScanner displayLine: line textTopLeft: tl leftInRun: leftInRun  ]! !


!TextComposer methodsFor: 'as yet unclassified' stamp: 'jmv 6/23/2016 16:50'!
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

!TextComposer methodsFor: 'as yet unclassified' stamp: 'jmv 6/23/2016 16:49'!
composeLinesFrom: argStart to: argStop delta: argDelta into: argLinesCollection priorLines: argPriorLines atY: argStartY text: argText extentForComposing: argExtentForComposing

	lines _ argLinesCollection.
	theText _ argText.
	extentForComposing _ argExtentForComposing.
	deltaCharIndex _ argDelta.
	currCharIndex _ startCharIndex _ argStart.
	stopCharIndex _ argStop.
	prevLines _ argPriorLines.
	currentY _ argStartY.
	maxRightX _ 0.
	possibleSlide _ stopCharIndex < theText size.
	nowSliding _ false.
	prevIndex _ 1.
	scanner _ CompositionScanner new text: theText.
	scanner defaultFont: self defaultFont.
	isFirstLine _ currCharIndex = 1 or: [
		 (theText at: currCharIndex-1) isLineSeparator ].
	anythingComposed _ false.
	self composeAllLines.
	(anythingComposed not or: [ theText last isLineSeparator ])
		ifTrue: [
			self addEmptyTrailingLine: true ]
		ifFalse: [
			scanner lastLineBreakingSpace = theText size ifTrue: [
				self addEmptyTrailingLine: false ]].
	^ {lines asArray. maxRightX}

! !


!TextComposition methodsFor: 'display' stamp: 'jmv 6/23/2016 16:51'!
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
					ifNil: [ model actualContents fontAt: idx default: self defaultFont ]
					ifNotNil: [ model actualContents fontIfApplying: textCursorAttributes default: self defaultFont ].
				b _ textTopLeft y + line top + line baseline + textCursorFont descent-1.
				t _ textTopLeft y + line top + line baseline - textCursorFont ascent.
				lastTextCursorRect _ nil.
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

!TextComposition methodsFor: 'selection' stamp: 'jmv 6/23/2016 16:47'!
characterBlockAtPoint: aPoint 
	"Answer a CharacterBlock for the character in the text at aPoint."
	| line |
	line _ lines at: (self lineIndexForPoint: aPoint).
	^ (CharacterBlockScanner new text: model actualContents)
		defaultFont: self defaultFont;
		characterBlockAtPoint: aPoint index: nil
		in: line! !

!TextComposition methodsFor: 'selection' stamp: 'jmv 6/23/2016 16:47'!
characterBlockForIndex: index 
	"Answer a CharacterBlock for the character in text at index."
	| line t |
	line _ lines at: (self lineIndexFor: index).
	t _ model actualContents.
	^ (CharacterBlockScanner new text: t)
		defaultFont: self defaultFont;
		characterBlockAtPoint: nil index: ((index max: line first) min: t size+1)
		in: line! !


!Theme methodsFor: 'menus' stamp: 'jmv 6/24/2016 09:46'!
miscellaneousIcons

	"Everything else."

	^ {
		#('restore display (r)' 'set display depth...' 'move windows onscreen' 'Utilities saveDisplay.') -> #displayIcon.
		#('changes...' 'dual change sorter' 'change sets with this method' 'find a change sorter' 'recent changes in file' 'Undo / Redo history') -> #changesIcon.
		#('Install New Updates') -> #updateIcon.
		#('print PS to file...' ) -> #printIcon.
		#('find again (g)' 'full stack (k)') -> #systemIcon.
		#('print it (p)' 'check change set for slips') -> #printIcon.
		#('accept (s)' 'make changes go to me (m)') -> #acceptIcon.
		#('cancel (l)' ) -> #cancelIcon.
		#('debug...' 'debug it' 'toggle break on entry') -> #debugIcon.
		#('close' 'close all debuggers' 'close top window') -> #closeIcon.
		#('collapse' 'hide taskbar' 'collapse all windows') -> #collapseIcon.
		#('expand / contract' 'show taskbar' 'restore all windows') -> #expandIcon.
		#('menu') -> #windowMenuIcon.
		#('browse all' 'browser' 'browse it (b)' 'MessageTally UI and browse' 'browse recent submissions' 'browse full (b)' 'find changed browsers...' 'browse (b)' 'browse my changes') -> #editFindReplaceIcon.
		#('workspace' 'workspace with contents') -> #terminalIcon.
		#('styled text editor' 'text editor' 'edit this list' 'edit postscript...' 'add postscript...') -> #textEditorIcon.
		#('file list' 'find a fileList') -> #systemFileManagerIcon.
		#('transcript' 'find a transcript' 'Transcript clear.' 'log to transcript') -> #printerIcon.
		#('process browser' 'vm statistics' 'MessageTally all Processes') -> #systemMonitorIcon.
		#('emergency evaluator' 'conflicts with other change sets' 'check for slips' 'conflicts with change set opposite' 'conflicts with category opposite') -> #emblemImportantIcon.
		#('change sorter') -> #halfRefreshIcon.
		#('SUnit Test Runner') -> #weatherFewCloudsIcon.
		#('Font Sizes...' 'system fonts...' 'set font... (k)') -> #preferencesDesktopFontIcon.
		#('full screen on') -> #viewFullscreenIcon.
		#('full screen off') -> #exitFullscreenIcon.
		#('set desktop color...') -> #wallpaperIcon.
		#('preferences...' 'All preferences...' 'what to show...') -> #preferencesIcon.
		#('Editor keyboard shortcuts') -> #keyboardShortcutsIcon.
		#('world menu help') -> #globeIcon.		"currently unused, but a neat icon"
		#('useful expressions' 'class comments with it' 'check for uncommented methods' 'check for uncommented classes') -> #chatIcon.
		#('set code author...' 'check for other authors' 'check for any other authors') -> #usersIcon.
		#('space left') -> #removableMediaIcon.
		#('start drawing all again' 'window color...') -> #graphicsIcon.
		#('start stepping again') -> #mediaPlaybackStartIcon.
		#('file out current change set' 'fileOut' 'File out and remove (o)' 'File out and keep (k)') -> #fileOutIcon.
		#('recently logged changes...' 'versions (v)' 'recent classes... (r)' 'trim history' 'profile messages (m)') -> #clockIcon.
		#('senders of it (n)' 'senders of... (n)' 'local senders of...' 'senders (n)') -> #mailForwardIcon.
		#('implementors of it (m)' 'implementors of... (m)' 'implementors of sent messages') -> #developmentIcon.
		#('references to it (N)') -> #addressBookIcon.
		#('class var refs...' 'class refs (N)' 'class variables' 'class vars' 'local implementors of...' 'subclass template') -> #classIcon.
		#('inst var refs...' 'inst var defs...' 'sample instance') -> #instanceIcon.
		#('Use Selection for Find (j)' 'rename class ...' 'rename...' 'change title...') -> #saveAsIcon.
		#('smalltalk options' 'method source with it' 'browse method (O)' 'check for uncategorized methods') -> #scriptIcon.
		#('method strings with it (E)' 'Toggle WordWrap') -> #genericTextIcon.
		#('browse hierarchy (h)' 'move to top' 'promote to top of list') -> #goTopIcon.
		#('move up' 'make next-to-topmost') -> #goUpIcon.
		#('move to bottom' 'send to back' 'send top window to back') -> #goBottomIcon.
		#('inheritance (i)' 'move down') -> #goDownIcon.
		#('browse protocol (p)' 'spawn sub-protocol') -> #spreadsheetIcon.
		#('spawn full protocol') -> #speadsheetTemplateIcon.
		#('alphabetize') -> #fontXGenericIcon.
		#('Installed Packages' 'browse' 'show category (C)' 'categorize all uncategorized' 'select change set...' 'view affected class categories') -> #packageIcon.
		#('remove from current change set' 'remove empty categories' 'subtract other side (-)' 'remove from this browser') -> #listRemoveIcon.
		#('add to current change set' 'add all meths to current chgs' 'add preamble (p)') -> #listAddIcon.
		#('toggle diffing (D)' 'toggle selections') -> #switchIcon.
		#('reorganize' 'create inst var accessors' 'ChangeSorter reorderChangeSets.' 'reorder all change sets' 'by name' 'by size' 'by date') -> #sendReceiveIcon.
		#('unsent methods' 'unreferenced class vars' 'unreferenced inst vars' 'Undeclared inspect.' 'Undeclared removeUnreferencedKeys; inspect.' 'ChangeSorter removeEmptyUnnamedChangeSets.' 'check for unsent messages') -> #junkIcon.
		#('update' 'turn on auto-update (a)' 'update list (u)') -> #updateIcon.
		#('find changed windows...') -> #newWindowIcon.
		#('make undraggable') -> #pushPinIcon.
		#('Utilities saveScreenshot.') -> #stillCameraIcon.
		#('add new directory') -> #newFolderIcon.
		#('select all' 'deselect all') -> #selectAllIcon.
		#('sort by date') -> #dateIcon.
		#('justified') -> #formatJustifyFillIcon.
		#('centered') -> #formatJustifyCenterIcon.
		#('set alignment...' 'leftFlush') -> #formatJustifyLeftIcon.
		#('rightFlush') -> #formatJustifyRightIcon.
		#('signal Semaphore (S)') -> #haloHelpIcon.
		#('Change Paragraph Style...' 'Change Character Style...' 'Remove Character Style' 'Replace all uses of Paragraph Style...' 'Replace all uses of Character Style...' 'Set default Font...') -> #fontXGenericIcon.
	}! !

!methodRemoval: Text #fontAt:!
Text removeSelector: #fontAt:!
!methodRemoval: Text #fontIfApplying:!
Text removeSelector: #fontIfApplying:!
!classDefinition: #CharacterScanner category: #'Graphics-Text'!
Object subclass: #CharacterScanner
	instanceVariableNames: 'destX lastIndex xTable map destY stopConditions text alignment leftMargin rightMargin font line runStopIndex spaceCount spaceWidth kern paragraphStyle tabWidth defaultFont'
	classVariableNames: 'DefaultStopConditions StopConditionsWithPaddedSpace StopConditionsWithSpace'
	poolDictionaries: ''
	category: 'Graphics-Text'!
!classDefinition: #TextEditor category: #'System-Text'!
Editor subclass: #TextEditor
	instanceVariableNames: 'model textComposition markBlock pointBlock emphasisHere lastParenLocation selectionStartBlocks selectionStopBlocks initialSelectionStart initialSelectionStop doWordSelection doParagraphSelection defaultFont'
	classVariableNames: 'ChangeText FindText'
	poolDictionaries: ''
	category: 'System-Text'!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

Editor initialize.
Theme current class beCurrent.
Editor allSubInstancesDo: [ :each | each instVarNamed: 'defaultFont' put: AbstractFont default ].
CharacterScanner allSubInstancesDo: [ :each | each instVarNamed: 'defaultFont' put: AbstractFont default ].!

