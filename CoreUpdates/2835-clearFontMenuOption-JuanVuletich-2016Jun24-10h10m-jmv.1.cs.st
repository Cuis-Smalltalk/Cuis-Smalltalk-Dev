'From Cuis 4.2 of 25 July 2013 [latest update: #2832] on 24 June 2016 at 10:56:23.910602 am'!

!TextModel methodsFor: 'undoable commands' stamp: 'jmv 6/24/2016 10:30'!
logUndoAndRemoveAttributes: textAttributes from: requestedStart to: requestedStop
	"As requested."

	| command |
	command _ actualContents commandForRemoveAttributes: textAttributes from: requestedStart to: requestedStop.
	undoRedoCommands
		nextPut: command;
		truncateAtPosition.	"To disable redo of previous commands, now invalidated."
	command doOn: self.
	lastEditTimeStamp _ nil! !


!TextEditor methodsFor: 'attributes' stamp: 'jmv 6/24/2016 10:44'!
clearFont
	"Remove any Font attribute"
	"This is a user command, and generates undo"

	| attributesToRemove firstIndex lastIndex |
	attributesToRemove _ Set new.
	firstIndex _ self startIndex.
	lastIndex _ self stopIndex - 1.
	model actualContents attributesFrom: firstIndex to: lastIndex do: [ :attribute |
		attribute isFont ifTrue: [ attributesToRemove add: attribute ]].

	self unapplyAttributes: attributesToRemove.
	morph resetTextComposition.
	self recomputeSelection.
	morph updateFromTextComposition.! !

!TextEditor methodsFor: 'private' stamp: 'jmv 6/24/2016 10:34'!
unapplyAttributes: textAttributes
	"The user selected textAttributes to be removed.
	If there is a selection, unapply the attributes to the selection.
	In any case do not use the attribute for the user input (emphasisHere)"
	"This generates undo"

	| interval |
	emphasisHere _ emphasisHere copyWithoutAll: textAttributes.
	interval _ self selectionInterval.
	(interval isEmpty and: [ textAttributes noneSatisfy: [ :each | each isParagraphAttribute ]])
		ifTrue: [ ^self ].
	model logUndoAndRemoveAttributes: textAttributes from: interval first to: interval last.
	textComposition recomposeFrom: interval first to: interval last delta: 0.
	self recomputeSelection.	"Needed so visible selection is updated to reflect new visual extent of selection"
	morph possiblyChanged! !


!TextEditor methodsFor: 'attributes' stamp: 'jmv 6/24/2016 10:33'!
changeEmphasisOrAlignment
	"This is a user command, and generates undo"

	| menuStrings aList reply code align menuList startIndex attribute |
	startIndex _ self startIndex.
	aList _ #(normal bold italic underlined struckThrough leftFlush centered rightFlush justified).
	align _ model actualContents alignmentAt: startIndex.
	code _ model actualContents emphasisAt: startIndex.
	menuList _ WriteStream on: Array new.
	menuList nextPut: (code isZero ifTrue:['<on>'] ifFalse:['<off>']), 'normal'.
	menuList nextPutAll: (#(bold italic underlined struckThrough superscript subscript withST80Glyphs) collect: [ :emph |
		(code anyMask: (TextEmphasis perform: emph) emphasisCode)
			ifTrue: [ '<on>', emph asString ]
			ifFalse: [ '<off>', emph asString ]]).
	menuList nextPutAll: (#(leftFlush centered rightFlush justified) withIndexCollect: [ :type :i |
		align = (i-1)
			ifTrue: [ '<on>', type asString ]
			ifFalse: [ '<off>', type asString ]]).
	menuStrings _ menuList contents.
	aList _ #(normal bold italic underlined struckThrough superscript subscript withST80Glyphs leftFlush centered rightFlush justified).
	reply _ (SelectionMenu labelList: menuStrings lines: #(1 8) selections: aList) startUpWithoutKeyboard.
	reply ifNotNil: [
		(#(leftFlush centered rightFlush justified) includes: reply)
			ifTrue: [
				attribute _ TextAlignment perform: reply]
			ifFalse: [
				attribute _ TextEmphasis perform: reply].
		((menuStrings at: (aList indexOf: reply)) beginsWith: '<on>')
			ifTrue: [ self unapplyAttributes: {attribute} ]
			ifFalse: [ self applyAttribute:  attribute ]].
	^ true! !

!TextEditor methodsFor: 'editing keys' stamp: 'jmv 6/24/2016 10:33'!
changeEmphasis: aKeyboardEvent
	"Change the emphasis of the current selection or prepare to accept characters with the change in emphasis."
	"control 0..9 -> 0..9"
	"This is a user command, and generates undo"

	| keyCode attributeToAdd attributeToRemove oldAttributes |
	keyCode _ ('01234567'
		indexOf: aKeyboardEvent keyCharacter
		ifAbsent: [ 1 ]) - 1.
	oldAttributes _ emphasisHere.

	keyCode = 0 ifTrue: [ attributeToAdd _ TextEmphasis normal ].

	(keyCode between: 1 and: 7) ifTrue: [
		attributeToAdd _ TextEmphasis perform: (#(#bold #italic #underlined #struckThrough #superscript #subscript #withST80Glyphs) at: keyCode).
		oldAttributes do: [ :oldAtt |
			oldAtt = attributeToAdd ifTrue: [
				attributeToAdd _ nil.
				attributeToRemove _ oldAtt ]]].

	attributeToAdd ifNotNil: [ self applyAttribute: attributeToAdd ].
	attributeToRemove ifNotNil: [ self unapplyAttributes: {attributeToRemove} ].
	^ true! !


!TextEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/24/2016 10:12'!
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
		{'Clear Font'.						#clearFont}.
		{'Set Default Font...'.			#offerDefaultFontMenu}.
		{'Set Alignment...'.				#chooseAlignment}.
	}! !


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 6/24/2016 10:53'!
initializeMenu
	"Initialize the mouseButton2 (right button) pop-up menu and corresponding messages."
	"
	Editor initialize
	"

	menu _ SelectionMenu fromArray: {
		{'Help...'.							#openHelp}.
		#-.
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
		#-.
		{'Accept (s)'.						#acceptContents}.
		{'Cancel (l)'.						#cancelEdits}.
		#-.
		{'More...'.							#getMenu2}.
	}.
	menu2 _ SelectionMenu fromArray: {
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
		{'Clear Font'.						#clearFont}.
		{'Set Default Font...'.			#offerDefaultFontMenu}.
		#-.
		{'More...'.							#getMenu}.
	}! !

!SmalltalkEditor class methodsFor: 'menu' stamp: 'jmv 6/24/2016 10:50'!
paneMenu2:  aMenu

	| m |
	m _ self menu2.
	aMenu labels: m labelString lines: m lineArray selections: m selections.
	aMenu addTitle: 'Additional options  '; addStayUpIcons.
	^ aMenu! !


!Theme methodsFor: 'menus' stamp: 'jmv 6/24/2016 10:49'!
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
		#('add to current change set' 'add all meths to current chgs' 'add preamble (p)' 'More...') -> #listAddIcon.
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
		#('Change Paragraph Style...' 'Change Character Style...' 'Remove Character Style' 'Replace all uses of Paragraph Style...' 'Replace all uses of Character Style...' 'Set Default Font...') -> #fontXGenericIcon.
		#('Clear Font') -> #newIcon.
	}! !

!methodRemoval: TextEditor #unapplyAttribute:!
TextEditor removeSelector: #unapplyAttribute:!
!methodRemoval: TextModel #logUndoAndRemoveAttribute:from:to:!
TextModel removeSelector: #logUndoAndRemoveAttribute:from:to:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."

Editor initialize.
Theme current class beCurrent.!

