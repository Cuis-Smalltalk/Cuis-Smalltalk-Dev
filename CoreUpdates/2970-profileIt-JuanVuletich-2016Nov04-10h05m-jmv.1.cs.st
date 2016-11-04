'From Cuis 4.2 of 25 July 2013 [latest update: #2968] on 4 November 2016 at 3:25:53 pm'!

!Compiler methodsFor: 'public access' stamp: 'jmv 11/4/2016 15:16:25'!
evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock logged: doLog profiled: doProfile
	"Compiles the sourceStream into a parse tree, then generates code into
	 a method. If aContext is not nil, the text can refer to temporaries in that
	 context (the Debugger uses this). If aRequestor is not nil, then it will receive
	 a notify:at: message before the attempt to evaluate is aborted. Finally, the 
	 compiled method is invoked from here via withArgs:executeMethod:, hence
	 the system no longer creates Doit method litter on errors."

	| methodNode method value toLog itsSelection itsSelectionString |
	class _ (aContext ifNil: [ receiver ] ifNotNil: [ aContext receiver ]) class.
	self from: textOrStream class: class context: aContext notifying: aRequestor.
	methodNode _ self translate: sourceStream noPattern: true ifFail: [^failBlock value].

	method _ methodNode generate.

	"(jmv) Log before evaluating. This way, if the evaluation is halted by whatever reason, it is logged anyway"
	doLog ifTrue: [
		toLog _ ((requestor respondsTo: #selection)  
			and: [ (itsSelection _ requestor selection) notNil
			and: [ (itsSelectionString _ itsSelection asString) isEmptyOrNil not ]])
				ifTrue: [ itsSelectionString ]
				ifFalse: [ sourceStream contents ].
		SystemChangeNotifier uniqueInstance evaluated: toLog context: aContext].

	"Evaluate now."
	doProfile
		ifTrue: [
			AndreasSystemProfiler spyOn: [
				value _ receiver
					withArgs: (context ifNil: [#()] ifNotNil: [{context}])
					executeMethod: method ]]
		ifFalse: [
			value _ receiver
				withArgs: (context ifNil: [#()] ifNotNil: [{context}])
				executeMethod: method ].

	^ value! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 11/4/2016 15:17:30'!
evaluateSelectionAndDo: aBlock ifFail: failBlock profiled: doProfile
	"Treat the current selection as an expression; evaluate it and return the result
	3 +4
	"
	| provider result rcvr ctxt |
	self lineSelectAndEmptyCheck: [^ ''].

	provider _ self codeProvider.
	(provider respondsTo: #doItReceiver) 
		ifTrue: [
				rcvr _ provider doItReceiver.
				ctxt _ provider doItContext]
		ifFalse: [rcvr _ ctxt _ nil].
	result _ [
		Compiler new
			sourceStreamGetter: #selectionAsStream;		"Cuis specific. Do not remove!!"
			evaluate: self selectionAsStream
			in: ctxt
			to: rcvr
			notifying: self
			ifFail: [^ failBlock value]
			logged: true
			profiled: doProfile.
	] 
		on: OutOfScopeNotification 
		do: [ :ex | ex resume: true].
	morph formatAndStyleIfNeeded.		"Needed to re-shout workspaces, that might have new variables binded."
	^ aBlock value: result! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 11/4/2016 15:19:17'!
profileIt
	"Like #doit, but profiling"
	^ self
		evaluateSelectionAndDo: [ :result | (result printString) print ]
		ifFail: nil
		profiled: true! !


!Compiler methodsFor: 'public access' stamp: 'jmv 11/4/2016 15:16:42'!
evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock
	^ self evaluate: textOrStream in: aContext to: receiver notifying: aRequestor ifFail: failBlock logged: false profiled: false! !


!Compiler class methodsFor: 'evaluating' stamp: 'jmv 11/4/2016 15:16:53'!
evaluate: textOrString for: anObject notifying: aRequestor logged: logFlag
	"Compile and execute the argument, textOrString with respect to the class 
	of anObject. If a compilation error occurs, notify aRequestor. If both 
	compilation and execution are successful then, if logFlag is true, log 
	(write) the text onto a system changes file so that it can be replayed if 
	necessary."

	^ self new
				evaluate: textOrString
				in: nil
				to: anObject
				notifying: aRequestor
				ifFail: [^nil]
				logged: logFlag
				profiled: false! !


!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 11/4/2016 15:18:17'!
doIt
	"Set the context to include pool vars of the model.  Then evaluate.
	Print the result on the Transcript"
	^ self
		evaluateSelectionAndDo: [ :result | (result printString) print ]
		ifFail: nil
		profiled: false! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 11/4/2016 15:18:12'!
exploreIt

	self
		evaluateSelectionAndDo: [ :result | result explore ]
		ifFail: [ morph flash ]
		profiled: false! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 11/4/2016 15:18:26'!
inspectIt
	self
		evaluateSelectionAndDo: [ :result | result inspect ]
		ifFail: [ morph flash ]
		profiled: false! !

!SmalltalkEditor methodsFor: 'do-its' stamp: 'jmv 11/4/2016 15:18:29'!
printIt
	"Treat the current text selection as an expression; evaluate it. Insert the 
	description of the result of evaluation after the selection and then make 
	this description the new text selection."
	self
		evaluateSelectionAndDo: [ :result |
			self afterSelectionInsertAndSelect: 
				((' ', result printText, ' ') initialFontFrom: emphasisHere)]
		ifFail: [ morph flash ]
		profiled: false! !


!SmalltalkEditor class methodsFor: 'keyboard shortcut tables' stamp: 'jmv 11/4/2016 15:19:56'!
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
		{'Profile it'.						#profileIt}.
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


!Theme methodsFor: 'menus' stamp: 'jmv 11/4/2016 15:23:42'!
basicIcons

	"Minimal menu scheme.
	Theme current class beCurrent
	"

	^ { 
		#('open...') -> #openIcon.
		#('windows...' 'find window' 'Focus follows mouse' 'Click to focus') -> #windowIcon.
		#('help...' 'explain' 'about this system...' 'Terse Guide to Cuis' 'Class Comment Browser' 'Code management in Cuis' 'Using GitHub to host Cuis packages' ) -> #helpIcon.
		#('themes...') -> #appearanceIcon.
		#('do it (d)') -> #doItIcon.
		#('new morph...' 'objects (o)' 'save world as morph file') -> #morphsIcon.
		#('save' ) -> #saveIcon.
		#('Save options...' 'save as...' 'change category...' 'rename change set (r)' 'rename') -> #saveAsIcon.
		#('save as new version') -> #saveAsNewVersionIcon.
		#('quit') -> #quitIcon.
		#('save and quit' ) -> #saveAndQuitIcon.
		#('inspect it (i)' 'inspect world'  'inspect model' 'inspect morph'
		 'inspect owner chain' 'inspect' 'inspect (i)' 'basic inspect' 'message names' 'find message names' 'inspect instances' 'inspect subinstances' 'inspect change set' 'inspect context (c)' 'inspect receiver (i)' 'start CPUWatcher' 'stop CPUWatcher')
			-> #inspectIcon.
		#('explore' 'explore it (I)' 'explore world' 'explore morph' 'explore (I)' 'explore context (C)' 'explore receiver (I)' 'references finder' 'weight explorer') -> #exploreIcon.
		#('find...(f)' 'find class... (f)' 'find method...' 'find recent submissions' 'show hierarchy' 'show definition' 'show comment' 'filter' 'filter message list...' 'find context... (f)') -> #findIcon.
		#('add item...' 'new category...' 'create new change set...' 'new change set... (n)' 'add new file') -> #newIcon.
		#('remove method (x)' 'remove' 'remove class (x)' 'remove method from system (x)' 'remove class from system (x)' 'remove postscript') -> #deleteIcon.
		#('delete method from changeset (d)' 'delete class from change set (d)' 'destroy change set (X)' 'revert & remove from changes' 'delete unchanged windows' 'delete non windows' 'delete both of the above' 'reset variables' 'remove contained in class categories...' 'clear this change set' 'uninstall this change set' 'delete directory...' 'delete') -> #warningIcon.
		#('do again (j)' 'Redo - multiple (Z)') -> #redoIcon.
		#('undo (z)' 'revert to previous version' 'Undo - multiple (z)') -> #undoIcon.
		#('copy (c)' 'copy class...' 'copy class chgs to other side' 'copy method to other side' 'copy all to other side (c)' 'copy name to clipboard' 'copy selector to clipboard') -> #copyIcon.
		#('paste (v)' 'Paste without Format') -> #pasteIcon.
		#('cut (x)' 'move class chgs to other side' 'move method to other side' 'submerge into other side') -> #cutIcon.
		#('paste...' 'icons...') -> #worldIcon.
}! !

!Theme methodsFor: 'menus' stamp: 'jmv 11/4/2016 15:24:07'!
miscellaneousIcons

	"Everything else."

	^ {
		#('restore display (r)' 'set display depth...' 'move windows onscreen' 'Utilities saveDisplay.') -> #displayIcon.
		#('changes...' 'dual change sorter' 'change sets with this method' 'find a change sorter' 'recent changes in file' 'Undo / Redo history' 'code file contents' 'package file contents') -> #changesIcon.
		#('Install New Updates' 'update' 'turn on auto-update (a)' 'update list (u)' 'install code' 'fileIn entire file' 'install package') -> #updateIcon.
		#('find again (g)' 'full stack (k)') -> #systemIcon.
		#('print it (p)' 'check change set for slips') -> #printIcon.
		#('accept (s)' 'make changes go to me (m)') -> #acceptIcon.
		#('cancel (l)' 'turn off auto-update (a)') -> #cancelIcon.
		#('debug...' 'debug it' 'toggle break on entry') -> #debugIcon.
		#('close' 'close all debuggers' 'close top window') -> #closeIcon.
		#('collapse' 'hide taskbar' 'collapse all windows') -> #collapseIcon.
		#('expand / contract' 'show taskbar' 'restore all windows') -> #expandIcon.
		#('menu') -> #windowMenuIcon.
		#('browse all' 'browser' 'browse it (b)' 'MessageTally UI and browse' 'browse recent submissions' 'browse full (b)' 'find changed browsers...' 'browse (b)' 'browse my changes') -> #editFindReplaceIcon.
		#('workspace' 'workspace with contents') -> #terminalIcon.
		#('styled text editor' 'text editor'  'basic text editor'  'unicode editor' 'edit this list' 'edit postscript...' 'add postscript...') -> #textEditorIcon.
		#('file list' 'find a fileList' 'compress file') -> #systemFileManagerIcon.
		#('transcript' 'find a transcript' 'Transcript clear.' 'log to transcript') -> #printerIcon.
		#('process browser' 'vm statistics' 'MessageTally all Processes') -> #systemMonitorIcon.
		#('emergency evaluator' 'conflicts with other change sets' 'check for slips' 'conflicts with change set opposite' 'conflicts with category opposite') -> #emblemImportantIcon.
		#('change sorter') -> #halfRefreshIcon.
		#('SUnit Test Runner') -> #weatherFewCloudsIcon.
		#('Font Sizes...' 'system fonts...' 'set font... (k)') -> #preferencesDesktopFontIcon.
		#('full screen on') -> #viewFullscreenIcon.
		#('full screen off') -> #exitFullscreenIcon.
		#('set desktop color...') -> #wallpaperIcon.
		#('preferences...' 'All preferences...' 'what to show...' 'view as hex') -> #preferencesIcon.
		#('Editor keyboard shortcuts') -> #keyboardShortcutsIcon.
		#('world menu help') -> #globeIcon.		"currently unused, but a neat icon"
		#('useful expressions' 'class comments with it' 'check for uncommented methods' 'check for uncommented classes') -> #chatIcon.
		#('set code author...' 'check for other authors' 'check for any other authors') -> #usersIcon.
		#('space left') -> #removableMediaIcon.
		#('start drawing all again' 'window color...') -> #graphicsIcon.
		#('start stepping again') -> #mediaPlaybackStartIcon.
		#('file out current change set' 'fileOut' 'File out and remove (o)' 'File out and keep (k)') -> #fileOutIcon.
		#('recently logged changes...' 'versions (v)' 'recent classes... (r)' 'trim history' 'Profile it') -> #clockIcon.
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
		#('code file browser' 'package file browser') -> #findIcon.
	}! !

!methodRemoval: SmalltalkEditor #evaluateSelectionAndDo:ifFail:!
SmalltalkEditor removeSelector: #evaluateSelectionAndDo:ifFail:!
!methodRemoval: Compiler #evaluate:in:to:notifying:ifFail:logged:!
Compiler removeSelector: #evaluate:in:to:notifying:ifFail:logged:!
"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Editor initialize.
Theme current class beCurrent.!

