'From Cuis7.1 [latest update: #6461] on 11 June 2024 at 10:20:39 am'!

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 6/11/2024 09:47:30'!
presumedSentMessages
	| sent |
	"
	In addition to those here, if it is desired to preserve some methods from deletion, see #nominallyUnsent:
	Smalltalk presumedSentMessages
	"

	"The following should be preserved for doIts, etc"
	sent := IdentitySet new.
	#( rehashWithoutBecome
		browseAllSelect:  lastRemoval
		vScrollBarValue: hScrollBarValue: 
		to: removeClassNamed:
		dragon: hilberts: mandala: web test3 factorial tinyBenchmarks benchFib
		newDepth: restoreAfter: zapAllMethods obsoleteClasses
		removeAllUnSentMessages abandonSources removeUnreferencedKeys
		zapOrganization condenseSources condenseChanges browseObsoleteReferences
		subclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		methodsFor:stamp: methodsFor:stamp:prior: instanceVariableNames:
		startTimerEventLoop unusedClasses allClasses
		unimplemented
		reduceCuis
		variableSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableByteSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		variableWordSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		weakSubclass:instanceVariableNames:classVariableNames:poolDictionaries:category:
		printSpaceAnalysis:on:) do: [ :sel |
			sent add: sel].
	"The following may be sent by perform: in dispatchOnChar..."
	Editor withAllSubclassesDo: [ :c |
		c shortcuts asSet do: [ :sel | sent add: sel ].
		c new defaultMenuSpec do: [ :spec |
			spec size >= 5 ifTrue: [
				spec fifth isSymbol ifTrue: [ sent add: spec fifth ]]]].
	#(beReadOnlyBinding beReadWriteBinding) do: [ :sel |
		sent add: sel].
	AppLauncher appSelector ifNotNil: [ :sel |
		sent add: sel ].
	^ sent! !


!Editor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 6/11/2024 09:44:29'!
defaultMenuSpec
	^#()! !


!Editor class methodsFor: 'class initialization' stamp: 'jmv 6/11/2024 10:19:40'!
initialize
	"
	Editor initialize
	"
	self withAllSubclassesDo: [ :c | c initializeShortcuts ].! !

!Editor class methodsFor: 'cached state access' stamp: 'jmv 6/11/2024 09:57:01'!
releaseClassCachedState

	shortcuts := nil.! !


!TextEditor class methodsFor: 'cached state access' stamp: 'jmv 6/11/2024 09:57:09'!
releaseClassCachedState

	FindText := nil.
	ChangeText := nil.
	"We are not calling super to avoid cleansing class vars many times.
	So, repeat inherited class instVars!!"
	shortcuts := nil.! !


!SmalltalkEditor class methodsFor: 'cached state access' stamp: 'jmv 6/11/2024 09:57:05'!
releaseClassCachedState

	"We are not calling super to avoid cleansing class vars many times.
	So, repeat inherited class instVars!!"
	shortcuts := nil.! !

!methodRemoval: SmalltalkEditor class #cmdShortcutsSpec stamp: 'jmv 6/11/2024 09:56:40'!
SmalltalkEditor class removeSelector: #cmdShortcutsSpec!
!methodRemoval: SmalltalkEditor #exploreIt: stamp: 'jmv 6/11/2024 10:08:05'!
SmalltalkEditor removeSelector: #exploreIt:!
!methodRemoval: SmalltalkEditor #printIt: stamp: 'jmv 6/11/2024 10:08:05'!
SmalltalkEditor removeSelector: #printIt:!
!methodRemoval: SmalltalkEditor #extractToTemporary: stamp: 'jmv 6/11/2024 10:08:05'!
SmalltalkEditor removeSelector: #extractToTemporary:!
!methodRemoval: SmalltalkEditor #browseIt: stamp: 'jmv 6/11/2024 10:08:05'!
SmalltalkEditor removeSelector: #browseIt:!
!methodRemoval: SmalltalkEditor #methodStringsContainingIt: stamp: 'jmv 6/11/2024 10:08:05'!
SmalltalkEditor removeSelector: #methodStringsContainingIt:!
!methodRemoval: SmalltalkEditor #temporaryToInstanceVariable: stamp: 'jmv 6/11/2024 10:08:06'!
SmalltalkEditor removeSelector: #temporaryToInstanceVariable:!
!methodRemoval: SmalltalkEditor #contextualInlineMethod: stamp: 'jmv 6/11/2024 10:08:05'!
SmalltalkEditor removeSelector: #contextualInlineMethod:!
!methodRemoval: SmalltalkEditor #enclose: stamp: 'jmv 6/11/2024 10:11:36'!
SmalltalkEditor removeSelector: #enclose:!
!methodRemoval: SmalltalkEditor #displayIfFalse: stamp: 'jmv 6/11/2024 09:58:15'!
SmalltalkEditor removeSelector: #displayIfFalse:!
!methodRemoval: SmalltalkEditor #contextualRemoveParameter: stamp: 'jmv 6/11/2024 10:08:05'!
SmalltalkEditor removeSelector: #contextualRemoveParameter:!
!methodRemoval: SmalltalkEditor #debugIt: stamp: 'jmv 6/11/2024 10:08:05'!
SmalltalkEditor removeSelector: #debugIt:!
!methodRemoval: SmalltalkEditor #implementorsOfIt: stamp: 'jmv 6/11/2024 10:08:05'!
SmalltalkEditor removeSelector: #implementorsOfIt:!
!methodRemoval: SmalltalkEditor #inspectIt: stamp: 'jmv 6/11/2024 10:08:05'!
SmalltalkEditor removeSelector: #inspectIt:!
!methodRemoval: SmalltalkEditor #methodSourceContainingIt: stamp: 'jmv 6/11/2024 10:08:06'!
SmalltalkEditor removeSelector: #methodSourceContainingIt:!
!methodRemoval: SmalltalkEditor #pasteInitials: stamp: 'jmv 6/11/2024 10:08:06'!
SmalltalkEditor removeSelector: #pasteInitials:!
!methodRemoval: SmalltalkEditor #referencesToIt: stamp: 'jmv 6/11/2024 10:08:06'!
SmalltalkEditor removeSelector: #referencesToIt:!
!methodRemoval: SmalltalkEditor #contextualExtractAsParameter: stamp: 'jmv 6/11/2024 10:08:06'!
SmalltalkEditor removeSelector: #contextualExtractAsParameter:!
!methodRemoval: SmalltalkEditor #argNext: stamp: 'jmv 6/11/2024 09:58:15'!
SmalltalkEditor removeSelector: #argNext:!
!methodRemoval: SmalltalkEditor #fileItIn: stamp: 'jmv 6/11/2024 10:08:06'!
SmalltalkEditor removeSelector: #fileItIn:!
!methodRemoval: SmalltalkEditor #argPrev: stamp: 'jmv 6/11/2024 09:58:15'!
SmalltalkEditor removeSelector: #argPrev:!
!methodRemoval: SmalltalkEditor #contextualAddParameter: stamp: 'jmv 6/11/2024 10:08:06'!
SmalltalkEditor removeSelector: #contextualAddParameter:!
!methodRemoval: SmalltalkEditor #extractMethod: stamp: 'jmv 6/11/2024 10:08:06'!
SmalltalkEditor removeSelector: #extractMethod:!
!methodRemoval: SmalltalkEditor #inlineTemporaryVariable: stamp: 'jmv 6/11/2024 10:08:06'!
SmalltalkEditor removeSelector: #inlineTemporaryVariable:!
!methodRemoval: SmalltalkEditor #displayIfTrue: stamp: 'jmv 6/11/2024 09:58:15'!
SmalltalkEditor removeSelector: #displayIfTrue:!
!methodRemoval: SmalltalkEditor #acceptAndDebugTest: stamp: 'jmv 6/11/2024 10:08:06'!
SmalltalkEditor removeSelector: #acceptAndDebugTest:!
!methodRemoval: SmalltalkEditor #openMenu: stamp: 'jmv 6/11/2024 10:08:06'!
SmalltalkEditor removeSelector: #openMenu:!
!methodRemoval: SmalltalkEditor #sendersOfIt: stamp: 'jmv 6/11/2024 10:08:06'!
SmalltalkEditor removeSelector: #sendersOfIt:!
!methodRemoval: SmalltalkEditor #doIt: stamp: 'jmv 6/11/2024 10:11:59'!
SmalltalkEditor removeSelector: #doIt:!
!methodRemoval: SmalltalkEditor #acceptAndTestAll: stamp: 'jmv 6/11/2024 10:08:06'!
SmalltalkEditor removeSelector: #acceptAndTestAll:!
!methodRemoval: SmalltalkEditor #cancel: stamp: 'jmv 6/11/2024 09:58:15'!
SmalltalkEditor removeSelector: #cancel:!
!methodRemoval: SmalltalkEditor #contextualRename: stamp: 'jmv 6/11/2024 10:08:06'!
SmalltalkEditor removeSelector: #contextualRename:!
!methodRemoval: SmalltalkEditor #acceptAndTest: stamp: 'jmv 6/11/2024 10:08:06'!
SmalltalkEditor removeSelector: #acceptAndTest:!
!methodRemoval: TextEditor class #cmdShortcutsSpec stamp: 'jmv 6/11/2024 09:56:40'!
TextEditor class removeSelector: #cmdShortcutsSpec!
!methodRemoval: TextEditor class #basicCmdShortcutsSpec stamp: 'jmv 6/11/2024 09:56:42'!
TextEditor class removeSelector: #basicCmdShortcutsSpec!
!methodRemoval: TextEditor #makeUppercase: stamp: 'jmv 6/11/2024 10:08:06'!
TextEditor removeSelector: #makeUppercase:!
!methodRemoval: TextEditor #undo: stamp: 'jmv 6/11/2024 10:08:06'!
TextEditor removeSelector: #undo:!
!methodRemoval: TextEditor #makeCapitalized: stamp: 'jmv 6/11/2024 10:08:06'!
TextEditor removeSelector: #makeCapitalized:!
!methodRemoval: TextEditor #save: stamp: 'jmv 6/11/2024 10:08:06'!
TextEditor removeSelector: #save:!
!methodRemoval: TextEditor #offerFontMenu: stamp: 'jmv 6/11/2024 09:58:15'!
TextEditor removeSelector: #offerFontMenu:!
!methodRemoval: TextEditor #makeLowercase: stamp: 'jmv 6/11/2024 10:08:06'!
TextEditor removeSelector: #makeLowercase:!
!methodRemoval: TextEditor #help: stamp: 'jmv 6/11/2024 10:08:06'!
TextEditor removeSelector: #help:!
!methodRemoval: TextEditor #enclose: stamp: 'jmv 6/11/2024 10:11:42'!
TextEditor removeSelector: #enclose:!
!methodRemoval: TextEditor #compareToClipboard: stamp: 'jmv 6/11/2024 10:08:06'!
TextEditor removeSelector: #compareToClipboard:!
!methodRemoval: TextEditor #findAgain: stamp: 'jmv 6/11/2024 10:08:06'!
TextEditor removeSelector: #findAgain:!
!methodRemoval: TextEditor #offerColorMenu: stamp: 'jmv 6/11/2024 09:58:15'!
TextEditor removeSelector: #offerColorMenu:!
!methodRemoval: TextEditor #changeLineEndsToLf: stamp: 'jmv 6/11/2024 10:08:06'!
TextEditor removeSelector: #changeLineEndsToLf:!
!methodRemoval: TextEditor #redo: stamp: 'jmv 6/11/2024 10:08:06'!
TextEditor removeSelector: #redo:!
!methodRemoval: TextEditor #align stamp: 'jmv 6/11/2024 10:10:26'!
TextEditor removeSelector: #align!
!methodRemoval: TextEditor #setSearchString: stamp: 'jmv 6/11/2024 10:08:06'!
TextEditor removeSelector: #setSearchString:!
!methodRemoval: TextEditor #find: stamp: 'jmv 6/11/2024 10:12:26'!
TextEditor removeSelector: #find:!
!methodRemoval: TextEditor #align: stamp: 'jmv 6/11/2024 09:58:15'!
TextEditor removeSelector: #align:!
!methodRemoval: TextEditor #changeEmphasis: stamp: 'jmv 6/11/2024 09:58:15'!
TextEditor removeSelector: #changeEmphasis:!
!methodRemoval: Editor class #formatShortcutsUsingModifierKey:andSpecs: stamp: 'jmv 6/11/2024 09:54:47'!
Editor class removeSelector: #formatShortcutsUsingModifierKey:andSpecs:!
!methodRemoval: Editor class #allCmdShortcutsSpec stamp: 'jmv 6/11/2024 09:55:30'!
Editor class removeSelector: #allCmdShortcutsSpec!
!methodRemoval: Editor class #basicInitialize stamp: 'jmv 6/11/2024 09:40:25'!
Editor class removeSelector: #basicInitialize!
!methodRemoval: Editor class #cmdShortcutsSpec stamp: 'jmv 6/11/2024 09:56:40'!
Editor class removeSelector: #cmdShortcutsSpec!
!methodRemoval: Editor class #cmdShortcuts stamp: 'jmv 6/11/2024 09:48:17'!
Editor class removeSelector: #cmdShortcuts!
!methodRemoval: Editor class #help stamp: 'jmv 6/11/2024 09:54:35'!
Editor class removeSelector: #help!
!methodRemoval: Editor class #collectCmdShortcutsSpecUsing: stamp: 'jmv 6/11/2024 09:55:59'!
Editor class removeSelector: #collectCmdShortcutsSpecUsing:!
!methodRemoval: Editor class #putIntoCmdShortcuts: stamp: 'jmv 6/11/2024 09:56:05'!
Editor class removeSelector: #putIntoCmdShortcuts:!
!methodRemoval: Editor class #basicCmdShortcutsSpec stamp: 'jmv 6/11/2024 09:56:43'!
Editor class removeSelector: #basicCmdShortcutsSpec!
!methodRemoval: Editor class #initializeCmdShortcuts stamp: 'jmv 6/11/2024 09:50:22'!
Editor class removeSelector: #initializeCmdShortcuts!
!methodRemoval: Editor #selectAll: stamp: 'jmv 6/11/2024 10:08:06'!
Editor removeSelector: #selectAll:!
!methodRemoval: Editor #selectWord: stamp: 'jmv 6/11/2024 10:08:06'!
Editor removeSelector: #selectWord:!
!methodRemoval: Editor #cmdShortcuts stamp: 'jmv 6/11/2024 09:48:03'!
Editor removeSelector: #cmdShortcuts!
!methodRemoval: Editor #paste: stamp: 'jmv 6/11/2024 10:08:06'!
Editor removeSelector: #paste:!
!methodRemoval: Editor #cut: stamp: 'jmv 6/11/2024 10:12:38'!
Editor removeSelector: #cut:!
!methodRemoval: Editor #cursorTopHome: stamp: 'jmv 6/11/2024 10:08:06'!
Editor removeSelector: #cursorTopHome:!
!methodRemoval: Editor #copySelection: stamp: 'jmv 6/11/2024 10:08:06'!
Editor removeSelector: #copySelection:!

!Editor reorganize!
('accessing' currentAttributes help morph morph: userHasEdited)
('accessing-selection' hasSelection markIndex pointIndex selectionInterval selectionIsEmptyOrSeparators startIndex stopIndex)
('clipboard' clipboardStringOrText clipboardTextPut:)
('menu messages' paste pasteString)
('new selection' delimitedRangeAt:hitOnRightHalf:leftDelimiters:rightDelimiters:simpleDelimiters: deselectAndPlaceCursorAt: selectInterval: selectInvisiblyFrom:to: selectInvisiblyMark:point: selectLine selectMark:point: selectWord wordRangeIncluding:)
('nonediting/nontyping keys' cursorDown: cursorLeft: cursorPageDown: cursorPageUp: cursorRight: cursorUp: noop: offerMenuFromEsc:)
('typing/selecting keys' backspace: cursorTopHome enter: newLine: normalCharacter: returnKey: selectAll)
('private' beginningOfLine: beginningOfNextParagraph: beginningOfParagraph: beginningOfText endOfLine: endOfParagraph: endOfText lineSelectAndEmptyCheck: moveCursor:forward:event: nextWordEnd: nextWordEndFrom:goingForwards: nextWordStart: nextWordStartFrom:goingForwards: previousWordStart: privateCurrentString sameColumn:newLine:forward: setIndices:forward: wordSelectAndEmptyCheck:)
('typing support' shortcuts)
('menu & kbd shortcuts' defaultMenuSpec)
!

Editor initialize!

!Editor class reorganize!
('class initialization' initialize initializeShortcuts)
('keyboard shortcut tables' allShiftShortcutsSpec shiftShortcutsSpec shortcuts)
('cached state access' releaseClassCachedState)
!


!TextEditor class reorganize!
('class initialization' initializeShortcuts)
('misc' openHelp)
('cached state access' changeText findText releaseClassCachedState)
('menu-world' worldMenuOptions)
('ui' open)
!


!SmalltalkEditor class reorganize!
('cached state access' releaseClassCachedState)
('explain' plateA plateB plateX)
!

