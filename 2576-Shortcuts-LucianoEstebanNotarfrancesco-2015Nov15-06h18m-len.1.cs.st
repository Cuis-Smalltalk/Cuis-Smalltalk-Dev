'From Cuis 4.2 of 25 July 2013 [latest update: #2571] on 15 November 2015 at 6:47:41.753762 am'!

!Theme methodsFor: 'keyboard shortcuts' stamp: 'len 11/15/2015 06:45'!
keyStroke: aKeyboardEvent morph: aMorph
	aKeyboardEvent controlKeyPressed ifTrue: [^false].
	aKeyboardEvent commandAltKeyPressed ifFalse: [^false].

"	aKeyboardEvent keyValue = 9
		ifTrue: [aKeyboardEvent inspect. ^true].
"
	aKeyboardEvent keyValue = $x asciiValue
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w delete. ^true ]].
	aKeyboardEvent keyValue = $m asciiValue
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w fullScreen. ^true ]].

	aKeyboardEvent keyValue = $[ asciiValue
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w resizeLeft. ^true ]].
	aKeyboardEvent keyValue = $] asciiValue
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w resizeRight. ^true ]].
	aKeyboardEvent keyValue = ${ asciiValue
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w resizeTop. ^true ]].
	aKeyboardEvent keyValue = $} asciiValue
		ifTrue: [aMorph owningWindow ifNotNil: [ :w | w resizeBottom. ^true ]].

	"open new windows"
	aKeyboardEvent keyValue = $B asciiValue
		ifTrue: [BrowserWindow openBrowser. ^true].
	aKeyboardEvent keyValue = $W asciiValue
		ifTrue: [Workspace new contents: ''; openLabel: 'Worksspace'. ^true].
"	aKeyboardEvent keyValue = $T asciiValue
		ifTrue: [TranscriptWindow openTranscript. ^true].
	aKeyboardEvent keyValue = $P asciiValue
		ifTrue: [CodePackageListWindow openPackageList. ^true].
	aKeyboardEvent keyValue = $F asciiValue
		ifTrue: [FileListWindow openFileList. ^true].
	aKeyboardEvent keyValue = $C asciiValue
		ifTrue: [ChangeSorterWindow open: ChangeSorter new label: nil. ^true].
"
	^ false! !


!Morph methodsFor: 'events' stamp: 'len 11/15/2015 06:45'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event."

	(Theme current keyStroke: aKeyboardEvent morph: self)
		ifTrue: [^ self].
	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
		
	"Allow instances to dynamically use properties for handling common events."
	self 
		valueOfProperty: #'keyStroke:'
		ifPresentDo: [ :handler | handler value: aKeyboardEvent ]! !


!PluggableScrollPane methodsFor: 'events' stamp: 'len 11/15/2015 06:46'!
keyStroke: aKeyboardEvent

	(Theme current keyStroke: aKeyboardEvent morph: self)
		ifTrue: [^ self].
	( self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	super keyStroke: aKeyboardEvent.
	(self scrollByKeyboard: aKeyboardEvent)
		ifTrue: [ ^self ].
	scroller keyStroke: aKeyboardEvent! !


!HierarchicalListMorph methodsFor: 'events' stamp: 'len 11/15/2015 06:45'!
keyStroke: aKeyboardEvent 
	"Process potential command keys"

	| args aCharacter |
	(Theme current keyStroke: aKeyboardEvent morph: self)
		ifTrue: [^ self].
	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self scrollByKeyboard: aKeyboardEvent)
		ifTrue: [ ^self ].
	aCharacter _ aKeyboardEvent keyCharacter.
	(self arrowKey: aCharacter)
		ifTrue: [ ^self ].
	keystrokeActionSelector ifNil: [^self].
	(args _ keystrokeActionSelector numArgs) = 1 
		ifTrue: [^mainView perform: keystrokeActionSelector with: aCharacter].
	args = 2 
		ifTrue: [
			^mainView 
				perform: keystrokeActionSelector
				with: aCharacter
				with: self].
	^self error: 'The keystrokeActionSelector must be a 1- or 2-keyword symbol'! !


!PluggableListMorph methodsFor: 'events' stamp: 'len 11/15/2015 06:46'!
keyStroke: aKeyboardEvent 
	"Process keys"
	
	| aCharacter |
	(Theme current keyStroke: aKeyboardEvent morph: self)
		ifTrue: [^ self].
	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	(self scrollByKeyboard: aKeyboardEvent) 
		ifTrue: [ ^self ].
	aCharacter _ aKeyboardEvent keyCharacter.
	(self arrowKey: aCharacter)
		ifTrue: [ ^self ].
	aCharacter asciiValue = 27 ifTrue: [	" escape key"
		^ self mouseButton2Activity].
	aKeyboardEvent anyModifierKeyPressed
		ifTrue: [
			(self keystrokeAction: aCharacter)
				ifTrue: [ ^self ]].
	^ self keyboardSearch: aCharacter! !


!TextModelMorph methodsFor: 'events' stamp: 'len 11/15/2015 06:46'!
keyStroke: aKeyboardEvent
	"A keystroke was hit while the receiver had keyboard focus.  Pass the keywtroke on to my textMorph, and and also, if I have an event handler, pass it on to that handler"

	(Theme current keyStroke: aKeyboardEvent morph: self)
		ifTrue: [^ self].
	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
	self textMorph keyStroke: aKeyboardEvent! !


!InnerTextMorph methodsFor: 'events' stamp: 'len 11/15/2015 06:45'!
keyStroke: aKeyboardEvent

	(Theme current keyStroke: aKeyboardEvent morph: self)
		ifTrue: [^ self].
	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].
		
	"Maybe disable? Precludes the use of up and down arrows with control,
	that are standard keystrokes in Windows to control the cursor.
	Problem: At least Mac and Win VM generate ctrl-up and ctrl-down for
	mouse wheel events.
	I guess most people would prefer the mouse wheel to work properly..."
	(owner scrollByKeyboard: aKeyboardEvent)
		ifTrue: [ ^self ].

	autoCompleter 
		ifNil: [ self processKeyStroke: aKeyboardEvent ]
		ifNotNil: [
			autoCompleter
				autoCompletionAround: [ self processKeyStroke: aKeyboardEvent ]
				keyStroke: aKeyboardEvent ]! !


!OneLineEditorMorph methodsFor: 'events' stamp: 'len 11/15/2015 06:45'!
keyStroke: aKeyboardEvent
	"Handle a keystroke event."

	(Theme current keyStroke: aKeyboardEvent morph: self)
		ifTrue: [^ self].

	(self focusKeyboardFor: aKeyboardEvent)
		ifTrue: [ ^ self ].

	"Return - check for special action
	Note: Code below assumes that this was some
	input field reacting on Return."
	aKeyboardEvent isReturnKey ifTrue: [
		crAction ifNotNil: [
			"Break the keyboard focus so that the receiver can be safely deleted."
			"Is this needed at all? (jmv)"
			"aKeyboardEvent hand newKeyboardFocus: nil."
			^crAction value ]].

	self pauseBlinking.
	self handleInteraction: [ self editor processKeyStroke: aKeyboardEvent ].
	self updateFromContents.
	super keyStroke: aKeyboardEvent  "sends to keyStroke event handler, if any"! !


!Theme reorganize!
('colors' background buttonColorFrom: buttonLabel focusIndicator line missingCommentTextColor paneBackgroundFrom: scrollbarButtonColor scrollbarSliderShadowColor text textCursor textHighlight textHighlightFocused: unfocusedTextHighlightFrom: windowLabel)
('menu colors' menu menuHighlight menuText menuTitleBar)
('tool colors' browser changeList changeSorter debugger defaultWindowColor dualChangeSorter fileContentsBrowser fileList messageNames messageSet object packageList testRunner textEditor transcript versionsBrowser workspace)
('widget colors' acceptButton cancelButton listHighlightFocused: listMultiHighlightFocused: listSelectedRowText listUnselectedRowText textPane)
('other options' buttonGradientBottomFactor buttonGradientHeight buttonGradientTopFactor buttonPaneHeight embossedButtonLabels embossedTitles layoutAdjusterThickness minimalWindows roundButtons roundWindowCorners roundedButtonRadius roundedWindowRadius scrollbarThickness steButtons titleGradientBottomFactor titleGradientExtraLightness titleGradientTopFactor useButtonGradient useTaskbar useUniformColors useWindowTitleGradient)
('private - shout mappings' ansiAssignment argumentTypes assignment blockLevelFive blockLevelFour blockLevelSeven blockLevelSix blockTemps defaults firstBlockLevel generateShoutConfig globals incompleteMessages instVar italic literals messages methodTags pattern pseudoVariables return symbols tempBar tempVars undefined)
('menus' allIcons basicIcons menuDecorations miscellaneousIcons noIcons)
('icons' acceptIcon addressBookIcon appearanceIcon blankIcon cancelIcon changesIcon chatIcon classIcon clockIcon closeIcon collapseIcon copyIcon cutIcon dateIcon debugIcon deleteIcon developmentIcon displayIcon doItIcon editFindReplaceIcon emblemImportantIcon exitFullscreenIcon expandIcon exploreIcon fileOutIcon findIcon fontXGenericIcon formatJustifyCenterIcon formatJustifyFillIcon formatJustifyLeftIcon formatJustifyRightIcon genericTextIcon globeIcon goBottomIcon goDownIcon goTopIcon goUpIcon graphicsIcon halfRefreshIcon haloCollapseIcon haloColorIcon haloDebugIcon haloDismissIcon haloDragIcon haloDuplicateIcon haloFontEmphasisIcon haloFontSizeIcon haloGrabIcon haloHelpIcon haloMenuIcon haloRotateIcon haloScaleIcon helpIcon inspectIcon instanceIcon junkIcon keyboardShortcutsIcon listAddIcon listRemoveIcon mailForwardIcon mailMarkJunkIcon mediaPlaybackStartIcon morphsIcon newFolderIcon newIcon newWindowIcon openIcon packageIcon pasteIcon preferencesDesktopFontIcon preferencesIcon printIcon printerIcon pushPinIcon quitIcon redoIcon removableMediaIcon saveAndQuitIcon saveAsIcon saveAsNewVersionIcon saveIcon scriptIcon selectAllIcon sendReceiveIcon speadsheetTemplateIcon spreadsheetIcon stillCameraIcon switchIcon systemFileManagerIcon systemIcon systemMonitorIcon terminalIcon textEditorIcon undoIcon updateIcon usersIcon viewFullscreenIcon wallpaperIcon warningIcon weatherFewCloudsIcon windowIcon windowMenuIcon worldIcon)
('icon lookup' appendExtensionToContentSpec: fetch: prepend:toContentSpec:)
('accessing' decorateMenu:)
('shout' shout)
('initialization' initialize)
('keyboard shortcuts' keyStroke:morph:)
!

