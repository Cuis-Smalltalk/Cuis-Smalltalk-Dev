'From Cuis 5.0 of 7 November 2016 [latest update: #3397] on 9 August 2018 at 10:46:43 am'!

!DirectoryEntry methodsFor: 'enumeration' stamp: 'jmv 8/9/2018 10:41:27'!
allChildrenDo: aBlock
	self childrenDo: [ :child | 
		aBlock value: child ].
	self allDirectoriesDo: [ :child |
		child allChildrenDo: aBlock]! !


!FeatureRequirement methodsFor: 'private' stamp: 'jmv 8/9/2018 10:45:45'!
withPackageSubfoldersOf: aDirectoryEntry do: aBlock

	"Look in the requested directory"
	aBlock value: aDirectoryEntry.

	"Look in ./Packages/ and subfolders"
	aDirectoryEntry / 'Packages' ifExists: [ :packagesFolder |
		aBlock value: packagesFolder.
		packagesFolder allDirectoriesDo: aBlock ]! !


!Theme reorganize!
('colors' background buttonColorFrom: buttonLabel errorColor failureColor focusIndicator line missingCommentTextColor paneBackgroundFrom: scrollbarButtonColor scrollbarColor scrollbarSliderShadowColor successColor text textCursor textHighlight textHighlightFocused: unfocusedTextHighlightFrom: windowLabel)
('menu colors' menu menuHighlight menuText menuTitleBar)
('tool colors' browser changeList changeSorter debugger defaultWindowColor dualChangeSorter fileContentsBrowser fileList messageNames messageSet object packageList testRunner textEditor transcript versionsBrowser workspace)
('widget colors' acceptButton cancelButton listHighlightFocused: listMultiHighlightFocused: listSelectedRowText listUnselectedRowText textPane)
('other options' buttonGradientBottomFactor buttonGradientHeight buttonGradientTopFactor buttonPaneHeight embossedButtonLabels embossedTitles fullScreenDeskMargin layoutAdjusterThickness minimalWindows roundButtons roundWindowCorners roundedButtonRadius roundedWindowRadius scrollbarThickness steButtons titleGradientBottomFactor titleGradientExtraLightness titleGradientTopFactor useButtonGradient useTaskbar useUniformColors useWindowTitleGradient)
('private - shout mappings' ansiAssignment argumentTypes assignment blockLevelFive blockLevelFour blockLevelOne blockLevelSeven blockLevelSix blockLevelThree blockLevelTwo blockLevelZero blockTemps defaults generateShoutConfig globals incompleteMessages instVar italic literals messages methodTags pattern pseudoVariables return symbols tempBar tempVars undefined)
('icons' acceptIcon addressBookIcon appearanceIcon blankIcon cancelIcon changesIcon chatIcon classIcon clockIcon closeIcon collapseIcon copyIcon cutIcon dateIcon debugIcon deleteIcon developmentIcon displayIcon doItIcon editFindReplaceIcon emblemImportantIcon exitFullscreenIcon expandIcon exploreIcon fileOutIcon findIcon fontXGenericIcon formatJustifyCenterIcon formatJustifyFillIcon formatJustifyLeftIcon formatJustifyRightIcon genericTextIcon globeIcon goBottomIcon goDownIcon goTopIcon goUpIcon graphicsIcon halfRefreshIcon haloCollapseIcon haloColorIcon haloDebugIcon haloDismissIcon haloDragIcon haloDuplicateIcon haloFontEmphasisIcon haloFontSizeIcon haloGrabIcon haloHelpIcon haloMenuIcon haloRotateIcon haloScaleIcon helpIcon inspectIcon instanceIcon junkIcon keyboardShortcutsIcon listAddIcon listRemoveIcon mailForwardIcon mailMarkJunkIcon mediaPlaybackStartIcon morphsIcon newFolderIcon newIcon newWindowIcon openIcon packageIcon pasteIcon preferencesDesktopFontIcon preferencesIcon printIcon printerIcon pushPinIcon quitIcon redoIcon removableMediaIcon saveAndQuitIcon saveAsIcon saveAsNewVersionIcon saveIcon scriptIcon selectAllIcon sendReceiveIcon speadsheetTemplateIcon spreadsheetIcon stillCameraIcon switchIcon systemFileManagerIcon systemIcon systemMonitorIcon terminalIcon textEditorIcon undoIcon updateIcon usersIcon viewFullscreenIcon wallpaperIcon warningIcon weatherFewCloudsIcon windowIcon windowMenuIcon worldIcon)
('icon lookup' appendExtensionToContentSpec: fetch: prepend:toContentSpec:)
('shout' shout)
('keyboard shortcuts' keyStroke:morph:)
('as yet unclassified' windowClosed: windowOpen:)
!

