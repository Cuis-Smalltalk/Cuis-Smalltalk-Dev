'From Cuis6.3 [latest update: #6297] on 25 March 2024 at 3:56:51 pm'!

!PluggableMorph methodsFor: 'access' stamp: 'jmv 3/25/2024 15:52:36'!
adoptWidgetsColor: paneColor
	"If the Theme wants to keep the color it specifies and that was set in #defaultColor,
	then don't change it."
	self useThemeColor ifFalse: [
		super adoptWidgetsColor: paneColor ].! !

!PluggableMorph methodsFor: 'testing' stamp: 'jmv 3/25/2024 15:51:50'!
useThemeColor
	^Theme current useThemeColorForPluggableMorph! !


!PluggableButtonMorph methodsFor: 'testing' stamp: 'jmv 3/25/2024 15:51:55'!
useThemeColor
	^Theme current useThemeColorForPluggableButton! !


!PluggableScrollPane methodsFor: 'testing' stamp: 'jmv 3/25/2024 15:51:58'!
useThemeColor
	^Theme current useThemeColorForPluggableScrollPane! !


!HierarchicalListMorph methodsFor: 'testing' stamp: 'jmv 3/25/2024 15:52:01'!
useThemeColor
	^Theme current useThemeColorForHierarchicalList! !


!PluggableListMorph methodsFor: 'testing' stamp: 'jmv 3/25/2024 15:52:19'!
useThemeColor
	^Theme current useThemeColorForPluggableList! !


!TextModelMorph methodsFor: 'testing' stamp: 'jmv 3/25/2024 15:52:04'!
useThemeColor
	^Theme current useThemeColorForTextPane! !


!ScrollBar methodsFor: 'testing' stamp: 'jmv 3/25/2024 15:52:07'!
useThemeColor
	^Theme current useThemeColorForScrollBar! !


!Theme methodsFor: 'widget color testing' stamp: 'jmv 3/25/2024 15:49:31'!
useThemeColorForHierarchicalList

	^false! !

!Theme methodsFor: 'widget color testing' stamp: 'jmv 3/25/2024 15:49:36'!
useThemeColorForPluggableButton

	^false! !

!Theme methodsFor: 'widget color testing' stamp: 'jmv 3/25/2024 15:49:40'!
useThemeColorForPluggableList

	^false! !

!Theme methodsFor: 'widget color testing' stamp: 'jmv 3/25/2024 15:49:46'!
useThemeColorForPluggableMorph

	^false! !

!Theme methodsFor: 'widget color testing' stamp: 'jmv 3/25/2024 15:50:00'!
useThemeColorForPluggableScrollPane

	^false! !

!Theme methodsFor: 'widget color testing' stamp: 'jmv 3/25/2024 15:50:04'!
useThemeColorForScrollBar

	^false! !

!Theme methodsFor: 'widget color testing' stamp: 'jmv 3/25/2024 15:50:09'!
useThemeColorForTextPane

	^false! !


!PluggableButtonMorph methodsFor: 'accessing' stamp: 'jmv 3/25/2024 15:52:40'!
adoptWidgetsColor: paneColor
	"If the Theme wants to keep the color it specifies and that was set in #defaultColor,
	then don't change it."
	self useThemeColor ifFalse: [
		super adoptWidgetsColor: paneColor.
		mouseOverColor ifNil: [
			self color: (Theme current buttonColorFrom: paneColor) ]].! !


!PluggableScrollPane methodsFor: 'access' stamp: 'jmv 3/25/2024 15:52:43'!
adoptWidgetsColor: paneColor
	"If the Theme wants to keep the color it specifies and that was set in #defaultColor,
	then don't change it."
	self useThemeColor ifFalse: [
		color := Theme current paneBackgroundFrom: paneColor.
		super adoptWidgetsColor: paneColor.
		scrollBar adoptWidgetsColor: paneColor.
		hScrollBar adoptWidgetsColor: paneColor ].! !


!ScrollBar methodsFor: 'accessing' stamp: 'jmv 3/25/2024 15:52:46'!
adoptWidgetsColor: paneColor
	"If the Theme wants to keep the color it specifies and that was set in #defaultColor,
	then don't change it."
	self useThemeColor ifFalse: [
		self color: paneColor ].! !


!Theme reorganize!
('colors' background buttonColorFrom: buttonLabel errorColor failureColor focusIndicator line missingCommentTextColor paneBackgroundFrom: scrollbarButtonColor scrollbarColor scrollbarSliderShadowColor successColor text textCursor textEmptyDisplayMessage textHighlight textHighlightFocused: unfocusedTextHighlightFrom: windowLabel)
('menu colors' menu menuHighlight menuText menuTitleBar)
('tool colors' autoCompleter autoCompleterDefaultBorderColor autoCompleterMaybeInvalid browser changeList changeSorter debugger defaultWindowColor dualChangeSorter fileContentsBrowser fileList messageNames messageSet packageDownloader packageInstaller packageList testRunner textEditor transcript versionsBrowser workspace)
('widget colors' acceptButton cancelButton hoverHelp listHighlightFocused: listMultiHighlightFocused: textPane)
('widget color testing' useThemeColorForHierarchicalList useThemeColorForPluggableButton useThemeColorForPluggableList useThemeColorForPluggableMorph useThemeColorForPluggableScrollPane useThemeColorForScrollBar useThemeColorForTextPane)
('other options' buttonBorderWidth buttonPaneHeight embossedButtonLabels embossedTitles fullScreenDeskMargin layoutAdjusterThickness menuBorderWidth roundButtons roundWindowCorners roundedButtonRadius roundedWindowRadius scrollbarShowButtons scrollbarThickness titleBarButtonsExtent useTaskbar useUniformColors windowBorderWidth)
('private - shout mappings' ansiAssignment argumentTypes assignment blockLevelFive blockLevelFour blockLevelOne blockLevelSeven blockLevelSix blockLevelThree blockLevelTwo blockLevelZero blockTemps defaults generateShoutConfig globals incompleteMessages instVar italic literals messages methodTags pattern pseudoVariables return symbols tempBar tempVars undefined)
('icons' acceptIcon addressBookIcon appearanceIcon blankIcon cancelIcon changesIcon chatIcon classIcon clockIcon closeIcon collapseIcon copyIcon cutIcon dateIcon debugIcon deleteIcon developmentIcon displayIcon doItIcon editFindReplaceIcon emblemImportantIcon exitFullscreenIcon expandIcon exploreIcon fileOutIcon findIcon fontIcon fontXGenericIcon formatJustifyCenterIcon formatJustifyFillIcon formatJustifyLeftIcon formatJustifyRightIcon genericTextIcon globeIcon goBottomIcon goDownIcon goTopIcon goUpIcon graphicsIcon halfRefreshIcon haloCollapseIcon haloColorIcon haloDebugIcon haloDismissIcon haloDragIcon haloDuplicateIcon haloFontEmphasisIcon haloFontSizeIcon haloGrabIcon haloHelpIcon haloMenuIcon haloResizeIcon haloRotateIcon haloScaleIcon helpIcon imageIcon inspectIcon instanceIcon junkIcon keyboardShortcutsIcon listAddIcon listRemoveIcon mailForwardIcon mailMarkJunkIcon mediaPlaybackStartIcon morphsIcon newFolderIcon newIcon newWindowIcon openIcon packageIcon pasteIcon playIcon preferencesDesktopFontIcon preferencesIcon printIcon printerIcon pushPinIcon quitIcon redoIcon refreshIcon removableMediaIcon resizeBottomIcon resizeBottomLeftIcon resizeBottomRightIcon resizeFullIcon resizeLeftIcon resizeRightIcon resizeTopIcon resizeTopLeftIcon resizeTopRightIcon saveAndQuitIcon saveAsIcon saveAsNewVersionIcon saveIcon scriptIcon selectAllIcon sendReceiveIcon speadsheetTemplateIcon spreadsheetIcon stillCameraIcon stopIcon switchIcon systemFileManagerIcon systemIcon systemMonitorIcon terminalIcon textEditorIcon undoIcon updateIcon usersIcon viewFullscreenIcon wallpaperIcon warningIcon weatherFewCloudsIcon windowIcon windowMenuIcon worldIcon)
('icon lookup' appendExtensionToContentSpec: fetch: prepend:toContentSpec:)
('shout' shout)
!

