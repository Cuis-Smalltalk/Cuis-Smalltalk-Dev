'From Cuis 5.0 [latest update: #3866] on 7 September 2019 at 8:37:40 pm'!

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/7/2019 20:20:19'!
bigFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences bigFonts
	"

	self defaultFont17! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/7/2019 20:34:57'!
defaultFont05
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences defaultFont05
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 5)
			(setMenuFontTo: 6)
			(setWindowTitleFontTo: 7)
			(setCodeFontTo: 5)
			(setButtonFontTo: 5)
			(setSystemFontTo: 6)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/7/2019 20:34:46'!
defaultFont06
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences defaultFont06
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 6)
			(setMenuFontTo: 7)
			(setWindowTitleFontTo: 8)
			(setCodeFontTo: 6)
			(setButtonFontTo: 6)
			(setSystemFontTo: 7)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/7/2019 20:34:28'!
defaultFont07
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences defaultFont07
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 7)
			(setMenuFontTo: 8)
			(setWindowTitleFontTo: 9)
			(setCodeFontTo: 7)
			(setButtonFontTo: 7)
			(setSystemFontTo: 8)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/7/2019 20:34:19'!
defaultFont08
	"Sets not only fonts but other GUI elements
	to fit low resolution or small screens
	Preferences defaultFont08
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 8)
			(setMenuFontTo: 9)
			(setWindowTitleFontTo: 10)
			(setCodeFontTo: 8)
			(setButtonFontTo: 8)
			(setSystemFontTo: 8)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/7/2019 20:33:57'!
defaultFont09
	"Sets not only fonts but other GUI elements
	to fit low resolution or small screens
	Preferences defaultFont09
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 9)
			(setMenuFontTo: 10)
			(setWindowTitleFontTo: 11)
			(setCodeFontTo: 9)
			(setButtonFontTo: 9)
			(setSystemFontTo: 9)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/7/2019 20:20:33'!
hugeFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences hugeFonts
	"

	self defaultFont28! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/7/2019 20:24:07'!
smallFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences smallFonts
	"
	
	self defaultFont11! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/7/2019 20:20:10'!
standardFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences standardFonts
	"

	self defaultFont14! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/7/2019 20:24:24'!
tinyFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences tinyFonts
	"

	self defaultFont06! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/7/2019 20:20:29'!
veryBigFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences veryBigFonts
	"

	self defaultFont22! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 9/7/2019 20:24:18'!
verySmallFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences verySmallFonts
	"
	
	self defaultFont08! !


!Preferences class reorganize!
('add preferences' addPreference:categories:default:balloonHelp: addPreference:categories:default:balloonHelp:changeInformee:changeSelector: addPreference:category:default:balloonHelp:)
('bigger and smaller GUI' bigFonts defaultFont05 defaultFont06 defaultFont07 defaultFont08 defaultFont09 defaultFont10 defaultFont11 defaultFont12 defaultFont14 defaultFont17 defaultFont22 defaultFont28 defaultFont36 defaultFont46 defaultFont60 defaultFont80 hugeFonts smallFonts standardFonts tinyFonts veryBigFonts verySmallFonts)
('class state access' parameters preferencesDictionary releaseClassState)
('code generation' leftArrowAssignmentsInGeneratedCodeWithComputedDefault)
('fonts' properDisplayAlphaForFonts restoreDefaultFonts setButtonFontTo: setCodeFontTo: setDefaultFont:spec: setDefaultFonts: setListFontTo: setMenuFontTo: setSystemFontTo: setWindowTitleFontTo: standardButtonFont standardCodeFont standardListFont standardMenuFont subPixelRenderColorFonts subPixelRenderFonts windowTitleFont)
('get/set' disable: disableFocusFollowsMouse disableTileResizerInWindowMenu doesNotUnderstand: enable: enableFocusFollowsMouse enableTileResizerInWindowMenu setPreference:toValue: togglePreference: valueOfFlag: valueOfFlag:ifAbsent:)
('halos' haloSpecifications iconicHaloSpecifications installHaloSpecsFromArray: installHaloTheme: resetHaloSpecifications)
('hard-coded prefs' desktopMenuTitle)
('initialization' compileAccessMethodForPreference: setPreferencesFrom:)
('menu icons' useMenuIcons useNoMenuIcons)
('menu parameters' menuBorderWidth)
('misc' wantsChangeSetLogging)
('parameters' annotationInfo defaultAnnotationRequests defaultAuthorName setDefaultAnnotationInfo)
('personalization' compileHardCodedPref:enable: disableProgrammerFacilities enableProgrammerFacilities usePreDebugWindow)
('preference-object access' preferenceAt: preferenceAt:ifAbsent:)
('preferences panel' openPreferencesInspector)
('scrollbar parameters' scrollbarThickness)
('shout' backgroundColorFillsAllBackground highlightBlockNesting italicsInShout shoutInWorkspaces useAssignmentGlyphLeftArrow useAssignmentGlyphRightArrow)
('standard queries' aaFontsColormapDepth allowBlockArgumentAssignment alternativeBrowseIt assignmentGlyphSelector automaticPlatformSettings balloonHelpEnabled biggerCursors browseWithPrettyPrint canRecordWhilePlaying caseSensitiveFinds changeSetVersionNumbers cheapWindowReframe checkForSlips clickGrabsMorphs cmdDotEnabled ctrlArrowsScrollHorizontally debugHaloHandle debugLogTimestamp debugShowDamage debugShowDamage2 decorateBrowserButtons diffsInChangeList diffsWithPrettyPrint dismissAllOnOptionClose extraDebuggerButtons focusFollowsMouse focusIndicatorWidth fullScreenLeavesDeskMargins haloEnclosesFullBounds leftArrowAssignmentsInGeneratedCode listClassesHierarchically logDebuggerStackToFile menuKeyboardControl optionalButtons prettyPrintRectangularBlocks selectionsMayShrink selectiveHalos serverMode showLinesInHierarchyViews soundQuickStart soundStopWhenDone soundsEnabled syntaxHighlightingAsYouType syntaxHighlightingAsYouTypeAnsiAssignment syntaxHighlightingAsYouTypeLeftArrowAssignment systemWindowEmbedOK tapAndHoldEmulatesButton2 thoroughSenders tileResizerInWindowMenu wantsMenuIcons warnIfNoChangesFile warnIfNoSourcesFile)
('start up' checkLostChangesOnStartUp)
('themes' cuisDefaults forMouse forTouch slowMachine smalltalk80)
!

