'From Cuis 6.0 [latest update: #5069] on 21 February 2022 at 2:32:51 pm'!

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/21/2022 14:24:44'!
largeFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences largeFonts
	"

	self defaultFontSize: 18.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/21/2022 14:25:01'!
veryLargeFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences veryLargeFonts
	"

	self defaultFontSize: 24.! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 2/21/2022 14:27:24'!
defaultFontSize: pointSize
	"
	Adjust sizes for GUI elements.
	
	Preferences defaultFontSize: 5
	Preferences defaultFontSize: 6
	
	Preferences defaultFontSize: 7
	Preferences defaultFontSize: 9
	Preferences defaultFontSize: 11
	Preferences defaultFontSize: 14
	Preferences defaultFontSize: 18
	Preferences defaultFontSize: 24
	Preferences defaultFontSize: 32
	"
	| font titleFont |
	font _ FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize..
	self
		setListFontTo: font;
		setMenuFontTo: font;
		setCodeFontTo: font;
		setButtonFontTo: font;
		setSystemFontTo: font.
	titleFont _ FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize * 12//10.
	self setWindowTitleFontTo: titleFont.
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
	pointSize > 14
		ifTrue: [ Preferences enable: #biggerCursors ]
		ifFalse: [ Preferences disable: #biggerCursors ].
	Cursor defaultCursor activateCursor.
	^font! !


!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/21/2022 14:24:37'!
hugeFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences hugeFonts
	"

	self defaultFontSize: 32.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/21/2022 14:24:48'!
smallFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences smallFonts
	"
	
	self defaultFontSize: 11.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/21/2022 14:24:52'!
standardFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences standardFonts
	"

	self defaultFontSize: 14.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/21/2022 14:24:57'!
tinyFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences tinyFonts
	"

	self defaultFontSize: 7.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 2/21/2022 14:25:05'!
verySmallFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences verySmallFonts
	"
	
	self defaultFontSize: 9.! !


!Form class methodsFor: 'creation - anti aliased' stamp: 'jmv 2/21/2022 13:23:58'!
topLeftCorner: r height: height gradientTop: gradientTopFactor gradientBottom: gradientBottomFactor
	"Create and answer a Form with a vertical gray gradient as specified for the topLeft corner of a rounded rectangle"
	| f bw topColor bottomColor l d c w |
	topColor _ `Color white` * gradientTopFactor.
	bottomColor _ `Color white` * gradientBottomFactor.
	w _ r.
	bw _ 1.3.
	f _ Form
		extent: w @ height
		depth: 32.
	0
		to: height - 1
		do: [ :y |
			c _ bottomColor
				mixed: 1.0 * y / (height - 1 max: 1)
				with: topColor.
			0
				to: w - 1
				do: [ :x |
					l _ 1.0.
					y < r ifTrue: [
						d _ (w - 1 - x @ (w - 1 - y)) r.
						l _ ((r - d max: 0.0) min: bw) / bw ].
					f
						colorAt: x @ y
						put: (c alpha: l) ]].
	^ f! !


!Theme class methodsFor: 'user interface' stamp: 'jmv 2/21/2022 13:29:28'!
changeFontSizes

	(MenuMorph new defaultTarget: Preferences)
		addTitle: 'Make GUI elements';
		addStayUpIcons;
		add: 'Huge' action: #hugeFonts;
		add: 'Very Large' action: #veryLargeFonts;
		add: 'Large' action: #largeFonts;
		add: 'Regular Size' action: #standardFonts;
		add: 'Small' action: #smallFonts;
		add: 'Very Small'action: #verySmallFonts;
		add: 'Tiny'action: #tinyFonts;
		popUpInWorld: self runningWorld! !

!methodRemoval: Preferences class #defaultFont07 stamp: 'jmv 2/21/2022 14:30:05'!
Preferences class removeSelector: #defaultFont07!
!methodRemoval: Preferences class #defaultFont08 stamp: 'jmv 2/21/2022 14:30:07'!
Preferences class removeSelector: #defaultFont08!
!methodRemoval: Preferences class #veryBigFonts stamp: 'jmv 2/21/2022 14:31:02'!
Preferences class removeSelector: #veryBigFonts!
!methodRemoval: Preferences class #defaultFont17 stamp: 'jmv 2/21/2022 14:30:15'!
Preferences class removeSelector: #defaultFont17!
!methodRemoval: Preferences class #defaultFont22 stamp: 'jmv 2/21/2022 14:30:56'!
Preferences class removeSelector: #defaultFont22!
!methodRemoval: Preferences class #defaultFont14 stamp: 'jmv 2/21/2022 14:30:14'!
Preferences class removeSelector: #defaultFont14!
!methodRemoval: Preferences class #defaultFont46 stamp: 'jmv 2/21/2022 14:31:05'!
Preferences class removeSelector: #defaultFont46!
!methodRemoval: Preferences class #defaultFont28 stamp: 'jmv 2/21/2022 14:30:58'!
Preferences class removeSelector: #defaultFont28!
!methodRemoval: Preferences class #defaultFont06 stamp: 'jmv 2/21/2022 14:30:03'!
Preferences class removeSelector: #defaultFont06!
!methodRemoval: Preferences class #defaultFont60 stamp: 'jmv 2/21/2022 14:31:06'!
Preferences class removeSelector: #defaultFont60!
!methodRemoval: Preferences class #defaultFont11 stamp: 'jmv 2/21/2022 14:30:12'!
Preferences class removeSelector: #defaultFont11!
!methodRemoval: Preferences class #defaultFont12 stamp: 'jmv 2/21/2022 14:30:13'!
Preferences class removeSelector: #defaultFont12!
!methodRemoval: Preferences class #defaultFont10 stamp: 'jmv 2/21/2022 14:30:09'!
Preferences class removeSelector: #defaultFont10!
!methodRemoval: Preferences class #defaultFont05 stamp: 'jmv 2/21/2022 14:30:02'!
Preferences class removeSelector: #defaultFont05!
!methodRemoval: Preferences class #bigFonts stamp: 'jmv 2/21/2022 14:30:52'!
Preferences class removeSelector: #bigFonts!
!methodRemoval: Preferences class #defaultFont80 stamp: 'jmv 2/21/2022 14:31:07'!
Preferences class removeSelector: #defaultFont80!
!methodRemoval: Preferences class #defaultFont36 stamp: 'jmv 2/21/2022 14:31:03'!
Preferences class removeSelector: #defaultFont36!
!methodRemoval: Preferences class #defaultFont09 stamp: 'jmv 2/21/2022 14:30:08'!
Preferences class removeSelector: #defaultFont09!

!Preferences class reorganize!
('add preferences' addPreference:categories:default:balloonHelp: addPreference:categories:default:balloonHelp:changeInformee:changeSelector: addPreference:category:default:balloonHelp:)
('bigger and smaller GUI' haloHandleSize hugeFonts largeFonts roundedButtonRadius roundedWindowRadius scrollbarThickness smallFonts standardFonts tinyFonts veryLargeFonts verySmallFonts)
('class finder' classFinder classFinder: restoreDefaultClassFinder)
('class state access' parameters preferencesDictionary releaseClassState)
('code generation' leftArrowAssignmentsInGeneratedCodeWithComputedDefault)
('exception handling' allowNonLocalReturnsInExceptionHandlers warnAboutNonLocalReturnsInExceptionHandlers)
('fonts' cacheTrueTypeGlyphs defaultFontSize: loadOnlyLatinGlyphData properDisplayAlphaForFonts restoreDefaultFonts setButtonFontTo: setCodeFontTo: setDefaultFont: setDefaultFont:spec: setListFontTo: setMenuFontTo: setSystemFontTo: setWindowTitleFontTo: standardButtonFont standardCodeFont standardListFont standardMenuFont subPixelRenderColorFonts windowTitleFont)
('get/set' disable: disableFocusFollowsMouse disableTileResizerInWindowMenu doesNotUnderstand: enable: enableFocusFollowsMouse enableTileResizerInWindowMenu setPreference:toValue: togglePreference: valueOfFlag: valueOfFlag:ifAbsent:)
('halos' haloSpecifications iconicHaloSpecifications installHaloSpecsFromArray: installHaloTheme: resetHaloSpecifications)
('hard-coded prefs' desktopMenuTitle)
('initialization' compileAccessMethodForPreference: setPreferencesFrom:)
('menu icons' useMenuIcons useNoMenuIcons)
('misc' cacheDisplayContentWhenMovingMorphs wantsChangeSetLogging)
('parameters' annotationInfo classAnnotations defaultAuthorName methodAnnotations setDefaultAnnotationsInBrowsers setQuickAnnotationsInBrowsers systemCategoryAnnotations)
('personalization' askConfirmationOnQuit askToInstallComplexPackage compileHardCodedPref:enable: disableProgrammerFacilities enableProgrammerFacilities fullPrintItInWorkspaces stylingWithEmphasisInWorkspaces usePreDebugWindow)
('preference-object access' preferenceAt: preferenceAt:ifAbsent:)
('preferences panel' openPreferencesInspector)
('shout' backgroundColorFillsAllBackground highlightBlockNesting italicsInShout shoutInWorkspaces useAlwaysLeftArrow useAssignmentGlyphLeftArrow)
('standard queries' aaFontsColormapDepth allowBlockArgumentAssignment alternativeBrowseIt assignmentGlyphSelector autoNumberUserChanges automaticPlatformSettings balloonHelpEnabled biggerCursors browseWithPrettyPrint canRecordWhilePlaying caseSensitiveFinds changeSetVersionNumbers cheapWindowReframe checkForSlips clickGrabsMorphs cmdDotEnabled cmdDotInterruptTakesStatistics ctrlArrowsScrollHorizontally debugHaloHandle debugLogTimestamp debugShowDamage debugShowDamage2 decorateBrowserButtons diffsInChangeList diffsWithPrettyPrint dismissAllOnOptionClose drawKeyboardFocusIndicator extraDebuggerButtons focusFollowsMouse focusIndicatorWidth fullScreenLeavesDeskMargins haloEnclosesFullBounds halosShowCoordinateSystem leftArrowAssignmentsInGeneratedCode listClassesHierarchically logDebuggerStackToFile menuKeyboardControl optionalButtons prettyPrintRectangularBlocks selectionsMayShrink selectiveHalos serverMode showAnnotations showLinesInHierarchyViews soundQuickStart soundStopWhenDone soundsEnabled subPixelRenderFonts syntaxHighlightingAsYouType syntaxHighlightingAsYouTypeAnsiAssignment syntaxHighlightingAsYouTypeLeftArrowAssignment systemWindowEmbedOK tapAndHoldEmulatesButton2 thoroughSenders tileResizerInWindowMenu transcriptLogVerbose wantsMenuIcons warnIfNoChangesFile warnIfNoSourcesFile)
('start up' checkLostChangesOnStartUp)
('themes' cuisDefaults forMouse forTouch slowMachine smalltalk80)
('user changes' use:asUserChangesFileNameExtensionWhile: userChangesFileNameExtension)
('file list' addInitialFileListDirectory: initialFileListDirectories initialFileListDirectories: isInitialFileListDirectory: removeInitialFileListDirectory:)
('system startup' initClassCachedState)
!

