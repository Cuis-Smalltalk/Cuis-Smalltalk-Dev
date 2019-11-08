'From Cuis 5.0 [latest update: #3944] on 31 October 2019 at 3:42:59 pm'!

!SystemDictionary methodsFor: 'image, changes name' stamp: 'HAW 10/31/2019 13:15:44'!
defaultUserChangesName
	"Answer the default full path to the changes file corresponding to the image file name."
	"
	Smalltalk defaultUserChangesName
	"
	^(FileIOAccessor default baseNameFor: self imageName), '.user.changes'! !

!methodRemoval: Preferences class #userChangesFileName stamp: 'HAW 10/31/2019 15:38:25'!
Preferences class removeSelector: #userChangesFileName!
!methodRemoval: Preferences class #defaultUserChangesFileName stamp: 'HAW 10/31/2019 15:38:32'!
Preferences class removeSelector: #defaultUserChangesFileName!

!Preferences class reorganize!
('add preferences' addPreference:categories:default:balloonHelp: addPreference:categories:default:balloonHelp:changeInformee:changeSelector: addPreference:category:default:balloonHelp:)
('bigger and smaller GUI' bigFonts defaultFont05 defaultFont06 defaultFont07 defaultFont08 defaultFont09 defaultFont10 defaultFont11 defaultFont12 defaultFont14 defaultFont17 defaultFont22 defaultFont28 defaultFont36 defaultFont46 defaultFont60 defaultFont80 hugeFonts smallFonts standardFonts tinyFonts veryBigFonts verySmallFonts)
('class state access' parameters preferencesDictionary releaseClassState)
('code generation' leftArrowAssignmentsInGeneratedCodeWithComputedDefault)
('exception handling' allowNonLocalReturnsInExceptionHandlers warnAboutNonLocalReturnsInExceptionHandlers)
('fonts' properDisplayAlphaForFonts restoreDefaultFonts setButtonFontTo: setCodeFontTo: setDefaultFont:spec: setDefaultFonts: setListFontTo: setMenuFontTo: setSystemFontTo: setWindowTitleFontTo: standardButtonFont standardCodeFont standardListFont standardMenuFont subPixelRenderColorFonts windowTitleFont)
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
('standard queries' aaFontsColormapDepth allowBlockArgumentAssignment alternativeBrowseIt assignmentGlyphSelector automaticPlatformSettings balloonHelpEnabled biggerCursors browseWithPrettyPrint canRecordWhilePlaying caseSensitiveFinds changeSetVersionNumbers cheapWindowReframe checkForSlips clickGrabsMorphs cmdDotEnabled ctrlArrowsScrollHorizontally debugHaloHandle debugLogTimestamp debugShowDamage debugShowDamage2 decorateBrowserButtons diffsInChangeList diffsWithPrettyPrint dismissAllOnOptionClose extraDebuggerButtons focusFollowsMouse focusIndicatorWidth fullScreenLeavesDeskMargins haloEnclosesFullBounds leftArrowAssignmentsInGeneratedCode listClassesHierarchically logDebuggerStackToFile menuKeyboardControl optionalButtons prettyPrintRectangularBlocks selectionsMayShrink selectiveHalos serverMode showLinesInHierarchyViews soundQuickStart soundStopWhenDone soundsEnabled subPixelRenderFonts syntaxHighlightingAsYouType syntaxHighlightingAsYouTypeAnsiAssignment syntaxHighlightingAsYouTypeLeftArrowAssignment systemWindowEmbedOK tapAndHoldEmulatesButton2 thoroughSenders tileResizerInWindowMenu wantsMenuIcons warnIfNoChangesFile warnIfNoSourcesFile)
('start up' checkLostChangesOnStartUp)
('themes' cuisDefaults forMouse forTouch slowMachine smalltalk80)
!

