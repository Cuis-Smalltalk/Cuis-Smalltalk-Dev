'From Cuis 5.0 of 7 November 2016 [latest update: #3168] on 9 September 2017 at 12:15:04 pm'!

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/9/2017 12:09:32'!
hasToRestoreChanges

	^Preferences checkLostChangesOnStartUp and: [ 
		self withChangesFileDo: [ :changesFile | self hasToRestoreChangesFrom: changesFile ]].
	! !

!SystemDictionary methodsFor: 'startup - restore lost changes' stamp: 'HAW 9/9/2017 12:09:46'!
hasToRestoreChangesFrom: changesFile

	| chunk  |
	
	changesFile position: self lastQuitLogPosition.
	[ changesFile atEnd ] whileFalse: [ chunk := changesFile nextChunk ].
	
	^(self isSnapshotQuitOrQuitNoSaveRecord: chunk) not
! !


!Preferences class methodsFor: 'start up' stamp: 'HAW 9/9/2017 12:07:37'!
checkLostChangesOnStartUp
	^ self
		valueOfFlag: #checkLostChangesOnStartUp
		ifAbsent: [ true ].! !


!Preferences class reorganize!
('add preferences' addPreference:categories:default:balloonHelp: addPreference:categories:default:balloonHelp:changeInformee:changeSelector: addPreference:category:default:balloonHelp:)
('fonts' properDisplayAlphaForFonts restoreDefaultFonts setButtonFontTo: setCodeFontTo: setDefaultFonts: setListFontTo: setMenuFontTo: setSystemFontTo: setWindowTitleFontTo: standardButtonFont standardCodeFont standardListFont standardMenuFont subPixelRenderColorFonts windowTitleFont)
('get/set' disable: disableFocusFollowsMouse doesNotUnderstand: enable: enableFocusFollowsMouse setPreference:toValue: togglePreference: valueOfFlag: valueOfFlag:ifAbsent:)
('halos' haloSpecifications iconicHaloSpecifications installHaloSpecsFromArray: installHaloTheme: resetHaloSpecifications)
('hard-coded prefs' desktopMenuTitle)
('initialization' compileAccessMethodForPreference: setPreferencesFrom:)
('menu parameters' menuBorderWidth)
('misc' staggerPolicyString toggleWindowPolicy wantsChangeSetLogging)
('parameters' annotationInfo defaultAnnotationRequests defaultAuthorName setDefaultAnnotationInfo)
('personalization' compileHardCodedPref:enable: disableProgrammerFacilities enableProgrammerFacilities taskbarIncludesAllWindows usePreDebugWindow)
('preference-object access' preferenceAt: preferenceAt:ifAbsent:)
('preferences panel' openPreferencesInspector)
('scrollbar parameters' scrollbarThickness)
('standard queries' aaFontsColormapDepth allowBlockArgumentAssignment alternativeBrowseIt assignmentGlyphSelector automaticPlatformSettings balloonHelpEnabled biggerCursors browseWithPrettyPrint canRecordWhilePlaying caseSensitiveFinds changeSetVersionNumbers cheapWindowReframe checkForSlips clickGrabsMorphs cmdDotEnabled commandClickOpensHalo debugHaloHandle debugLogTimestamp debugShowDamage debugShowDamage2 decorateBrowserButtons diffsInChangeList diffsWithPrettyPrint dismissAllOnOptionClose extraDebuggerButtons focusFollowsMouse focusIndicatorWidth fullScreenLeavesDeskMargins haloEnclosesFullBounds logDebuggerStackToFile menuIcons menuKeyboardControl optionalButtons prettyPrintRectangularBlocks reverseWindowStagger selectionsMayShrink selectiveHalos serverMode showLinesInHierarchyViews soundQuickStart soundStopWhenDone soundsEnabled subPixelRenderFonts syntaxHighlightingAsYouType syntaxHighlightingAsYouTypeAnsiAssignment syntaxHighlightingAsYouTypeLeftArrowAssignment systemWindowEmbedOK tapAndHoldEmulatesButton2 thoroughSenders twentyFourHourFileStamps warnIfNoChangesFile warnIfNoSourcesFile wordStyleCursorMovement)
('menu icons' useAllIcons useBasicIcons useNoIcons)
('themes' cuisDefaults forMouse forTouch slowMachine smalltalk80)
('shout' italicsInShout shoutInWorkspaces useAssignmentGlyphLeftArrow useAssignmentGlyphRightArrow)
('bigger and smaller GUI' bigFonts hugeFonts smallFonts standardFonts tinyFonts veryBigFonts verySmallFonts)
('class state access' parameters preferencesDictionary releaseClassState)
('start up' checkLostChangesOnStartUp)
!

