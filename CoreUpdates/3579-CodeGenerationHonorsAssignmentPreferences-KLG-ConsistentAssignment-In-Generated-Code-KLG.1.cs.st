'From Cuis 5.0 of 7 November 2016 [latest update: #3564] on 16 January 2019 at 12:05:04 pm'!

!Preferences class methodsFor: 'code generation' stamp: 'KLG 1/16/2019 11:51:32'!
leftArrowAssignmentsInGeneratedCodeWithComputedDefault
	^ self
		valueOfFlag: #leftArrowAssignmentsInGeneratedCode
		ifAbsent: [ 
			self syntaxHighlightingAsYouTypeLeftArrowAssignment or: [
				self syntaxHighlightingAsYouTypeAnsiAssignment not ]].! !


!Browser methodsFor: 'class functions' stamp: 'KLG 1/16/2019 11:52:48'!
createInstVarAccessors
	"Create getters and setters for all inst vars defined at the level of the current class selection, except do NOT clobber or override any selectors already understood by the instances of the selected class"
	self selectedClassOrMetaClass ifNotNil: [ :aClass |
		aClass instVarNames do: [ :aName | | newMessage setter |
			(aClass canUnderstand: aName asSymbol) ifFalse: [
				newMessage _ aName , '
	"Answer the value of ' , aName , '"

	^ ' , aName.
				aClass
					compile: newMessage
					classified: 'accessing'
					notifying: nil ].
			(aClass canUnderstand: (setter _ aName , ':') asSymbol) ifFalse: [
				newMessage _ setter , ' anObject
	"Set the value of ' , aName , '"

	' , aName , ' ' ,
					(Preferences leftArrowAssignmentsInGeneratedCodeWithComputedDefault
						ifTrue: [ '_' ]
						ifFalse: [ ':=' ]) , ' anObject'.
				aClass
					compile: newMessage
					classified: 'accessing'
					notifying: nil ]]]! !


!Message methodsFor: 'stub creation' stamp: 'KLG 1/16/2019 11:51:59'!
addSetterCodeOn: stream 
						
	stream
		newLine; tab;
		nextPutAll: selector allButLast;
		nextPutAll: (Preferences leftArrowAssignmentsInGeneratedCodeWithComputedDefault
			ifTrue: [ ' _ ' ]
			ifFalse: [ ' := ' ]);
		nextPutAll: self arguments first argumentName ! !


!Preferences class methodsFor: 'standard queries' stamp: 'KLG 1/16/2019 12:03:07'!
leftArrowAssignmentsInGeneratedCode
	^ self
		valueOfFlag: #leftArrowAssignmentsInGeneratedCode
		ifAbsent: [ false ]! !


!Preferences class reorganize!
('add preferences' addPreference:categories:default:balloonHelp: addPreference:categories:default:balloonHelp:changeInformee:changeSelector: addPreference:category:default:balloonHelp:)
('fonts' defaultFontFamily properDisplayAlphaForFonts restoreDefaultFonts setButtonFontTo: setCodeFontTo: setDefaultFont:spec: setDefaultFontFamilyTo: setDefaultFonts: setListFontTo: setMenuFontTo: setSystemFontTo: setWindowTitleFontTo: standardButtonFont standardCodeFont standardListFont standardMenuFont subPixelRenderColorFonts windowTitleFont)
('get/set' disable: disableFocusFollowsMouse disableTileResizerInWindowMenu doesNotUnderstand: enable: enableFocusFollowsMouse enableTileResizerInWindowMenu setPreference:toValue: togglePreference: valueOfFlag: valueOfFlag:ifAbsent:)
('halos' haloSpecifications iconicHaloSpecifications installHaloSpecsFromArray: installHaloTheme: resetHaloSpecifications)
('hard-coded prefs' desktopMenuTitle)
('initialization' compileAccessMethodForPreference: setPreferencesFrom:)
('menu parameters' menuBorderWidth)
('misc' wantsChangeSetLogging)
('parameters' annotationInfo defaultAnnotationRequests defaultAuthorName setDefaultAnnotationInfo)
('personalization' compileHardCodedPref:enable: disableProgrammerFacilities enableProgrammerFacilities taskbarIncludesAllWindows usePreDebugWindow)
('preference-object access' preferenceAt: preferenceAt:ifAbsent:)
('preferences panel' openPreferencesInspector)
('scrollbar parameters' scrollbarThickness)
('standard queries' aaFontsColormapDepth allowBlockArgumentAssignment alternativeBrowseIt assignmentGlyphSelector automaticPlatformSettings balloonHelpEnabled biggerCursors browseWithPrettyPrint canRecordWhilePlaying caseSensitiveFinds changeSetVersionNumbers cheapWindowReframe checkForSlips clickGrabsMorphs cmdDotEnabled ctrlArrowsScrollHorizontally debugHaloHandle debugLogTimestamp debugShowDamage debugShowDamage2 decorateBrowserButtons diffsInChangeList diffsWithPrettyPrint dismissAllOnOptionClose extraDebuggerButtons focusFollowsMouse focusIndicatorWidth fullScreenLeavesDeskMargins haloEnclosesFullBounds leftArrowAssignmentsInGeneratedCode listClassesHierarchically logDebuggerStackToFile menuKeyboardControl optionalButtons prettyPrintRectangularBlocks selectionsMayShrink selectiveHalos serverMode showLinesInHierarchyViews soundQuickStart soundStopWhenDone soundsEnabled subPixelRenderFonts syntaxHighlightingAsYouType syntaxHighlightingAsYouTypeAnsiAssignment syntaxHighlightingAsYouTypeLeftArrowAssignment systemWindowEmbedOK tapAndHoldEmulatesButton2 thoroughSenders tileResizerInWindowMenu wantsMenuIcons warnIfNoChangesFile warnIfNoSourcesFile)
('menu icons' useMenuIcons useNoMenuIcons)
('themes' cuisDefaults forMouse forTouch slowMachine smalltalk80)
('shout' backgroundColorFillsAllBackground highlightBlockNesting italicsInShout shoutInWorkspaces useAssignmentGlyphLeftArrow useAssignmentGlyphRightArrow)
('bigger and smaller GUI' bigFonts hugeFonts smallFonts standardFonts tinyFonts veryBigFonts verySmallFonts)
('class state access' parameters preferencesDictionary releaseClassState)
('start up' checkLostChangesOnStartUp)
('code generation' leftArrowAssignmentsInGeneratedCodeWithComputedDefault)
!

