'From Cuis 5.0 [latest update: #3923] on 18 October 2019 at 10:50:50 am'!

!Preferences class methodsFor: 'exception handling' stamp: 'jmv 10/18/2019 10:49:40'!
allowNonLocalReturnsInExceptionHandlers
	^ self
		valueOfFlag: #allowNonLocalReturnsInExceptionHandlers
		ifAbsent: [ true ].! !

!Preferences class methodsFor: 'exception handling' stamp: 'jmv 10/18/2019 10:49:45'!
warnAboutNonLocalReturnsInExceptionHandlers
	^ self
		valueOfFlag: #warnAboutNonLocalReturnsInExceptionHandlers
		ifAbsent: [ true ].! !


!Exception methodsFor: 'priv handling' stamp: 'jmv 10/18/2019 10:50:17'!
evaluateHandlerBlock: aBlock

	| handlerEx |
	handlerBlockNotCurtailed := false.
	^[
			| answer |
			answer _ [aBlock valueWithPossibleArgument: self] on: Exception do: [:ex | handlerEx _ ex.  ex pass].
			signalContext := nil.	"To enable recycling of exceptions, but only after handler block has finished execution."
			handlerBlockNotCurtailed _ true.
			answer
	] ifCurtailed:
		[
			signalContext := nil.	"To enable recycling of exceptions, but only after handler block has finished execution."
			(handlerBlockNotCurtailed not and: [handlerEx isNil or: [handlerEx handlerBlockNotCurtailed not]])
				ifTrue: [
					"Please see
					https://lists.cuis.st/mailman/archives/cuis-dev/2019-October/000800.html
					https://lists.cuis.st/mailman/archives/cuis-dev/2019-October/000809.html
					Also see the rest of the tread in detail.
					This is work in progress."
					Preferences allowNonLocalReturnsInExceptionHandlers
						ifFalse: [ self error: 'Exception handler blocks must not do non local returns' ]
						ifTrue: [
							Preferences warnAboutNonLocalReturnsInExceptionHandlers
								ifTrue: [ 'It is advisable to avoid method returns (non local returns) in exception handler blocks' print ].
							handlerBlockNotCurtailed _ true ].
					]
				ifFalse: [handlerBlockNotCurtailed _ true]
		]! !

!Exception methodsFor: 'signaling' stamp: 'jmv 10/18/2019 09:50:15'!
signalIn: aContext
	"Ask ContextHandlers in the sender chain to handle this signal.  The default is to execute and return my defaultAction."

	signalContext ifNotNil: [self error: 'This exception has already been signaled and its handler block is being executed.'].
	signalContext _ aContext contextTag.
	^ aContext nextHandlerContext handleSignal: self! !


!Preferences class reorganize!
('add preferences' addPreference:categories:default:balloonHelp: addPreference:categories:default:balloonHelp:changeInformee:changeSelector: addPreference:category:default:balloonHelp:)
('bigger and smaller GUI' bigFonts defaultFont05 defaultFont06 defaultFont07 defaultFont08 defaultFont09 defaultFont10 defaultFont11 defaultFont12 defaultFont14 defaultFont17 defaultFont22 defaultFont28 defaultFont36 defaultFont46 defaultFont60 defaultFont80 hugeFonts smallFonts standardFonts tinyFonts veryBigFonts verySmallFonts)
('class state access' parameters preferencesDictionary releaseClassState)
('code generation' leftArrowAssignmentsInGeneratedCodeWithComputedDefault)
('exception handling' allowNonLocalReturnsInExceptionHandlers warnAboutNonLocalReturnsInExceptionHandlers)
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

