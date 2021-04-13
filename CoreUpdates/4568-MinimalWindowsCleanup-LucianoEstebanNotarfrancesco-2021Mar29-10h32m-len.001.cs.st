'From Cuis 5.0 [latest update: #4550] on 29 March 2021 at 10:53:54 am'!

!Theme methodsFor: 'other options' stamp: 'len 3/29/2021 10:36:18'!
buttonBorderWidth
	^ 1! !

!Theme methodsFor: 'other options' stamp: 'len 3/29/2021 10:34:25'!
menuBorderWidth
	^ self roundWindowCorners ifTrue: [0] ifFalse: [1]! !

!Theme methodsFor: 'other options' stamp: 'len 3/29/2021 10:46:06'!
scrollbarShowButtons
	^ true! !

!Theme methodsFor: 'other options' stamp: 'len 3/29/2021 10:39:24'!
windowBorderWidth
	| w |
	w _ Preferences standardListFont pointSize / 11.
	w _ w * (self roundWindowCorners ifTrue: [4] ifFalse: [2]).
	^ w rounded max: 1! !


!PluggableButtonMorph methodsFor: 'initialization' stamp: 'len 3/29/2021 10:36:35'!
defaultBorderWidth
	^ Theme current buttonBorderWidth! !


!SystemWindow methodsFor: 'drawing' stamp: 'len 3/29/2021 10:41:37'!
drawOn: aCanvas
	| titleColor roundCorners |
	titleColor _ self widgetsColor.
	self isTopWindow
		ifTrue: [ titleColor _ titleColor lighter ].
	roundCorners _ Theme current roundWindowCorners.
	roundCorners
		ifTrue: [
			"Round corners. Optional title gradient."
			self drawRoundedFrameOn: aCanvas color: titleColor ]
		ifFalse: [
			"No round corners. No title gradient."
			self drawClassicFrameOn: aCanvas color: titleColor ].
	labelString ifNotNil: [ self drawLabelOn: aCanvas ]! !

!SystemWindow methodsFor: 'initialization' stamp: 'len 3/29/2021 10:39:47'!
defaultBorderWidth
	"Answer the default border width for the receiver."
	^ Theme current windowBorderWidth! !

!SystemWindow methodsFor: 'initialization' stamp: 'len 3/29/2021 10:42:48'!
titleBarButtonsExtent
	"Answer the extent to use for close & other title bar buttons.  
	The label height is used to be proportional to the fonts preferences."
	| e |
	e _ Preferences windowTitleFont pointSize * 13 // 10.
	^e@e! !

!SystemWindow methodsFor: 'label' stamp: 'len 3/29/2021 10:42:01'!
labelHeight
	"Answer the height for the window label."
	^ Preferences windowTitleFont lineSpacing+1! !


!BrowserWindow methodsFor: 'GUI building' stamp: 'len 3/29/2021 10:52:17'!
buildMorphicClassColumnWith: classList
	| column |
	column _ LayoutMorph newColumn.
	column
		addMorphUseAll: classList;
		addAdjusterAndMorph: self buildMorphicSwitches fixedHeight: Theme current buttonPaneHeight.
	^column! !


!ScrollBar methodsFor: 'geometry' stamp: 'len 3/29/2021 10:46:54'!
computeSlider

	| delta |
	delta _ (Theme current scrollbarShowButtons ifTrue: [self buttonExtent] ifFalse: [0]) + (self freeSliderRoom * value) asInteger.
	self isHorizontal
		ifTrue: [
			slider morphPosition: borderWidth +  delta @ borderWidth ]
		ifFalse: [
			slider morphPosition: borderWidth @ (borderWidth + delta) ] ! !

!ScrollBar methodsFor: 'geometry' stamp: 'len 3/29/2021 10:47:46'!
freeSliderRoom
	"Answer the length or height of the free slider area, i.e. subtract the slider itself.
	If we are really too short of room, lie a little bit. Answering at least 4, even when the
	free space might be actually negative, makes the scrollbar somewhat usable."
	| buttonsRoom |
	buttonsRoom _ Theme current scrollbarShowButtons ifTrue: [self buttonExtent * 2] ifFalse: [0].
	^ ((self isHorizontal
		ifTrue: [extent x - slider morphWidth]
		ifFalse: [extent y - slider morphHeight])
			- (borderWidth * 2) - buttonsRoom) max: 4! !

!ScrollBar methodsFor: 'initialization' stamp: 'len 3/29/2021 10:48:07'!
initializeDownButton
	"initialize the receiver's downButton"

	| e |
	e _ self buttonExtent.
	downButton _ self buttonClass new.
	downButton model: self.
	downButton morphExtent: e@e.
	Theme current scrollbarShowButtons ifFalse: [^ self].
	self addMorph: downButton position: extent - borderWidth - e.
	downButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down".
	self isHorizontal
		ifTrue: [ self updateRightButton: downButton ]
		ifFalse: [ self updateDownButton: downButton ]! !

!ScrollBar methodsFor: 'initialization' stamp: 'len 3/29/2021 10:48:16'!
initializeUpButton
	"initialize the receiver's upButton"

	| e |
	e _ self buttonExtent.
	upButton _ self buttonClass new.
	upButton model: self.
	upButton morphExtent: e@e.
	Theme current scrollbarShowButtons ifTrue: [^ self].
	self addMorph: upButton position: borderWidth@borderWidth.
	upButton
		actWhen: #buttonStillDown.		"to enable multiple action if held down"
	self isHorizontal
		ifTrue: [ self updateLeftButton: upButton ]
		ifFalse: [ self updateUpButton: upButton ].
! !


!MenuMorph methodsFor: 'initialization' stamp: 'len 3/29/2021 10:35:08'!
defaultBorderWidth
	^ Theme current menuBorderWidth! !

!methodRemoval: Theme #minimalWindows stamp: 'len 3/29/2021 10:52:57'!
Theme removeSelector: #minimalWindows!
!methodRemoval: Theme #showScrollbarButtons stamp: 'len 3/29/2021 10:46:09'!
Theme removeSelector: #showScrollbarButtons!
!methodRemoval: Theme #steButtons stamp: 'len 3/29/2021 10:44:46'!
Theme removeSelector: #steButtons!
!methodRemoval: Preferences class #menuBorderWidth stamp: 'len 3/29/2021 10:35:24'!
Preferences class removeSelector: #menuBorderWidth!
!methodRemoval: Preferences class #systemWindowBorderSize stamp: 'len 3/29/2021 10:39:59'!
Preferences class removeSelector: #systemWindowBorderSize!

!Preferences class reorganize!
('add preferences' addPreference:categories:default:balloonHelp: addPreference:categories:default:balloonHelp:changeInformee:changeSelector: addPreference:category:default:balloonHelp:)
('bigger and smaller GUI' bigFonts defaultFont05 defaultFont06 defaultFont07 defaultFont08 defaultFont09 defaultFont10 defaultFont11 defaultFont12 defaultFont14 defaultFont17 defaultFont22 defaultFont28 defaultFont36 defaultFont46 defaultFont60 defaultFont80 haloHandleSize hugeFonts smallFonts standardFonts tinyFonts veryBigFonts verySmallFonts)
('class finder' classFinder classFinder: restoreDefaultClassFinder)
('class state access' parameters preferencesDictionary releaseClassState)
('code generation' leftArrowAssignmentsInGeneratedCodeWithComputedDefault)
('exception handling' allowNonLocalReturnsInExceptionHandlers warnAboutNonLocalReturnsInExceptionHandlers)
('fonts' loadOnlyLatinGlyphData properDisplayAlphaForFonts restoreDefaultFonts setButtonFontTo: setCodeFontTo: setDefaultFont: setDefaultFont:spec: setListFontTo: setMenuFontTo: setSystemFontTo: setWindowTitleFontTo: standardButtonFont standardCodeFont standardListFont standardMenuFont subPixelRenderColorFonts windowTitleFont)
('get/set' disable: disableFocusFollowsMouse disableTileResizerInWindowMenu doesNotUnderstand: enable: enableFocusFollowsMouse enableTileResizerInWindowMenu setPreference:toValue: togglePreference: valueOfFlag: valueOfFlag:ifAbsent:)
('halos' haloSpecifications iconicHaloSpecifications installHaloSpecsFromArray: installHaloTheme: resetHaloSpecifications)
('hard-coded prefs' desktopMenuTitle)
('initialization' compileAccessMethodForPreference: setPreferencesFrom:)
('menu icons' useMenuIcons useNoMenuIcons)
('misc' wantsChangeSetLogging)
('parameters' annotationInfo defaultAnnotationRequests defaultAuthorName setDefaultAnnotationInfo)
('personalization' askToInstallComplexPackage compileHardCodedPref:enable: disableProgrammerFacilities enableProgrammerFacilities fullPrintItInWorkspaces stylingWithEmphasisInWorkspaces usePreDebugWindow)
('preference-object access' preferenceAt: preferenceAt:ifAbsent:)
('preferences panel' openPreferencesInspector)
('scrollbar parameters' scrollbarThickness)
('shout' backgroundColorFillsAllBackground highlightBlockNesting italicsInShout shoutInWorkspaces useAlwaysLeftArrow useAssignmentGlyphLeftArrow)
('standard queries' aaFontsColormapDepth allowBlockArgumentAssignment alternativeBrowseIt assignmentGlyphSelector autoNumberUserChanges automaticPlatformSettings balloonHelpEnabled biggerCursors browseWithPrettyPrint canRecordWhilePlaying caseSensitiveFinds changeSetVersionNumbers cheapWindowReframe checkForSlips clickGrabsMorphs cmdDotEnabled ctrlArrowsScrollHorizontally debugHaloHandle debugLogTimestamp debugShowDamage debugShowDamage2 decorateBrowserButtons diffsInChangeList diffsWithPrettyPrint dismissAllOnOptionClose extraDebuggerButtons focusFollowsMouse focusIndicatorWidth fullScreenLeavesDeskMargins haloEnclosesFullBounds halosShowCoordinateSystem leftArrowAssignmentsInGeneratedCode listClassesHierarchically logDebuggerStackToFile menuKeyboardControl optionalButtons prettyPrintRectangularBlocks selectionsMayShrink selectiveHalos serverMode showAnnotations showLinesInHierarchyViews soundQuickStart soundStopWhenDone soundsEnabled subPixelRenderFonts syntaxHighlightingAsYouType syntaxHighlightingAsYouTypeAnsiAssignment syntaxHighlightingAsYouTypeLeftArrowAssignment systemWindowEmbedOK tapAndHoldEmulatesButton2 thoroughSenders tileResizerInWindowMenu transcriptLogVerbose wantsMenuIcons warnIfNoChangesFile warnIfNoSourcesFile)
('start up' checkLostChangesOnStartUp)
('themes' cuisDefaults forMouse forTouch slowMachine smalltalk80)
('user changes' use:asUserChangesFileNameExtensionWhile: userChangesFileNameExtension)
!

