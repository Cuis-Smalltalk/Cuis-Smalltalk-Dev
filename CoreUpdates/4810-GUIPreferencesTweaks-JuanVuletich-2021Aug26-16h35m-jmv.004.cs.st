'From Cuis 5.0 [latest update: #4809] on 26 August 2021 at 4:59:09 pm'!

!Preferences class methodsFor: 'bigger and smaller GUI element sizes' stamp: 'jmv 8/26/2021 12:05:02'!
roundedButtonRadius
	"Only effective if #roundButtons answers true.
	Provide a reasonable default for subclasses."

	^Preferences standardListFont pointSize * 8 // 14! !

!Preferences class methodsFor: 'bigger and smaller GUI element sizes' stamp: 'jmv 8/26/2021 12:05:21'!
roundedWindowRadius
	"Only effective if #roundWindowCorners answers true.
	Provide a reasonable default for subclasses."

	^Preferences standardListFont pointSize! !


!MorphicCanvas class methodsFor: 'services' stamp: 'jmv 8/26/2021 11:29:32'!
guiSizePreferenceChanged
	"Some preference related to size of gui elements may have changed."! !


!Theme methodsFor: 'other options' stamp: 'jmv 8/26/2021 16:32:33'!
titleBarButtonsExtent
	"Answer the extent to use for close & other title bar buttons.  
	The label height is used to be proportional to the fonts preferences."
	| e |
	e _ Preferences windowTitleFont pointSize * 13 // 10.
	^e@e! !


!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/26/2021 11:34:11'!
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
	Preferences disable: #biggerCursors.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/26/2021 16:10:08'!
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
	Preferences disable: #biggerCursors.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/26/2021 16:10:24'!
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
	Preferences disable: #biggerCursors.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/26/2021 11:44:48'!
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
	Preferences disable: #biggerCursors.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/26/2021 16:11:11'!
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
	Preferences disable: #biggerCursors.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/26/2021 16:11:25'!
defaultFont10
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont10
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 10)
			(setMenuFontTo: 11)
			(setWindowTitleFontTo: 12)
			(setCodeFontTo: 10)
			(setButtonFontTo: 10)
			(setSystemFontTo: 10)).
	Preferences disable: #biggerCursors.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/26/2021 16:11:39'!
defaultFont11
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont11
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 11)
			(setMenuFontTo: 12)
			(setWindowTitleFontTo: 14)
			(setCodeFontTo: 11)
			(setButtonFontTo: 11)
			(setSystemFontTo: 11)).
	Preferences disable: #biggerCursors.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/26/2021 16:11:56'!
defaultFont12
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont12
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 12)
			(setMenuFontTo: 12)
			(setWindowTitleFontTo: 14)
			(setCodeFontTo: 12)
			(setButtonFontTo: 12)
			(setSystemFontTo: 12)).
	Preferences disable: #biggerCursors.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/26/2021 11:34:32'!
defaultFont14
	"Sets not only fonts but other GUI elements
	to fit medium resolution or medium size screens
	Preferences defaultFont14
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 14)
			(setMenuFontTo: 14)
			(setWindowTitleFontTo: 17)
			(setCodeFontTo: 14)
			(setButtonFontTo: 14)
			(setSystemFontTo: 14)).
	Preferences disable: #biggerCursors.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/26/2021 16:12:19'!
defaultFont17
	"Sets not only fonts but other GUI elements
	to fit high resolution or large screens
	Preferences defaultFont17
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 17)
			(setMenuFontTo: 17)
			(setWindowTitleFontTo: 22)
			(setCodeFontTo: 17)
			(setButtonFontTo: 17)
			(setSystemFontTo: 17)).
	Preferences enable: #biggerCursors.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/26/2021 16:12:27'!
defaultFont22
	"Sets not only fonts but other GUI elements
	to fit high resolution or large screens
	Preferences defaultFont22
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 22)
			(setMenuFontTo: 22)
			(setWindowTitleFontTo: 28)
			(setCodeFontTo: 22)
			(setButtonFontTo: 22)
			(setSystemFontTo: 22)).
	Preferences enable: #biggerCursors.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/26/2021 16:13:01'!
defaultFont28
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont28
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 28)
			(setMenuFontTo: 28)
			(setWindowTitleFontTo: 36)
			(setCodeFontTo: 28)
			(setButtonFontTo: 28)
			(setSystemFontTo: 28)).
	Preferences enable: #biggerCursors.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/26/2021 16:13:18'!
defaultFont36
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont36
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 36)
			(setMenuFontTo: 36)
			(setWindowTitleFontTo: 46)
			(setCodeFontTo: 36)
			(setButtonFontTo: 36)
			(setSystemFontTo: 36)).
	Preferences enable: #biggerCursors.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/26/2021 16:13:39'!
defaultFont46
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont46
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 46)
			(setMenuFontTo: 46)
			(setWindowTitleFontTo: 60)
			(setCodeFontTo: 46)
			(setButtonFontTo: 46)
			(setSystemFontTo: 46)).
	Preferences enable: #biggerCursors.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/26/2021 16:14:03'!
defaultFont60
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont60
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 60)
			(setMenuFontTo: 60)
			(setWindowTitleFontTo: 80)
			(setCodeFontTo: 60)
			(setButtonFontTo: 60)
			(setSystemFontTo: 60)).
	Preferences enable: #biggerCursors.! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 8/26/2021 16:14:44'!
defaultFont80
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences defaultFont80
	"

	Preferences
		setDefaultFont: FontFamily defaultFamilyName
		spec: #(
			(setListFontTo: 80)
			(setMenuFontTo: 80)
			(setWindowTitleFontTo: 80)
			(setCodeFontTo: 80)
			(setButtonFontTo: 80)
			(setSystemFontTo: 80)).
	Preferences enable: #biggerCursors.! !

!Preferences class methodsFor: 'fonts' stamp: 'jmv 8/26/2021 16:51:21'!
setDefaultFont: fontFamilyName spec: defaultFontsSpec
		
	| font |
	defaultFontsSpec do: [ :triplet |
		font _ FontFamily familyName: fontFamilyName pointSize: triplet second.
		font ifNil: [ font _ FontFamily defaultFamilyAndPointSize ]. 
		triplet size > 2 ifTrue: [
			font _ font emphasized: triplet third ].
		self
			perform: triplet first
			with: font ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].! !

!Preferences class methodsFor: 'bigger and smaller GUI element sizes' stamp: 'jmv 8/26/2021 11:56:07'!
scrollbarThickness
	"Includes border"
	^Preferences standardListFont pointSize * 9 // 7! !


!SystemWindow methodsFor: 'geometry' stamp: 'jmv 8/26/2021 16:33:48'!
minimumExtent

	^layoutMorph minimumExtent + (borderWidth * 2) + (0@self labelHeight)
		max: Theme current titleBarButtonsExtent x * 6 @ 0! !

!SystemWindow methodsFor: 'geometry' stamp: 'jmv 8/26/2021 16:33:53'!
rescaleButtons
	"boxExtent changed.  Update my buttons."
	| buttonPos buttonExtent  buttonDelta|
	buttonExtent := Theme current titleBarButtonsExtent.
	buttonPos _ self labelHeight + borderWidth - buttonExtent // 2 * (1@1).
	buttonDelta _ buttonExtent x *14//10.
	self submorphsReverseDo: [ :aMorph |
		(aMorph is: #PluggableButtonMorph) 
		  ifTrue: [ 
				aMorph morphExtent: buttonExtent. 
				aMorph morphPosition: buttonPos.
				buttonPos _ buttonPos + (buttonDelta@0).
		].
	]! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 8/26/2021 16:33:15'!
createCloseButton
	^ (PluggableButtonMorph model: self action: #closeBoxHit)
		iconDrawSelector: #drawCloseIcon;
		setBalloonText: 'close this window';
		morphExtent: Theme current titleBarButtonsExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 8/26/2021 16:33:20'!
createCollapseButton
	^(PluggableButtonMorph model: self action: #collapse)
		iconDrawSelector: #drawCollapseIcon;
		setBalloonText: 'collapse this window';
		morphExtent: Theme current titleBarButtonsExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 8/26/2021 16:33:24'!
createExpandButton
	^ (PluggableButtonMorph model: self action: #expandBoxHit)
		iconDrawSelector: #drawExpandIcon;
		setBalloonText: 'expand to full screen';
		morphExtent: Theme current titleBarButtonsExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 8/26/2021 16:33:28'!
createMenuButton
	^ (PluggableButtonMorph model: self action: #offerWindowMenu)
		iconDrawSelector: #drawMenuIcon;
		setBalloonText: 'window menu';
		morphExtent: Theme current titleBarButtonsExtent! !

!SystemWindow methodsFor: 'initialization' stamp: 'jmv 8/26/2021 16:33:37'!
initializeLabelArea
	"Initialize the label area (titlebar) for the window."

	| topLeft spacing |
	topLeft _ (self labelHeight + borderWidth - Theme current titleBarButtonsExtent / 2) ceiling asPoint.
	spacing _ Theme current titleBarButtonsExtent x *14//10.
	self addMorph: self createCloseButton position: topLeft.
	self addMorph: self createCollapseButton position: spacing@0 + topLeft.
	self addMorph: self createExpandButton position: spacing*2@0 + topLeft.
	self addMorph: self createMenuButton position: spacing*3@0 + topLeft! !


!MenuMorph methodsFor: 'construction' stamp: 'jmv 8/26/2021 16:58:52'!
addStayUpIcons
	| closeButton pinButton w buttonHW |
	Preferences optionalButtons ifFalse: [ ^self ].
	(self valueOfProperty: #hasStayUpIcons ifAbsent: [ false ])
		ifTrue: [
		 	self removeProperty: #needsStayUpIcons.
			^self ].
	titleMorph ifNil: [
		"Title not yet there. Flag ourself, so this method is called again when adding title."
		self setProperty: #needsStayUpIcons toValue: true.
		^ self].
	buttonHW _ Theme current titleBarButtonsExtent x.
	closeButton _ PluggableButtonMorph model: self action: #delete.
	closeButton iconDrawSelector: #drawCloseIcon; color: `Color transparent`.
	pinButton _ PluggableButtonMorph model: self action: #stayUp.
	pinButton iconDrawSelector: #drawPushPinIcon; color: `Color transparent`.
	w _ (titleMorph hasSubmorphs ifTrue: [ titleMorph firstSubmorph morphWidth ] ifFalse: [ 0 ]) + 60.
	self addMorphFront: 
		(LayoutMorph newRow
			"Make room for buttons"
			morphExtent: w @ (titleMorph morphHeight max: buttonHW);
			color: `Color transparent`;
			addMorph: closeButton fixedWidth: buttonHW;
			addMorph: (WidgetMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//7;
			addMorph: titleMorph proportionalWidth: 1;
			addMorph: (WidgetMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//7;
			addMorph: pinButton fixedWidth: buttonHW;
			addMorph: (WidgetMorph new noBorder color: `Color transparent`) fixedWidth: buttonHW//3).

	self setProperty: #hasStayUpIcons toValue: true.
	self removeProperty: #needsStayUpIcons! !

!MenuMorph methodsFor: 'menu' stamp: 'jmv 8/26/2021 16:46:43'!
removeStayUpBox
	| box |
	submorphs isEmpty ifTrue: [^self].
	(submorphs first is: #LayoutMorph) ifFalse: [^self].
	box _ submorphs first submorphs second.
	(box is: #PluggableButtonMorph) 
		ifTrue: [ box hide ]! !


!Theme methodsFor: 'other options' stamp: 'jmv 8/26/2021 11:55:18'!
roundedButtonRadius
	"Only effective if #roundButtons answers true.
	Provide a reasonable default for subclasses."
	^ Preferences roundedButtonRadius! !

!Theme methodsFor: 'other options' stamp: 'jmv 8/26/2021 11:55:29'!
roundedWindowRadius
	"Only effective if #roundWindowCorners answers true.
	Provide a reasonable default for subclasses."
	^Preferences roundedWindowRadius! !

!methodRemoval: SystemWindow #titleBarButtonsExtent stamp: 'jmv 8/26/2021 16:40:08'!
SystemWindow removeSelector: #titleBarButtonsExtent!

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
('personalization' askConfirmationOnQuit askToInstallComplexPackage compileHardCodedPref:enable: disableProgrammerFacilities enableProgrammerFacilities fullPrintItInWorkspaces stylingWithEmphasisInWorkspaces usePreDebugWindow)
('preference-object access' preferenceAt: preferenceAt:ifAbsent:)
('preferences panel' openPreferencesInspector)
('scrollbar parameters')
('shout' backgroundColorFillsAllBackground highlightBlockNesting italicsInShout shoutInWorkspaces useAlwaysLeftArrow useAssignmentGlyphLeftArrow)
('standard queries' aaFontsColormapDepth allowBlockArgumentAssignment alternativeBrowseIt assignmentGlyphSelector autoNumberUserChanges automaticPlatformSettings balloonHelpEnabled biggerCursors browseWithPrettyPrint canRecordWhilePlaying caseSensitiveFinds changeSetVersionNumbers cheapWindowReframe checkForSlips clickGrabsMorphs cmdDotEnabled ctrlArrowsScrollHorizontally debugHaloHandle debugLogTimestamp debugShowDamage debugShowDamage2 decorateBrowserButtons diffsInChangeList diffsWithPrettyPrint dismissAllOnOptionClose drawKeyboardFocusIndicator extraDebuggerButtons focusFollowsMouse focusIndicatorWidth fullScreenLeavesDeskMargins haloEnclosesFullBounds halosShowCoordinateSystem leftArrowAssignmentsInGeneratedCode listClassesHierarchically logDebuggerStackToFile menuKeyboardControl optionalButtons prettyPrintRectangularBlocks selectionsMayShrink selectiveHalos serverMode showAnnotations showLinesInHierarchyViews soundQuickStart soundStopWhenDone soundsEnabled subPixelRenderFonts syntaxHighlightingAsYouType syntaxHighlightingAsYouTypeAnsiAssignment syntaxHighlightingAsYouTypeLeftArrowAssignment systemWindowEmbedOK tapAndHoldEmulatesButton2 thoroughSenders tileResizerInWindowMenu transcriptLogVerbose wantsMenuIcons warnIfNoChangesFile warnIfNoSourcesFile)
('start up' checkLostChangesOnStartUp)
('themes' cuisDefaults forMouse forTouch slowMachine smalltalk80)
('user changes' use:asUserChangesFileNameExtensionWhile: userChangesFileNameExtension)
('file list' addInitialFileListDirectory: initialFileListDirectories initialFileListDirectories: isInitialFileListDirectory: removeInitialFileListDirectory:)
('system startup' initClassCachedState)
('bigger and smaller GUI element sizes' roundedButtonRadius roundedWindowRadius scrollbarThickness)
!

