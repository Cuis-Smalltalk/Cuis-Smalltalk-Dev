'From Haver 5.0 [latest update: #4789] on 23 August 2021 at 9:42:33 pm'!

!FileList methodsFor: 'user interface' stamp: 'KLG 8/23/2021 20:51:44'!
toggleInitialDirectory
	"Toggle the initial directory setting of the currently selected directory."
	
	| directoryEntry |
	Preferences isInitialFileListDirectory: (directoryEntry _ currentDirectorySelected item)::
		ifTrue: [
			Preferences removeInitialFileListDirectory: directoryEntry ]
		ifFalse: [
			Preferences addInitialFileListDirectory: directoryEntry ]! !


!Preferences class methodsFor: 'file list' stamp: 'KLG 8/23/2021 20:33:08'!
addInitialFileListDirectory: aDirectoryEntry
	"Add an initial directory entry to the collection of initial directories."

	| currentValue |
	(currentValue _ self initialFileListDirectories) isSymbol 
		ifTrue: [ 
			self initialFileListDirectories: (Set with: aDirectoryEntry) ]
		ifFalse: [
			currentValue add: aDirectoryEntry ]
	! !

!Preferences class methodsFor: 'file list' stamp: 'KLG 8/23/2021 20:01:01'!
initialFileListDirectories
	"Answer the initial collection of file list directory entries.
	
	Special values:
	
	#root: Use the usual roots
	#image: Use the image directory
	#vm: Use the vm directory
	#current: Use the current directory "

	^ self
		valueOfFlag: #initialFileListDirectories
		ifAbsent: [ #roots ].! !

!Preferences class methodsFor: 'file list' stamp: 'KLG 8/23/2021 19:53:28'!
initialFileListDirectories: aValue
	"Set the initial fileList directories.

	See #initialFileListDirectories "
	^ self setPreference: #initialFileListDirectories toValue: aValue! !

!Preferences class methodsFor: 'file list' stamp: 'KLG 8/23/2021 20:40:48'!
isInitialFileListDirectory: aDirectoryEntry
	"Answer true if aDirectoryEntry is an initial directory entry."

	| currentValue |
	(currentValue _ self initialFileListDirectories) isSymbol 		ifTrue: [ ^ false ].
	^ currentValue includes: aDirectoryEntry! !

!Preferences class methodsFor: 'file list' stamp: 'KLG 8/23/2021 20:35:42'!
removeInitialFileListDirectory: aDirectoryEntry
	"Add an initial directory entry to the collection of initial directories."

	| currentValue |
	(currentValue _ self initialFileListDirectories) isSymbol 		ifTrue: [ ^ self ].
	currentValue remove: aDirectoryEntry ifAbsent: []	.
	currentValue ifEmpty: [ self initialFileListDirectories: #roots ]! !

!Preferences class methodsFor: 'system startup' stamp: 'KLG 8/23/2021 21:33:24'!
initClassCachedState
	"Check the initial file list directories for existence."
	
	| initialDirectories |
	(initialDirectories _ self initialFileListDirectories) isSymbol ifTrue: [ ^ self ].
	initialDirectories copy do: [ :directoryEntry |
		directoryEntry exists ifFalse:
			[ self removeInitialFileListDirectory: directoryEntry ] ]! !


!FileList methodsFor: 'initialization' stamp: 'KLG 8/23/2021 20:24:48'!
initialDirectoryList

	| initialDirectoryListFromPreferences wrapperCreator |
	wrapperCreator _ [ :directoryEntry |
		FileDirectoryWrapper
			with: directoryEntry
			name: (directoryEntry name ifNil: [ '/' ])
			model: self ].
	(initialDirectoryListFromPreferences _ Preferences initialFileListDirectories) 
		caseOf: {
			[ #roots ] -> [ 	| dirList |
				dirList _ DirectoryEntry roots collect: wrapperCreator.
				dirList isEmpty ifTrue: [
					dirList _ Array with: (FileDirectoryWrapper 
						with: directory
						name: directory localName 
						model: self) ].
				^ dirList ].
			[ #image ] -> [ 
				^ { wrapperCreator value: DirectoryEntry smalltalkImageDirectory } ].
			[ #vm  ] -> [ 
				^ { wrapperCreator value: DirectoryEntry vmDirectory } ].
			[ #current ] -> [
				^ { wrapperCreator value: DirectoryEntry currentDirectory } ] }
		otherwise: [ ^ initialDirectoryListFromPreferences collect: wrapperCreator ]! !


!FileListWindow methodsFor: 'menu building' stamp: 'KLG 8/23/2021 21:39:39'!
volumeMenu
	| aMenu initialDirectoriesMenu  |
	aMenu _ MenuMorph new defaultTarget: model.
	aMenu
		add: 'delete directory...'
		action: #deleteDirectory
		icon: #warningIcon :: setBalloonText: 'Delete the selected directory'.
	model currentDirectorySelected
		ifNil: [ aMenu add: 'initial directory' action: #yourself :: isEnabled: false ]
		ifNotNil: [ :selectedWrapper |
	aMenu 
		add: (Preferences isInitialFileListDirectory: selectedWrapper item ::
			ifTrue: [ '<Yes>' ]
			ifFalse: [ '<No>' ]), 'initial directory'
		action: #toggleInitialDirectory ::
			setBalloonText: 'The selected directory is an initial director for new file list windows' ].
	initialDirectoriesMenu _ MenuMorph new.
	#(
		(roots  'default roots' 'Use the usual root directories. Drives on Windows; "/" on Unix')
	   	(image 'image directory' 'Use the directory with Smalltalk image')
		(vm 'VM directory' 'Use the virtual machine directory')
		(current 'current directory' 'Use the current directory; usually the directory the VM was started in')
	)
		do: [ :entry |
			initialDirectoriesMenu
				add: entry second
				target: Preferences
				action: #initialFileListDirectories:
				argument: entry first ::
					setBalloonText: entry third ].
	aMenu add: 'default initial directories' subMenu: initialDirectoriesMenu.
	^ aMenu! !


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
('scrollbar parameters' scrollbarThickness)
('shout' backgroundColorFillsAllBackground highlightBlockNesting italicsInShout shoutInWorkspaces useAlwaysLeftArrow useAssignmentGlyphLeftArrow)
('standard queries' aaFontsColormapDepth allowBlockArgumentAssignment alternativeBrowseIt assignmentGlyphSelector autoNumberUserChanges automaticPlatformSettings balloonHelpEnabled biggerCursors browseWithPrettyPrint canRecordWhilePlaying caseSensitiveFinds changeSetVersionNumbers cheapWindowReframe checkForSlips clickGrabsMorphs cmdDotEnabled ctrlArrowsScrollHorizontally debugHaloHandle debugLogTimestamp debugShowDamage debugShowDamage2 decorateBrowserButtons diffsInChangeList diffsWithPrettyPrint dismissAllOnOptionClose extraDebuggerButtons focusFollowsMouse focusIndicatorWidth fullScreenLeavesDeskMargins haloEnclosesFullBounds halosShowCoordinateSystem leftArrowAssignmentsInGeneratedCode listClassesHierarchically logDebuggerStackToFile menuKeyboardControl optionalButtons prettyPrintRectangularBlocks selectionsMayShrink selectiveHalos serverMode showAnnotations showLinesInHierarchyViews soundQuickStart soundStopWhenDone soundsEnabled subPixelRenderFonts syntaxHighlightingAsYouType syntaxHighlightingAsYouTypeAnsiAssignment syntaxHighlightingAsYouTypeLeftArrowAssignment systemWindowEmbedOK tapAndHoldEmulatesButton2 thoroughSenders tileResizerInWindowMenu transcriptLogVerbose wantsMenuIcons warnIfNoChangesFile warnIfNoSourcesFile)
('start up' checkLostChangesOnStartUp)
('themes' cuisDefaults forMouse forTouch slowMachine smalltalk80)
('user changes' use:asUserChangesFileNameExtensionWhile: userChangesFileNameExtension)
('file list' defaultHoverHelpBorder hoverHelpBorderWidth: mediumHoverHelpBorder noHoverHelpBorder thickHoverHelpBorder thinHoverHelpBorder hoverHelpBorderWidth addInitialFileListDirectory: initialFileListDirectories initialFileListDirectories: isInitialFileListDirectory: removeInitialFileListDirectory:)
('system startup' initClassCachedState)
!

