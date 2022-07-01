'From Cuis 6.0 [latest update: #5305] on 23 June 2022 at 10:49:19 am'!
!classDefinition: #PreferenceSet category: #'System-Support'!
Object subclass: #PreferenceSet
	instanceVariableNames: 'contents'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Support'!

!PreferenceSet commentStamp: '<historical>' prior: 0!
I hold a set of preferences. An application may want to instanciate me to hold its related preferences.
- contents :  a Dictionary whose keys are preference names (symbol) and value a PreferenceNG instance.!


!PreferenceSet methodsFor: 'instance creation' stamp: 'hlsf 6/22/2022 22:19:59'!
name: nameSymbol category: categorySymbol value: aValue
	self name: nameSymbol description: nil category: categorySymbol type: nil value: aValue 
! !

!PreferenceSet methodsFor: 'instance creation' stamp: 'hlsf 6/22/2022 22:21:43'!
name: nameSymbol description: aString category: categorySymbol type: aType value: aValue
"If I exist, I am updated with the newer sent data "
	(nameSymbol isSymbol or: [categorySymbol isSymbol]) ifFalse: 
		[self error: 'Preference Name & Category are not valid symbol.'].
	^ contents 
		at: nameSymbol
		ifPresent: [:thePref |
			thePref 
				description: aString;
				category: categorySymbol;
				value: aValue;
				yourself]
		ifAbsent: [ | newPref |
			newPref _ PreferenceNG new ::
				name: nameSymbol 
				description: aString 
				category: categorySymbol 
				type: aType 
				value: aValue.
			contents at: nameSymbol put: newPref ].
	! !

!PreferenceSet methodsFor: 'accessing' stamp: 'hlsf 6/22/2022 22:17:03'!
allPreferences
	^ contents 
	! !

!PreferenceSet methodsFor: 'accessing' stamp: 'hlsf 6/22/2022 22:16:51'!
at: symbolName
	^ (self instanceAt: symbolName) value! !

!PreferenceSet methodsFor: 'accessing' stamp: 'hlsf 6/22/2022 22:16:43'!
at: symbolName put: aValue
	| myPref |
	myPref _ self instanceAt: symbolName. 
	myPref value: aValue! !

!PreferenceSet methodsFor: 'accessing' stamp: 'hlsf 6/22/2022 22:16:19'!
categories
	| categories |
	categories _ Set new.
	contents values do: [:aPreference | categories add: aPreference category].
	^ categories sorted! !

!PreferenceSet methodsFor: 'accessing' stamp: 'hlsf 6/22/2022 22:16:00'!
instanceAt: symbolName
	^ contents at: symbolName ifAbsent: [self error: 'Unknown preference ', symbolName ]! !

!PreferenceSet methodsFor: 'accessing' stamp: 'hlsf 6/23/2022 10:40:51'!
openPreferencesInspector
	"Open a window on the current set of preferences choices, allowing the user to view and change their settings"
	self allPreferences inspectWithLabel: 'Preferences'! !

!PreferenceSet methodsFor: 'accessing' stamp: 'hlsf 6/22/2022 22:16:30'!
selectCategory: aCategory
" I select the preferences of the given category "
	^ contents values select: [:aPreference | aPreference category == aCategory ]! !

!PreferenceSet methodsFor: 'error handling' stamp: 'hlsf 6/23/2022 10:19:47'!
doesNotUnderstand: aMessage
	aMessage hasArguments ifTrue: [^ super doesNotUnderstand: aMessage].
	^ self at: aMessage selector! !

!PreferenceSet methodsFor: 'fileIn/Out' stamp: 'hlsf 6/23/2022 10:24:29'!
loadFrom: aFileEntry
" Load all preferences from file, update the existing preferences with the data from file "
	(SmartRefStream restoreFromFile: aFileEntry) do: [:aPref | 
		self 
			name: aPref name 
			description: aPref description 
			category: aPref category 
			type: aPref type 
			value: (aPref instVarNamed: #value)	 " We want the raw value "]! !

!PreferenceSet methodsFor: 'fileIn/Out' stamp: 'hlsf 6/23/2022 10:25:38'!
save: nameSymbol to: aFileEntry
" Save one preference to a file, even for a sole preference we save in a Dictionary"
	| myPref |
	myPref _ self instanceAt: nameSymbol.
	SmartRefStream 
		dumpOnFile: aFileEntry 
		object: {myPref name -> myPref} asDictionary! !

!PreferenceSet methodsFor: 'fileIn/Out' stamp: 'hlsf 6/23/2022 10:26:23'!
saveAllTo: aFileEntry
" Save all the preferences to a file "
	SmartRefStream dumpOnFile: aFileEntry object: self allPreferences! !

!PreferenceSet methodsFor: 'fileIn/Out' stamp: 'hlsf 6/23/2022 10:26:42'!
saveCategory: categorySymbol to: aFileEntry
	| myPref |
	myPref _ self selectCategory: categorySymbol.
	SmartRefStream 
		dumpOnFile: aFileEntry 
		object: 	(myPref collect: [:aPref | aPref name -> aPref ]) asDictionary! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'hlsf 6/22/2022 22:44:59'!
iconicHaloSpecifications
"Answer an array that characterizes the locations, colors, icons, and selectors of the halo handles 
that may be used in the iconic halo scheme"

^ #(	"selector					horiz			vert			color info				icon key"
(addDismissHandle:						left			top			(red)				haloDismissIcon 					'Remove')
(addMenuHandle:						leftCenter			top			(blue lighter)				haloMenuIcon 					'Menu')
(addGrabHandle:						center			top			(black)				haloGrabIcon 					'Pick up')
(addDragHandle:						rightCenter			top			(brown)				haloDragIcon 					'Move')
(addDupHandle:						right			top			(green)				haloDuplicateIcon     					'Duplicate')	
(addExploreHandle:						left			topCenter			(orange)				haloDebugIcon 					'Explore')
(addDebugHandle:						right			topCenter			(orange)				haloDebugIcon 					'Debug')
(addCollapseHandle:						left			center			(tan)				haloCollapseIcon 					'Collapse')
(addScaleHandle:						right			center			(blue)				haloScaleIcon 					'Change scale')
(addRotateHandle:						left			bottom			(blue)				haloRotateIcon 					'Rotate')
(addHelpHandle:						center			bottom			(lightBlue)				haloHelpIcon 					'Help')
(addResizeHandle:						right			bottom			(yellow)				haloResizeIcon 					'Change size')
	"FIXME - Currently non-functional...
	(addRecolorHandle:						right			bottomCenter			(magenta darker)						haloColorIcon 						'Change color')	"
)! !

!PreferenceSet class methodsFor: 'sys data' stamp: 'hlsf 6/22/2022 22:45:05'!
machineDefault
	^ #(
		#(drawKeyboardFocusIndicator true )
		#(balloonHelpEnabled true )
		#(biggerCursors false )
		#(browseWithPrettyPrint false )
		#(caseSensitiveFinds false )
		#(checkForSlips true )
		#(cmdDotEnabled true )
		#(diffsInChangeList true )
		#(diffsWithPrettyPrint false )
		#(menuKeyboardControl true )
		#(optionalButtons true )
		#(extraDebuggerButtons true )
		#(subPixelRenderFonts true )
		#(thoroughSenders true )
		#(cheapWindowReframe false )
		#(syntaxHighlightingAsYouType true )
		#(tapAndHoldEmulatesButton2 true )
		#(clickGrabsMorphs false )
		#(syntaxHighlightingAsYouTypeAnsiAssignment false )
		#(syntaxHighlightingAsYouTypeLeftArrowAssignment false )
		#(wantsMenuIcons true )
	)! !

!PreferenceSet class methodsFor: 'sys data' stamp: 'hlsf 6/22/2022 22:45:10'!
machineSlow
	^ #(
		(drawKeyboardFocusIndicator false )
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(cheapWindowReframe true)
		(syntaxHighlightingAsYouType false)
		(tapAndHoldEmulatesButton2 false)
		(clickGrabsMorphs true)
		(wantsMenuIcons false )
		(methodAnnotations #(timeStamp messageCategory packages changeSets) )
		(classAnnotations  #(instanceMethodsCount classMethodsCount) )
		(systemCategoryAnnotations #(classCount instanceMethodsCount classMethodsCount) )
	)! !

!PreferenceSet class methodsFor: 'sys data' stamp: 'hlsf 6/22/2022 22:45:16'!
machineSmalltalk80
	^ #(
		(drawKeyboardFocusIndicator false )
		(balloonHelpEnabled false)
		(browseWithPrettyPrint false)
		(caseSensitiveFinds true)
		(checkForSlips false)
		(cmdDotEnabled true)
		(diffsInChangeList true)
		(diffsWithPrettyPrint false)
		(menuKeyboardControl false)
		(optionalButtons false)
		(subPixelRenderFonts true)
		(thoroughSenders true)
		(cheapWindowReframe true)
		(syntaxHighlightingAsYouType false)
		(tapAndHoldEmulatesButton2 false)
		(clickGrabsMorphs true)
		(wantsMenuIcons false )
	)! !

!PreferenceSet class methodsFor: 'sys data' stamp: 'hlsf 6/22/2022 22:45:21'!
prefFont
	^ #( 
		#(aaFontsColormapDepth 4)
		#(cacheTrueTypeGlyphs true )
		#(guiElementsSize standardFonts #(tinyFonts verySmallFonts smallFonts standardFonts largeFonts veryLargeFonts hugeFonts) defaultFontSize: )
		#(loadOnlyLatinGlyphData false )
		#(properDisplayAlphaForFonts false )	
		#(subPixelRenderColorFonts true )
		#(subPixelRenderFonts true )	
	)! !

!PreferenceSet class methodsFor: 'sys data' stamp: 'hlsf 6/22/2022 22:45:27'!
prefGui
	^ #( 
		#(balloonHelpEnabled true )
		#(biggerCursors false )		
		#(cacheDisplayContentWhenMovingMorphs true )
		#(cheapWindowReframe false )		
		#(clickGrabsMorphs false )	
		#(ctrlArrowsScrollHorizontally false ) 
		#(drawKeyboardFocusIndicator true )
		#(focusFollowsMouse true )
		#(focusIndicatorWidth 1)
		#(fullScreenLeavesDeskMargins true )
		#(haloEnclosesFullBounds true )
		#(halosShowCoordinateSystem true )
		#(menuKeyboardControl true )
		#(optionalButtons true )		
		#(selectiveHalos true )
		#(tapAndHoldEmulatesButton2 true )
		#(tileResizerInWindowMenu true )
		#(wantsMenuIcons true )
	)! !

!PreferenceSet class methodsFor: 'sys data' stamp: 'hlsf 6/22/2022 22:45:33'!
prefProgramming
	^ #(
		#(atMinusDigitMeaning st80 #(st80 ansiSmalltalk disabled) )
		#(allowBlockArgumentAssignment false )
		#(alternativeBrowseIt false )
		#(assignmentGlyphSelector useLeftArrow #(useLeftArrow useAlwaysLeftArrow) assignmentGlyph: )
		#(backgroundColorFillsAllBackground true )
		#(browseWithPrettyPrint false )
		#(classAnnotations #(instanceMethodsCount classMethodsCount linesOfCode) )
		#(debugHaloHandle true )
		#(debugLogTimestamp true )
		#(debugShowDamage false )
		#(decorateBrowserButtons true )
		#(diffsInChangeList true )
		#(diffsWithPrettyPrint false )
		#(extraDebuggerButtons true )
		#(fullPrintItInWorkspaces false )
		#(highlightBlockNesting true )
		#(italicsInShout true)
		#(leftArrowAssignmentsInGeneratedCode false )
		#(listClassesHierarchically true )
		#(logDebuggerStackToFile false )
		#(methodAnnotations #(timeStamp linesOfCode messageCategory implementorsCount sendersCount  packages changeSets) )	
		#(prettyPrintRectangularBlocks false )
		#(shiftClickShowsImplementors false )
		#(shoutInWorkspaces true )
		#(showAnnotations true )
		#(showLinesInHierarchyViews true )
		#(stylingWithEmphasisInWorkspaces false )
		#(systemCategoryAnnotations #(classCount instanceMethodsCount classMethodsCount linesOfCode) )
		#(syntaxHighlightingAsYouType true )
		#(syntaxHighlightingAsYouTypeAnsiAssignment false )
		#(syntaxHighlightingAsYouTypeLeftArrowAssignment false )
		#(thoroughSenders true )
		#(usePreDebugWindow false)
	)! !

!PreferenceSet class methodsFor: 'sys data' stamp: 'hlsf 6/22/2022 22:45:38'!
prefSystem
	^ #( #(allowNonLocalReturnsInExceptionHandlers true )
		#(askConfirmationOnQuit true )
		#(askToInstallComplexPackage true )
		#(automaticPlatformSettings true )
		#(autoNumberUserChanges true )
		#(caseSensitiveFinds false )
		#(changeSetVersionNumbers true )
		#(checkForSlips true )
		#(checkLostChangesOnStartUp false )
		#(cmdDotEnabled true )
		#(cmdDotInterruptTakesStatistics false )
		#(dismissAllOnOptionClose false )
		#(initialFileListDirectories #roots #(root image vm current) )
		#(machine default #(default slow smalltalk80) machine: )
		#(pointer mouse #(touch mouse) pointer:)
		#(selectionsMayShrink true )
		#(serverMode false )
		#(systemWindowEmbedOK false )
		#(transcriptLogVerbose true )
		#(userChangesFileNameExtension '.user.changes' )
		#(warnAboutNonLocalReturnsInExceptionHandlers false )
		#(warnIfNoChangesFile true )
		#(warnIfNoSourcesFile true )
	)! !

!PreferenceSet class methodsFor: 'sys preference' stamp: 'hlsf 6/22/2022 22:34:58'!
init
" Not nme initialize to avoid auto start at class installation "
	#(gui font programming system) do: [: category | 
		(self perform: (#pref, category capitalized) asSymbol) do: [:aPrefArray |
			self installDefault: aPrefArray in: category] ].
	self installHaloPreferencesWith: self iconicHaloSpecifications.
	self installMiscPreferences.
	self defaultFontSize: #standardFonts.
	self setDefaultFont: FontFamily defaultFamilyName.! !

!PreferenceSet class methodsFor: 'sys preference' stamp: 'hlsf 6/23/2022 10:27:57'!
installDefault: anArray in: category
" Description of preference in Array: name - value - type (optional) - event handler (optional) "
	| myPref |
	myPref _ self sysPreferences 
		name: anArray first 
		description: '' 
		category: category 
		type: (anArray at: 3 ifAbsent: [PreferenceNG detectType: anArray second]) value: anArray second.
		
	anArray size = 4 ifTrue: [ "fourth record is an event listener hooked to Preference"
		myPref when: #preferenceChanged send: anArray fourth to: PreferenceSet.
		myPref triggerEvent: #preferenceChanged with: myPref]
	! !

!PreferenceSet class methodsFor: 'sys preference' stamp: 'hlsf 6/22/2022 22:42:05'!
installHaloPreferencesWith: anArray
	| aColor |
	^ self sysPreferences 
		name: #haloSpecifications 
		description: 'Halo specifications describing which halos are to be used, what they should look like, and where they should be situated'
		category: #gui
		type: Array
		value: (anArray collect: [ :each |
			aColor _ Color.
			each fourth do: [ :sel | aColor _ aColor perform: sel].
			HaloSpec new 
				horizontalPlacement: each second
				verticalPlacement: each third 
				color: aColor
				iconSymbol: each fifth
				addHandleSelector: each first
				hoverHelp: each sixth])! !

!PreferenceSet class methodsFor: 'sys preference' stamp: 'hlsf 6/22/2022 22:43:37'!
installMiscPreferences
" Preference value defined with closure. We could insert in a dynamic array...
  Preference with event handler "
	self sysPreferences 

		name: #haloHandleSize 
		category: #gui 
		value: [(PreferenceNG at: #standardListFont) pointSize * 5 // 3 max: 16];
	
		name: #roundedButtonRadius 	
		category: #gui 
		value: [(PreferenceNG at: #standardListFont) pointSize * 8 // 14];
		
		name: #roundedWindowRadius 	
		category: #gui 
		value: [(PreferenceNG at: #standardListFont) pointSize] ;
		
		name: #scrollbarThickness 
		category: #gui 		
		value: [(PreferenceNG at: #windowTitleFont) pointSize + 2];
		
		name: #classFinder 
		category: #programming 
		value: [ BrowserWindow findClass ];
		
		name: #defaultAuthorName 
		category: #programming 
		value: [Utilities authorName].! !

!PreferenceSet class methodsFor: 'sys preference' stamp: 'hlsf 6/22/2022 22:32:48'!
sysPreferences
"Access to the Preference Set dedicated to the Cuis system"
	^ Smalltalk at: #Preferences ifAbsentPut: [PreferenceSet new]! !

!PreferenceSet class methodsFor: 'sys fonts' stamp: 'hlsf 6/23/2022 10:33:07'!
fontSizes
	^ `{
		#tinyFonts -> 7 .
		#verySmallFonts -> 9 .
		#smallFonts ->11 .
		#standardFonts -> 14 .
		#largeFonts -> 18 .
		#veryLargeFonts -> 24 .
		#hugeFonts -> 32} asDictionary`! !

!PreferenceSet class methodsFor: 'sys fonts' stamp: 'hlsf 6/23/2022 10:36:35'!
setDefaultFont: aFontName
	"Change the font on the whole system without changing point sizes."
	FontFamily defaultFamilyName: aFontName.
	self
		setDefaultFont: FontFamily defaultFamilyName
		spec: {
			{#standardListFont. (self sysPreferences at: #standardListFont) pointSize.}.
			{#standardMenuFont. (self sysPreferences at: #standardMenuFont) pointSize.}.
			{#windowTitleFont. (self sysPreferences at: #windowTitleFont) pointSize.}.
			{#standardCodeFont. (self sysPreferences at: #standardCodeFont) pointSize.}.
			{#standardButtonFont. (self sysPreferences at: #standardButtonFont) pointSize.}.
		}.
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].! !

!PreferenceSet class methodsFor: 'sys fonts' stamp: 'hlsf 6/23/2022 10:37:23'!
setDefaultFont: fontFamilyName spec: defaultFontsSpec
	| font |
	defaultFontsSpec do: [ :triplet |
		font _ FontFamily familyName: fontFamilyName pointSize: triplet second.
		font ifNil: [ font _ FontFamily defaultFamilyAndPointSize ]. 
		triplet size > 2 ifTrue: [			font _ font emphasized: triplet third ].
		self sysPreferences at: triplet first put: font ].
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].! !

!PreferenceSet class methodsFor: 'sys obsolete' stamp: 'hlsf 6/23/2022 10:40:08'!
desktopMenuTitle
"I have project to get rid of this one"
	^ 'World'   ! !

!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'hlsf 6/23/2022 10:29:37'!
assignmentGlyph: assignmentPref
" #assignmentGliphSelector changed "
	AbstractFont withAllSubclassesDo: [ :fontClass | fontClass assignmentGlyphSelectorPreferenceChanged ]! !

!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'hlsf 6/23/2022 10:30:25'!
defaultFontSize: guiElementSizePref
" #guiElementSize changed "
	| font titleFont pointSize |
	pointSize _ self fontSizes at: guiElementSizePref value.
	font _ FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize.
	PreferenceNG name: #standardListFont category: #font value: font.
	PreferenceNG name: #standardMenuFont category: #font value: font.
	PreferenceNG name: #standardCodeFont category: #font value: font.
	PreferenceNG name: #standardButtonFont category: #font value: font.
	FontFamily defaultFamilyName: font familyName defaultPointSize: pointSize.	
	titleFont _ FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize * 12//10.
	PreferenceNG name: #windowTitleFont category: #font value: titleFont.
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
	PreferenceNG at: #biggerCursors put: (pointSize > 14).
	Cursor defaultCursor activateCursor.
	^ font! !

!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'hlsf 6/23/2022 10:31:46'!
machine: machinePref
" Update the preferences for this kind of machine (#default, #slow or #smalltalk80) "
	| prefArray |
	prefArray _ self perform: (#machine, machinePref value capitalized) asSymbol.
	prefArray do: [:array | 		self sysPreferences at: array first put: array second].
	machinePref value == #slow ifTrue: [		self runningWorld backgroundImageData: nil]! !

!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'hlsf 6/23/2022 10:32:10'!
menuIcon: wantsMenuIconsPref
	Theme current class beCurrent! !

!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'hlsf 6/23/2022 10:32:36'!
pointer: pointerPref
	| enabled |
	enabled _ pointerPref value == #touch.
	#(tapAndHoldEmulatesButton2 clickGrabsMorphs) do: [:aPref | 	
		self sysPreferences at: aPref put: enabled]! !


!PreferenceNG class methodsFor: 'accessing' stamp: 'hlsf 6/23/2022 10:20:29'!
allPreferences
	^ ThePreferences 
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'accessing' stamp: 'hlsf 6/23/2022 10:21:04'!
at: symbolName
	^ (self instanceAt: symbolName) value
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'accessing' stamp: 'hlsf 6/23/2022 10:20:39'!
at: symbolName put: aValue
	| myPref |
	myPref _ self instanceAt: symbolName. 
	myPref value: aValue
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'accessing' stamp: 'hlsf 6/23/2022 10:20:45'!
categories
	| categories |
	categories _ Set new.
	ThePreferences values do: [:aPreference | categories add: aPreference category].
	^ categories sorted
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'accessing' stamp: 'hlsf 6/23/2022 10:20:53'!
instanceAt: symbolName
	^ ThePreferences at: symbolName ifAbsent: [self error: 'Unknown preference ', symbolName ]
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'accessing' stamp: 'hlsf 6/23/2022 10:20:58'!
selectCategory: aCategory
" I select the preferences of the given category "
	^ ThePreferences values select: [:aPreference | aPreference category == aCategory ]
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'instance creation' stamp: 'hlsf 6/23/2022 10:21:15'!
name: nameSymbol category: categorySymbol value: aValue
	self name: nameSymbol description: nil category: categorySymbol type: nil value: aValue 
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'instance creation' stamp: 'hlsf 6/23/2022 10:21:20'!
name: nameSymbol description: aString category: categorySymbol type: aType value: aValue
"If I exist, I am updated with the newer sent data "
	(nameSymbol isSymbol or: [categorySymbol isSymbol]) ifFalse: 
		[self error: 'Preference Name & Category are not valid symbol.'].
	^ ThePreferences 
		at: nameSymbol
		ifPresent: [:thePref |
			thePref 
				description: aString;
				category: categorySymbol;
				value: aValue;
				yourself]
		ifAbsent: [ | newPref |
			newPref _ self new ::
				name: nameSymbol 
				description: aString 
				category: categorySymbol 
				type: aType 
				value: aValue.
			ThePreferences at: nameSymbol put: newPref ].
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'error handling' stamp: 'hlsf 6/23/2022 10:21:26'!
doesNotUnderstand: aMessage
	aMessage hasArguments ifTrue: [^ super doesNotUnderstand: aMessage].
	^ self at: aMessage selector
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'event handlers' stamp: 'hlsf 6/23/2022 10:29:46'!
assignmentGlyph: assignmentPref
" #assignmentGliphSelector changed "
	AbstractFont withAllSubclassesDo: [ :fontClass | fontClass assignmentGlyphSelectorPreferenceChanged ]
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'event handlers' stamp: 'hlsf 6/23/2022 10:30:39'!
defaultFontSize: guiElementSizePref
" #guiElementSize changed "
	| font titleFont pointSize |
	pointSize _ self fontSizes at: guiElementSizePref value.
	font _ FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize.
	PreferenceNG name: #standardListFont category: #font value: font.
	PreferenceNG name: #standardMenuFont category: #font value: font.
	PreferenceNG name: #standardCodeFont category: #font value: font.
	PreferenceNG name: #standardButtonFont category: #font value: font.
	FontFamily defaultFamilyName: font familyName defaultPointSize: pointSize.	
	titleFont _ FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize * 12//10.
	PreferenceNG name: #windowTitleFont category: #font value: titleFont.
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
	PreferenceNG at: #biggerCursors put: (pointSize > 14).
	Cursor defaultCursor activateCursor.
	^ font
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'event handlers' stamp: 'hlsf 6/23/2022 10:31:50'!
machine: machinePref
" Update the preferences for this kind of machine (#default, #slow or #smalltalk80) "
	| prefArray |
	prefArray _ self perform: (#machine, machinePref value capitalized) asSymbol.
	prefArray do: [:array | 		PreferenceNG at: array first put: array second].
	machinePref value == #slow ifTrue: [		self runningWorld backgroundImageData: nil]
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'event handlers' stamp: 'hlsf 6/23/2022 10:32:14'!
menuIcon: wantsMenuIconsPref
	Theme current class beCurrent
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'event handlers' stamp: 'hlsf 6/23/2022 10:32:42'!
pointer: pointerPref
	| enabled |
	enabled _ pointerPref value == #touch.
	#(tapAndHoldEmulatesButton2 clickGrabsMorphs) do: [:aPref | 		PreferenceNG at: aPref put: enabled]
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'fonts' stamp: 'hlsf 6/23/2022 10:32:52'!
fontSizes
	^ `{
		#tinyFonts -> 7 .
		#verySmallFonts -> 9 .
		#smallFonts ->11 .
		#standardFonts -> 14 .
		#largeFonts -> 18 .
		#veryLargeFonts -> 24 .
		#hugeFonts -> 32} asDictionary`
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'fonts' stamp: 'hlsf 6/23/2022 10:36:26'!
setDefaultFont: aFontName
	"Change the font on the whole system without changing point sizes."
	FontFamily defaultFamilyName: aFontName.
	PreferenceNG
			setDefaultFont: FontFamily defaultFamilyName
			spec: {
				{#standardListFont. (PreferenceNG at: #standardListFont) pointSize.}.
				{#standardMenuFont. (PreferenceNG at: #standardMenuFont) pointSize.}.
				{#windowTitleFont. (PreferenceNG at: #windowTitleFont) pointSize.}.
				{#standardCodeFont. (PreferenceNG at: #standardCodeFont) pointSize.}.
				{#standardButtonFont. (PreferenceNG at: #standardButtonFont) pointSize.}.
			}.
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'fonts' stamp: 'hlsf 6/23/2022 10:39:04'!
setDefaultFont: fontFamilyName spec: defaultFontsSpec
	| font |
	defaultFontsSpec do: [ :triplet |
		font _ FontFamily familyName: fontFamilyName pointSize: triplet second.
		font ifNil: [ font _ FontFamily defaultFamilyAndPointSize ]. 
		triplet size > 2 ifTrue: [			font _ font emphasized: triplet third ].
		PreferenceNG at: triplet first put: font ].
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'obsolete' stamp: 'hlsf 6/23/2022 10:40:23'!
desktopMenuTitle
"I have project to get rid of this one"
	^ 'World' 
" Copied to PreferenceSet "! !

!PreferenceNG class methodsFor: 'as yet unclassified' stamp: 'hlsf 6/23/2022 10:41:06'!
openPreferencesInspector
	"Open a window on the current set of preferences choices, allowing the user to view and change their settings"
	
	self allPreferences inspectWithLabel: 'Preferences'
" Copied to PreferenceSet "! !

!methodRemoval: PreferenceSet class #cuisPreferences stamp: 'hlsf 6/22/2022 22:29:10'!
PreferenceSet class removeSelector: #cuisPreferences!
!methodRemoval: PreferenceNG class #installDefault:in: stamp: 'hlsf 6/23/2022 10:18:27'!
PreferenceNG class removeSelector: #installDefault:in:!
!methodRemoval: PreferenceNG class #loadFrom: stamp: 'hlsf 6/23/2022 10:26:56'!
PreferenceNG class removeSelector: #loadFrom:!
!methodRemoval: PreferenceNG class #prefFont stamp: 'hlsf 6/23/2022 10:18:04'!
PreferenceNG class removeSelector: #prefFont!
!methodRemoval: PreferenceNG class #prefProgramming stamp: 'hlsf 6/23/2022 10:18:08'!
PreferenceNG class removeSelector: #prefProgramming!
!methodRemoval: PreferenceNG class #prefSystem stamp: 'hlsf 6/23/2022 10:18:09'!
PreferenceNG class removeSelector: #prefSystem!
!methodRemoval: PreferenceNG class #machineDefault stamp: 'hlsf 6/23/2022 10:17:57'!
PreferenceNG class removeSelector: #machineDefault!
!methodRemoval: PreferenceNG class #initialize stamp: 'hlsf 6/23/2022 10:18:23'!
PreferenceNG class removeSelector: #initialize!
!methodRemoval: PreferenceNG class #saveCategory:to: stamp: 'hlsf 6/23/2022 10:26:52'!
PreferenceNG class removeSelector: #saveCategory:to:!
!methodRemoval: PreferenceNG class #machineSlow stamp: 'hlsf 6/23/2022 10:18:00'!
PreferenceNG class removeSelector: #machineSlow!
!methodRemoval: PreferenceNG class #machineSmalltalk80 stamp: 'hlsf 6/23/2022 10:18:03'!
PreferenceNG class removeSelector: #machineSmalltalk80!
!methodRemoval: PreferenceNG class #installMiscPreferences stamp: 'hlsf 6/23/2022 10:18:34'!
PreferenceNG class removeSelector: #installMiscPreferences!
!methodRemoval: PreferenceNG class #installHaloPreferencesWith: stamp: 'hlsf 6/23/2022 10:18:31'!
PreferenceNG class removeSelector: #installHaloPreferencesWith:!
!methodRemoval: PreferenceNG class #prefGui stamp: 'hlsf 6/23/2022 10:18:06'!
PreferenceNG class removeSelector: #prefGui!
!methodRemoval: PreferenceNG class #saveAllTo: stamp: 'hlsf 6/23/2022 10:26:53'!
PreferenceNG class removeSelector: #saveAllTo:!
!methodRemoval: PreferenceNG class #save:to: stamp: 'hlsf 6/23/2022 10:26:55'!
PreferenceNG class removeSelector: #save:to:!
!methodRemoval: PreferenceNG class #iconicHaloSpecifications stamp: 'hlsf 6/23/2022 10:17:55'!
PreferenceNG class removeSelector: #iconicHaloSpecifications!
PreferenceNG initialize!

!PreferenceNG class reorganize!
('accessing' allPreferences at: at:put: categories instanceAt: selectCategory:)
('instance creation' name:category:value: name:description:category:type:value:)
('error handling' doesNotUnderstand:)
('helpers' detectType:)
('event handlers' assignmentGlyph: defaultFontSize: machine: menuIcon: pointer:)
('fonts' fontSizes setDefaultFont: setDefaultFont:spec:)
('obsolete' desktopMenuTitle)
('as yet unclassified' openPreferencesInspector)
!


!PreferenceSet reorganize!
('instance creation' name:category:value: name:description:category:type:value:)
('accessing' allPreferences at: at:put: categories instanceAt: openPreferencesInspector selectCategory:)
('error handling' doesNotUnderstand:)
('fileIn/Out' loadFrom: save:to: saveAllTo: saveCategory:to:)
!


!PreferenceSet class reorganize!
('sys data' iconicHaloSpecifications machineDefault machineSlow machineSmalltalk80 prefFont prefGui prefProgramming prefSystem)
('sys preference' init installDefault:in: installHaloPreferencesWith: installMiscPreferences sysPreferences)
('sys fonts' fontSizes setDefaultFont: setDefaultFont:spec:)
('sys obsolete' desktopMenuTitle)
('sys event handlers' assignmentGlyph: defaultFontSize: machine: menuIcon: pointer:)
!

