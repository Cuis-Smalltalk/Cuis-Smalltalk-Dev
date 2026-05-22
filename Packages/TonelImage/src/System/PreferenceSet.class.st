"
I hold a set of preferences. An application may want to instanciate me to hold its related preferences.
- contents :  a Dictionary whose keys are preference names (symbol) and value a Preference instance.
"
Class {
	#name : 'PreferenceSet',
	#superclass : 'Object',
	#instVars : [
		'contents'
	],
	#category : 'System-Support',
	#package : 'System',
	#tag : 'Support'
}

{ #category : 'sys event handlers' }
PreferenceSet class >> defaultFontSize: aPreference [
	"
	Preferences at: #defaultFontSize put: '12'
	Preferences at: #defaultFontSize put: 14
	Preferences at: #defaultFontSize put: 21
	"
	| pointSize font titleFont |
	pointSize := aPreference value.
	font := FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize.
	Preferences name: #standardListFont category: #font value: font.
	Preferences name: #standardMenuFont category: #font value: font.
	Preferences name: #standardCodeFont category: #font value: font.
	Preferences name: #standardButtonFont category: #font value: font.
	FontFamily defaultFamilyName: font familyName defaultPointSize: pointSize.	
	titleFont := FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize * 12//10.
	Preferences name: #windowTitleFont category: #font value: titleFont.
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
	Preferences at: #biggerCursors put: (pointSize > 30).
	Cursor defaultCursor activateCursor.
	MenuItemMorph initializeSubMenuMarker.
	^ font
]

{ #category : 'sys data' }
PreferenceSet class >> iconicHaloSpecifications [
"Answer an array that characterizes the locations, colors, icons, and selectors of the halo handles 
that may be used in the iconic halo scheme"

	^ #("selector					horiz			vert				color info			icon key"
		(addDismissHandle:		left				top				(red)				haloDismissIcon 		'Remove')
		(addMenuHandle:			leftCenter		top				(blue lighter)		haloMenuIcon 			'Menu')
		(addGrabHandle:			center			top				(black)				haloGrabIcon 			'Pick up')
		(addDragHandle:			rightCenter		top				(brown)				haloDragIcon 			'Move')
		(addDupHandle:			right			top				(green)				haloDuplicateIcon 		'Duplicate')	
		(addExploreHandle:		left				topCenter		(orange)			haloDebugIcon 		'Explore')
		(addDebugHandle:			right			topCenter		(orange)			haloDebugIcon 		'Debug')
		(addCollapseHandle:		left				center			(tan)				haloCollapseIcon 		'Collapse')
		(addScaleHandle:			right			center			(blue)				haloScaleIcon 			'Change scale')
		(addRotateHandle:			left				bottom			(blue)				haloRotateIcon 		'Rotate')
		(addHelpHandle:			center			bottom			(lightBlue)			haloHelpIcon 			'Help')
		(addResizeHandle:			right			bottom			(yellow)			haloResizeIcon 			'Change size')
		"FIXME - Currently non-functional...
		(addRecolorHandle:			right			bottomCenter	(magenta darker)	haloColorIcon 			'Change color')"
	)
]

{ #category : 'sys preference' }
PreferenceSet class >> init [
" Not name #initialize to avoid auto start at class installation "
	#(gui font programming system) with: #(prefGui prefFont prefProgramming prefSystem) do: [ :category :selector |
		(self perform: selector) do: [ :aPrefArray |
			self installDefault: aPrefArray in: category ]].
	self installHaloPreferencesWith: self iconicHaloSpecifications.
	self installMiscPreferences.
	self setDefaultFont: FontFamily defaultFamilyName.
]

{ #category : 'sys preference' }
PreferenceSet class >> initProgramming [
	#(programming) with: #(prefProgramming) do: [ :category :selector |
		(self perform: selector) do: [ :aPrefArray |
			self installDefault: aPrefArray in: category ] ].
]

{ #category : 'sys preference' }
PreferenceSet class >> installDefault: anArray in: category [
	"Description of preference in Array: name - value - type (optional) - event handler (optional) "

	| type myPref |
	type := anArray at: 3 ifAbsent: [Preference detectType: anArray second].
	type isSymbol ifTrue: [
		type := Smalltalk at: type ifAbsent: [Preference detectType: anArray second]].
	myPref := self sysPreferences 
		name: anArray first 
		description: '' 
		category: category 
		type: type
		value: anArray second.
	anArray size = 4 ifTrue: [ "fourth record is an event listener hooked to Preference"
		myPref when: #preferenceChanged send: anArray fourth to: PreferenceSet.
		myPref triggerEvent: #preferenceChanged with: myPref ].
]

{ #category : 'sys preference' }
PreferenceSet class >> installHaloPreferencesWith: anArray [
	^ self sysPreferences 
		name: #haloSpecifications 
		description: 'Halo specifications describing which halos are to be used, what they should look like, and where they should be situated'
		category: #gui
		type: Array
		value: (anArray collect: [ :each | HaloSpec from: each ])
]

{ #category : 'sys preference' }
PreferenceSet class >> installMiscPreferences [
" Preferences with value defined as a closure. We could insert in a dynamic array...
  Preferences with an event handler.
  Preferences with default value of anything but simple literals."
	self sysPreferences

		name: #haloHandleSize 
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize * 5 // 6 * 2 max: 16];

		name: #halosMorphBoundsFrameColor
		description: 'When set, use this color for a rectangle on halos morph bounds. Useful for debugging morph sizes.' 
		category: #gui 
		type: Object "Using Object as type as the property value can be either nil or a Color."
		value: nil;

		name: #halosMorphBoundsFillColor
		description: 'When set, use this color for filling a rectangle on halos morph bounds. Useful for debugging morph sizes.'
		category: #gui
		type: Object "Using Object as type as the property value can be either nil or a Color."
		value: nil;

		name: #roundedButtonRadius 	
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize * 8 // 14];

		name: #roundedWindowRadius 	
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize] ;

		name: #scrollbarThickness 
		category: #gui 		
		value: [(Preferences at: #windowTitleFont) pointSize + 2];

		name: #backgroundEffect
		description: 'determines how backgroundImage is applied'
		category: #gui
		type: Symbol
		value: #stretch;

		name: #classFinder 
		category: #programming 
		value: [ Smalltalk findAndBrowseClass ];

		name: #defaultAuthorName 
		category: #programming 
		value: [Utilities authorName];

		name: #worldMenu
		category: #system
		value: TheWorldMenu;

		name: #browserWindowClass
		category: #programming
		value: BrowserWindow.

	Preferences
		name: #haltThrowsUnhandledError
		description: 'If set, the Halt exception will be catched by an UnhandledError exception handler.'
		category: #system type: Boolean value: false.
]

{ #category : 'sys event handlers' }
PreferenceSet class >> machine: machinePref [
" Update the preferences for this kind of machine (#default, #slow or #smalltalk80) "
	| prefArray selector |
	selector := machinePref caseOf: {
			[#default]		 		-> [#machineDefault].
			[#slow] 				-> [#machineSlow].
			[#smalltalk80] 			-> [#machineSmalltalk80] }
		otherwise: [ #machineDefault ].
	prefArray := self perform: selector.
	prefArray do: [:array | 		self sysPreferences at: array first put: array second].
	machinePref value == #slow ifTrue: [		self runningWorld backgroundImageData: nil].
]

{ #category : 'sys data' }
PreferenceSet class >> machineDefault [
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
		#(cheapWindowReframe false )
		#(syntaxHighlightingAsYouType true )
		#(tapAndHoldEmulatesButton2 true )
		#(clickGrabsMorphs false )
		#(showAssignmentAsLeftArrow false )
		#(wantsMenuIcons true )
	)
]

{ #category : 'sys data' }
PreferenceSet class >> machineSlow [
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
		(cheapWindowReframe true)
		(syntaxHighlightingAsYouType false)
		(tapAndHoldEmulatesButton2 false)
		(clickGrabsMorphs true)
		(wantsMenuIcons false )
		(methodAnnotations #(timeStamp messageCategory packages changeSets) )
		(classAnnotations  #(instanceMethodsCount classMethodsCount) )
		(systemCategoryAnnotations #(classCount instanceMethodsCount classMethodsCount) )
	)
]

{ #category : 'sys data' }
PreferenceSet class >> machineSmalltalk80 [
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
		(cheapWindowReframe true)
		(syntaxHighlightingAsYouType false)
		(tapAndHoldEmulatesButton2 false)
		(clickGrabsMorphs true)
		(wantsMenuIcons false )
	)
]

{ #category : 'services' }
PreferenceSet class >> openPreferencesInspector [
	"Open a window on the current set of preferences choices, allowing the user to view and change their settings"
	Preferences openPreferencesInspector
]

{ #category : 'sys event handlers' }
PreferenceSet class >> pointer: pointerPref [
	| enabled |
	enabled := pointerPref value == #touch.
	#(tapAndHoldEmulatesButton2 clickGrabsMorphs) do: [:aPref | 	
		self sysPreferences at: aPref put: enabled]
]

{ #category : 'sys data' }
PreferenceSet class >> prefFont [

	^ #(
		#(defaultFontSize 18 SmallInteger defaultFontSize:)
		#(aaFontsColormapDepth 4)
		#(properDisplayAlphaForFonts false )	
		#(subPixelRenderColorFonts true )
		#(subPixelRenderFonts true )	
	)
]

{ #category : 'sys data' }
PreferenceSet class >> prefGui [
	^ #( 
		#(balloonHelpEnabled true )
		#(biggerCursors false )		
		#(cacheDisplayContentWhenMovingMorphs true )
		#(cheapWindowReframe false )		
		#(clickGrabsMorphs false )	
		#(ctrlArrowsScrollHorizontally false ) 
		#(drawKeyboardFocusIndicator true )
		#(focusFollowsMouse false )
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
		#(autoReplaceNamedCharacters true)
		#(spaceTabOpensAutoCompletion true)
		#(defaultLineSpacingFactor 1.15)
		#(clickToFocusClickToSelect #never) "#never, #always, #ifSelection"
	)
]

{ #category : 'sys data' }
PreferenceSet class >> prefProgramming [
	^ #(
		#(atMinusDigitMeaning st80 #(st80 ansiSmalltalk disabled) )
		#(allowBlockArgumentAssignment false )
		#(allowNonBooleanReceiversOfBooleanMessages false )
		#(alternativeBrowseIt false )
		#(browseWithPrettyPrint false )
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
		#(listClassesHierarchically true )
		#(logDebuggerStackToFile false )
		#(prettyPrintRectangularBlocks false )
		#(shiftClickShowsImplementors false )
		#(shoutInWorkspaces true )
		#(showAnnotations true )
		#(showLinesInHierarchyViews true )
		#(stylingWithEmphasisInWorkspaces false )
		#(systemCategoryAnnotations #(classCount instanceMethodsCount classMethodsCount "linesOfCode" messageSendsCount) )
		#(classAnnotations #(instanceMethodsCount classMethodsCount "linesOfCode" messageSendsCount) )
		#(messageCategoryAnnotations #(messagesCount messageSendsCount) )
		#(methodAnnotations #(timeStamp "linesOfCode" messageSendsCount messageCategory implementorsCount sendersCount packages library changeSets) )	
		#(syntaxHighlightingAsYouType true )
		#(showAssignmentAsLeftArrow false )
		#(usePreDebugWindow false)
		#(clearPackagePathsOnImageMove true)
	)
]

{ #category : 'sys data' }
PreferenceSet class >> prefSystem [
	^ #(
		#(allowNonLocalReturnsInExceptionHandlers true )
		#(warnAboutNonLocalReturnsInExceptionHandlers false )
		#(askConfirmationOnQuit true )
		#(askToInstallComplexPackage true )
		#(automaticPlatformSettings true )
		#(autoNumberUserChanges true )
		#(caseSensitiveFinds false )
		#(checkForSlips true )
		#(checkLostChangesOnStartUp false )
		#(warnAndOfferLastUserChanges true )
		#(cmdDotEnabled true )
		#(cmdDotInterruptTakesStatistics false )
		#(dismissAllOnOptionClose false )
		#(initialFileListDirectories #cuis #(cuis roots) )
		#(machine default #(default slow smalltalk80) machine: )
		#(pointer mouse #(touch mouse) pointer:)
		#(selectionsMayShrink true )
		#(serverMode false )
		#(systemWindowEmbedOK false )
		#(transcriptLogVerbose true )
		#(useUserPrefsFile true )
		#(userChangesFileNameExtension '.user.changes' )
		#(warnIfNoChangesFile true )
		#(warnIfNoSourcesFile true )
		#(saveReleaseOptionsInWorldMenu false)
	)
]

{ #category : 'services' }
PreferenceSet class >> readAndApplyUserPrefs [
	"
	PreferenceSet readAndApplyUserPrefs
	"
	| file |
	file := DirectoryEntry userBaseDirectory // 'UserPrefs.txt'.
	file exists ifTrue: [
		file readStreamDo: [ :stream |
			[ stream atEnd ] whileFalse: [ | line parts |
				line := stream nextLine.
				parts := line substringsSeparatedBy: $=.
				[ | pref value |
					pref := Preferences instanceAt: parts first asSymbol.
					value := parts second.
					value first == $# ifTrue: [ value := (value withoutPrefix: '#') asSymbol ].
					pref value: value.
					pref justReadFromDisk.
				]
					on: Error
					do: [ :error |
						'--- UserPrefs.txt Error' print.
						error print.
						line print ].
				]]].
]

{ #category : 'services' }
PreferenceSet class >> saveUserPrefs [
	"
	PreferenceSet saveUserPrefs
	"
	| file |
	file := DirectoryEntry userBaseDirectory // 'UserPrefs.txt'.
	file forceWriteStreamDo: [ :stream |
		Preferences allPreferences keysAndValuesDo: [ :symbolName :preference |
			preference isSavedToDisk ifTrue: [
				stream
					nextPutAll: symbolName;
					nextPut: $=.
				preference value printOn: stream.
				stream newLine ]]].
]

{ #category : 'sys fonts' }
PreferenceSet class >> setDefaultFont: aFontName [
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
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
]

{ #category : 'sys fonts' }
PreferenceSet class >> setDefaultFont: fontFamilyName spec: defaultFontsSpec [

	defaultFontsSpec do: [ :triplet | | font |
		font := FontFamily familyName: fontFamilyName pointSize: triplet second.
		font ifNil: [ font := FontFamily defaultFamilyAndPointSize ]. 
		triplet size > 2 ifTrue: [			font := font emphasized: triplet third ].
		self sysPreferences at: triplet first put: font ].
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
]

{ #category : 'sys preference' }
PreferenceSet class >> sysPreferences [
"Access to the Preference Set dedicated to the Cuis system"
	^ Smalltalk at: #Preferences ifAbsentPut: [PreferenceSet new]
]

{ #category : 'accessing' }
PreferenceSet >> allPreferences [
	^ contents 
	
]

{ #category : 'accessing' }
PreferenceSet >> at: symbolName [
	^ (self instanceAt: symbolName) value
]

{ #category : 'accessing' }
PreferenceSet >> at: symbolName ifAbsentAddDescription: aString category: categorySymbol type: aType value: aValue [
	^ (contents
		at: symbolName ifAbsent: [
			self
				name: symbolName
				description: aString
				category: categorySymbol
				type: aType
				value: aValue ]) value
]

{ #category : 'accessing' }
PreferenceSet >> at: symbolName put: aValue [
	| myPref |
	myPref := self instanceAt: symbolName. 
	myPref value: aValue
]

{ #category : 'accessing' }
PreferenceSet >> categories [
	| categories |
	categories := Set new.
	contents values do: [:aPreference | categories add: aPreference category].
	^ categories sorted
]

{ #category : 'error handling' }
PreferenceSet >> doesNotUnderstand: aMessage [
	aMessage hasArguments ifTrue: [^ super doesNotUnderstand: aMessage].
	^ self at: aMessage selector
]

{ #category : 'accessing' }
PreferenceSet >> includesKey: aSymbol [
	^contents includesKey: aSymbol
]

{ #category : 'initialization' }
PreferenceSet >> initialize [
	super initialize.
	contents := Dictionary new.
]

{ #category : 'accessing' }
PreferenceSet >> instanceAt: symbolName [
	^ contents at: symbolName ifAbsent: [self error: 'Unknown preference ', symbolName ]
]

{ #category : 'fileIn/Out' }
PreferenceSet >> loadFrom: aFileEntry [
" Load all preferences from file, update the existing preferences with the data from file "
	(SmartRefStream restoreFromFile: aFileEntry) do: [:aPref | 
		self 
			name: aPref name 
			description: aPref description 
			category: aPref category 
			type: aPref type 
			value: (aPref instVarNamed: #value)	 " We want the raw value "]
]

{ #category : 'instance creation' }
PreferenceSet >> name: nameSymbol category: categorySymbol value: aValue [
	self name: nameSymbol description: nil category: categorySymbol type: nil value: aValue 

]

{ #category : 'instance creation' }
PreferenceSet >> name: nameSymbol description: aString category: categorySymbol type: aType value: aValue [
	"If I exist, I am updated with the newer sent data "

	| pref |
	(nameSymbol isSymbol or: [categorySymbol isSymbol]) ifFalse: [
		self error: 'Preference Name & Category are not valid symbol.' ].
	pref := contents at: nameSymbol ifAbsentPut: [ Preference new ].
	pref
		name: nameSymbol 
		description: aString 
		category: categorySymbol 
		type: aType 
		value: aValue.
	^pref
]

{ #category : 'accessing' }
PreferenceSet >> openPreferencesInspector [
	"Open a window on the current set of preferences choices, allowing the user to view and change their settings"
	self allPreferences inspectWithLabel: 'Preferences'
]

{ #category : 'accessing' }
PreferenceSet >> removeKey: aSymbol [
	^contents removeKey: aSymbol
]

{ #category : 'services' }
PreferenceSet >> removeUnused [
	"
	Preferences removeUnused
	Preferences allPreferences keys select: [ :key | (Smalltalk numberOfSendersOf: key) = 0 ]
	"
	(self allPreferences keys select: [ :key | (Smalltalk numberOfSendersOf: key) = 0 ])
		do: [ :key | self removeKey: key ].
]

{ #category : 'fileIn/Out' }
PreferenceSet >> save: nameSymbol to: aFileEntry [
" Save one preference to a file, even for a sole preference we save in a Dictionary"
	| myPref |
	myPref := self instanceAt: nameSymbol.
	SmartRefStream 
		dumpOnFile: aFileEntry 
		object: {myPref name -> myPref} asDictionary
]

{ #category : 'fileIn/Out' }
PreferenceSet >> saveAllTo: aFileEntry [
" Save all the preferences to a file "
	SmartRefStream dumpOnFile: aFileEntry object: self allPreferences
]

{ #category : 'fileIn/Out' }
PreferenceSet >> saveCategory: categorySymbol to: aFileEntry [
	| myPref |
	myPref := self selectCategory: categorySymbol.
	SmartRefStream 
		dumpOnFile: aFileEntry 
		object: 	(myPref collect: [:aPref | aPref name -> aPref ]) asDictionary
]

{ #category : 'services' }
PreferenceSet >> saveToDisk: nameSymbol [
	"Add / update this preference in the User Preferences file."

	(self instanceAt: nameSymbol) isSavedToDisk: true.
]

{ #category : 'accessing' }
PreferenceSet >> selectCategory: aCategory [
" I select the preferences of the given category "
	^ contents values select: [:aPreference | aPreference category == aCategory ]
]
