'From Cuis 6.0 [latest update: #5152] on 13 May 2022 at 4:26:43 pm'!
!classDefinition: #PreferenceNG category: #'System-Support'!
Object subclass: #PreferenceNG
	instanceVariableNames: 'name description category value type'
	classVariableNames: 'ThePreferences'
	poolDictionaries: ''
	category: 'System-Support'!

!PreferenceNG commentStamp: '<historical>' prior: 0!
My instance is a Preference whose value is of a given class (type), or follow the description of the type instance

- name, category: symbol
- description: string
- type: a class (Boolean, String, Color, BlockClosure, etc.) or an instance (collection, intervale, etc.)
- value: an object whose class match type or the instance description of the type

Modus Operendi
- To create a Preference, invoke the instance creation class method (Preference name:description:category:type:value:). The new preference is automatically remembered. If a preference with same name already exist, its instance is updated with the new provided data and its instance returned.
- To access, invoke as dictionary (Preference at: #biggerCursors), a short cut exist (Preference biggerCursors)
- When a preference value is changed with its #value message, an event #preferenceChanged is triggered.
- Tools can listen to a specific #preferenceChanged event: 
	(Preference instanceAt: #guiElementSize) when: #preferenceChanged send: #shakeScreen: to: myTool
By default the Preference class listen to a few event to adjust the behavior of the image to some preference changes.	
!


!PreferenceNG methodsFor: 'printing' stamp: 'hlsf 5/5/2022 12:31:23'!
printOn: aStream
	aStream nextPutAll: self class name ;
		nextPutAll: ' (#';
		nextPutAll: name ;
		nextPutAll: ' = ';
		nextPutAll: type printString;
		nextPutAll: '::';
		nextPutAll: value printString;
		nextPut: $)! !

!PreferenceNG methodsFor: 'initialization' stamp: 'hlsf 5/5/2022 12:21:27'!
name: nameSymbol description: aString category: categorySymbol type: aType value: aValue
	name _ nameSymbol.
	description _ aString.
	category _ categorySymbol.
	type _ aType ifNotNil: [aType] ifNil: [self class detectType: aValue ].
	value _ 	aValue! !

!PreferenceNG methodsFor: 'accessing' stamp: 'hlsf 5/10/2022 19:34:21'!
accept: aVisitor
	| visitMethod |
	self isTypeAClass 
		ifTrue: [ visitMethod _ ('visit', type name, 'Preference:') asSymbol ]
		ifFalse: [visitMethod _ ('visit', type species, 'Preference:') asSymbol ].
	aVisitor perform: visitMethod with: self! !

!PreferenceNG methodsFor: 'accessing' stamp: 'hlsf 4/30/2022 14:25:45'!
category
	^ category ! !

!PreferenceNG methodsFor: 'accessing' stamp: 'hlsf 5/2/2022 22:19:11'!
category: aSymbol
	aSymbol isNil ifFalse: [category _ aSymbol ]! !

!PreferenceNG methodsFor: 'accessing' stamp: 'hlsf 4/30/2022 14:25:57'!
description
	^ description! !

!PreferenceNG methodsFor: 'accessing' stamp: 'hlsf 5/2/2022 22:18:30'!
description: aString
	aString isEmptyOrNil ifFalse: [description _ aString ]! !

!PreferenceNG methodsFor: 'accessing' stamp: 'hlsf 4/30/2022 14:26:02'!
name
	^ name! !

!PreferenceNG methodsFor: 'accessing' stamp: 'hlsf 5/4/2022 23:32:57'!
type
	^ type! !

!PreferenceNG methodsFor: 'accessing' stamp: 'hlsf 5/3/2022 21:16:04'!
value
" If my type is closure without argument, return my value "
	^ (type == BlockClosure and: [value argumentCount = 0])
		ifTrue: [value value]
		ifFalse: [value]
	! !

!PreferenceNG methodsFor: 'accessing' stamp: 'hlsf 5/9/2022 22:01:23'!
value: aValue	
	self isTypeAClass 
		ifTrue: [
			(aValue isKindOf: type) ifFalse: [self error: aValue printString, ' is not a ', type printString].
			value _ aValue ]
		ifFalse: [ "Should be handled somehow by the preference editor "
			value _ aValue].
	self triggerEvent: #preferenceChanged with: self! !

!PreferenceNG methodsFor: 'testing' stamp: 'hlsf 5/9/2022 22:01:40'!
isTypeAClass
" is type a Class or an Instance "
	^ type class class == Metaclass ! !


!PreferenceNG class methodsFor: 'accessing' stamp: 'hlsf 4/30/2022 21:11:22'!
all
	^ ThePreferences 
	! !

!PreferenceNG class methodsFor: 'accessing' stamp: 'hlsf 4/30/2022 22:21:36'!
at: symbolName
	^ (self instanceAt: symbolName) value! !

!PreferenceNG class methodsFor: 'accessing' stamp: 'hlsf 5/3/2022 20:47:15'!
at: symbolName put: aValue
	| myPref |
	myPref _ self instanceAt: symbolName. 
	myPref value: aValue! !

!PreferenceNG class methodsFor: 'accessing' stamp: 'hlsf 4/30/2022 14:28:57'!
categories
	| categories |
	categories _ Set new.
	ThePreferences values do: [:aPreference | categories add: aPreference category].
	^ categories sorted! !

!PreferenceNG class methodsFor: 'accessing' stamp: 'hlsf 4/30/2022 22:19:26'!
instanceAt: symbolName
	^ ThePreferences at: symbolName ifAbsent: [self error: 'Unknown preference ', symbolName ]! !

!PreferenceNG class methodsFor: 'accessing' stamp: 'hlsf 4/30/2022 14:29:56'!
select: aCategory
" I select the preferences of the given category "
	^ ThePreferences values select: [:aPreference | aPreference category == aCategory ]! !

!PreferenceNG class methodsFor: 'instance creation' stamp: 'hlsf 5/3/2022 19:16:59'!
name: nameSymbol category: categorySymbol value: aValue
	self name: nameSymbol description: nil category: categorySymbol type: nil value: aValue 
! !

!PreferenceNG class methodsFor: 'instance creation' stamp: 'hlsf 5/3/2022 19:59:49'!
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
	! !

!PreferenceNG class methodsFor: 'error handling' stamp: 'hlsf 4/30/2022 22:20:35'!
doesNotUnderstand: aMessage
	aMessage hasArguments ifTrue: [^ super doesNotUnderstand: aMessage].
	^ self at: aMessage selector! !

!PreferenceNG class methodsFor: 'class initialization' stamp: 'hlsf 5/7/2022 18:06:54'!
initialize
	ThePreferences _ Dictionary new.
	#(gui font programming system) do: [: category | 
		(self perform: (#pref, category capitalized) asSymbol) do: [:aPrefArray |
			self installDefault: aPrefArray in: category] ].
	self installHaloPreferencesWith: self iconicHaloSpecifications.
	self installMiscPreferences ! !

!PreferenceNG class methodsFor: 'class initialization' stamp: 'hlsf 5/7/2022 00:47:32'!
installDefault: anArray in: category
" Description of preference in Array: name - value - type (optional) - event handler (optional) "
	| myPref |
	myPref _ PreferenceNG name: anArray first 				description: '' category: category 
		type: (anArray at: 3 ifAbsent: [self detectType: anArray second]) value: anArray second.
	anArray size = 4 ifTrue: [ "fourth record is an event listener hooked to Preference"
		myPref when: #preferenceChanged send: anArray fourth to: PreferenceNG.
		myPref triggerEvent: #preferenceChanged with: myPref]
	! !

!PreferenceNG class methodsFor: 'class initialization' stamp: 'hlsf 5/4/2022 16:10:52'!
installHaloPreferencesWith: anArray
	| aColor |
	^ PreferenceNG 
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

!PreferenceNG class methodsFor: 'class initialization' stamp: 'hlsf 5/12/2022 11:35:43'!
installMiscPreferences
" Preference value defined with closure. We could insert in a dynamic array...
  Preference with event handler "
	PreferenceNG 
		name: #haloHandleSize category: #gui 
		value: [(PreferenceNG at: #standardListFont) pointSize * 5 // 3 max: 16].
	PreferenceNG
		name: #roundedButtonRadius 	category: #gui 
		value: [(PreferenceNG at: #standardListFont) pointSize * 8 // 14].
	PreferenceNG
		name: #roundedWindowRadius 	category: #gui value: [(PreferenceNG at: #standardListFont) pointSize].
	PreferenceNG
		name: #scrollbarThickness category: #gui 		value: [(PreferenceNG at: #windowTitleFont) pointSize + 2].
	PreferenceNG name: #classFinder category: #programming value: [ BrowserWindow findClass ].
	PreferenceNG name: #defaultAuthorName category: #programming value: [Utilities authorName].! !

!PreferenceNG class methodsFor: 'fileIn/Out' stamp: 'hlsf 5/6/2022 11:42:57'!
loadFrom: aFileEntry
" Load all preferences from file, update the existing preferences with the data from file "
	(SmartRefStream restoreFromFile: aFileEntry) do: [:aPref | 
		PreferenceNG 
			name: aPref name 
			description: aPref description 
			category: aPref category 
			type: aPref type 
			value: (aPref instVarNamed: #value)	 " We want the raw value "]! !

!PreferenceNG class methodsFor: 'fileIn/Out' stamp: 'hlsf 4/30/2022 22:20:56'!
save: nameSymbol to: aFileEntry
" Save one preference to a file, even for a sole preference we save in a Dictionary"
	| myPref |
	myPref _ self instanceAt: nameSymbol.
	SmartRefStream 
		dumpOnFile: aFileEntry 
		object: {myPref name -> myPref} asDictionary! !

!PreferenceNG class methodsFor: 'fileIn/Out' stamp: 'hlsf 4/30/2022 21:58:19'!
saveAllTo: aFileEntry
" Save all the preferences to a file "
	SmartRefStream dumpOnFile: aFileEntry object: ThePreferences! !

!PreferenceNG class methodsFor: 'fileIn/Out' stamp: 'hlsf 4/30/2022 21:57:59'!
saveCategory: categorySymbol to: aFileEntry
	| myPref |
	myPref _ self select: categorySymbol.
	SmartRefStream 
		dumpOnFile: aFileEntry 
		object: 	(myPref collect: [:aPref | aPref name -> aPref ]) asDictionary! !

!PreferenceNG class methodsFor: 'helpers' stamp: 'hlsf 5/12/2022 13:07:56'!
detectType: anObject
"When the type is not provided, we can try to deduce it "
	^ anObject class name
		caseOf: {
			[#True] -> [Boolean].
			[#False] -> [Boolean].
			[#StrikeFont] -> [AbstractFont].
			[#TrueTypeFont ] -> [AbstractFont]
		}
		otherwise: [anObject class]
! !

!PreferenceNG class methodsFor: 'data' stamp: 'hlsf 5/6/2022 23:07:40'!
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

!PreferenceNG class methodsFor: 'data' stamp: 'hlsf 5/7/2022 17:54:36'!
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

!PreferenceNG class methodsFor: 'data' stamp: 'hlsf 5/7/2022 17:54:24'!
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

!PreferenceNG class methodsFor: 'data' stamp: 'hlsf 5/7/2022 17:54:51'!
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

!PreferenceNG class methodsFor: 'data' stamp: 'hlsf 5/12/2022 10:49:26'!
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

!PreferenceNG class methodsFor: 'data' stamp: 'hlsf 5/12/2022 08:48:37'!
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

!PreferenceNG class methodsFor: 'data' stamp: 'hlsf 5/12/2022 12:05:02'!
prefProgramming
	^ #(
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

!PreferenceNG class methodsFor: 'data' stamp: 'hlsf 5/12/2022 11:50:23'!
prefSystem
	^ #( #(allowNonLocalReturnsInExceptionHandlers true )
		#(askConfirmationOnQuit true )
		#(askToInstallComplexPackage true )
		#(automaticPlatformSettings true )
		#(autoNumberUserChanges false )
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

!PreferenceNG class methodsFor: 'event handlers' stamp: 'hlsf 5/7/2022 00:41:45'!
assignmentGlyph: assignmentPref
" #assignmentGliphSelector changed "
	AbstractFont withAllSubclassesDo: [ :fontClass | fontClass assignmentGlyphSelectorPreferenceChanged ]! !

!PreferenceNG class methodsFor: 'event handlers' stamp: 'hlsf 5/7/2022 00:41:12'!
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
	^font! !

!PreferenceNG class methodsFor: 'event handlers' stamp: 'hlsf 5/7/2022 18:08:45'!
machine: machinePref
" Update the preferences for this kind of machine (#default, #slow or #smalltalk80) "
	| prefArray |
	prefArray _ self perform: (#machine, machinePref value capitalized) asSymbol.
	prefArray do: [:array | 		PreferenceNG at: array first put: array second].
	machinePref value == #slow ifTrue: [		self runningWorld backgroundImageData: nil]
	! !

!PreferenceNG class methodsFor: 'event handlers' stamp: 'hlsf 5/7/2022 01:09:27'!
menuIcon: wantsMenuIconsPref
	Theme current class beCurrent! !

!PreferenceNG class methodsFor: 'event handlers' stamp: 'hlsf 5/7/2022 17:43:12'!
pointer: pointerPref
	| enabled |
	enabled _ pointerPref value == #touch.
	#(tapAndHoldEmulatesButton2 clickGrabsMorphs) do: [:aPref | 		PreferenceNG at: aPref put: enabled]! !

!PreferenceNG class methodsFor: 'fonts' stamp: 'hlsf 5/3/2022 18:58:38'!
fontSizes
	^ `{
		#tinyFonts -> 7 .
		#verySmallFonts -> 9 .
		#smallFonts ->11 .
		#standardFonts -> 14 .
		#largeFonts -> 18 .
		#veryLargeFonts -> 24 .
		#hugeFonts -> 32} asDictionary`
		! !

!PreferenceNG class methodsFor: 'fonts' stamp: 'hlsf 5/12/2022 11:53:18'!
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
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].! !

!PreferenceNG class methodsFor: 'fonts' stamp: 'hlsf 5/3/2022 19:39:30'!
setDefaultFont: fontFamilyName spec: defaultFontsSpec
	| font |
	defaultFontsSpec do: [ :triplet |
		font _ FontFamily familyName: fontFamilyName pointSize: triplet second.
		font ifNil: [ font _ FontFamily defaultFamilyAndPointSize ]. 
		triplet size > 2 ifTrue: [			font _ font emphasized: triplet third ].
		PreferenceNG at: triplet first put: font ].
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].! !

!PreferenceNG class methodsFor: 'obsolete' stamp: 'hlsf 5/4/2022 14:56:05'!
desktopMenuTitle
"I have project to get rid of this one"
	^ 'World'   ! !

PreferenceNG initialize!
