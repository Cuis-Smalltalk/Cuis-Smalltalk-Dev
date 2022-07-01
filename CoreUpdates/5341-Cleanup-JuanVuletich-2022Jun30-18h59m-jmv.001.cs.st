'From Cuis 6.0 [latest update: #5337] on 30 June 2022 at 7:13:37 pm'!

!PreferenceSet class methodsFor: 'sys preference' stamp: 'jmv 6/30/2022 19:10:15'!
installMiscPreferences
" Preference value defined with closure. We could insert in a dynamic array...
  Preference with event handler "
	self sysPreferences

		name: #haloHandleSize 
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize * 5 // 3 max: 16];
	
		name: #roundedButtonRadius 	
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize * 8 // 14];
		
		name: #roundedWindowRadius 	
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize] ;
		
		name: #scrollbarThickness 
		category: #gui 		
		value: [(Preferences at: #windowTitleFont) pointSize + 2];
		
		name: #classFinder 
		category: #programming 
		value: [ BrowserWindow findClass ];
		
		name: #defaultAuthorName 
		category: #programming 
		value: [Utilities authorName].! !

!PreferenceSet class methodsFor: 'sys event handlers' stamp: 'jmv 6/30/2022 19:10:06'!
defaultFontSize: guiElementSizePref
" #guiElementSize changed "
	| font titleFont pointSize |
	pointSize _ self fontSizes at: guiElementSizePref value.
	font _ FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize.
	Preferences name: #standardListFont category: #font value: font.
	Preferences name: #standardMenuFont category: #font value: font.
	Preferences name: #standardCodeFont category: #font value: font.
	Preferences name: #standardButtonFont category: #font value: font.
	FontFamily defaultFamilyName: font familyName defaultPointSize: pointSize.	
	titleFont _ FontFamily familyName: FontFamily defaultFamilyName pointSize: pointSize * 12//10.
	Preferences name: #windowTitleFont category: #font value: titleFont.
	MorphicCanvas allSubclassesDo: [ :c| c guiSizePreferenceChanged ].
	UISupervisor ui ifNotNil: [ :w | w fontPreferenceChanged ].
	Preferences at: #biggerCursors put: (pointSize > 14).
	Cursor defaultCursor activateCursor.
	^ font! !

!methodRemoval: Preference class #menuIcon: stamp: 'jmv 6/30/2022 19:05:52'!
Preference class removeSelector: #menuIcon:!
!methodRemoval: Preference class #name:description:category:type:value: stamp: 'jmv 6/30/2022 19:12:43'!
Preference class removeSelector: #name:description:category:type:value:!
!methodRemoval: Preference class #name:category:value: stamp: 'jmv 6/30/2022 19:10:55'!
Preference class removeSelector: #name:category:value:!
!methodRemoval: Preference class #machine: stamp: 'jmv 6/30/2022 19:04:30'!
Preference class removeSelector: #machine:!
!methodRemoval: Preference class #fontSizes stamp: 'jmv 6/30/2022 19:03:02'!
Preference class removeSelector: #fontSizes!
!methodRemoval: Preference class #pointer: stamp: 'jmv 6/30/2022 19:06:47'!
Preference class removeSelector: #pointer:!
!methodRemoval: Preference class #openPreferencesInspector stamp: 'jmv 6/30/2022 19:00:46'!
Preference class removeSelector: #openPreferencesInspector!
!methodRemoval: Preference class #desktopMenuTitle stamp: 'jmv 6/30/2022 19:02:51'!
Preference class removeSelector: #desktopMenuTitle!
!methodRemoval: Preference class #assignmentGlyph: stamp: 'jmv 6/30/2022 19:02:29'!
Preference class removeSelector: #assignmentGlyph:!
!methodRemoval: Preference class #setDefaultFont: stamp: 'jmv 6/30/2022 19:07:48'!
Preference class removeSelector: #setDefaultFont:!
!methodRemoval: Preference class #setDefaultFont:spec: stamp: 'jmv 6/30/2022 19:07:59'!
Preference class removeSelector: #setDefaultFont:spec:!
!methodRemoval: Preference class #defaultFontSize: stamp: 'jmv 6/30/2022 19:02:42'!
Preference class removeSelector: #defaultFontSize:!
