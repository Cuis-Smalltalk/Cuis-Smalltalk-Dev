'From Cuis7.1 [latest update: #6735] on 20 September 2024 at 3:26:55 pm'!

!SystemDictionary methodsFor: 'browsing - main' stamp: 'jmv 9/20/2024 15:24:04'!
browserWindowClass
	^Preferences at: #browserWindowClass! !


!PreferenceSet class methodsFor: 'sys preference' stamp: 'jmv 9/20/2024 15:26:34'!
installMiscPreferences
" Preference value defined with closure. We could insert in a dynamic array...
  Preference with event handler "
	self sysPreferences

		name: #haloHandleSize 
		category: #gui 
		value: [(Preferences at: #standardListFont) pointSize * 5 // 3 max: 16];
		
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
		value: BrowserWindow.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences name: #browserWindowClass category: #programming value: BrowserWindow.!

