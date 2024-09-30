'From Cuis7.1 [latest update: #6752] on 26 September 2024 at 4:15:27 pm'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 9/26/2024 16:12:16'!
openSourceFiles
	self imageName = LastImageName ifFalse: [
		LastImageName := self imageName.
		"Reset the author initials to blank when the image gets moved"
		Utilities clearAuthor.
		"And clear any absolute path that is most likely invalid now."
		(Preferences at: #clearPackagePathsOnImageMove) ifTrue: [
			CodePackage clearPaths ]].
	"Warning: Do open the source files only if nil.
	If not nil, it is because they are internalized and the files should not be opened"
	self openSourcesAndChanges.
	CuisSourceFileArray install! !


!PreferenceSet class methodsFor: 'sys preference' stamp: 'jmv 9/26/2024 16:08:00'!
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
		value: BrowserWindow;

		name: #clearPackagePathsOnImageMove
		category: #programming
		value: true.! !

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences name: #clearPackagePathsOnImageMove category: #programming value: true.!

