'From Cuis 6.0 [latest update: #5434] on 4 August 2022 at 4:20:39 pm'!

!TheWorldMenu class methodsFor: 'instance creation' stamp: 'hlsf 8/7/2022 12:48:06'!
getMenuIn: aWorld at: anActiveHand
	^ (self new :: world: aWorld 	hand: anActiveHand)
		buildWorldMenu! !


!PreferenceSet class methodsFor: 'sys data' stamp: 'hlsf 8/4/2022 16:10:05'!
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

!PreferenceSet class methodsFor: 'sys preference' stamp: 'hlsf 8/4/2022 15:59:48'!
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
		value: [Utilities authorName];
		
		name: #worldMenu
		category: #system
		value: TheWorldMenu ! !


!WorldMorph methodsFor: 'world menu' stamp: 'hlsf 8/4/2022 16:20:07'!
invokeWorldMenu
	"Put up the world menu, triggered by the passed-in event.
	Perhaps a good place to disable it if needed"

	| menu |
	menu _ (Preferences at: #worldMenu) getMenuIn: self at: self activeHand.
	menu popUpInWorld: self! !


!TheWorldMenu methodsFor: 'construction' stamp: 'hlsf 8/4/2022 16:08:32'!
buildWorldMenu
	"Build the menu that is put up when the screen-desktop is clicked on"

	^(DynamicMenuBuilder buildTargeting: self collectingMenuOptionsWith: #worldMenuOptions)
		addStayUpIcons;
		addTitle: 'World';
		yourself
	
	! !

!methodRemoval: TheWorldMenu class #getMenuIn:with: stamp: 'hlsf 8/4/2022 16:20:10'!
TheWorldMenu class removeSelector: #getMenuIn:with:!
!methodRemoval: TheWorldMenu class #world:hand: stamp: 'hlsf 8/4/2022 16:20:13'!
TheWorldMenu class removeSelector: #world:hand:!
!methodRemoval: PreferenceSet class #desktopMenuTitle stamp: 'hlsf 8/4/2022 15:45:31'!
PreferenceSet class removeSelector: #desktopMenuTitle!

!PreferenceSet class reorganize!
('sys data' iconicHaloSpecifications machineDefault machineSlow machineSmalltalk80 prefFont prefGui prefProgramming prefSystem)
('sys preference' init installDefault:in: installHaloPreferencesWith: installMiscPreferences sysPreferences)
('sys fonts' fontSizes setDefaultFont: setDefaultFont:spec:)
('sys event handlers' assignmentGlyph: defaultFontSize: machine: pointer:)
('file list' isInitialFileListDirectory:)
('services' openPreferencesInspector)
!


!TheWorldMenu reorganize!
('commands' cleanUpWorld disableFocusFollowsMouse enableFocusFollowsMouse fullScreenOff fullScreenOn garbageCollect newMorphOfClass:event: quitSession saveWorldInFile splitNewMorphList:depth: startMessageTally vmStatistics)
('construction' alphabeticalMorphMenu buildWorldMenu newMorph)
('mechanics' menu: myWorld world:hand:)
('menu' openCommentGuide openPackageDownloader openTerseGuide)
('popups' doPopUp:)
('private' pvtAlphaSplitListDepth pvtCategorySplitListDepth pvtMaxItemsPerCategorySubMenu pvtNewMenuForSplitLists:)
!


!TheWorldMenu class reorganize!
('menu building' additionalMenuOptions changesLabel changesMenuOptions debugLabel debugMenuOptions helpLabel helpMenuOptions openLabel preferencesLabel preferencesMenuOptions windowsLabel windowsMenuOptions worldMainMenuOptions worldMenuOptions)
('instance creation' getMenuIn:at:)
!

"Postscript:
Install the the desktop menu title as a preference entry"
Preferences name: #worldMenu category: #system value: TheWorldMenu.
!

