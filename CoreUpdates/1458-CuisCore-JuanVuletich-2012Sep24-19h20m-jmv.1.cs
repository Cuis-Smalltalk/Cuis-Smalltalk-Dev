'From Cuis 4.0 of 21 April 2012 [latest update: #1457] on 24 September 2012 at 7:22:55 pm'!
!classDefinition: #Utilities category: #'System-Support'!
Object subclass: #Utilities
	instanceVariableNames: ''
	classVariableNames: 'AuthorInitials AuthorName CommonRequestStrings LastStats RecentSubmissions '
	poolDictionaries: ''
	category: 'System-Support'!

!PluggableListMorph methodsFor: 'menus' stamp: 'jmv 9/24/2012 18:58'!
addCustomMenuItems:  aMenu hand: aHandMorph
	"Add halo menu items to be handled by the invoking hand. The halo menu is invoked by clicking on the menu-handle of the receiver's halo."

	super addCustomMenuItems: aMenu hand: aHandMorph.
	aMenu addLine.
	aMenu add: 'copy list to clipboard' target: self action: #copyListToClipboard.
	aMenu add: 'copy selection to clipboard' target: self action: #copySelectionToClipboard! !


!RealEstateAgent class methodsFor: 'as yet unclassified' stamp: 'jmv 9/24/2012 18:43'!
maximumUsableArea

	| allowedArea |
	allowedArea _ Display boundingBox.
	^allowedArea
! !


!MessageSetWindow methodsFor: 'menu commands' stamp: 'jmv 9/24/2012 18:50'!
filterMessageList
	"Allow the user to refine the list of messages."

	| aMenu |
	model messageList size <= 1 
		ifTrue: [^self inform: 'this is not a propitious filtering situation'].

	aMenu := MenuMorph new defaultTarget: model.
	aMenu addTitle: 'Filter by only showing...'.
	aMenu addStayUpIcons.
	aMenu 
		addList: #(
			#('unsent messages' #filterToUnsentMessages 'filter to show only messages that have no senders')
			#-
			#('messages that send...' #filterToSendersOf 'filter to show only messages that send a selector I specify')
			#('messages that do not send...' #filterToNotSendersOf 'filter to show only messages that do not send a selector I specify')
			#-
			#('messages whose selector is...' #filterToImplementorsOf 'filter to show only messages with a given selector I specify')
			#('messages whose selector is NOT...' #filterToNotImplementorsOf 'filter to show only messages whose selector is NOT a seletor I specify')
			#-
			#('messages in any change set' #filterToAnyChangeSet 'filter to show only messages that occur in at least one change set')
			#('messages not in any change set' #filterToNotAnyChangeSet 'filter to show only messages that do not occur in any change set in the system')
			#-
			#('messages authored by me' #filterToCurrentAuthor 'filter to show only messages whose authoring stamp has my initials')
			#('messages not authored by me' #filterToNotCurrentAuthor 'filter to show only messages whose authoring stamp does not have my initials')
			#-
			#('messages logged in .changes file' #filterToMessagesInChangesFile 'filter to show only messages whose latest source code is logged in the .changes file')
			#('messages only in .sources file' #filterToMessagesInSourcesFile 'filter to show only messages whose latest source code is logged in the .sources file')
			#-
			#('messages with prior versions' #filterToMessagesWithPriorVersions 'filter to show only messages that have at least one prior version')
			#('messages without prior versions' #filterToMessagesWithoutPriorVersions 'filter to show only messages that have no prior versions')
			#-
			#('uncommented messages' #filterToUncommentedMethods 'filter to show only messages that do not have comments at the beginning')
			#('commented messages' #filterToCommentedMethods 'fileter to show only messages that have comments at the beginning')
			#-
			#('messages that...'
			#filterToMessagesThat 'let me type in a block taking a class and a selector, which will specify yea or nay concerning which elements should remain in the list')
		).
	aMenu popUpInWorld: self world! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 9/24/2012 18:56'!
buildWorldMenu
	"Build the menu that is put up when the screen-desktop is clicked on"

	| menu |
	menu _ MenuMorph new defaultTarget: self.
	self colorForDebugging: menu.
	menu addStayUpIcons.
	self fillIn: menu
		from: {
				{ 'Open...'. { self. #openWindow}}.
				{ 'New morph...'. { self. #newMorph}.
					'Offers a variety of ways to create new objects'}.
				{ 'Preferences...'. { self. #preferencesDo}.
					'put up a menu offering many controls over appearance and system preferences.'}.
				{ 'Windows...'. { self. #windowsDo}}.
				{ 'Help...'. { self. #helpDo}.
					'puts up a menu of useful items for updating the system, determining what version you are running, and much else'}.
				nil.
				{ 'Changes...'. { self. #changesDo}}.
				{ 'Debug...'. { self. #debugDo}.
					'a menu of debugging items'}.
				{ 'Restore Display (r)'. { World. #restoreMorphicDisplay}.
					'repaint the screen -- useful for removing unwanted display artifacts, lingering cursors, etc.'}.
				nil.
				{ 'Save'. { Smalltalk . #saveSession}.
					'save the current version of the image on disk'}.
				{ 'Save as...'. { Smalltalk . #saveAs}.
					'save the current version of the image on disk under a new name.'}.
				{ 'Save as New Version'. { Smalltalk . #saveAsNewVersion}.
					'give the current image a new version-stamped name and save it under that name on disk.'}.
				{ 'Save and Quit'. { self. #saveAndQuit}.
					'save the current image on disk, and quit out of Cuis.'}.
				{ 'Quit'. { self. #quitSession}.
					'quit out of Cuis.'}}.
	^menu! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 9/24/2012 18:59'!
preferencesMenu
	"Build the preferences menu for the world."

	^self fillIn: (self menu: 'Preferences...') from: {
		{'Themes...' . {Theme . #changeTheme} . 'switch to another theme.'}.
		{'Icons...' . {Theme . #changeIcons} . 'show more or less icons.'}.
		nil.
		{'Show taskbar' . {Taskbar . #show} . 'show the taskbar'}.
		{'Hide taskbar' . {Taskbar . #hide} . 'hide the taskbar'}.
		nil.
		{'Full screen on' . { self . #fullScreenOn} . 'puts you in full-screen mode, if not already there.'}.
		{'Full screen off' . { self . #fullScreenOff} . 'if in full-screen mode, takes you out of it.'}.
		nil.
		{'Set display depth...' . {self. #setDisplayDepth} . 'choose how many bits per pixel.'}.
		{'Set desktop color...' . {self. #changeBackgroundColor} . 'choose a uniform color to use as desktop background.'}.
		nil.
		{'Set Code Author...'. {Utilities. #setAuthor}. 'supply initials to be used to identify the author of code and other content.'}.
		{'All preferences...'. {Preferences. #openPreferencesInspector}. 'view and change various options.'}.
	}! !


!Utilities class methodsFor: 'common requests' stamp: 'jmv 9/24/2012 18:56'!
initialize
	"Initialize the class variables.  5/16/96 sw"
	RecentSubmissions _ OrderedCollection new! !

!methodRemoval: WorldState #listOfSteppingMorphs!
WorldState removeSelector: #listOfSteppingMorphs!
!methodRemoval: WorldState #stepListSize!
WorldState removeSelector: #stepListSize!
!methodRemoval: Utilities class #appendToCommonRequests:!
Utilities class removeSelector: #appendToCommonRequests:!
!methodRemoval: Utilities class #cleanseOtherworldlySteppers!
Utilities class removeSelector: #cleanseOtherworldlySteppers!
!methodRemoval: Utilities class #commonRequestStrings:!
Utilities class removeSelector: #commonRequestStrings:!
!methodRemoval: Utilities class #editCommonRequestStrings!
Utilities class removeSelector: #editCommonRequestStrings!
!methodRemoval: Utilities class #initializeCommonRequestStrings!
Utilities class removeSelector: #initializeCommonRequestStrings!
!methodRemoval: Utilities class #offerCommonRequestsInMorphic!
Utilities class removeSelector: #offerCommonRequestsInMorphic!
Utilities initialize!
!classDefinition: #Utilities category: #'System-Support'!
Object subclass: #Utilities
	instanceVariableNames: ''
	classVariableNames: 'AuthorInitials AuthorName LastStats RecentSubmissions'
	poolDictionaries: ''
	category: 'System-Support'!
!methodRemoval: TheWorldMenu #standardFontDo!
TheWorldMenu removeSelector: #standardFontDo!
!methodRemoval: SystemWindow #amendSteppingStatus!
SystemWindow removeSelector: #amendSteppingStatus!
!methodRemoval: Preferences class #chooseCodeFont!
Preferences class removeSelector: #chooseCodeFont!
!methodRemoval: Preferences class #chooseFontWithPrompt:andSendTo:withSelector:highlight:!
Preferences class removeSelector: #chooseFontWithPrompt:andSendTo:withSelector:highlight:!
!methodRemoval: Preferences class #chooseInitialSettings!
Preferences class removeSelector: #chooseInitialSettings!
!methodRemoval: Preferences class #chooseListFont!
Preferences class removeSelector: #chooseListFont!
!methodRemoval: Preferences class #chooseMenuFont!
Preferences class removeSelector: #chooseMenuFont!
!methodRemoval: Preferences class #chooseSystemFont!
Preferences class removeSelector: #chooseSystemFont!
!methodRemoval: Preferences class #chooseWindowTitleFont!
Preferences class removeSelector: #chooseWindowTitleFont!
!methodRemoval: Preferences class #fontConfigurationMenu!
Preferences class removeSelector: #fontConfigurationMenu!
!methodRemoval: Preferences class #modalColorPickers!
Preferences class removeSelector: #modalColorPickers!
!methodRemoval: Preferences class #printStandardSystemFonts!
Preferences class removeSelector: #printStandardSystemFonts!
!methodRemoval: Preferences class #setFlag:toValue:during:!
Preferences class removeSelector: #setFlag:toValue:during:!
!methodRemoval: Preferences class #setNotificationParametersForStandardPreferences!
Preferences class removeSelector: #setNotificationParametersForStandardPreferences!
!methodRemoval: Preferences class #smartUpdatingChanged!
Preferences class removeSelector: #smartUpdatingChanged!
!methodRemoval: Preference #categoryList!
Preference removeSelector: #categoryList!
!methodRemoval: Preference #categoryList:!
Preference removeSelector: #categoryList:!
!methodRemoval: Preference #defaultValue:!
Preference removeSelector: #defaultValue:!
!methodRemoval: Preference #helpString!
Preference removeSelector: #helpString!
!methodRemoval: Preference #name:defaultValue:helpString:lcategoryList:changeInformee:changeSelector:!
Preference removeSelector: #name:defaultValue:helpString:lcategoryList:changeInformee:changeSelector:!
!methodRemoval: Preference #rawValue:!
Preference removeSelector: #rawValue:!
!methodRemoval: PluggableListMorph #setListFont!
PluggableListMorph removeSelector: #setListFont!
!methodRemoval: PasteUpMorph #listOfSteppingMorphs!
PasteUpMorph removeSelector: #listOfSteppingMorphs!
!methodRemoval: PasteUpMorph #stepListSize!
PasteUpMorph removeSelector: #stepListSize!
!methodRemoval: PasteUpMorph #steppingMorphsNotInWorld!
PasteUpMorph removeSelector: #steppingMorphsNotInWorld!

!PasteUpMorph reorganize!
('WiW support' shouldGetStepsFrom:)
('accessing' color:)
('alarms-scheduler' addAlarm:withArguments:for:at: removeAlarm:for:)
('caching' releaseCachedState)
('change reporting' invalidateRect: redrawNeeded)
('classification' isPlayfieldLike isWorldMorph)
('drawing' drawOn:)
('dropping/grabbing' acceptDroppingMorph:event: dropEnabled morphToDropFrom: repelsMorph:event: wantsDroppedMorph:event:)
('errors on draw' addKnownFailing: isKnownFailing: removeAllKnownFailing removeKnownFailing:)
('events' click:localPosition: mouseDown:localPosition: windowEvent:)
('event handling testing' handlesMouseDown:)
('event handling' morphToGrab: mouseButton2Activity wantsWindowEvent: windowEventHandler)
('events-processing' dispatchEvent:localPosition:)
('geometry' externalizeToWorld: internalizeFromWorld: morphExtent: morphPositionInWorld)
('initialization' clearWaitDelay defaultBorderColor defaultBorderWidth defaultColor initialize)
('interaction loop' doOneCycleNow)
('menu & halo' addCustomMenuItems:hand: addWorldHaloMenuItemsTo:hand: addWorldToggleItemsToHaloMenu: deleteBalloonTarget:)
('misc' backgroundImage backgroundImageData: buildMagnifiedBackgroundImage)
('printing' printOn:)
('project state' canvas firstHand hands handsDo: handsReverseDo: viewBox viewBox:)
('stepping' cleanseStepList runLocalStepMethods runStepMethods startStepping: startStepping:at:selector:arguments:stepTime: stopStepping: stopStepping:selector:)
('stepping and presenter' step wantsSteps)
('structure' world)
('submorphs-accessing' allMorphsDo:)
('submorphs-add/remove' addAllMorphs: addMorphFront:)
('testing' isReallyVisible stepTime)
('world menu' bringWindowsFullOnscreen closeUnchangedWindows collapseAll collapseNonWindows deleteNonWindows expandAll findAChangeSorter: findAFileList: findAMessageNamesWindow: findATranscript: findAWindowSatisfying:orMakeOneUsing: findDirtyBrowsers: findDirtyWindows: findWindow: invokeWorldMenu openRecentSubmissionsBrowser:)
('world state' addMorph:centeredNear: allNonFlapRelatedSubmorphs assuredNonDisplayCanvas deleteAllHalos displayWorld displayWorldSafely doOneCycle doOneSubCycle flashRects: fullRepaintNeeded haloMorphs install privateOuterDisplayWorld restoreMorphicDisplay startSteppingSubmorphsOf:)
('halos and balloon help' wantsHaloHandleWithSelector:inHalo:)
!

!methodRemoval: AbstractFont class #promptForFont:andSendTo:withSelector:highlight:!
AbstractFont class removeSelector: #promptForFont:andSendTo:withSelector:highlight:!
