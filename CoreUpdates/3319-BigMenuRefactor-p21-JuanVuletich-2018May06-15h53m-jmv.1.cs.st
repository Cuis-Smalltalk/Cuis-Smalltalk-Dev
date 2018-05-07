'From Cuis 5.0 of 7 November 2016 [latest update: #3318] on 6 May 2018 at 4:50:12 pm'!

!TextEditor methodsFor: 'attributes' stamp: 'jmv 5/6/2018 16:14:40'!
changeEmphasisOrAlignment
	"This is a user command, and generates undo"

	| menuStrings aList reply code align menuList startIndex attribute |
	startIndex _ self startIndex.
	aList _ #(normal bold italic underlined struckThrough leftFlush centered rightFlush justified).
	align _ model actualContents alignmentAt: startIndex.
	code _ model actualContents emphasisAt: startIndex.
	menuList _ WriteStream on: Array new.
	menuList nextPut: (code isZero ifTrue:['<on>'] ifFalse:['<off>']), 'normal'.
	menuList nextPutAll: (#(bold italic underlined struckThrough superscript subscript withST80Glyphs) collect: [ :emph |
		(code anyMask: (TextEmphasis perform: emph) emphasisCode)
			ifTrue: [ '<on>', emph asString ]
			ifFalse: [ '<off>', emph asString ]]).
	menuList nextPutAll: (#(leftFlush centered rightFlush justified) withIndexCollect: [ :type :i |
		align = (i-1)
			ifTrue: [ '<on>', type asString ]
			ifFalse: [ '<off>', type asString ]]).
	menuStrings _ menuList contents.
	aList _ #(normal bold italic underlined struckThrough superscript subscript withST80Glyphs leftFlush centered rightFlush justified).
	reply _ (SelectionMenu labelList: menuStrings lines: #(1 8) selections: aList) startUpMenu.
	reply ifNotNil: [
		(#(leftFlush centered rightFlush justified) includes: reply)
			ifTrue: [
				attribute _ TextAlignment perform: reply]
			ifFalse: [
				attribute _ TextEmphasis perform: reply].
		((menuStrings at: (aList indexOf: reply)) beginsWith: '<on>')
			ifTrue: [ self unapplyAttributes: {attribute} ]
			ifFalse: [ self applyAttribute:  attribute ]].
	^ true! !


!Preferences class methodsFor: 'menu icons' stamp: 'jmv 5/6/2018 16:43:04'!
useMenuIcons

	self setPreference: #wantsMenuIcons toValue: true.
	Theme current class beCurrent! !

!Preferences class methodsFor: 'menu icons' stamp: 'jmv 5/6/2018 16:43:10'!
useNoMenuIcons

	self setPreference: #wantsMenuIcons toValue: false.
	Theme current class beCurrent! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/6/2018 16:38:18'!
bigFonts
	"Sets not only fonts but other GUI elements
	to fit high resolution or large screens
	Preferences bigFonts
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 11)
			(setListFontTo: 11)
			(setMenuFontTo: 12)
			(setWindowTitleFontTo: 14)
			(setCodeFontTo: 11)
			(setButtonFontTo: 11)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/6/2018 16:38:21'!
hugeFonts
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences hugeFonts
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 17)
			(setListFontTo: 17)
			(setMenuFontTo: 17)
			(setWindowTitleFontTo: 22)
			(setCodeFontTo: 17)
			(setButtonFontTo: 17)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/6/2018 16:38:23'!
smallFonts
	"Sets not only fonts but other GUI elements
	to fit low resolution or small screens
	Preferences smallFonts
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 8)
			(setListFontTo: 8)
			(setMenuFontTo: 8)
			(setWindowTitleFontTo: 11)
			(setCodeFontTo: 8)
			(setButtonFontTo: 8)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/6/2018 16:38:26'!
standardFonts
	"Sets not only fonts but other GUI elements
	to fit regular resolution and size screens
	Preferences standardFonts
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 9)
			(setListFontTo: 9)
			(setMenuFontTo: 10)
			(setWindowTitleFontTo: 12)
			(setCodeFontTo: 9)
			(setButtonFontTo: 9)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/6/2018 16:38:28'!
tinyFonts
	"Sets not only fonts but other GUI elements
	to fit very low resolution or very small screens
	Preferences tinyFonts
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 7)
			(setListFontTo: 5)
			(setMenuFontTo: 5)
			(setWindowTitleFontTo: 7)
			(setCodeFontTo: 5)
			(setButtonFontTo: 5)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/6/2018 16:38:30'!
veryBigFonts
	"Sets not only fonts but other GUI elements
	to fit very high resolution or very large screens
	Preferences veryBigFonts
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 14)
			(setListFontTo: 14)
			(setMenuFontTo: 14)
			(setWindowTitleFontTo: 17)
			(setCodeFontTo: 14)
			(setButtonFontTo: 14)).
	Preferences enable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !

!Preferences class methodsFor: 'bigger and smaller GUI' stamp: 'jmv 5/6/2018 16:38:32'!
verySmallFonts
	"Sets not only fonts but other GUI elements
	to fit low resolution or small screens
	Preferences verySmallFonts
	"

	Preferences
		setDefaultFont: Preferences defaultFontFamily
		spec: #(
			(setSystemFontTo: 8)
			(setListFontTo: 7)
			(setMenuFontTo: 7)
			(setWindowTitleFontTo: 9)
			(setCodeFontTo: 7)
			(setButtonFontTo: 7)).
	Preferences disable: #biggerCursors.
	DisplayScreen runningWorld ifNotNil: [ :world | world fontPreferenceChanged ]! !


!TheWorldMenu methodsFor: 'commands' stamp: 'jmv 5/6/2018 16:04:24'!
startMessageTally
	"Tally on all the processes in the system, and not only the UI"
	
	| d |
	(self confirm: 'MessageTally all the processes in
the system, until the mouse pointer
goes to the top of the screen') ifTrue: [
		[
			d _ Delay forMilliseconds: 100.
			AndreasSystemProfiler spyAllOn: [
				[Sensor peekMousePt y > 10] whileTrue: [d wait]]
			] forkAt: Processor userInterruptPriority
		]! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/6/2018 16:07:49'!
debugMenu

	^ (self menu: 'Debug...')
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'Inspect World'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#inspect.
				#icon 			-> 		#inspectIcon.
			} asDictionary.
			{
				#label 			-> 		'Explore World'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#explore.
				#icon 			-> 		#exploreIcon.
			} asDictionary.
			{
				#label 			-> 		'MessageTally all Processes'.
				#selector 		-> 		#startMessageTally.
				#icon 			-> 		#systemMonitorIcon.
			} asDictionary.
			nil.
			{
				#label 			-> 		'Start drawing all again'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#removeAllKnownFailing.
				#icon 			-> 		#graphicsIcon.
			} asDictionary.
			{
				#label 			-> 		'Start stepping again'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#resumeAfterStepError.
				#icon 			-> 		#mediaPlaybackStartIcon.
			} asDictionary.
			{
				#label 			-> 		'Close all Debuggers'.
				#object 			-> 		DebuggerWindow.
				#selector 		-> 		#closeAllDebuggers.
				#icon 			-> 		#closeIcon.
			} asDictionary.
		}`! !

!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 5/6/2018 16:21:32'!
preferencesMenu
	"Build the preferences menu for the world."

	^ (self menu: 'Preferences...')
		addItemsFromDictionaries: `{
			{
				#label 			-> 		'Focus follows mouse'.
				#object 			-> 		Preferences.
				#selector 		-> 		#enableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#label 			-> 		'Click to focus'.
				#object 			-> 		Preferences.
				#selector 		-> 		#disableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.
			{
				#label 			-> 		'Font Sizes...'.
				#object 			-> 		Theme.
				#selector 		-> 		#changeFontSizes.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'use larger or smaller text and widgets'
			} asDictionary.
			{
				#label 			-> 		'Icons...'.
				#object 			-> 		Theme.
				#selector 		-> 		#changeIcons.
				#icon 			-> 		#worldIcon.
				#balloonText 	-> 		'show more or less icons.'
			} asDictionary.
			{
				#label 			-> 		'Themes...'.
				#object 			-> 		Theme.
				#selector 		-> 		#changeTheme.
				#icon 			-> 		#appearanceIcon.
				#balloonText 	-> 		'switch to another theme.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Show taskbar'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#showTaskbar.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'show the taskbar'
			} asDictionary.
			{
				#label 			-> 		'Hide taskbar'.
				#object 			-> 		#myWorld.
				#selector 		-> 		#hideTaskbar.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'hide the taskbar'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Full screen on'.
				#selector 		-> 		#fullScreenOn.
				#icon 			-> 		#viewFullscreenIcon.
				#balloonText 	-> 		'puts you in full-screen mode, if not already there.'
			} asDictionary.
			{
				#label 			-> 		'Full screen off'.
				#selector 		-> 		#fullScreenOff.
				#icon 			-> 		#exitFullscreenIcon.
				#balloonText 	-> 		'if in full-screen mode, takes you out of it.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Set Code Author...'.
				#object 			-> 		Utilities.
				#selector 		-> 		#setAuthor.
				#icon 			-> 		#usersIcon.
				#balloonText 	-> 		'supply initials to be used to identify the author of code and other content.'
			} asDictionary.
			{
				#label 			-> 		'All preferences...'.
				#object 			-> 		Preferences.
				#selector 		-> 		#openPreferencesInspector.
				#icon 			-> 		#preferencesIcon.
				#balloonText 	-> 		'view and change various options.'
			} asDictionary.
		}`! !


!Theme class methodsFor: 'instance creation' stamp: 'jmv 5/6/2018 16:45:50'!
beCurrent
	self currentTheme: self.
	self inform: 'Please close and reopen all windows'! !

!Theme class methodsFor: 'user interface' stamp: 'jmv 5/6/2018 16:42:00'!
changeFontSizes

	(MenuMorph new defaultTarget: Preferences)
		addTitle: 'Choose a size';
		addStayUpIcons;
		add: 'Huge Fonts' action: #hugeFonts;
		add: 'Very big Fonts' action: #veryBigFonts;
		add: 'Big Fonts' action: #bigFonts;
		add: 'Standard Fonts' action: #standardFonts;
		add: 'Small Fonts' action: #smallFonts;
		add: 'Very small Fonts'action: #verySmallFonts;
		add: 'Tiny Fonts'action: #tinyFonts;
		popUpInWorld: self runningWorld! !

!Theme class methodsFor: 'user interface' stamp: 'jmv 5/6/2018 16:43:14'!
changeIcons

	(MenuMorph new defaultTarget: Preferences)
		addTitle: 'Menu Icons';
		addStayUpIcons;
		add: 'Use icons for menu entries' action: #useMenuIcons;
		add: 'Don''t use icons for menu entries' action: #useNoMenuIcons;
		popUpInWorld: self runningWorld! !

!Theme class methodsFor: 'user interface' stamp: 'jmv 5/6/2018 16:49:26'!
changeTheme

	| menu |
	menu _ MenuMorph new.
	menu
		addTitle: 'Choose a theme';
		addStayUpIcons.
	Theme withAllSubclassesDo: [ :themeClass |
		menu add: themeClass name target: themeClass action: #beCurrent ].
	menu popUpInWorld: self runningWorld! !

!Theme class methodsFor: 'icons by menu' stamp: 'jmv 5/6/2018 16:44:00'!
addBasicIconsTo: aCollectorCollection

	aCollectorCollection
		add: #('save' ) -> #saveIcon;
		add: #('change category...' 'rename') -> #saveAsIcon;
		add: #('quit') -> #quitIcon;
		add: #('inspect it (i)' 'inspect world'  'inspect model' 'inspect morph'
		 'inspect owner chain' 'inspect' 'inspect (i)' 'basic inspect' 'message names' 'find message names' 'inspect instances' 'inspect subinstances' 'inspect change set' 'inspect context (c)' 'inspect receiver (i)' 'start CPUWatcher' 'stop CPUWatcher')
			-> #inspectIcon;
		add: #('explore' 'explore it (I)' 'explore morph' 'explore (I)' 'explore context (C)' 'explore receiver (I)' 'references finder' 'weight explorer' 'explore CompiledMethod') -> #exploreIcon;
		add: #('find...(f)' 'find class... (f)' 'find method...' 'show hierarchy' 'show definition' 'show comment' 'filter' 'filter message list...' 'find context... (f)') -> #findIcon;
		add: #('add item...' 'new category...' 'create new change set...' 'new change set... (n)' 'add new file') -> #newIcon;
		add: #('remove method (x)' 'remove' 'remove class (x)' 'remove method from system (x)' 'remove class from system (x)' 'remove postscript''stop StackSizeWatcher' ) -> #deleteIcon;
		add: #('delete method from changeset (d)' 'delete class from change set (d)' 'revert & remove from changes' 'reset variables' 'remove contained in class categories...' 'clear this change set' 'uninstall this change set' 'delete directory...' 'delete' 'start StackSizeWatcher') -> #warningIcon;
		add: #('revert to previous version') -> #undoIcon;
		add: #('copy to clipboard (c)' 'copy class...' 'copy name to clipboard') -> #copyIcon
! !


!PopUpMenu methodsFor: 'basic control sequence' stamp: 'jmv 5/6/2018 16:27:31'!
startUpWithCaption: captionOrNil
	"Display the menu, slightly offset from the cursor,
	so that a slight tweak is required to confirm any action."
	^ self startUpWithCaption: captionOrNil at: Sensor mousePoint allowKeyboard: Preferences menuKeyboardControl! !

!methodRemoval: PopUpMenu #startUpWithCaption:at:!
PopUpMenu removeSelector: #startUpWithCaption:at:!
!methodRemoval: PopUpMenu #startUpWithoutKeyboard!
PopUpMenu removeSelector: #startUpWithoutKeyboard!
!methodRemoval: TheWorldMenu #changeBackgroundColor!
TheWorldMenu removeSelector: #changeBackgroundColor!
!methodRemoval: TheWorldMenu #setDisplayDepth!
TheWorldMenu removeSelector: #setDisplayDepth!
!methodRemoval: TheWorldMenu #startThenBrowseMessageTally!
TheWorldMenu removeSelector: #startThenBrowseMessageTally!
!methodRemoval: ProgressBarMorph #addCustomMenuItems:hand:!
ProgressBarMorph removeSelector: #addCustomMenuItems:hand:!
!methodRemoval: ProgressBarMorph #changeProgressColor:!
ProgressBarMorph removeSelector: #changeProgressColor:!
!methodRemoval: ProgressBarMorph #changeProgressValue:!
ProgressBarMorph removeSelector: #changeProgressValue:!
!methodRemoval: ProgressBarMorph #progressColor!
ProgressBarMorph removeSelector: #progressColor!
!methodRemoval: ProgressBarMorph #progressColor:!
ProgressBarMorph removeSelector: #progressColor:!
!methodRemoval: Morph #changeColorTarget:selector:originalColor:hand:!
Morph removeSelector: #changeColorTarget:selector:originalColor:hand:!

!ProgressBarMorph reorganize!
('accessing' progressValue progressValue:)
('drawing' drawOn:)
('initialization' defaultBorderWidth defaultColor initialize)
!

