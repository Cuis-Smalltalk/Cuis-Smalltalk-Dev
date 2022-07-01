'From Cuis 6.0 [latest update: #5332] on 30 June 2022 at 3:59:30 pm'!

!PreferenceNG class methodsFor: 'toBeRemoved' stamp: 'jmv 6/30/2022 15:57:12'!
isInitialFileListDirectory: x
^false! !

!PreferenceNG class methodsFor: 'toBeRemoved' stamp: 'jmv 6/30/2022 15:55:35'!
soundsEnabled
^true! !


!FileList methodsFor: 'user interface' stamp: 'jmv 6/30/2022 15:56:20'!
toggleInitialDirectory
	"Toggle the initial directory setting of the currently selected directory."
	
	
"
	Preferences isInitialFileListDirectory: (directoryEntry _ currentDirectorySelected item)::
		ifTrue: [
			Preferences removeInitialFileListDirectory: directoryEntry ]
		ifFalse: [
			Preferences addInitialFileListDirectory: directoryEntry ]
"! !


!SystemDictionary methodsFor: 'ui' stamp: 'jmv 6/30/2022 15:55:50'!
beep
	"
	Smalltalk beep
	"
	PreferenceNG soundsEnabled ifTrue: [
		Smalltalk
			at: #SampledSound
			ifPresent: [ :cls | cls beep ]
			ifAbsent: [ self primitiveBeep ]]! !


!FileListWindow methodsFor: 'menu building' stamp: 'jmv 6/30/2022 15:57:17'!
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
		add: (PreferenceNG isInitialFileListDirectory: selectedWrapper item)
			asMenuItemTextPrefix, 'initial directory'
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
				target: PreferenceNG
				action: #at:put:
				argumentList: {#initialFileListDirectories . entry first} ::
					setBalloonText: entry third ].
	aMenu add: 'default initial directories' subMenu: initialDirectoriesMenu.
	^ aMenu! !


!TheWorldMenu methodsFor: 'construction' stamp: 'jmv 6/30/2022 15:58:00'!
preferencesMenu
	"Build the preferences menu for the world."

	^ (self menu: 'Preferences...')
		addItemsFromDictionaries: {
			{
				#label 			-> 		'Focus follows mouse'.
				#object 			-> 		PreferenceNG.
				#selector 			-> 		#enableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one on which the mouse is located.'
			} asDictionary.
			{
				#label 			-> 		'Click to focus'.
				#object 			-> 		PreferenceNG.
				#selector 			-> 		#disableFocusFollowsMouse.
				#icon 			-> 		#windowIcon.
				#balloonText 	-> 		'At all times, make the active window and widget the one where the mouse was clicked.'
			} asDictionary.
			{
				#label 			-> 		'Size of GUI elements...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeFontSizes.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'use larger or smaller text and widgets'
			} asDictionary.
			{
				#label			->		'Set System Font...'.
				#object 			-> 		FontFamily.
				#selector 			-> 		#promptUserAndSetDefault.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'change the current system font family.'
			} asDictionary.
			{
				#label			->		'Load all TrueType Fonts'.
				#object 			-> 		FontFamily.
				#selector 			-> 		#readAdditionalTrueTypeFonts.
				#icon 			-> 		#preferencesDesktopFontIcon.
				#balloonText 	-> 		'Load additional TrueType fonts included with Cuis.'
			} asDictionary.
			{
				#label 			-> 		'Icons...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeIcons.
				#icon 			-> 		#worldIcon.
				#balloonText 	-> 		'show more or less icons.'
			} asDictionary.
			{
				#label 			-> 		'Themes...'.
				#object 			-> 		Theme.
				#selector 			-> 		#changeTheme.
				#icon 			-> 		#appearanceIcon.
				#balloonText 	-> 		'switch to another theme.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Show taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#showTaskbar.
				#icon 			-> 		#expandIcon.
				#balloonText 	-> 		'show the taskbar'
			} asDictionary.
			{
				#label 			-> 		'Hide taskbar'.
				#object 			-> 		#myWorld.
				#selector 			-> 		#hideTaskbar.
				#icon 			-> 		#collapseIcon.
				#balloonText 	-> 		'hide the taskbar'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Full screen on'.
				#selector 			-> 		#fullScreenOn.
				#icon 			-> 		#viewFullscreenIcon.
				#balloonText 	-> 		'puts you in full-screen mode, if not already there.'
			} asDictionary.
			{
				#label 			-> 		'Full screen off'.
				#selector 			-> 		#fullScreenOff.
				#icon 			-> 		#exitFullscreenIcon.
				#balloonText 	-> 		'if in full-screen mode, takes you out of it.'
			} asDictionary.
			nil.
			{
				#label 			-> 		'Set Code Author...'.
				#object 			-> 		Utilities.
				#selector 			-> 		#setAuthor.
				#icon 			-> 		#usersIcon.
				#balloonText 	-> 		'supply initials to be used to identify the author of code and other content.'
			} asDictionary.
			{
				#label 			-> 		'All preferences...'.
				#object 			-> 		PreferenceNG.
				#selector 			-> 		#openPreferencesInspector.
				#icon 			-> 		#preferencesIcon.
				#balloonText 	-> 		'view and change various options.'
			} asDictionary.
		}! !


!PreferenceNG class reorganize!
('accessing' allPreferences at: at:put: categories instanceAt: selectCategory:)
('instance creation' name:category:value: name:description:category:type:value:)
('error handling' doesNotUnderstand:)
('class initialization' initialize installDefault:in: installHaloPreferencesWith: installMiscPreferences)
('fileIn/Out' loadFrom: save:to: saveAllTo: saveCategory:to:)
('helpers' detectType:)
('data' iconicHaloSpecifications machineDefault machineSlow machineSmalltalk80 prefFont prefGui prefProgramming prefSystem)
('event handlers' assignmentGlyph: defaultFontSize: machine: menuIcon: pointer:)
('fonts' fontSizes setDefaultFont: setDefaultFont:spec:)
('obsolete' desktopMenuTitle)
('as yet unclassified' openPreferencesInspector)
('toBeRemoved' isInitialFileListDirectory: soundsEnabled)
!

