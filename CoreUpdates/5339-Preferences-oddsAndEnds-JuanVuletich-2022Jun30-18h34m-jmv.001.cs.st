'From Cuis 6.0 [latest update: #5337] on 30 June 2022 at 6:50:24 pm'!

!SystemDictionary methodsFor: 'ui' stamp: 'jmv 6/30/2022 18:49:27'!
beep
	"
	Smalltalk beep
	"
	(Preferences at: #soundsEnabled) ifTrue: [
		Smalltalk
			at: #SampledSound
			ifPresent: [ :cls | cls beep ]
			ifAbsent: [ self primitiveBeep ]]! !


!PreferenceSet class methodsFor: 'sys preference' stamp: 'jmv 6/30/2022 18:39:49'!
init
" Not name #initialize to avoid auto start at class installation "
	#(gui font programming system) with: #(prefGui prefFont prefProgramming prefSystem) do: [ :category :selector |
		(self perform: selector) do: [ :aPrefArray |
			self installDefault: aPrefArray in: category ] ].
	self installHaloPreferencesWith: self iconicHaloSpecifications.
	self installMiscPreferences.
	self defaultFontSize: #standardFonts.
	self setDefaultFont: FontFamily defaultFamilyName.! !

!PreferenceSet class methodsFor: 'file list' stamp: 'jmv 6/30/2022 18:42:00'!
isInitialFileListDirectory: aDirectoryEntry
	"Answer true if aDirectoryEntry is an initial directory entry."

	| currentValue |
	(currentValue _ Preferences at: #initialFileListDirectories) isSymbol 		ifTrue: [ ^ false ].
	^ currentValue includes: aDirectoryEntry! !


!FileListWindow methodsFor: 'menu building' stamp: 'jmv 6/30/2022 18:42:28'!
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
		add: (PreferenceSet isInitialFileListDirectory: selectedWrapper item)
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
				target: Preferences
				action: #at:put:
				argumentList: {#initialFileListDirectories . entry first} ::
					setBalloonText: entry third ].
	aMenu add: 'default initial directories' subMenu: initialDirectoriesMenu.
	^ aMenu! !

!methodRemoval: Preference class #isInitialFileListDirectory: stamp: 'jmv 6/30/2022 18:43:20'!
Preference class removeSelector: #isInitialFileListDirectory:!
!methodRemoval: Preference class #soundsEnabled stamp: 'jmv 6/30/2022 18:50:05'!
Preference class removeSelector: #soundsEnabled!

!Preference class reorganize!
('accessing' allPreferences at: at:put: categories instanceAt: selectCategory:)
('instance creation' name:category:value: name:description:category:type:value:)
('error handling' doesNotUnderstand:)
('helpers' detectType:)
('event handlers' assignmentGlyph: defaultFontSize: machine: menuIcon: pointer:)
('fonts' fontSizes setDefaultFont: setDefaultFont:spec:)
('obsolete' desktopMenuTitle)
('as yet unclassified' openPreferencesInspector)
!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!!).
Be sure to put any further comments in double-quotes, like this one."
Preferences name: #soundsEnabled category: #system value: true
!

