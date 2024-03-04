'From Cuis6.3 [latest update: #6252] on 4 March 2024 at 5:38:06 pm'!

!TheWorldMenu methodsFor: 'menu' stamp: 'jmv 3/4/2024 17:23:34'!
openPackageInstaller
	"In PackageInstaller.pck"
	Smalltalk at: #PackageInstallerWindow ifAbsent: [ 
		(PopUpMenu 
		confirm: 'The Package Installer is not loaded', String newLineString, 'Would you like me to load it for you now?')
		ifTrue: [Feature require: #PackageInstaller ] 
	].
	Smalltalk at: #PackageInstallerWindow ifPresent: [ :cls | ^cls open ].
	self inform: 'Please install optional package PackageInstaller.pck.st', String newLineString, 'Feature require: #''PackageInstaller'' '! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 3/4/2024 17:29:19'!
additionalMenuOptions

	^`{
		{
			#submenuOf  				-> 		TheWorldMenu openLabel.
			#itemGroup 				-> 		20.
			#itemOrder 				-> 		30.
			#label 				->		'Package Installer'.
			#selector 				-> 		#openPackageInstaller.
			#icon 				-> 		#packageIcon.
			#balloonText 				-> 		'A tool for installing Cuis packages from the local file system.'.
		} asDictionary.
		{
			#submenuOf  				-> 		TheWorldMenu openLabel.
			#itemGroup 				-> 		20.
			#itemOrder 				-> 		40.
			#label 				->		'Package Downloader'.
			#selector 				-> 		#openPackageDownloader.
			#icon 				-> 		#packageIcon.
			#balloonText 				-> 		'A tool for downloading Cuis packages from the internet.'.
		} asDictionary.
	}`	
	! !

!TheWorldMenu class methodsFor: 'menu building' stamp: 'jmv 3/4/2024 17:29:57'!
worldMenuOptions

	^`
		TheWorldMenu worldMainMenuOptions,
		"TheWorldMenu additionalMenuOptions,"
		TheWorldMenu preferencesMenuOptions,
		TheWorldMenu windowsMenuOptions,
		TheWorldMenu helpMenuOptions,
		TheWorldMenu changesMenuOptions,
		TheWorldMenu debugMenuOptions`.! !

!classRemoval: #PackageInstaller stamp: 'jmv 3/4/2024 17:35:54'!
Smalltalk removeClassNamed: #PackageInstaller!

!classRemoval: #PackageInstallerWindow stamp: 'jmv 3/4/2024 17:35:51'!
Smalltalk removeClassNamed: #PackageInstallerWindow!

