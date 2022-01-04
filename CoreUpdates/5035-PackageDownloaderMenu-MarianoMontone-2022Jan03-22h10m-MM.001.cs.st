'From Cuis 6.0 [latest update: #5032] on 3 January 2022 at 9:19:11 pm'!

!TheWorldMenu methodsFor: 'menu' stamp: 'MM 1/3/2022 21:10:34'!
openPackageDownloader
	"In PackageDownloader.pck"
	Smalltalk at: #PackageDownloaderWindow ifAbsent: [ 
		(PopUpMenu 
		confirm: 'The Package Downloader is not loaded', String newLineString, 'Would you like me to load it for you now?')
		ifTrue: [Feature require: #'PackageDownloader'] 
	].
	Smalltalk at: #PackageDownloaderWindow ifPresent: [ :cls | ^cls open ].
	self inform: 'Please install optional package PackageDownloader.pck.st', String newLineString, 'Feature require: #''PackageDownloader'' '! !


!TheWorldMenu class methodsFor: 'menu building' stamp: 'MM 1/3/2022 21:17:30'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 		-> 		20.
			#itemOrder 		-> 		10.
			#label 			->	'Package Downloader'.
			#selector 		-> 		#openPackageDownloader.
			#icon 			-> 		#packageIcon.
			#balloonText 	-> 		'A tool for downloading Cuis packages from the internet.'.
		} asDictionary}`! !

