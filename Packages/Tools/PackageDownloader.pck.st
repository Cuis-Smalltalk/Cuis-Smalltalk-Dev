'From Cuis 6.0 [latest update: #5036] on 3 January 2022 at 10:38:50 pm'!
'Description Tool for downloading Cuis packages from the internet.'!
!provides: 'PackageDownloader' 1 39!
!requires: 'WebClient' 1 22 nil!
!requires: 'JSON' 1 19 nil!
SystemOrganization addCategory: 'PackageDownloader'!


!classDefinition: #PackageDownloaderWindow category: 'PackageDownloader'!
SystemWindow subclass: #PackageDownloaderWindow
	instanceVariableNames: 'filterString filterInput currentIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PackageDownloader'!
!classDefinition: 'PackageDownloaderWindow class' category: 'PackageDownloader'!
PackageDownloaderWindow class
	instanceVariableNames: ''!

!classDefinition: #PackageDownloader category: 'PackageDownloader'!
Object subclass: #PackageDownloader
	instanceVariableNames: 'packageList downloadDirectory'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PackageDownloader'!
!classDefinition: 'PackageDownloader class' category: 'PackageDownloader'!
PackageDownloader class
	instanceVariableNames: ''!

!classDefinition: #RemotePackageDescription category: 'PackageDownloader'!
Object subclass: #RemotePackageDescription
	instanceVariableNames: 'packageName description provides requires downloadUrl homepage tags'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'PackageDownloader'!
!classDefinition: 'RemotePackageDescription class' category: 'PackageDownloader'!
RemotePackageDescription class
	instanceVariableNames: ''!


!PackageDownloaderWindow commentStamp: '<historical>' prior: 0!
I'm a tool for downloading and installing Cuis packages from the internet.!

!PackageDownloader commentStamp: '<historical>' prior: 0!
I'm the model for PackageDownloaderWindow.!

!RemotePackageDescription commentStamp: '<historical>' prior: 0!
I'm a description of a remote package.!

!PackageDownloaderWindow methodsFor: 'initialization' stamp: 'MM 11/25/2021 21:23:05'!
initialize
	super initialize.
	filterString _ ''.! !

!PackageDownloaderWindow methodsFor: 'GUI building' stamp: 'MM 11/26/2021 14:03:24'!
buildActionsBar

	| actionsBar | 
	
	actionsBar _ LayoutMorph newRow.
	
	actionsBar addMorph:
		(PluggableButtonMorph 
				model: self 
				action: #downloadAndInstallPackage
				label: 'Download and install package').
	actionsBar addMorph:
		(PluggableButtonMorph
				model: self
				action: #updatePackageList
				label: 'Update package list').
	
	actionsBar addMorph: (LabelMorph contents: 'Search: ').
	
	filterInput _ TextModelMorph textProvider: self textGetter: #filterString textSetter: #filterString:.
	filterInput acceptOnCR: true;
		askBeforeDiscardingEdits: false.
	actionsBar addMorph: filterInput layoutSpec: (LayoutSpec fixedHeight: 30).
	
	^ actionsBar! !

!PackageDownloaderWindow methodsFor: 'GUI building' stamp: 'MM 11/25/2021 21:23:05'!
buildDetailPane
	^ TextModelMorph textProvider:  self textGetter: #packageDescription! !

!PackageDownloaderWindow methodsFor: 'GUI building' stamp: 'MM 11/25/2021 21:23:05'!
buildMorphicWindow

	|layout|
		
	layout _ LayoutMorph newRow.
	layout addMorph: self buildPackageListMorph layoutSpec: (LayoutSpec proportionalWidth: 0.5).
	layout addAdjusterAndMorph: self buildDetailPane layoutSpec: (LayoutSpec proportionalWidth: 0.5).
	self addMorph: layout layoutSpec: (LayoutSpec proportionalWidth: 1).
	self addMorph: self buildActionsBar layoutSpec: (LayoutSpec new fixedHeight: 30; proportionalWidth: 1; yourself). 
	! !

!PackageDownloaderWindow methodsFor: 'GUI building' stamp: 'MM 11/25/2021 21:23:05'!
buildPackageListMorph
	
	^ PluggableListMorph model: self listGetter: #packageListNames indexGetter: #currentIndex indexSetter: #currentIndex:.! !

!PackageDownloaderWindow methodsFor: 'GUI building' stamp: 'MM 12/27/2021 20:32:43'!
windowColor

	^ Color fromHexString: '#d0e973'! !

!PackageDownloaderWindow methodsFor: 'actions' stamp: 'MM 11/26/2021 14:02:26'!
downloadAndInstallPackage
	self selectedPackage ifNotNil: [:package | 
		model downloadAndInstall: package]! !

!PackageDownloaderWindow methodsFor: 'actions' stamp: 'MM 11/25/2021 21:23:05'!
open
	self buildMorphicWindow.
	labelString _ 'Package Installer'.
	self openInWorld! !

!PackageDownloaderWindow methodsFor: 'actions' stamp: 'MM 11/25/2021 21:23:05'!
updatePackageList

	model updatePackageList.
	self changed: #packageListNames! !

!PackageDownloaderWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:23:05'!
currentIndex
	^ currentIndex ifNil: [0]! !

!PackageDownloaderWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:23:05'!
currentIndex: index
	currentIndex _ index.
	self changed: #acceptedContents! !

!PackageDownloaderWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:23:05'!
filterInput
	"Answer the value of filterInput"

	^ filterInput! !

!PackageDownloaderWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:23:05'!
filterInput: anObject
	"Set the value of filterInput"

	filterInput _ anObject! !

!PackageDownloaderWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:23:05'!
filterString
	"Answer the value of filterString"

	^ filterString! !

!PackageDownloaderWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:23:05'!
filterString: aString
	"Set the value of filterString"

	filterString _ aString.
	filterInput hasUnacceptedEdits: false.
	self currentIndex: nil.
	self changed: #packageListNames! !

!PackageDownloaderWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:23:05'!
filteredPackageList

	| packageList |
	
	packageList _ model packageList.
	
	^ filterString isEmpty
		ifTrue: [packageList]
		ifFalse: [packageList select: [:package | package packageName includesSubstring: filterString caseSensitive: false]]! !

!PackageDownloaderWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:23:05'!
getFilter
	^ [:str :el | el printString includesSubstring: str caseSensitive: false]! !

!PackageDownloaderWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:23:05'!
model
	"Answer the value of model"

	^ model! !

!PackageDownloaderWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:23:05'!
model: anObject
	"Set the value of model"

	model _ anObject! !

!PackageDownloaderWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 22:29:27'!
packageDescription

	|package|
	
	package _ self selectedPackage.
	
	package ifNil: [^''].
	
	^ String streamContents: [:s | 
		s nextPutAll: package packageName;
			newLine.
		60 timesRepeat: [	s nextPut: $-].
		s newLine; newLine.
		
		package packageDescription ifNotEmpty: [:description |
			s nextPutAll: description;
				newLine;
				newLine].
		
		s nextPutAll: 'Download url: '; nextPutAll: package downloadUrl]! !

!PackageDownloaderWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:23:05'!
packageListNames

	^ self filteredPackageList collect: [:package | package packageName].
	
	! !

!PackageDownloaderWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:23:05'!
selectedPackage
	currentIndex ifNil: [^nil].
	currentIndex isZero ifTrue: [^nil].
	^ self filteredPackageList at: currentIndex.! !

!PackageDownloaderWindow class methodsFor: 'as yet unclassified' stamp: 'MM 11/25/2021 22:30:18'!
open

	^ self open: PackageDownloader new label: 'Package Downloader'! !

!PackageDownloader methodsFor: 'accessing' stamp: 'MM 12/27/2021 19:56:59'!
downloadDirectory

	^ downloadDirectory ifNil: [self class defaultDownloadDirectory ]! !

!PackageDownloader methodsFor: 'accessing' stamp: 'MM 1/3/2022 22:18:06'!
jsonPackagesFile

	^ (CodePackage named: 'PackageDownloader' createIfAbsent: false registerIfNew: false)
		packageDirectory / 'PackageDownloader' // 'packages.json'! !

!PackageDownloader methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:16:11'!
packageList
	"Answer the value of packageList"

	^ packageList! !

!PackageDownloader methodsFor: 'initialization' stamp: 'MM 11/25/2021 23:38:10'!
initialize
	
	self loadPackageList ! !

!PackageDownloader methodsFor: 'packages-collecting' stamp: 'MM 11/25/2021 22:09:55'!
collectPackagesFromDirectory: aDirectory

	"Create a collection of PackageSpec with package files found in aDirectory."
	
	| packageFiles |
	
	packageFiles _ aDirectory fileNamesMatching: '*.pck.st'.
		
	packageFiles do: [:packageFile | | codePackage |
		codePackage _ CodePackageFile onFileEntry: (aDirectory // packageFile).
		self addPackage: codePackage]! !

!PackageDownloader methodsFor: 'packages-collecting' stamp: 'MM 11/25/2021 21:16:11'!
collectPackagesFromDisk

	"Collect PackageSpec with package files found in Cuis packages directories."
	
	self placesToLookForPackagesDo: [:aDirectory | 
		self collectPackagesFromDirectory: aDirectory]! !

!PackageDownloader methodsFor: 'packages-collecting' stamp: 'MM 11/26/2021 11:21:39'!
collectPackagesFromJsonFile: aJsonFile

	aJsonFile readStreamDo: [:fs | |ps|
		ps _ Json readFrom: fs.
		
		ps do: [:p | |package|
			package _ RemotePackageDescription new.
			package packageName: (p at: 'name');
					description: (p at: 'description');
					downloadUrl: (p at: 'downloadUrl');
					homepage: (p at: 'homepage');
					requires: (p at: 'requires');
					provides: (p at: 'provides').
			self addPackage: package]]
					! !

!PackageDownloader methodsFor: 'packages-collecting' stamp: 'MM 11/25/2021 21:16:11'!
placesToLookForPackagesDo: aBlock

	| base packagesDirectory |

	"Look in Cuis image folder and reasonable subfolders"
	base _ DirectoryEntry smalltalkImageDirectory.
	aBlock value: base.
	packagesDirectory _ base / 'Packages'.
	aBlock value: packagesDirectory.
	packagesDirectory allRegularDirectoriesDo: aBlock.
	base regularDirectoriesDo: [ :child |
		child = packagesDirectory ifFalse: [
			aBlock value: child.
			child allRegularDirectoriesDo: aBlock]].
	
	"Look in parent directory and reasonable subfolders. 
	Useful when image is stored in a subdirectory of the main app directory.
	This could be the case when the package comes from a 'main' git repo, and image is copied from gitHub.
	First try directories including the word Cuis in the name. Then try others."
	base parent regularDirectoriesDo: [ :dir |
		dir ~= base ifTrue: [
			('*Cuis*' match: dir name)
				ifTrue: [aBlock value: dir. dir allRegularDirectoriesDo: aBlock]]].
	base parent regularDirectoriesDo: [ :dir |
		dir ~= base ifTrue: [
			('*Cuis*' match: dir name)
				ifFalse: [aBlock value: dir. dir allRegularDirectoriesDo: aBlock]]].

	"Also look in host OS current directory"
	(base ~= DirectoryEntry currentDirectory and: [base parent ~= DirectoryEntry currentDirectory])
		ifTrue: [
			base _ DirectoryEntry currentDirectory.
			base allRegularDirectoriesDo: aBlock ]! !

!PackageDownloader methodsFor: 'packages-update' stamp: 'MM 11/26/2021 11:57:37'!
isPackageLocallyAvailable: packageName

	^ (PackageInstaller new packageList 
		detect: [:package | package packageName = packageName]
		ifNone: [nil]) isNil not! !

!PackageDownloader methodsFor: 'packages-update' stamp: 'MM 1/3/2022 22:14:15'!
loadPackageList

	packageList _ OrderedCollection new.
	
	self jsonPackagesFile exists 
		ifFalse: [self updatePackageList] 
		ifTrue: [self collectPackagesFromJsonFile: self jsonPackagesFile].
	
	packageList sort: [:p1 :p2 | p1 packageName < p2 packageName].
	
	self changed: #packageList.! !

!PackageDownloader methodsFor: 'packages-update' stamp: 'MM 11/26/2021 19:24:56'!
packagesFileUrl

	"^ 'https://gist.githubusercontent.com/mmontone/f43e8c36fa8d3954163289b40670b1d9/raw/cuis-packages.json?a={1}'
		format: 1000 atRandom printString"
		
	^ 'https://bitbucket.org/mmontone/cuis-smalltalk-packageinstaller/downloads/packages.json'! !

!PackageDownloader methodsFor: 'packages-update' stamp: 'MM 11/25/2021 23:36:23'!
updatePackageList

	(self confirm: 'Download list of packages?')
		ifTrue: [self downloadPackagesFile]
		ifFalse: [^ self].
		
	self loadPackageList .	
	! !

!PackageDownloader methodsFor: 'packages' stamp: 'MM 11/25/2021 21:16:11'!
addPackage: aPackageSpec

	packageList add: aPackageSpec! !

!PackageDownloader methodsFor: 'packages' stamp: 'MM 11/26/2021 14:05:54'!
calculatePackageDependenciesOf: aPackageDescription

	| deps packages |
	
	deps _ Set new.
	packages _ OrderedCollection new
				addFirst: aPackageDescription;
				yourself.
				
	[packages isEmpty] whileFalse: [ |package|
		package _ packages removeFirst.
		package requires do: [:req | |pd|
			(req name = 'Cuis-Base') ifFalse: [
				deps add: req name.
				pd _ (self findPackageDescription: req name) ifNil: [
						PackageInstaller new packageList 
							detect: [:p | p packageName = req name]
							ifNone: [self error: 'Cannot satisfy requirement: ', req name]].
				packages addFirst: pd]]].
		
	^ deps! !

!PackageDownloader methodsFor: 'packages' stamp: 'MM 11/26/2021 11:23:15'!
findPackageDescription: packageName

	^ packageList 
		detect: [:packageDescription | packageDescription packageName = packageName]
		ifNone: [nil]! !

!PackageDownloader methodsFor: 'packages' stamp: 'MM 11/26/2021 11:57:37'!
packagesThatNeedDownload: packageNames
	"Return the list of packages that need to be downloaded (are not available locally)."
	
	^ packageNames select: [:packageName |
		(self isPackageLocallyAvailable: packageName) not]! !

!PackageDownloader methodsFor: 'packages' stamp: 'MM 11/25/2021 22:22:07'!
writePackagesListToJsonFile: aFileEntry

	aFileEntry writeStreamDo: [:file |
		self packageList jsonWriteOn: file].! !

!PackageDownloader methodsFor: 'packages-download' stamp: 'MM 11/26/2021 13:35:39'!
downloadAndInstall: aPackageDescription

	| dependencies needDownload filePath |
	
	(self isPackageLocallyAvailable: aPackageDescription packageName)
		ifTrue: [
			(self confirm: ('There''s a version of {1} available in your system. Download anyway?'
						format: {aPackageDescription packageName}))
				ifFalse: [^ nil]].
			
	(self confirm: ('Download and install {1}?' format: {aPackageDescription packageName}))
		ifFalse: [^ nil].
		
	dependencies _ self calculatePackageDependenciesOf: aPackageDescription.
	needDownload _ self packagesThatNeedDownload: dependencies.
	
	needDownload ifNotEmpty: [
		(self confirm: ('The following dependencies need to be downloaded: {1}. Download?'
					format: {', ' join: needDownload}))
			ifFalse: [^ nil].
			
		needDownload do: [:packageName | | depDesc |
			depDesc _ self findPackageDescription: packageName.
			depDesc ifNil: [self inform: 'Don''t know how to download ', packageName. ^ nil].
			self downloadPackage: depDesc packageName from: depDesc downloadUrl]].
	
	filePath _ self downloadPackage: aPackageDescription packageName from: aPackageDescription downloadUrl.
	
	(FeatureRequirement name: aPackageDescription packageName)
		pathName: filePath asString;
		satisfyRequirementsAndInstall
	
	
	
	! !

!PackageDownloader methodsFor: 'packages-download' stamp: 'MM 12/27/2021 19:56:39'!
downloadPackage: packageName from: aUrl

	"Download a Cuis package from aUrl."
	
	| filePath |
	
	filePath _ self downloadDirectory // (packageName, '.pck.st').
	
	self downloadUrl: aUrl into: filePath.
		
	^ filePath! !

!PackageDownloader methodsFor: 'packages-download' stamp: 'MM 11/25/2021 23:19:46'!
downloadPackagesFile

	Transcript show: 'Downloading ', self packagesFileUrl, ' ... '; newLine.
	self jsonPackagesFile fileContents: (WebClient httpGet: self packagesFileUrl) content
	! !

!PackageDownloader methodsFor: 'packages-download' stamp: 'MM 11/26/2021 13:33:29'!
downloadUrl: url into: aFileEntry
	Transcript show: 'Downloading ', url, ' ... '; newLine.
	aFileEntry fileContents: (WebClient httpGet: url) content! !

!PackageDownloader class methodsFor: 'as yet unclassified' stamp: 'MM 1/3/2022 22:37:33'!
defaultDownloadDirectory

	^ ((CodePackage named: 'PackageDownloader' createIfAbsent: false registerIfNew: false)
		packageDirectory / 'PackageDownloader' / 'download')
			assureExistence ;
			yourself! !

!RemotePackageDescription methodsFor: 'accessing' stamp: 'MM 11/25/2021 22:25:35'!
description
	^ description! !

!RemotePackageDescription methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:03:41'!
description: anObject
	"Set the value of description"

	description _ anObject! !

!RemotePackageDescription methodsFor: 'accessing' stamp: 'MM 11/21/2021 10:49:22'!
downloadUrl
	"Answer the value of downloadUrl"

	^ downloadUrl! !

!RemotePackageDescription methodsFor: 'accessing' stamp: 'MM 11/21/2021 10:49:22'!
downloadUrl: anObject
	"Set the value of downloadUrl"

	downloadUrl _ anObject! !

!RemotePackageDescription methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:03:41'!
homepage
	"Answer the value of homepage"

	^ homepage! !

!RemotePackageDescription methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:03:41'!
homepage: anObject
	"Set the value of homepage"

	homepage _ anObject! !

!RemotePackageDescription methodsFor: 'accessing' stamp: 'MM 11/25/2021 22:24:48'!
packageDescription
	^ description! !

!RemotePackageDescription methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:03:41'!
packageName
	"Answer the value of packageName"

	^ packageName! !

!RemotePackageDescription methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:03:41'!
packageName: anObject
	"Set the value of packageName"

	packageName _ anObject! !

!RemotePackageDescription methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:03:41'!
provides
	"Answer the value of provides"

	^ provides! !

!RemotePackageDescription methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:03:41'!
provides: anObject
	"Set the value of provides"

	provides _ anObject! !

!RemotePackageDescription methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:03:41'!
requires
	"Answer the value of requires"

	^ requires! !

!RemotePackageDescription methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:03:41'!
requires: anObject
	"Set the value of requires"

	requires _ anObject! !

!RemotePackageDescription methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:03:41'!
tags
	"Answer the value of tags"

	^ tags! !

!RemotePackageDescription methodsFor: 'accessing' stamp: 'MM 11/25/2021 21:03:41'!
tags: anObject
	"Set the value of tags"

	tags _ anObject! !

!RemotePackageDescription methodsFor: 'json' stamp: 'MM 11/26/2021 10:17:11'!
jsonWriteOn: aStream

	{
		'name' -> packageName.
		'downloadUrl' -> downloadUrl.
		'description' -> (String streamContents: [ :s |
			description ifNotEmpty: [s nextPutAll: description; newLine; newLine].
			s nextPutAll: 'Provides: '.
			provides printDetailsOn: s.
			s newLine.
			(self requires sorted: [:a :b | a name < b name]) do: [ :req |
				s nextPutAll: 'Requires: '.
				req printDetailsOn: s]]).
		'homepage' -> homepage.
		'tags' -> tags.
		'requires' -> (requires collect: [:req | 
					{'name' -> req name.
						'minVersion' -> req minVersion.
						'maxVersion' -> req maxVersion.
						'minRevision' -> req minRevision .
						} asDictionary]).
		'provides' -> {'name' -> provides name.
					'version' -> provides version.
					'revision' -> provides revision} asDictionary.
	} asDictionary jsonWriteOn: aStream! !
