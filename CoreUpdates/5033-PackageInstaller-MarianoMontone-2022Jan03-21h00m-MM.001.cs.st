'From Cuis 6.0 [latest update: #5032] on 3 January 2022 at 8:33:41 pm'!
!classDefinition: #PackageInstallerWindow category: #'Tools-Packages'!
SystemWindow subclass: #PackageInstallerWindow
	instanceVariableNames: 'filterString filterInput currentIndex'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Packages'!

!PackageInstallerWindow commentStamp: '<historical>' prior: 0!
I'm a tool for listing locally available Cuis packages and installing them.!

!classDefinition: #PackageInstaller category: #'Tools-Packages'!
Object subclass: #PackageInstaller
	instanceVariableNames: 'packageList'
	classVariableNames: 'PackageList'
	poolDictionaries: ''
	category: 'Tools-Packages'!

!PackageInstaller commentStamp: '<historical>' prior: 0!
I'm the model of PackageInstallerWindow.!


!PackageInstallerWindow methodsFor: 'GUI building' stamp: 'MM 11/25/2021 15:21:52'!
buildActionsBar

	| actionsBar | 
	
	actionsBar _ LayoutMorph newRow.
	
	actionsBar addMorph:
		(PluggableButtonMorph 
				model: self 
				action: #installPackage
				label: 'Install package').
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

!PackageInstallerWindow methodsFor: 'GUI building' stamp: 'MM 11/25/2021 13:56:23'!
buildDetailPane
	^ TextModelMorph textProvider:  self textGetter: #packageDescription! !

!PackageInstallerWindow methodsFor: 'GUI building' stamp: 'MM 11/25/2021 15:23:37'!
buildMorphicWindow

	|layout|
		
	layout _ LayoutMorph newRow.
	layout addMorph: self buildPackageListMorph layoutSpec: (LayoutSpec proportionalWidth: 0.5).
	layout addAdjusterAndMorph: self buildDetailPane layoutSpec: (LayoutSpec proportionalWidth: 0.5).
	self addMorph: layout layoutSpec: (LayoutSpec proportionalWidth: 1).
	self addMorph: self buildActionsBar layoutSpec: (LayoutSpec new fixedHeight: 30; proportionalWidth: 1; yourself). 
	! !

!PackageInstallerWindow methodsFor: 'GUI building' stamp: 'MM 11/25/2021 15:29:21'!
buildPackageListMorph
	
	^ PluggableListMorph model: self listGetter: #packageListNames indexGetter: #currentIndex indexSetter: #currentIndex:.! !

!PackageInstallerWindow methodsFor: 'GUI building' stamp: 'MM 12/27/2021 20:33:57'!
windowColor
	^ Color fromHexString: '#88e6d8'! !

!PackageInstallerWindow methodsFor: 'initialization' stamp: 'MM 11/25/2021 15:18:18'!
initialize
	super initialize.
	filterString _ ''.! !

!PackageInstallerWindow methodsFor: 'actions' stamp: 'MM 11/25/2021 19:26:26'!
installPackage
	self selectedPackage ifNotNil: [:package | 
		(FeatureRequirement name: package packageName)
		pathName: package fullName;
		satisfyRequirementsAndInstall]! !

!PackageInstallerWindow methodsFor: 'actions' stamp: 'MM 11/25/2021 14:04:10'!
open
	self buildMorphicWindow.
	labelString _ 'Package Installer'.
	self openInWorld! !

!PackageInstallerWindow methodsFor: 'actions' stamp: 'MM 11/25/2021 15:30:07'!
updatePackageList

	model updatePackageList.
	self changed: #packageListNames! !

!PackageInstallerWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 15:17:00'!
currentIndex
	^ currentIndex ifNil: [0]! !

!PackageInstallerWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 15:17:08'!
currentIndex: index
	currentIndex _ index.
	self changed: #acceptedContents! !

!PackageInstallerWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 13:43:45'!
filterInput
	"Answer the value of filterInput"

	^ filterInput! !

!PackageInstallerWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 13:43:45'!
filterInput: anObject
	"Set the value of filterInput"

	filterInput _ anObject! !

!PackageInstallerWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 13:43:45'!
filterString
	"Answer the value of filterString"

	^ filterString! !

!PackageInstallerWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 16:53:16'!
filterString: aString
	"Set the value of filterString"

	filterString _ aString.
	filterInput hasUnacceptedEdits: false.
	self currentIndex: nil.
	self changed: #packageListNames! !

!PackageInstallerWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 16:18:21'!
filteredPackageList

	| packageList |
	
	packageList _ model packageList.
	
	^ filterString isEmpty
		ifTrue: [packageList]
		ifFalse: [packageList select: [:package | package packageName includesSubstring: filterString caseSensitive: false]]! !

!PackageInstallerWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 14:18:16'!
getFilter
	^ [:str :el | el printString includesSubstring: str caseSensitive: false]! !

!PackageInstallerWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 13:43:45'!
model
	"Answer the value of model"

	^ model! !

!PackageInstallerWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 13:43:45'!
model: anObject
	"Set the value of model"

	model _ anObject! !

!PackageInstallerWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 19:19:54'!
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
		
		s nextPutAll: package description]! !

!PackageInstallerWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 16:12:17'!
packageListNames

	^ self filteredPackageList collect: [:package | package packageName].
	
	! !

!PackageInstallerWindow methodsFor: 'accessing' stamp: 'MM 11/25/2021 16:10:59'!
selectedPackage
	currentIndex ifNil: [^nil].
	currentIndex isZero ifTrue: [^nil].
	^ self filteredPackageList at: currentIndex.! !


!PackageInstallerWindow class methodsFor: 'as yet unclassified' stamp: 'MM 11/25/2021 15:11:25'!
open

	^ self open: PackageInstaller new label: 'Package Installer'! !

!PackageInstallerWindow class methodsFor: 'as yet unclassified' stamp: 'MM 11/25/2021 17:15:50'!
worldMenuForOpenGroup
	^ `{{
			#itemGroup 		-> 		20.
			#itemOrder 		-> 		10.
			#label 			->	'Package Installer'.
			#object 			-> 	PackageInstallerWindow.
			#selector 		-> 		#open.
			#icon 			-> 		#packageIcon.
			#balloonText 	-> 		'A tool for installing local Cuis packages.'.
		} asDictionary}`! !


!PackageInstaller methodsFor: 'accessing' stamp: 'MM 11/25/2021 15:12:28'!
addPackage: aPackageSpec

	packageList add: aPackageSpec! !

!PackageInstaller methodsFor: 'accessing' stamp: 'MM 11/25/2021 14:24:04'!
packageList
	"Answer the value of packageList"

	^ packageList! !

!PackageInstaller methodsFor: 'actions' stamp: 'MM 11/25/2021 19:16:55'!
collectPackagesFromDirectory: aDirectory

	"Create a collection of PackageSpec with package files found in aDirectory."
	
	|packageFiles|
	
	packageFiles _ aDirectory fileNamesMatching: '*.pck.st'.
		
	packageFiles do: [:packageFile | | codePackage |
		codePackage _ CodePackageFile onFileEntry: (aDirectory // packageFile).
		self addPackage: codePackage]! !

!PackageInstaller methodsFor: 'actions' stamp: 'MM 11/25/2021 14:39:54'!
collectPackagesFromDisk

	"Collect PackageSpec with package files found in Cuis packages directories."
	
	self placesToLookForPackagesDo: [:aDirectory | 
		self collectPackagesFromDirectory: aDirectory]! !

!PackageInstaller methodsFor: 'actions' stamp: 'MM 11/25/2021 14:40:18'!
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

!PackageInstaller methodsFor: 'actions' stamp: 'MM 1/1/2022 18:39:02'!
updatePackageList
	
	(PopUpMenu confirm: 'I need to scan for packages. This takes some time. Continue?')
		ifFalse: [^ OrderedCollection new].
	packageList _ OrderedCollection new.
	self collectPackagesFromDisk.
	packageList sort: [:p1 :p2 | p1 packageName < p2 packageName].
	PackageList _ packageList.
	self changed: #packageList.
	^ packageList! !

!PackageInstaller methodsFor: 'initialization' stamp: 'MM 11/25/2021 16:15:28'!
initialize
	packageList _ PackageList ifNil: [self updatePackageList]! !


!CodePackageFile methodsFor: '*PackageInstaller' stamp: 'MM 11/19/2021 14:30:09'!
packageDescription
	^ description! !

