'From Cuis 6.0 [latest update: #5488] on 19 September 2022 at 9:51:16 pm'!

!PackageInstallerWindow methodsFor: 'accessing' stamp: 'MM 9/19/2022 21:50:32'!
filteredPackageList

	| packageList |
	
	packageList _ model packageList.
	
	^ filterString isEmpty
		ifTrue: [packageList]
		ifFalse: [packageList select: [:package | package packageName includesSubstring: filterString asString caseSensitive: false]]! !


!PackageInstaller methodsFor: 'actions' stamp: 'MM 9/19/2022 16:16:07'!
collectPackagesFromDirectory: aDirectory

	"Create a collection of PackageSpec with package files found in aDirectory."
	
	|packageFiles|
	
	packageFiles _ aDirectory fileNamesMatching: '*.pck.st'.
		
	packageFiles do: [:packageFile | | codePackage |
		[codePackage _ CodePackageFile onFileEntry: (aDirectory // packageFile).
			self addPackage: codePackage] 
		on: Error
		do: [:e | Transcript nextPutAll: 'Warning: Error scanning the package ', packageFile, '. Ignoring ...'; newLine]]! !

