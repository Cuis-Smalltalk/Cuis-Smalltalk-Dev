'From Cuis 4.0 of 16 November 2011 [latest update: #1144] on 22 March 2012 at 11:41:42 am'!

!FileStream class methodsFor: 'file reader services' stamp: 'jmv 3/22/2012 11:07'!
                install: fullName
	"File in the entire contents of the file specified by the name provided.
	Do not affect the user change sets, store changes in separate one"

	| localName |
	localName _ FileDirectory localNameFor: fullName.
	ChangeSet installing: localName do: [ self fileIn: fullName ]! !

!FileStream class methodsFor: 'file reader services' stamp: 'jmv 3/22/2012 10:59'!
       serviceInstall
	"Answer a service for filing in an entire file"

	^ SimpleServiceEntry 
		provider: self 
		label: 'install code'
		selector: #install:
		description: 'install code (like fileIn), but store changes in a separate change set'
		buttonLabel: 'install'! !


!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 3/22/2012 10:53'!
         fileReaderServicesForFile: fullName suffix: suffix
	| services |
	services _ OrderedCollection new.
	(FileStream isSourceFileSuffix: suffix)
		ifTrue: [ services add: self serviceBrowseChangeFile ].
	suffix = 'pck'
		ifTrue: [ services add: self serviceBrowsePackageFile ].
	(suffix = 'changes') | (suffix = '*')
		ifTrue: [ services add: self serviceBrowseDotChangesFile ].
	(suffix = 'mcz' or: [ suffix = '*'])
		ifTrue: [ services add: self serviceBrowseMCZContents ].
	^services! !

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 3/22/2012 10:54'!
                services
	"Answer potential file services associated with this class"

	^ { self serviceBrowseChangeFile. 
		self serviceBrowseDotChangesFile.
		self serviceBrowseMCZContents }! !


!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/22/2012 11:10'!
                           changeSetForBaseSystem

	| csName |
	csName _ installing
		ifNil: [ 'ChangesTo-BaseSystem' ]
		ifNotNil: [ 'Modified-BaseSystem--Installing-', installing ].
	^ChangeSorter existingOrNewChangeSetNamed: csName! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/22/2012 11:10'!
                             changeSetForPackage: aCodePackage

	| csName |
	csName _ installing
		ifNil: [ 'ChangesTo-', aCodePackage name ]
		ifNotNil: [
			installing = aCodePackage packageName
				ifTrue: [ 'Installing-', installing ]
				ifFalse: [ 'Modified-', aCodePackage name, '--Installing-', installing ]].
	^ChangeSorter existingOrNewChangeSetNamed: csName! !

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 3/22/2012 11:05'!
                         installing: aCodePackageName do: aBlock

	installing _ aCodePackageName.
	aBlock ensure: [ installing _ nil ]! !


!CodeFileBrowser class methodsFor: 'instance creation' stamp: 'jmv 3/22/2012 10:53'!
                        fileReaderServicesForFile: fullName suffix: suffix

	(FileStream isSourceFileSuffix: suffix)
		ifTrue: [ ^ { self serviceBrowseCode } ].

	suffix = 'mcz'
		ifTrue: [ ^ { self serviceBrowseMCZCode. self serviceInstallMonticelloPackage } ].

	suffix = 'pck'
		ifTrue: [ ^ { self serviceBrowseCode. self serviceInstallPackage } ].
	^#()! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 3/22/2012 11:04'!
       install: aFileStream
	"Installs the package. Replace all existing code in the PackageInfo, removing any code that is not included in us."
	| localName newCodePackage |

	"Give reasonable warnings if there is stuff that can't be properly cleaned. Give the user the chance to abort."
	'=============' print.
	('classesToRemove: ', classesToRemove printString) print.
	'=============' print.
	'methodsToRemove: ' print.
	methodsToRemove do: [ :methodReference | methodReference print ].
	'=============' print.
	
	"Tirar warning si hay que borrar cosas que no se puede, si hay referencias, etc. Quizas si vamos a borrar el ultimo implementor de un mensaje enviado?"

	"Crear, instalar y devolver una instancia de PackageInfo"
	newCodePackage _ CodePackage newNamed: packageName.
	CodePackage register: newCodePackage.

	"Esto crea el change set y carga ahi. OJO. En ese CS, hay que borrar todo lo que hay que borrar"
	"These were created in #fromFileStream: ... reuse?"
	localName _ FileDirectory localNameFor: fullName.
	ChangeSet installing: newCodePackage packageName do: [
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ].
		Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine ].
	
	"Tirar undeclared al transcript. warning si quedaron undeclared
	Es feo que tire an transcript undeclareds que despues no lo son..."
	Smalltalk cleanOutUndeclared.
	Undeclared print.

	"Descartar la instancia de CodePackageFile"
	^newCodePackage! !


!FileStream class methodsFor: 'file reader services' stamp: 'jmv 3/22/2012 10:52'!
                           fileReaderServicesForFile: fullName suffix: suffix
	"Answer services for the given file"

	^ (self isSourceFileSuffix: suffix)
		ifTrue: [ {self serviceFileIn . self serviceInstall} ]
		ifFalse: [ #() ]! !


!GZipReadStream class methodsFor: 'fileIn/Out' stamp: 'jmv 3/22/2012 10:50'!
   fileReaderServicesForFile: fullName suffix: suffix 
	| services |
	(suffix = 'gz')
		ifFalse: [^ #()].
	services _ OrderedCollection new.
	services addAll: self services.
	^ services! !

!methodRemoval: GZipReadStream class #fileIn:!
GZipReadStream class removeSelector: #fileIn:!
!methodRemoval: GZipReadStream class #serviceFileIn!
GZipReadStream class removeSelector: #serviceFileIn!
!methodRemoval: CodeFileBrowser class #browseCompressedCode:!
CodeFileBrowser class removeSelector: #browseCompressedCode:!
!methodRemoval: CodeFileBrowser class #serviceBrowseCompressedCode!
CodeFileBrowser class removeSelector: #serviceBrowseCompressedCode!
!methodRemoval: ChangeList class #browseCompressedContents:!
ChangeList class removeSelector: #browseCompressedContents:!
!methodRemoval: ChangeList class #serviceBrowseCompressedChangeFile!
ChangeList class removeSelector: #serviceBrowseCompressedChangeFile!
