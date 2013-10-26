'From Cuis 3.3 of 2 June 2011 [latest update: #1024] on 27 March 2012 at 10:28:43 pm'!

!ChangeList class methodsFor: 'fileIn/Out' stamp: 'jmv 3/27/2012 22:25'!
                               browseMCZContents: aStream
	"Browse the selected file."
	| unzipped changeList fullName packageFile pkName |
	
	"For Monticello packages we do as we do with our own .pck files: Instead of just browsing
	contents, also include what is no longer part of the package (and should therefore be removed on install)
	See #browsePackageContents:
	However, this was never tested to run!!"
	self flag: #jmvVer.

	fullName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullName.
	unzipped _ aStream asUnZippedStream: 'snapshot/source.st'.
	unzipped ascii.
	Cursor read showWhile: [
		changeList _ self new scanFile: unzipped from: 0 to: unzipped size.
		aStream reset.
		packageFile _ CodePackageFile
			buildFileStream: unzipped
			packageName: pkName
			fullName: fullName ].
	"Add deletions of methods and classes that are in the PackageInfo (i.e., active in the system)
	but are no longer in the PackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChangeRecord new methodReference: methodReference)
			text: 'method no longer in package: ', methodReference asStringOrText ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChangeRecord new clsName: clsName)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aStream name! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 3/27/2012 22:19'!
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
	newCodePackage fullFileName: fullName.
	CodePackage register: newCodePackage.

	"Esto crea el change set y carga ahi. OJO. En ese CS, hay que borrar todo lo que hay que borrar"
	"These were created in #fromFileStream: ... reuse?"
	localName _ FileDirectory localNameFor: fullName.
	ChangeSet installing: newCodePackage packageName do: [
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ]].
	newCodePackage hasUnsavedChanges: false.
	Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
	
	"Tirar undeclared al transcript. warning si quedaron undeclared
	Es feo que tire an transcript undeclareds que despues no lo son..."
	Smalltalk cleanOutUndeclared.
	Undeclared print.

	"Descartar la instancia de CodePackageFile"
	^newCodePackage! !

