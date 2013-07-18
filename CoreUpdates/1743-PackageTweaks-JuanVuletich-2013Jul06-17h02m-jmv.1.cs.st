'From Cuis 4.1 of 12 December 2012 [latest update: #1741] on 6 July 2013 at 5:10:18.145382 pm'!

!ClassDeletionChangeRecord commentStamp: '<historical>' prior: 0!
Polymorphic with ChangeRecord. Used to represent classes that are not part of a CodePackageFile, but are part of the corresponding CodePackage: Classes that will be deleted on install.!


!MethodDeletionChangeRecord commentStamp: 'jmv 7/4/2013 22:29' prior: 0!
Polymorphic with ChangeRecord. Used to represent methods that are not part of a CodePackageFile, but are part of the corresponding CodePackage: Methods that will be deleted on install.!


!CodePackage commentStamp: '<historical>' prior: 0!
A CodePackage is a package that is currently loaded in the system. If saved (.pck.st), then it is stored in a file that can be dealt with as an instance of CodePackageFile. As the code is already in the system, all we need to know is the packageName. Implementation is originally based on PackageInfo, but has diverged.

CodePackage instances are usually created when installing CodePackageFiles. These instances track the code for that package, that we'll need to save if we don't want to lose changes. These instances are held in the InstalledPackages class variable.

We can also create 'transient' instances with whatever name (and classes and extension methods) we chose, like
	(CodePackage named: 'Collections' createIfAbsent: true registerIfNew: false) inspect; save
This won't mean the system is actually partitioned in such way.

(CodePackage named: 'TestPackage' createIfAbsent: true registerIfNew: false) inspect; save!


!ChangeList class methodsFor: 'public access' stamp: 'jmv 7/4/2013 22:27'!
browsePackageContents: aStream
	"Opens a changeList on a fileStream"
	| changeList fullName pkName packageFile |
	fullName _ aStream name.
	pkName _ CodePackageFile packageNameFrom: fullName.
	aStream readOnly.
	changeList _ self new scanFile: aStream from: 0 to: aStream size.
	aStream reset.
	packageFile _ CodePackageFile
		buildFileStream: aStream
		packageName: pkName
		fullName: fullName.
	aStream close.
	"Add deletions of methods and classes that are in the CodePackage (i.e., active in the system)
	but are no longer in the CodePackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChangeRecord new methodReference: methodReference)
			text: 'method no longer in package: ', methodReference asStringOrText ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChangeRecord new clsName: clsName)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aStream localName! !


!ChangeRecord methodsFor: 'access' stamp: 'jmv 7/6/2013 17:07'!
changeClass
	| methodClassName methodClass |
	(#(method classComment classDefinition) includes: type) ifFalse: [ ^ nil ].
	methodClassName _ class substrings
		ifEmpty: [ ^ nil ]
		ifNotEmpty: [ :parts |
			parts first asSymbol ].
	(Smalltalk includesKey: methodClassName) ifFalse: [ ^ nil ].
	methodClass _ Smalltalk at: methodClassName.
	^ meta
		ifTrue: [ methodClass class ]
		ifFalse: [ methodClass ]! !


!CodePackageFile methodsFor: 'initialize' stamp: 'jmv 7/4/2013 22:26'!
buildFileStream: aFileStream packageName: pkName fullName: fullFileName
	"Just build the CodePackageFile object. Don't install the code."

	| classesDefined classesExtended classesToDeleteButCant classesToReallyDelete packageInMemory |
	packageName _ pkName.
	fullName _ fullFileName.
	"Don't register a package!!"
	packageInMemory _ CodePackage
		named: packageName
		createIfAbsent: true
		registerIfNew: false.
	self buildFrom: aFileStream.
	"Compute stuff no longer in package: Should be removed from system."
	classesDefined _ Set new.
	classesExtended _ Set new.
	classes do: [ :pseudoClass |
		pseudoClass hasDefinition
			ifTrue: [ classesDefined add: pseudoClass name ]
			ifFalse: [ classesExtended add: pseudoClass name ]].
	classesToRemove _ packageInMemory classNames difference: classesDefined.
	"Add here:
		- classes in classesToDelete, that #allCallsOn answers selectors that aren't in classesToDelete or methodsToRemove
		- classes with #subclasses that aren't in classesToDelete.
		- classes with existing instances (#instanceCount)? Not really sure... Maybe sole instance referenced from classVar or such...
		- something else I forgot?
	Warning: This search for stuff that can't be removed must be iterated again until it doesn't find any more."
	classesToDeleteButCant _ classesToRemove intersection: classesExtended.
	classesToReallyDelete _ classesToRemove difference: classesToDeleteButCant.
	"Methods. Could also mean classes that can't be deleted!! (include in the iteration)
	Warn if deleting last implementor of sent messages?"
	methodsToRemove _ packageInMemory methods asSet difference: self allMethodReferences.
	methodsToRemove _ methodsToRemove reject: [ :methodReference | classesToReallyDelete includes: methodReference classSymbol ].

"
	'=============' print.
	('classesToRemove: ', classesToRemove printString) print.
	('classesToDeleteButCant: ', classesToDeleteButCant printString) print.
	('classesToReallyDelete: ', classesToReallyDelete printString) print.
	'=============' print.
	'methodsToRemove: ' print.
	methodsToRemove do: [ :methodReference | methodReference print ].
	'=============' print.
"! !

!CodePackageFile methodsFor: 'services' stamp: 'jmv 7/4/2013 22:28'!
install: aFileStream
	"Installs the package. Replace all existing code in the PackageInfo, removing any code that is not included in us."
	| localName newCodePackage |

	"Give reasonable warnings if there is stuff that can't be properly cleaned. Give the user the chance to abort."
	'=============' print.
	classesToRemove notEmpty ifTrue: [
		('classesToRemove: ', classesToRemove printString) print.
		'=============' print ].
	methodsToRemove notEmpty ifTrue: [
		'methodsToRemove: ' print.
		methodsToRemove do: [ :methodReference | methodReference print ].
		'=============' print ].
	
	"Tirar warning si hay que borrar cosas que no se puede, si hay referencias, etc. Quizas si vamos a borrar el ultimo implementor de un mensaje enviado?"

	"Create, install and answer an instance of CodePackage"
	newCodePackage _ CodePackage
		named: packageName
		createIfAbsent: true
		registerIfNew: true.
	newCodePackage
		fullFileName: fullName;
		sourceSystem: sourceSystem;
		description: description.

	"Esto crea el change set y carga ahi. OJO. En ese CS, hay que borrar todo lo que hay que borrar"
	"These were created in #fromFileStream: ... reuse?"
	localName _ FileDirectory localNameFor: fullName.
	ChangeSet installing: newCodePackage packageName do: [
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ]].
	newCodePackage hasUnsavedChanges: false.
	"If we are installing an already installed package, zap the change set with possible changes done, 
	as they are irrelevant now: we have the package from disk"
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: newCodePackage).
	Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
	
	"Tirar undeclared al transcript. warning si quedaron undeclared
	Es feo que tire an transcript undeclareds que despues no lo son..."
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ].

	"Descartar la instancia de CodePackageFile"
	^newCodePackage! !


!CodePackageFile class methodsFor: 'file list services' stamp: 'jmv 7/4/2013 22:29'!
serviceInstallPackage
	"Answer the service of installing a CodePackageFile (a chunk format file for a CodePackage)"

	^ (SimpleServiceEntry
		provider: self 
		label: 'install package'
		selector: #installPackageStream:
		description: 'install the package, replacing all existing code in the package (if any); create a new change set just for the installation'
		buttonLabel: 'install package')
		argumentGetter: [ :fileList | fileList readOnlyStream ]! !


!CodeWindow methodsFor: 'menu commands' stamp: 'jmv 7/6/2013 17:07'!
browseMethodFull
	"Create and schedule a full Browser and then select the current class and message."

	model selectedClassOrMetaClass ifNotNil: [ :myClass |
		BrowserWindow fullOnClass: myClass selector: model selectedMessageName]! !

