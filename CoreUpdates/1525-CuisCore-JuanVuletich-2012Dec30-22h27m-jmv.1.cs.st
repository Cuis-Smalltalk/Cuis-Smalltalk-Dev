'From Cuis 4.1 of 12 December 2012 [latest update: #1524] on 30 December 2012 at 10:47:36 pm'!
!classDefinition: #ChangeSet category: #'Tools-Changes'!
Object subclass: #ChangeSet
	instanceVariableNames: 'name preamble postscript changeRecords structures superclasses hasUnsavedChanges isForBaseSystem codePackage '
	classVariableNames: 'CurrentBaseCSNumber Installing '
	poolDictionaries: ''
	category: 'Tools-Changes'!

!ChangeSet methodsFor: 'accessing' stamp: 'jmv 12/30/2012 22:38'!
codePackage
	^codePackage! !

!ChangeSet methodsFor: 'accessing' stamp: 'jmv 12/30/2012 22:37'!
codePackage: aCodePackage
	codePackage _ codePackage.
	self isForBaseSystem: false! !


!ChangeSorter class methodsFor: 'enumerating' stamp: 'jmv 12/30/2012 22:37'!
existingOrNewChangeSetForPackage: aCodePackage
	| answer |
	answer _ self
		existingOrNewChangeSetNamed: 'UnsavedChangesTo-' , aCodePackage name
		forBaseSystem: false.
	answer codePackage: aCodePackage.
	^answer! !


!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 12/30/2012 22:35'!
changeSetForPackage: aCodePackage
	| csName |
	aCodePackage ifNil: [ ^ self changeSetForBaseSystem ].
	^self notInstallOrTestRun
		ifTrue: [
			ChangeSorter existingOrNewChangeSetForPackage: aCodePackage ]
		ifFalse: [
			csName _ Installing = ('Install-' , aCodePackage packageName)
				ifTrue: [ Installing ]
				ifFalse: [ 'Affects-' , aCodePackage name , '--' , Installing ].
			ChangeSorter
				existingOrNewChangeSetNamed: csName
				forBaseSystem: false ]! !


!ChangeSorter class methodsFor: 'removing' stamp: 'jmv 12/30/2012 22:47'!
zapAllChangeSets
	"Keep those change sets that are recording the unsaved changes of some package"
	self allChangeSets copy do: [ :changeSet |
		(changeSet codePackage notNil and: [changeSet isEmpty not]) ifFalse: [
			self removeChangeSet: changeSet ]]! !


!CodePackage methodsFor: 'saving' stamp: 'jmv 12/30/2012 22:42'!
save
	| nameToUse |
	fullFileName ifNil: [
		fullFileName _
			ChangeSet defaultChangeSetDirectory fullNameFor: (self packageName, FileDirectory dot, 'pck.st')].
	nameToUse _ fullFileName.
"	nameToUse _ Preferences changeSetVersionNumbers
		ifTrue: [
			ChangeSet defaultChangeSetDirectory
				nextNameFor: self packageName coda: '-', Utilities authorInitials
				extension: 'pck.st' ]
		ifFalse: [ (self packageName , FileDirectory dot , Utilities dateTimeSuffix , FileDirectory dot , 'pck.st') asFileName ]."
	Cursor write
		showWhile: [
			| file |
			file _ ChangeSet defaultChangeSetDirectory forceNewFileNamed: nameToUse.
			[
				file timeStamp.
				self writeOnStream: file ]
					ensure: [ file close ]].
	self hasUnsavedChanges: false.
	ChangeSorter removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self)! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 12/30/2012 22:41'!
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

	"Crear, instalar y devolver una instancia de PackageInfo"
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
	ChangeSorter removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: newCodePackage).
	Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
	
	"Tirar undeclared al transcript. warning si quedaron undeclared
	Es feo que tire an transcript undeclareds que despues no lo son..."
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ].

	"Descartar la instancia de CodePackageFile"
	^newCodePackage! !


!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 12/30/2012 22:41'!
browseChanges
	| current |
	current _ model selection.
	current ifNil: [ ^self ].

	ChangeSorterWindow
		open: (SingleSetChangeSorter new 
			myChangeSet: (ChangeSet existingOrNewChangeSetForPackage: current))
		label: nil! !

!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 12/30/2012 22:42'!
deletePackage

	| current cs |
	current _ model selection.
	current ifNil: [ ^self ].
	model selectionIndex: 0.	"no selection"
	cs _ ChangeSet existingOrNewChangeSetForPackage: current.
	cs isEmpty ifFalse: [
		cs name: cs hash asString, cs name.
		cs isForBaseSystem: true ].
	CodePackage deregister: current! !

!classDefinition: #ChangeSet category: #'Tools-Changes'!
Object subclass: #ChangeSet
	instanceVariableNames: 'name preamble postscript changeRecords structures superclasses hasUnsavedChanges isForBaseSystem codePackage'
	classVariableNames: 'CurrentBaseCSNumber Installing'
	poolDictionaries: ''
	category: 'Tools-Changes'!
