'From Cuis 4.0 of 21 April 2012 [latest update: #1280] on 12 May 2012 at 11:08:45 pm'!

!CodePackageFile methodsFor: 'services' stamp: 'jmv 5/12/2012 23:08'!
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
	newCodePackage_ CodePackage
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
	ChangeSorter removeChangeSet: (ChangeSet changeSetForPackage: newCodePackage).
	Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
	
	"Tirar undeclared al transcript. warning si quedaron undeclared
	Es feo que tire an transcript undeclareds que despues no lo son..."
	Smalltalk cleanOutUndeclared.
	Undeclared notEmpty ifTrue: [
		('Undeclared: ', Undeclared printString) print ].

	"Descartar la instancia de CodePackageFile"
	^newCodePackage! !


!CodePackageListWindow methodsFor: 'commands' stamp: 'jmv 5/12/2012 23:03'!
deletePackage

	| current cs |
	current _ model selection.
	current ifNil: [ ^self ].
	model selectionIndex: 0.	"no selection"
	cs _ ChangeSet changeSetForPackage: current.
	cs isEmpty ifFalse: [
		cs name: cs hash asString, cs name.
		cs isForBaseSystem: true ].
	CodePackage deregister: current! !

