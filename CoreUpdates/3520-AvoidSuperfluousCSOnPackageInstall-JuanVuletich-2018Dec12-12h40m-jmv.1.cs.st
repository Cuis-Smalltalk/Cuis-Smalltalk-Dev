'From Cuis 5.0 of 7 November 2016 [latest update: #3511] on 12 December 2018 at 12:40:54 pm'!

!ChangeSet class methodsFor: 'change set to use' stamp: 'jmv 12/12/2018 12:22:30'!
notInstallOrTestRun

	^Installing isNil or: [
		(Installing beginsWith: 'RunningTest-') and: [ Processor activeProcess name ~= 'TestRunner' ]]
	
	"
	(Installing beginsWith: 'RunningTest-') and: [ Processor activeProcess name = 'TestRunner' ]		-> Test
	(Installing beginsWith: Install-')																					-> Install
	Installing isNil 																											-> Normal
	(Installing beginsWith: 'RunningTest-') and: [ Processor activeProcess name ~= 'TestRunner' ]	-> Normal
	"! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 12/12/2018 12:40:08'!
install: aFileStream
	"Installs the package. Replace all existing code in the CodePackage, removing any code that is not included in us."
	| localName newCodePackage pckClass currentCS |

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

	"Create, install and answer a (sub)instance of CodePackage"
	localName _ fullName asFileEntry name.
	ChangeSet installing: packageName do: [
		"This change set will capture a possible class definition for a subclass of CodePackage.
		If it only has that, then remove it after package install.
		One example needing this is 'Color-Extras.pck.st'"
		currentCS _ ChangeSet changeSetForBaseSystem.
		currentCS isEmpty ifFalse: [ currentCS _ nil ].
		pckClass _ CodePackage.
		classes do: [ :ee |
			(ee hasDefinition and: [ee superclassName = 'CodePackage']) ifTrue: [
				ee fileInDefinitionAndMetaclass.
				pckClass _ Smalltalk at: ee name ]].
		newCodePackage _ pckClass
			named: packageName
			createIfAbsent: true
			registerIfNew: true.
		newCodePackage
			fullFileName: fullName;
			sourceSystem: sourceSystem;
			description: description;
			featureSpec: featureSpec.
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ].
		currentCS ifNotNil: [ ChangeSet removeChangeSet: currentCS ]].
	newCodePackage hasUnsavedChanges: false; triggerEvent: #dirtyFlagChanged.
	DataStream initialize. "Just in case"
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

