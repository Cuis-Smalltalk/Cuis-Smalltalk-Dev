'From Cuis 5.0 of 7 November 2016 [latest update: #3347] on 15 June 2018 at 10:10:07 am'!

!PseudoClass methodsFor: 'fileIn/fileOut' stamp: 'jmv 6/15/2018 10:06:19'!
fileInDefinitionAndMetaclass
	self fileInDefinition.
	metaClass ifNotNil: [ metaClass fileInDefinition ]! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 6/15/2018 10:09:12'!
install: aFileStream
	"Installs the package. Replace all existing code in the CodePackage, removing any code that is not included in us."
	| localName newCodePackage pckClass |

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
	pckClass _ CodePackage.
	classes do: [ :ee |
		(ee hasDefinition and: [ee superclassName = 'CodePackage']) ifTrue: [
			ee fileInDefinitionAndMetaclass.
			pckClass _ Smalltalk at: ee name ]].
	newCodePackage _ pckClass
		named: packageName
		createIfAbsent: true
		registerIfNew: true.

	localName _ fullName asFileEntry name.
	ChangeSet installing: packageName do: [
		newCodePackage
			fullFileName: fullName;
			sourceSystem: sourceSystem;
			description: description;
			featureSpec: featureSpec.
		aFileStream fileInAnnouncing: 'Installing ', localName, '...'.
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ]].
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

