'From Cuis 5.0 [latest update: #4103] on 4 April 2020 at 1:04:53 pm'!
!classDefinition: #FeatureRequirement category: #'System-Package Support'!
Object subclass: #FeatureRequirement
	instanceVariableNames: 'name minVersion minRevision maxVersion pathName codePackageFile '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!

!FeatureRequirement methodsFor: 'accessing' stamp: 'jmv 4/4/2020 12:24:23'!
codePackageFile
	codePackageFile isNil ifTrue: [
		codePackageFile _ CodePackageFile onFileEntry: pathName asFileEntry.
		pathName _ nil ].
	^codePackageFile! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 4/4/2020 12:54:10'!
install
	"Create, install and answer a (sub)instance of CodePackage
	Replace all existing code in the possibly existing CodePackage, removing any code that is not included in us."
	| localName newCodePackage pckClass currentCS |

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

		fullName asFileEntry readStreamDo: [ :stream | stream fileInAnnouncing: 'Installing ', localName, '...' ].
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ].
		currentCS ifNotNil: [ ChangeSet removeChangeSet: currentCS ]].

	newCodePackage hasUnsavedChanges: false; triggerEvent: #dirtyFlagChanged.
	DataStream initialize. "Just in case"
	"If we are installing an already installed package, zap the change set with possible changes done, 
	as they are irrelevant now: we have the package from disk"
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: newCodePackage).
	Preferences transcriptLogVerbose ifTrue: [
		Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
		Smalltalk cleanOutUndeclared.
		Undeclared notEmpty ifTrue: [
			('Undeclared: ', Undeclared printString) print ]].
	^newCodePackage! !

!CodePackageFile methodsFor: 'accessing' stamp: 'jmv 4/4/2020 12:07:26'!
packageName
	^packageName! !


!CodePackageFile class methodsFor: 'instance creation' stamp: 'jmv 4/4/2020 11:55:38'!
onFileEntry: aFileEntry
	| fullFileName instance pkName |
	fullFileName _ aFileEntry pathName.
	pkName _ CodePackageFile packageNameFrom: fullFileName.
	aFileEntry readStreamDo: [ :stream |
		instance _ self new.
		instance buildFileStream: stream packageName: pkName fullName: fullFileName ].
	^instance! !


!ChangeList methodsFor: 'scanning' stamp: 'jmv 4/4/2020 11:38:19'!
scanFile: aFile from: startPosition to: stopPosition

	file _ aFile.
	changeList _ OrderedCollection new.
	list _ OrderedCollection new.
	listIndex _ 0.
	file position: startPosition.

	'Scanning ', aFile localName
		displayProgressAt: Sensor mousePoint
		from: startPosition to: stopPosition
		during: [ :barBlock | self scanUpTo: stopPosition informing: barBlock ].
		
	self clearSelections! !


!ChangeList class methodsFor: 'public access' stamp: 'jmv 4/4/2020 12:29:13'!
browsePackageContents: aFileEntry
	"Opens a changeList on a fileStream"
	| changeList packageFile |
	packageFile _ CodePackageFile onFileEntry: aFileEntry.
	aFileEntry readStreamDo: [ :stream |
		changeList _ self new scanFile: stream from: 0 to: stream size ].
	"Add deletions of methods and classes that are in the CodePackage (i.e., active in the system)
	but are no longer in the CodePackageFile being viewed."
	packageFile methodsToRemove do: [ :methodReference |
		changeList
			addItem: (MethodDeletionChangeRecord new methodReference: methodReference)
			text: 'method no longer in package: ', methodReference stringVersion ].
	packageFile classesToRemove do: [ :clsName |
		changeList
			addItem: (ClassDeletionChangeRecord new className: clsName)
			text: 'class no longer in package: ', clsName ].
	changeList clearSelections.
	ChangeListWindow open: changeList label: aFileEntry name! !


!FeatureRequirement methodsFor: 'accessing' stamp: 'jmv 4/4/2020 12:22:56'!
pathName: aPathNameString
	
	pathName _ aPathNameString.
	codePackageFile _ nil! !

!FeatureRequirement methodsFor: 'accessing' stamp: 'jmv 4/4/2020 11:58:01'!
requirements
	"Answer my requirements"

	^self codePackageFile requires! !

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 4/4/2020 12:25:19'!
findPackageFileAsReqOf: mainFeatureOrNil
	"Look in known places for packages providing required feature.
	Answer wether search was successful."
	| packageFileName entry |
	pathName ifNotNil: [
		pathName asFileEntry exists ifTrue: [ ^ true ]].
	packageFileName _ self packageFileName.
	(mainFeatureOrNil ifNil: [ self ]) placesToLookForPackagesDo: [ :directory |
		entry _ directory // packageFileName.
		entry exists ifTrue: [
			"Try this one. If success, keep it."
			self pathName: entry pathName.
			self checkRequirement ifTrue: [ ^true ].
			"Nope. Don't keep it."
			self pathName: nil ]].
	^ false! !

!FeatureRequirement methodsFor: 'requires' stamp: 'jmv 4/4/2020 12:24:31'!
install
	"Preconditions have been satisfied.  Install the required package."

	| existing |
	existing _ CodePackage named: self codePackageFile packageName createIfAbsent: false registerIfNew: false.
	(existing isNil
		or: [ existing hasUnsavedChanges not
			or: [ self confirm: 'If you install this package, there are unsaved changes that will be lost.', String newLineString, 'Continue?' ]]) ifTrue: [
		self codePackageFile install.
	]! !

!FeatureRequirement methodsFor: 'testing' stamp: 'jmv 4/4/2020 11:59:07'!
checkRequirement
	"Answer if I am satisfied by package found at pathName"

	| featureSpec |
	featureSpec _ self codePackageFile featureSpec.
	^ featureSpec notNil and: [featureSpec provides satisfies: self ]! !

!methodRemoval: CodePackageFile class #buildFileStream:packageName:fullName: stamp: 'jmv 4/4/2020 13:03:47'!
CodePackageFile class removeSelector: #buildFileStream:packageName:fullName:!
!methodRemoval: CodePackageFile class #installFileStream:packageName:fullName: stamp: 'jmv 4/4/2020 13:03:47'!
CodePackageFile class removeSelector: #installFileStream:packageName:fullName:!
!methodRemoval: CodePackageFile class #buildFileStream: stamp: 'jmv 4/4/2020 13:03:47'!
CodePackageFile class removeSelector: #buildFileStream:!
!methodRemoval: CodePackageFile class #basicInstallPackageStream: stamp: 'jmv 4/4/2020 13:03:47'!
CodePackageFile class removeSelector: #basicInstallPackageStream:!
!methodRemoval: CodePackageFile #install: stamp: 'jmv 4/4/2020 13:03:47'!
CodePackageFile removeSelector: #install:!
!methodRemoval: CodePackageFile #installFileStream:packageName:fullName: stamp: 'jmv 4/4/2020 13:03:47'!
CodePackageFile removeSelector: #installFileStream:packageName:fullName:!
!classDefinition: #FeatureRequirement category: #'System-Package Support'!
Object subclass: #FeatureRequirement
	instanceVariableNames: 'name minVersion minRevision maxVersion pathName codePackageFile'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-Package Support'!
