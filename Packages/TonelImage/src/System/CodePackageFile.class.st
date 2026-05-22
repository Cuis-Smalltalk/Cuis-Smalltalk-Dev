"
A CodePackageFile represents a file with code for a package, regardless of whether it is installed (as a CodePackage) or not. It supports Cuis' .pck.st.
"
Class {
	#name : 'CodePackageFile',
	#superclass : 'CodeFile',
	#instVars : [
		'packageName',
		'description',
		'classesToRemove',
		'methodsToRemove',
		'featureSpec'
	],
	#category : 'System-Package Support',
	#package : 'System',
	#tag : 'Package Support'
}

{ #category : 'file list services' }
CodePackageFile class >> fileReaderServicesForFileEntry: filename suffix: suffix [
	"FileList buttons that are serviced by us."

	(suffix = 'pck') | (suffix = 'pck.st')
		ifTrue: [ ^ { self serviceInstallPackage } ].
	^#()
]

{ #category : 'services' }
CodePackageFile class >> installPackage: aFileEntry [

	| fullName pkName req |
	fullName := aFileEntry pathName.
	pkName := CodePackageFile packageNameFrom: fullName.
	req := (FeatureRequirement name: pkName)
		pathName: fullName.
	"Install with all requirements"
	[ req satisfyRequirementsAndInstall ]
		on: FeatureRequirementUnsatisfied
		do: [ :ex | PopUpMenu inform: ex messageText ]
]

{ #category : 'instance creation' }
CodePackageFile class >> onFileEntry: aFileEntry [
	| fullFileName instance pkName |
	fullFileName := aFileEntry pathName.
	pkName := CodePackageFile packageNameFrom: fullFileName.
	aFileEntry readStreamDo: [ :stream |
		instance := self new.
		instance buildFileStream: stream packageName: pkName fullName: fullFileName ].
	^instance
]

{ #category : 'services' }
CodePackageFile class >> packageNameFrom: fullName [

	^fullName asFullFileEntry nameWithoutExtension
]

{ #category : 'file contents' }
CodePackageFile class >> providedFeatureInFile: aFileEntry [
	| item |
	aFileEntry readStreamDo: [ :stream |
		[
			stream skipSeparators.
			stream atEnd ] whileFalse: [
				item := stream nextChunk.
				(item beginsWith:  'provides:') ifTrue: [
					^Feature fromTokens: (Scanner new scanTokens: item) ]]].
	^ nil
]

{ #category : 'file list services' }
CodePackageFile class >> serviceInstallPackage [
	"Answer the service of installing a CodePackageFile (a chunk format file for a CodePackage)"

	^ (SimpleServiceEntry
		provider: self 
		label: 'install package'
		selector: #installPackage:
		description: 'install the package, replacing all existing code in the package (if any); create a new change set just for the installation'
		buttonLabel: 'install package'
		icon: #saveIcon)
			sortOrder: 100
]

{ #category : 'change list' }
CodePackageFile >> browseFileChanges [

	ChangeList browsePackageContents: (FileEntry withAbsolutePathName: fullName).
]

{ #category : 'initialization' }
CodePackageFile >> buildFileStream: aFileStream packageName: pkName fullName: fullFileName [
	"Just build the CodePackageFile object. Don't install the code."

	| classesDefined classesExtended classesToDeleteButCant classesToReallyDelete packageInMemory |
	packageName := pkName.
	fullName := fullFileName.
	"This will most likely be updated with the actual data from the file, calling #provides:"
	featureSpec provides: (Feature name: pkName version: 1 revision: 0).
	"Don't register a package!"
	packageInMemory := CodePackage
		named: packageName
		createIfAbsent: true
		registerIfNew: false.
	self buildFrom: aFileStream.
	"Compute stuff no longer in package: Should be removed from system."
	classesDefined := Set new.
	classesExtended := Set new.
	classes do: [ :pseudoClass |
		pseudoClass hasDefinition
			ifTrue: [ classesDefined add: pseudoClass name ]
			ifFalse: [ classesExtended add: pseudoClass name ]].
	classesToRemove := packageInMemory classNames difference: classesDefined.
	"Add here:
		- classes in classesToDelete, that #allCallsOn answers selectors that aren't in classesToDelete or methodsToRemove
		- classes with #subclasses that aren't in classesToDelete.
		- classes with existing instances (#instanceCount)? Not really sure... Maybe sole instance referenced from classVar or such...
		- something else I forgot?
	Warning: This search for stuff that can't be removed must be iterated again until it doesn't find any more."
	classesToDeleteButCant := classesToRemove intersection: classesExtended.
	classesToReallyDelete := classesToRemove difference: classesToDeleteButCant.
	"Methods. Could also mean classes that can't be deleted! (include in the iteration)
	Warn if deleting last implementor of sent messages?"
	methodsToRemove := packageInMemory methods asSet difference: self allMethodReferences.
	methodsToRemove := methodsToRemove reject: [ :methodReference | classesToReallyDelete includes: methodReference classSymbol ].

"
	'=============' print.
	('classesToRemove: ', classesToRemove printString) print.
	('classesToDeleteButCant: ', classesToDeleteButCant printString) print.
	('classesToReallyDelete: ', classesToReallyDelete printString) print.
	'=============' print.
	'methodsToRemove: ' print.
	methodsToRemove do: [ :methodReference | methodReference print ].
	'=============' print.
"
]

{ #category : 'accessing' }
CodePackageFile >> classesToRemove [
	^classesToRemove
]

{ #category : 'accessing' }
CodePackageFile >> description [
	^String streamContents: [ :s |
		s nextPutAll: 'Code Package File: '.
		s nextPutAll: fullName; newLine; newLine.
		s nextPutAll: 'Provides: '.
		self provides printDetailsOn: s.
		s newLine.
		self requires ifNotEmpty: [ :requirements | 
			s nextPutAll: 'Requires: '.
			(requirements sorted: [:a :b | a name < b name]) do: [ :req |
				s newLine; space.
				req printDetailsOn: s ].
			s newLine ].
		s newLine.
		sourceSystem isEmpty ifFalse:[
			s nextPutAll: sourceSystem; newLine; newLine ]
	]
]

{ #category : 'change record types' }
CodePackageFile >> doIt: chgRec [
	| string |
	string := chgRec string.
	('''Description *'
		match: string) ifTrue:[^self possibleDescription: chgRec].
	super doIt: chgRec
]

{ #category : 'accessing' }
CodePackageFile >> featureSpec [

	^ featureSpec
]

{ #category : 'initialization' }
CodePackageFile >> initialize [
	super initialize.
	description := ''.
	featureSpec := FeatureSpec new
]

{ #category : 'services' }
CodePackageFile >> install [
	"Create, install and answer a (sub)instance of CodePackage
	Replace all existing code in the possibly existing CodePackage, removing any code that is not included in us."
	| localName newCodePackage pckClass currentCS |

	localName := self name.
	ChangeSet installing: packageName do: [
		"This change set will capture a possible class definition for a subclass of CodePackage.
		If it only has that, then remove it after package install.
		One example needing this is 'Color-Extras.pck.st'"
		currentCS := ChangeSet changeSetForBaseSystem.
		currentCS isEmpty ifFalse: [ currentCS := nil ].
		pckClass := CodePackage.
		classes do: [ :ee |
			(ee hasDefinition and: [ee superclassName = 'CodePackage']) ifTrue: [
				((self packageName asIdentifier: true), 'Package') = ee name ifTrue: [
					ee fileInDefinitionAndMetaclass.
					pckClass := Smalltalk at: ee name ]]].
		newCodePackage := pckClass
			named: packageName
			createIfAbsent: true
			registerIfNew: true.
		newCodePackage
			fullFileName: fullName;
			sourceSystem: sourceSystem;
			description: description;
			featureSpec: featureSpec.

		fullName asFullFileEntry readStreamDo: [ :stream | stream fileInAnnouncing: 'Installing ', localName, '...' ].
		methodsToRemove do: [ :methodReference | methodReference actualClass removeSelector: methodReference selector ].
		classesToRemove do: [ :className | (Smalltalk at: className) removeFromSystem ].
		currentCS ifNotNil: [ ChangeSet removeChangeSet: currentCS ]].

	newCodePackage hasUnsavedChanges: false; triggerEvent: #dirtyFlagChanged.
	DataStream initialize. "Just in case"
	"If we are installing an already installed package, zap the change set with possible changes done, 
	as they are irrelevant now: we have the package from disk"
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: newCodePackage).
	(Preferences at: #transcriptLogVerbose) ifTrue: [
		Transcript newLine; show: 'Package ', packageName, ' successfully installed'; newLine.
		Smalltalk cleanOutUndeclared.
		Undeclared notEmpty ifTrue: [
			('Undeclared: ', Undeclared printString) print ]].
	^newCodePackage
]

{ #category : 'accessing' }
CodePackageFile >> methodsToRemove [
	^methodsToRemove
]

{ #category : 'accessing' }
CodePackageFile >> packageDescription [
	^ description
]

{ #category : 'accessing' }
CodePackageFile >> packageName [
	^packageName
]

{ #category : 'change record types' }
CodePackageFile >> possibleDescription: chgRec [
	| tokens prefix token |
	description isEmpty ifTrue:[
		tokens := Scanner new scanTokens: chgRec string.
		(tokens size = 1 and: [ (token := tokens first) isString ]) ifTrue: [
			prefix := 'Description '.
			(token beginsWith: prefix) ifTrue: [
				description := token copyFrom: prefix size + 1 to: token size.
				^self ]]].
	doIts add: chgRec.
]

{ #category : 'accessing' }
CodePackageFile >> provides [

	^featureSpec provides
]

{ #category : 'change record types' }
CodePackageFile >> provides: aFeatureChangeRecord [
	featureSpec provides: aFeatureChangeRecord feature
]

{ #category : 'accessing' }
CodePackageFile >> requires [

	^featureSpec requires
]

{ #category : 'change record types' }
CodePackageFile >> requires: aFeatureChangeRecord [
	featureSpec requires: aFeatureChangeRecord feature
]

{ #category : 'accessing' }
CodePackageFile >> summary [
	^featureSpec printString, String newLineString, super summary
]
