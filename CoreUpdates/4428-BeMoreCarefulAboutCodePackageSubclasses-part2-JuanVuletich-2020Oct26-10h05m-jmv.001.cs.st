'From Cuis 5.0 [latest update: #4427] on 26 October 2020 at 10:05:48 am'!

!CodePackageFile methodsFor: 'services' stamp: 'jmv 10/26/2020 10:05:36'!
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
				((self packageName asIdentifier: true), 'Package') = ee name ifTrue: [
					ee fileInDefinitionAndMetaclass.
					pckClass _ Smalltalk at: ee name ]]].
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

