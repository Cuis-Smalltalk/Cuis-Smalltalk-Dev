'From Cuis6.3 [latest update: #6318] on 9 April 2024 at 3:08:58 pm'!

!CodeFile methodsFor: 'accessing' stamp: 'jmv 4/9/2024 14:46:23'!
description
	^String streamContents:[:s|
		s nextPutAll: 'CodeFile: '.
		s nextPutAll: fullName; newLine; newLine.
		sourceSystem isEmpty ifFalse:[
			s nextPutAll: sourceSystem; newLine; newLine ].
		doIts isEmpty ifFalse:[
			s nextPutAll: 'Unresolvable doIts:'; newLine; newLine.
			doIts do: [ :chgRec |
				s
					nextPut:$!!;
					nextPutAll: chgRec string;
					nextPut: $!!;
					newLine ]]]! !

!CodeFile methodsFor: 'accessing' stamp: 'jmv 4/9/2024 14:46:28'!
name

	^ fullName asFullFileEntry name! !

!CodeFile methodsFor: 'reading' stamp: 'jmv 4/9/2024 15:02:30'!
buildFrom: aStream

	| changes |
	changes := (ChangeList new scanFile: aStream from: 0 to: aStream size) changeList.
	'Processing ', self name
		displayProgressAt: Sensor mousePoint
		from: 1
		to: changes size
		during: [ :barBlock | self buildFrom: changes informingTo: barBlock ].! !


!CodePackageFile methodsFor: 'services' stamp: 'jmv 4/9/2024 15:05:45'!
install
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
	^newCodePackage! !

!CodePackageFile methodsFor: 'accessing' stamp: 'jmv 4/9/2024 14:46:38'!
description
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
	]! !

