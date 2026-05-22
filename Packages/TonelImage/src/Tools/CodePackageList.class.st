Class {
	#name : 'CodePackageList',
	#superclass : 'ActiveModel',
	#instVars : [
		'packages',
		'selection'
	],
	#category : 'Tools-Packages',
	#package : 'Tools',
	#tag : 'Packages'
}

{ #category : 'ui' }
CodePackageList class >> open [
	^ CodePackageListWindow open: self new
]

{ #category : 'accessing' }
CodePackageList >> description [

	selection ifNil: [ ^'' ].
	^selection description
]

{ #category : 'accessing' }
CodePackageList >> description: aText [

	selection ifNotNil: [
		selection description: aText string ].
	^true
]

{ #category : 'user interface support' }
CodePackageList >> editorClassFor: textGetter [
	^TextEditor
]

{ #category : 'initialization' }
CodePackageList >> initialize [
	self updatePackageList.
	CodePackage
		when: #installedPackagesChanged
		send: #updatePackageList
		to: self
]

{ #category : 'method nodes' }
CodePackageList >> methodNodeOf: aSourceCode ifErrorsParsing: aParsingErrorBlock [

	^aParsingErrorBlock value: nil
]

{ #category : 'accessing' }
CodePackageList >> packageFullNames [

	^ packages collect: [ :each | each fullFileName ifNil: '---Never saved yet' ]
]

{ #category : 'accessing' }
CodePackageList >> packageNames [
	^ packages collect: [ :each | (each hasUnsavedChanges ifTrue: ['*'] ifFalse: ['']), each packageNameAndVersion ]
]

{ #category : 'accessing' }
CodePackageList >> packages [
	^CodePackage installedPackages asArray sort: [ :a :b |
		 a packageName < b packageName ]
]

{ #category : 'commands' }
CodePackageList >> save [

	selection ifNotNil: [
		selection save ]
]

{ #category : 'accessing' }
CodePackageList >> selection [

	^selection
]

{ #category : 'accessing' }
CodePackageList >> selectionIndex [

	^ packages indexOf: selection
]

{ #category : 'accessing' }
CodePackageList >> selectionIndex: anInteger [

	selection := anInteger = 0 ifFalse: [ packages at: anInteger ].
	self
		changed: #packageNames;
		changed: #packageFullNames;
		changed: #description;
		changed: #summary;
		changed: #requirements
]

{ #category : 'accessing' }
CodePackageList >> summary [

	| count |
	selection ifNil: [ ^'' ].
	^ String streamContents: [ :strm |
		| methods  linesOfCode |
		strm
			nextPutAll: 'Package: ';
			nextPutAll: selection packageName;
			nextPutAll: ' -- ';
			nextPutAll: selection sourceSystem.
		strm nextPutAll: ' -- Number of system categories '.
		selection systemCategories size printOn: strm.
		strm nextPutAll: '.'.
		strm nextPutAll: ' -- Number of classes: '.
		count := 0.
		selection classesDo: [ :cls | count := count + 1 ].
		count printOn: strm.
		strm nextPutAll: '. Number of extension methods: '.
		selection extensionMethodCount printOn: strm.
		strm nextPutAll: '. Total number of methods: '.
		methods := selection methodCount.
		methods printOn: strm.
		strm nextPutAll: '. Total lines of code: '.
		linesOfCode := selection linesOfCode.
		linesOfCode printOn: strm.
		strm nextPutAll: ' ('.
		linesOfCode / (methods asFloat max: 1.0) printOn: strm fractionDigits: 2.
		strm nextPutAll: ' per method)'.
		strm nextPutAll: '. Total message sends: '.
		linesOfCode := selection messageSendsCount.
		linesOfCode printOn: strm ]
]

{ #category : 'events' }
CodePackageList >> updatePackageList [
	
	| newPackages oldPackages |
	oldPackages := (packages ifNil: [#()]) asIdentitySet.
	newPackages := CodePackage installedPackages asIdentitySet.
	oldPackages do: [ :old |
		(newPackages includes: old) ifFalse: [
			old removeActionsWithReceiver: self ]].
	newPackages do: [ :new |
		(oldPackages includes: new) ifFalse: [
			new when: #dirtyFlagChanged send:#updatePackageList to: self ]].
	newPackages := newPackages asArray sort: [ :a :b |
		 a packageName < b packageName ].
	packages := newPackages.
	self
		changed: #packageNames;
		changed: #packageFullNames;
		changed: #description;
		changed: #summary;
		changed: #requirements
]
