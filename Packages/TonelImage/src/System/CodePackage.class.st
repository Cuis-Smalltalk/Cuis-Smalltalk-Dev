"
A CodePackage is a package that is currently loaded in the system. If saved (.pck.st), then it is stored in a file that can be dealt with as an instance of CodePackageFile. As the code is already in the system, all we need to know is the packageName. Implementation is originally based on PackageInfo, but has diverged.

CodePackage instances are usually created when installing CodePackageFiles. These instances track the code for that package, that we'll need to save if we don't want to lose changes. These instances are held in the InstalledPackages class variable.

We can also create 'transient' instances with whatever name (and classes and extension methods) we chose, like
	(CodePackage named: 'Collections' createIfAbsent: true registerIfNew: false) inspect; save
This won't mean the system is actually partitioned in such way.

(CodePackage named: 'TestPackage' createIfAbsent: true registerIfNew: false) inspect; save
"
Class {
	#name : 'CodePackage',
	#superclass : 'Object',
	#instVars : [
		'packageName',
		'methodCategoryPrefix',
		'fullFileName',
		'hasUnsavedChanges',
		'description',
		'sourceSystem',
		'featureSpec'
	],
	#classVars : [
		'InstalledPackages'
	],
	#category : 'System-Package Support',
	#package : 'System',
	#tag : 'Package Support'
}

{ #category : 'startup' }
CodePackage class >> clearPaths [
	InstalledPackages ifNotNil: [
		InstalledPackages do: [ :a | a fullFileName: nil ]]
]

{ #category : 'packages access' }
CodePackage class >> deregister: aCodePackage [
	"
	CodePackage register: (CodePackage newNamed: 'Tests-Files')
	CodePackage deregister: (CodePackage newNamed: 'Tests-Files')
	"
	self installedPackages removeKey: aCodePackage packageName.
	self triggerEvent: #installedPackagesChanged
]

{ #category : 'packages access' }
CodePackage class >> installedPackages [

	InstalledPackages ifNil: [
		InstalledPackages := Dictionary new ].
	^InstalledPackages
]

{ #category : 'packages access' }
CodePackage class >> named: aString createIfAbsent: neverAnswerNil registerIfNew: doRegisterIfJustCreated [
	"Answer the instance with name aString.
	If there's no package named aString, answer nil or a new instance as requested.
	If a new instance is created, register it or not as requested.
	CodePackage named: 'Morphic' createIfAbsent: false registerIfNew: false
	CodePackage named: 'Morphic' createIfAbsent: true registerIfNew: false
	CodePackage named: 'XXXXXTest' createIfAbsent: true registerIfNew: true
	"

	| newInstance |
	^self installedPackages
		at: aString
		ifAbsent: [
			neverAnswerNil ifTrue: [ 
				newInstance := self new packageName: aString.
				doRegisterIfJustCreated ifTrue: [ self register: newInstance ].
				newInstance ]].
]

{ #category : 'searching' }
CodePackage class >> packageOfClass: aClass ifNone: errorBlock [

	InstalledPackages ifNil: [ ^errorBlock value ].
	^ InstalledPackages
		detect: [:ea | ea includesClass: aClass]
		ifNone: errorBlock
]

{ #category : 'searching' }
CodePackage class >> packageOfMethod: aMethodReference ifNone: errorBlock [

	InstalledPackages ifNil: [ ^errorBlock value ].
	^ InstalledPackages
		detect: [:ea | ea includesMethodReference: aMethodReference]
		ifNone: errorBlock
]

{ #category : 'searching' }
CodePackage class >> packageOfMethodCategory: categoryName ofClass: aClass ifNone: errorBlock [

	InstalledPackages ifNil: [ ^errorBlock value ].
	^ InstalledPackages
		detect: [:ea | ea includesMethodCategory: categoryName ofClass: aClass]
		ifNone: errorBlock
]

{ #category : 'searching' }
CodePackage class >> packageOfSystemCategory: aSystemCategory ifNone: errorBlock [

	InstalledPackages ifNil: [ ^errorBlock value ].
	^ InstalledPackages
		detect: [:ea | ea includesSystemCategory: aSystemCategory]
		ifNone: errorBlock
]

{ #category : 'installing' }
CodePackage class >> postPackageInstall [
	"This gets called after installing all the package code.
	Redefine as appropriate"
]

{ #category : 'installing' }
CodePackage class >> prePackageInstall [
	"This gets called after creating the package class and installing its code, but before installing the rest of the package code
	Redefine as appropriate"
]

{ #category : 'packages access' }
CodePackage class >> register: aCodePackage [
	"
	Usually call #named:createIfAbsent:registerIfNew: instead
	CodePackage register: (CodePackage newNamed: 'Tests-Files')
	"
	| any base |
	self installedPackages at: aCodePackage packageName put: aCodePackage.

	"Anything that now belongs in this package, was moved out of the base system!"
	base := ChangeSet changeSetForBaseSystem.
	any := false.
	aCodePackage classesDo: [ :cls |
		any := true.
		base noteClassMoveToOtherPackage: cls ].
	aCodePackage extensionMethodsDo: [ :methodReference |
		methodReference isValid ifTrue: [
			any := true.
			base
				noteMethodMoveToOtherPackage: methodReference selector
				forClass: methodReference actualClass ]].
	any ifTrue: [
		aCodePackage hasUnsavedChanges: true ].

	self triggerEvent: #installedPackagesChanged
]

{ #category : 'cached state access' }
CodePackage class >> releaseClassCachedState [

	InstalledPackages ifNotNil: [
		InstalledPackages isEmpty ifTrue: [
			InstalledPackages := nil ]]
]

{ #category : 'comparing' }
CodePackage >> = other [

	self == other ifTrue: [ ^ true ].
	^ self species == other species and: [ packageName = other packageName ]
]

{ #category : 'testing' }
CodePackage >> category: categoryName matches: prefix [
	| prefixSize catSize |
	categoryName ifNil: [ ^false ].
	catSize := categoryName size.
	prefixSize := prefix size.
	catSize < prefixSize ifTrue: [ ^false ].
	(categoryName findString: prefix startingAt: 1 caseSensitive: false) = 1
		ifFalse: [ ^false ].
	^(categoryName at: prefix size + 1 ifAbsent: [ ^true ]) == $-
]

{ #category : 'accessing' }
CodePackage >> classNames [
	| classNames |
	classNames := Set new.
	self classesDo: [ :cls | classNames add: cls name ].
	^classNames
]

{ #category : 'listing' }
CodePackage >> classesDo: aBlock [
	self systemCategories do: [ :cat |
		(SystemOrganization listAtCategoryNamed: cat) do: [ :className |
			aBlock value: (Smalltalk at: className) ]]
]

{ #category : 'listing' }
CodePackage >> classesInSafeOrderDo: aBlock [

	| myClasses myPoolDicts |
	myPoolDicts := Set new.
	myClasses := Set new.
	self classesDo: [ :cls |
		(cls inheritsFrom: SharedPool)
			ifTrue: [ myPoolDicts add: cls ]
			ifFalse: [ myClasses add: cls ]].
	Smalltalk hierarchySorted: myPoolDicts do: aBlock.
	Smalltalk hierarchySorted: myClasses do: aBlock
]

{ #category : 'accessing' }
CodePackage >> codePackageClass [
	"Answer the specific CodePackage subclass to use."

	self class == CodePackage ifFalse: [
		^ self class ].
	self classesDo: [ :cls |
		(cls inheritsFrom: CodePackage)
			ifTrue: [
				((self packageName asIdentifier: true), 'Package') = cls name ifTrue: [
					^ cls ]]].
	^ nil
]

{ #category : 'listing' }
CodePackage >> coreMethods [
	^Array streamContents: [ :strm |
		self coreMethodsDo: [ :methodRef |
			strm nextPut: methodRef ]]
]

{ #category : 'listing' }
CodePackage >> coreMethodsDo: aBlock [
	self classesDo: [ :cls |
		self coreMethodsOf: cls do: aBlock.
		self coreMethodsOf: cls class do: aBlock ].
]

{ #category : 'enumerating' }
CodePackage >> coreMethodsOf: aClass do: aBlock [
	"Evaluate aBlock with the actual method objects in this package."

	aClass organization categories do: [ :cat |
		(self isForeignClassExtension: cat) ifFalse: [
			(aClass organization listAtCategoryNamed: cat) do: [ :s |
				aBlock value: (self referenceForMethod: s ofClass: aClass) ]]].
]

{ #category : 'accessing' }
CodePackage >> description [

	^description
]

{ #category : 'accessing' }
CodePackage >> description: aString [

	description = aString
		ifFalse: [
			description := aString.
			self hasUnsavedChanges: true ]
]

{ #category : 'testing' }
CodePackage >> extensionCategoriesForClass: aClassOrMetaClass [
	"Pass the class as the argument for instance side.
	Pass the metaclass as the argument for class side."
	^ aClassOrMetaClass organization categories select: [:cat |
		self isYourClassExtension: cat]
]

{ #category : 'listing' }
CodePackage >> extensionClassNamesIn: aCategoryNameOrPrefix [

	^ (SystemOrganization fullListAtCategoryNamed: aCategoryNameOrPrefix) select: [ :className | | cls |
		cls := Smalltalk at: className.
		(self hasAnyExtensionCategoriesForClass: cls) or: [
			self hasAnyExtensionCategoriesForClass: cls theMetaClass ]]
]

{ #category : 'source code management' }
CodePackage >> extensionMethodCount [

	| sum |
	sum := 0.
	self extensionMethodsDo: [ :each |
		sum := sum + 1 ].
	^sum
]

{ #category : 'listing' }
CodePackage >> extensionMethods [
	"Include both class and instance methods we define, for classes we don't define."
	^Array streamContents: [ :stream |
		self extensionMethodsDo: [ :methodRef |
			stream nextPut: methodRef ]]
]

{ #category : 'listing' }
CodePackage >> extensionMethodsDo: aBlock [
	"Include both class and instance methods we define, for classes we don't define."

	self externalClasses do: [ :classOrMetaClass |
		(self extensionCategoriesForClass: classOrMetaClass) do: [ :cat |
			self methodsInCategory: cat ofClass: classOrMetaClass do: [ :m |
				aBlock value: m ]]]
]

{ #category : 'dependencies' }
CodePackage >> externalClasses [
	"Answer Classes and Metaclasses not defined in self."
	| myClasses |
	myClasses := Set new.
	self classesDo: [ :cls | myClasses add: cls ].
	^ Array streamContents: [ :s |
		ProtoObject
			allSubclassesWithLevelDo: [ :cls :l |
				(myClasses includes: cls) ifFalse: [
					s nextPut: cls; nextPut: cls class ]]
			startingLevel: 1
			sortByCategory: true ]
]

{ #category : 'accessing' }
CodePackage >> featureSpec [

	^ featureSpec
]

{ #category : 'accessing' }
CodePackage >> featureSpec: aFeatureSpec [

	featureSpec := aFeatureSpec
]

{ #category : 'testing' }
CodePackage >> foreignExtensionCategoriesForClass: aClass [
	^ aClass organization categories select: [:cat | self isForeignClassExtension: cat]
]

{ #category : 'testing' }
CodePackage >> foreignExtensionMethodsForClass: aClass [
	^Array streamContents: [ :stream |
		(self foreignExtensionCategoriesForClass: aClass) do: [ :cat |
			(aClass organization listAtCategoryNamed: cat) do: [ :sel |
				stream nextPut: (self referenceForMethod: sel ofClass: aClass) ]]]
]

{ #category : 'accessing' }
CodePackage >> fullFileName [

	^fullFileName
]

{ #category : 'accessing' }
CodePackage >> fullFileName: aString [

	fullFileName := aString
]

{ #category : 'testing' }
CodePackage >> hasAnyExtensionCategoriesForClass: aClassOrMetaClass [
	"Pass the class as the argument for instance side.
	Pass the metaclass as the argument for class side."
	^ aClassOrMetaClass organization hasAnyCategoriesSuchThat: [ :cat |
		self isYourClassExtension: cat ]
]

{ #category : 'accessing' }
CodePackage >> hasUnsavedChanges [

	"Might be nil and breaks when a code package window is open and loading packages - Hernan
	This is not a lazy initialization, the variable is set to non nil value only when certain."
	^hasUnsavedChanges = true
]

{ #category : 'accessing' }
CodePackage >> hasUnsavedChanges: aBoolean [

	"Not during package install"
	(ChangeSet isInstalling: self) ifFalse: [
		hasUnsavedChanges = aBoolean ifFalse: [
			hasUnsavedChanges := aBoolean.
			hasUnsavedChanges ifTrue: [
				featureSpec provides newRevision ].
			self triggerEvent: #dirtyFlagChanged ]]
]

{ #category : 'comparing' }
CodePackage >> hash [
	^ packageName hash
]

{ #category : 'testing' }
CodePackage >> includesAnyCode [
	self classesDo: [ :cls |
		^true ].
	self methodsDo: [ :cls |
		^true ].
	^false
]

{ #category : 'testing' }
CodePackage >> includesClass: aClass [
	^ self includesSystemCategory: aClass category
]

{ #category : 'testing' }
CodePackage >> includesMethod: aSymbol ofClass: aClass [
	aClass ifNil: [^ false].
	^ self
		includesMethodCategory: ((aClass organization categoryOfElement: aSymbol)
										ifNil: [' '])
		ofClass: aClass
]

{ #category : 'testing' }
CodePackage >> includesMethodCategory: categoryName ofClass: aClass [
	^ (self isYourClassExtension: categoryName)
		or: [(self includesClass: aClass)
				and: [(self isForeignClassExtension: categoryName) not]]
]

{ #category : 'testing' }
CodePackage >> includesMethodReference: aMethodRef [
	^ self includesMethod: aMethodRef methodSymbol ofClass: aMethodRef actualClass
]

{ #category : 'testing' }
CodePackage >> includesSystemCategory: categoryName [
	^ self category: categoryName matches: self systemCategoryPrefix
]

{ #category : 'testing' }
CodePackage >> isForeignClassExtension: categoryName [
	"Answer true if <categoryName> is a foreign class extension reference"
	
	categoryName ifNil: [ ^false ].
	(categoryName size = 0) ifTrue:[^false].
	^ categoryName first == $* and: [(self isYourClassExtension: categoryName) not]
]

{ #category : 'testing' }
CodePackage >> isYourClassExtension: categoryName [
	^ categoryName notNil and: [self category: categoryName matches: self methodCategoryPrefix]
]

{ #category : 'source code management' }
CodePackage >> linesOfCode [
	"An approximate measure of lines of code.
	Does not includes comments, or excludes blank lines.
	See comment at CompiledMethod >> #linesOfCode"

	| sum |
	sum := 0.
	self methodsDo: [ :each |
		sum := sum + each compiledMethod linesOfCode ].
	^sum
]

{ #category : 'source code management' }
CodePackage >> messageSendsCount [
	"See comment at CompiledMethod >> #messageSendsCount"

	| sum |
	sum := 0.
	self methodsDo: [ :each |
		sum := sum + each compiledMethod messageSendsCount ].
	^sum
]

{ #category : 'naming' }
CodePackage >> methodCategoryPrefix [
	^ methodCategoryPrefix ifNil: [
		methodCategoryPrefix := '*', packageName asLowercase ]
]

{ #category : 'source code management' }
CodePackage >> methodCount [

	| sum |
	sum := 0.
	self methodsDo: [ :each |
		sum := sum + 1 ].
	^sum
]

{ #category : 'listing' }
CodePackage >> methods [

	^Array streamContents: [ :strm |
		self extensionMethodsDo: [ :methodRef |
			methodRef isValid ifTrue: [ strm nextPut: methodRef ]].
		self coreMethodsDo: [ :methodRef |
			methodRef isValid ifTrue: [ strm nextPut: methodRef ]]]
]

{ #category : 'listing' }
CodePackage >> methodsDo: aBlock [
	self coreMethodsDo: [ :methodRef |
		methodRef isValid ifTrue: [ aBlock value: methodRef ]].
	self extensionMethodsDo: [ :methodRef |
		methodRef isValid ifTrue: [ aBlock value: methodRef ]].
]

{ #category : 'enumerating' }
CodePackage >> methodsInCategory: aString ofClass: aClass do: aBlock [
	((aClass organization listAtCategoryNamed: aString) ifNil: [^self])
		do: [:sel | aBlock value: (self referenceForMethod: sel ofClass: aClass)]
]

{ #category : 'naming' }
CodePackage >> packageDirectory [

	^self packageDirectoryName asFullDirectoryEntry
]

{ #category : 'naming' }
CodePackage >> packageDirectoryName [

  ^self fullFileName withoutSuffix: self packageFileName
]

{ #category : 'naming' }
CodePackage >> packageFileName [
	^ packageName, '.pck.st'
]

{ #category : 'accessing' }
CodePackage >> packageName [
	^ packageName
]

{ #category : 'naming' }
CodePackage >> packageName: aString [
	packageName := aString.
	description := ''.
	featureSpec := FeatureSpec new.
	featureSpec provides: (Feature name: packageName version: 1 revision: 0).
	hasUnsavedChanges := self includesAnyCode.
	"But reset revision if it was incremented because of marking it dirty!"
	featureSpec provides name: packageName version: 1 revision: 0
]

{ #category : 'accessing' }
CodePackage >> packageNameAndVersion [
	^String streamContents: [ :strm |
		self printNameAndVersionOn: strm ]
]

{ #category : 'printing' }
CodePackage >> printNameAndVersionOn: aStream [
	aStream
		nextPutAll: packageName;
		nextPut: $ .
	featureSpec ifNotNil: [
		featureSpec provides printVersionRevisionOn: aStream ]
]

{ #category : 'printing' }
CodePackage >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	self printNameAndVersionOn: aStream.
	aStream nextPut: $)
]

{ #category : 'accessing' }
CodePackage >> provides [

	^featureSpec provides
]

{ #category : 'testing' }
CodePackage >> referenceForMethod: aSymbol ofClass: aClass [
	^ MethodReference class: aClass selector: aSymbol
]

{ #category : 'modifying' }
CodePackage >> removeMethod: aMethodReference [

]

{ #category : 'accessing' }
CodePackage >> requirementOfMe [

	^ self provides requirementOfMe
]

{ #category : 'accessing' }
CodePackage >> requires [

	^featureSpec requires
]

{ #category : 'saving' }
CodePackage >> save [
	"If we can't save, find a new destination directory."
	fullFileName ifNotNil: [
		fullFileName asFullFileEntry parent exists ifFalse: [
			fullFileName := nil ]].

	"If we were never saved, or never saved since image was moved, or target directory disappeared,
	then ask where to save, suggesting the NewPackages folder."
	fullFileName ifNil: [
		fullFileName :=
			self request: 'Save on path: '
				initialAnswer: (DirectoryEntry newPackagesDirectory // self packageFileName) pathName
				orCancel: [ ^self ]].

	fullFileName asFullFileEntry forceWriteStreamDo: [ :stream |
		stream writeSystemTimeStamp.
		self writeOnStream: stream ].

	self hasUnsavedChanges: false.
	ChangeSet removeChangeSet: (ChangeSet existingOrNewChangeSetForPackage: self)
]

{ #category : 'listing' }
CodePackage >> selectors [

	^ Array streamContents: [ :strm |
		self methodsDo: [ :ea | strm nextPut: ea methodSymbol ]]
]

{ #category : 'listing' }
CodePackage >> sortedExtensionMethodsDo: aBlock displayingProgress: aString [
	"Include both class and instance methods we define, for classes we don't define."
	| externalClasses |
	externalClasses := self externalClasses.
	aString
		displayProgressAt: Sensor mousePoint
		from: 0 to: externalClasses size
		during: [ :barBlock |
			externalClasses withIndexDo: [ :classOrMetaClass :i | | methods |
				barBlock value: i.
				methods := Array streamContents: [ :stream |
					(self extensionCategoriesForClass: classOrMetaClass) do: [ :cat |
						self methodsInCategory: cat ofClass: classOrMetaClass do: [ :m |
							stream nextPut: m ]]].
				methods sort: [ :a :b |
					a methodSymbol < b methodSymbol ].
				methods do: aBlock.
				]
			]
]

{ #category : 'accessing' }
CodePackage >> sourceSystem [
	^sourceSystem ifNil: ['']
]

{ #category : 'accessing' }
CodePackage >> sourceSystem: aString [

	sourceSystem := aString
]

{ #category : 'listing' }
CodePackage >> systemCategories [
	^ SystemOrganization categories select: [:cat | self includesSystemCategory: cat]
]

{ #category : 'listing' }
CodePackage >> systemCategoriesWithExtensionMethods [

	^ SystemOrganization categories select: [ :cat |
		(SystemOrganization listAtCategoryNamed: cat) anySatisfy: [ :className | | cls |
			cls := Smalltalk at: className.
			(self hasAnyExtensionCategoriesForClass: cls) or: [
				self hasAnyExtensionCategoriesForClass: cls theMetaClass ]]]
]

{ #category : 'naming' }
CodePackage >> systemCategoryPrefix [
	^ packageName
]

{ #category : 'saving' }
CodePackage >> write: classes classCommentsOn: aStream [

	classes do: [ :class |
		class organization classComment isEmpty ifFalse: [
			class organization
				putCommentOnFile: aStream
				numbered: 0
				moveSource: false
				forClass: class ]]
]

{ #category : 'saving' }
CodePackage >> write: classes classDefinitionsOn: aStream [

	classes
		do: [ :class |
			aStream
				nextPut: $!; nextChunkPut: class definitionPreambleWithoutStamp; newLine;
				nextChunkPut: class definition; newLine;

				nextPut: $!; nextChunkPut: class class definitionPreambleWithoutStamp; newLine;
				nextChunkPut: class class definition; newLine;

				newLine ]
		displayingProgress: 'Saving class definitions...'.
]

{ #category : 'saving' }
CodePackage >> write: classes initializersOn: aStream [
	"Write the call to #initialize method of classes defined in us."

	classes do: [ :class |
		(class class includesSelector: #initialize) ifTrue: [
			aStream nextChunkPut: class name, ' initialize'; newLine ]]
]

{ #category : 'saving' }
CodePackage >> write: classes methodsOn: aStream [

	classes
		do: [ :class |
			self 
				writeCoreMethodsOf: class on: aStream;
			 	writeCoreMethodsOf: class class on: aStream ]
		displayingProgress: 'Saving methods...'
]

{ #category : 'saving' }
CodePackage >> writeCoreMethodsOf: aClass on: aStream [

	self coreMethodsOf: aClass do: [ :methodReference |
		methodReference isValid
			ifTrue: [
				self writeMethod: methodReference on: aStream ]]
]

{ #category : 'saving' }
CodePackage >> writeFeatureSpecOn: aStream [
	| provides |
	provides := featureSpec provides.
	aStream
		nextPut: $!; 
		nextChunkPut: 'provides: ', provides name asPlainString printString, ' ', provides version printString, ' ', provides revision printString;
		newLine.
	featureSpec requires do: [ :requires |
		aStream
		nextPut: $!; 
			nextChunkPut: 'requires: ', requires name asPlainString printString, ' ', requires minVersion printString, ' ', requires minRevision printString, ' ', requires maxVersion printString;
			newLine ]
]

{ #category : 'saving' }
CodePackage >> writeMethod: methodReference on: aStream [
	methodReference actualClass
		printMethodChunk: methodReference methodSymbol
		withPreamble: true
		on: aStream
		moveSource: false
		toFile: 0
]

{ #category : 'saving' }
CodePackage >> writeOnStream: aStream [

	| sortedClasses |
	aStream
		nextChunkPut: ('Description ', description) printString;
		newLine.
	self writeFeatureSpecOn: aStream.
	self writeSystemCategoriesOn: aStream.

	self codePackageClass ifNotNil: [ :codePackageClass |
		self
			write: {codePackageClass} classDefinitionsOn: aStream;
			write: {codePackageClass} classCommentsOn: aStream;
			write: {codePackageClass} methodsOn: aStream.
		aStream nextChunkPut: codePackageClass name, ' prePackageInstall'; newLine ].

	sortedClasses := Array streamContents: [ :strm |
		self classesInSafeOrderDo: [ :cls |
			cls == self class ifFalse: [
				strm nextPut: cls ]]].
	self
		write: sortedClasses classDefinitionsOn: aStream;
		write: sortedClasses classCommentsOn: aStream;
		write: sortedClasses methodsOn: aStream.

	self
		sortedExtensionMethodsDo:  [ :methodReference |
			methodReference isValid ifTrue: [
				self writeMethod: methodReference on: aStream ]]
		displayingProgress: 'Saving extension methods...'.
	self write: sortedClasses initializersOn: aStream.

	self codePackageClass ifNotNil: [ :codePackageClass |
		self write: { codePackageClass } initializersOn: aStream.
		aStream nextChunkPut: codePackageClass name, ' postPackageInstall'; newLine ]
]

{ #category : 'saving' }
CodePackage >> writeSystemCategoriesOn: aStream [

	self systemCategories do: [ :categoryName |
		aStream
			nextChunkPut: 'SystemOrganization addCategory: ', categoryName printString;
			newLine ].
	aStream newLine; newLine
]
