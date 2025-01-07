'From Cuis7.3 [latest update: #6968] on 6 January 2025 at 3:21:23 pm'!

!CodePackage methodsFor: 'listing' stamp: 'jmv 1/6/2025 12:17:20'!
coreMethodsDo: aBlock
	self classesDo: [ :cls |
		self coreMethodsOf: cls do: aBlock.
		self coreMethodsOf: cls class do: aBlock ].! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 1/6/2025 12:32:58'!
extensionMethodsDo: aBlock
	"Include both class and instance methods we define, for classes we don't define."

	self externalClasses do: [ :classOrMetaClass |
		(self extensionCategoriesForClass: classOrMetaClass) do: [ :cat |
			self methodsInCategory: cat ofClass: classOrMetaClass do: [ :m |
				aBlock value: m ]]]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 1/6/2025 12:35:34'!
methodsDo: aBlock
	self coreMethodsDo: [ :methodRef |
		methodRef isValid ifTrue: [ aBlock value: methodRef ]].
	self extensionMethodsDo: [ :methodRef |
		methodRef isValid ifTrue: [ aBlock value: methodRef ]].! !

!CodePackage methodsFor: 'source code management' stamp: 'jmv 1/6/2025 12:36:49'!
extensionMethodCount

	| sum |
	sum := 0.
	self extensionMethodsDo: [ :each |
		sum := sum + 1 ].
	^sum! !

!CodePackage methodsFor: 'source code management' stamp: 'jmv 1/6/2025 12:25:16'!
methodCount

	| sum |
	sum := 0.
	self methodsDo: [ :each |
		sum := sum + 1 ].
	^sum! !


!CodePackageList methodsFor: 'accessing' stamp: 'jmv 1/6/2025 12:37:41'!
summary

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
		linesOfCode printOn: strm ]! !


!CodePackage methodsFor: 'listing' stamp: 'jmv 1/6/2025 12:20:46'!
coreMethods
	^Array streamContents: [ :strm |
		self coreMethodsDo: [ :methodRef |
			strm nextPut: methodRef ]]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 1/6/2025 12:34:04'!
extensionMethods
	"Include both class and instance methods we define, for classes we don't define."
	^Array streamContents: [ :stream |
		self extensionMethodsDo: [ :methodRef |
			stream nextPut: methodRef ]]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 1/6/2025 12:39:48'!
methods

	^Array streamContents: [ :strm |
		self extensionMethodsDo: [ :methodRef |
			methodRef isValid ifTrue: [ strm nextPut: methodRef ]].
		self coreMethodsDo: [ :methodRef |
			methodRef isValid ifTrue: [ strm nextPut: methodRef ]]]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 1/6/2025 12:29:01'!
selectors

	^ Array streamContents: [ :strm |
		self methodsDo: [ :ea | strm nextPut: ea methodSymbol ]]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 1/6/2025 12:27:34'!
includesAnyCode
	self classesDo: [ :cls |
		^true ].
	self methodsDo: [ :cls |
		^true ].
	^false! !

!CodePackage methodsFor: 'source code management' stamp: 'jmv 1/6/2025 12:24:29'!
linesOfCode
	"An approximate measure of lines of code.
	Does not includes comments, or excludes blank lines.
	See comment at CompiledMethod >> #linesOfCode"

	| sum |
	sum := 0.
	self methodsDo: [ :each |
		sum := sum + each compiledMethod linesOfCode ].
	^sum! !

!CodePackage methodsFor: 'source code management' stamp: 'jmv 1/6/2025 12:27:02'!
messageSendsCount
	"See comment at CompiledMethod >> #messageSendsCount"

	| sum |
	sum := 0.
	self methodsDo: [ :each |
		sum := sum + each compiledMethod messageSendsCount ].
	^sum! !


!CodePackage class methodsFor: 'packages access' stamp: 'jmv 1/6/2025 12:36:13'!
register: aCodePackage
	"
	Usually call #named:createIfAbsent:registerIfNew: instead
	CodePackage register: (CodePackage newNamed: 'Tests-Files')
	"
	| any base |
	self installedPackages at: aCodePackage packageName put: aCodePackage.

	"Anything that now belongs in this package, was moved out of the base system!!"
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

	self triggerEvent: #installedPackagesChanged! !

!methodRemoval: CodePackage #coreMethodsForClass: stamp: 'jmv 1/6/2025 15:20:04'!
CodePackage removeSelector: #coreMethodsForClass:!
