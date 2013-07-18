'From Cuis 4.1 of 12 December 2012 [latest update: #1724] on 14 June 2013 at 7:40:35.512063 pm'!

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 6/11/2013 16:18'!
superclassOrderIn: category 
	"Answer an OrderedCollection containing references to the classes in the 
	category whose name is the argument, category (a string). The classes 
	are ordered with superclasses first so they can be filed in."

	| list |
	list _ 
		(self listAtCategoryNamed: category asSymbol) 
			collect: [:title | Smalltalk at: title].
	^Array streamContents: [ :strm |
		Smalltalk hierarchySorted: list do: [ :cls | strm nextPut: cls ]].! !


!CodePackage methodsFor: 'listing' stamp: 'jmv 6/11/2013 14:36'!
classesDo: aBlock
	self systemCategories do: [ :cat |
		(SystemOrganization listAtCategoryNamed: cat) do: [ :className |
			aBlock value: (Smalltalk at: className) ]]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 6/11/2013 16:15'!
classesInSuperclassOrderDo: aBlock

	| myClasses |
	myClasses _ Set new.
	self classesDo: [ :cls | myClasses add: cls ].
	Smalltalk hierarchySorted: myClasses do: aBlock! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 6/11/2013 15:53'!
sortedExtensionMethodsDo: aBlock displayingProgress: aString
	"Include both class and instance methods we define, for classes we don't define."
	| methods |
	self externalClasses do: [ :classOrMetaClass |
		methods _ Array streamContents: [ :stream |
			(self extensionCategoriesForClass: classOrMetaClass) do: [ :cat |
				self methodsInCategory: cat ofClass: classOrMetaClass do: [ :m |
					stream nextPut: m ]]].
		methods sort: [ :a :b |
			a methodSymbol < b methodSymbol ].
		methods do: aBlock displayingProgress: aString ]! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 6/11/2013 14:44'!
write: classes classCommentsOn: aStream

	classes do: [ :class |
		class organization classComment isEmpty ifFalse: [
			class organization
				putCommentOnFile: aStream
				numbered: 0
				moveSource: false
				forClass: class ]]! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 6/11/2013 14:44'!
write: classes classDefinitionsOn: aStream

	classes
		do: [ :class |
			aStream
				nextPut: $!!; nextChunkPut: class definitionPreamble; newLine;
				nextChunkPut: class definition; newLine;

				nextPut: $!!; nextChunkPut: class class definitionPreamble; newLine;
				nextChunkPut: class class definition; newLine;

				newLine ]
		displayingProgress: 'Saving class definitions...'.! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 6/11/2013 14:45'!
write: classes initializersOn: aStream

	classes do: [ :class |
		(class class includesSelector: #initialize) ifTrue: [	
			aStream nextChunkPut: class name, ' initialize'; newLine ]]! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 6/11/2013 16:21'!
write: classes methodsOn: aStream

	self
		sortedExtensionMethodsDo:  [ :methodReference |
			methodReference isValid ifTrue: [
				self writeMethod: methodReference on: aStream ]]
		displayingProgress: 'Saving extension methods...'.
	classes
		do: [ :class | | core |
			core _ self coreMethodsForClass: class.
			core sort: [ :a :b |
				a methodSymbol < b methodSymbol ].
			core do: [ :methodReference |
				methodReference isValid ifTrue: [
					self writeMethod: methodReference on: aStream ]]]
		displayingProgress: 'Saving methods...'! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 6/11/2013 15:27'!
writeMethod: methodReference on: aStream
	methodReference actualClass
		printMethodChunk: methodReference methodSymbol
		withPreamble: true
		on: aStream
		moveSource: false
		toFile: 0! !

!CodePackage methodsFor: 'accessing' stamp: 'jmv 6/11/2013 16:34'!
classNames
	| classNames |
	classNames := Set new.
	self classesDo: [ :cls | classNames add: cls name ].
	^classNames! !

!CodePackage methodsFor: 'as yet unclassified' stamp: 'jmv 6/14/2013 19:27'!
popoinst! !


!SystemDictionary methodsFor: 'query' stamp: 'jmv 6/14/2013 19:35'!
hierarchySorted: classes do: aBlock

	| s |
	s _ classes asSet.
	ProtoObject
		allSubclassesWithLevelDo: [ :cls :l |
			(s includes: cls) ifTrue: [
				aBlock value: cls ].
			(s includes: cls class) ifTrue: [
				aBlock value: cls class ].
			]
		startingLevel: 1! !


!CodePackageList methodsFor: 'accessing' stamp: 'jmv 6/11/2013 16:32'!
summary

	| count |
	selection ifNil: [ ^'' ].
	^ String streamContents: [ :strm |
		strm
			nextPutAll: 'Package: ';
			nextPutAll: selection packageName;
			nextPutAll: ' -- ';
			nextPutAll: selection sourceSystem.
		strm nextPutAll: ' -- Number of system categories '.
		selection systemCategories size printOn: strm.
		strm nextPutAll: '.'.
		strm nextPutAll: ' -- Number of classes: '.
		count _ 0.
		selection classesDo: [ :cls | count _ count + 1 ].
		count printOn: strm.
		strm nextPutAll: '. Number of extension methods: '.
		selection extensionMethods size printOn: strm.
		strm nextPutAll: '. Total number of methods: '.
		selection methods size printOn: strm.
		strm nextPutAll: '. Total lines of code: '.
		selection linesOfCode printOn: strm.
		strm nextPutAll: '.' ]! !


!Behavior methodsFor: 'accessing class hierarchy' stamp: 'jmv 6/11/2013 15:23'!
allSubclassesWithLevelDo: classAndLevelBlock startingLevel: level 
	"Walk the tree of subclasses, giving the class and its level.
	Sort nicely."

	classAndLevelBlock value: self value: level.
	self == Class ifTrue:  [^ self].  "Don't visit all the metaclasses"
	"Visit subclasses in alphabetical order"
	self subclasses
		sort: [ :a :b | a name <= b name ];
		do: [ :subclass | 
			subclass
				allSubclassesWithLevelDo: classAndLevelBlock
				startingLevel: level + 1 ]! !


!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 6/11/2013 14:16'!
fileOutCategory: category on: aFileStream initializing: aBool
	"Store on the file associated with aFileStream, all the classes associated 
	with the category and any requested shared pools."

	| first poolSet tempClass classes |
	classes _ (self superclassOrderIn: category).
	poolSet _ Set new.
	classes do: 
		[:class | class sharedPools do: [:eachPool | poolSet add: eachPool]].
	poolSet size > 0 ifTrue:
		[tempClass _ Class new.
		tempClass shouldFileOutPools ifTrue:
			[poolSet _ poolSet select: [:aPool | tempClass shouldFileOutPool: (Smalltalk keyAtIdentityValue: aPool)].
			poolSet do: [:aPool | tempClass fileOutPool: aPool onFileStream: aFileStream]]].
	first _ true.
	classes do: 
		[:class | 
		first
			ifTrue: [first _ false]
			ifFalse: [aFileStream newLine; nextPut: Character newPage; newLine].
		class
			fileOutOn: aFileStream
			moveSource: false
			toFile: 0
			initializing: false].
	aBool ifTrue:[classes do:[:cls| cls fileOutInitializerOn: aFileStream]].! !

!SystemOrganizer methodsFor: 'fileIn/Out' stamp: 'jmv 6/11/2013 14:16'!
fileOutCategoryNoPoolsNoInit: category on: aFileStream
	"Store on the file associated with aFileStream, all the classes associated 
	with the category and any requested shared pools."

	| first classes |
	classes _ self superclassOrderIn: category.
	first _ true.
	classes do: [ :class | 
		first
			ifTrue: [first _ false]
			ifFalse: [aFileStream newLine; nextPut: Character newPage; newLine].
		class
			fileOutOn: aFileStream
			moveSource: false
			toFile: 0
			initializing: false]! !

!SystemOrganizer methodsFor: 'remove' stamp: 'jmv 6/11/2013 14:16'!
removeSystemCategory: category
	"remove all the classes associated with the category"

	(self superclassOrderIn: category) reverseDo: [ :class | class removeFromSystem].

	self removeCategory: category.
! !


!ChangeSet methodsFor: 'fileIn/Out' stamp: 'jmv 6/11/2013 16:18'!
fileOutOn: stream 
	"Write out all the changes the receiver knows about"

	| classList |
	(self isEmpty and: [ stream isKindOf: FileStream ])
		ifTrue: [ self inform: 'Warning: no changes to file out' ].
	classList _ Array streamContents: [ :strm |
		Smalltalk hierarchySorted: self changedClasses do: [ :cls | strm nextPut: cls ]].

	"First put out rename, max classDef and comment changes."
	classList do: [ :aClass | self fileOutClassDefinition: aClass on: stream ].

	"Then put out all the method additions"
	classList do: [ :aClass | self fileOutMethodAdditionsFor: aClass on: stream ].

	"Then put out all the method changes"
	classList do: [ :aClass | self fileOutMethodChangesFor: aClass on: stream ].

	"Finally put out removals, final class defs and reorganization if any"
	classList reverseDo: [ :aClass |
		self fileOutMethodRemovalsFor: aClass on: stream.
		self fileOutMethodMovedToOtherPackagesFor: aClass on: stream.
		self fileOutPSFor: aClass on: stream ].

	self classRemoves sort do: [ :aClassName |
		stream nextPut: $!!; nextChunkPut: ('classRemoval: #', aClassName); newLine.
		stream nextChunkPut: 'Smalltalk removeClassNamed: #', aClassName; newLine ].

	self classMovesToOtherPackage sort do: [ :aClassName |
		stream nextPut: $!!; nextChunkPut: ('classMoveToSomePackage: #', aClassName); newLine.
		stream nextChunkPut: 'Smalltalk removeClassNamedIfInBaseSystem: #', aClassName; newLine ]! !


!CodeFile methodsFor: 'accessing' stamp: 'jmv 6/11/2013 14:28'!
classAt: className
	^ classes at: className! !

!CodeFile methodsFor: 'accessing' stamp: 'jmv 6/11/2013 14:29'!
removeClass: aPseudoClass
	classes removeKey: aPseudoClass name.
	classOrder copy do: [ :cls |
		cls name = aPseudoClass name ifTrue: [ classOrder remove: cls]]! !

!CodeFile methodsFor: 'accessing' stamp: 'jmv 6/11/2013 14:29'!
renameClass: aPseudoClass to: newName
	| oldName |
	oldName _ aPseudoClass name.
	classes removeKey: oldName.
	classes at: newName put: aPseudoClass.
	aPseudoClass renameTo: newName.! !

!CodeFile methodsFor: 'fileIn/fileOut' stamp: 'jmv 6/11/2013 14:28'!
fileOut
	| fileName stream |
	fileName := FillInTheBlankMorph request: 'Enter the file name' initialAnswer:''.
	stream := FileStream newFileNamed: fileName.
	sourceSystem isEmpty ifFalse:[
		stream nextChunkPut: sourceSystem printString; newLine ].
	self fileOutOn: stream.
	stream newLine; newLine.
	classes do: [ :cls |
		cls needsInitialize ifTrue: [
			stream newLine; nextChunkPut: cls name,' initialize']].
	stream newLine.
	stream close! !

!CodeFile methodsFor: 'xtras' stamp: 'jmv 6/11/2013 14:28'!
allMethodReferences
	"Create an answer a Set with method references for all methods in us"
	| answer className metaClass |
	answer _ Set new.
	
	classes do: [ :pseudoClass | 
		className _ pseudoClass name.
		pseudoClass selectors do: [ :selector |
			answer add: 
				(MethodReference new
					setClassSymbol: className
					classIsMeta: false
					methodSymbol: selector
					stringVersion: className, ' ' , selector) ].
		pseudoClass hasMetaclass ifTrue: [
			metaClass _ pseudoClass metaClass.
			metaClass selectors do: [ :selector |
				answer add: 
					(MethodReference new
						setClassSymbol: className
						classIsMeta: true
						methodSymbol: selector
						stringVersion: className, ' class ' , selector) ].
				]].
	^answer! !


!CodePackageFile methodsFor: 'initialize' stamp: 'jmv 6/11/2013 16:34'!
buildFileStream: aFileStream packageName: pkName fullName: fullFileName
	"Just build the PackageFile object. Don't install the code."

	| classesDefined classesExtended classesToDeleteButCant classesToReallyDelete packageInMemory |
	packageName _ pkName.
	fullName _ fullFileName.
	"Don't register a package!!"
	packageInMemory _ CodePackage
		named: packageName
		createIfAbsent: true
		registerIfNew: false.
	self buildFrom: aFileStream.
	"Compute stuff no longer in package: Should be removed from system."
	classesDefined _ Set new.
	classesExtended _ Set new.
	classes do: [ :pseudoClass |
		pseudoClass hasDefinition
			ifTrue: [ classesDefined add: pseudoClass name ]
			ifFalse: [ classesExtended add: pseudoClass name ]].
	classesToRemove _ packageInMemory classNames difference: classesDefined.
	"Add here:
		- classes in classesToDelete, that #allCallsOn answers selectors that aren't in classesToDelete or methodsToRemove
		- classes with #subclasses that aren't in classesToDelete.
		- classes with existing instances (#instanceCount)? Not really sure... Maybe sole instance referenced from classVar or such...
		- something else I forgot?
	Warning: This search for stuff that can't be removed must be iterated again until it doesn't find any more."
	classesToDeleteButCant _ classesToRemove intersection: classesExtended.
	classesToReallyDelete _ classesToRemove difference: classesToDeleteButCant.
	"Methods. Could also mean classes that can't be deleted!! (include in the iteration)
	Warn if deleting last implementor of sent messages?"
	methodsToRemove _ packageInMemory methods asSet difference: self allMethodReferences.
	methodsToRemove _ methodsToRemove reject: [ :methodReference | classesToReallyDelete includes: methodReference classSymbol ].

"
	'=============' print.
	('classesToRemove: ', classesToRemove printString) print.
	('classesToDeleteButCant: ', classesToDeleteButCant printString) print.
	('classesToReallyDelete: ', classesToReallyDelete printString) print.
	'=============' print.
	'methodsToRemove: ' print.
	methodsToRemove do: [ :methodReference | methodReference print ].
	'=============' print.
"! !


!CodePackage methodsFor: 'enumerating' stamp: 'jmv 6/11/2013 16:23'!
actualMethodsDo: aBlock
	"Evaluate aBlock with the actual method objects in this package."
	| enum |
	self extensionMethods do: [ :mr |
		aBlock value: mr compiledMethod ].
	enum _ [ :behavior |
			behavior organization categories do: [ :cat |
				(self isForeignClassExtension: cat) ifFalse: [
					(behavior organization listAtCategoryNamed: cat) do: [ :s |
						aBlock value: (behavior compiledMethodAt: s)]]]].
	self classesDo: [ :c|
		enum value: c; value: c class ]! !

!CodePackage methodsFor: 'enumerating' stamp: 'jmv 6/11/2013 15:23'!
allOverriddenMethodsDo: aBlock
	"Evaluates aBlock with all the overridden methods in the system"
	ProtoObject
		allSubclassesWithLevelDo: [ :cls :l |
			self overriddenMethodsInClass: cls do: aBlock.
			self overriddenMethodsInClass: cls class do: aBlock ]
		startingLevel: 1! !

!CodePackage methodsFor: 'enumerating' stamp: 'jmv 6/11/2013 16:36'!
overriddenMethodsInClass: aClass do: aBlock
	"Evaluates aBlock with the overridden methods in aClass"
	^ self overrideCategoriesForClass: aClass do: [ :cat |
		self methodsInCategory: cat ofClass: aClass do: aBlock]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 6/11/2013 16:06'!
coreMethods
	^Array streamContents: [ :strm |
		self classesDo: [ :cls |
			strm nextPutAll: (self coreMethodsForClass: cls).
			strm nextPutAll: (self coreMethodsForClass: cls class) ]]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 6/11/2013 14:49'!
extensionClasses
	"Classes and metaClasses for which we do define extensions.
	Include a class if we define some instance method.
	Include a metaclass if we define some class method."
	^ self externalClasses select: [ :classOrMetaClass |
		self hasAnyExtensionCategoriesForClass: classOrMetaClass ]! !

!CodePackage methodsFor: 'listing' stamp: 'jmv 6/11/2013 15:41'!
extensionMethods
	"Include both class and instance methods we define, for classes we don't define."
	^Array streamContents: [ :stream |
		self externalClasses do: [ :classOrMetaClass |
			(self extensionCategoriesForClass: classOrMetaClass) do: [ :cat |
				self methodsInCategory: cat ofClass: classOrMetaClass do: [ :m |
					stream nextPut: m ]]]]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 6/11/2013 16:41'!
foreignExtensionMethodsForClass: aClass
	^Array streamContents: [ :stream |
		(self foreignExtensionCategoriesForClass: aClass) do: [ :cat |
			(aClass organization listAtCategoryNamed: cat) do: [ :sel |
				stream nextPut: (self referenceForMethod: sel ofClass: aClass) ]]]! !

!CodePackage methodsFor: 'testing' stamp: 'jmv 6/11/2013 16:30'!
includesAnyCode
	self classesDo: [ :cls |
		^true ].
	^self methods notEmpty! !

!CodePackage methodsFor: 'dependencies' stamp: 'jmv 6/11/2013 16:05'!
externalClasses
	"Answer Classes and Metaclasses not defined in self."
	| myClasses |
	myClasses _ Set new.
	self classesDo: [ :cls | myClasses add: cls ].
	^ Array streamContents: [ :s |
		ProtoObject
			allSubclassesWithLevelDo: [ :cls :l |
				(myClasses includes: cls) ifFalse: [
					s nextPut: cls; nextPut: cls class ]]
			startingLevel: 1 ]! !

!CodePackage methodsFor: 'dependencies' stamp: 'jmv 6/11/2013 16:38'!
externalRefsSelect: selBlock thenCollect: colBlock
	| pkgMethods dependents extMethods otherClasses otherMethods classNames |

	classNames := self classNames.
	extMethods := self extensionMethods collect: [:mr | mr methodSymbol].
	otherClasses := self externalClasses difference: self externalSubclasses.
	otherMethods := Array streamContents: [ :strm | otherClasses do: [ :c | strm nextPutAll: c selectors ]].
	pkgMethods := self methods asSet collect: [:mr | mr methodSymbol].
	pkgMethods removeAllFoundIn: otherMethods.

	dependents := Set new.
	otherClasses do: [:c |
		c selectorsAndMethodsDo: [ :sel :compiled |
			| refs |
			(extMethods includes: sel) ifFalse: [
				refs := compiled literals select: selBlock thenCollect: colBlock.
				refs do: [ :ea |
					((classNames includes: ea) or: [pkgMethods includes: ea])
						ifTrue: [
							dependents add: (self referenceForMethod: sel ofClass: c) -> ea]]]]].
	^ dependents! !

!CodePackage methodsFor: 'dependencies' stamp: 'jmv 6/11/2013 16:27'!
externalSubclasses
	| subClasses pkgClasses |
	pkgClasses := Set new.
	subClasses := Set new.
	self classesDo: [ :c |
		pkgClasses add: c.
		subClasses addAll: c allSubclasses ].
	^ subClasses difference: pkgClasses
! !

!CodePackage methodsFor: 'saving' stamp: 'jmv 6/11/2013 16:09'!
writeOnStream: aStream

	| sortedClasses |
	sortedClasses _ Array streamContents: [ :strm |
		self classesInSuperclassOrderDo: [ :cls |
			strm nextPut: cls ]].
	aStream
		nextChunkPut: ('Description ', description) printString;
		newLine.
	self
		write: sortedClasses classDefinitionsOn: aStream;
		write: sortedClasses classCommentsOn: aStream;
		write: sortedClasses methodsOn: aStream;
		write: sortedClasses initializersOn: aStream! !


!CodePackage class methodsFor: 'packages access' stamp: 'jmv 6/14/2013 19:26'!
register: aCodePackage
	"
	Usually call #named:createIfAbsent:registerIfNew: instead
	CodePackage register: (CodePackage newNamed: 'Tests-Files')
	"
	| any base |
	InstalledPackages at: aCodePackage packageName put: aCodePackage.

	"Anything that now belongs in this package, was moved out of the base system!!"
	base _ ChangeSet changeSetForBaseSystem.
	any _ false.
	aCodePackage classesDo: [ :cls |
		any _ true.
		base noteClassMoveToOtherPackage: cls ].
	aCodePackage extensionMethods do: [ :methodReference |
		methodReference isValid ifTrue: [
			any _ true.
			base
				noteMethodMoveToOtherPackage: methodReference selector
				forClass: methodReference actualClass ]].
	any ifTrue: [
		aCodePackage hasUnsavedChanges: true ].

	self triggerEvent: #installedPackagesChanged! !

!methodRemoval: Collection #gather:!
Collection removeSelector: #gather:!
!methodRemoval: CodePackage #allOverridenMethods!
CodePackage removeSelector: #allOverridenMethods!
!methodRemoval: CodePackage #classes!
CodePackage removeSelector: #classes!
!methodRemoval: CodePackage #classesAndMetaClasses!
CodePackage removeSelector: #classesAndMetaClasses!
!methodRemoval: CodePackage #extensionMethodsForClass:!
CodePackage removeSelector: #extensionMethodsForClass:!
!methodRemoval: CodePackage #extensionMethodsFromClasses:!
CodePackage removeSelector: #extensionMethodsFromClasses:!
!methodRemoval: CodePackage #externalBehaviors!
CodePackage removeSelector: #externalBehaviors!
!methodRemoval: CodePackage #methodsInCategory:ofClass:!
CodePackage removeSelector: #methodsInCategory:ofClass:!
!methodRemoval: CodePackage #outsideClasses!
CodePackage removeSelector: #outsideClasses!
!methodRemoval: CodePackage #sortedMethods!
CodePackage removeSelector: #sortedMethods!
!methodRemoval: CodePackage #writeClassCommentsOn:!
CodePackage removeSelector: #writeClassCommentsOn:!
!methodRemoval: CodePackage #writeClassDefinitionsOn:!
CodePackage removeSelector: #writeClassDefinitionsOn:!
!methodRemoval: CodePackage #writeInitializersOn:!
CodePackage removeSelector: #writeInitializersOn:!
!methodRemoval: CodePackage #writeMethodsOn:!
CodePackage removeSelector: #writeMethodsOn:!
!methodRemoval: ChangeSet class #doWeFileOut:given:cache:!
ChangeSet class removeSelector: #doWeFileOut:given:cache:!
!methodRemoval: ChangeSet class #superclassOrder:!
ChangeSet class removeSelector: #superclassOrder:!
!methodRemoval: SystemOrganizer #superclassOrder:!
SystemOrganizer removeSelector: #superclassOrder:!
