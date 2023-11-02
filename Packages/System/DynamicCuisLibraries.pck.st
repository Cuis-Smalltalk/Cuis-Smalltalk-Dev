'From Cuis 6.0 [latest update: #6068] on 2 November 2023 at 10:43:20 am'!
'Description '!
!provides: 'DynamicCuisLibraries' 1 22!
SystemOrganization addCategory: #'DynamicCuisLibraries-Code Model'!
SystemOrganization addCategory: #'DynamicCuisLibraries-Dynamic Libraries'!
SystemOrganization addCategory: #DynamicCuisLibraries!


!classDefinition: #CodeElementSpec category: #'DynamicCuisLibraries-Code Model'!
Object subclass: #CodeElementSpec
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'DynamicCuisLibraries-Code Model'!
!classDefinition: 'CodeElementSpec class' category: #'DynamicCuisLibraries-Code Model'!
CodeElementSpec class
	instanceVariableNames: ''!

!classDefinition: #ClassSpec category: #'DynamicCuisLibraries-Code Model'!
CodeElementSpec subclass: #ClassSpec
	instanceVariableNames: 'name superclass type instVarNames classVarNames poolDictionaryNames classInstVarNames instVarCountBeforeInstall instVarCountAfterInstall methodSpecsInstVarIndexMap'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'DynamicCuisLibraries-Code Model'!
!classDefinition: 'ClassSpec class' category: #'DynamicCuisLibraries-Code Model'!
ClassSpec class
	instanceVariableNames: ''!

!classDefinition: #LiteralVariableSpec category: #'DynamicCuisLibraries-Code Model'!
CodeElementSpec subclass: #LiteralVariableSpec
	instanceVariableNames: 'variableName'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'DynamicCuisLibraries-Code Model'!
!classDefinition: 'LiteralVariableSpec class' category: #'DynamicCuisLibraries-Code Model'!
LiteralVariableSpec class
	instanceVariableNames: ''!

!classDefinition: #MethodSpec category: #'DynamicCuisLibraries-Code Model'!
CodeElementSpec subclass: #MethodSpec
	instanceVariableNames: 'classSpec isClassSide selector header literals pragmas bytecodes'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'DynamicCuisLibraries-Code Model'!
!classDefinition: 'MethodSpec class' category: #'DynamicCuisLibraries-Code Model'!
MethodSpec class
	instanceVariableNames: ''!

!classDefinition: #DynamicCuisLibrary category: #'DynamicCuisLibraries-Dynamic Libraries'!
Object subclass: #DynamicCuisLibrary
	instanceVariableNames: 'classSpecs methodSpecs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'DynamicCuisLibraries-Dynamic Libraries'!
!classDefinition: 'DynamicCuisLibrary class' category: #'DynamicCuisLibraries-Dynamic Libraries'!
DynamicCuisLibrary class
	instanceVariableNames: ''!

!classDefinition: #BasicFileServices category: #DynamicCuisLibraries!
Object subclass: #BasicFileServices
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'DynamicCuisLibraries'!
!classDefinition: 'BasicFileServices class' category: #DynamicCuisLibraries!
BasicFileServices class
	instanceVariableNames: ''!


!CodeElementSpec commentStamp: '<historical>' prior: 0!
CodeElementSpec is an abstract class that groups classes that represent Smalltalk code. See subclasses for details.!

!ClassSpec commentStamp: '<historical>' prior: 0!
A ClassSpec describes a class. It includes enough detail for a real class to be created from it. But it is not a class:
- It can not create instances. You need a real class for that.
- It is not a global. It is not in the Smalltalk dictionary. It can not clash.
- There can be several ClassSpecs for different versions of a class in memory.
- It is a regular object. Doesn't include any detail specific to Cuis ObjectMemory or VM (Spur). Doesn't refer to globals.
- It is easy to serialize and deserialize. Nothing special to take care of.!

!LiteralVariableSpec commentStamp: '<historical>' prior: 0!
A LiteralVariableSpec represent a Class variable, SharedPool variable or Global variable.
It is stored in MethodSpec literals, instead of the original Association or ReadOnlyVariableBinding, that could refer to any Global or other object we'd rather not refer to from ClassSpec / MethodSpec.!

!MethodSpec commentStamp: 'jmv 4/17/2023 11:03:54' prior: 0!
A MethodSpec describes a CompiledMethod. It includes enough detail for a real CompiledMethod to be created from it. But it is not a CompiledMethod
- It can't be run. Can't be added to a class.
- There can be several MethodSpecs for different versions of a method in memory.
- It is a regular object. Doesn't include any detail specific to Cuis ObjectMemory or VM (Spur). Doesn't refer to globals.
- It is easy to serialize and deserialize. Nothing special to take care of.!

!DynamicCuisLibrary commentStamp: '<historical>' prior: 0!
A DynamicCuisLibrary defines a set of classes and methods that can be loaded into a running Cuis image. Its parts, instances of ClassSpec and MethodSpec are not actual Classes and CompiledMethods, but regular objects. Actual Classes and CompiledMethods are created from them at load time.!

!BasicFileServices commentStamp: '<historical>' prior: 0!
This class is only meant to be used by for loading DynamicCuisLibraries. Main reaso is that MinimalImages, don't have the full System-File System and System-Files categories.

 This class can't replace any kind of Stream, and can only deal with the whole contents of files at once.!

!CodeElementSpec methodsFor: 'testing' stamp: 'jmv 5/16/2023 14:51:44'!
is: aSymbol
	^aSymbol == #CodeElementSpec or: [ super is: aSymbol]! !

!ClassSpec methodsFor: 'accessing' stamp: 'jmv 9/6/2023 17:49:25'!
allInstVarCountInSpec
	"In this Spec object. Unrelated to actual Class.
	Including inherited from superclasses"
	| inherited |
	inherited := superclass
		ifNotNil: [ superclass allInstVarCountInSpec ]
		ifNil: [ 0 ].
	^instVarNames size + inherited! !

!ClassSpec methodsFor: 'accessing' stamp: 'jmv 5/20/2023 13:53:54'!
classInstVarNames
	^classInstVarNames! !

!ClassSpec methodsFor: 'accessing' stamp: 'jmv 6/30/2023 15:09:56'!
classVarNames
	^classVarNames! !

!ClassSpec methodsFor: 'accessing' stamp: 'fz 5/24/2023 20:48:53'!
instVarNames
	^instVarNames! !

!ClassSpec methodsFor: 'accessing' stamp: 'jmv 9/6/2023 17:49:30'!
methodSpecsInstVarIndexMap
	"To be used during materialization and installing"
	^methodSpecsInstVarIndexMap! !

!ClassSpec methodsFor: 'accessing' stamp: 'jmv 4/17/2023 15:12:32'!
name
	^name! !

!ClassSpec methodsFor: 'accessing' stamp: 'jmv 4/17/2023 11:44:53'!
name: aSymbol

	name := aSymbol.! !

!ClassSpec methodsFor: 'accessing' stamp: 'jmv 6/23/2023 11:04:06'!
superclass
	^superclass! !

!ClassSpec methodsFor: 'accessing' stamp: 'jmv 9/6/2023 17:49:34'!
totalClassInstVarCountInSpec
	"Including inherited from superclasses"
	| inherited |
	inherited := superclass
		ifNotNil: [ superclass totalClassInstVarCountInSpec ]
		ifNil: [ 0 ].
	^classInstVarNames size + inherited! !

!ClassSpec methodsFor: 'accessing' stamp: 'jmv 8/1/2023 15:54:10'!
typeOfClass
	^type! !

!ClassSpec methodsFor: 'materialization' stamp: 'jmv 9/6/2023 17:49:38'!
allInstVarCountAfterInstall
	"In the actual Class. How it will look after installing us.
	Including inherited from superclasses"
	| inherited |
	inherited := superclass
		ifNotNil: [ superclass allInstVarCountAfterInstall ]
		ifNil: [ 0 ].
	^instVarCountAfterInstall + inherited! !

!ClassSpec methodsFor: 'materialization' stamp: 'jmv 9/6/2023 17:49:41'!
allInstVarCountBeforeInstall
	"In the actual Class. Before installing us.
	Including inherited from superclasses"
	| inherited |
	inherited := superclass
		ifNotNil: [ superclass allInstVarCountBeforeInstall ]
		ifNil: [ 0 ].
	^instVarCountBeforeInstall + inherited! !

!ClassSpec methodsFor: 'materialization' stamp: 'jmv 9/6/2023 17:49:44'!
instVarCountAfterInstall
	"In the actual Class. How it will look after installing us."

	^instVarCountAfterInstall! !

!ClassSpec methodsFor: 'materialization' stamp: 'jmv 9/6/2023 17:49:47'!
instVarCountBeforeInstall
	"In the actual Class. Before installing us."

	^instVarCountBeforeInstall! !

!ClassSpec methodsFor: 'materialization' stamp: 'jmv 8/10/2023 16:14:14'!
materializeAndInstall
	
	^self doesExist
		ifFalse: [ self materializeNewClass ]
		ifTrue: [ self mergeInto: (Smalltalk at: name) ].! !

!ClassSpec methodsFor: 'materialization' stamp: 'jmv 9/14/2023 10:15:17'!
materializeNewClass
	"Only for new classes	"

	| newClass metaclass newMeta builder category |
	builder := ClassBuilder new.
	newClass := builder
		newSubclassOf: (superclass ifNotNil: [ Smalltalk at: superclass name ])
		type: type
		instanceVariables: instVarNames
		from: nil.
	newClass setName: name.

	newClass declareClassVariables: classVarNames.

	poolDictionaryNames ifNotNil: [
		poolDictionaryNames do: [ :poolName | | pool |
			pool := Smalltalk at: poolName asSymbol ifAbsentPut: [ Dictionary new ].
			newClass addSharedPool: pool ]].

	classInstVarNames notEmpty ifTrue: [
		metaclass := newClass class.
		newMeta := builder
			newSubclassOf: metaclass superclass 
			type: metaclass typeOfClass
			instanceVariables: classInstVarNames
			from: metaclass.
		builder mutate: metaclass to: newMeta ].

	"... classify ..."
	(Smalltalk includesKey: #SystemOrganization) ifTrue: [
		category := nil.
		Smalltalk organization classify: newClass name under: category ].
	Smalltalk at: newClass name put: newClass.
	"ver si falta algo de
	name:subclassOf:type:instanceVariableNames:classVariableNames:poolDictionaries:category:unsafe:
	"
	^newClass! !

!ClassSpec methodsFor: 'materialization' stamp: 'jmv 9/14/2023 10:13:02'!
mergeInto: existingClass
	"Ensure our instance variables, class variables, class instance variables and sharedPools are 
	included in existingClass. Add them as needed."

	| existingIvarNames newIvarNames existingClassVarNames newClassVarNames
	existingClassInstanceVarNames newClassInstanceVarNames classInstanceVarNames
	existingSharedPools newSharedPools
	newClass metaclass newMeta builder |

	existingIvarNames := existingClass instVarNames.
	newIvarNames := instVarNames reject: [ :any | existingIvarNames includes: any ].

	existingClassVarNames := existingClass classPool keys.
	newClassVarNames := classVarNames reject: [ :any | existingClassVarNames includes: any ].

	existingSharedPools := existingClass sharedPools asIdentitySet.
	newSharedPools := #(). "Could create it with #streamContents:...."
	poolDictionaryNames do: [ :poolName | | pool |
		pool := Smalltalk at: poolName asSymbol ifAbsentPut: [ Dictionary new ].
		(existingSharedPools includes: pool) ifFalse: [
			newSharedPools := newSharedPools copyWith: pool ]].

	existingClassInstanceVarNames := existingClass class instVarNames.
	newClassInstanceVarNames := classInstVarNames reject: [ :any | existingClassInstanceVarNames includes: any ].

	"If existingClass is already OK, do nothing."
	(newIvarNames isEmpty and: [ newClassVarNames isEmpty and:
			[ newSharedPools isEmpty and: [ newClassInstanceVarNames isEmpty ]]]) ifTrue: [
		^existingClass ].

	builder := ClassBuilder new.
	newClass := builder
		newSubclassOf: existingClass superclass
		type: type
		instanceVariables: existingIvarNames, newIvarNames
		from: nil.
	newClass setName: existingClass name.
	newClass declareClassVariables: existingClassVarNames, newClassVarNames.

	newSharedPools do: [ :pool |
		newClass addSharedPool: pool ].

	classInstanceVarNames := existingClassInstanceVarNames, newClassInstanceVarNames.
	classInstanceVarNames notEmpty ifTrue: [
		metaclass := newClass class.
		newMeta := builder
			newSubclassOf: metaclass superclass 
			type: metaclass typeOfClass
			instanceVariables: classInstanceVarNames
			from: metaclass.
		builder update: metaclass to: newMeta ].

	existingClass selectorsAndMethodsDo: [ :selector :method |
		newClass addSelectorSilently: selector withMethod: method ].
	existingClass methodDict removeAll.
	existingClass class selectorsAndMethodsDo: [ :selector :method |
		newClass class addSelectorSilently: selector withMethod: method ].
	existingClass class methodDict removeAll.
	
	existingClass hasSubclasses ifTrue: [
		newClass instVarNamed: 'subclasses' put: existingClass subclasses ].
	builder update: existingClass to: newClass.
	^newClass! !

!ClassSpec methodsFor: 'materialization' stamp: 'jmv 9/6/2023 17:49:00'!
preInstall
	"Prepare classInstVarCountBeforeInstall, classInstVarCountAfterInstall, methodSpecsInstVarIndexMap
	These will be needed for remapping instance variable accesses both from existing CompiledMethods
	and our own MethodSpecs.
	Note: we assume we #canBeInstalled.
	Note: This method is assumed to be called in hierarchy order, especially that our superclassSpec
		instVarCountBeforeInstall and instVarCountAfterInstall have already been set."

	| existingInstVarNames specIvarsOffset afterInstallIvarsOffset |

	methodSpecsInstVarIndexMap := nil.
	"For new classes, nothing to do. No existing class, no existing subclasses.
	Therefore, no existing methods in class or subclasses.
	Additionally any MethodSpec is already OK, no ivar remapping needed."
	self doesExist ifFalse: [
		instVarCountBeforeInstall := nil.
		instVarCountAfterInstall := instVarNames size.
		^self ].

	"We already exist in the system."
	existingInstVarNames := (Smalltalk at: name) instVarNames.
	instVarCountBeforeInstall := existingInstVarNames size.
	instVarCountAfterInstall := instVarCountBeforeInstall. "to be incremented below"

	specIvarsOffset := 0.
	afterInstallIvarsOffset := 0.
	superclass notNil ifTrue: [
		specIvarsOffset := superclass allInstVarCountInSpec.
		afterInstallIvarsOffset := superclass allInstVarCountAfterInstall ].

	"Asking for equality is faster than repeated #indexOf:
	No new instance variables in any superclass and
	no changes in instance variables in us, means no remapping needed."
	(specIvarsOffset = afterInstallIvarsOffset and: [
		existingInstVarNames = instVarNames ]) ifTrue: [
			^self ].

	"Build ivar remapping dictionary. Update classInstVarCountAfterInstall."
	superclass notNil ifTrue: [
		superclass methodSpecsInstVarIndexMap ifNotNil: [ :superclassMap |
			methodSpecsInstVarIndexMap := superclassMap copy ]].
	methodSpecsInstVarIndexMap isNil ifTrue: [
		methodSpecsInstVarIndexMap := Dictionary new ].
	instVarNames withIndexDo: [ :eachVar :j | | specIvarIndex afterInstallIvarsIndex |
		specIvarIndex := specIvarsOffset + j.
		afterInstallIvarsIndex := afterInstallIvarsOffset +
			(existingInstVarNames indexOf: eachVar
				ifAbsent: [ instVarCountAfterInstall := instVarCountAfterInstall + 1 ]).
		afterInstallIvarsIndex = specIvarIndex ifFalse: [
			methodSpecsInstVarIndexMap add: specIvarIndex -> afterInstallIvarsIndex ]].! !

!ClassSpec methodsFor: 'testing' stamp: 'jmv 10/25/2023 10:48:13'!
canBeInstalled
	"Answer false if we can't be installed in the running system.
	Note: it is assumed that installation will happen in hierarchy order, starting from ProtoObject.

	(ClassSpec for: Morph addClassesTo: OrderedDictionary new) doesExist.
	(ClassSpec for: Morph addClassesTo: OrderedDictionary new) equalsExisting.
	(ClassSpec for: Morph addClassesTo: OrderedDictionary new) canBeInstalled.
	"

	| existing existingSuperclass addedInstanceVariables |
	existing := nil.
	self doesExist ifTrue: [
		"We already exist in the system. Our superclass must match existing superclass."
		existing := Smalltalk at: name.
		existingSuperclass := existing superclass.

		"If either we are sublcass of nil or existing is (but not both), it means we can't be installed"
		existingSuperclass isNil = superclass isNil ifFalse: [ ^false ].

		"Not subclass of nil. Superclass must match by name."
		existingSuperclass notNil ifTrue: [
			existingSuperclass name = superclass name ifFalse: [ ^false ]].

		"We don't allow turning a regular class (ivars) into an array, or any other such changes."
		existing typeOfClass = type ifFalse: [ ^false ].

		"We don't adjust existing class methods for differences in class ivars.
		We don't migrate or become: classes.
		So, class ivars must be the same as in existing class."
		existing class allClassInstVarNames =		classInstVarNames ifFalse: [ ^false ]].

	"If no new instance variables are added, shape will not change.
	Also, we'd be not requiring migration of any instances."
	addedInstanceVariables := existing
		ifNil: [ instVarNames ]
		ifNotNil: [ instVarNames reject: [ :v | existing definesInstanceVariableNamed: v ]].
	addedInstanceVariables ifEmpty: [ ^true ].

	"So we are adding instance variables, either to a new or existing class."
	existing ifNotNil: [
		"We're modifying the shape of the existing class, fail if there are (sub)instances"
		existing withAllSubclassesDo: [ :cls |
			cls someInstance ifNotNil: [ ^false ]].
		"Also fail if some subclass already includes any variable we want to add."
		addedInstanceVariables do: [ :v |
			existing allSubclassesDo: [ :cls |
				(cls definesInstanceVariableNamed: v) ifTrue: [ ^false ]]]].

	"If we try to add an instance variable that is already defined in an existing superclass, we can't be installed."
	self allSuperclassesDo: [ :clsSpec |
		Smalltalk at: clsSpec name ifPresent: [ :cls |
			addedInstanceVariables do: [ :v |
				(cls definesInstanceVariableNamed: v) ifTrue: [ ^false ]]]].

	"It looks like there are no problems then."
	^true! !

!ClassSpec methodsFor: 'testing' stamp: 'jmv 8/3/2023 13:48:20'!
doesExist
	^(Smalltalk at: name ifAbsent: nil) isBehavior! !

!ClassSpec methodsFor: 'testing' stamp: 'jmv 8/10/2023 15:33:40'!
equalsExisting
	"
	(ClassSpec for: Morph addClassesTo: OrderedDictionary new) equalsExisting
	"
	"
	| allClasses |
	allClasses := OrderedDictionary new.
	Smalltalk allClassesDo: [ :each | 
		self assert: (ClassSpec for: each addClassesTo: allClasses) equalsExisting ].
	allClasses
	" 
	| existing existingClassPool |
	superclass notNil ifTrue: [
		superclass equalsExisting ifFalse: [ ^false ]].
	self doesExist ifFalse: [ ^false ].
	existing := 	Smalltalk at: name ifAbsent: [ ^false ].
	existing isBehavior ifFalse: [ ^false ].  "Must be a class!!"
	existing isMeta ifTrue: [ ^false ].          "We only represent the instance side"
	existing name = name ifFalse: [ ^false ].
	existing typeOfClass = type ifFalse: [ ^false ].
	existing instVarNames = instVarNames ifFalse: [ ^false ].
	existingClassPool := existing classPool.
	existingClassPool size = classVarNames size ifFalse: [ ^false ].
	classVarNames do: [ :cVarName |
		(existingClassPool includesKey: cVarName) ifFalse: [ ^false ]].
	existing sharedPools = poolDictionaryNames ifFalse: [ ^false ].
	^true! !

!ClassSpec methodsFor: 'enumerating' stamp: 'jmv 8/3/2023 18:53:12'!
allSuperclassesDo: aBlock 
	"Evaluate the argument, aBlock, for each of the receiver's superclasses."

	superclass ifNotNil: [
		aBlock value: superclass.
		superclass allSuperclassesDo: aBlock]! !

!ClassSpec methodsFor: 'enumerating' stamp: 'jmv 9/19/2023 12:37:29'!
withAllSuperclassesDo: aBlock 
	"Evaluate the argument, aBlock, for each of the receiver's superclasses."
	aBlock value: self.
	superclass
		ifNotNil: [superclass withAllSuperclassesDo: aBlock]! !

!ClassSpec methodsFor: 'private' stamp: 'jmv 5/20/2023 13:52:05'!
classInstVarNames: iVarNameCollection
	classInstVarNames := iVarNameCollection! !

!ClassSpec methodsFor: 'private' stamp: 'jmv 9/12/2023 15:57:36'!
name: aSymbol superclass: aClassSpec type: classTypeSymbol instVarNames: iVarNameCollection classVarNames: cVarNameCollection

	name := aSymbol.
	superclass := aClassSpec.
	type := classTypeSymbol.
	instVarNames := iVarNameCollection.
	classVarNames := cVarNameCollection.
	poolDictionaryNames := #().! !

!ClassSpec methodsFor: 'printing' stamp: 'fz 5/12/2023 13:43:14'!
printOn: stream

	stream nextPut: $<; nextPutAll: self name; nextPutAll: ' Spec'; nextPut: $>! !

!ClassSpec class methodsFor: 'instance creation' stamp: 'jmv 8/2/2023 11:03:59'!
for: aClass addClassesTo: anOrderedDictionaryOfClasses
	"aClass is a regular class that exists in the system
	anOrderedDictionaryOfClasses will end having all required ClassSpecs, in a proper hierarchy order."
	"
	| sortedClassSpecs |
	sortedClassSpecs := OrderedDictionary new.
	ClassSpec for: SystemWindow addClassesTo: sortedClassSpecs.
	ClassSpec for: Morph addClassesTo: sortedClassSpecs.
	sortedClassSpecs
	"
	| className answer |
	className := aClass name.
	"Remove prefixes such as 'Noclash_', 'Noclash003_', NoclashMySpecialName_', etc "
	(className beginsWith: 'Noclash') ifTrue: [
		className := (className copyFrom: (className indexOf: $_ )+1 to: className size) asSymbol ].
	anOrderedDictionaryOfClasses at: className ifPresent: [ :existing |
		^existing ].
	answer := self basicNew
		name: className
		superclass: (aClass superclass ifNotNil: [ :sc | self for: sc addClassesTo: anOrderedDictionaryOfClasses ])
		type: aClass typeOfClass
		instVarNames: aClass instVarNames
		classVarNames: aClass classVarNames asArray.
	answer classInstVarNames: aClass class instVarNames.
	
	"Or implement!!"
	self assert: aClass sharedPools isEmpty.
	
	anOrderedDictionaryOfClasses at: className put: answer.
	^answer! !

!LiteralVariableSpec methodsFor: 'accessing' stamp: 'jmv 4/20/2023 16:28:40'!
variableName
	^variableName! !

!LiteralVariableSpec methodsFor: 'testing' stamp: 'jmv 5/16/2023 14:51:47'!
is: aSymbol
	^aSymbol == #LiteralVariableSpec or: [ super is: aSymbol]! !

!LiteralVariableSpec methodsFor: 'private' stamp: 'jmv 4/20/2023 14:17:08'!
variableName: aSymbol
	variableName := aSymbol! !

!LiteralVariableSpec methodsFor: 'printing' stamp: 'fz 5/12/2023 17:25:52'!
printOn: stream

	stream nextPut: $[; nextPutAll: variableName class name ; nextPut: $ ; nextPutAll: 'literal spec -> '; nextPutAll: variableName; nextPut: $]! !

!LiteralVariableSpec class methodsFor: 'instance creation' stamp: 'jmv 4/20/2023 14:17:08'!
variableName: aSymbol
	^self basicNew variableName: aSymbol! !

!MethodSpec methodsFor: 'accessing' stamp: 'jmv 6/30/2023 17:41:22'!
bytecodes
	^bytecodes! !

!MethodSpec methodsFor: 'accessing' stamp: 'fz 6/21/2023 19:57:43'!
classSpec
	^classSpec! !

!MethodSpec methodsFor: 'accessing' stamp: 'jmv 6/30/2023 16:43:19'!
header
	^header! !

!MethodSpec methodsFor: 'accessing' stamp: 'fz 6/23/2023 13:09:41'!
isClassSide
	^isClassSide! !

!MethodSpec methodsFor: 'accessing' stamp: 'jmv 8/31/2023 15:15:25'!
literals
	"Regular literals.
	CompiledMethods have two special last literals, that are not included here.
	They hold #selector, maybe Pragmas or method properties (Associations),
	and #methodClass binding."

	^literals ifNil: [ #() ]! !

!MethodSpec methodsFor: 'accessing' stamp: 'jmv 8/31/2023 15:18:27'!
pragmaSpecs
	^pragmas ifNil: [ #() ]! !

!MethodSpec methodsFor: 'accessing' stamp: 'fz 6/21/2023 22:23:04'!
selector
	^selector! !

!MethodSpec methodsFor: 'accessing' stamp: 'jmv 8/1/2023 16:32:08'!
selector: aSymbol
	selector := aSymbol.! !

!MethodSpec methodsFor: 'accessing' stamp: 'jmv 6/2/2023 16:00:26'!
spurNumLiterals
	"Answer the number of literals used by the receiver.
	See #numLiterals"

	^ header bitAnd: 16r7FFF! !

!MethodSpec methodsFor: 'materialization' stamp: 'jmv 8/10/2023 15:41:49'!
materializeAndInstall
	| theClass method |
	theClass := Smalltalk at: classSpec name.
	isClassSide ifTrue: [ theClass := theClass class ].
	method := self materializeIn: theClass.
	theClass addSelectorSilently: selector withMethod: method.
	^method! !

!MethodSpec methodsFor: 'materialization' stamp: 'jmv 9/6/2023 17:47:21'!
materializeIn: aClass
"
Currently:
	methodTrailer is zeros (No source code pointer)
	Pragmas are supported. Any other method property is not.
classSpec isClassSide selector 
header literals pragmas bytecodes
"
	| bytesCount method j |
	bytesCount := bytecodes size.
	method := CompiledMethod
		createMethod: bytesCount trailer: #(0 0 0 0) header: header.
	1 to: self literals size do: [ :i | | lit varName |
		lit := literals at: i.
		(lit is: #LiteralVariableSpec) ifTrue: [
			varName := lit variableName.
			lit := aClass bindingOf: varName.
			"Create as global if needed."
			lit isNil ifTrue: [
				Smalltalk at: varName put: nil.
				lit := Smalltalk bindingOf: varName]].
		method literalAt: i put: lit].
	method penultimateLiteral: selector.
	self pragmaSpecs do: [ :pragmaData |
		method addPragma: (Pragma
			for: method
			selector: pragmaData first 
			arguments: (pragmaData copyFrom: 2 to: pragmaData size)) ].
	method methodClass: aClass.
	j := 1.
	method initialPC to: method endPC do: [ :i |
		method at: i put: (bytecodes at: j).
		j := j + 1 ].
	^method! !

!MethodSpec methodsFor: 'private' stamp: 'fgz 9/5/2023 15:00:07'!
at: index
	"Compatibility with CompiledMethod"
	^bytecodes at: index! !

!MethodSpec methodsFor: 'private' stamp: 'fgz 9/5/2023 15:00:16'!
at: index put: value
	"Compatibility with CompiledMethod"
	^bytecodes at: index put: value! !

!MethodSpec methodsFor: 'private' stamp: 'jmv 4/12/2023 15:31:52'!
classSpec: aClassSpec isClassSide: aBoolean selector: aSymbol header: aNumber literals: arrayOfLiterals pragmas: arrayOfPragmas bytecodes: aByteArray

	classSpec := aClassSpec.
	isClassSide := aBoolean.
	selector := aSymbol.
	header := aNumber.
	literals := arrayOfLiterals.
	pragmas := arrayOfPragmas.
	bytecodes := aByteArray.! !

!MethodSpec methodsFor: 'printing' stamp: 'jmv 7/17/2023 13:53:02'!
printOn: aStream

	aStream
		nextPutAll: 'MethodSpec:';
		nextPutAll: classSpec name;
		nextPutAll: (isClassSide ifTrue: [ ' class'] ifFalse: ['']);
		nextPutAll: '>>';
		print: selector! !

!MethodSpec methodsFor: 'compatibility with CompiledMethod' stamp: 'jmv 8/23/2023 14:48:38'!
endPC
	"Compatibility with CompiledMethod"
	^bytecodes size! !

!MethodSpec methodsFor: 'compatibility with CompiledMethod' stamp: 'jmv 8/26/2023 17:39:26'!
flushCache
	"NOP here"! !

!MethodSpec methodsFor: 'compatibility with CompiledMethod' stamp: 'jmv 9/6/2023 16:55:16'!
initialPC
	"Compatibility with CompiledMethod"
	^1! !

!MethodSpec methodsFor: 'compatibility with CompiledMethod' stamp: 'jmv 9/6/2023 17:50:13'!
isReturnField
	"Answer whether the receiver is a quick return of an instance variable."
	^ self primitive between: 264 and: 519! !

!MethodSpec methodsFor: 'compatibility with CompiledMethod' stamp: 'jmv 9/6/2023 17:50:17'!
primitive
	"Answer the primitive index associated with the receiver.
	Zero indicates that this is not a primitive method."

	Smalltalk isSpur ifFalse: [ self halt ].
	^(self header anyMask: 65536) "Is the hasPrimitive? flag set?"
		ifTrue: [(self at: 2) + ((self at: 3) bitShift: 8)]
		ifFalse: [0].! !

!MethodSpec methodsFor: 'compatibility with CompiledMethod' stamp: 'jmv 9/6/2023 17:02:08'!
returnField
	"Answer the index of the instance variable returned by a quick return 
	method."
	| prim |
	prim := self primitive.
	prim < 264
		ifTrue: [self error: 'only meaningful for quick-return']
		ifFalse: [^ prim - 264]! !

!MethodSpec class methodsFor: 'instance creation' stamp: 'jmv 8/2/2023 10:28:44'!
for: aCompiledMethod addClassesTo: anOrderedDictionaryOfClasses
	"aCompiledMethod is a regular method that exists in the system
	"
	"
	| methods sortedClassSpecs |
	methods := OrderedCollection new.
	sortedClassSpecs := OrderedDictionary new.
	methods add: (MethodSpec for: AdditionalMethodState >> #selector addClassesTo: sortedClassSpecs).
	methods add: (MethodSpec for: Object>>#copyFrom: addClassesTo: sortedClassSpecs).
	methods add: (MethodSpec for: Object class >> #howToModifyPrimitives addClassesTo: sortedClassSpecs).
	methods add: (MethodSpec for: MethodSpec >> #testMethod addClassesTo: sortedClassSpecs).
	{methods. sortedClassSpecs}
	"

	| selector itsClass |
	"Note: We are currently ignoring any non pragma property"
	self assert: aCompiledMethod properties properties isEmpty.
	selector := aCompiledMethod selector.
	"Remove prefixes such as 'noclash_', 'noclash003_', noclashMySpecialName_', etc "
	(selector beginsWith: 'noclash') ifTrue: [
		selector := (selector copyFrom: (selector indexOf: $_ )+1 to: selector size) asSymbol ].
	itsClass := aCompiledMethod methodClass.
	^self basicNew
		classSpec: (ClassSpec for: itsClass theNonMetaClass addClassesTo: anOrderedDictionaryOfClasses)
		isClassSide: itsClass isMeta
		selector: selector
		header: aCompiledMethod header
		literals: (self literalSpecsFor: aCompiledMethod)
		pragmas: (self pragmaSpecsFor: aCompiledMethod)
		bytecodes: aCompiledMethod bytecodes.! !

!MethodSpec class methodsFor: 'instance creation' stamp: 'jmv 9/4/2023 10:45:32'!
literalSpecsFor: aCompiledMethod
	"Answer an Array with the regular literals. I.e. not including the last two, #penultimateLiteral and the #methodClassAssociation.
	Convert variable bindings (shared Associations in globals, class pool or shared pools) into LiteralVariableSpec"

	| answer s |
	s := aCompiledMethod numLiterals-2.
	s = 0 ifTrue: [ ^nil ].
	answer := Array new: s.
	1 to: answer size do: [ :i | | lit |
		lit := aCompiledMethod literalAt: i.
		(aCompiledMethod isNamedPrimitive and: [i=1]) ifTrue: [
			"The VM stores information about named primitive runs in the last two entries.
			Clear them.
			See #isLinkedNamedPrimitive.
			For additional details, In VMMaker package check references to 
				ExternalCallLiteralFlagsIndex
				ExternalCallLiteralTargetFunctionIndex
			(Apparently -1 means failure, most likely named primitive not found)"
			lit := {lit first. lit second. 0. 0} ].
		(lit isVariableBinding and: [(aCompiledMethod methodClass bindingOf: lit key) == lit]) ifTrue: [
			lit := LiteralVariableSpec variableName: lit key ].
		(MethodSpec classesAssumedToExist includes: lit class name) ifFalse: [
			"self halt:" ('What to do with ', lit class printString) print ].
		answer at: i put: lit ].
	^answer
	
	"
	preguntar isLiteral
	Cualquier imagen minima debera incluir todas las clases que #isLiteral
	Lo que no sea isLiteral o un binding que anda, sera un StreamedLiteralSpec o LiteralObjectSpec o LiteralButNotLiteralObjectSpec...
	O no soportarlo. No sabemos que les puede pasar a esos bichos. si la clase va a estar del otro lado, etc...
	"
"
Posibles candidatas. O fallar.
SHRange
CursorWithMask
Color
TranslucentColor
CharacterSet
TextBackgroundColor
Text + RunArray, NotRunArray, atributos... mhhhhhh
"! !

!MethodSpec class methodsFor: 'instance creation' stamp: 'jmv 8/31/2023 15:20:30'!
pragmaSpecsFor: aCompiledMethod
	"Answer an Array with the information of Pragmas."

	| ps |
	ps := aCompiledMethod pragmas.
	ps size = 0 ifTrue: [ ^nil ].
	^aCompiledMethod pragmas collect: [ :p | Array with: p keyword withAll: p arguments ]! !

!MethodSpec class methodsFor: 'services' stamp: 'jmv 4/20/2023 16:19:58'!
classesAssumedToExist
	"
	MethodSpec classesAssumedToExist collect: [ :clsName | Smalltalk at: clsName ]
	"
	^#(
	ProtoObject
		Object
			UndefinedObject
			Boolean
				True
				False
			Magnitude
				DateAndTime
				Duration
				LookupKey
					Association
				Number
					Float
						BoxedFloat64
						SmallFloat64
					Fraction
					Integer
						LargePositiveInteger
							LargeNegativeInteger
						SmallInteger
			Character
			UnicodeCodePoint
			Collection
				Set
					Dictionary
						IdentityDictionary
							SystemDictionary
						MethodDictionary
				WeakSet
				SequenceableCollection
					ArrayedCollection
						Array
						ByteArray
							CompiledMethod
						FloatArray
							Float32Array
							Float64Array
					CharacterSequence
						String
							Symbol
						UnicodeString
							UnicodeSymbol
					LinkedList
						Semaphore
					OrderedCollection
			Behavior
				ClassDescription
					Class
					Metaclass
			BlockClosure
			Link
				Process
				ProcessorScheduler
			Stream
				DataStream
					ReferenceStream
				PositionableStream
					ReadStream
					WriteStream
						ReadWriteStream
							UniFileStream
				StdIOReadStream
				StdIOWriteStream
			UniStreamReadAdaptor
				ByteStringReadAdaptor
				BytesReadAdaptor
				UnicodeReadAdaptor
			FileSystemEntry
				DirectoryEntry
				FileEntry
			FileIOAccessor
			ClassSpec
			MethodSpec
			LiteralVariableSpec
		)! !

!DynamicCuisLibrary methodsFor: 'creation' stamp: 'jmv 4/25/2023 10:50:23'!
buildClassesAndMethods: aBlock

	| classSpecOrderedDictionary methodSpecOrderedCollection |
	classSpecs := nil.
	methodSpecs := nil.
	classSpecOrderedDictionary := OrderedDictionary new.
	methodSpecOrderedCollection := OrderedCollection new.
	aBlock value: classSpecOrderedDictionary value: methodSpecOrderedCollection.
	classSpecs := classSpecOrderedDictionary values.
	methodSpecs := methodSpecOrderedCollection asArray.! !

!DynamicCuisLibrary methodsFor: 'creation from image' stamp: 'jmv 4/25/2023 14:41:28'!
classes: classesOrNames classSelectors: classSideSelectors instanceSelectors: instanceSideSelectors
	"
	classesOrNames is an Array
	classSideSelectors and instanceSideSelectors are Arrays with same size of classesOrNames.
		Each element is an Array.
	"
	self buildClassesAndMethods: [ :classSpecODict :methodSpecOC |
		classesOrNames with: classSideSelectors with: instanceSideSelectors
					do: [ :classOrName :classSelectors :instanceSelectors | | eachClass |
			eachClass := classOrName isSymbol
				ifTrue: [ Smalltalk at: classOrName ]
				ifFalse: [classOrName ].
			ClassSpec for: eachClass addClassesTo: classSpecODict.
			classSelectors do: [ :selector |
				methodSpecOC add: (MethodSpec
					for: (eachClass class compiledMethodAt: selector)
					addClassesTo: classSpecODict) ].
			instanceSelectors do: [ :selector |
				methodSpecOC add: (MethodSpec
					for: (eachClass compiledMethodAt: selector)
					addClassesTo: classSpecODict) ].
		].
	]! !

!DynamicCuisLibrary methodsFor: 'installing' stamp: 'jmv 9/6/2023 17:49:04'!
canBeInstalled
	classSpecs do: [ :each | each canBeInstalled ifFalse: [ ^false ]].

	"Prepare for possible later validation and conversion of MethodSpecs and existing CompiledMethods"
	classSpecs do: [ :classSpec | classSpec preInstall ].

	"Possible difference between class shapes when the library was built and the shape
	they will have after library install. Check that methods in the library can be corrected for them."
	(self remapMethodSpecInstVarAccesses: false)
		ifFalse: [ ^false ].

	"Possible difference between class shapes in the running image, before and after installing.
	Check that existing methods in the image that would be affected can be corrected for this."
	(self remapExistingMethodsInstVarAccesses: false)
		ifFalse: [ ^false ].

	^true! !

!DynamicCuisLibrary methodsFor: 'installing' stamp: 'jmv 9/6/2023 17:50:56'!
installLibrary
	"Install classes. Merge into existing classes if appropriate."
	classSpecs do: [ :classSpec |
		classSpec equalsExisting ifFalse: [
			classSpec materializeAndInstall ]].

	"Convert existing methods to new class shapes if needed.
	(ClassSpecs are allowed to add new instance variables to existing classes).
	Failure during this phase is catastrophic."
	self remapExistingMethodsInstVarAccesses: true.

	"Convert method specs to the final shape classes will have.
	(Maybe different because of instance variables in the actual classes that
	are reordered or missing in ClassSpecs)"
	self remapMethodSpecInstVarAccesses: true.

	"Install methods"
	methodSpecs do: [ :methodSpec |
		methodSpec materializeAndInstall ].! !

!DynamicCuisLibrary methodsFor: 'installing' stamp: 'jmv 9/6/2023 17:48:49'!
remapExistingMethodsInstVarAccesses: doIt
	"Convert existing methods to new class shapes if needed.
	(ClassSpecs are allowed to add new instance variables to existing classes)
	If doIt is false, then only validate if remapping is possible and answer the result.
	If doIt is true, go ahead and update CompiledMethods. Raise an error if problem arises.
	Warning: Failure during conversion (doIt = true) means an inconsistent system where
		potentially many CompiledMethods access the wrong instance variables.
		It is extremely dangerous to continue. Safest thing is to EXIT THE IMAGE."

	classSpecs do: [ :classSpec | | previousIvarCount delta |
		previousIvarCount := classSpec instVarCountBeforeInstall.
		previousIvarCount notNil ifTrue: [
			delta := classSpec instVarCountAfterInstall - previousIvarCount.
			delta > 0 ifTrue: [
				(Smalltalk at: classSpec name) allSubclassesDo: [ :modifiedShapeClass |
					modifiedShapeClass methodsDo: [ :compiledMethod |
						(CompiledMethod
							displaceInstVarAccessesIn: compiledMethod 
							starting: previousIvarCount+1 by: delta
							doIt: doIt)
								ifFalse: [ ^false ]]]]]].
	doIt ifTrue: [Smalltalk voidCogVMState].
	^true.! !

!DynamicCuisLibrary methodsFor: 'installing' stamp: 'jmv 9/6/2023 17:48:56'!
remapMethodSpecInstVarAccesses: doIt
	"Remap all instance variable accesses in our methodSpecs to match the final shape of classes.
	If doIt is false, then only validate if remapping is possible and answer the result.
	If doIt is true, go ahead and update MethodSpecs. Raise an error if problem arises."

	methodSpecs do: [ :methodSpec |
		methodSpec classSpec methodSpecsInstVarIndexMap ifNotNil: [ :map |
			(CompiledMethod remapInstVarAccessesIn: methodSpec asPer: map doIt: doIt)
				ifFalse: [ ^false ]]].
	^true! !

!DynamicCuisLibrary methodsFor: 'file io' stamp: 'jmv 11/2/2023 10:39:15'!
writeOnFile: fileEntry
	"
	Feature require: 'TestDynamicCuisLibraries'.
	""
	| sourceForLibrary lib |
	sourceForLibrary := NoclashABC_DclExample03.
	lib := DynamicCuisLibrary
		withClasses: { sourceForLibrary }
		classSelectors: { sourceForLibrary class selectors }
		instanceSelectors: { sourceForLibrary selectors }.
	lib writeOnFile: 'DclExample03.dcl' asFileEntry
	"
	fileEntry forceWriteStreamDo: [ :fileStream |
		fileStream binary.
		(ReferenceStream on: fileStream) nextPut: self ].! !

!DynamicCuisLibrary class methodsFor: 'instance creation' stamp: 'jmv 4/24/2023 14:27:42'!
withClasses: arrayOfClasses classSelectors: arrayOfClassSelectors instanceSelectors: arrayOfInstanceSelectors
	"
	DynamicCuisLibrary
		withClasses: #( Point )
		classSelectors: #( #(x:y:) )
		instanceSelectors: #( #( initializePvtX:y: x y #printOn:) ).
	"
	^ self new
		classes: arrayOfClasses
		classSelectors: arrayOfClassSelectors
		instanceSelectors: arrayOfInstanceSelectors! !

!DynamicCuisLibrary class methodsFor: 'file io' stamp: 'jmv 9/4/2023 12:15:41'!
readFromFileNamed: fileName
	"
	(DynamicCuisLibrary readFromFileNamed: 'DclExample03.dcl')
		canBeInstalled
	"
	"que pasa si el file no existe?"
	| bytes |
	bytes := BasicFileServices binaryContentsOf: fileName.
	^(ReferenceStream on: bytes readStream) next! !

!BasicFileServices class methodsFor: 'services' stamp: 'jmv 9/14/2023 10:47:07'!
binaryContentsOf: fileName
	"
	Answer a ByteArray with the complete contents of a file.
	Name must be ASCII. It is just a file name. No path.
	BasicFileServices binaryContentsOf: 'comoVamos.txt'
	"
	| fileID size bytes count |
	fileID := self primOpen: fileName writable: false.
	size := self primSize: fileID.
	bytes := ByteArray new: size.
	count := self primRead: fileID into: bytes startingAt: 1 count: size.
	count ifNil: [
		"Alternatively, answer nil."
		^ self error: 'Could not read file: ', fileName ].
	count < size ifTrue: [ bytes := bytes copyFrom: 1 to: count ].
	self primClose: fileID.
	^bytes! !

!BasicFileServices class methodsFor: 'primitives' stamp: 'jmv 9/4/2023 08:29:31'!
primClose: id
	<primitive: 'primitiveFileClose' module: 'FilePlugin'>
	^nil! !

!BasicFileServices class methodsFor: 'primitives' stamp: 'jmv 9/4/2023 08:29:25'!
primOpen: fileNameAsUtf8Bytes writable: writableFlag
	<primitive: 'primitiveFileOpen' module: 'FilePlugin'>
	^ nil
! !

!BasicFileServices class methodsFor: 'primitives' stamp: 'jmv 9/4/2023 08:29:19'!
primRead: id into: byteArray startingAt: startIndex count: count
	<primitive: 'primitiveFileRead' module: 'FilePlugin'>
	^nil! !

!BasicFileServices class methodsFor: 'primitives' stamp: 'jmv 9/4/2023 08:29:13'!
primSize: id
	<primitive: 'primitiveFileSize' module: 'FilePlugin'>
	^0
! !
