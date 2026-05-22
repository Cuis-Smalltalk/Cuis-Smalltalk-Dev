"
Responsible for creating a new class or changing the format of an existing class (from a class definition in a browser or a fileIn). This includes validating the definition, computing the format of instances, creating or modifying the accompanying Metaclass, setting up the class and metaclass objects themselves, registering the class as a global, recompiling methods, modifying affected subclasses, mutating existing instances to the new format, and more.

You typically only need to use or modify this class, or even know how it works, when making fundamental changes to how the Smalltalk system and language works.

Implementation notes:
ClassBuilder relies on the assumption that it can see ALL subclasses of some class. If there are any existing subclasses of some class, regardless of whether they have instances or not, regardless of whether they are considered obsolete or not, ClassBuilder MUST SEE THEM.

"
Class {
	#name : 'ClassBuilder',
	#superclass : 'Object',
	#instVars : [
		'instVarMap',
		'progress',
		'maxClassIndex',
		'currentClassIndex'
	],
	#classVars : [
		'QuietMode'
	],
	#category : 'Kernel-Classes',
	#package : 'Kernel',
	#tag : 'Classes'
}

{ #category : 'accessing' }
ClassBuilder class >> beSilent: aBool [
	"ClassDefiner beSilent: true"
	"ClassDefiner beSilent: false"
	QuietMode := aBool.
]

{ #category : 'accessing' }
ClassBuilder class >> beSilentDuring: aBlock [
	"Temporarily suppress information about what is going on"
	| wasSilent result |
	wasSilent := self isSilent.
	self beSilent: true.
	result := aBlock value.
	self beSilent: wasSilent.
	^result
]

{ #category : 'cleanup obsolete classes' }
ClassBuilder class >> checkClassHierarchyConsistency [
	"Check the consistency of the class hierarchy. The class hierarchy is consistent if the following
	two logical equivalences hold for classes A and B:
	- B is obsolete and 'B superclass' yields A  <-->  'A obsoleteSubclasses' contains B
	- B is not obsolete and 'B superclass' yields A  <-->  'A subclasses' contains B
	ClassBuilder checkClassHierarchyConsistency
	"
	Utilities informUserDuring: [ :bar |
		self checkClassHierarchyConsistency: bar.
	].
]

{ #category : 'cleanup obsolete classes' }
ClassBuilder class >> checkClassHierarchyConsistency: informer [
	"Check the consistency of the class hierarchy. The class hierarchy is consistent if the following
	two logical equivalences hold for classes A and B:
	- B is obsolete and 'B superclass' yields A  <-->  'A obsoleteSubclasses' contains B
	- B is not obsolete and 'B superclass' yields A  <-->  'A subclasses' contains B"
	| classes |
	Transcript newLine; show: 'Start checking the class hierarchy...'.
	Smalltalk garbageCollect.
	classes := Metaclass allInstances.
	classes keysAndValuesDo: [:index :meta |
		informer value:'Validating class hierarchy ', (index * 100 // classes size) printString,'%'.
		meta allInstances do: [:each | self checkClassHierarchyConsistencyFor: each].
		self checkClassHierarchyConsistencyFor: meta.
	].
	Transcript show: 'OK'.
]

{ #category : 'cleanup obsolete classes' }
ClassBuilder class >> checkClassHierarchyConsistencyFor: aClassDescription [
	"Check whether aClassDescription has a consistent superclass and consistent regular and obsolete
	subclasses"

	| mySuperclass |
	mySuperclass := aClassDescription superclass.
	(mySuperclass subclasses includes: aClassDescription) = aClassDescription isObsolete
			ifTrue: [self error: 'Something wrong!'].
	mySuperclass ifNil: [^ self].  "Obsolete subclasses of nil cannot be stored"
	(mySuperclass obsoleteSubclasses includes: aClassDescription) = aClassDescription isObsolete
			ifFalse: [self error: 'Something wrong!'].

	aClassDescription subclasses do: [:each |
		each isObsolete ifTrue: [self error: 'Something wrong!'].
		each superclass == aClassDescription ifFalse: [self error: 'Something wrong!']
	].
	aClassDescription obsoleteSubclasses do: [:each |
		each isObsolete ifFalse: [self error: 'Something wrong!'].
		each superclass == aClassDescription ifFalse: [self error: 'Something wrong!']
	].
]

{ #category : 'cleanup obsolete classes' }
ClassBuilder class >> cleanupAndCheckClassHierarchy [
	"Makes the class hierarchy consistent and removes obsolete classes from the SystemDictionary.
	Afterwards it checks whether the hierarchy is really consistent.
	ClassBuilder cleanupAndCheckClassHierarchy
	"
	Utilities informUserDuring: [ :bar |
		self cleanupAndCheckClassHierarchy: bar.
	].

]

{ #category : 'cleanup obsolete classes' }
ClassBuilder class >> cleanupAndCheckClassHierarchy: informer [
	"Makes the class hierarchy consistent and removes obsolete classes from the SystemDictionary.
	Afterwards it checks whether the hierarchy is really consistent."

	Transcript newLine; show: '*** Before cleaning up ***'.
	self countReallyObsoleteClassesAndMetaclasses.
	self cleanupClassHierarchy: informer.
	self checkClassHierarchyConsistency: informer.
	Transcript newLine; newLine; show: '*** After cleaning up ***'.
	self countReallyObsoleteClassesAndMetaclasses.
]

{ #category : 'cleanup obsolete classes' }
ClassBuilder class >> cleanupClassHierarchy [
	"Makes the class hierarchy consistent and removes obsolete classes from the SystemDictionary."
	self informUserDuring:[:bar|
		self cleanupClassHierarchy: bar.
	].
]

{ #category : 'cleanup obsolete classes' }
ClassBuilder class >> cleanupClassHierarchy: informer [
	"Makes the class hierarchy consistent and removes obsolete classes from the SystemDictionary."
	| classes |
	Transcript newLine; show: 'Start fixing the class hierarchy and cleaning up...'.
	Smalltalk garbageCollect.
	classes := Metaclass allInstances.
	classes keysAndValuesDo: [:index :meta |
		informer value:'Fixing  class hierarchy ', (index * 100 // classes size) printString,'%'.
		"Check classes before metaclasses (because Metaclass>>isObsolete
		checks whether the related class is obsolete)"
		meta allInstances do: [:each | self cleanupClassHierarchyFor: each].
		self cleanupClassHierarchyFor: meta.
	].
	Transcript show: 'DONE'.
]

{ #category : 'cleanup obsolete classes' }
ClassBuilder class >> cleanupClassHierarchyFor: aClassDescription [
	
	| myName mySuperclass |
	mySuperclass := aClassDescription superclass.
	(self isReallyObsolete: aClassDescription) ifTrue: [
		
		"Remove class >>>from SystemDictionary if it is obsolete"
		myName := aClassDescription name asPlainString.
		Smalltalk keys do: [:each | 
			(each asPlainString = myName and: [(Smalltalk at: each) == aClassDescription])
				ifTrue: [Smalltalk removeKey: each]].

		"Make class officially obsolete if it is not"
		(aClassDescription name asPlainString beginsWith: 'AnObsolete')
			ifFalse: [aClassDescription obsolete].

		aClassDescription isObsolete 
			ifFalse: [self error: 'Something wrong!'].

		"Add class to obsoleteSubclasses of its superclass"
		mySuperclass
			ifNil: [self error: 'Obsolete subclasses of nil cannot be stored'].
		(mySuperclass obsoleteSubclasses includes: aClassDescription)
			ifFalse: [mySuperclass addObsoleteSubclass: aClassDescription].
	] ifFalse:[
		"check if superclass has aClassDescription in its obsolete subclasses"
		mySuperclass ifNil:[mySuperclass := Class]. "nil subclasses"
		mySuperclass removeObsoleteSubclass: aClassDescription.
	].
	"And remove its obsolete subclasses if not actual superclass"
	aClassDescription obsoleteSubclasses do:[:obs|
		obs superclass == aClassDescription ifFalse:[
			aClassDescription removeObsoleteSubclass: obs]].

]

{ #category : 'cleanup obsolete classes' }
ClassBuilder class >> countReallyObsoleteClassesAndMetaclasses [
	"Counting really obsolete classes and metaclasses"

	| metaSize classSize |
	Smalltalk garbageCollect.
	metaSize := self reallyObsoleteMetaclasses size.
	Transcript newLine; show: 'Really obsolete metaclasses: ', metaSize printString.
	classSize := self reallyObsoleteClasses size.
	Transcript newLine; show: 'Really obsolete classes: ', classSize printString; newLine.
	"Metaclasses must correspond to classes!"
	metaSize ~= classSize 
		ifTrue: [self error: 'Serious metalevel inconsistency!!'].
]

{ #category : 'cleanup obsolete classes' }
ClassBuilder class >> isReallyObsolete: aClassDescription [
	"Returns whether the argument class is *really* obsolete. (Due to a bug, the method isObsolete
	isObsolete does not always return the right answer"

	^ aClassDescription isObsolete or: [(aClassDescription superclass subclasses includes: aClassDescription) not]
]

{ #category : 'accessing' }
ClassBuilder class >> isSilent [
	QuietMode ifNil: [
		QuietMode := false ].
	^QuietMode
]

{ #category : 'cleanup obsolete classes' }
ClassBuilder class >> reallyObsoleteClasses [
	| obsoleteClasses |
	obsoleteClasses := OrderedCollection new.
	Metaclass allInstances do: [:meta | meta allInstances do: [:each | 
		(self isReallyObsolete: each) ifTrue: [obsoleteClasses add: each]]].
	^ obsoleteClasses
]

{ #category : 'cleanup obsolete classes' }
ClassBuilder class >> reallyObsoleteMetaclasses [
	^ Metaclass allInstances select: [:each | self isReallyObsolete: each].
]

{ #category : 'cached state access' }
ClassBuilder class >> releaseClassCachedState [
	QuietMode := nil
]

{ #category : 'accessing' }
ClassBuilder class >> reservedNames [

	"Return a list of names that must not be used for variables"

	^#(#self #super #true #false #nil #thisContext)
]

{ #category : 'validation' }
ClassBuilder >> checkInstSize: instSize errorMessage: errorMessage [
		
	instSize > self class maxInstSize ifTrue:
		[self error: errorMessage.
		 ^false].
	Smalltalk isDevelopmentEnvironmentPresent ifTrue:
		[instSize > self class maxAccessibleInstSize ifTrue:
			[^self confirm: 'To access ', instSize printString, ' instance variables\requires the SistaV1 bytecode set.\See #preferredBytecodeSetEncoderClass:.' withNewLines]].
	^true
]

{ #category : 'public' }
ClassBuilder >> class: oldClass instanceVariableNames: instVarString [
	"This is the basic initialization message to change the definition of
	an existing Metaclass"
	oldClass isMeta ifFalse:[^self error: oldClass name, 'is not a Metaclass'].
	^self class: oldClass instanceVariableNames: instVarString unsafe: false
]

{ #category : 'class definition' }
ClassBuilder >> class: oldClass instanceVariableNames: instVarString unsafe: unsafe [
	"This is the basic initialization message to change the definition of
	an existing Metaclass"
	| instVars newClass needNew copyOfOldClass oldClassDefinition |
	instVars := Scanner new scanFieldNames: instVarString.
	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateInstvars: instVars from: oldClass forSuper: oldClass superclass) ifFalse:[^nil].
		(self validateSubclassFormat: oldClass typeOfClass from: oldClass forSuper: oldClass superclass extra: instVars size) ifFalse:[^nil]].
	"See if we need a new subclass or not"
	needNew := self needsSubclassOf: oldClass superclass type: oldClass typeOfClass instanceVariables: instVars from: oldClass.
	needNew ifNil:[^nil]. "some error"
	needNew ifFalse:[^oldClass]. "no new class needed"

	"Create the new class. Include all methods from oldClass recompiled for it."
	oldClassDefinition := oldClass definition.
	copyOfOldClass := oldClass copy.
	newClass := self 
		newSubclassOf: oldClass superclass 
		type: oldClass typeOfClass
		instanceVariables: instVars
		from: oldClass.

	"Please see comment at this method. Also see other senders."
	newClass := self recompileSingleRecursion: false from: oldClass to: newClass.
	newClass isNil ifTrue: [ ^nil ].

	self doneCompiling: newClass.
	SystemChangeNotifier uniqueInstance
		classDefinitionChangedFrom: copyOfOldClass to: newClass oldClassDefinition: oldClassDefinition.
	^newClass
]

{ #category : 'class format' }
ClassBuilder >> computeFormat: type fullInstancesSize: newInstSize [
	"Compute the new format for making oldClass a subclass of newSuper.
	 Answer the format or nil if there is any problem."

	| isVar isPointers isWeak bitsUnitSize |
	type == #compiledMethod ifTrue: [
		newInstSize > 0 ifTrue: [
			self error: 'A compiled method class cannot have named instance variables'.
			^nil].
		^CompiledMethod format].
	(self checkInstSize: newInstSize
		 errorMessage:  'Class has too many instance variables (', newInstSize printString,')') ifFalse: [
		^nil].
	type == #normal ifTrue:[isVar := isWeak := false. isPointers := true].
	type == #bytes ifTrue:[isVar := true. bitsUnitSize := 1. isPointers := isWeak := false].
	type == #shorts ifTrue:[isVar := true. bitsUnitSize := 2. isPointers := isWeak := false].
	type == #words ifTrue:[isVar := true. bitsUnitSize := 4. isPointers := isWeak := false].
	type == #longs ifTrue:[isVar := true. bitsUnitSize := 8. isPointers := isWeak := false].
	type == #variable ifTrue:[isVar := isPointers := true. isWeak := false].
	type == #weak ifTrue:[isVar := isWeak := isPointers := true].
	type == #ephemeron ifTrue:[isVar := false. isWeak := isPointers := true].
	type == #immediate ifTrue:[isVar := isWeak := isPointers := false].
	isVar ifNil: [ self error: 'Unsupported class format type: ', type. ^ nil ].
	(isPointers not and: [newInstSize > 0]) ifTrue: [
		self error: 'A non-pointer class cannot have named instance variables'.
		^nil].
	^self format: newInstSize variable: isVar bitsUnitSize: bitsUnitSize pointers: isPointers weak: isWeak
]

{ #category : 'class format' }
ClassBuilder >> computeFormat: type instSize: newInstSize forSuper: newSuper [
	"Compute the new format for making oldClass a subclass of newSuper.
	Return the format or nil if there is any problem."

	^ self computeFormat: type fullInstancesSize: (newInstSize + (newSuper ifNil: [0] ifNotNil: [newSuper instSize]))
]

{ #category : 'class mutation' }
ClassBuilder >> createHierarchy: oldClass to: newClass [
	"Create a new class hierarchy from the old class and subclasses into newClass and subclasses.
	Recompile all the methods in the newly created classes."

	self showProgressFor: oldClass.
	"Convert the subclasses"
	oldClass subclasses do:[:oldSubclass| | newSubclass |
		"create new class. recompile all methods for it."
		newSubclass := self reshapeClass: oldSubclass toSuper: newClass.
		self createHierarchy: oldSubclass to: newSubclass.
	].
	"And any obsolete ones"
	oldClass obsoleteSubclasses do:[:oldSubclass| | newSubclass |
		oldSubclass ifNotNil:[
			"create new class. recompile all methods for it."
			newSubclass := self reshapeClass: oldSubclass toSuper: newClass.
			self createHierarchy: oldSubclass to: newSubclass.
		].
	].
	^newClass
]

{ #category : 'class definition' }
ClassBuilder >> doneCompiling: aClass [
	"The receiver has finished modifying the class hierarchy.
	Do any necessary cleanup."
	aClass doneCompiling.
]

{ #category : 'class format' }
ClassBuilder >> format: nInstVars variable: isVar bitsUnitSize: bitsUnitSize pointers: isPointers weak: isWeak [

	"Compute the format for the given instance specfication.
	The class format is
		<5 bits inst spec><16 bits inst size>
	where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character, SmallFloat64)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap, WideString)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable (ByteString)
		24-31	= compiled methods (CompiledMethod)"
	| instSpec |
	instSpec := isWeak
					ifTrue:
						[isVar
							ifTrue: [4]
							ifFalse: [5]]
					ifFalse:
						[isPointers
							ifTrue:
								[isVar
									ifTrue: [nInstVars > 0 ifTrue: [3] ifFalse: [2]]
									ifFalse: [nInstVars > 0 ifTrue: [1] ifFalse: [0]]]
							ifFalse:
								[isVar
									ifTrue: [bitsUnitSize caseOf: {
											[1] -> [16].
											[2] -> [12].
											[4] -> [10].
											[8] -> [9] }]
									ifFalse: [7]]].
	^(instSpec bitShift: 16) + nInstVars
]

{ #category : 'private' }
ClassBuilder >> informUserDuring: aBlock [
	self class isSilent ifTrue:[^aBlock value].
	Utilities informUserDuring:[:barBlock|
		progress := barBlock.
		aBlock value].
	progress := nil.
]

{ #category : 'initialization' }
ClassBuilder >> initialize [
	instVarMap := IdentityDictionary new.
]

{ #category : 'class mutation' }
ClassBuilder >> mutate: oldClass to: newClass [
	"Mutate the old class and subclasses into newClass and subclasses.
	Recompile all the methods in the newly created classes."

	self showProgressFor: oldClass.
	"Convert the subclasses"
	oldClass subclasses do:[:oldSubclass| | newSubclass |
		"create new class. recompile all methods for it."
		newSubclass := self reshapeClass: oldSubclass toSuper: newClass.
		self mutate: oldSubclass to: newSubclass.
	].
	"And any obsolete ones"
	oldClass obsoleteSubclasses do:[:oldSubclass| | newSubclass |
		oldSubclass ifNotNil:[
			"create new class. recompile all methods for it."
			newSubclass := self reshapeClass: oldSubclass toSuper: newClass.
			self mutate: oldSubclass to: newSubclass.
		].
	].
	"New class creation with all new methods is done.
	Make newClass thake the place of oldClass in the system: update instances and references."
	self update: oldClass to: newClass.
	^newClass
]

{ #category : 'class mutation' }
ClassBuilder >> mutateInstances: oldClass to: newClass [
	"Make newClass thake the place of oldClass in the system: update instances and references.
	Do this for the whole sub hierarchy."

	self showProgressFor: oldClass.
	"Convert the subclasses"
	oldClass subclasses do: [ :oldSubclass | | newSubclass |
		newSubclass := newClass subclasses detect: [ :c | c name = oldSubclass name ]. 
		self mutateInstances: oldSubclass to: newSubclass.
	].
	"And any obsolete ones"
	oldClass obsoleteSubclasses do: [ :oldSubclass | | newSubclass |
		oldSubclass ifNotNil: [
			newSubclass := newClass subclasses detect: [ :c | c name = oldSubclass name ] ifNone: nil.
			newSubclass ifNotNil: [
				self mutateInstances: oldSubclass to: newSubclass ].
		].
	].
	"Make newClass thake the place of oldClass in the system: update instances and references."
	self update: oldClass to: newClass.
	^newClass
]

{ #category : 'class definition' }
ClassBuilder >> name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category [
	"Define a new class"
	^self 
		name: className 
		subclassOf: newSuper 
		type: type 
		instanceVariableNames: instVarString 
		classVariableNames: classVarString 
		poolDictionaries: poolString 
		category: category
		unsafe: false
]

{ #category : 'class definition' }
ClassBuilder >> name: className subclassOf: newSuper type: type instanceVariableNames: instVarString classVariableNames: classVarString poolDictionaries: poolString category: category unsafe: unsafe [
	"Define a new class.
	If unsafe is true do not run any validation checks.
	This facility is provided to implement important system changes."
	
	| oldClass newClass organization instVars classVars force needNew oldCategory copyOfOldClass newCategory behaviorClasses oldClassDefinition |
	
	instVars := Scanner new scanFieldNames: instVarString.
	classVars := (Scanner new scanFieldNames: classVarString) collect: [:x | x asSymbol].

	"Validate the proposed name"
	unsafe ifFalse:[(self validateClassName: className) ifFalse:[^nil]].
	oldClass := Smalltalk at: className ifAbsent: nil.
	oldClass isBehavior 
		ifFalse:[oldClass := nil]. "Already checked in #validateClassName:"
	oldClassDefinition := oldClass ifNotNil: [ oldClass definition ].
	copyOfOldClass := oldClass copy.

	unsafe ifFalse:[
		"Run validation checks so we know that we have a good chance for recompilation"
		(self validateSuperclass: newSuper forSubclass: oldClass) ifFalse:[^nil].
		(self validateInstvars: instVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateClassvars: classVars from: oldClass forSuper: newSuper) ifFalse:[^nil].
		(self validateSubclassFormat: type from: oldClass forSuper: newSuper extra: instVars size) ifFalse:[^nil]].

	"See if we need a new subclass"
	needNew := self needsSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass.
	needNew ifNil: [^nil]. "some error"

	(needNew and:[unsafe not]) ifTrue:[
		"Make sure we don't redefine any dangerous classes"
		(self tooDangerousClasses includes: oldClass name) ifTrue:[
			self error: oldClass name, ' cannot be changed'.
		].
		"Check if the receiver should not be redefined"
		(oldClass notNil and:[oldClass shouldNotBeRedefined]) ifTrue:[
			self notify: oldClass name asText allBold, 
						' should not be redefined! \Proceed to store over it.' withNewLines]].

	needNew ifTrue:[
		"Create the new class. Include all methods from oldClass recompiled for it."
		newClass := self 
			newSubclassOf: newSuper 
			type: type 
			instanceVariables: instVars
			from: oldClass.
		newClass ifNil: [ ^nil]. "Some error"
		newClass setName: className.
	] ifFalse:[
		"Reuse the old class"
		newClass := oldClass.
	].

	"Install the class variables and pool dictionaries... "
	force := (newClass declareClassVarString: classVarString) | (newClass declareSharedPoolString: poolString).

	"... classify ..."
	organization := Smalltalk organization.
	newCategory := (category isNil or: [ category isEmpty ])
		ifFalse: [category withBlanksTrimmed]
		ifTrue: [organization class defaultClassCategory].
	oldClass ifNotNil: [oldCategory := (organization categoryOfElement: oldClass name) asSymbol].
	organization classify: newClass name under: newCategory.

	"... recompile ..."
	(oldClass notNil and: [oldClass includesBehavior: Behavior])
		ifTrue: [
			"This is only true for Behavior, ClassDescription, Class and Metaclass. Special treatment."
			unsafe ifFalse: [
				behaviorClasses := (oldClass withAllSubclasses select: [ :c | c isMeta not ]) asArray.
				(Processor anyReceiverInStackInstanceOfClassIn: behaviorClasses) ifTrue: [
					self error: oldClass name,' has some subInstance running some CompiledMethod.'.
					^nil ]].
			"Please see comment at this method"
			newClass := self recompileSingleRecursion: force from: oldClass to: newClass ]
		ifFalse: [
			"Other classes"
			newClass := self recompile: force from: oldClass to: newClass mutate: false ].
	newClass isNil ifTrue: [ ^nil ].

	"... export if not yet done ..."
	(Smalltalk at: newClass name ifAbsent: nil) == newClass ifFalse:[
		[Smalltalk at: newClass name put: newClass]
			on: AttemptToWriteReadOnlyGlobal do:[:ex| ex resume: true].
		Smalltalk flushClassNameCache.
	].

	self doneCompiling: newClass.
	
	"... notify interested clients ..."
	oldClass ifNil: [
		SystemChangeNotifier uniqueInstance classAdded: newClass inCategory: newCategory.
		^ newClass].
	SystemChangeNotifier uniqueInstance
		classDefinitionChangedFrom: copyOfOldClass to: newClass oldClassDefinition: oldClassDefinition.
	newCategory ~= oldCategory 
		ifTrue: [SystemChangeNotifier uniqueInstance classRecategorized: newClass from: oldCategory to: newCategory ].
	^newClass
]

{ #category : 'class definition' }
ClassBuilder >> needsSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass [
	"Answer whether we need a new subclass to conform to the requested changes"
	| newFormat |
	"Compute the format of the new class"
	newFormat := 
		self computeFormat: type 
			instSize: instVars size 
			forSuper: newSuper .
	newFormat
		ifNil: [^ nil].

	"Check if we really need a new subclass"
	oldClass ifNil:[^true]. "yes, it's a new class"
	newSuper == oldClass superclass ifFalse:[^true]. "yes, it's a superclass change"
	newFormat = oldClass format ifFalse:[^true]. "yes, it's a format change"
	instVars = oldClass instVarNames ifFalse:[^true]. "yes, it's an iVar change"

	^false

]

{ #category : 'class definition' }
ClassBuilder >> newSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass [
	"Create a new subclass of the given superclass with the given specification."
	| newFormat newClass |
	"Compute the format of the new class"
	newFormat := 
		self computeFormat: type 
			instSize: instVars size 
			forSuper: newSuper.

	newFormat
		ifNil: [^ nil].

	(oldClass == nil or:[oldClass isMeta not]) 
		ifTrue:[newClass := self privateNewSubclassOf: newSuper from: oldClass]
		ifFalse:[newClass := oldClass shallowCopy].

	newClass 
		superclass: newSuper
		methodDictionary: MethodDictionary new
		format: newFormat;
		setInstVarNames: instVars.

	oldClass ifNotNil:[
		newClass organization: oldClass organization.
		"Recompile the new class"
		oldClass hasMethods 
			ifTrue:[newClass compileAllFrom: oldClass].
		self recordClass: oldClass replacedBy: newClass.
	].

	(oldClass == nil or:[oldClass isObsolete not]) 
		ifTrue:[newSuper addSubclass: newClass]
		ifFalse:[newSuper addObsoleteSubclass: newClass].

	^newClass
]

{ #category : 'private' }
ClassBuilder >> privateNewSubclassOf: newSuper [
	"Create a new meta and non-meta subclass of newSuper"
	"WARNING: This method does not preserve the superclass/subclass invariant!"
	| newSuperMeta newMeta |
	newSuperMeta := newSuper ifNil:[Class] ifNotNil:[newSuper class].
	newMeta := Metaclass new.
	newMeta 
		superclass: newSuperMeta 
		methodDictionary: MethodDictionary new 
		format: newSuperMeta format.
	^newMeta new

]

{ #category : 'private' }
ClassBuilder >> privateNewSubclassOf: newSuper from: oldClass [
	"Create a new meta and non-meta subclass of newSuper using oldClass as template"
	"WARNING: This method does not preserve the superclass/subclass invariant!"
	| newSuperMeta oldMeta newMeta |
	oldClass ifNil:[^self privateNewSubclassOf: newSuper].
	newSuperMeta := newSuper ifNil:[Class] ifNotNil:[newSuper class].
	oldMeta := oldClass class.
	newMeta := oldMeta shallowCopy.
	newMeta 
		superclass: newSuperMeta
		methodDictionary: MethodDictionary new
		format: (self computeFormat: oldMeta typeOfClass 
					instSize: oldMeta instVarNames size 
					forSuper: newSuperMeta);
		setInstVarNames: oldMeta instVarNames;
		organization: oldMeta organization.
	"Recompile the meta class"
	oldMeta hasMethods 
		ifTrue:[newMeta compileAllFrom: oldMeta].
	"Record the meta class change"
	self recordClass: oldMeta replacedBy: newMeta.
	"And create a new instance"
	^newMeta adoptInstance: oldClass from: oldMeta
]

{ #category : 'class definition' }
ClassBuilder >> recompile: force from: oldClass to: newClass mutate: forceMutation [
	"Do the necessary recompilation after changing oldClass to newClass.
	If required (e.g., when oldClass ~~ newClass) mutate oldClass to newClass
	and all its subclasses. If forceMutation is true force a mutation even
	if oldClass and newClass are the same."

	| compilationSuccess compilationErrorDescription |
	oldClass
		ifNil: [^ newClass].

	(newClass == oldClass and:[force not and:[forceMutation not]]) ifTrue:[
		^newClass].

	currentClassIndex := 0.
	maxClassIndex := oldClass withAllSubclasses size.

	(oldClass == newClass and:[forceMutation not]) ifTrue:[
		"Recompile from newClass without mutating"
		self informUserDuring:[
			newClass withAllSubclassesDo: [ :cl |
				self showProgressFor: cl.
				cl compileAll]].
		^newClass].

	"Recompile oldClass and subclasses to newClass and subclasses.
	Recompile all the methods in the newly created classes."
	self informUserDuring: [
		compilationSuccess := true.
		[self createHierarchy: oldClass to: newClass] on: Error do: [ :exception |
			"If compilation failed, don't continue!"
			compilationErrorDescription := exception printString.
			compilationSuccess := false ].
	].

	compilationSuccess ifFalse: [
		oldClass superclass removeSubclass: newClass.
		Smalltalk garbageCollect.
		Error signal: compilationErrorDescription, '. Please see Transcript.'.
		^ nil ].

	(newClass canUpdateFrom: oldClass) ifFalse: [
		MethodInCallStackToBecomeInvalid signal ifFalse: [
			oldClass superclass removeSubclass: newClass.
			Smalltalk garbageCollect.
			'Class reshaping aborted.' print.
			Error signal: oldClass name, 
				' has some subInstance running a CompiledMethod that would become invalid. Please see Transcript.'.
			^nil ]].

	"Make newClass thake the place of oldClass in the system: update instances and references.
	Do it for the whole hierarchy."
	self informUserDuring: [
		self mutateInstances: oldClass to: newClass.
	].
	^oldClass "now mutated to newClass"
]

{ #category : 'class definition' }
ClassBuilder >> recompileSingleRecursion: force from: oldClass to: newClass [
	"Based on #recompile:from:to:mutate:
	Older behavior of that method (before January 2022) did a single BFS on the hierarchy tree,
	creating new classes when traveling downwards, and migrating instances before returning upwards.
	This was changed to build the entire hierarchy, then validate with #canUpdateFrom: and only keep
	the new classes, and migrate instances if #canUpdateFrom: answered true.
	This breaks when recompiling the instance side of the Behavior hierarchy (Behavior, ClassDescription,
	Class and Metaclass).
	This method, appropriate for these classes does a single recursion, i.e. the pre-2022 behavior.
	It uses a more restrictive check for existing instances: It will not allow recompilation if any instance is
	executing any method, regardless of it being affected by the reshaping of the class or not. For this
	reason, for other classes, we prefer the more careful check done by #canUpdateFrom:
	
	See senders."

	(newClass == oldClass and: [force not]) ifTrue:[
		^newClass].

	currentClassIndex := 0.
	maxClassIndex := oldClass withAllSubclasses size.

	(oldClass == newClass) ifTrue: [
		"Recompile from newClass without mutating"
		self informUserDuring: [
			newClass withAllSubclassesDo: [ :cl |
				self showProgressFor: cl.
				cl compileAll ]].
		^newClass].

	"Recompile oldClass and subclasses to newClass and subclasses.
	Recompile all the methods in the newly created classes."
	self informUserDuring: [
		self mutate: oldClass to: newClass.
	].
	^oldClass "now mutated to newClass"
]

{ #category : 'private' }
ClassBuilder >> recordClass: oldClass replacedBy: newClass [
	"Keep the changes up to date when we're moving instVars around"

	(instVarMap includesKey: oldClass name) ifTrue:[
		SystemChangeNotifier uniqueInstance
			classDefinitionChangedFrom: oldClass
			to: newClass
			oldClassDefinition: oldClass definition.
	].
]

{ #category : 'private' }
ClassBuilder >> reservedNames [

	"Return a list of names that must not be used for variables"

	^self class reservedNames 
]

{ #category : 'class mutation' }
ClassBuilder >> reshapeClass: oldClass toSuper: newSuper [
	"Reshape the given class to the new super class. Recompile all the methods in the newly created class. Answer the new class."
	| instVars |
	instVars := instVarMap at: oldClass name ifAbsent: [oldClass instVarNames].

	^self newSubclassOf: newSuper 
			type: oldClass typeOfClass 
			instanceVariables: instVars 
			from: oldClass
]

{ #category : 'private' }
ClassBuilder >> showProgressFor: aClass [
	"Announce that we're processing aClass"
	progress
		ifNil: [^ self].
	aClass isObsolete ifTrue:[^self].
	currentClassIndex := currentClassIndex + 1.
	(aClass hasMethods and: [aClass wantsRecompilationProgressReported]) ifTrue: [
		[	"As we might be recompiling the  classes involved in showing progress, ignore errors. After all, they are harmless."
			progress value: ('Recompiling ', aClass name,' (', currentClassIndex printString,'/', maxClassIndex printString,')')
		] on: Error do: nil]
]

{ #category : 'public' }
ClassBuilder >> superclass: aClass [
	ephemeronSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have ephemeron semantics, i.e. where the object will be queued for
	finalization when the key (first) inst var is not reachable other than through
	the other fields of ephemerons with unreachable keys."

	aClass isPointers
		ifFalse: [^ self error: 'cannot make a pointer subclass of a class with non-pointer fields'].
	aClass instSize + f substrings size < 2
		ifTrue: [^ self error: 'cannot make an ephemeron class with less than two named instance varaibles'].
	^self 
		name: t
		subclassOf: aClass
		type: #ephemeron
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
]

{ #category : 'public' }
ClassBuilder >> superclass: aClass [
	immediateSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a
	 new immediate class as a subclass of an existing class."

	aClass instSize > 0
		ifTrue: [^self error: 'cannot make an immediate subclass of a class with named fields'].
	aClass isVariable
		ifTrue: [^self error: 'cannot make an immediate subclass of a class with indexed instance variables'].
	aClass isPointers
		ifFalse: [^self error: 'cannot make an immediate subclass of a class without pointer fields'].

	^self 
		name: t
		subclassOf: aClass
		type: #immediate
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
]

{ #category : 'public' }
ClassBuilder >> superclass: newSuper [
	subclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat 
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class."
	^self 
		name: t
		subclassOf: newSuper
		type: newSuper typeOfClass
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
]

{ #category : 'public' }
ClassBuilder >> superclass: aClass [
	variableByteSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class in which the subclass is to 
	have indexable byte-sized nonpointer variables."
	| oldClassOrNil actualType |
	(aClass instSize > 0)
		ifTrue: [^self error: 'cannot make a byte subclass of a class with named fields'].
	(aClass isVariable and: [aClass isPointers])
		ifTrue: [^self error: 'cannot make a byte subclass of a class with pointer fields'].
	(aClass isVariable and: [aClass isBytes not])
		ifTrue: [^self error: 'cannot make a byte subclass of a class with 16, 32 or 64 bit fields'].
	oldClassOrNil := Smalltalk at: t ifAbsent: nil.
	actualType := (oldClassOrNil notNil
				   and: [oldClassOrNil typeOfClass == #compiledMethod])
					ifTrue: [#compiledMethod]
					ifFalse: [#bytes].
	^self 
		name: t
		subclassOf: aClass
		type: actualType
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
]

{ #category : 'public' }
ClassBuilder >> superclass: aClass [
	variableDoubleByteSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class in which the subclass is to 
	have indexable double-byte-sized (16 bit) nonpointer variables."

	(aClass instSize > 0)
		ifTrue: [^self error: 'cannot make a 16-bit word subclass of a class with named fields'].
	(aClass isVariable and: [aClass isPointers])
		ifTrue: [^self error: 'cannot make a 16-bit word subclass of a class with pointer fields'].
	(aClass isVariable and: [aClass isDoubleBytes not])
		ifTrue: [^self error: 'cannot make a 16-bit word subclass of a class with 8, 32 or 64 bit fields'].

	^self 
		name: t
		subclassOf: aClass
		type: #shorts
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
]

{ #category : 'public' }
ClassBuilder >> superclass: aClass [
	variableDoubleWordSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class in which the subclass is to 
	have indexable double-word-sized (64 bit) nonpointer variables."

	(aClass instSize > 0)
		ifTrue: [^self error: 'cannot make a 64-bit word subclass of a class with named fields'].
	(aClass isVariable and: [aClass isPointers])
		ifTrue: [^self error: 'cannot make a 64-bit word subclass of a class with pointer fields'].
	(aClass isVariable and: [aClass isDoubleWords not])
		ifTrue: [^self error: 'cannot make a 64-bit word subclass of a class with 8, 16 or 32 bit fields'].

	^self 
		name: t
		subclassOf: aClass
		type: #longs
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
]

{ #category : 'public' }
ClassBuilder >> superclass: aClass [
	variableSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class in which the subclass is to 
	have indexable pointer variables."
	aClass isBits 
		ifTrue: [^self error: 'cannot make a pointer subclass of a class with non-pointer fields'].
	^self 
		name: t
		subclassOf: aClass
		type: #variable
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
]

{ #category : 'public' }
ClassBuilder >> superclass: aClass [
	variableWordSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class in which the subclass is to 
	have indexable word-sized (32 bit) nonpointer variables."
	(aClass instSize > 0)
		ifTrue: [^self error: 'cannot make a 32-bit word subclass of a class with named fields'].
	(aClass isVariable and: [aClass isPointers])
		ifTrue: [^self error: 'cannot make a 32-bit word subclass of a class with pointer fields'].
	(aClass isVariable and: [aClass isWords not])
		ifTrue: [^self error: 'cannot make a 32-bit word subclass of a class with 8, 16 or 64 bit fields'].

	^self 
		name: t
		subclassOf: aClass
		type: #words
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
]

{ #category : 'public' }
ClassBuilder >> superclass: aClass [
	weakSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have weak indexable pointer variables."
	aClass isBits 
		ifTrue: [^self error: 'cannot make a pointer subclass of a class with non-pointer fields'].
	^self 
		name: t
		subclassOf: aClass
		type: #weak
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat
]

{ #category : 'private' }
ClassBuilder >> tooDangerousClasses [
	"Return a list of class names which will not be modified in the public interface"
	^#(
		"Object will break immediately"
		ProtoObject Object
		"Contexts and their superclasses"
		InstructionStream ContextPart MethodContext BlockClosure
		"Superclasses of basic collections"
		Collection SequenceableCollection ArrayedCollection
		"Collections known to the VM"
		Array Bitmap String Symbol ByteArray CompiledMethod
		"Basic Numbers"
		Magnitude Number SmallInteger Float
		"Misc other"
		LookupKey Association Link Point Rectangle Behavior PositionableStream UndefinedObject
	)

]

{ #category : 'class mutation' }
ClassBuilder >> update: oldClass to: newClass [
	"Convert oldClass, all its instances and possibly its meta class into newClass,
	 instances of newClass and possibly its meta class. The process is surprisingly
	 simple in its implementation and surprisingly complex in its nuances and potentially
	 bad side effects.
	 We can rely on two assumptions (which are critical):
		#1: The method #updateInstancesFrom: will not create any lasting pointers to
			 'old' instances ('old' is quote on quote since #updateInstancesFrom: will do
			 a become of the old vs. the new instances and therefore it will not create
			 pointers to *new* instances before the #become: which are *old* afterwards)
		#2: The non-preemptive execution of the critical piece of code guarantees that
			 nobody can get a hold by 'other means' (such as process interruption and
			 reflection) on the old instances.
	 Given the above two, we know that after #updateInstancesFrom: there are no pointers
	 to any old instances. After the forwarding become there will be no pointers to the old
	 class or meta class either.
	 Andreas Raab, 2/27/2003 23:42"
	| meta allInstances |
	meta := oldClass isMeta.
	"Note: Everything from here on will run without the ability to get interrupted
	to prevent any other process to create new instances of the old class."
	["Note: The following removal may look somewhat obscure and needs an explanation.
	  When we mutate the class hierarchy we create new classes for any existing subclass.
	  So it may look as if we don't have to remove the old class from its superclass. However,
	  at the top of the hierarchy (the first class we reshape) that superclass itself is not newly
	  created so therefore it will hold both the oldClass and newClass in its (obsolete or not)
	  subclasses. Since the #become: below will transparently replace the pointers to oldClass
	  with newClass the superclass would have newClass in its subclasses TWICE. With rather
	  unclear effects if we consider that we may convert the meta-class hierarchy itself (which
	  is derived from the non-meta class hierarchy).
	  Due to this problem ALL classes are removed from their superclass just prior to converting
	  them. Here, breaking the superclass/subclass invariant really doesn't matter since we will
	  effectively remove the oldClass (becomeForward:) just a few lines below."

		"Convert the instances of oldClass into instances of newClass.
		Note: #canUpdateFrom: has already been called. Instance mutation is safe wrt methods currently in execution."
		allInstances := oldClass allInstances.
		allInstances notEmpty ifTrue: [
			newClass updateInstances: allInstances asArray from: oldClass isMeta: meta ].

		oldClass superclass ifNotNil: [ :oldSuperclass |
			oldSuperclass removeSubclass: oldClass.
			oldSuperclass removeObsoleteSubclass: oldClass ].

		"make sure that the VM cache is clean"
		oldClass methodDict do: [:cm | cm flushCache].

		meta
			ifTrue: [
				oldClass becomeForward: newClass.
				oldClass updateMethodBindingsTo: oldClass binding]
			ifFalse: [
				{oldClass. oldClass class} elementsForwardIdentityTo: {newClass. newClass class}.
				oldClass updateMethodBindingsTo: oldClass binding.
				oldClass class updateMethodBindingsTo: oldClass class binding].

		"eem 5/31/2014 07:22 At this point there used to be a garbage collect whose purpose was
		 to ensure no old instances existed after the becomeForward:.  Without the GC it was possible
		 to resurrect old instances using e.g. allInstancesDo:.  This was because the becomeForward:
		 updated references from the old objects to new objects but didn't destroy the old objects.
		 But as of late 2013/early 2014 becomeForward: has been modified to free all the old objects."
	] valueUnpreemptively
]

{ #category : 'validation' }
ClassBuilder >> validateClassName: aSymbol [
	"Validate the new class name"

	aSymbol isValidGlobalName ifFalse: [
		self error: 'Invalid class name'.
		^false].
	Smalltalk at: aSymbol ifPresent:[:old|
		(old isKindOf: Behavior) ifFalse:[
			self notify: aSymbol asText allBold, 
						' already exists!\Proceed will store over it.' withNewLines]].
	^true
]

{ #category : 'validation' }
ClassBuilder >> validateClassvars: classVarArray from: oldClass forSuper: newSuper [
	"Check if any of the classVars of oldClass conflict with the new superclass"
	| usedNames classVars temp |
	classVarArray isEmpty ifTrue:[^true]. "Okay"

	"Validate the class var names"
	usedNames := classVarArray asSet.
	usedNames size = classVarArray size 
		ifFalse:[	classVarArray do:[:var|
					usedNames remove: var ifAbsent:[temp := var]].
				self error: temp,' is multiply defined'. ^false].
	(usedNames includesAnyOf: self reservedNames) 
		ifTrue:[	self reservedNames do:[:var|
					(usedNames includes: var) ifTrue:[temp := var]].
				self error: temp,' is a reserved name'. ^false].

	newSuper ifNotNil: [
		usedNames := newSuper allClassVarNames asSet.
		classVarArray do:[:iv|
			(usedNames includes: iv) ifTrue:[
				newSuper withAllSuperclassesDo:[:cl|
					(cl classVarNames includes: iv) ifTrue:[temp := cl]].
				self error: iv, ' is already defined in ', temp name.
				^false]]].

	oldClass ifNotNil: [
		usedNames := Set new.
		oldClass allSubclassesDo:[:cl| usedNames addAll: cl classVarNames].
		classVars := classVarArray.
		newSuper ifNotNil: [ classVars := classVars, newSuper allClassVarNames asArray].
		classVars do: [ :iv |
			(usedNames includes: iv) ifTrue: [
				self error: iv, ' is already defined in a subclass of ', oldClass name.
				^false]]].
	^true
]

{ #category : 'validation' }
ClassBuilder >> validateInstvars: instVarArray from: oldClass forSuper: newSuper [
	"Check if any of the instVars of oldClass conflict with the new superclass"
	| instVars usedNames temp |
	instVarArray isEmpty ifTrue:[^true]. "Okay"
	newSuper allowsSubInstVars ifFalse: [
		self error: newSuper printString, ' does not allow subclass inst vars. See allowsSubInstVars.'. ^ false].

	"Validate the inst var names"
	usedNames := instVarArray asSet.
	usedNames size = instVarArray size 
		ifFalse:[	instVarArray do:[:var|
					usedNames remove: var ifAbsent:[temp := var]].
				self error: temp,' is multiply defined'. ^false].
	(usedNames includesAnyOf: self reservedNames) 
		ifTrue:[	self reservedNames do:[:var|
					(usedNames includes: var) ifTrue:[temp := var]].
				self error: temp,' is a reserved name'. ^false].

	newSuper ifNotNil: [
		usedNames := newSuper allInstVarNames asSet.
		instVarArray do:[:iv|
			(usedNames includes: iv) ifTrue:[
				newSuper withAllSuperclassesDo:[:cl|
					(cl instVarNames includes: iv) ifTrue:[temp := cl]].
				self error: iv,' is already defined in ', temp name.
				^false]]].
	oldClass ifNotNil: [
		usedNames := Set new.
		oldClass allSubclassesDo:[:cl| usedNames addAll: cl instVarNames].
		instVars := instVarArray.
		newSuper ifNotNil: [instVars := instVars, newSuper allInstVarNames].
		instVars do:[:iv|
			(usedNames includes: iv) ifTrue:[
				self error: iv, ' is already defined in a subclass of ', oldClass name.
				^false]]].
	^true
]

{ #category : 'validation' }
ClassBuilder >> validateSubclass: subclass canKeepLayoutFrom: oldClass forSubclassFormat: newType  [
	"Returns whether the immediate subclasses of oldClass can keep its layout"
	"Note: Squeak does not appear to model classFormat relationships.. so I'm putting some logic here. bkv 4/2/2003"
	
	"Only run this test for a real subclass - otherwise this prevents changing
	a class from #subclass: to #variableSubclass: etc."
	subclass = oldClass ifTrue:[^true].

	 "isWeak implies isVariant"					
	 (oldClass isVariable and: [ subclass isWeak ])
		ifFalse: [ "In general we discourage format mis-matches"
				  (subclass typeOfClass == newType) 
				   	ifFalse: [ self error: subclass name,' cannot be recompiled'.
							  ^ false ]].
	^ true
]

{ #category : 'validation' }
ClassBuilder >> validateSubclassFormat: newType from: oldClass forSuper: newSuper extra: newInstSize [
	"Validate the # of instVars and the format of the subclasses"
	| deltaSize errorMessage |
	oldClass ifNil: [^ true]. "No subclasses"
	"Compute the # of instvars needed for all subclasses"
	deltaSize := newInstSize.
	oldClass
		ifNotNil: [deltaSize := deltaSize - oldClass instVarNames size].
	newSuper
		ifNotNil: [deltaSize := deltaSize + newSuper instSize].
	(oldClass notNil and: [oldClass superclass notNil]) 
		ifTrue: [deltaSize := deltaSize - oldClass superclass instSize].
	errorMessage := 'more than ', self class maxInstSize printString, ' instance variables'.
	oldClass
		ifNil: [^self checkInstSize: deltaSize errorMessage: errorMessage capitalized].

	oldClass withAllSubclassesDo:
		[ :sub |
		(self
			checkInstSize: sub instSize + deltaSize
			errorMessage: sub name,' has ', errorMessage) ifFalse:
			[^false].

		"If we get this far, check whether the immediate subclasses of oldClass can keep its layout."
		(newType ~~ #normal) 
			ifTrue: [ self validateSubclass: sub canKeepLayoutFrom: oldClass forSubclassFormat: newType ]].

	^ true
]

{ #category : 'validation' }
ClassBuilder >> validateSuperclass: aSuperClass forSubclass: aClass [
	"Check if it is okay to use aSuperClass as the superclass of aClass"
	aClass ifNil: [ "New class"
		(aSuperClass == nil or:[aSuperClass isBehavior and:[aSuperClass isMeta not]])
			ifFalse:[self error: aSuperClass name,' is not a valid superclass'.
					^false].
		^true].
	aSuperClass == aClass superclass ifTrue:[^true]. "No change"
	(aClass isMeta) "Not permitted - meta class hierarchy is derived from class hierarchy"
		ifTrue:[^self error: aClass name, ' must inherit from ', aClass superclass name].
	"Check for circular references"
	(aSuperClass notNil and:[aSuperClass == aClass or:[aSuperClass inheritsFrom: aClass]])
		ifTrue:[self error: aSuperClass name,' inherits from ', aClass name.
				^false].
	^true
]
