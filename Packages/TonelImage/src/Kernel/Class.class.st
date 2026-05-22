"
I add a number of facilities to those in ClassDescription:
	A set of all my subclasses (defined in ClassDescription, but only used here and below)
	A name by which I can be found in a SystemDictionary
	A classPool for class variables shared between this class and its metaclass
	A list of sharedPools which probably should be supplanted by some better mechanism.

My instances describe the representation and behavior of objects. I add more comprehensive programming support facilities to the basic attributes of Behavior and the descriptive facilities of ClassDescription.

The slot 'subclasses' is a redundant structure.  It is never used during execution, but is used by the development system to simplify or speed certain operations.  
"
Class {
	#name : 'Class',
	#superclass : 'ClassDescription',
	#instVars : [
		'subclasses',
		'name',
		'classPool',
		'sharedPools'
	],
	#category : 'Kernel-Classes',
	#package : 'Kernel',
	#tag : 'Classes'
}

{ #category : 'instance creation' }
Class class >> template: aSystemCategoryName  [
	"Answer an expression that can be edited and evaluated in order to define a new class."

	^ self templateForSubclassOf: Object category: aSystemCategoryName 
]

{ #category : 'instance creation' }
Class class >> templateForSubclassOf: priorClass category: systemCategoryName  [
	"Answer an expression that can be edited and evaluated in order to define a new class, given that the class previously looked at was as given"

	^ priorClass name asPlainString, ' subclass: #NameOfSubclass
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''' , systemCategoryName asPlainString , ''''
]

{ #category : 'class variables' }
Class >> addClassVarName: aString [
	"Add the argument, aString, as a class variable of the receiver.
	Signal an error if the first character of aString is not capitalized,
	or if it is already a variable named in the class."
	| symbol oldState oldDefinition |
	oldDefinition := self definition.
	oldState := self copy.
	aString first isLowercase
		ifTrue: [^self error: aString, ' class variable name should be capitalized; proceed to include anyway.'].
	symbol := aString asSymbol.
	self withAllSubclasses do: 
		[:subclass | 
		(subclass bindingOf: symbol) ifNotNil:[
			^ self error: aString 
				, ' is already used as a variable name in class ' 
				, subclass name]].
	classPool ifNil: [classPool := Dictionary new].
	(classPool includesKey: symbol) ifFalse: [
		"Pick up any refs in Undeclared"
		classPool declare: symbol from: Undeclared.
		SystemChangeNotifier uniqueInstance
			classDefinitionChangedFrom: oldState
			to: self
			oldClassDefinition: oldDefinition ].
]

{ #category : 'instance variables' }
Class >> addInstVarName: aString [
	"Add the argument, aString, as one of the receiver's instance variables."
	
	| answer |
	answer := ClassBuilder new
		name: self name
		subclassOf: superclass
		type: self typeOfClass
		instanceVariableNames: self instanceVariablesString, ' ', aString
		classVariableNames: self classVariablesString
		poolDictionaries: self sharedPoolsString
		category: self category.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer

]

{ #category : 'pool variables' }
Class >> addSharedPool: aSharedPool  [
	"Add the argument, aSharedPool, as one of the receiver's shared pools. 
	Create an error if the shared pool is already one of the pools.
	This method will work with shared pools that are plain Dictionaries or thenewer SharedPool subclasses"

	(self sharedPools includes: aSharedPool)
		ifTrue: [^self error: 'This is already in my shared pool list'].
	sharedPools := sharedPools
		ifNil: [Array with: aSharedPool]
		ifNotNil: [sharedPools copyWith: aSharedPool]
]

{ #category : 'initialization' }
Class >> addSubclass: aSubclass [
	"Make the argument, aSubclass, be one of the subclasses of the receiver. 
	Create an error notification if the argument's superclass is not the receiver."

	aSubclass superclass ~~ self
		ifTrue: [^self error: aSubclass name , ' is not my subclass'].
	subclasses ifNil: [
		subclasses := Array with: aSubclass.
		^self].
	subclasses do: [ :cl | cl == aSubclass ifTrue: [ ^self ]]. "Already my subclass"
	subclasses := Array withAll: subclasses with: aSubclass.
]

{ #category : 'class variables' }
Class >> allClassVarNames [
	"Answer a Set of the names of the receiver's class variables, including those
	defined in the superclasses of the receiver."

	| aSet |
	superclass
		ifNil: [^ self classVarNames].	 "This is the keys so it is a new Set."
	aSet := superclass allClassVarNames.
	aSet addAll: self classVarNames.
	^ aSet
]

{ #category : 'pool variables' }
Class >> allSharedPools [
	"Answer a collection of the pools the receiver shares, including those defined  
	in the superclasses of the receiver."

	^ superclass
		ifNil: [ self sharedPools copy ]
		ifNotNil: [ superclass allSharedPools, self sharedPools ].
]

{ #category : 'enumerating' }
Class >> allSubclassesSortedDo: aBlock [
	"Evaluate the argument, aBlock, for each of the receiver's subclasses.
	At each level, sort subclasses."

	self subclassesSortedDo: [ :cl | 
		aBlock value: cl.
		cl allSubclassesSortedDo: aBlock ].
]

{ #category : 'accessing' }
Class >> binding [

	^ Smalltalk associationAt: name ifAbsent: [nil -> self]
]

{ #category : 'accessing' }
Class >> bindingOf: varName [
	"Answer the binding of some variable resolved in the scope of the receiver"
	| aSymbol |
	aSymbol := varName asSymbol.

	"First look in receiver, and up the hierarchy. Inherited variables take precedence over globals."
	(self localBindingOf: aSymbol) ifNotNil: [ :binding | ^binding ].

	"Next look in globals."
	(Smalltalk bindingOf: aSymbol) ifNotNil: [ :binding | ^binding ].

	"Fail at the end."
	^nil
]

{ #category : 'class variables' }
Class >> classPool [
	"Answer the dictionary of class variables."

	^classPool
		ifNil: [Dictionary new]
]

{ #category : 'class variables' }
Class >> classPool: aDictionary [
	classPool := aDictionary
]

{ #category : 'class variables' }
Class >> classPoolFrom: aClass [
	"share the classPool with aClass."

	classPool := aClass classPool
]

{ #category : 'accessing' }
Class >> classVarBindingOf: varNameSymbol [

	^self classPool bindingOf: varNameSymbol
]

{ #category : 'class variables' }
Class >> classVarNames [
	"Answer a Set of the names of the class variables defined in the receiver."

	^self classPool keys asSet
]

{ #category : 'class variables' }
Class >> classVarValueOf: varNameSymbol [

	^(self classPool bindingOf: varNameSymbol) ifNotNil: [:binding | binding value ]
]

{ #category : 'compiling' }
Class >> compileAllFrom: oldClass [
	"Recompile all the methods in the receiver's method dictionary (not the
	subclasses). Also recompile the methods in the metaclass."

	super compileAllFrom: oldClass.
	self class compileAllFrom: oldClass class
]

{ #category : 'copying' }
Class >> copy  [
	"Answer a copy of the receiver without a list of subclasses."
	| newClass |
	newClass := self class copy new
		superclass: superclass
		methodDict: self methodDict copy
		format: format
		name: name
		organization: self organization copy
		instVarNames: instanceVariables copy
		classPool: classPool copy
		sharedPools: sharedPools copy.
	Class instSize+1 to: self class instSize do:
		[:offset | newClass instVarAt: offset put: (self instVarAt: offset)].
	^ newClass
]

{ #category : 'initialization' }
Class >> declareClassVarString: aString [
	"Declare class variables common to all instances.
	Answer whether recompilation is advisable."

	| varNames |
	varNames := (Scanner new scanFieldNames: aString) collect: [ :x | x asSymbol ].
	varNames do: [ :varName |
		varName first isLowercase ifTrue: [
			self error: varName, ' class variable name should be capitalized; proceed to include anyway.' ]].
	^self declareClassVariables: varNames.
]

{ #category : 'initialization' }
Class >> declareClassVariables: newVars [
	"Declare class variables common to all instances.
	Answer whether recompilation is advisable."

	| conflicts |
	conflicts := false.
	classPool
		ifNotNil: [
			(classPool keys reject: [ :x | newVars includes: x ]) 
				do: [ :var | self removeClassVarName: var ]].
	(newVars reject: [:var | self classPool includesKey: var])
		do: [ :var | "adding"
			"check if new vars defined elsewhere"
			(self bindingOf: var)
				ifNotNil: [ 
					self error: var, ' is defined elsewhere'.
					conflicts := true ]].
	newVars size > 0
		ifTrue: [
			"in case it was nil"
			classPool := self classPool.
			newVars do: [:var |
				classPool declare: var from: 
					(Smalltalk at: #Undeclared ifAbsent: nil) ]].
	^ conflicts.
]

{ #category : 'initialization' }
Class >> declareSharedPoolString: poolString [
	"Set up sharedPools. Answer whether recompilation is advisable."

	^self declareSharedPools: (Scanner new scanFieldNames: poolString)
]

{ #category : 'initialization' }
Class >> declareSharedPools: sharedPoolNames [
	"Set up sharedPools. Answer whether recompilation is advisable."
	| oldPools s |
	oldPools := self sharedPools.
	s := sharedPoolNames size.
	s > 0 ifTrue: [
		sharedPools := Array new: s.
		sharedPoolNames withIndexDo: [ :poolName :i | | poolDict |
			poolDict := Smalltalk at: poolName asSymbol ifAbsent: [
				(PoolDefinitionNotification signalNamed: poolName)
					ifTrue: [ Smalltalk at: poolName asSymbol put: Dictionary new ]
					ifFalse: [^self error: poolName,' does not exist']].
			sharedPools at: i put: poolDict ]]
	ifFalse: [
		sharedPools := nil ].
	^oldPools anySatisfy: [ :pool |
		self sharedPools noneSatisfy: [ :p | p == pool ]].
]

{ #category : 'fileIn/Out' }
Class >> definitionReplacingCategoryWith: aNewCategory [

	| definition categoryDefinitionIndex currentCategoryDefinition definitionWithNewCategory |
	
	definition := self definition.
	"category can be nil, that is why I sent asString to it - Hernan"
	currentCategoryDefinition := 'category: ''', self category asString, ''''.
	categoryDefinitionIndex := definition 
		indexOfSubCollection: currentCategoryDefinition 
		startingAt: 1
		ifAbsent: [ self error: 'Definition of category not found!' ].
	
	definitionWithNewCategory := definition first: categoryDefinitionIndex - 1.
	definitionWithNewCategory := definitionWithNewCategory, 'category: ''', aNewCategory asString, ''''.
	
	^definitionWithNewCategory		
]

{ #category : 'class variables' }
Class >> ensureClassPool [

	classPool ifNil: [classPool := Dictionary new].
]

{ #category : 'subclass creation' }
Class >> ephemeronSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat  [
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have ephemeron semantics, i.e. where the object will be queued for
	finalization when the key (first) inst var is not reachable other than through
	the other fields of ephemerons with unreachable keys."

	| answer |
	answer := ClassBuilder new
			superclass: self
			ephemeronSubclass: t
			instanceVariableNames: f
			classVariableNames: d
			poolDictionaries: s
			category: cat.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer
]

{ #category : 'fileIn/Out' }
Class >> fileOut [
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Method ordering is by method categories, and in each category, alphabetical."

	DirectoryEntry fileOutsDirQuery: (self name, '.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
		self fileOutOn: fileStream moveSource: false toFile: 0 ].
]

{ #category : 'fileIn/Out' }
Class >> fileOutAlphabetically [
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Method ordering is alphabetical, igoring categories."

	DirectoryEntry fileOutsDirQuery: (self name, '.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
		self fileOutOn: fileStream moveSource: false toFile: 0 initializing: true sortMethodsByCategory: false ].
]

{ #category : 'fileIn/Out' }
Class >> fileOutHierarchy [
	"File a description of the receiver onto a new file whose base name is the name of the receiver.
	Morph fileOutHierarchy
	"
	DirectoryEntry fileOutsDirQuery: (self name, '-hierarchy.st') writeStreamDo: [ :fileStream |
		fileStream writeSystemTimeStamp.
		self sharedPools size > 0 ifTrue: [
			self shouldFileOutPools
				ifTrue: [ self fileOutSharedPoolsOn: fileStream ]].
		self withAllSubclassesDo: [ :each |
			each fileOutOn: fileStream moveSource: false toFile: 0 ]].
]

{ #category : 'fileIn/Out' }
Class >> fileOutInitializerOn: aStream [
	^self class fileOutInitializerOn: aStream
]

{ #category : 'fileIn/Out' }
Class >> fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: aBool sortMethodsByCategory: byCategory [
	"File a description of the receiver on aFileStream. If the boolean argument,
	moveSource, is true, then set the trailing bytes to the position of aFileStream and
	to fileIndex in order to indicate where to find the source code."

	Transcript newLine; show: name.
	super
		fileOutOn: aFileStream
		moveSource: moveSource
		toFile: fileIndex
		initializing: nil
		sortMethodsByCategory: byCategory.
	self class nonTrivial
		ifTrue: [
			aFileStream newLine; nextPutAll: '"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!'; newLine; newLine.
			self class
				fileOutOn: aFileStream
				moveSource: moveSource
				toFile: fileIndex
				initializing: aBool
				sortMethodsByCategory: byCategory ].
]

{ #category : 'fileIn/Out' }
Class >> fileOutPool: aPool onFileStream: aFileStream  [

	| aPoolName |
	(aPool  isKindOf: SharedPool class) ifTrue:[^self notify: 'we do not fileout SharedPool type shared pools for now'].
	aPoolName := Smalltalk keyAtIdentityValue: aPool.
	Transcript newLine; show: aPoolName.
	aFileStream nextPutAll: 'Transcript show: ''' , aPoolName , '''; newLine!'; newLine.
	aFileStream nextPutAll: 'Smalltalk at: #' , aPoolName , ' put: Dictionary new!'; newLine.
	aPool keys sort do: [ :aKey | | aValue |
		aValue := aPool at: aKey.
		aFileStream nextPutAll: aPoolName , ' at: #''' , aKey asString , '''', ' put:  '.
		aValue isNumber
			ifTrue: [ aValue printOn: aFileStream ]
			ifFalse: [
				aFileStream nextPutAll: '('.
				aValue printOn: aFileStream.
				aFileStream nextPutAll: ')' ].
		aFileStream nextPutAll: '!'; newLine ].
	aFileStream newLine
]

{ #category : 'fileIn/Out' }
Class >> fileOutSharedPoolsOn: aFileStream [
	"file out the shared pools of this class after prompting the user about each pool"
	| poolsToFileOut |
	poolsToFileOut := self sharedPools select: 
		[:aPool | (self shouldFileOutPool: (Smalltalk keyAtIdentityValue: aPool))].
	poolsToFileOut do: [:aPool | self fileOutPool: aPool onFileStream: aFileStream].
	
]

{ #category : 'testing' }
Class >> hasMethods [
	"Answer a Boolean according to whether any methods are defined for the 
	receiver (includes whether there are methods defined in the receiver's 
	metaclass)."

	^super hasMethods or: [self class hasMethods]
]

{ #category : 'accessing class hierarchy' }
Class >> hasSubclasses [

	^ subclasses notNil and: [ subclasses notEmpty ]
]

{ #category : 'subclass creation' }
Class >> immediateSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat  [
	"This is the standard initialization message for creating a new
	 immediate class as a subclass of an existing class (the receiver)."

	| answer |
	answer := ClassBuilder new
			superclass: self
			immediateSubclass: t
			instanceVariableNames: ''
			classVariableNames: d
			poolDictionaries: s
			category: cat.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer
]

{ #category : 'testing' }
Class >> isObsolete [
	"Return true if the receiver is obsolete."
	^(Smalltalk at: name ifAbsent: nil) ~~ self
]

{ #category : 'accessing' }
Class >> localBindingOf: varNameSymbol [
	"Answer the binding of some variable resolved in the scope of the receiver."

	"First look in classVar dictionary."
	(self classVarBindingOf: varNameSymbol) ifNotNil: [ :binding | ^binding ].

	"Next look in shared pools."
	self sharedPools do: [ :pool | 
		(pool bindingOf: varNameSymbol) ifNotNil: [ :binding | ^binding ].
	].

	"Finally look higher up the superclass chain and fail at the end."
	^superclass ifNotNil: [ superclass localBindingOf: varNameSymbol ]
]

{ #category : 'accessing' }
Class >> name [
	"Answer the name of the receiver."

	^name ifNil: [super name]
]

{ #category : 'fileIn/Out' }
Class >> objectForDataStream: refStrm [

	"I am about to be written on an object file.  Write a reference to a class in Smalltalk instead."

	^ DiskProxy global: self theNonMetaClass name selector: #yourself args: #()
]

{ #category : 'release' }
Class >> obsolete [
	"Change the receiver and all of its subclasses to an obsolete class."
	self == Object 
		ifTrue:[^self error:'Object is NOT obsolete'].
	name := 'AnObsolete' , name.
	Object class instSize + 1 to: self class instSize do:
		[:i | self instVarAt: i put: nil]. "Store nil over class instVars."
	classPool := nil.
	sharedPools := nil.
	self class obsolete.
	super obsolete.

]

{ #category : 'compiling' }
Class >> possibleVariablesFor: misspelled continuedFrom: oldResults [

	| results |
	results := misspelled correctAgainstDictionary: self classPool continuedFrom: oldResults.
	self sharedPools do: [:pool | 
		results := misspelled correctAgainstDictionary: pool continuedFrom: results ].
	^ superclass
		ifNil: [misspelled correctAgainstDictionary: Smalltalk continuedFrom: results]
		ifNotNil: [superclass possibleVariablesFor: misspelled continuedFrom: results]
]

{ #category : 'printing' }
Class >> printNameOn: aStream [
	name notNil
		ifTrue: [ aStream nextPutAll: name ]
		ifFalse: [ super printNameOn: aStream ]
]

{ #category : 'fileIn/Out' }
Class >> reformatAll  [
	"Reformat all methods in this class.
	Leaves old code accessible to version browsing"
	super reformatAll.		"me..."
	self class reformatAll	"...and my metaclass"
]

{ #category : 'class variables' }
Class >> removeClassVarName: aString [
	"Remove the class variable whose name is the argument, aString, from
	the names defined in the receiver, a class. Create an error notification if
	aString is not a class variable or if it is still being used in the code of
	the class."

	| aSymbol |
	aSymbol := aString asSymbol.
	(classPool isNil or: [(classPool includesKey: aSymbol) not])
		ifTrue: [ ^self error: aString, ' is not a class variable'].
	self withAllSubclasses do:[:subclass |
		(Array with: subclass with: subclass class) do: [ :classOrMeta |
			(classOrMeta whichSelectorsReferTo: (classPool associationAt: aSymbol))
				isEmpty ifFalse: [
					InMidstOfFileinNotification signal ifTrue: [
						Transcript newLine; show: self name, ' (' , aString , ' is Undeclared) '.
						^Undeclared declare: aSymbol from: classPool ].
					(self confirm: (aString,' is still used in code of class ', classOrMeta name,
						'.\Is it okay to move it to Undeclared?') withNewLines)
						ifTrue: [ ^Undeclared declare: aSymbol from: classPool ]
						ifFalse: [ ^self ]]]].
	classPool removeKey: aSymbol.
	classPool isEmpty ifTrue: [ classPool := nil ]
]

{ #category : 'release' }
Class >> removeFromSystem [
	"Forget the receiver from the Smalltalk global dictionary. Any existing 
	instances will refer to an obsolete version of the receiver."
	self removeFromSystem: true.
]

{ #category : 'release' }
Class >> removeFromSystem: logged [
	"Forget the receiver from the Smalltalk global dictionary. Any existing
	instances will refer to an obsolete version of the receiver."

	"keep the class name and category for triggering the system change message. If we wait to long, then we get obsolete information which is not what we want."

	"tell class to unload itself"
	self unload.
	self superclass ifNotNil: [:aSuperclass |
		"If we have no superclass there's nothing to be remembered"
		aSuperclass addObsoleteSubclass: self].
	Smalltalk forgetClass: self logged: logged.
	self obsolete.
]

{ #category : 'release' }
Class >> removeFromSystemUnlogged [
	"Forget the receiver from the Smalltalk global dictionary. Any existing instances will refer to an obsolete version of the receiver.  Do not log the removal either to the current change set nor to the system changes log"
	^self removeFromSystem: false
]

{ #category : 'instance variables' }
Class >> removeInstVarName: aString  [
	"Remove the argument, aString, as one of the receiver's instance variables."

	| newInstVarString answer |
	(self instVarNames includes: aString)
		ifFalse: [self error: aString , ' is not one of my instance variables'].
	newInstVarString := ''.
	(self instVarNames copyWithout: aString) do: 
		[:varName | newInstVarString := newInstVarString , ' ' , varName].
	answer := ClassBuilder new
		name: self name
		subclassOf: superclass
		type: self typeOfClass
		instanceVariableNames: newInstVarString
		classVariableNames: self classVariablesString
		poolDictionaries: self sharedPoolsString
		category: self category.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer
]

{ #category : 'pool variables' }
Class >> removeSharedPool: aDictionary [
	"Remove the pool dictionary, aDictionary, as one of the receiver's pool
	dictionaries. Create an error notification if the dictionary is not one of
	the pools.
	: Note that it removes the wrong one if there are two empty Dictionaries in the list."

	| satisfiedSet workingSet aSubclass |

	(sharedPools isNil or: [(sharedPools includes: aDictionary) not ])
		ifTrue: [^self error: 'the dictionary is not in my pool'].

	"first see if it is declared in a superclass in which case we can remove it."
	(self selectSuperclasses: [ :class | class sharedPools includes: aDictionary]) isEmpty
		ifFalse: [
			sharedPools := sharedPools size = 1
				ifTrue: [nil]
				ifFalse: [sharedPools reject: [ :any | any = aDictionary]].
			^self ].

	"second get all the subclasses that reference aDictionary through me rather than a
	superclass that is one of my subclasses."

	workingSet := self subclasses asOrderedCollection.
	satisfiedSet := Set new.
	[workingSet isEmpty] whileFalse: [
		aSubclass := workingSet removeFirst.
		(aSubclass sharedPools includes: aDictionary)
			ifFalse: [
				satisfiedSet add: aSubclass.
				workingSet addAll: aSubclass subclasses]].

	"for each of these, see if they refer to any of the variables in aDictionary because
	if they do, we can not remove the dictionary."
	satisfiedSet add: self.
	satisfiedSet do: [ :sub |
		aDictionary associationsDo: [ :aGlobal |
			(sub whichSelectorsReferTo: aGlobal) isEmpty
				ifFalse: [
					^self error: aGlobal key
						, ' is still used in code of class '
						, sub name ]]].
	sharedPools := sharedPools size = 1
		ifTrue: [nil]
		ifFalse: [sharedPools reject: [ :any | any = aDictionary]].
]

{ #category : 'accessing class hierarchy' }
Class >> removeSubclass: aSubclass  [
	"If the argument, aSubclass, is one of the receiver's subclasses, remove it."

	subclasses
		ifNotNil: [
			subclasses := subclasses copyWithout: aSubclass.
			subclasses isEmpty
				ifTrue: [subclasses := nil]]
]

{ #category : 'class name' }
Class >> rename: aString [
	"The new name of the receiver is the argument, aString."

	| newName |
	(newName := aString asSymbol) ~= self name
		ifFalse: [^ self].
	(Smalltalk includesKey: newName)
		ifTrue: [^ self error: newName , ' already exists'].
	(Undeclared includesKey: newName)
		ifTrue: [
			Undeclared removeUnreferencedKeys.
			(Undeclared includesKey: newName)
				ifTrue: [
					self inform: 'There are references to, ' , aString printString , '
from Undeclared. Check them after this change.']].

	self safeRenameTo: newName.
]

{ #category : 'class name' }
Class >> safeRenameTo: newName [

	| oldName |
	
	oldName := name.
	Smalltalk prepareToRenameClass: self as: newName.
	name := newName.
	Smalltalk renamedClass: self from: oldName
]

{ #category : 'initialization' }
Class >> setName: aSymbol [
	"Private - set the name of the class"
	name := aSymbol.
]

{ #category : 'pool variables' }
Class >> sharedPools [
	"Answer a Set of the pool dictionaries declared in the receiver."

	^sharedPools ifNil: [#()]
]

{ #category : 'fileIn/Out' }
Class >> shouldFileOutPool: aPoolName [
	"respond with true if the user wants to file out aPoolName"
	^self confirm: ('FileOut the sharedPool ', aPoolName, '?')
]

{ #category : 'fileIn/Out' }
Class >> shouldFileOutPools [
	"respond with true if the user wants to file out the shared pools"
	^self confirm: 'FileOut selected sharedPools?'
]

{ #category : 'private' }
Class >> spaceUsed [

	"Object spaceUsed"
	^ super spaceUsed + self class spaceUsed
]

{ #category : 'fileIn/Out' }
Class >> storeDataOn: aDataStream [
	"I don't get stored.  Use a DiskProxy"

	self error: 'use a DiskProxy to store a Class'
]

{ #category : 'subclass creation-convenience' }
Class >> subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s [
	"Some chunk files may not include class category in the class declarations."
	^self
		subclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: SystemOrganizer defaultClassCategory.
]

{ #category : 'subclass creation' }
Class >> subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver)."

	| answer |
	answer := ClassBuilder new
		superclass: self
		subclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat.

	Smalltalk
		logChange: answer definition
		preamble: answer definitionPreamble.
	^answer 
	

]

{ #category : 'accessing class hierarchy' }
Class >> subclasses [
	"Answer a Set containing the receiver's subclasses."

	^ subclasses
		ifNil: [#()]
		ifNotNil: [subclasses copy]
]

{ #category : 'accessing class hierarchy' }
Class >> subclassesDo: aBlock  [
	"Evaluate the argument, aBlock, for each of the receiver's immediate subclasses."
	subclasses
		ifNotNil: [subclasses do: aBlock]
]

{ #category : 'enumerating' }
Class >> subclassesSortedDo: aBlock [
	"Evaluate the argument, aBlock, for each of the receiver's immediate subclasses.
	Sort by name."

	subclasses notNil ifTrue: [
		((subclasses reject: [ :c | c isMeta ]) sort: [:c1 :c2 | c1 name < c2 name ]) do: aBlock ].
]

{ #category : 'accessing class hierarchy' }
Class >> superclass: aClassOrNil [
	"Change the receiver's superclass to be aClass.
	Keep the subclass / superclass invariant.
	Not to be used lightly!
	Usually used only to create subclasses of nil."

	| oldSuperclass |
	oldSuperclass := superclass.
	super superclass: aClassOrNil.
	oldSuperclass == superclass ifFalse: [
		oldSuperclass removeSubclass: self.
		aClassOrNil addSubclass: self ].
]

{ #category : 'initialization' }
Class >> superclass: sup methodDict: md format: ft name: nm organization: org instVarNames: nilOrArray classPool: pool sharedPools: poolSet  [
	"Answer an instance of me, a new class, using the arguments of the 
	message as the needed information.
	Must only be sent to a new instance; else we would need Object flushCache."

	superclass := sup.
	methodDict := md.
	format := ft.
	name := nm.
	instanceVariables := nilOrArray.
	classPool := pool.
	sharedPools := poolSet.
	self organization: org.
]

{ #category : 'initialization' }
Class >> superclass: aClass methodDictionary: mDict format: fmt [
	"Basic initialization of the receiver"
	super superclass: aClass methodDictionary: mDict format: fmt.
	subclasses := nil. "Important for moving down the subclasses field into Class"

]

{ #category : 'SUnit tests' }
Class >> testCaseClass [
		
	| potentialTestCaseClass |
	
	potentialTestCaseClass := Smalltalk classNamed: self name, 'Test'.
	
	^potentialTestCaseClass 
	
 
]

{ #category : 'release' }
Class >> unload [
	"Sent when a the class is removed.  Does nothing, but may be overridden by (class-side) subclasses."
	""

]

{ #category : 'subclass creation' }
Class >> variableByteSubclass: t instanceVariableNames: f  [
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable byte-sized nonpointer variables."
	
	| answer |
	answer := ClassBuilder new
		superclass: self
		variableByteSubclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer

]

{ #category : 'subclass creation' }
Class >> variableDoubleByteSubclass: t instanceVariableNames: f  [
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable double-byte-sized (16 bits) nonpointer variables."
	
	| answer |
	answer := ClassBuilder new
		superclass: self
		variableDoubleByteSubclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer

]

{ #category : 'subclass creation' }
Class >> variableDoubleWordSubclass: t instanceVariableNames: f  [
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable double-word-sized (64 bits) nonpointer variables."

	| answer |
	answer := ClassBuilder new
		superclass: self
		variableDoubleWordSubclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer
]

{ #category : 'subclass creation' }
Class >> variableSubclass: t instanceVariableNames: f  [
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable pointer variables."
	
	| answer |
	answer := ClassBuilder new
		superclass: self
		variableSubclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer
]

{ #category : 'subclass creation' }
Class >> variableWordSubclass: t instanceVariableNames: f  [
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable word-sized nonpointer variables."

	| answer |
	answer := ClassBuilder new
		superclass: self
		variableWordSubclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer
]

{ #category : 'subclass creation' }
Class >> weakSubclass: t instanceVariableNames: f  [
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have weak indexable pointer variables."

	| answer |
	answer := ClassBuilder new
		superclass: self
		weakSubclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer
]
