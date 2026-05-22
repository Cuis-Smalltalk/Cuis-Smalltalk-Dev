"
My instances add instance-specific behavior to various class-describing objects in the system. This typically includes messages for initializing class variables and instance creation messages particular to a class. There is only one instance of a particular Metaclass, namely the class which is being described. A Metaclass shares the class variables of its instance.
	
[Subtle] In general, the superclass hierarchy for metaclasses parallels that for classes. Thus,
	Integer superclass == Number, and
	Integer class superclass == Number class.
However there is a singularity at Object. Here the class hierarchy terminates, but the metaclass hierarchy must wrap around to Class, since ALL metaclasses are subclasses of Class. Thus,
	Object superclass == nil, and
	Object class superclass == Class.

"
Class {
	#name : 'Metaclass',
	#superclass : 'ClassDescription',
	#instVars : [
		'thisClass'
	],
	#category : 'Kernel-Classes',
	#package : 'Kernel',
	#tag : 'Classes'
}

{ #category : 'anti-corruption' }
Metaclass class >> isScarySelector: newbieSelector [

	"Return true if newbieSelector is already a part of Metaclass protocol."
	(#(new new: lastUnderscoreMeansSubscript) statePointsTo: newbieSelector) ifTrue: [ ^false ].
	(Metaclass includesSelector: newbieSelector) ifTrue: [^ true].
	(ClassDescription includesSelector: newbieSelector) ifTrue: [^ true].
	(Behavior includesSelector: newbieSelector) ifTrue: [^ true].
	^ false

]

{ #category : 'compiling' }
Metaclass >> acceptsLoggingOfCompilation [
	"Answer whether the receiver's method submisions and class defintions should be logged to the changes file and to the current change set.  The metaclass follows the rule of the class itself.  6/18/96 sw"

	^ thisClass acceptsLoggingOfCompilation
]

{ #category : 'instance variables' }
Metaclass >> addInstVarName: aString  [
	"Add the argument, aString, as one of the receiver's instance variables."

	| fullString |
	fullString := aString.
	self instVarNames do: [:aString2 | fullString := aString2 , ' ' , fullString].
	self instanceVariableNames: fullString
]

{ #category : 'class hierarchy' }
Metaclass >> addObsoleteSubclass: aClass [
	"Do nothing."
]

{ #category : 'class hierarchy' }
Metaclass >> addSubclass: aClass [
	"Do nothing."
]

{ #category : 'initialization' }
Metaclass >> adoptInstance: oldInstance from: oldMetaClass  [
	"Recreate any existing instances of the argument, oldClass, as instances of 
	the receiver, which is a newly changed class. Permute variables as 
	necessary."
	thisClass class == self ifTrue:[^self error:'Metaclasses have only one instance'].
	oldMetaClass isMeta ifFalse:[^self error:'Argument must be Metaclass'].
	oldInstance class == oldMetaClass ifFalse:[^self error:'Not the class of argument'].
	^thisClass := self 
		newInstanceFrom: oldInstance 
		variable: self isVariable 
		size: self instSize 
		map: (self instVarMappingFrom: oldMetaClass)
]

{ #category : 'accessing' }
Metaclass >> allClassInstVarNames [
	"Similar to #allInstVarNames, but don't include class instance variables used by the system itself,
	only those defined as class side #instanceVariableNames: "

	^(superclass notNil and: [ superclass isMeta ])
		ifTrue: [superclass allClassInstVarNames , self instVarNames]
		ifFalse: [self instVarNames copy]	"Guarantee a copy is answered."
]

{ #category : 'accessing variables' }
Metaclass >> allClassVarNames [

	"Metaclasses do not define class vars - Hernan"

	^superclass allClassVarNames
]

{ #category : 'accessing' }
Metaclass >> allInstances [
	thisClass class == self ifTrue:[^Array with: thisClass].
	^super allInstances
]

{ #category : 'enumerating' }
Metaclass >> allInstancesDo: aBlock [
	"There should be only one"
	thisClass class == self ifTrue:[^aBlock value: thisClass].
	^super allInstancesDo: aBlock
]

{ #category : 'accessing' }
Metaclass >> bindingOf: varName [

	^thisClass classBindingOf: varName
]

{ #category : 'testing' }
Metaclass >> canZapMethodDictionary [
	"Return true if it is safe to zap the method dictionary on #obsolete"
	^thisClass
		ifNil: [ true ]
		ifNotNil: [ thisClass canZapMethodDictionary ]
]

{ #category : 'accessing' }
Metaclass >> category [
	^thisClass category
]

{ #category : 'class variables' }
Metaclass >> classPool [
	"Answer the dictionary of class variables."

	^thisClass classPool
]

{ #category : 'fileIn/Out' }
Metaclass >> definition [
	"Refer to the comment in ClassDescription|definition."

	^ String streamContents: [ :strm |
		strm print: self;
			newLine;
			tab;
			nextPutAll: 'instanceVariableNames: ';
			store: self instanceVariablesString]
]

{ #category : 'fileIn/Out' }
Metaclass >> fileOutInitializerOn: aStream [
	(self methodDict includesKey: #initialize) ifTrue: [
		aStream newLine.
		aStream nextChunkPut: thisClass name , ' initialize'].
]

{ #category : 'fileIn/Out' }
Metaclass >> fileOutOn: aFileStream moveSource: moveSource toFile: fileIndex initializing: aBool sortMethodsByCategory: byCategory [
	super
		fileOutOn: aFileStream
		moveSource: moveSource
		toFile: fileIndex
		initializing: nil
		sortMethodsByCategory: byCategory.
	(aBool and:[moveSource not and: [self methodDict includesKey: #initialize]]) ifTrue: [
		aFileStream newLine.
		aFileStream newLine.
		aFileStream nextChunkPut: thisClass name , ' initialize'.
		aFileStream newLine ]
]

{ #category : 'class hierarchy' }
Metaclass >> hasSubclasses [

	^ thisClass hasSubclasses
]

{ #category : 'initialization' }
Metaclass >> instanceVariableNames: instVarString  [
	"Declare additional named variables for my instance."
	| answer |
	answer := ClassBuilder new
		class: self
		instanceVariableNames: instVarString.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer
]

{ #category : 'testing' }
Metaclass >> isMeta [
	^ true
]

{ #category : 'testing' }
Metaclass >> isObsolete [
	"Return true if the receiver is obsolete"
	^thisClass == nil "Either no thisClass"
		or:[thisClass class ~~ self "or I am not the class of thisClass"
			or:[thisClass isObsolete]] "or my instance is obsolete"
]

{ #category : 'accessing' }
Metaclass >> localBindingOf: varNameSymbol [

	^thisClass localBindingOf: varNameSymbol 
]

{ #category : 'accessing' }
Metaclass >> name [
	"Answer a String that is the name of the receiver, either 'Metaclass' or 
	the name of the receiver's class followed by ' class'."

	^thisClass
		ifNil: [ 'a Metaclass']
		ifNotNil: [ thisClass name , ' class']
]

{ #category : 'instance creation' }
Metaclass >> new [
	"The receiver can only have one instance. Create it or complain that
	one already exists."

	thisClass class ~~ self
		ifTrue: [^thisClass := self basicNew]
		ifFalse: [self error: 'A Metaclass should only have one instance!']
]

{ #category : 'fileIn/Out' }
Metaclass >> nonTrivial  [
	"Answer whether the receiver has any methods or instance variables."

	^ self instVarNames size > 0 or: [self methodDict size > 0]
]

{ #category : 'fileIn/Out' }
Metaclass >> objectForDataStream: refStrm [
	| dp |
	"I am about to be written on an object file.  Write a reference to a class in Smalltalk instead."

	dp := DiskProxy global: self theNonMetaClass name selector: #class
			args: (Array new).
	refStrm replace: self with: dp.
	^ dp

]

{ #category : 'class hierarchy' }
Metaclass >> obsoleteSubclasses [
	"Answer the receiver's subclasses."
	thisClass
		ifNil: [^ #()].
	^thisClass obsoleteSubclasses 
		select:[:aSubclass| aSubclass isMeta not] 
		thenCollect:[:aSubclass| aSubclass class]

	"Metaclass allInstancesDo:
		[:m | Compiler evaluate: 'subclasses_nil' for: m logged: false]"
]

{ #category : 'compiling' }
Metaclass >> possibleVariablesFor: misspelled continuedFrom: oldResults [

	^ thisClass possibleVariablesFor: misspelled continuedFrom: oldResults

]

{ #category : 'copying' }
Metaclass >> postCopy [
	"Don't share the reference to the sole instance."

	super postCopy.
	thisClass := nil.
]

{ #category : 'printing' }
Metaclass >> printNameOn: aStream [
	thisClass notNil
		ifTrue: [
			thisClass printNameOn: aStream.
			aStream nextPutAll: ' class']
		ifFalse: [ aStream nextPutAll: 'a Metaclass']
]

{ #category : 'instance variables' }
Metaclass >> removeInstVarName: aString  [
	"Remove the argument, aString, as one of the receiver's instance variables."


	| newArray newString |
	(self instVarNames includes: aString)
		ifFalse: [self error: aString , ' is not one of my instance variables'].
	newArray := self instVarNames copyWithout: aString.
	newString := ''.
	newArray do: [:aString2 | newString := aString2 , ' ' , newString].
	self instanceVariableNames: newString
]

{ #category : 'class hierarchy' }
Metaclass >> removeObsoleteSubclass: aClass [
	"Do nothing."
]

{ #category : 'class hierarchy' }
Metaclass >> removeSubclass: aClass [
	"Do nothing."
]

{ #category : 'accessing' }
Metaclass >> soleInstance [
	"The receiver has only one instance.  Answer it."

	^thisClass
]

{ #category : 'fileIn/Out' }
Metaclass >> storeDataOn: aDataStream [
	"I don't get stored.  Use a DiskProxy"

	self error: 'use a DiskProxy to store a Class'
]

{ #category : 'class hierarchy' }
Metaclass >> subclasses [
	"Answer the receiver's subclasses."
	thisClass
		ifNil: [^ #()].
	^thisClass subclasses 
		select:[:aSubclass| aSubclass isMeta not] 
		thenCollect:[:aSubclass| aSubclass class]

	"Metaclass allInstancesDo:
		[:m | Compiler evaluate: 'subclasses_nil' for: m logged: false]"
]

{ #category : 'class hierarchy' }
Metaclass >> subclassesDo: aBlock [
	"Evaluate aBlock for each of the receiver's immediate subclasses."
	thisClass subclassesDo:[:aSubclass|
		"The following test is for Class class which has to exclude
		the Metaclasses being subclasses of Class."
		aSubclass isMeta ifFalse:[aBlock value: aSubclass class]].
]

{ #category : 'SUnit tests' }
Metaclass >> testCaseClass [

	^self soleInstance testCaseClass 
]

{ #category : 'accessing' }
Metaclass >> theMetaClass [
	"Sent to a class or metaclass, always return the metaclass"

	^self
]

{ #category : 'accessing' }
Metaclass >> theNonMetaClass [
	"Sent to a class or metaclass, always return the class"

	^thisClass
]

{ #category : 'compiling' }
Metaclass >> wantsChangeSetLogging [
	"Answer whether code submitted for the receiver should be remembered by the changeSet mechanism.The metaclass follows the rule of the class itself.  7/12/96 sw"

	^ thisClass wantsChangeSetLogging
]

{ #category : 'compiling' }
Metaclass >> wantsRecompilationProgressReported [
	"The metaclass follows the rule of the class itself."

	^ thisClass wantsRecompilationProgressReported
]
