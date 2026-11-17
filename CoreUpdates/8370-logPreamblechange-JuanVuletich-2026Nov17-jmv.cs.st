'From Cuis7.9 [latest update: #8369] on 17 November 2026 at 7:39:03 am'!

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 16/Nov/2026, 7:03:29 pm (UTC)'!
logPreamble: preambleOrNil change: aStringOrText
	"Write the argument, aString, onto the changes file."

	| aString changesFile |
	aString := aStringOrText asPlainString.
	aString firstNonSeparator = 0 ifTrue: [^ self].  "empty doits confuse replay"
	
	self userChangesAppendDo: [ :stream |
		stream newLine; newLine.
		preambleOrNil ifNotNil: [
			stream nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
		stream nextChunkPut: aString.
		stream flush ].
	
	(SourceFiles notNil and: [(changesFile := SourceFiles at: 2) notNil]) ifTrue: [
		changesFile isReadOnly ifFalse: [
			changesFile setToEnd; newLine; newLine.
			preambleOrNil ifNotNil: [
				changesFile nextPut: $!!; nextChunkPut: preambleOrNil; newLine ].
			changesFile nextChunkPut: aString.
			self forceChangesToDisk ]].! !


!Class methodsFor: 'instance variables' stamp: 'jmv 27/Nov/2008, 7:02:00 pm (UTC)'!
addInstVarName: aString
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
		logPreamble: answer definitionPreamble
		change: answer definition.
	^answer! !

!Class methodsFor: 'instance variables' stamp: 'jmv 27/Nov/2008, 7:02:00 pm (UTC)'!
removeInstVarName: aString
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
		logPreamble: answer definitionPreamble
		change: answer definition.
	^answer! !

!Class methodsFor: 'subclass creation' stamp: 'eem 25/Aug/2015, 7:37:00 pm (UTC)'!
ephemeronSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat
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
		logPreamble: answer definitionPreamble
		change: answer definition.
	^answer! !

!Class methodsFor: 'subclass creation' stamp: 'jmv 4/Jan/2024, 6:55:01 pm (UTC)'!
immediateSubclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat
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
		logPreamble: answer definitionPreamble
		change: answer definition.
	^answer! !

!Class methodsFor: 'subclass creation' stamp: 'jmv 4/Oct/2024, 4:50:25 pm (UTC)'!
subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat
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
		logPreamble: answer definitionPreamble
		change: answer definition.
	^answer! !

!Class methodsFor: 'subclass creation' stamp: 'jmv 7/Apr/2008, 2:11:00 am (UTC)'!
variableByteSubclass: t instanceVariableNames: f
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
		logPreamble: answer definitionPreamble
		change: answer definition.
	^answer! !

!Class methodsFor: 'subclass creation' stamp: 'jmv 4/Jan/2024, 6:45:17 pm (UTC)'!
variableDoubleByteSubclass: t instanceVariableNames: f
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
		logPreamble: answer definitionPreamble
		change: answer definition.
	^answer! !

!Class methodsFor: 'subclass creation' stamp: 'jmv 4/Jan/2024, 6:45:26 pm (UTC)'!
variableDoubleWordSubclass: t instanceVariableNames: f
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
		logPreamble: answer definitionPreamble
		change: answer definition.
	^answer! !

!Class methodsFor: 'subclass creation' stamp: 'jmv 7/Apr/2008, 2:11:00 am (UTC)'!
variableSubclass: t instanceVariableNames: f
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
		logPreamble: answer definitionPreamble
		change: answer definition.
	^answer! !

!Class methodsFor: 'subclass creation' stamp: 'jmv 7/Apr/2008, 2:11:00 am (UTC)'!
variableWordSubclass: t instanceVariableNames: f
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
		logPreamble: answer definitionPreamble
		change: answer definition.
	^answer! !

!Class methodsFor: 'subclass creation' stamp: 'jmv 7/Apr/2008, 2:12:00 am (UTC)'!
weakSubclass: t instanceVariableNames: f
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
		logPreamble: answer definitionPreamble
		change: answer definition.
	^answer! !


!Metaclass methodsFor: 'initialization' stamp: 'jmv 7/Apr/2008, 2:12:00 am (UTC)'!
instanceVariableNames: instVarString
	"Declare additional named variables for my instance."
	| answer |
	answer := ClassBuilder new
		class: self
		instanceVariableNames: instVarString.
		
	Smalltalk
		logPreamble: answer definitionPreamble
		change: answer definition.
	^answer! !


!SystemDictionary methodsFor: 'class names' stamp: 'HAW 17/Jan/2024, 11:50:21 pm (UTC)'!
renamedClass: aClass from: oldName
	
	| newName |
	
	newName := aClass name.
	
	SystemChangeNotifier uniqueInstance classRenamed: aClass from: oldName to: newName inCategory: aClass category.
	self
		logPreamble: 'classRenamed: #', oldName, ' as: #', newName, Utilities changeStampField
		change: 'Smalltalk renameClassNamed: #', oldName, ' as: #', newName.! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'HAW 27/Oct/2019, 2:45:14 am (UTC)'!
classRemoved: aClass fromCategory: aCategoryName

	| classDefinition |
	
	aClass acceptsLoggingOfCompilation 
		ifTrue: [
			"I have to recreate the category because the classs has already been removed form the
			SystemOrganizer - Hernan"
			classDefinition := aClass definitionReplacingCategoryWith: aCategoryName. 
			
			self
				logPreamble: 'classRemoval: ', aClass name printString, Utilities changeStampField
				change: classDefinition ].! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'jmv 26/Jul/2018, 8:58:32 pm (UTC)'!
logChange: aStringOrText
	"Write the argument, aString, onto the changes file."
	^ self logPreamble: nil change: aStringOrText! !

!SystemDictionary methodsFor: 'sources, change log' stamp: 'HAW 27/Oct/2019, 2:45:25 am (UTC)'!
methodRemoved: aMethod selector: aSymbol inProtocol: protocol class: aClass

	aClass acceptsLoggingOfCompilation 
		ifTrue: [
			self
				logPreamble: 'methodRemoval: ', aClass name, ' ', aSymbol storeString, Utilities changeStampField
				change: aMethod sourceCode ].! !


!RenameInstanceVariable methodsFor: 'applying - private' stamp: 'jmv 11/Jul/2025, 7:45:42 pm (UTC)'!
logChange
	
	Smalltalk
		logPreamble: classToRefactor definitionPreamble
		change: classToRefactor definition.
	
	ChangeSet
		classDefinitionChangedFrom: originalClassToRefactor
		to: classToRefactor
		oldClassDefinition: originalClassDefinition.! !

!methodRemoval: SystemDictionary #logChange:preamble: stamp: 'jmv 17/Nov/2026, 10:38:39 am (UTC)'!
SystemDictionary removeSelector: #logChange:preamble:!
