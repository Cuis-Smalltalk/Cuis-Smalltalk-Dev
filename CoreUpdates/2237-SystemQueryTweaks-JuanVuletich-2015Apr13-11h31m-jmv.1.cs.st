'From Cuis 4.2 of 25 July 2013 [latest update: #2236] on 13 April 2015 at 11:36:20.192091 am'!

!Behavior methodsFor: 'accessing instances and variables' stamp: 'jmv 4/13/2015 09:58'!
allRegularInstVarNames
	"Answer an Array of the names of the receiver's instance variables. The 
	Array ordering is the order in which the variables are stored and 
	accessed by the interpreter.
	
	Quite like asking #allInstVarNames, but do not include Behavior state (i.e. Smalltalk internals)"

	| vars |
	self == ProtoObject class
		ifTrue: [vars _ self instVarNames copy]	"Guarantee a copy is answered."
		ifFalse: [vars _ superclass allRegularInstVarNames , self instVarNames].
	^vars! !


!SystemOrganizer methodsFor: 'printing' stamp: 'jmv 4/13/2015 09:16'!
printOn: aStream
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."

	| title |
	title _ self class name.
	aStream
		nextPutAll: (title first isVowel ifTrue: ['an '] ifFalse: ['a ']);
		nextPutAll: title! !


!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 4/13/2015 10:06'!
hierachySortedAllClassesDo: aBlock
	"Evaluate the argument, aBlock, for each class in the system.
	Smalltalk hierachySortedAllClassesDo: [ :cls | cls print ]
	"

	| s |
	s _ self allClasses asSet.
	ProtoObject
		allSubclassesWithLevelDo: [ :cls :l |
			(s includes: cls) ifTrue: [
				aBlock value: cls ]]
		startingLevel: 1! !

