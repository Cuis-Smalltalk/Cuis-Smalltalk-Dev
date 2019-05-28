'From Cuis 5.0 of 7 November 2016 [latest update: #3771] on 26 May 2019 at 5:08:55 pm'!

!Behavior methodsFor: 'testing' stamp: 'len 5/16/2019 05:56:15'!
isDoubleBytes
	"Answer whether the receiver's instances indexed 16-bit integer instance variables.
	 Above Cog Spur the class format is
		<5 bits inst spec><16 bits inst size>
	 where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ Smalltalk isSpur and: [ self instSpec = 12 ]! !

!Behavior methodsFor: 'testing' stamp: 'len 5/16/2019 05:56:02'!
isDoubleWords
	"Answer whether the receiver's instances indexed 64-bit integer instance variables.
	 Above Cog Spur the class format is
		<5 bits inst spec><16 bits inst size>
	 where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ Smalltalk isSpur and: [ self instSpec = 9 ]! !


!Class methodsFor: 'subclass creation' stamp: 'len 5/16/2019 05:11:11'!
variableDoubleByteSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable double-byte-sized (16 bits) nonpointer variables."
	"Note: Only for Spur images"
	
	| answer |
	answer _ ClassBuilder new
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
! !

!Class methodsFor: 'subclass creation' stamp: 'len 5/16/2019 05:10:47'!
variableDoubleWordSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class (the receiver) in which the subclass is to 
	have indexable double-word-sized (64 bits) nonpointer variables."
	"Note: Only for Spur images"

	| answer |
	answer _ ClassBuilder new
		superclass: self
		variableDoubleWordSubclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		category: cat.
		
	Smalltalk
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer! !


!ClassBuilder methodsFor: 'public' stamp: 'len 5/16/2019 06:13:03'!
superclass: aClass
	variableDoubleByteSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class in which the subclass is to 
	have indexable double-byte-sized (16 bit) nonpointer variables."
	"Note: Only for Spur images"

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
		category: cat! !

!ClassBuilder methodsFor: 'public' stamp: 'len 5/16/2019 06:12:49'!
superclass: aClass
	variableDoubleWordSubclass: t instanceVariableNames: f 
	classVariableNames: d poolDictionaries: s category: cat
	"This is the standard initialization message for creating a new class as a 
	subclass of an existing class in which the subclass is to 
	have indexable double-word-sized (64 bit) nonpointer variables."
	"Note: Only for Spur images"

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
		category: cat! !


!Behavior methodsFor: 'testing' stamp: 'len 5/16/2019 05:53:44'!
isWords
	"Answer whether the receiver's instances indexed 32-bit integer instance variables.
	 Above Cog Spur the class format is
		<5 bits inst spec><16 bits inst size>
	 where the 5-bit inst spec is
			0	= 0 sized objects (UndefinedObject True False et al)
			1	= non-indexable objects with inst vars (Point et al)
			2	= indexable objects with no inst vars (Array et al)
			3	= indexable objects with inst vars (MethodContext AdditionalMethodState et al)
			4	= weak indexable objects with inst vars (WeakArray et al)
			5	= weak non-indexable objects with inst vars (ephemerons) (Ephemeron)
			6	= unused
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ Smalltalk isSpur
		ifTrue: [ self instSpec = 10 ]
		ifFalse: [ self isBytes not ]! !


!ClassBuilder methodsFor: 'public' stamp: 'len 5/16/2019 06:09:08'!
superclass: aClass
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
		category: cat! !

!ClassBuilder methodsFor: 'public' stamp: 'len 5/16/2019 06:12:34'!
superclass: aClass
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
		category: cat! !

