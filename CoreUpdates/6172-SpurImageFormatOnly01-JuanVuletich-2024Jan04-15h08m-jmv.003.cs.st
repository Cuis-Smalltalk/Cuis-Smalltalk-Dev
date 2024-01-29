'From Cuis6.3 [latest update: #6164] on 4 January 2024 at 4:33:20 pm'!

!ClassBuilder methodsFor: 'class format' stamp: 'jmv 1/4/2024 15:17:52'!
computeFormat: type instSize: newInstSize forSuper: newSuper
	"Compute the new format for making oldClass a subclass of newSuper.
	Return the format or nil if there is any problem."

	^ self computeFormat: type fullInstancesSize: (newInstSize + (newSuper ifNil: [0] ifNotNil: [newSuper instSize]))! !


!ProtoObject methodsFor: 'comparing' stamp: 'jmv 1/4/2024 16:19:39'!
identityHash
	"Answer a SmallInteger whose value is related to the receiver's identity.
	This method must not be overridden, except by immediate classes such as
	SmallInteger, Character and SmallFloat64.
	Primitive. Fails if the receiver is a SmallInteger. Essential.
	See Object documentation whatIsAPrimitive.

	Do not override."

	<primitive: 75>
	self primitiveFailed! !


!Behavior methodsFor: 'accessing' stamp: 'jmv 1/4/2024 15:32:35'!
typeOfClass
	"Answer a symbol uniquely describing the type of the receiver. c.f. kindOfSubclass, instSpec"
	^(#(normal
		normal
		variable
		variable
		weak
		ephemeron
		nil
		immediate
		nil
		longs
		words				nil
		shorts				nil nil nil
		bytes				nil nil nil nil nil nil nil
		compiledMethod	nil nil nil nil nil nil nil)
			at: self instSpec + 1) ifNil:
				[self error: 'invalid class type']! !

!Behavior methodsFor: 'testing' stamp: 'jmv 1/4/2024 15:26:02'!
instSize
	"Answer the number of named instance variables
	(as opposed to indexed variables) of the receiver.
	Class format is
		<5 bits inst spec><16 bits inst size>"

	^ format bitAnd: 16rFFFF! !

!Behavior methodsFor: 'testing' stamp: 'jmv 1/4/2024 15:27:16'!
instSpec
	"Answer the instance specification part of the format that defines what kind of object
	 an instance of the receiver is."
	"The formats are
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
		10-11	= 32-bit indexable (Bitmap)					(plus one odd bit, unused in 32-bits)
		12-15	= 16-bit indexable							(plus two odd bits, one unused in 32-bits)
		16-23	= 8-bit indexable							(plus three odd bits, one unused in 32-bits)
		24-31	= compiled methods (CompiledMethod)	(plus three odd bits, one unused in 32-bits)
	 Note that in the VM instances also have a 5 bit format field that relates to their class's format.
	 Formats 11, 13-15, 17-23 & 25-31 are unused in classes but used in instances to define the
	 number of elements missing up to the slot size.  For example, a 2-byte ByteString instance
	 has format 18 in 32-bits, since its size is one 32-bit slot - 2 bytes ((18 bitAnd: 3) = 2), and
	 22 in 64 bits, since its size is one 64-bit slot - 6 bytes ((22 bitAnd: 7) = 6)."

	^(format bitShift: -16) bitAnd: 16r1F! !

!Behavior methodsFor: 'testing' stamp: 'jmv 1/4/2024 15:28:45'!
isBits
	"Answer whether the receiver contains just bits (not pointers).
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
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ self instSpec >= 7! !

!Behavior methodsFor: 'testing' stamp: 'jmv 1/4/2024 15:28:31'!
isBytes
	"Answer whether the receiver contains just bits (not pointers).
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
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ self instSpec >= 16! !

!Behavior methodsFor: 'testing' stamp: 'jmv 1/4/2024 15:28:35'!
isDoubleBytes
	"Answer whether the receiver's instances indexed 16-bit integer instance variables.
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
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ self instSpec = 12! !

!Behavior methodsFor: 'testing' stamp: 'jmv 1/4/2024 15:28:18'!
isDoubleWords
	"Answer whether the receiver's instances indexed 64-bit integer instance variables.
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
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ self instSpec = 9! !

!Behavior methodsFor: 'testing' stamp: 'jmv 1/4/2024 15:10:32'!
isVariable
	"Answer whether the receiver has indexable variables.
	 Class format is
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
	| instSpec |
	instSpec := self instSpec.
	^instSpec >= 2 and: [instSpec <= 4 or: [instSpec >= 9]]! !

!Behavior methodsFor: 'testing' stamp: 'jmv 1/4/2024 16:08:47'!
isWords
	"Answer whether the receiver's instances indexed 32-bit integer instance variables.
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
			7	= immediates (SmallInteger, Character)
			8	= unused
			9	= 64-bit indexable
		10-11	= 32-bit indexable (Bitmap)
		12-15	= 16-bit indexable
		16-23	= 8-bit indexable
		24-31	= compiled methods (CompiledMethod)"

	^ self instSpec = 10! !

!Behavior methodsFor: 'testing' stamp: 'jmv 1/4/2024 15:24:23'!
shouldNotBeRedefined
	"Return true if the receiver should not be redefined.
	The assumption is that classes in Smalltalk specialObjects and 
	Behaviors should not be redefined"

	^(Smalltalk specialObjectsArray identityIncludes: self)
			or: [self isKindOf: self]! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'jmv 1/4/2024 16:08:12'!
byteSizeOfInstance
	"Answer the total memory size of an instance of the receiver."

	<primitive: 181 error: ec>
	self isVariable ifTrue:
		[^self byteSizeOfInstanceOfSize: 0].
	self primitiveFailed! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'jmv 1/4/2024 16:08:18'!
byteSizeOfInstanceOfSize: basicSize
	"Answer the total memory size of an instance of the receiver
	 with the given number of indexable instance variables."

	<primitive: 181 error: ec>
	self isVariable
		ifTrue: "If the primitive overflowed answer a close approximation"
			[(basicSize isInteger
			  and: [basicSize >= 16r1000000]) ifTrue:
				[^2 * (self byteSizeOfInstanceOfSize: basicSize + 1 // 2)
				   - (self byteSizeOfInstanceOfSize: 0)]]
		ifFalse:
			[basicSize = 0 ifTrue:
				[^self byteSizeOfInstance]].
	self primitiveFailed! !

!Behavior methodsFor: 'testing class hierarchy' stamp: 'jmv 1/4/2024 15:31:55'!
kindOfSubclass
	"Answer a String that is the keyword that describes the receiver's kind of subclass,
	 either a regular subclass, a variableSubclass, a variableByteSubclass,
	 a variableWordSubclass, a weakSubclass, an ephemeronSubclass or an immediateSubclass.
	 c.f. typeOfClass & instSpec"
	^(#(' subclass: '
		' subclass: '
		' variableSubclass: '
		' variableSubclass: '
		' weakSubclass: '
		' ephemeronSubclass: '
		nil
		' immediateSubclass: '
		nil
		' variableDoubleWordSubclass: '
		' variableWordSubclass: '		nil
		' variableDoubleByteSubclass: '	nil nil nil
		' variableByteSubclass: '		nil nil nil nil nil nil nil
		' variableByteSubclass: '		nil nil nil nil nil nil nil )
			at: self instSpec + 1) ifNil:
				[self error: 'invalid class type']! !

!Behavior methodsFor: 'comparing' stamp: 'jmv 1/4/2024 16:08:35'!
identityHash
	"Answer a SmallInteger whose value is related to the receiver's identity.
	 Behavior implements identityHash to allow the VM to use an object representation which
	 does not include a direct reference to an object's class in an object.  If the VM is using
	 this implementation then classes are held in a class table and instances contain the index
	 of their class in the table.  A class's class table index is its identityHash so that an instance
	 can be created without searching the table for a class's index.  The VM uses this primitive
	 to enter the class into the class table, assigning its identityHash with an as yet unused
	 class table index. If this primitive fails it means that the class table is full.
	 There are 22 bits of classTable index and 22 bits of identityHash per object.

	 Primitive. Essential. Do not override. See Object documentation whatIsAPrimitive."

	<primitive: 175>
	self primitiveFailed.! !


!Class methodsFor: 'subclass creation' stamp: 'jmv 1/4/2024 15:55:01'!
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
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer! !

!Class methodsFor: 'subclass creation' stamp: 'jmv 1/4/2024 15:44:55'!
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
		logChange: answer definition
		preamble: answer definitionPreamble.
	^answer 
	
! !

!Class methodsFor: 'subclass creation' stamp: 'jmv 1/4/2024 15:45:17'!
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
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer
! !

!Class methodsFor: 'subclass creation' stamp: 'jmv 1/4/2024 15:45:26'!
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
		logChange: answer definition 
		preamble: answer definitionPreamble.
	^answer! !


!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 1/4/2024 15:16:50'!
needsSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass
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
! !

!ClassBuilder methodsFor: 'class definition' stamp: 'jmv 1/4/2024 15:18:32'!
newSubclassOf: newSuper type: type instanceVariables: instVars from: oldClass
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

	^newClass! !

!ClassBuilder methodsFor: 'class format' stamp: 'jmv 1/4/2024 16:09:36'!
computeFormat: type fullInstancesSize: newInstSize
	"Compute the new format for making oldClass a subclass of newSuper.
	 Answer the format or nil if there is any problem."

	| isVar isPointers isWeak bitsUnitSize |
	type == #compiledMethod ifTrue: [
		newInstSize > 0 ifTrue: [
			self error: 'A compiled method class cannot have named instance variables'.
			^nil].
		^CompiledMethod format].
	newInstSize > 65535 ifTrue: [
		self error: 'Class has too many instance variables (', newInstSize printString,')'.
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
	^self format: newInstSize variable: isVar bitsUnitSize: bitsUnitSize pointers: isPointers weak: isWeak! !

!ClassBuilder methodsFor: 'class format' stamp: 'jmv 1/4/2024 16:13:22'!
format: nInstVars variable: isVar bitsUnitSize: bitsUnitSize pointers: isPointers weak: isWeak

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
	^(instSpec bitShift: 16) + nInstVars! !

!ClassBuilder methodsFor: 'private' stamp: 'jmv 1/4/2024 15:17:05'!
privateNewSubclassOf: newSuper from: oldClass
	"Create a new meta and non-meta subclass of newSuper using oldClass as template"
	"WARNING: This method does not preserve the superclass/subclass invariant!!"
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
	^newMeta adoptInstance: oldClass from: oldMeta! !

!ClassBuilder methodsFor: 'public' stamp: 'jmv 1/4/2024 15:35:25'!
superclass: aClass
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
		category: cat! !

!ClassBuilder methodsFor: 'public' stamp: 'jmv 1/4/2024 15:35:41'!
superclass: aClass
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
		category: cat! !

!ClassBuilder methodsFor: 'public' stamp: 'jmv 1/4/2024 16:13:46'!
superclass: aClass
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
		category: cat! !

!ClassBuilder methodsFor: 'public' stamp: 'jmv 1/4/2024 16:13:52'!
superclass: aClass
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
		category: cat! !

!ClassBuilder methodsFor: 'class mutation' stamp: 'jmv 1/4/2024 15:57:44'!
update: oldClass to: newClass
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

		oldClass superclass removeSubclass: oldClass.
		oldClass superclass removeObsoleteSubclass: oldClass.

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
	] valueUnpreemptively! !


!Character methodsFor: 'accessing' stamp: 'jmv 1/4/2024 16:16:33'!
numericValue
	"Answer the value of the receiver that represents its ISO 8859-15 (Latin-9) encoding."

	<primitive: 171>
	^self primitiveFailed! !

!Character methodsFor: 'comparing' stamp: 'jmv 1/4/2024 15:12:44'!
identityHash
	"Answer the receiver's character code."

	<primitive: 171>! !


!Character class methodsFor: 'instance creation' stamp: 'jmv 1/4/2024 16:17:57'!
numericValue: anInteger
	"Answer the Character whose value is anInteger."

	<primitive: 170>
	^self primitiveFailed! !


!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 1/4/2024 15:38:13'!
numLiterals
	"Answer the number of literals used by the receiver."

	^ self header bitAnd: 16r7FFF! !

!CompiledMethod methodsFor: 'accessing' stamp: 'jmv 1/4/2024 15:38:37'!
primitive
	"Answer the primitive index associated with the receiver.
	Zero indicates that this is not a primitive method."

	| initialPC |
	^(self header anyMask: 65536) "Is the hasPrimitive? flag set?"
		ifTrue: [(self at: (initialPC := self initialPC) + 1) + ((self at: initialPC + 2) bitShift: 8)]
		ifFalse: [0]! !


!CompiledMethod class methodsFor: 'instance creation' stamp: 'jmv 1/4/2024 15:37:18'!
newBytes: numberOfBytes trailerBytes: trailer nArgs: nArgs nTemps: nTemps nStack: stackSize nLits: nLits primitive: primitiveIndex
	"Answer an instance of me. The header is specified by the message 
	 arguments. The remaining parts are not as yet determined."
	| method pc |
"15 | 63 | 32767"
	nArgs > 15
		ifTrue: [^self error: 'Cannot compile -- too many arguments'].
	nTemps > 63
		ifTrue: [^self error: 'Cannot compile -- too many temporary variables'].	
	nLits > 32767
		ifTrue: [^self error: 'Cannot compile -- too many literals'].

	method := self
				createMethod: numberOfBytes
				trailer: trailer
				header:    (nArgs bitShift: 24)
						+ (nTemps bitShift: 18)
						+ ((nTemps + stackSize) > SmallFrame ifTrue: [1 bitShift: 17] ifFalse: [0])
						+ nLits
						+ (primitiveIndex > 0 ifTrue: [1 bitShift: 16] ifFalse: [0]).
	primitiveIndex > 0 ifTrue: [
		pc := method initialPC.
		method
			at: pc + 0 put: method encoderClass callPrimitiveCode;
			at: pc + 1 put: (primitiveIndex bitAnd: 16rFF);
			at: pc + 2 put: (primitiveIndex bitShift: -8)].
	^method! !

!CompiledMethod class methodsFor: 'constants' stamp: 'jmv 1/4/2024 16:21:20'!
maxNumLiterals
	"The current header format and the VM's interpretation of it allows for a maximum of 32767 literals."

	^32767! !


!SystemDictionary methodsFor: 'memory space' stamp: 'jmv 1/4/2024 16:11:49'!
growMemoryByAtLeast: numBytes
	"Grow memory by at least the requested number of bytes.
	 Primitive.  Fail if no memory is available.  Essential.
	 N.B. In Spur, the heap is composed of segments. Growing memory adds a new
	 segment; it does not extend existing segments. Growth is by at least that
	 established by Smalltalk vmParameterAt: 25, which defaults to 16Mb."

	<primitive: 180>
	^(numBytes isInteger and: [numBytes > 0])
		ifTrue: [OutOfMemory signal]
		ifFalse: [self primitiveFailed]! !

!SystemDictionary methodsFor: 'retrieving' stamp: 'jmv 1/4/2024 16:21:43'!
allObjectsDo: aBlock 
	"Evaluate the argument, aBlock, for each object in the system, excluding immediates
	 such as SmallInteger, Character and SmallFloat64."
	self allObjectsOrNil
		ifNotNil: [:allObjects| allObjects do: aBlock]
		ifNil:
			["Fall back on the old single object primitive code.  With closures, this needs
			  to use an end marker (lastObject) since activation of the block will create
			  new contexts and cause an infinite loop.  The lastObject must be created
			  before calling someObject, so that the VM can settle the enumeration (e.g.
			  by flushing new space) as a side effect of  someObject"
			| object lastObject |
			lastObject := Object new.
			object := self someObject.
			[lastObject == object or: [0 == object]] whileFalse: [
				| nextObject |
				nextObject := object nextObject.
				aBlock value: object.
				object := nextObject ]]! !

!SystemDictionary methodsFor: 'shrinking' stamp: 'jmv 1/4/2024 16:11:15'!
reduceCuis
	"
	Smalltalk reduceCuis
	"
	| keep n unused newDicts oldDicts |

	self nominallyUnsent: #reduceCuis.
	
	"Remove icons"
	Smalltalk at: #ClassicTheme ifPresent: [ :cls | cls beCurrent ].
	WorldMorph allInstancesDo: [ :w |
		w backgroundImageData: nil.
		w submorphsDo: [ :a | a delete ]].
	Preferences at: #wantsMenuIcons put: false.
	Theme current initialize.
	Theme content: nil.
	Color shutDown.
	BitBltCanvas releaseClassCachedState.

	Transcript clear.
	Clipboard default initialize.


	"Remove some methods, even if they have senders."
	Utilities removeSelector: #vmStatisticsReportString.
	SystemDictionary removeSelector: #recreateSpecialObjectsArray.

	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.

	keep := OrderedCollection new.
	keep addAll: #(SpaceTally DynamicTypingSmalltalkCompleter).
	AppLauncher appGlobalName ifNotNil: [ :any |
		keep add: any ].
	unused := Smalltalk unusedClasses copyWithoutAll: keep.
	[
		#hereWeGo print.
		unused do: [:c | 
			c print.
			(Smalltalk at: c) removeFromSystem]. 
		n := Smalltalk removeAllUnSentMessages.
		unused := Smalltalk unusedClasses copyWithoutAll: keep.
		n > 0 or: [ 
			unused notEmpty ]] whileTrue.
	ChangeSet zapAllChangeSets.
	Smalltalk garbageCollect.

	Smalltalk removeEmptyMessageCategories.
	Smalltalk organization removeEmptyCategories.
	Symbol rehash.

	"Shrink method dictionaries."
	Smalltalk garbageCollect.
	oldDicts := MethodDictionary allInstances.
	newDicts := Array new: oldDicts size.
	oldDicts withIndexDo: [:d :index | 
		newDicts at: index put: d rehashWithoutBecome ].
	oldDicts elementsExchangeIdentityWith: newDicts.
	oldDicts := newDicts := nil.

	 SmalltalkCompleter initialize .

   "Sanity checks"
"   Undeclared
   Smalltalk cleanOutUndeclared
   Smalltalk browseUndeclaredReferences
   Smalltalk obsoleteClasses
   Smalltalk obsoleteBehaviors 
   Smalltalk browseObsoleteMethodReferences
   Smalltalk browseAllUnimplementedCalls"! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 1/4/2024 15:39:16'!
filenameForUpdatedImage: aBoolean
	"If aBoolean, include highest update number	"
	| fileName newName changesName systemVersion |
	systemVersion := SystemVersion current.
	fileName := String streamContents: [ :strm |
		strm nextPutAll: systemVersion versionString.
		aBoolean ifTrue: [
			strm
				nextPut: $-;
				print: systemVersion highestUpdate ].
		Smalltalk wordSize = 4 ifTrue: [
			strm nextPutAll: '-32' ]].
	newName := fileName, '.image'.
	(DirectoryEntry smalltalkImageDirectory // newName) exists ifTrue: [
		newName := DirectoryEntry smalltalkImageDirectory
			nextNameFor: fileName
			extension: 'image' ].
	changesName := self fullNameForChangesNamed: newName.
	"Check to see if there is a .changes file that would cause a problem if we saved a new .image file with the new version number"
	changesName asFileEntry exists ifTrue: [
		self inform:
'There is already .changes file of the desired name,
', changesName, '
curiously already present, even though there is no
corresponding .image file.
Please remedy manually and then repeat your request.'.
		^nil  ].
	^newName! !

!SystemDictionary methodsFor: 'snapshot and quit' stamp: 'jmv 1/4/2024 16:26:42'!
setGCParameters
	"Adjust the VM's default GC parameters to avoid premature tenuring, and too frequent scavenging.
	Parameters set here don't persist, so we set them on startup.
	See #setDefaultGCParameters"

	| proportion edenSize survivorSize averageObjectSize numObjects |

	"Grow old memory in chunks of: "
	Smalltalk vmParameterAt: 25 put: `32*1024*1024`.
	"Shrink heap when unused memory is at least: "
	Smalltalk vmParameterAt: 24 put: `64*1024*1024`.

	"Note: (jmv, 9/2021) It seems modern Spur VMs ignore parameter 6. This all seems to be bogus."
	proportion := 0.9. "tenure when 90% of pastSpace is full"
	edenSize := self vmParameterAt: 44.
	survivorSize := edenSize / 5.0. "David's paper uses 140Kb eden + 2 x 28kb survivor spaces; Spur uses the same ratios :-)"
	averageObjectSize := 8 * self wordSize. "a good approximation"
	numObjects := (proportion * survivorSize / averageObjectSize) rounded.
	self vmParameterAt: 6 put: numObjects.  "tenure when more than this many objects survive the GC"
	"/Note: (jmv, 9/2021)"
	
	"Do a full GC when used memory grows by this factor.
	Default is 0.333.
	2.0 means gull GC when heap size triples."
	Smalltalk vmParameterAt: 55 put: 2.0.! !

!SystemDictionary methodsFor: 'special objects' stamp: 'jmv 1/4/2024 16:25:58'!
recreateSpecialObjectsArray
	"Smalltalk recreateSpecialObjectsArray"
	
	"To external package developers:
	**** DO NOT OVERRIDE THIS METHOD.  *****
	If you are writing a plugin and need additional special object(s) for your own use, 
	use addGCRoot() function and use own, separate special objects registry "
	
	"The Special Objects Array is an array of objects used by the Squeak virtual machine.
	 Its contents are critical and accesses to it by the VM are unchecked, so don't even
	 think of playing here unless you know what you are doing."
	| newArray |
	newArray := Array new: 60.
	"Nil false and true get used throughout the interpreter"
	newArray at: 1 put: nil.
	newArray at: 2 put: false.
	newArray at: 3 put: true.
	"This association holds the active process (a ProcessScheduler)"
	newArray at: 4 put: (self associationAt: #Processor).
	"Numerous classes below used for type checking and instantiation"
	newArray at: 5 put: Bitmap.
	newArray at: 6 put: SmallInteger.
	newArray at: 7 put: String.
	newArray at: 8 put: Array.
	newArray at: 9 put: Smalltalk.
	newArray at: 10 put: BoxedFloat64.
	newArray at: 11 put: MethodContext.
	newArray at: 11 put: (self at: #MethodContext ifAbsent: [self at: #Context]).
	newArray at: 12 put: nil. "was BlockContext."
	newArray at: 13 put: Point.
	newArray at: 14 put: LargePositiveInteger.
	newArray at: 15 put: Display.
	newArray at: 16 put: Message.
	newArray at: 17 put: CompiledMethod.
	newArray at: 18 put: ((self specialObjectsArray at: 18) ifNil: [Semaphore new]). "low space Semaphore"
	newArray at: 19 put: Semaphore.
	newArray at: 20 put: Character.
	newArray at: 21 put: #doesNotUnderstand:.
	newArray at: 22 put: #cannotReturn:.
	newArray at: 23 put: nil. "This is the process signalling low space."
	"An array of the 32 selectors that are compiled as special bytecodes,
	 paired alternately with the number of arguments each takes."
	newArray at: 24 put: #(	#+ 1 #- 1 #< 1 #> 1 #<= 1 #>= 1 #= 1 #~= 1
							#* 1 #/ 1 #\\ 1 #@ 1 #bitShift: 1 #// 1 #bitAnd: 1 #bitOr: 1
							#at: 1 #at:put: 2 #size 0 #next 0 #nextPut: 1 #atEnd 0 #== 1 #class 0
							#blockCopyNOWUNUSED: 1 #value 0 #value: 1 #do: 1 #new 0 #new: 1 #x 0 #y 0 ).

	newArray at: 25 put: nil.	"Was an array of 256 Characters. nil in Spur."
	newArray at: 26 put: #mustBeBoolean.
	newArray at: 27 put: ByteArray.
	newArray at: 28 put: Process.
	newArray at: 29 put: nil. "Was the #compactClassesArray. nil in Spur."
	newArray at: 30 put: ((self specialObjectsArray at: 30) ifNil: [Semaphore new]). "delay Semaphore"
	newArray at: 31 put: ((self specialObjectsArray at: 31) ifNil: [Semaphore new]). "user interrupt Semaphore"

	"Entries 32 - 34 unreferenced. Previously these contained prototype instances to be copied for fast initialization"
	newArray at: 32 put: nil. "was the prototype Float"
	newArray at: 33 put: nil. "was the prototype 4-byte LargePositiveInteger"
	newArray at: 34 put: nil. "was the prototype Point"

	newArray at: 35 put: #cannotInterpret:.

	newArray at: 36 put: nil. "was the prototype MethodContext"

	newArray at: 37 put: BlockClosure.
	newArray at: 38 put: nil. "was the prototype BlockContext"

	"array of objects referred to by external code"
	newArray at: 39 put: (self specialObjectsArray at: 39).	"preserve external semaphores"
	newArray at: 40 put: nil. "Reserved for Mutex in Cog VMs"
	newArray at: 41 put: ((self specialObjectsArray at: 41) ifNil: [LinkedList new]). "Reserved for a LinkedList instance for overlapped calls in CogMT"
	newArray at: 42 put: ((self specialObjectsArray at: 42) ifNil: [Semaphore new]). "finalization Semaphore"
	newArray at: 43 put: LargeNegativeInteger.
	"External objects for callout.
	 Note: Written so that one can actually completely remove the FFI."
	newArray at: 44 put: (self at: #ExternalAddress ifAbsent: []).
	newArray at: 45 put: (self at: #ExternalStructure ifAbsent: []).
	newArray at: 46 put: (self at: #ExternalData ifAbsent: []).
	newArray at: 47 put: (self at: #ExternalFunction ifAbsent: []).
	newArray at: 48 put: (self at: #ExternalLibrary ifAbsent: []).
	newArray at: 49 put: #aboutToReturn:through:.
	newArray at: 50 put: #run:with:in:.
	"51 reserved for immutability message"
	newArray at: 51 put: #attemptToAssign:withIndex:.
	newArray at: 52 put: #(nil "nil => generic error" #'bad receiver'
							#'bad argument' #'bad index'
							#'bad number of arguments'
							#'inappropriate operation'  #'unsupported operation'
							#'no modification' #'insufficient object memory'
							#'insufficient C memory' #'not found' #'bad method'
							#'internal error in named primitive machinery'
							#'object may move' #'resource limit exceeded'
							#'object is pinned' #'primitive write beyond end of object').
	"53 to 55 are for Alien"
	newArray at: 53 put: (self at: #Alien ifAbsent: []).
	newArray at: 54 put: #invokeCallbackContext:. "use invokeCallback:stack:registers:jmpbuf: for old Alien callbacks."
	newArray at: 55 put: (self at: #UnsafeAlien ifAbsent: []).

	"Weak reference finalization"
	"Used to be WeakFinalizationList for WeakFinalizationList hasNewFinalization, obsoleted by ephemeron support."
	newArray at: 56 put: nil.

	"reserved for foreign callback process"
	newArray at: 57 put: (self specialObjectsArray at: 57 ifAbsent: []).

	newArray at: 58 put: #unusedBytecode.
	"59 reserved for Sista counter tripped message"
	newArray at: 59 put: #conditionalBranchCounterTrippedOn:.
	"60 reserved for Sista class trap message"
	newArray at: 60 put: #classTrapFor:.

	"Now replace the interpreter's reference in one atomic operation"
	self specialObjectsArray become: newArray.! !

!SystemDictionary methodsFor: 'system attributes' stamp: 'jmv 1/4/2024 15:39:42'!
maxIdentityHash
	"Answer the maximum identityHash value supported by the VM.
	Usually the VM is able to answer. It seems that SqueakJS (as of October 5, 2020) isn't.
	On primitive failure, still give a correct answer."

	<primitive: 176>	
	^ 16r3FFFFF							"22 bits in Spur"! !

!SystemDictionary methodsFor: 'vm parameters' stamp: 'jmv 1/4/2024 16:29:03'!
vmParameterAt: parameterIndex
	"parameterIndex is a positive integer corresponding to one of the VM's internal parameter/metric registers. 
	Answer with the current value of that register.
	Answer nil if the VM doesn't provide the register, and the primitive fails.
	
	Also see #getVMParameters and #vmParameterAt:put: These 3 methods call the 
	same primitive, whose behavior depends on argument count:
		0 args:	return an Array of VM parameter values;
  		1 arg:	return the indicated VM parameter;
  		2 args:	set the VM indicated parameter.

	VM parameters are numbered as follows:
		1	size of old-space (0-based, read-only)
		2	size of new-space (read-only)
		3	size of heap (read-only)
		4	nil (was allocationCount (read-only))
		5	nil (was allocations between GCs (read-write)
		6	survivor count tenuring threshold (read-write)
		7	full GCs since startup (read-only)
		8	total milliseconds in full GCs since startup (read-only)
		9	scavenges since startup (read-only)
		10	scavenges since startup (read-only)
		11	tenures of surving objects since startup or reset (read-write)
		12-20 were specific to ikp's JITTER VM, now 12-15 are open for use
		16	total microseconds at idle since start-up (if non-zero)
		17	fraction of the code zone to use (Sista only; used to control code zone use to preserve sendAndBranchData on counter tripped callback)
		18	total milliseconds in compaction phase of full GC since start-up
		19	scavenge threshold, the effective size of eden.  When eden fills to the threshold a scavenge is scheduled.
		20	utc microseconds at VM start-up (actually at time initialization, which precedes image load).
		21	root/remembered table size (occupancy) (read-only)
		22	root table overflows since startup (read-only)
		23	bytes of extra memory to reserve for VM buffers, plugins, etc (stored in image file header).
		24	memory threshold above which shrinking object memory (rw)
		25	memory headroom when growing object memory (rw)
		26	interruptChecksEveryNms - force an ioProcessEvents every N milliseconds (rw)
		27	number of times mark loop iterated for current IGC/FGC (read-only) includes ALL marking
		28	number of times sweep loop iterated for current IGC/FGC (read-only)
		29	number of times make forward loop iterated for current IGC/FGC (read-only)
		30	number of times compact move loop iterated for current IGC/FGC (read-only)
		31	number of grow memory requests (read-only)
		32	number of shrink memory requests (read-only)
		33	number of root table entries used for current IGC/FGC (read-only)
		34	bytes allocated in total since start-up or reset (read-write)
		35	number of survivor objects after current IGC/FGC (read-only)
		36	millisecond clock when current IGC/FGC completed (read-only)
		37	number of marked objects for Roots of the world, not including Root Table entries for current IGC/FGC (read-only)
		38	milliseconds taken by current IGC (read-only)
		39	Number of finalization signals for Weak Objects pending when current IGC/FGC completed (read-only)
		40	BytesPerOop for this image
		41	imageFormatVersion for the VM
		42	number of stack pages in use
		43	desired number of stack pages (stored in image file header, max 65535)
		44	size of eden, in bytes
		45	desired size of eden, in bytes (stored in image file header)
		46	machine code zone size, in bytes (Cog only; otherwise nil)
		47	desired machine code zone size (stored in image file header; Cog only; otherwise nil)
		48	various header flags.  
			Bit 0: tells the VM that the image's Process class has threadId as its 5th inst var (after nextLink, suspendedContext, priority & myList)
			Bit 1: on Cog JIT VMs asks the VM to set the flag bit in interpreted methods
			Bit 2: if set, preempting a process puts it to the head of its run queue, not the back,
					i.e. preempting a process by a higher priority one will not cause the preempted process to yield
						to others at the same priority.
			Bit 3: in a muilt-threaded VM, if set, the Window system will only be accessed from the first VM thread
			Bit 4: if set, causes weaklings and ephemerons to be queued individually for finalization
			Bit 5: (on VMs that support it) if set, implies wheel events will be delivered as such and not mapped to arrow key events
			Bit 6: (on VMs that support it) whether the arithmetic primitives perform conversion in case of mixed SmallInteger/Float (not set) or fail (set)
				(on VMs that don't support it, those primitives will fail in those cases)
		49	max size the image promises to grow the external semaphore table to (0 sets to default, which is 256 as of writing)
		50-51 nil; reserved for VM parameters that persist in the image (such as eden above)
		52	root/remembered table capacity
		53	number of segments
		54	total size of free old space
		55	ratio of growth and image size at or above which a GC will be performed post scavenge
		56	number of process switches since startup (read-only)
		57	number of ioProcessEvents calls since startup (read-only)
		58	number of ForceInterruptCheck calls since startup (read-only)
		59	number of check event calls since startup (read-only)
		60	number of stack page overflows since startup (read-only)
		61	number of stack page divorces since startup (read-only)
		62	compiled code compactions since startup (read-only; Cog only; otherwise nil)
		63	total milliseconds in compiled code compactions since startup (read-only; Cog only; otherwise nil)
		64	the number of methods that currently have jitted machine-code
		65	whether the VM supports a certain feature, MULTIPLE_BYTECODE_SETS is bit 0, IMMUTABILITY is bit 1
		66	the byte size of a stack page
		67	the max allowed size of old space (0 implies no limit except that of the underlying platform)
		68	the average number of live stack pages when scanned by GC (at scavenge/gc/become et al) (read-write)
		69	the maximum number of live stack pages when scanned by GC (at scavenge/gc/become et al) (read-write)
		70	the vmProxyMajorVersion (the interpreterProxy VM_MAJOR_VERSION)
		71	the vmProxyMinorVersion (the interpreterProxy VM_MINOR_VERSION)
		72  total milliseconds in full GCs Mark phase since startup (read-only)
		73  total milliseconds in full GCs Sweep phase since startup (read-only, can be 0 depending on compactors)
		74  maximum pause time due to segment allocation"

	<primitive: 254>
	^nil! !

!SystemDictionary methodsFor: 'vm parameters' stamp: 'jmv 1/4/2024 16:30:34'!
vmParameterAt: parameterIndex put: newValue
	"parameterIndex is a positive integer corresponding to one of the VM's internal
	parameter/metric registers.  Store newValue (a positive integer) into that
	register and answer with the previous value that was stored there.
	Fail if newValue is out of range, if parameterIndex has no corresponding
	register, or if the corresponding register is read-only.

	As of mid 2017 the parameters which can be set are
		5	allocations between GCs (read-write; nil in Cog VMs)
		6	survivor count tenuring threshold (read-write)
		17	proportion of code zone available for use (Sista VMs only)
		23	bytes of extra memory to reserve for VM buffers, plugins, etc.
		24	memory threshold above whichto shrink object memory (read-write)
		25	memory headroom when growing object memory (read-write)
		26	interruptChecksEveryNms - force an ioProcessEvents every N milliseconds (read-write)
		34	bytes allocated in total since start-up or reset (read-write)
		43	desired number of stack pages (stored in image file header, max 65535; Cog VMs only, otherwise nil)
		45	desired size of eden, in bytes (stored in image file header; Cog VMs only, otherwise nil)
		47	desired size of machine code zone, in bytes (applies at startup only, stored in image file header; Cog JIT VM only)
		48	various properties of the Cog VM as an integer encoding an array of bit flags.
			Bit 0: tells the VM that the image's Process class has threadId as its 5th inst var (after nextLink, suspendedContext, priority & myList)
			Bit 1: on Cog JIT VMs asks the VM to set the flag bit in interpreted methods
			Bit 2: if set, preempting a process puts it to the head of its run queue, not the back,
					i.e. preempting a process by a higher priority one will not cause the preempted process to yield
						to others at the same priority.
			Bit 3: in a muilt-threaded VM, if set, the Window system will only be accessed from the first VM thread
			Bit 4: in if set, causes weaklings and ephemerons to be queued individually for finalization
			Bit 5: (on VMs that support it) if set, implies wheel events will be delivered as such and not mapped to arrow key events
			Bit 6: (on VMs that support it) whether the arithmetic primitives perform conversion in case of mixed SmallInteger/Float (not set) or fail (set)
				(on VMs that don't support it, those primitives will fail in those cases)
		49	the size of the external semaphore table (read-write; Cog VMs only)
		55	ratio of growth and image size at or above which a GC will be performed post scavenge
		67	the maximum allowed size of old space in bytes, 0 implies no internal limit."

	<primitive: 254>
	self primitiveFailed! !


!BytecodeEncoder methodsFor: 'opcode sizing' stamp: 'jmv 1/4/2024 16:12:33'!
sizeCallPrimitive: primitiveIndex
	^self sizeOpcodeSelector: #genCallPrimitive: withArguments: {primitiveIndex}! !

!BytecodeEncoder methodsFor: 'method generation' stamp: 'jmv 1/4/2024 16:12:19'!
computeMethodHeaderForNumArgs: numArgs numTemps: numTemps numLits: numLits primitive: primitiveIndex
	numArgs > 15
		ifTrue: [^self error: 'Cannot compile -- too many arguments'].
	numTemps > 63
		ifTrue: [^self error: 'Cannot compile -- too many temporary variables'].	
	numLits > 65535
		ifTrue: [^self error: 'Cannot compile -- too many literals'].
	^(CompiledMethod headerFlagForEncoder: self)
		+ (numArgs bitShift: 24)
		+ (numTemps bitShift: 18)
		"+ (largeBit bitShift: 17)" "largeBit gets filled in later"
		+ (primitiveIndex > 0 ifTrue: [1 bitShift: 16] ifFalse: [0])
		+ numLits! !


!EncoderForV3PlusClosures methodsFor: 'bytecode generation' stamp: 'jmv 1/4/2024 16:12:46'!
genCallPrimitive: primitiveIndex
	"139	11101111	iiiiiiii jjjjjjjj	Call Primitive #iiiiiiii + (jjjjjjjj * 256)"
	(primitiveIndex < 1 or: [primitiveIndex > 65535]) ifTrue: [
		self outOfRangeError: 'primitive index' index: primitiveIndex range: 1 to: 65535].
	stream
		nextPut: 139;
		nextPut: (primitiveIndex bitAnd: 255);
		nextPut: (primitiveIndex bitShift: -8)! !


!MethodNode methodsFor: 'code generation' stamp: 'jmv 1/4/2024 15:40:43'!
generate: trailer using: aCompiledMethodClass
	"The receiver is the root of a parse tree. Answer an instance of aCompiledMethodClass.
	 The argument, trailer, is arbitrary but is typically the reference to the source code
	 that is stored with every CompiledMethod."

	| primErrNode blkSize nLits locals literals stack header method |
	self generate: trailer
		using: aCompiledMethodClass
		ifQuick:
			[:m |
			 encoder noteBlockExtent: (0 to: 2) hasLocals: arguments.
			 m	literalAt: 2 put: encoder associationForClass;
				properties: properties.
			 ^m].
	primErrNode := self primitiveErrorVariableName ifNotNil:
						[encoder fixTemp: self primitiveErrorVariableName].
	self ensureClosureAnalysisDone.
	encoder rootNode: self. "this is for BlockNode>>sizeCodeForClosureValue:"
	blkSize := (block sizeCodeForEvaluatedValue: encoder)
				+ (primitive > 0
					ifTrue: [encoder sizeCallPrimitive: primitive]
					ifFalse: [0])
				+ (primErrNode
					ifNil: [0]
					ifNotNil:
						[primErrNode
							index: arguments size + temporaries size;
							sizeCodeForStore: encoder "The VM relies on storeIntoTemp: (129)"]).
	locals := arguments, temporaries, (primErrNode ifNil: [#()] ifNotNil: [{primErrNode}]).
	encoder noteBlockExtent: block blockExtent hasLocals: locals.
	header := encoder computeMethodHeaderForNumArgs: arguments size
					numTemps: locals size
					numLits: (nLits := (literals := encoder allLiterals) size)
					primitive: primitive.
	method := aCompiledMethodClass
					createMethod: blkSize
					trailer: trailer
					header: header.
	1 to: nLits do: [:lit | method literalAt: lit put: (literals at: lit)].
	encoder streamToMethod: method.
	stack := ParseStack new init.
	primitive > 0 ifTrue:
		[encoder genCallPrimitive: primitive.
		 primErrNode ifNotNil:
			[primErrNode emitCodeForStore: stack encoder: encoder]].
	stack position: method numTemps.
	[block emitCodeForEvaluatedValue: stack encoder: encoder]
		on: Error "If an attempt is made to write too much code the method will be asked"
		do: [:ex|  "to grow, and the grow attempt will fail in CompiledMethod class>>#new:"
			ex signalerContext sender method = (CompiledMethod class>>#new:)
				ifTrue: [^self error: 'Compiler code size discrepancy']
				ifFalse: [ex pass]].
	stack position ~= (method numTemps + 1) ifTrue:
		[^self error: 'Compiler stack discrepancy'].
	encoder methodStreamPosition ~= (method size - trailer size) ifTrue:
		[^self error: 'Compiler code size discrepancy'].
	method needsFrameSize: stack size - method numTemps.
	method properties: properties.
	^method! !


!Utilities class methodsFor: 'vm statistics' stamp: 'jmv 1/4/2024 16:32:05'!
vmStatisticsReportOn: str
	"
	(TextModel new contents: Utilities vmStatisticsReportString) openLabel: 'VM Statistics'
	"

	| params oldSpaceEnd youngSpaceEnd memorySize fullGCs fullGCTime fullGCCompactionTime incrGCs incrGCTime tenureCount upTime
	  upTime2 fullGCs2 fullGCTime2  incrGCs2 incrGCTime2 tenureCount2 freeSize youngSize used timeReporter idleMs idleMs2 codeCompactionTime codeCompactionsOrNil codeCompactionTime2 codeCompactions2 maxParams |
	"Pad to protect for a VM that does not answer the expected parameter array"
	maxParams := 63. "max position referenced in this method"
	params := Array streamContents: [ :strm |
				strm nextPutAll: Smalltalk getVMParameters.
				[strm position < maxParams] whileTrue: [strm nextPut: nil]].
	oldSpaceEnd			:= params at: 1.
	youngSpaceEnd		:= params at: 2.
	memorySize			:= params at: 3.
	fullGCs			:= params at: 7.
	fullGCTime			:= params at: 8.
	incrGCs			:= params at: 9.
	incrGCTime			:= params at: 10.
	tenureCount			:= params at: 11.
	fullGCCompactionTime	:= params at: 18.
	codeCompactionsOrNil	:= params at: 62.
	codeCompactionTime	:= params at: 63.
	
	upTime := (params at: 20) ~= 0 "utcMicrosecondClock at startup"
				ifTrue: [Time primUtcMicrosecondClock - (params at: 20) + 500 // 1000]
				ifFalse: [Time primMillisecondClock]. "Fall back on old microsecond clock; Good for 47.5 days"
	timeReporter := [:time| | seconds |
					seconds := time / 1000.
					seconds >= (60*60*24)
						ifTrue:
							[str print: seconds / 60 // 60 // 24; nextPut: $d; space;
								print: seconds / 60 // 60 \\ 24; nextPut: $h; space]
						ifFalse:
							[seconds >= (60*60) ifTrue:
								[str print: (seconds / 60 // 60); nextPut: $h; space]].
					str
						print: (seconds / 60 \\ 60) asInteger; nextPut: $m; space;
						print: (seconds \\ 60) asInteger; nextPut: $s].
	str	nextPutAll: 'uptime		'.
	timeReporter value: upTime.
	(idleMs := params at: 16) ~= 0 ifTrue:
		[str tab; nextPutAll: '(runtime '.
		 idleMs := idleMs // 1000.
		 timeReporter value: upTime - idleMs.
		 str nextPutAll: ', idletime '.
		 timeReporter value: idleMs.
		 str nextPut: $)].
	str newLine.

	str	nextPutAll: 'memory			';
		nextPutAll: (self formatVMParameter: memorySize); nextPutAll: ' bytes'; newLine.
	str	nextPutAll:	'	old		';
		nextPutAll: (self formatVMParameter: oldSpaceEnd); nextPutAll: ' bytes (';
		print: oldSpaceEnd / memorySize * 100 fractionDigits: 1; nextPutAll: '%)'; newLine.

	(youngSize := (params at: 19)  * 7 // 5) = 0 ifTrue: [params at: 44].
	 youngSize := youngSize roundUpTo: 1024.
	 str	nextPutAll: '	young		';
		nextPutAll: (self formatVMParameter: youngSize); nextPutAll: ' bytes (';
		print: youngSize / memorySize * 100 fractionDigits: 1; nextPutAll: '%)'; newLine.
	youngSize := youngSpaceEnd. "used eden"
	freeSize := (params at: 54) + (params at: 44) - youngSize.
	used := youngSize + oldSpaceEnd - freeSize.
	str	nextPutAll: '	used		';
		nextPutAll: (self formatVMParameter: used); nextPutAll: ' bytes (';
		print: used / memorySize * 100 fractionDigits: 1; nextPutAll: '%)'; newLine.
	str	nextPutAll: '	free		';
		nextPutAll: (self formatVMParameter: freeSize); nextPutAll: ' bytes (';
		print: freeSize / memorySize * 100 fractionDigits: 1; nextPutAll: '%)'; newLine.

	str	nextPutAll: 'GCs					';
		nextPutAll: (self formatVMParameter: (fullGCs + incrGCs)).
	fullGCs + incrGCs > 0 ifTrue:
		[str
			nextPutAll: ' ('; 
			print: upTime / (fullGCs + incrGCs) fractionDigits: 1; 
			nextPutAll: ' ms between GCs'.
		 idleMs > 0 ifTrue:
			[str
				space;
				print: upTime - idleMs / (fullGCs + incrGCs) fractionDigits: 1; 
				nextPutAll: ' ms runtime between GCs'].
		 str nextPut: $)].
	str newLine.
	str	nextPutAll: '	full				';
		nextPutAll: (self formatVMParameter: fullGCs); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: fullGCTime); nextPutAll: ' ms (';
		print: fullGCTime / (upTime - idleMs) * 100 fractionDigits: 2;
		nextPutAll: '% runtime)';
		nextPutAll: ', avg '; print: (fullGCTime / (fullGCs max: 1)) fractionDigits: 1; nextPutAll: ' ms'.
	fullGCCompactionTime ~= 0 ifTrue:
		[str	newLine; nextPutAll: '		marking			';
			nextPutAll: (self formatVMParameter: (fullGCTime - fullGCCompactionTime)); nextPutAll: ' ms (';
			print: (fullGCTime - fullGCCompactionTime) / fullGCTime * 100 fractionDigits: 1;
			nextPutAll: '%) avg '; print: (fullGCTime - fullGCCompactionTime / (fullGCs max: 1)) fractionDigits: 1;
			nextPutAll: ' ms,'; newLine;
			nextPutAll: '		compacting			';
			nextPutAll: (self formatVMParameter: fullGCCompactionTime); nextPutAll: ' ms (';
			print: fullGCCompactionTime / fullGCTime * 100 fractionDigits: 1;
			nextPutAll: '%) avg '; print: fullGCCompactionTime / (fullGCs max: 1) fractionDigits: 1;
			nextPutAll: ' ms'].
	str	newLine.
	str	nextPutAll: '	scavenges				';
		nextPutAll: (self formatVMParameter: incrGCs); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: incrGCTime); nextPutAll: ' ms (';
		print: incrGCTime / (upTime - idleMs) * 100 fractionDigits: 2;
		nextPutAll: '% runtime), avg '; print: incrGCTime / incrGCs fractionDigits: 1; nextPutAll: ' ms'; newLine.
	str	nextPutAll: '	tenures				';
		nextPutAll: (self formatVMParameter: tenureCount).
	tenureCount = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: tenureCount // (incrGCs max: 1); nextPutAll: ' tenures per scavenge)'].
	str	newLine.

	codeCompactionsOrNil ifNotNil:
		[str
			nextPutAll: '	code compactions				'.
		codeCompactionsOrNil = 0
			ifTrue: [str
				nextPutAll: codeCompactionsOrNil printString; newLine]
			ifFalse: [str
				nextPutAll: (self formatVMParameter: codeCompactionsOrNil); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: codeCompactionTime); nextPutAll: ' ms (';
				print: codeCompactionTime / (upTime - idleMs) * 100 fractionDigits: 3;
				nextPutAll: '% runtime), avg '; print: codeCompactionTime / codeCompactionsOrNil fractionDigits: 1; nextPutAll: ' ms'; newLine]].

LastStats ifNil: [LastStats := Array new: 9]
ifNotNil: [
	LastStats size < 9 ifTrue:
		[LastStats := LastStats, (Array new: 9 - LastStats size withAll: 0)].
	upTime2 := upTime - (LastStats at: 1).
	fullGCs2 := fullGCs - (LastStats at: 2).
	fullGCTime2 := fullGCTime - (LastStats at: 3).
	incrGCs2 := incrGCs - (LastStats at: 4).
	incrGCTime2 := incrGCTime - (LastStats at: 5).
	tenureCount2 := tenureCount - (LastStats at: 6).
	idleMs2 := idleMs - (LastStats at: 7).
	codeCompactionsOrNil ifNotNil:
		[codeCompactions2	:= codeCompactionsOrNil - (LastStats at: 8).
		 codeCompactionTime2	:= codeCompactionTime - (LastStats at: 9)].

	str	nextPutAll: 'GCs since last view					';
		nextPutAll: (self formatVMParameter: (fullGCs2 + incrGCs2)).
	fullGCs2 + incrGCs2 > 0 ifTrue:
		[str
			nextPutAll: ' ('; 
			print: upTime2 // (fullGCs2 + incrGCs2); 
			nextPutAll: ' ms between GCs'.
			idleMs2 > 0 ifTrue:
				[str
					nextPutAll: ', ';
					print: upTime2 - idleMs2 // (fullGCs2 + incrGCs2);
					nextPutAll: ' ms runtime between GCs'].
		 str nextPut: $)].
	str	newLine.
	str	nextPutAll: '	uptime				'; print: (upTime2 / 1000.0) fractionDigits: 1;
		nextPutAll: ' s, runtime ';		   print: (upTime2 - idleMs2 / 1000.0) fractionDigits: 1;
		nextPutAll: ' s, idletime ';		   print: (idleMs2 / 1000.0) fractionDigits: 1; nextPutAll: ' s'; newLine.
	str	nextPutAll: '	full				';
		nextPutAll: (self formatVMParameter: fullGCs2).
	fullGCs2 + fullGCTime2 > 0 ifTrue:
		[str	nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: fullGCTime2); nextPutAll: ' ms (';
			print: fullGCTime2 / (upTime2 - idleMs2) * 100 fractionDigits: 1;
			nextPutAll: '% runtime)'.
		 str	nextPutAll: ', avg '; print: fullGCTime2 / (fullGCs2 max: 1) fractionDigits: 1; nextPutAll: ' ms'].
	str	newLine.
	str	nextPutAll: '	scavenges				';
		nextPutAll: (self formatVMParameter: incrGCs2); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: incrGCTime2); nextPutAll: ' ms (';
		print: incrGCTime2 / (upTime2 - idleMs2) * 100 fractionDigits: 1;
		nextPutAll: '% runtime), avg '; print: incrGCTime2 / (incrGCs2 max: 1) fractionDigits: 1; nextPutAll: ' ms'.
	str	newLine.
	str	nextPutAll: '	tenures				';
		nextPutAll: (self formatVMParameter: tenureCount2).
	tenureCount2 = 0 ifFalse:
		[str nextPutAll: ' (avg '; print: tenureCount2 // (incrGCs2 max: 1); nextPutAll: ' tenures per scavenge)'].
	str	newLine.
	codeCompactionsOrNil ifNotNil:
		[str	nextPutAll: '	code compactions				';
			nextPutAll: (self formatVMParameter: codeCompactions2); nextPutAll: ' totalling '; nextPutAll: (self formatVMParameter: codeCompactionTime2); nextPutAll: ' ms (';
			print: codeCompactionTime2 / (upTime2 - idleMs2) * 100 fractionDigits: 3;
			nextPutAll: '% runtime)'.
		 codeCompactions2 > 0 ifTrue:
			[str nextPutAll: ', avg '; print: codeCompactionTime2 / codeCompactions2 fractionDigits: 1; nextPutAll: ' ms'; newLine.]]
].
	LastStats := { upTime. fullGCs. fullGCTime. incrGCs. incrGCTime. tenureCount. idleMs. codeCompactionsOrNil. codeCompactionTime }.! !


!SpaceTally methodsFor: 'instance size' stamp: 'jmv 1/4/2024 15:20:50'!
spaceForInstancesOf: aClass
	"Answer a pair of the number of bytes consumed by all instances of the
	 given class, including their object headers, and the number of instances."

	| instances total |
	instances := aClass allInstances.
	instances isEmpty ifTrue: [^#(0 0)].
	total := 0.
	aClass isVariable
		ifTrue:
			[instances do:
				[:i| total := total + (aClass byteSizeOfInstanceOfSize: i basicSize)]]
		ifFalse:
			[total := instances size * aClass byteSizeOfInstance].
	^{ total. instances size }! !

!SpaceTally methodsFor: 'fileOut' stamp: 'jmv 1/4/2024 15:20:08'!
printSpaceAnalysis: threshold on: aStream
	"
	SpaceTally new printSpaceAnalysis: 1 on:(FileStream forceNewFileNamed: 'STspace.text')
	"
	"sd-This method should be rewrote to be more coherent within the rest of the class 
	ie using preAllocate and spaceForInstanceOf:"

	"If threshold > 0, then only those classes with more than that number
	of instances will be shown, and they will be sorted by total instance space.
	If threshold = 0, then all classes will appear, sorted by name."

	| codeSpace instCount instSpace totalCodeSpace totalInstCount totalInstSpace eltSize n totalPercent percent |
	Smalltalk garbageCollect.
	totalCodeSpace := totalInstCount := totalInstSpace := n := 0.
	results := OrderedCollection new: Smalltalk classNames size.
	'Taking statistics...'
		displayProgressAt: Sensor mousePoint
		from: 0 to: Smalltalk classNames size
		during: [ :barBlock |
			Smalltalk allClassesDo: [ :cl |
				codeSpace := cl spaceUsed.
				barBlock value: (n := n+1).
				Smalltalk garbageCollectMost.
				instCount := cl instanceCount.
				instSpace := 8 * instCount. "Object headers""Warning: The 3rd header word for big objects is not considered!!"
				cl isVariable
					ifTrue: [
						eltSize := cl isBytes ifTrue: [1] ifFalse: [4].
						cl allInstancesDo: [ :x |
							instSpace := instSpace + (x basicSize * eltSize)]]
					ifFalse: [instSpace := instSpace + (cl instSize * instCount * 4)].
				results add: (SpaceTallyItem analyzedClassName: cl name codeSize: codeSpace instanceCount:  instCount spaceForInstances: instSpace).
				totalCodeSpace := totalCodeSpace + codeSpace.
				totalInstCount := totalInstCount + instCount.
				totalInstSpace := totalInstSpace + instSpace]].
	totalPercent := 0.0.

	aStream timeStamp.
	aStream
		nextPutAll: ('Class' padded: #right to: 30 with: $ );
		nextPutAll: ('code space' padded: #left to: 12 with: $ );
		nextPutAll: ('# instances' padded: #left to: 12 with: $ );
		nextPutAll: ('inst space' padded: #left to: 12 with: $ );
		nextPutAll: ('percent' padded: #left to: 8 with: $ ); newLine.

	threshold > 0 ifTrue: [
		"If inst count threshold > 0, then sort by space"
		results := (results select: [:s | s instanceCount >= threshold or: [s spaceForInstances > (totalInstSpace // 500)]])
			asArray sort: [:s :s2 | s spaceForInstances > s2 spaceForInstances]].

	results do: [:s |
		aStream
			nextPutAll: (s analyzedClassName padded: #right to: 30 with: $ );
			nextPutAll: (s codeSize printString padded: #left to: 12 with: $ );
			nextPutAll: (s instanceCount printString padded: #left to: 12 with: $ );
			nextPutAll: (s spaceForInstances printString padded: #left to: 14 with: $ ).
		percent := s spaceForInstances*100.0/totalInstSpace.
		totalPercent := totalPercent + percent.
		percent >= 0.1 ifTrue: [
			percent printOn: aStream integerDigits: 6 padWith: $  fractionDigits: 1 positiveIndicator: nil ].
		aStream newLine].

	aStream
		newLine; nextPutAll: ('Total' padded: #right to: 30 with: $ );
		nextPutAll: (totalCodeSpace printString padded: #left to: 12 with: $ );
		nextPutAll: (totalInstCount printString padded: #left to: 12 with: $ );
		nextPutAll: (totalInstSpace printString padded: #left to: 14 with: $ ).
	totalPercent printOn: aStream integerDigits: 6 padWith: $  fractionDigits: 1 positiveIndicator: nil! !

!methodRemoval: SpaceTally #spaceForInstancesOfSpur: stamp: 'jmv 1/4/2024 15:20:56'!
SpaceTally removeSelector: #spaceForInstancesOfSpur:!
!methodRemoval: SpaceTally #spaceForInstancesOfPreSpur: stamp: 'jmv 1/4/2024 15:21:01'!
SpaceTally removeSelector: #spaceForInstancesOfPreSpur:!
!methodRemoval: MethodNode #generatePreSpur:using: stamp: 'jmv 1/4/2024 15:41:02'!
MethodNode removeSelector: #generatePreSpur:using:!
!methodRemoval: MethodNode #generateSpur:using: stamp: 'jmv 1/4/2024 15:40:46'!
MethodNode removeSelector: #generateSpur:using:!
!methodRemoval: SystemDictionary #compactClassesArray stamp: 'jmv 1/4/2024 16:08:00'!
SystemDictionary removeSelector: #compactClassesArray!
!methodRemoval: SystemDictionary #isSpur stamp: 'jmv 1/4/2024 15:47:35'!
SystemDictionary removeSelector: #isSpur!
!methodRemoval: SystemDictionary #cleanCompactObsoleteClasses stamp: 'jmv 1/4/2024 15:19:02'!
SystemDictionary removeSelector: #cleanCompactObsoleteClasses!
!methodRemoval: SystemDictionary #compactClassesArrayIncludes: stamp: 'jmv 1/4/2024 16:07:51'!
SystemDictionary removeSelector: #compactClassesArrayIncludes:!
!methodRemoval: CompiledMethod class #newPreSpurBytes:trailerBytes:nArgs:nTemps:nStack:nLits:primitive: stamp: 'jmv 1/4/2024 15:38:01'!
CompiledMethod class removeSelector: #newPreSpurBytes:trailerBytes:nArgs:nTemps:nStack:nLits:primitive:!
!methodRemoval: CompiledMethod class #newSpurBytes:trailerBytes:nArgs:nTemps:nStack:nLits:primitive: stamp: 'jmv 1/4/2024 15:37:22'!
CompiledMethod class removeSelector: #newSpurBytes:trailerBytes:nArgs:nTemps:nStack:nLits:primitive:!
!methodRemoval: Character class #definition stamp: 'jmv 1/4/2024 15:52:57'!
Character class removeSelector: #definition!
!methodRemoval: Character class #immediateNumericValue: stamp: 'jmv 1/4/2024 16:18:01'!
Character class removeSelector: #immediateNumericValue:!
!methodRemoval: Character class #nonImmediateNumericValue: stamp: 'jmv 1/4/2024 16:18:03'!
Character class removeSelector: #nonImmediateNumericValue:!
!methodRemoval: Character #nonImmediateNumericValue stamp: 'jmv 1/4/2024 16:16:44'!
Character removeSelector: #nonImmediateNumericValue!
!methodRemoval: Character #immediateNumericValue stamp: 'jmv 1/4/2024 16:16:46'!
Character removeSelector: #immediateNumericValue!
!methodRemoval: ClassBuilder #computeFormat:instSize:forSuper:ccIndex: stamp: 'jmv 1/4/2024 15:17:55'!
ClassBuilder removeSelector: #computeFormat:instSize:forSuper:ccIndex:!
!methodRemoval: ClassBuilder #format:variable:words:pointers:weak: stamp: 'jmv 1/4/2024 15:55:54'!
ClassBuilder removeSelector: #format:variable:words:pointers:weak:!
!methodRemoval: Class #becomeCompactSimplyAt: stamp: 'jmv 1/4/2024 15:13:37'!
Class removeSelector: #becomeCompactSimplyAt:!
!methodRemoval: Class #becomeCompact stamp: 'jmv 1/4/2024 15:13:29'!
Class removeSelector: #becomeCompact!
!methodRemoval: Class #becomeUncompact stamp: 'jmv 1/4/2024 15:14:08'!
Class removeSelector: #becomeUncompact!
!methodRemoval: ClassDescription #updateInstancesFrom: stamp: 'jmv 1/4/2024 15:15:00'!
ClassDescription removeSelector: #updateInstancesFrom:!
!methodRemoval: Behavior #typeOfClassSpur stamp: 'jmv 1/4/2024 15:32:38'!
Behavior removeSelector: #typeOfClassSpur!
!methodRemoval: Behavior #kindOfSubclassSpur stamp: 'jmv 1/4/2024 15:31:59'!
Behavior removeSelector: #kindOfSubclassSpur!
!methodRemoval: Behavior #indexIfCompact stamp: 'jmv 1/4/2024 15:21:18'!
Behavior removeSelector: #indexIfCompact!
!methodRemoval: Behavior #primitiveIdentityHash stamp: 'jmv 1/4/2024 15:11:47'!
Behavior removeSelector: #primitiveIdentityHash!
!methodRemoval: Behavior #isVariableSpur stamp: 'jmv 1/4/2024 15:10:46'!
Behavior removeSelector: #isVariableSpur!
!methodRemoval: Behavior #isVariablePreSpur stamp: 'jmv 1/4/2024 15:10:43'!
Behavior removeSelector: #isVariablePreSpur!
!methodRemoval: Behavior #typeOfClassPreSpur stamp: 'jmv 1/4/2024 15:32:42'!
Behavior removeSelector: #typeOfClassPreSpur!
!methodRemoval: Behavior #kindOfSubclassPreSpur stamp: 'jmv 1/4/2024 15:32:04'!
Behavior removeSelector: #kindOfSubclassPreSpur!
!methodRemoval: SmallInteger class #definition stamp: 'jmv 1/4/2024 15:49:55'!
SmallInteger class removeSelector: #definition!
!methodRemoval: SmallFloat64 class #definition stamp: 'jmv 1/4/2024 15:50:45'!
SmallFloat64 class removeSelector: #definition!
