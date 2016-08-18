'From Cuis 4.2 of 25 July 2013 [latest update: #2880] on 11 August 2016 at 7:43:47 pm'!

!UndefinedObject class methodsFor: 'accessing' stamp: 'nice 11/14/2009 19:22'!
allInstancesDo: aBlock
	"It is well known there is a single instance"
	
	aBlock value: nil! !


!Behavior methodsFor: 'accessing' stamp: 'jmv 8/11/2016 19:17:43'!
typeOfClassPreSpur
	"Answer a symbol uniquely describing the type of the receiver"
	self instSpec = CompiledMethod instSpec ifTrue:[^#compiledMethod]. "Very special!!"
	self isBytes ifTrue:[^#bytes].
	(self isWords and:[self isPointers not]) ifTrue:[^#words].
	self isWeak ifTrue:[^#weak].
	self isVariable ifTrue:[^#variable].
	^#normal.! !

!Behavior methodsFor: 'accessing' stamp: 'jmv 8/11/2016 19:17:49'!
typeOfClassSpur
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

!Behavior methodsFor: 'testing' stamp: 'jmv 8/11/2016 19:18:59'!
isVariablePreSpur
	"Answer whether the receiver has indexable variables."

	^ self instSpec >= 2! !

!Behavior methodsFor: 'testing' stamp: 'jmv 8/11/2016 19:19:05'!
isVariableSpur
	"Answer whether the receiver has indexable variables.
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
	| instSpec |
	instSpec := self instSpec.
	^instSpec >= 2 and: [instSpec <= 4 or: [instSpec >= 9]]! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'jmv 8/11/2016 19:24:22'!
allInstancesPreSpur
	"Answer a collection of all current instances of the receiver."

	| all |
	all _ OrderedCollection new.
	self allInstancesDo: [:x | x == all ifFalse: [all add: x]].
	^ all asArray
! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'jmv 8/11/2016 19:24:39'!
allInstancesSpur
	"Answer all instances of the receiver."
	<primitive: 177>
	"The primitive can fail because memory is low.  If so, fall back on the old
	 enumeration code, which gives the system a chance to GC and/or grow.
	 Because aBlock might change the class of inst (for example, using become:),
	 it is essential to compute next before aBlock value: inst."
	| inst insts next |
	insts := WriteStream on: (Array new: 64).
	inst := self someInstance.
	[inst == nil] whileFalse:
		[next := inst nextInstance.
		 (inst == insts or: [inst == insts originalContents]) ifFalse: [insts nextPut: inst].
		 inst := next].
	^insts contents! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'jmv 8/11/2016 19:25:48'!
byteSizeOfInstanceOfSize: basicSize
	"Answer the total memory size of an instance of the receiver
	 with the given number of indexable instance variables."
	"Only for Spur"

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

!Behavior methodsFor: 'testing class hierarchy' stamp: 'jmv 8/11/2016 19:29:04'!
kindOfSubclassPreSpur
	"Answer a String that is the keyword that describes the receiver's kind 
	of subclass, either a regular subclass, a variableSubclass, a  
	variableByteSubclass, a variableWordSubclass, or a weakSubclass."
	self isWeak
		ifTrue: [^ ' weakSubclass: '].
	^ self isVariable
		ifTrue: [self isBits
				ifTrue: [self isBytes
						ifTrue: [ ' variableByteSubclass: ']
						ifFalse: [ ' variableWordSubclass: ']]
				ifFalse: [ ' variableSubclass: ']]
		ifFalse: [ ' subclass: ']! !

!Behavior methodsFor: 'testing class hierarchy' stamp: 'jmv 8/11/2016 19:29:13'!
kindOfSubclassSpur
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

!Behavior methodsFor: 'enumerating' stamp: 'jmv 8/11/2016 19:35:12'!
allInstancesPreSpurDo: aBlock 
	"Evaluate the argument, aBlock, for each of the current instances of the 
	receiver.
	
	Because aBlock might change the class of inst (for example, using become:),
	it is essential to compute next before aBlock value: inst."
	| inst next |
	inst := self someInstance.
	[inst == nil]
		whileFalse:
		[
		next := inst nextInstance.
		aBlock value: inst.
		inst := next]! !

!Behavior methodsFor: 'enumerating' stamp: 'jmv 8/11/2016 19:35:17'!
allInstancesSpurDo: aBlock
	"Evaluate aBlock with each of the current instances of the receiver."
	| instances inst next |
	instances := self allInstancesOrNil.
	instances ifNotNil:
		[instances do: aBlock.
		 ^self].
	"allInstancesOrNil can fail because memory is low.  If so, fall back on the old
	 enumeration code.  Because aBlock might change the class of inst (for example,
	 using become:), it is essential to compute next before aBlock value: inst."
	inst := self someInstance.
	[inst == nil] whileFalse:
		[next := inst nextInstance.
		 aBlock value: inst.
		 inst := next]! !


!MethodContext class methodsFor: 'private' stamp: 'jmv 8/11/2016 19:41:14'!
allInstancesPreSpurDo: aBlock
	"Only count until thisContext since evaluation of aBlock will create new contexts."
	| inst next |
	inst := self someInstance.
	[inst == thisContext] whileFalse:[
		next := inst nextInstance.
		aBlock value: inst.
		inst := next]
! !

!MethodContext class methodsFor: 'private' stamp: 'jmv 8/11/2016 19:42:00'!
allInstancesSpurDo: aBlock
	"Evaluate aBlock with each of the current instances of the receiver."
	| instances inst next |
	instances := self allInstancesOrNil.
	instances ifNotNil:
		[instances do: aBlock.
		 ^self].
	"allInstancesOrNil can fail because memory is low.  If so, fall back on the old
	 enumeration code.  Because aBlock might change the class of inst (for example,
	 using become:), it is essential to compute next before aBlock value: inst.
	 Only count until thisContext since evaluation of aBlock will create new contexts."
	inst := self someInstance.
	[inst == thisContext or: [inst == nil]] whileFalse:
		[next := inst nextInstance.
		 aBlock value: inst.
		 inst := next]! !


!Behavior methodsFor: 'accessing' stamp: 'jmv 8/11/2016 19:18:29'!
typeOfClass
	"Answer a symbol uniquely describing the type of the receiver. c.f. kindOfSubclass, instSpec"
	^ Smalltalk isSpur
		ifTrue: [ self typeOfClassSpur ]
		ifFalse: [ self typeOfClassPreSpur ]! !

!Behavior methodsFor: 'testing' stamp: 'jmv 8/11/2016 19:19:28'!
isVariable
	"Answer whether the receiver has indexable variables."
	^ Smalltalk isSpur
		ifTrue: [ self isVariableSpur ]
		ifFalse: [ self isVariablePreSpur ]! !

!Behavior methodsFor: 'instance creation' stamp: 'topa 10/7/2015 20:41'!
basicNew: sizeRequested
	"Primitive. Answer an instance of this class with the number of indexable
	 variables specified by the argument, sizeRequested.  Fail if this class is not
	 indexable or if the argument is not a positive Integer, or if there is not
	 enough memory available. Essential. See Object documentation whatIsAPrimitive.
	
	 If the primitive fails because space is low then the scavenger will run before the
	 method is activated.  Check args and retry via handleFailingBasicNew: if they're OK."

	<primitive: 71 error: ec>
	(ec == #'insufficient object memory' or: [ec == #'bad argument']) ifTrue:
		[^self handleFailingBasicNew: sizeRequested].
	self isVariable ifFalse:
		[self error: self printString, ' cannot have variable sized instances'].
	self primitiveFailed! !

!Behavior methodsFor: 'accessing instances and variables' stamp: 'jmv 8/11/2016 19:25:07'!
allInstances 
	"Answer a collection of all current instances of the receiver."

	^ Smalltalk isSpur 
		ifTrue: [ self allInstancesSpur ]
		ifFalse: [ self allInstancesPreSpur ]! !

!Behavior methodsFor: 'testing class hierarchy' stamp: 'jmv 8/11/2016 19:31:24'!
kindOfSubclass
	"Answer a String that is the keyword that describes the receiver's kind of subclass,
	 either a regular subclass, a variableSubclass, a variableByteSubclass,
	 a variableWordSubclass, a weakSubclass, an ephemeronSubclass or an immediateSubclass
	(the last 2, only if Spur).
	 c.f. typeOfClass & instSpec"

	^ Smalltalk isSpur
		ifTrue: [ self kindOfSubclassSpur ]
		ifFalse: [ self kindOfSubclassPreSpur ]! !

!Behavior methodsFor: 'enumerating' stamp: 'jmv 8/11/2016 19:36:16'!
allInstancesDo: aBlock
	"Evaluate aBlock with each of the current instances of the receiver."
	Smalltalk isSpur
		ifTrue: [ self allInstancesSpurDo: aBlock ]
		ifFalse: [ self allInstancesPreSpurDo: aBlock ]! !

!Behavior methodsFor: 'private' stamp: 'jmv 8/11/2016 19:39:02'!
indexIfCompact
	"If these 5 bits are non-zero, then instances of this class
	will be compact.  It is crucial that there be an entry in
	Smalltalk compactClassesArray for any class so optimized.
	See the msgs becomeCompact and becomeUncompact."

	"Spur doesn't allow Compact Classes"
	Smalltalk isSpur ifTrue: [ ^ 0 ].

	^ (format bitShift: -11) bitAnd: 16r1F
"
Smalltalk compactClassesArray withIndexDo: 
	[:c :i | c == nil ifFalse:
		[c indexIfCompact = i ifFalse: [self halt]]]
"! !

!methodRemoval: MethodContext class #allInstancesDo:!
MethodContext class removeSelector: #allInstancesDo:!
!methodRemoval: MethodContext class #allInstancesDoPreSpur:!
MethodContext class removeSelector: #allInstancesDoPreSpur:!
!methodRemoval: Behavior #basicNewPreSpur:!
Behavior removeSelector: #basicNewPreSpur:!
