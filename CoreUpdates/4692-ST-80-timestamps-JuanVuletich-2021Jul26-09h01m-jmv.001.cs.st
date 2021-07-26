'From Cuis 5.0 [latest update: #4691] on 26 July 2021 at 9:05:09 am'!

!Object methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
yourself
	"Answer self."! !

!Object methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
errorImproperStore
	"Create an error notification that an improper store was attempted."

	self error: 'Improper store into indexable object'! !

!Object methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
errorNonIntegerIndex
	"Create an error notification that an improper object was used as an index."

	self error: 'only integers should be used as indices'! !

!Object methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
species
	"Answer the preferred class for reconstructing the receiver.  For example, 
	collections create new collections whenever enumeration messages such as 
	collect: or select: are invoked.  The new kind of collection is determined by 
	the species of the original collection.  Species and class are not always the 
	same.  For example, the species of Interval is Array."

	^self class! !


!Object class methodsFor: 'documentation' stamp: 'ST-80 5/31/1983 9:10:35'!
whatIsAPrimitive
	"Some messages in the system are responded to primitively. A primitive   
	response is performed directly by the interpreter rather than by evaluating   
	expressions in a method. The methods for these messages indicate the   
	presence of a primitive response by including <primitive: xx> before the   
	first expression in the method.   
	  
	Primitives exist for several reasons. Certain basic or 'primitive' 
	operations cannot be performed in any other way. Smalltalk without 
	primitives can move values from one variable to another, but cannot add two 
	SmallIntegers together. Many methods for arithmetic and comparison 
	between numbers are primitives. Some primitives allow Smalltalk to 
	communicate with I/O devices such as the disk, the display, and the keyboard. 
	Some primitives exist only to make the system run faster; each does the same 
	thing as a certain Smalltalk method, and its implementation as a primitive is 
	optional.  
	  
	When the Smalltalk interpreter begins to execute a method which specifies a 
	primitive response, it tries to perform the primitive action and to return a 
	result. If the routine in the interpreter for this primitive is successful, 
	it will return a value and the expressions in the method will not be evaluated. 
	If the primitive routine is not successful, the primitive 'fails', and the 
	Smalltalk expressions in the method are executed instead. These 
	expressions are evaluated as though the primitive routine had not been 
	called.  
	  
	The Smalltalk code that is evaluated when a primitive fails usually 
	anticipates why that primitive might fail. If the primitive is optional, the 
	expressions in the method do exactly what the primitive would have done (See 
	Number @). If the primitive only works on certain classes of arguments, the 
	Smalltalk code tries to coerce the argument or appeals to a superclass to find 
	a more general way of doing the operation (see SmallInteger +). If the 
	primitive is never supposed to fail, the expressions signal an error (see 
	SmallInteger asFloat).  
	  
	Each method that specifies a primitive has a comment in it. If the primitive is 
	optional, the comment will say 'Optional'. An optional primitive that is not 
	implemented always fails, and the Smalltalk expressions do the work 
	instead.  
	 
	If a primitive is not optional, the comment will say, 'Essential'. Some 
	methods will have the comment, 'No Lookup'. See Object 
	howToModifyPrimitives for an explanation of special selectors which are 
	not looked up.  
	  
	For the primitives for +, -, *, and bitShift: in SmallInteger, and truncated 
	in Float, the primitive constructs and returns a 16-bit 
	LargePositiveInteger when the result warrants it. Returning 16-bit 
	LargePositiveIntegers from these primitives instead of failing is 
	optional in the same sense that the LargePositiveInteger arithmetic 
	primitives are optional. The comments in the SmallInteger primitives say, 
	'Fails if result is not a SmallInteger', even though the implementor has the 
	option to construct a LargePositiveInteger. For further information on 
	primitives, see the 'Primitive Methods' part of the chapter on the formal 
	specification of the interpreter in the Smalltalk book."

	self error: 'comment only'! !


!Switch methodsFor: 'state' stamp: 'ST-80 5/31/1983 9:10:35'!
switch
	"Change the state of the receiver from 'on' to 'off' or from 'off' to 'on' (see 
	Switch|turnOn, Switch|turnOff)."

	self isOn
		ifTrue: [self turnOff]
		ifFalse: [self turnOn]! !


!Switch class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
new
	"Answer an instance of me such that the on and off actions are set to nil
	('no action'), and the state is set to 'off'."

	^self newOff! !


!Boolean methodsFor: 'logical operations' stamp: 'ST-80 5/31/1983 9:10:35'!
eqv: aBoolean 
	"Answer true if the receiver is equivalent to aBoolean."

	^self == aBoolean! !

!Boolean methodsFor: 'copying' stamp: 'ST-80 5/31/1983 9:10:35'!
shallowCopy 
	"Receiver has two concrete subclasses, True and False.
	Only one instance of each should be made, so return self."! !


!Boolean class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
new
	self error: 'You may not create any more Booleans - this is two-valued logic'! !


!False methodsFor: 'logical operations' stamp: 'ST-80 5/31/1983 9:10:35'!
& alternativeObject 
	"Evaluating conjunction -- answer false since receiver is false."

	^self! !

!False methodsFor: 'logical operations' stamp: 'ST-80 5/31/1983 9:10:35'!
not
	"Negation -- answer true since the receiver is false."

	^true! !

!False methodsFor: 'logical operations' stamp: 'ST-80 5/31/1983 9:10:35'!
| aBoolean 
	"Evaluating disjunction (OR) -- answer with the argument, aBoolean."

	^aBoolean! !

!False methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
and: alternativeBlock 
	"Nonevaluating conjunction -- answer with false since the receiver is false."

	^self! !

!False methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
ifFalse: alternativeBlock 
	"Answer the value of alternativeBlock. Execution does not actually
	reach here because the expression is compiled in-line."

	^alternativeBlock value! !

!False methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock 
	"Answer the value of falseAlternativeBlock. Execution does not
	actually reach here because the expression is compiled in-line."

	^falseAlternativeBlock value! !

!False methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
ifTrue: alternativeBlock 
	"Since the condition is false, answer the value of the false alternative, 
	which is nil. Execution does not actually reach here because the
	expression is compiled in-line."

	^nil! !

!False methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock 
	"Answer the value of falseAlternativeBlock. Execution does not
	actually reach here because the expression is compiled in-line."

	^falseAlternativeBlock value! !

!False methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
or: alternativeBlock 
	"Nonevaluating disjunction -- answer value of alternativeBlock."

	^alternativeBlock value! !


!True methodsFor: 'logical operations' stamp: 'ST-80 5/31/1983 9:10:35'!
& alternativeObject 
	"Evaluating conjunction -- answer alternativeObject since receiver is true."

	^alternativeObject! !

!True methodsFor: 'logical operations' stamp: 'ST-80 5/31/1983 9:10:35'!
not
	"Negation--answer false since the receiver is true."

	^false! !

!True methodsFor: 'logical operations' stamp: 'ST-80 5/31/1983 9:10:35'!
| aBoolean 
	"Evaluating disjunction (OR) -- answer true since the receiver is true."

	^self! !

!True methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
and: alternativeBlock 
	"Nonevaluating conjunction -- answer the value of alternativeBlock since
	the receiver is true."

	^alternativeBlock value! !

!True methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
ifFalse: alternativeBlock 
	"Since the condition is true, the value is the true alternative, which is nil. 
	Execution does not actually reach here because the expression is compiled 
	in-line."

	^nil! !

!True methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock 
	"Answer the value of trueAlternativeBlock. Execution does not 
	actually reach here because the expression is compiled in-line."

	^trueAlternativeBlock value! !

!True methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
ifTrue: alternativeBlock 
	"Answer the value of alternativeBlock. Execution does not actually 
	reach here because the expression is compiled in-line."

	^alternativeBlock value! !

!True methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock 
	"Answer with the value of trueAlternativeBlock. Execution does not 
	actually reach here because the expression is compiled in-line."

	^trueAlternativeBlock value! !

!True methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
or: alternativeBlock 
	"Nonevaluating disjunction -- answer true since the receiver is true."

	^self! !


!UndefinedObject methodsFor: 'copying' stamp: 'ST-80 5/31/1983 9:10:35'!
shallowCopy
	"Only one instance of UndefinedObject should ever be made, so answer 
	with self."! !


!UndefinedObject class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
new
	self error: 'You may not create any more undefined objects--use nil'! !


!Behavior methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
isFixed
	"Answer whether the receiver does not have a variable (indexable) part."

	^self isVariable not! !

!Behavior methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
isPointers
	"Answer whether the receiver contains just pointers (not bits)."

	^self isBits not! !

!Behavior methodsFor: 'compiling' stamp: 'ST-80 5/31/1983 9:10:35'!
compileAll
	^ self compileAllFrom: self! !

!Behavior methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
flushCache
	"Tell the interpreter to remove the contents of its method lookup cache, if it has 
	one.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 89>
	self primitiveFailed! !


!ClassDescription methodsFor: 'fileIn/Out' stamp: 'ST-80 5/31/1983 9:10:35'!
fileOutOn: aFileStream 
	"File a description of the receiver on aFileStream."

	self fileOutOn: aFileStream
		moveSource: false
		toFile: 0! !

!ClassDescription methodsFor: 'instance variables' stamp: 'ST-80 5/31/1983 9:10:35'!
addInstVarName: aString 
	"Add the argument, aString, as one of the receiver's instance variables."

	self subclassResponsibility! !

!ClassDescription methodsFor: 'organization' stamp: 'ST-80 5/31/1983 9:10:35'!
category
	"Answer the system organization category for the receiver."

	^SystemOrganization categoryOfElement: self name! !

!ClassDescription methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
errorCategoryName
	self error: 'Category name must be a String'! !


!Metaclass methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
isMeta
	^ true! !

!Metaclass methodsFor: 'instance variables' stamp: 'ST-80 5/31/1983 9:10:35'!
addInstVarName: aString 
	"Add the argument, aString, as one of the receiver's instance variables."

	| fullString |
	fullString _ aString.
	self instVarNames do: [:aString2 | fullString _ aString2 , ' ' , fullString].
	self instanceVariableNames: fullString! !

!Metaclass methodsFor: 'pool variables' stamp: 'ST-80 5/31/1983 9:10:35'!
classPool
	"Answer the dictionary of class variables."

	^thisClass classPool! !


!Magnitude methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
= aMagnitude 
	"Compare the receiver with the argument and answer with true if the 
	receiver is equal to the argument. Otherwise answer false."

	^self subclassResponsibility! !

!Magnitude methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
hash
	"Hash must be redefined whenever = is redefined."

	^self subclassResponsibility! !


!Number methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
even
	"Answer whether the receiver is an even number."

	^self \\ 2 = 0! !

!Number methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
sign
	"Answer 1 if the receiver is greater than 0, -1 if less than 0, else 0."

	self > 0 ifTrue: [^1].
	self < 0 ifTrue: [^-1].
	^0! !

!Number methodsFor: 'truncation and round off' stamp: 'ST-80 5/31/1983 9:10:35'!
floor
	"Answer the integer nearest the receiver toward negative infinity."

	| truncation |
	truncation _ self truncated.
	self >= 0 ifTrue: [^truncation].
	self = truncation
		ifTrue: [^truncation]
		ifFalse: [^truncation - 1]! !


!Float class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
readFrom: aStream 
	"Answer a new Float as described on the stream, aStream."

	^(super readFrom: aStream) asFloat! !


!LargePositiveInteger methodsFor: 'arithmetic' stamp: 'ST-80 5/31/1983 9:10:35'!
abs! !


!Association methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
key: aKey value: anObject 
	"Store the arguments as the variables of the receiver."

	key _ aKey.
	value _ anObject! !


!Character class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
new
	"Creating new characters is not allowed."

	self error: 'cannot create new characters'! !


!InstructionStream methodsFor: 'scanning' stamp: 'ST-80 5/31/1983 9:10:35'!
method
	"Answer the compiled method that supplies the receiver's bytecodes."

	^sender		"method access when used alone (not as part of a context)"! !

!InstructionStream methodsFor: 'scanning' stamp: 'ST-80 5/31/1983 9:10:35'!
pc
	"Answer the index of the next bytecode."

	^pc! !


!ContextPart methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
home
	"Answer the context in which the receiver was defined."

	self subclassResponsibility! !

!ContextPart methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
receiver
	"Answer the receiver of the message that created this context."

	self subclassResponsibility! !

!ContextPart methodsFor: 'debugger access' stamp: 'ST-80 5/31/1983 9:10:35'!
sender
	"Answer the context that sent the message that created the receiver."

	^sender! !

!ContextPart methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
top
	"Answer the top of the receiver's stack."

	^self at: stackp! !


!Decompiler methodsFor: 'instruction decoding' stamp: 'ST-80 5/31/1983 9:10:35'!
blockReturnTop
	"No action needed"! !


!Message class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
selector: aSymbol
	"Answer an instance of me with unary selector, aSymbol."

	^self new setSelector: aSymbol arguments: (Array new: 0)! !


!InputSensor methodsFor: 'keyboard' stamp: 'ST-80 5/31/1983 9:10:35'!
flushKeyboard
	"Remove all characters from the keyboard buffer."

	[self keyboardPressed]
		whileTrue: [self keyboard]! !


!ProcessorScheduler methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
activePriority
	"Answer the priority level of the currently running Process."

	^activeProcess priority! !

!ProcessorScheduler methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
activeProcess
	"Answer the currently running Process."

	^activeProcess! !

!ProcessorScheduler methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
highestPriority
	"Answer the number of priority levels currently available for use."

	^quiescentProcessLists size! !

!ProcessorScheduler methodsFor: 'process state change' stamp: 'ST-80 5/31/1983 9:10:35'!
suspendFirstAt: aPriority 
	"Suspend the first Process that is waiting to run with priority aPriority."

	^self suspendFirstAt: aPriority
		  ifNone: [self error: 'No Process to suspend']! !

!ProcessorScheduler methodsFor: 'process state change' stamp: 'ST-80 5/31/1983 9:10:35'!
terminateActive
	"Terminate the process that is currently running."

	activeProcess terminate! !


!ProcessorScheduler class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
new
	"New instances of ProcessorScheduler should not be created."

	self error:
'New ProcessSchedulers should not be created since
the integrity of the system depends on a unique scheduler'! !


!Collection methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
size
	"Answer how many elements the receiver contains."

	| tally |
	tally _ 0.
	self do: [:each | tally _ tally + 1].
	^tally! !

!Collection methodsFor: 'enumerating' stamp: 'ST-80 5/31/1983 9:10:35'!
do: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument."

	self subclassResponsibility! !

!Collection methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
occurrencesOf: anObject 
	"Answer how many of the receiver's elements are equal to anObject."

	| tally |
	tally _ 0.
	self do: [:each | anObject = each ifTrue: [tally _ tally + 1]].
	^tally! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
swap: oneIndex with: anotherIndex 
	"Move the element at oneIndex to anotherIndex, and vice-versa."

	| element |
	element _ self at: oneIndex.
	self at: oneIndex put: (self at: anotherIndex).
	self at: anotherIndex put: element! !

!SequenceableCollection methodsFor: 'copying' stamp: 'ST-80 5/31/1983 9:10:35'!
copyWith: newElement 
	"Answer a copy of the receiver that is 1 bigger than the receiver and has 
	newElement at the last element."

	| newIC |
	newIC _ self species new: self size + 1.
	newIC 
		replaceFrom: 1
		to: self size
		with: self
		startingAt: 1.
	newIC at: newIC size put: newElement.
	^newIC! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'ST-80 5/31/1983 9:10:35'!
findLast: aBlock
	"Return the index of my last element for which aBlock evaluates as true."

	| index |
	index _ self size + 1.
	[(index _ index - 1) >= 1] whileTrue:
		[(aBlock value: (self at: index)) ifTrue: [^index]].
	^ 0! !

!SequenceableCollection methodsFor: 'removing' stamp: 'ST-80 5/31/1983 9:10:35'!
remove: oldObject ifAbsent: anExceptionBlock 
	"SequencableCollections cannot implement removing."

	self shouldNotImplement! !


!ArrayedCollection class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
with: anObject 
	"Answer a new instance of me, containing only anObject."

	| newCollection |
	newCollection _ self new: 1.
	newCollection at: 1 put: anObject.
	^newCollection! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
with: firstObject with: secondObject 
	"Answer a new instance of me, containing firstObject and secondObject."

	| newCollection |
	newCollection _ self new: 2.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	^newCollection! !


!Array methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
storeOn: aStream 
	"Use the literal form if possible."

	self isLiteral
		ifTrue: 
			[aStream nextPut: $#; nextPut: $(.
			self do: 
				[:element | 
				element printOn: aStream.
				aStream space].
			aStream nextPut: $)]
		ifFalse: [super storeOn: aStream]! !


!CompiledMethod methodsFor: 'literals' stamp: 'ST-80 5/31/1983 9:10:35'!
literalAt: index 
	"Answer the literal indexed by the argument."

	^self objectAt: index + 1! !

!CompiledMethod methodsFor: 'scanning' stamp: 'ST-80 5/31/1983 9:10:35'!
messages
	"Answer a Set of all the message selectors sent by this method."

	| scanner aSet |
	aSet _ Set new.
	scanner _ InstructionStream on: self.
	scanner	
		scanFor: 
			[:x | 
			scanner addSelectorTo: aSet.
			false	"keep scanning"].
	^aSet! !


!Interval methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
at: anInteger put: anObject 
	"Storing into an Interval is not allowed."

	self error: 'you can not store into an interval'! !

!Interval methodsFor: 'removing' stamp: 'ST-80 5/31/1983 9:10:35'!
remove: newObject 
	"Removing from an Interval is not allowed."

	self error: 'elements cannot be removed from an Interval'! !


!Semaphore class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
new
	"Answer a new instance of Semaphore that contains no signals."

	^self basicNew initSignals! !


!OrderedCollection methodsFor: 'copying' stamp: 'ST-80 5/31/1983 9:10:35'!
copyWith: newElement 
	"Answer a copy of the receiver that is 1 bigger than the receiver and 
	includes the argument, newElement, at the end."

	| newCollection |
	newCollection _ self copy.
	newCollection add: newElement.
	^newCollection! !


!Text methodsFor: 'emphasis' stamp: 'ST-80 5/31/1983 9:10:35'!
runLengthFor: characterIndex 
	"Answer the count of characters remaining in run beginning with 
	characterIndex."

	^runs runLengthAt: characterIndex! !


!Dictionary methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
occurrencesOf: anObject 
	"Answer how many of the receiver's elements are equal to anObject."

	| count |
	count _ 0.
	self do: [:each | anObject = each ifTrue: [count _ count + 1]].
	^count! !

!Dictionary methodsFor: 'enumerating' stamp: 'ST-80 5/31/1983 9:10:35'!
keysDo: aBlock 
	"Evaluate aBlock for each of the receiver's keys."

	self associationsDo: [:association | aBlock value: association key]! !


!SharedQueue methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
size
	"Answer the number of objects that have been sent through the
	receiver and not yet received by anyone."

	^writePosition - readPosition! !


!SharedQueue class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
new
	"Answer a new instance of SharedQueue that has 10 elements."

	^self new: 10! !


!PositionableStream methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
contents
	"Answer with a copy of my collection from 1 to readLimit."

	^collection copyFrom: 1 to: readLimit! !

!PositionableStream methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
positionError
	"Since I am not necessarily writable, it is up to my subclasses to override 
	position: if expanding the collection is preferrable to giving this error."

	self error: 'Attempt to set the position of a PositionableStream out of bounds'! !


!ReadStream class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
on: aCollection from: firstIndex to: lastIndex 
	"Answer with a new instance streaming over a copy of aCollection from
	firstIndex to lastIndex."

	^self basicNew
		on: aCollection
		from: firstIndex
		to: lastIndex! !


!WriteStream methodsFor: 'character writing' stamp: 'ST-80 5/31/1983 9:10:35'!
space
	"Append a space character to the receiver."

	self nextPut: Character space! !

!WriteStream methodsFor: 'character writing' stamp: 'ST-80 5/31/1983 9:10:35'!
tab
	"Append a tab character to the receiver."

	self nextPut: Character tab! !


!ReadWriteStream methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
contents
	"Answer with a copy of my collection from 1 to readLimit."

	readLimit _ readLimit max: position.
	^collection copyFrom: 1 to: readLimit! !


!FileStream methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
contentsOfEntireFile
	"Read all of the contents of the receiver."

	| s |
	self readOnly.
	self reset.
	s _ self next: self size.
	self close.
	^s! !


!Process methodsFor: 'changing suspended state' stamp: 'ST-80 5/31/1983 9:10:35'!
install: aContext 
	"Replace the suspendedContext with aContext."

	self == Processor activeProcess
		ifTrue: [^self error: 'The active process cannot install contexts'].
	suspendedContext _ aContext! !


!Scanner methodsFor: 'error handling' stamp: 'ST-80 5/31/1983 9:10:35'!
offEnd: aString 
	"Parser overrides this"

	^self notify: aString! !


!ReturnNode methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asReturnNode! !


!Form methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
bits: aBitmap 
	"Reset the Bitmap containing the receiver's bits."

	bits _ aBitmap! !


!Point methodsFor: 'point functions' stamp: 'ST-80 5/31/1983 9:10:35'!
dist: aPoint 
	"Answer the distance between aPoint and the receiver."

	^(aPoint - self) r! !

!Point methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
printOn: aStream 
	"The receiver prints on aStream in terms of infix notation."

	x printOn: aStream.
	aStream nextPut: $@.
	y printOn: aStream! !


!Rectangle methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
containsPoint: aPoint 
	"Answer whether aPoint is within the receiver."

	^origin <= aPoint and: [aPoint < corner]! !


!Metaclass reorganize!
('accessing' allInstances category isMeta name soleInstance theMetaClass theNonMetaClass)
('copying' postCopy)
('instance creation' new)
('instance variables' addInstVarName: removeInstVarName:)
('pool variables' classPool)
('class hierarchy' addObsoleteSubclass: addSubclass: obsoleteSubclasses removeObsoleteSubclass: removeSubclass: subclasses subclassesDo: subclassesDoGently:)
('compiling' acceptsLoggingOfCompilation bindingOf: localBindingOf: possibleVariablesFor:continuedFrom: wantsChangeSetLogging wantsRecompilationProgressReported)
('fileIn/Out' definition fileOutInitializerOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing: nonTrivial objectForDataStream: storeDataOn:)
('testing' canZapMethodDictionary isObsolete)
('enumerating' allInstancesDo:)
('private' replaceObsoleteInstanceWith:)
('initialization' adoptInstance:from: instanceVariableNames:)
('tdd' testCaseClass)
('accessing instances and variables' allClassVarNames)
!


!SequenceableCollection reorganize!
('accessing' after: after:ifNone: allButFirst allButFirst: allButLast allButLast: antepenultimate antepenultimateIfAbsent: anyOne at:ifAbsent: atAll: atAll:put: atAll:putAll: atAllPut: atLast: atLast:ifAbsent: atLast:put: atPin: atRandom: atWrap: atWrap:put: before: before:ifNone: customizeExplorerContents eighth fifth fillWith: first first: firstAvailable: fourth from:to:put: head: identityIndexOf: identityIndexOf:ifAbsent: indexOf: indexOf:ifAbsent: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfMax indexOfSubCollection:startingAt: indexOfSubCollection:startingAt:ifAbsent: last last: lastAvailable: lastIfEmpty: lastIndexOf: lastIndexOf:ifAbsent: lastIndexOf:startingAt:endingAt:do: lastIndexOf:startingAt:endingAt:ifAbsent: lastIndexOf:startingAt:ifAbsent: middle ninth penultimate penultimateIfAbsent: quickIndexOf: replaceAll:with: replaceFrom:to:with: replaceFrom:to:with:startingAt: second seventh sixth swap:with: tail: third)
('comparing' = hasEqualElements: hash hashQuick)
('converting' @ asByteArray asColorArray asNewArray concatenation isSequenceable printStringWithNewline readStream reverse reversed writeStream)
('copying' , copyAfter: copyAfterLast: copyFrom:count: copyFrom:to: copyReplaceAll:with: copyReplaceFrom:to:with: copyUpThrough: copyUpTo: copyUpToLast: copyWith: shuffleBy: shuffled shuffledBy:)
('enumerating' allButFirstDo: allButLastDo: asDigitsToPower:do: collect: collect:from:to: combinations:atATimeDo: do: do:separatedBy: do:without: findBinary: findBinary:do:ifNone: findBinaryIndex: findBinaryIndex:do:ifNone: findFirst: findLast: from:to:do: groupsOf:atATimeDo: intervalOfSubCollection: keysAndValuesDo: pairsCollect: pairsDo: permutationsDo: polynomialEval: quickFindFirst: quickFindLast: replace: reverseDo: reverseWith:do: select: upTo: with:collect: with:do: with:do:separatedBy: with:reverseDo: with:with:collect: with:with:do: withIndexCollect: withIndexDo: withIndexDo:separatedBy: withNextDo: withPreviousCyclicDo: withPreviousDo:)
('private' asDigitsAt:in:do: checkedAt: combinationsAt:upTo:in:after:upTo:do: copyReplaceAll:with:asTokens: errorOutOfBounds permutationsStartingAt:do:)
('removing' remove:ifAbsent:)
('testing' beginsWith: endsWith: ifInBounds:ifNot: includes: isInBounds:)
('gui' do:displayingProgress:)
('math functions' += -= derivative incrementFraction integral)
('assertions' assertIsInBounds:)
!


!Dictionary reorganize!
('accessing' associationAt: associationAt:ifAbsent: associations at: at:ifAbsent: at:ifAbsentPut: at:ifPresent: at:ifPresent:ifAbsent: at:ifPresentAndInMemory: at:put: customizeExplorerContents keyAtIdentityValue: keyAtIdentityValue:ifAbsent: keyAtValue: keyAtValue:ifAbsent: keys keysSortedSafely values)
('testing' hasBindingThatBeginsWith: hasContentsInExplorer includes: includesKey: is: keyForIdentity: occurrencesOf:)
('adding' add: addAll: declare:from:)
('removing' keysAndValuesRemove: remove: remove:ifAbsent: removeKey: removeKey:ifAbsent: removeUnreferencedKeys unreferencedKeys)
('enumerating' associationsDo: collect: do: keysAndValuesDo: keysDo: select: valuesDo:)
('printing' printElementsOn: storeOn:)
('private' errorKeyNotFound errorValueNotFound keyAt: noCheckAdd: postCopy rehash scanFor: valueAtNewKey:put:atIndex:declareFrom:)
('user interface' explorerContentsWithIndexCollect:)
('comparing' = emptyCollectionHash hash)
('inspecting' inspectorClass)
('compiling' bindingOf: bindingsDo:)
!


!Form reorganize!
('accessing' bits bits: bitsSize center depth extent getCanvas height nativeDepth offset offset: pixelsPerWord size width wordsPerLine)
('analyzing' cgForPixelValue:orNot: dominantColor innerPixelRectFor:orNot: isAnyPixel: pixelCompare:with:at: primCountBits tallyPixelValuesInRect:into: xTallyPixelValue:orNot: yTallyPixelValue:orNot:)
('bordering' border:width: border:width:borderHeight:fillColor: border:width:fillColor: border:width:rule:fillColor: border:widthRectangle:rule:fillColor: borderWidth:borderHeight:fillColor:)
('coloring' fill:fillColor: fillBlack fillBlack: fillColor: fillGray fillGray: fillShape:fillColor: fillShape:fillColor:at: fillWhite fillWhite: fillWithColor: reverse reverse: reverse:fillColor:)
('color mapping' bitPatternFor: colormapIfNeededFor: colormapIfNeededForDepth: colormapIfNeededForGray8bpp colormapIfNeededFrom: mapColor:to: maskingMap newColorMap pixelValueFor:)
('converting' as8BitColorForm as8BitStandardPaletteColorForm asColorFormOfDepth: asCursorForm asForm asFormAutoRange asFormOfDepth: asGrayForm asGrayForm: asGrayScale asGrayScaleAndTransparent asGrayScaleAndTransparent: convertAlphaToZeroValueTransparency icon orderedDither32To16)
('copying' blankCopyOf:scaledBy: copy: copy:from:in:rule: copy:from:to:rule: copyBits:at:translucent: copyBits:from:at:clippingBox:rule: copyBits:from:at:clippingBox:rule:map: postCopy)
('display box access' boundingBox)
('displaying' displayAutoRangeAt: displayAutoRangeAt:zoom: displayOn:at:clippingBox:rule: paintBits:at:translucent:)
('displaying-generic' display displayAt: displayOn: displayOn:at: displayOn:at:rule:)
('fileIn/Out' printOn: readAttributesFrom: readBitsFrom: readFrom: storeBitsOn:base: storeOn: storeOn:base: storeSmall1BitBitsOn: storeSmall1BitOn: writeAttributesOn: writeBMPfileNamed: writeFileNamed: writeJPEGfileNamed: writeJPEGfileNamed:progressive: writeOn: writeOnFileNamed:)
('filling' eraseShape: fill:rule:fillColor: fillFromXColorBlock: fillFromXYColorBlock: fillFromYColorBlock:)
('image manipulation' smear:distance: tallyPixelValues)
('other' divideByAlpha fixAlpha makeAllPixelsAlpha: makeAllPixelsOpaque makeAllPixelsOpaqueBlack preMultiplyAlpha primPrintHScale:vScale:landscape:)
('pixel access' colorAt: colorAt:put: colorInterpolatedAt: i:j: i:j:put: isTransparentAt: pixelValueAt: pixelValueAt:put: primPixelValueAtX:y:)
('scaling, rotation' flippedBy: magnify:by: magnify:by:smoothing: magnify:to:smoothing: magnifyBy: magnifyTo: rotatedBy:smoothing: rotatedByDegrees: shrink:by:)
('testing' couldBeTranslucent is: isAllWhite isBigEndian isLittleEndian mightBeTranslucent)
('transitions' pageWarp:at:forward: slideImage:at:delta: wipeImage:at:clippingBox:rectForIndex: wipeImage:at:delta: wipeImage:at:delta:clippingBox: zoomIn:orOutTo:at:vanishingPoint: zoomInTo:at: zoomOutTo:at:)
('private' copyFromByteArray2:to: copyFromByteArray: fillSmall1BitWith: hackBits64: hackBits: initFromArray: setExtent:depth: setExtent:depth:bits:)
('comparing' = hash)
('initialization' fromDisplay: swapEndianness)
('encoding' addDeltasFrom:)
!

