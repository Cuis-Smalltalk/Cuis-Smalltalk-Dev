'From Cuis7.7 [latest update: #7805] on 22 January 2026 at 12:43:08 pm'!

!Object methodsFor: '[mk] comparing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
= anObject 
	"Answer true if the receiver and the argument represent the same object 
	and false otherwise. If = is redefined in any subclass, consider also
	redefining the message hash."

	^self == anObject! !

!Object methodsFor: '[mk] comparing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
~= anObject 
	"Answer true if the receiver and the argument do not represent the same 
	object and false otherwise."

	^self = anObject == false! !

!Object methodsFor: '[mk] error handling' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
shouldNotImplement
	"Announce that although the receiver inherits this message, it
	should not implement it."

	self error: 'This message is not appropriate for this object'! !


!Behavior methodsFor: '[mk] accessing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
format
	"Answer an Integer that encodes the kinds and numbers of variables of instances
	of the receiver."

	^format! !

!Behavior methodsFor: '[mk] testing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
isFixed
	"Answer whether the receiver does not have a variable (indexable) part."

	^self isVariable not! !

!Behavior methodsFor: '[mk] testing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
isPointers
	"Answer whether the receiver contains just pointers (not bits)."

	^self isBits not! !

!Behavior methodsFor: '[mk] printing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
printOn: aStream 
	aStream nextPutAll: 'a descendent of '.
	superclass printOn: aStream! !

!Behavior methodsFor: '[mkClassAndMethodCreation] private' library: #MinimalKernel05ClassAndMethodCreation stamp: 'ST-80 31/May/1983 09:10:35'!
flushCache
	"Tell the interpreter to remove the contents of its method lookup cache, if it has 
	one.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 89>
	self primitiveFailed! !


!ClassDescription methodsFor: '[mk] printing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
printOn: aStream 
	aStream nextPutAll: self name! !


!Metaclass methodsFor: '[mk] testing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
isMeta
	^ true! !

!Metaclass methodsFor: '[mk] class variables' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
classPool
	"Answer the dictionary of class variables."

	^thisClass classPool! !


!Collection methodsFor: '[mk] accessing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
size
	"Answer how many elements the receiver contains."

	| tally |
	tally := 0.
	self do: [:each | tally := tally + 1].
	^tally! !

!Collection methodsFor: '[mk] enumerating' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
reject: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect into a new collection like the receiver, only those elements for which
	aBlock evaluates to false.  Answer the new collection."

	^self select: [:element | (aBlock value: element) == false]! !

!Collection methodsFor: '[mk] enumerating' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
select: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect into a new collection like the receiver, only those elements for which
	aBlock evaluates to true.  Answer the new collection."

	| newCollection |
	newCollection := self species new.
	self do: [:each | (aBlock value: each) ifTrue: [newCollection add: each]].
	^newCollection! !


!SequenceableCollection methodsFor: '[mk] accessing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
indexOf: anElement 
	"Answer the index of anElement within the receiver.  If the receiver does
	not contain anElement, answer 0."

	^self indexOf: anElement ifAbsent: [0]! !

!SequenceableCollection methodsFor: '[mk] accessing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
replaceFrom: start to: stop with: replacement 
	"This destructively replaces elements from start to stop in the receiver.  
	Answer the receiver itself.
	Use copyReplaceFrom:to:with: for insertion/deletion which may alter the 
	size of the result."

	replacement size = (stop - start + 1)
		ifFalse: [self error: 'Size of replacement doesnt match'].
	^self replaceFrom: start to: stop with: replacement startingAt: 1! !


!ArrayedCollection methodsFor: '[mk] accessing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
size
	"Answer the number of indexable fields in the receiver. This value is the 
	same as the largest legal subscript. Primitive is specified here to override 
	SequenceableCollection size. Essential. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 62>
	^self basicSize! !


!CompiledMethod methodsFor: '[mk] literals' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
header
	"Answer the word containing the information about the form of the
	receiver and the form of the context needed to run the receiver."

	^self objectAt: 1! !

!CompiledMethod methodsFor: '[mk] literals' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
literalAt: index 
	"Answer the literal indexed by the argument."

	^self objectAt: index + 1! !

!CompiledMethod methodsFor: '[mk] literals' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
objectAt: index 
	"Answer with the method header (if index=1) or a literal (if index >1) from the 
	receiver.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 68>
	self primitiveFailed! !

!CompiledMethod methodsFor: '[mkClassAndMethodCreation] literals' library: #MinimalKernel05ClassAndMethodCreation stamp: 'ST-80 31/May/1983 09:10:35'!
literalAt: index put: value 
	"Replace the literal indexed by the first argument with the
	second argument."

	^self objectAt: index + 1 put: value! !

!CompiledMethod methodsFor: '[mkClassAndMethodCreation] literals' library: #MinimalKernel05ClassAndMethodCreation stamp: 'ST-80 31/May/1983 09:10:35'!
objectAt: index put: value 
	"Store the value argument into a literal in the receiver.   An index of 2 
	corresponds to the first literal.  Fails if the index is less than 2 or greater than 
	the number of literals.  Answer the value as the result.  Normally only the
	compiler sends this message, because only the compiler stores values in 
	CompiledMethods.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 69>
	self primitiveFailed! !


!Semaphore methodsFor: '[mk] communication' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
signal
	"Send a signal through the receiver. If one or more processes have been 
	suspended trying to receive a signal, allow the first one to proceed. If no 
	process is waiting, remember the excess signal. Essential. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 85>
	self primitiveFailed

	"self isEmpty    
		ifTrue: [excessSignals := excessSignals+1]    
		ifFalse: [Processor resume: self removeFirstLink]"! !

!Semaphore methodsFor: '[mk] communication' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
wait
	"The active Process must receive a signal through the receiver before 
	proceeding.  If no signal has been sent, the active Process will be suspended
	until one is sent.  Essential.  See  
	Object whatIsAPrimitive."

	<primitive: 86>
	self primitiveFailed

	"excessSignals>0  
		ifTrue: [excessSignals := excessSignals-1]  
		ifFalse: [self addLastLink: Processor activeProcess suspend]"! !

!Semaphore methodsFor: '[mk] initialize-release' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
initSignals
	"Consume any excess signals the receiver may have accumulated."

	excessSignals := 0! !


!Semaphore class methodsFor: '[mk] instance creation' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
forMutualExclusion
	"Answer a new instance of me that contains a single signal.  
	This new instance can now be used for mutual exclusion (see the 
	critical: message to Semaphore)."

	^self new signal! !

!Semaphore class methodsFor: '[mk] instance creation' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
new
	"Answer a new instance of Semaphore that contains no signals."

	^self basicNew initSignals! !


!CharacterSequence methodsFor: '[mk] accessing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
string
	"Answer the receiver itself.  This is for compatibility with other text classes."
	^self! !

!CharacterSequence methodsFor: '[mkSymbol] converting' library: #MinimalKernel02Symbol stamp: 'ST-80 31/May/1983 09:10:35'!
asSymbol
	"Answer the unique symbol whose characters are the characters of the string."
	^Symbol intern: self! !


!String methodsFor: '[mk] accessing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
size
	"Answer the number of indexable fields in the receiver. This value is the 
	same as the largest legal subscript. Primitive is specified here to override 
	SequenceableCollection size. Essential. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 62>
	^self basicSize! !


!String class methodsFor: '[mk] instance creation' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
with: aCharacter 
	"Answer a new instance of me, containing only aCharacter.
	String with: $a
	String with: $á
	String with: Character euro
	"

	| newCollection |
	newCollection := self new: 1.
	newCollection at: 1 put: aCharacter.
	^newCollection! !


!Symbol methodsFor: '[mk] accessing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
at: anInteger put: anObject 
	"you can not modify the receiver."

	self errorNoModification! !

!Symbol methodsFor: '[mk] accessing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	self errorNoModification! !

!Symbol methodsFor: '[mk] copying' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
shallowCopy
	"Answer with me, because Symbols are unique."! !

!Symbol methodsFor: '[mk] private' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
species
	^String! !

!Symbol methodsFor: '[mkSymbol] converting' library: #MinimalKernel02Symbol stamp: 'ST-80 31/May/1983 09:10:35'!
asSymbol! !

!Symbol methodsFor: '[mkSymbol] private' library: #MinimalKernel02Symbol stamp: 'ST-80 31/May/1983 09:10:35'!
errorNoModification
	self error:  'symbols can not be modified.'! !

!Symbol methodsFor: '[mkSymbol] private' library: #MinimalKernel02Symbol stamp: 'ST-80 31/May/1983 09:10:35'!
string: aString 
	1 to: aString size do: [:j | super at: j put: (aString at: j)].
	^self! !


!Set methodsFor: '[mkHashedCollections] accessing' library: #MinimalKernel04HashedCollections stamp: 'ST-80 31/May/1983 09:10:35'!
size
	^tally! !


!Dictionary methodsFor: '[mkHashedCollections] enumerating' library: #MinimalKernel04HashedCollections stamp: 'ST-80 31/May/1983 09:10:35'!
associationsDo: aBlock 
	"Evaluate aBlock for each of the receiver's key/value associations."

	super do: aBlock! !

!Dictionary methodsFor: '[mkHashedCollections] enumerating' library: #MinimalKernel04HashedCollections stamp: 'ST-80 31/May/1983 09:10:35'!
do: aBlock 
	super do: [:assoc | aBlock value: assoc value]! !

!Dictionary methodsFor: '[mkHashedCollections] enumerating' library: #MinimalKernel04HashedCollections stamp: 'ST-80 31/May/1983 09:10:35'!
select: aBlock 
	"Evaluate aBlock with each of my values as the argument.  Collect into a new 
	dictionary, only those associations for which aBlock evaluates to true."

	| newCollection |
	newCollection := self species new.
	self associationsDo: 
		[:each | 
		(aBlock value: each value) ifTrue: [newCollection add: each]].
	^newCollection! !


!PositionableStream methodsFor: '[mk] positioning' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
position
	"Answer the current position of accessing the stream."
	^position! !

!PositionableStream methodsFor: '[mk] positioning' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
position: anInteger 
	"Set position to anInteger as long as anInteger is within the bounds of the 
	receiver's contents.  If it is not, cause an error."

	anInteger >= 0 & (anInteger <= readLimit)
		ifTrue: [position := anInteger]
		ifFalse: [self positionError]! !


!PositionableStream class methodsFor: '[mk] instance creation' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
on: aCollection 
	"Answer a new instance of me, streaming over aCollection."

	^self basicNew on: aCollection! !


!WriteStream methodsFor: '[mk] reading' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
next
	self shouldNotImplement! !

!WriteStream methodsFor: '[mk] writing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
space
	"Append a space character to the receiver."

	self nextPut: Character space! !

!WriteStream methodsFor: '[mk] positioning' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
position: anInteger 
	readLimit := readLimit max: position.
	super position: anInteger! !

!WriteStream methodsFor: '[mk] positioning' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
reset
	readLimit := readLimit max: position.
	position := 0! !


!ContextPart methodsFor: '[mk] debugger access' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
sender
	"Answer the context that sent the message that created the receiver."

	^sender! !


!MethodContext methodsFor: '[mk] accessing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
method
	^method! !

!MethodContext methodsFor: '[mk] accessing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
receiver
	^receiver! !


!Message methodsFor: '[mk] accessing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
selector
	"Answer the selector of the receiver."
	^selector! !


!Boolean methodsFor: '[mk] logical operations' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
& aBoolean 
	"Evaluating conjunction -- Evaluate the argument.  Then answer true if both the
	receiver and the argument are true."
	self subclassResponsibility! !

!Boolean methodsFor: '[mk] logical operations' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
| aBoolean 
	"Evaluating disjunction (OR) -- Evaluate the argument.  Then answer true if
	either the receiver or the argument is true."
	self subclassResponsibility! !

!Boolean methodsFor: '[mk] logical operations' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
not
	"Negation-- answer true if the receiver is false, answer false if the receiver is true."
	self subclassResponsibility! !

!Boolean methodsFor: '[mk] copying' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
shallowCopy 
	"Receiver has two concrete subclasses, True and False.
	Only one instance of each should be made, so return self."! !


!False methodsFor: '[mk] logical operations' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
& alternativeObject 
	"Evaluating conjunction -- answer false since receiver is false."

	^self! !

!False methodsFor: '[mk] logical operations' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
| aBoolean 
	"Evaluating disjunction (OR) -- answer with the argument, aBoolean."

	^aBoolean! !

!False methodsFor: '[mk] logical operations' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
not
	"Negation -- answer true since the receiver is false."

	^true! !

!False methodsFor: '[mk] printing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
printOn: aStream 
	"Print false."
	aStream nextPutAll: 'false'! !


!True methodsFor: '[mk] logical operations' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
& alternativeObject 
	"Evaluating conjunction -- answer alternativeObject since receiver is true."

	^alternativeObject! !

!True methodsFor: '[mk] logical operations' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
| aBoolean 
	"Evaluating disjunction (OR) -- answer true since the receiver is true."

	^self! !

!True methodsFor: '[mk] logical operations' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
not
	"Negation--answer false since the receiver is true."

	^false! !

!True methodsFor: '[mk] printing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
printOn: aStream 
	aStream nextPutAll: 'true'! !


!Magnitude methodsFor: '[mk] comparing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
< aMagnitude 
	"Compare the receiver with the argument and answer with true if the 
	receiver is less than the argument. Otherwise answer false."

	^self subclassResponsibility! !

!Magnitude methodsFor: '[mk] comparing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
<= aMagnitude 
	"Compare the receiver with the argument and answer with true if the 
	receiver is less than or equal to the argument. Otherwise answer false."

	^(self > aMagnitude) not! !

!Magnitude methodsFor: '[mk] comparing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
= aMagnitude 
	"Compare the receiver with the argument and answer with true if the 
	receiver is equal to the argument. Otherwise answer false."

	^self subclassResponsibility! !

!Magnitude methodsFor: '[mk] comparing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
> aMagnitude 
	"Compare the receiver with the argument and answer with true if the 
	receiver is greater than the argument. Otherwise answer false."

	^aMagnitude < self! !

!Magnitude methodsFor: '[mk] comparing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
between: min and: max 
	"Answer whether the receiver is less than or equal to the argument, max,
	and greater than or equal to the argument, min."

	^self >= min and: [self <= max]! !

!Magnitude methodsFor: '[mk] comparing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
hash
	"Hash must be redefined whenever = is redefined."

	^self subclassResponsibility! !

!Magnitude methodsFor: '[mk] comparing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
max: aMagnitude 
	"Answer the receiver or the argument, whichever has the greater magnitude."

	self > aMagnitude
		ifTrue: [^self]
		ifFalse: [^aMagnitude]! !

!Magnitude methodsFor: '[mk] comparing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
min: aMagnitude 
	"Answer the receiver or the argument, whichever has the lesser magnitude."

	self < aMagnitude
		ifTrue: [^self]
		ifFalse: [^aMagnitude]! !


!Number methodsFor: '[mk] arithmetic' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
abs
	"Answer a Number that is the absolute value (positive magnitude) of the receiver."

	self < 0
		ifTrue: [^self negated]
		ifFalse: [^self]! !

!Number methodsFor: '[mk] arithmetic' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
negated
	"Answer a Number that is the negation of the receiver."
	^0 - self! !

!Number methodsFor: '[mk] truncation and round off' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
floor
	"Answer the integer nearest the receiver toward negative infinity."

	| truncation |
	truncation := self truncated.
	self >= 0 ifTrue: [^truncation].
	self = truncation
		ifTrue: [^truncation]
		ifFalse: [^truncation - 1]! !

!Number methodsFor: '[mk] converting' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
asInteger
	"Answer an integer nearest the receiver toward zero."
	^self truncated! !


!Float class methodsFor: '[mkFloat] constants' library: #MinimalKernel03Float stamp: 'ST-80 31/May/1983 09:10:35'!
pi
	"Answer the constant, Pi."
	^Pi! !


!Integer methodsFor: '[mk] converting' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
asInteger
	"Answer with the receiver itself."

	^self! !

!Integer methodsFor: '[mk] testing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
isInteger
	^true! !


!SmallInteger methodsFor: '[mk] comparing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
< aNumber 
	"Compare the receiver with the argument and answer with true if the receiver is less 
	than the argument.  Otherwise answer false.  Fail if the argument is not a 
	SmallInteger.  Essential.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 3>
	^super < aNumber! !

!SmallInteger methodsFor: '[mk] comparing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
<= aNumber 
	"Compare the receiver with the argument and answer true if the receiver is less 
	than or equal to the argument.  Otherwise answer false.  Fail if the argument is 
	not a SmallInteger.  Optional.  No Lookup.  See Object documentation 
	whatIsAPrimitive. "

	<primitive: 5>
	^super <= aNumber! !

!SmallInteger methodsFor: '[mk] comparing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
= aNumber 
	"Compare the receiver with the argument and answer true if the receiver is 
	equal to the argument.  Otherwise answer false.  Fail if the argument is not a 
	SmallInteger.  Essential.  No Lookup.  See Object documentation 
	whatIsAPrimitive. "

	<primitive: 7>
	^super = aNumber! !

!SmallInteger methodsFor: '[mk] comparing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
> aNumber 
	"Compare the receiver with the argument and answer true if the receiver is 
	greater than the argument.  Otherwise answer false.  Fail if the argument is not a 
	SmallInteger.  Essential.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 4>
	^super > aNumber! !

!SmallInteger methodsFor: '[mk] comparing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
>= aNumber 
	"Compare the receiver with the argument and answer true if the receiver is 
	greater than or equal to the argument.  Otherwise answer false.  Fail if the 
	argument is not a SmallInteger.  Optional.  No Lookup.  See Object 
	documentation whatIsAPrimitive."

	<primitive: 6>
	^super >= aNumber! !

!SmallInteger methodsFor: '[mk] comparing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
~= aNumber 
	"Compare the receiver with the argument and answer true if the receiver is not 
	equal to the argument.  Otherwise answer false.  Fail if the argument is not a 
	SmallInteger.  Essential.  No Lookup.  See Object documentation 
	whatIsAPrimitive. "

	<primitive: 8>
	^super ~= aNumber! !

!SmallInteger methodsFor: '[mk] testing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
even
	^(self bitAnd: 1) = 0! !


!LookupKey methodsFor: '[mk] accessing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
key
	"Answer the lookup key of the receiver."
	^key! !

!LookupKey methodsFor: '[mk] accessing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
key: anObject 
	"Store the argument, anObject, as the lookup key of the receiver."
	key := anObject! !

!LookupKey methodsFor: '[mk] comparing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
< aLookupKey 
	^key < aLookupKey key! !


!LookupKey class methodsFor: '[mk] instance creation' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
key: aKey 
	"Answer a new instance of me with the argument as the lookup up."
	^self new key: aKey! !


!Association methodsFor: '[mk] accessing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
key: aKey value: anObject 
	"Store the arguments as the variables of the receiver."

	key := aKey.
	value := anObject! !

!Association methodsFor: '[mk] accessing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
value
	"Answer the value of the receiver."
	^value! !

!Association methodsFor: '[mk] accessing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
value: anObject 
	"Store the argument, anObject, as the value of the receiver."
	value := anObject! !


!Association class methodsFor: '[mk] instance creation' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
key: newKey value: newValue
	"Answer a new instance of me with the arguments as the key and
	value of the association."
	^(super key: newKey) value: newValue! !


!UndefinedObject methodsFor: '[mk] testing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
isNil
	^true! !

!UndefinedObject methodsFor: '[mk] testing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
notNil
	^false! !

!UndefinedObject methodsFor: '[mk] printing' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
printOn: aStream 
	aStream nextPutAll: 'nil'! !

!UndefinedObject methodsFor: '[mk] copying' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
shallowCopy
	"Only one instance of UndefinedObject should ever be made, so answer 
	with self."! !


!Character class methodsFor: '[mk] instance creation' library: #MinimalKernel01 stamp: 'ST-80 31/May/1983 09:10:35'!
new
	"Creating new characters is not allowed."

	self error: 'cannot create new characters'! !


!Object reorganize!
('accessing' addInstanceVarNamed:withValue: at: at:put: basicAt: basicAt:put: basicSize rawBasicAt: rawBasicAt:put: size yourself)
('literals' allLiteralsDo: allLiteralsDo:scanForSpecial: hasLiteral: hasLiteralSuchThat: hasLiteralSuchThat:scanForSpecial:)
('associating' ->)
('binding' bindingOf:)
('casing' caseOf: caseOf:otherwise:)
('class membership' class isKindOf: isMemberOf: respondsTo:)
('comparing' hash literalEqual:)
('converting' adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: as: asString complexContents copyAs: mustBeBoolean mustBeBooleanIn: withoutListWrapper)
('copying' copy copyForClipboard copyFrom: copySameFrom: postCopy shallowCopy veryDeepCopy)
('error handling' assert: assert:description: caseError deny: doesNotUnderstand: error: handles: notYetImplemented notify: notify:at: primitiveFail primitiveFailed primitiveFailed: shouldBeImplemented shouldNotHappen shouldNotHappenBecause: shouldNotHappenBecauseErrorMessage shouldNotHappenErrorMessage subclassResponsibility)
('evaluating' value valueWithArguments: valueWithPossibleArgument:)
('dependency mechanism' addDependent: breakDependents changed: removeDependent:)
('events-accessing' actionForEvent: actionMap actionSequenceForEvent: actionsDo: hasActionForEvent: setActionSequence:forEvent: updateableActionMap)
('events-registering' when:evaluate: when:send:to: when:send:to:with: when:send:to:withArguments:)
('events-removing' releaseActionMap removeAction:forEvent: removeActionsForEvent: removeActionsSatisfying: removeActionsSatisfying:forEvent: removeActionsWithReceiver: removeActionsWithReceiver:forEvent: removeAllActions)
('events-triggering' triggerEvent: triggerEvent:with: triggerEvent:withArguments:)
('finalization' actAsExecutor finalizationRegistry finalize mourn retryWithGC:until: toFinalizeSend:to: toFinalizeSend:to:with:)
('inspecting' basicInspect copyToClipboard customizeExplorerContents explore hasContentsInExplorer inspect inspectWithLabel: inspectorClass)
('message handling' argumentName argumentNameSufix disableCode: executeMethod: perform: perform:with: perform:with:with: perform:with:with:with: perform:with:with:with:with: perform:withArguments: perform:withArguments:inSuperclass: perform:withPossiblyWrongSizedArguments: with:executeMethod: with:with:executeMethod: with:with:with:executeMethod: with:with:with:with:executeMethod: withArgs:executeMethod:)
('object serialization' comeFullyUpOnReload: convertToCurrentVersion:refStream: objectForDataStream: readDataFrom:size: releaseCachedState storeDataOn:)
('printing' displayStringOrText fullPrintString isLiteral longPrintOn: longPrintOn:limitedTo:indent: longPrintString longPrintStringLimitedTo: longPrintText longPrintTextLimitedTo: nominallyUnsent: print printAs:limitedTo: printOn: printString printStringLimitedTo: printText printTextLimitedTo: printWithClosureAnalysisOn: shortPrintString storeOn: storeString)
('system primitives' become: becomeForward: becomeForward:copyHash: className instVarAt: instVarAt:put: instVarAtPrim73: instVarAtPrim74:put: instVarNamed: instVarNamed:put: primitiveChangeClassTo: run:with:in: someObject)
('pinning' isPinned pin unpin)
('stepping' stepAt: wantsSteps)
('testing' is: isArray isBehavior isBlock isByteString isCharacter isClosure isCollection isCompiledCode isComplex isContext isFloat isFloatOrFloatComplex isFraction isInteger isInterval isMethodProperties isNumber isObject isPoint isPseudoContext isString isSymbol isUnicodeString isVariableBinding name renameTo: shouldBePrintedAsLiteral shouldBePrintedAsLiteralVisiting:)
('translation support' inline: success: var:declareC:)
('user interface' browseClassHierarchy confirm: confirm:orCancel: inform: request:do: request:initialAnswer: request:initialAnswer:do: request:initialAnswer:do:orCancel: request:initialAnswer:orCancel: request:initialAnswer:verifying: request:initialAnswer:verifying:do: request:initialAnswer:verifying:do:orCancel: request:initialAnswer:verifying:orCancel:)
('user interface support' autoCompleterClassFor: editorClassFor: textStylerClassFor:)
('morphic' activeHand flash runningWorld)
('private' deprecatedMethod errorDescriptionForSubcriptBounds: errorImproperStore errorNonIntegerIndex errorNotIndexable errorSubscriptBounds: primitiveError:signalContext: setPinned: species storeAt:inTempFrame:)
('tracing' inboundPointers inboundPointersExcluding: outboundPointers outboundPointersDo:)
('profiler support' profilerFriendlyCall:)
('debugging-halt' clearHaltOnce halt halt: haltOnce haltOnceEnabled setHaltOnce toggleHaltOnce)
('updating' changed update:)
('[mk] comparing' = ~=)
('[mk] error handling' shouldNotImplement)
!


!Behavior reorganize!
('accessing' binding bindingOf: classBindingOf: classDepth decompilerClass definesInstanceVariableNamed: instSize instSpec instVarNames name parserClass sourceCodeTemplate theNonMetaClass typeOfClass)
('accessing class hierarchy' allSubclasses allSubclassesWithLevelDo:startingLevel: allSubclassesWithLevelDo:startingLevel:sortByCategory: allSuperclasses allSuperclassesUpTo: hasSubclasses subclasses subclassesDo: superclass: withAllSubclasses withAllSuperclasses withAllSuperclassesDownFrom: withAllSuperclassesUpTo:)
('accessing instances and variables' addReferencesOf:at:to: allAccessesTo: allBroadAccessesTo: allClassVarNames allInstVarNames allInstances allRegularInstVarNames allSubInstances allowsSubInstVars byteSizeOfInstance byteSizeOfInstanceOfSize: classVarNames definesClassVariableNamedInHierarchy: definesInstanceVariableNamedInHierarchy: elementSize hasReferencesToInstanceVariableNamed: indexOfInstanceVariable: inspectAllInstances inspectSubInstances instanceCount isInstanceVariableNamedReferencedInHierarchy: sharedPools unreferencedInstanceVariables whichClassDefinesInstanceVariable:ifNone:)
('accessing method dictionary' >> allSelectors compiledMethodAt:ifAbsent: compiledMethodAt:ifPresent:ifAbsent: compress firstCommentAt: firstPrecodeCommentFor: highestClassImplementing:ifNone: methodDictionary methodHeaderFor: methodsDo: methodsSelect: methodsWithArgumentOrTemporaryNamed: precodeCommentOrInheritedCommentFor: recoverFromMDFaultWithTrace removeSelector: selectorAtMethod:setClass: selectors selectorsAndMethodsDo: selectorsDo: sourceCodeAt: sourceCodeAt:ifAbsent: standardMethodHeaderFor: supermostPrecodeCommentFor: ultimateSourceCodeAt:ifAbsent: withSuperclassThatIncludesSelector:do:ifNone: zapAllMethods)
('method dictionary' addSelectorSilently:withMethod: compiledMethodAt: includesSelector: lookupSelector: methodDict methodDict:)
('auto complete' typeName)
('class hierarchy' deepSubclassNamed: superclass)
('comparing' identityHash)
('compiling' compile: compile:notifying: compileAll compileAllFrom: compilerClass decompile: defaultMethodTrailer instVarNamesAndOffsetsDo: recompile: recompile:from: recompileChanges variablesAndOffsetsDo:)
('constants' maxInstSize)
('copying' copyOfMethodDictionary postCopy)
('creating method dictionary' methodDictionary:)
('enumerating' allInstancesDo: allSubInstancesDo: allSubclassesDo: allSuperclassesDo: selectSuperclasses: withAllSubclassesDo: withAllSuperAndSubclassesDo: withAllSuperclassesDo:)
('instance creation' adoptInstance: basicNew basicNew: initializedInstance new new: uninitializedPinnedNew:)
('instances' allInstancesOrNil someInstance)
('methods' addMethodsTo:thatReferExplicitlyTo: addMethodsTo:thatReferTo: addTo:methodsThatReferInHierarchyTo: methodNodeFor: methodNodeFor:noPattern: methodNodeFor:noPattern:optimizeSpecialSends: methodNodeWithoutImplicitSelfReturnFor: stampAt:)
('obsolete subclasses' addObsoleteSubclass: obsoleteSubclasses recreateObsoleteClassesCollection removeObsoleteSubclass:)
('printing' literalScannedAs:notifying: longPrintOn: printWithClosureAnalysisOn: storeLiteral:on:)
('release' obsolete)
('system startup' initClassCachedState releaseClassCachedState releaseClassState shutDown shutDown: startUp startUp:)
('system-support' allCallsOn)
('testing' canZapMethodDictionary hasChangedComparedTo: isBehavior isBits isBytes isCompiledMethodClass isDoubleBytes isDoubleWords isEphemeronClass isImmediateClass isMeta isObsolete isVariable isWeak isWords shouldNotBeRedefined sourceMatchesBytecodeAt:)
('testing class hierarchy' includesBehavior: inheritsFrom: kindOfSubclass)
('testing method dictionary' allUnsentMessages canUnderstand: hasMethods whichClassIncludesSelector: whichSelectorsAccess: whichSelectorsReferExplicitlyTo: whichSelectorsReferTo: whichSelectorsStoreInto:)
('user interface' allLocalCallsOn: crossReference lastUnderscoreMeansSubscript withAllSubAndSuperclassesDo:)
('private' basicCompile:notifying:trailer:ifFail: basicCompile:notifying:trailer:ifFail:optimizeSpecialSends: basicRemoveSelector: handleFailingBasicNew handleFailingBasicNew: handleFailingFailingBasicNew handleFailingFailingBasicNew: handleFailingFailingUninitializedPinnedNew: handleFailingUninitializedPinnedNew: setFormat: spaceUsed)
('SUnit tests' testCaseClass withTestCaseClassDo:ifNone:)
('initialization' superclass:methodDictionary:format:)
('instances and variables' maxAccessibleInstSize)
('[mk] accessing' format)
('[mk] testing' isFixed isPointers)
('[mk] printing' printOn:)
('[mkClassAndMethodCreation] private' flushCache)
!


!ClassDescription reorganize!
('accessing' classVersion comment comment: comment:stamp: hasComment instVarNames localBindingOf: smalltalkComment theMetaClass theNonMetaClass)
('accessing class hierarchy' classesThatImplementAllOf: removeUninstantiatedSubclassesSilently subclasses)
('accessing method dictionary' addAndClassifySelector:withMethod:inCategory:methodTimeStamp:notifying: allMethodCategoriesIntegratedThrough: changeRecordsAt: induceMDFault methodCategories methodCategoriesAsSortedCollection recoverFromMDFault recoverFromMDFaultWithTrace removeCategory: removeSelector: removeSelectorIfInBaseSystem: selectorsInCategory:)
('accessing variables' allUnreferencedInstanceVariables)
('class variables' classPool)
('compiling' acceptsLoggingOfCompilation compile:classified: compile:classified:notifying: compile:classified:withStamp:libraryName:notifying: compile:classified:withStamp:libraryName:notifying:logSource: compile:classified:withStamp:notifying: compile:notifying: compileSilently:classified: compileSilently:classified:notifying: instVarNamesAndOffsetsDo: isOkToAddMethod:isMeta: isOkToChangeMethod:isMeta: isOkToRemoveMethod:isMeta: wantsChangeSetLogging wantsRecompilationProgressReported)
('copying' copy:from: copy:from:classified: copyAll:from: copyAll:from:classified: copyAllCategoriesFrom: copyCategory:from: copyCategory:from:classified: copyMethodDictionaryFrom:)
('fileIn/Out' classComment: classComment:stamp: commentStamp: commentStamp:prior: definition definitionPreamble definitionPreambleWithoutStamp fileOutCategory: fileOutCategory:on:moveSource:toFile: fileOutChangedMessages:on: fileOutChangedMessages:on:moveSource:toFile: fileOutMethod: fileOutOn: fileOutOn:moveSource:toFile: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: fileOutOrganizationOn: fileOutOrganizationOn:excludingExtensions: methods methodsFor: methodsFor:library:stamp: methodsFor:library:stamp:prior: methodsFor:stamp: methodsFor:stamp:prior: moveChangesTo: printCategoryChunk:on:withStamp:libraryName:priorMethod: printMethodChunk:withPreamble:on:moveSource:toFile: putClassCommentToCondensedChangesFile: reformatAll reformatMethodAt: reorganize)
('initialization' canUpdateFrom: doneCompiling organization: setInstVarNames: superclass:methodDictionary:format:)
('initialize-release' updateInstances:from:isMeta: updateMethodBindingsTo:)
('instance variables' addInstVarName: allInstVarNamesEverywhere browseClassVarRefs checkForInstVarsOK: chooseClassVarName chooseDefiningInstanceVariableAlphabeticallyWith:thenDo: chooseDefiningInstanceVariableAlphabeticallyWith:thenDo:ifNone: chooseInstVarAlphabeticallyThenDo: chooseInstVarThenDo: classThatDefinesClassVariable: classThatDefinesInstanceVariable: forceNewFrom: instVarNameForIndex: removeInstVarName: withClassesThatDefineInHierarchyInstanceVariable:do:ifNone: withSubclassesThatDefineInstanceVariable:do:ifNone:)
('method dictionary' allMethodsInCategory:)
('organization' category category: organization whichCategoryIncludesSelector: zapOrganization)
('printing' classVariablesString instanceVariablesString printNameOn: printWithClosureAnalysisOn: sharedPoolsString storeOn:)
('release' obsolete)
('source code management' linesOfCode messageSendsCount messageSendsCountInCategory: messagesCountInCategory:)
('system-support' allUnreferencedClassVariables)
('testing' hasDefinition)
('private' errorCategoryName instVarMappingFrom: logMethodSource:forMethodWithNode:inCategory:withStamp:libraryName:notifying: newInstanceFrom:variable:size:map:)
('[mk] printing' printOn:)
!


!Metaclass reorganize!
('accessing' allClassInstVarNames allInstances bindingOf: category localBindingOf: name soleInstance theMetaClass theNonMetaClass)
('accessing variables' allClassVarNames)
('class hierarchy' addObsoleteSubclass: addSubclass: hasSubclasses obsoleteSubclasses removeObsoleteSubclass: removeSubclass: subclasses subclassesDo:)
('class variables')
('compiling' acceptsLoggingOfCompilation possibleVariablesFor:continuedFrom: wantsChangeSetLogging wantsRecompilationProgressReported)
('enumerating' allInstancesDo:)
('fileIn/Out' definition fileOutInitializerOn: fileOutOn:moveSource:toFile:initializing:sortMethodsByCategory: nonTrivial objectForDataStream: storeDataOn:)
('initialization' adoptInstance:from: instanceVariableNames:)
('instance variables' addInstVarName: removeInstVarName:)
('printing' printNameOn:)
('testing' canZapMethodDictionary isObsolete)
('SUnit tests' testCaseClass)
('copying' postCopy)
('instance creation' new)
('[mk] testing' isMeta)
('[mk] class variables' classPool)
!


!Collection reorganize!
('accessing' anyOne atRandom atRandom:)
('adapting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend:)
('adding' add: add:withOccurrences: addAll:)
('arithmetic' * + - / // \\ raisedTo:)
('comparing' = emptyCollectionHash hash)
('converting' as:on:delimiter:last: asArray asBag asByteArray asCommaSeparated:on: asCommaStringAnd asDictionary asFloat32Array asFloat64Array asIdentitySet asIntegerArray asNewArray asOrderedCollection asPointArray asSet asSortedCollection asSortedCollection: asStringOn:delimiter:last: asWordArray)
('copying' , copyWith: copyWithout: copyWithoutAll: flatten flattenTo:)
('enumerating' allSatisfy: anySatisfy: associationsDo: associationsDo:separatedBy: collect: collect:andFold: collect:andFold:ifEmpty: collect:thenSelect: count: detect: detect:ifFound:ifNone: detect:ifNone: detectMax: detectMin: difference: do: do:separatedBy: do:without: fold: fold:ifEmpty: groupBy: groupBy:having: inject:into: intersection: noneSatisfy: reduce: reject:thenCollect: reject:thenDo: select:thenCollect: select:thenDo: union:)
('filter streaming' contents)
('inspecting' hasContentsInExplorer)
('math functions' abs arcCos arcSin arcTan ceiling cos degreeCos degreeSin exp floor ln log log2 negated reciprocal roundTo: rounded sign sin sqrt squared tan truncateTo: truncated)
('printing' printElementsOn: printNameOn: printOn: storeOn:)
('removing' remove: remove:ifAbsent: removeAll: removeAllFoundIn: removeAllSuchThat:)
('sorting' sorted sorted:)
('statistics' average average: average:ifEmpty: max max: mean min min: product product: range sum sum: sum:ifEmpty:)
('testing' canHoldAnyCodePoint identityIncludes: ifEmpty: ifEmpty:ifNotEmpty: ifNotEmpty: ifNotEmpty:ifEmpty: includes: includesAllOf: includesAnyOf: includesSubstringAnywhere: is: isCollection isEmpty isEmptyOrNil isSequenceable notEmpty occurrencesOf:)
('private' emptyCheck errorCollectionTooSmall errorEmptyCollection errorNoMatch errorNotFound: errorNotKeyed species)
('[mk] accessing' size)
('[mk] enumerating' reject: select:)
!


!SequenceableCollection reorganize!
('accessing' after: after:ifNone: allButFirst allButFirst: allButLast allButLast: antepenultimate antepenultimateIfAbsent: anyOne at:ifAbsent: atAll: atAll:put: atAll:putAll: atAllPut: atLast: atLast:ifAbsent: atLast:put: atPin: atRandom: atWrap: atWrap:put: before: before:ifNone: eighth fifth fillWith: first first: firstAvailable: fourth from:to:put: head: identityIndexOf: identityIndexOf:ifAbsent: indexOf:ifAbsent: indexOf:startingAt: indexOf:startingAt:ifAbsent: indexOfMax indexOfSubCollection:startingAt: indexOfSubCollection:startingAt:ifAbsent: last last: lastAvailable: lastIfEmpty: lastIndexOf: lastIndexOf:ifAbsent: lastIndexOf:startingAt:endingAt:do: lastIndexOf:startingAt:endingAt:ifAbsent: lastIndexOf:startingAt:ifAbsent: middle ninth penultimate penultimateIfAbsent: quickIndexOf: replaceAll:with: replaceFrom:to:with:startingAt: second seventh sixth swap:with: tail: third)
('assertions' assertIsInBounds:)
('comparing' = hasEqualElements: hash hashQuick hashQuick: is:subcollectionAt:)
('converting' @ asByteArray asColorArray asNewArray concatenation printStringWithNewline readStreamFrom:to: reverse reversed)
('copying' , copyAfter: copyAfterLast: copyBlank: copyFrom:count: copyFrom:to: copyReplaceAll:with: copyReplaceFrom:to:with: copyUpThrough: copyUpTo: copyUpToLast: copyWith: grownTo: shuffleBy: shuffled shuffledBy:)
('enumerating' allButFirstDo: allButLastDo: asDigitsToPower:do: collect: collect:from:to: combinations:atATimeDo: do: do:separatedBy: do:without: findBinary: findBinary:do:ifNone: findBinaryIndex: findBinaryIndex:do:ifNone: findFirst: findFirst:startingAt: findFirstBasicAt:startingAt: findLast: from:to:do: groupsOf:atATimeDo: intervalOfSubCollection: keysAndValuesDo: pairsCollect: pairsDo: permutationsDo: polynomialEval: quickFindFirst: quickFindLast: replace: reverseDo: reverseWith:do: select: upTo: with:allSatisfy: with:anySatisfy: with:collect: with:do: with:do:separatedBy: with:reverseDo: with:with:collect: with:with:do: withIndexCollect: withIndexDo: withIndexDo:separatedBy: withNextDo: withPreviousCyclicDo: withPreviousDo:)
('gui' do:displayingProgress:)
('inspecting' customizeExplorerContents explorerContents)
('math functions' += -=)
('removing' remove:ifAbsent:)
('sorting' sorted:)
('stream creation' asStreamResult readStream thatCanBeModified writeStream)
('testing' beginsWith: endsWith: ifInBounds:ifNot: includes: isInBounds: isSequenceable)
('private' asDigitsAt:in:do: combinationsAt:upTo:in:after:upTo:do: copyReplaceAll:with:asTokens: errorOutOfBounds permutationsStartingAt:do:)
('[mk] accessing' indexOf: replaceFrom:to:with:)
!


!ArrayedCollection reorganize!
('accessing')
('adding' add:)
('inserting' insert:shiftingRightAt:)
('inspecting' explorerContents)
('math functions' derivative incrementFraction integral)
('objects from disk' byteSize restoreEndianness writeOn:)
('printing' printNameOn: storeOn:)
('sorting' isSorted isSortedBy: mergeFirst:middle:last:into:by: mergeSortFrom:to:by: mergeSortFrom:to:src:dst:by: quickSortFrom:to:by: sort sort:)
('testing' isEmpty)
('private' defaultElement storeElementsFrom:to:on:)
('[mk] accessing' size)
!


!CompiledMethod reorganize!
('accessing' accessorDescriptionOf:at: bytecodeSetName bytecodes clearFlag clearSignFlag defaultSelector encoderClass endPC flag frameSize headerWithoutSignFlag initialPC methodClass methodClass: methodClassAssociation methodClassAssociation: methodReference numArgs numLiterals numTemps primitive properties properties: returnField scanner searchForClass searchForSelector selector selector: setSignFlag signFlag trailer withPropertiesDo:ifSelector:)
('accessing-pragmas & properties' addPragma: pragmaAt: pragmas propertyKeysAndValuesDo: propertyValueAt: propertyValueAt:ifAbsent: propertyValueAt:put: removeProperties removeProperty: removeProperty:ifAbsent: withPropertiesDo:)
('auto complete' autoCompleterDocumentationAppendingToParameter:toReturn: commentAutoCompleterDocumentationAppendigTo:using: commentAutoCompleterDocumentationOf: dynamicTypingAutoCompleterDocumentation receiverTextAutoCompleterDocumentation selectorAutoCompleterDocumentationAppendingTo:using:appendingToParameter:)
('breakpoints' hasBreakpoint)
('closures' containsBlockClosures embeddedBlockClosures)
('comparing' = equivalentTo: hash)
('converting' asMethodReference asString)
('copying' flattenTo:)
('debugger support' abstractPCForConcretePC: blockExtentsInto:from:to:scanner:numberer: debuggerMap mapFromBlockKeys:toSchematicTemps: pcPreviousTo: startpcsToBlockExtents tempsSubSequenceFrom:)
('decompiling' compilerClass decompile decompilerClass methodForDecompile parserClass selectorAndArgumentsAsString)
('evaluating' valueWithReceiver:arguments:)
('file in/out' objectForDataStream: storeDataOn: zapSourcePointer)
('initialization' copyWithBytecodes: copyWithTrailerBytes: needsFrameSize:)
('inspecting' explorerContents inspectorClass)
('literals' allImplicitLiteralsDo: allLiterals allLiteralsDo: allLiteralsDo:scanForSpecial: codeLiterals codeLiteralsDo: hasLiteral: hasLiteralSuchThat: headerDescription implicitLiteralsDo: indexOfLiteral: literalStrings literals literalsDo: regularLiterals sendsSelector: xtraBindings)
('method node' createMethodNode createMethodNodeOptimizeSpecialSends: methodNode methodNode: methodNodeForLookingEquivalentNodes methodNodeWithoutImplicitSelfReturn notOptimizedMethodNode sourceRangesOfNodesEquivalentTo: sourceRangesOfNodesEquivalentTo:keepingSelection:)
('organization' category)
('printing' abstractSymbolic classAndSelector decompileString longPrintOn: longPrintOn:indent: longPrintRelativeOn:indent: primitiveErrorVariableName printClassAndSelectorOn: printOn: printPrimitiveOn: printQuickOn:indent: storeLiteralsOn:forClass: storeOn: symbolic symbolicLinesDo:)
('scanning' messages messagesDo: messagesSequence readsField: readsRef: scanFor: sendsToSuper writesField: writesRef:)
('source code management' checkOKToAdd:at:in: destroySourcePointer fileIndex filePosition getPreamble getSource getSourceFor:in: getSourceFromFile linesOfCode messageSendsCount putSource:fromParseNode:class:category:withStamp:libraryName:inFile:priorMethod: putSource:fromParseNode:inFile:withPreamble: setSourcePointer: setSourcePosition:inFile: sourceCode sourcePointer)
('source code ranges' messageSendsRangesOf:)
('testing' accessesInstanceVariable: hasArgumentOrTemporaryNamed: hasLiteralExplicitly: hasNewPropertyFormat hasReportableSlip hasVariableBindingTo: is: isAbstract isCompiledBlock isCompiledCode isCompiledMethod isGetterOf:at: isInstalled isLinkedNamedPrimitive isNamedPrimitive isQuick isReturnField isReturnSelf isReturnSpecial isSetterOf:at: isTestMethod isValid readsInstanceVariable: referencesParameterAt: referencesSelf referencesSuper usesClosureBytecodes writesInstanceVariable:)
('time stamp' author authorAndStamp authorAndStampAsSaved dateAndTime dateMethodLastSaved)
('tracing' outboundPointersDo:)
('user interface support' browse)
('private' flushCache penultimateLiteral penultimateLiteral: voidCogVMState)
('object serialization' readDataFrom:size:)
('[mk] literals' header literalAt: objectAt:)
('[mkClassAndMethodCreation] literals' literalAt:put: objectAt:put:)
!


!Semaphore reorganize!
('communication' waitTimeoutMSecs: waitTimeoutSeconds:)
('mutual exclusion' critical: critical:ifCurtailed: critical:ifError: critical:ifLocked:)
('testing' isSignaled)
('initialization' terminateProcess)
('printing' printOn:)
('comparing' = hash)
('initialize-release')
('private' species)
('[mk] communication' signal wait)
('[mk] initialize-release' initSignals)
!


!Semaphore class reorganize!
('instance creation')
('[mk] instance creation' forMutualExclusion new)
!


!CharacterSequence reorganize!
('accessing' findAnySubStr:startingAt: findBetweenSubStrs: findDelimiters:startingAt: findLastOccurrenceOfString:startingAt: findString: findString:startingAt:caseSensitive: findTokens: findTokens:keep: includesSubString: includesSubstring:caseSensitive: indexOf: indexOfSubCollection:startingAt:ifAbsent: lineCount lineNumber: lines linesDo: skipAnySubStr:startingAt: skipDelimiters:startingAt: subStrings: substringsSeparatedBy: utf8BytesSize)
('adding' add:)
('copying' append: appendToText: copyReplaceTokens:with: copyReplacing: copyReplacing:into: copyReplacing:with: copyReplacing:with:into: copyReplacing:withAll: copyReplacing:withAll:into: flattenTo: join: newFromAfterAppending:into:keepingFrom:skipping: padded:to:with:)
('comparing' < <= = > >= alike: beginsWith: commonPartWith:startAt:stopAt:applying: commonPrefixWith: endsWith: hash is:substringAt: match: startingAt:match:startingAt:)
('converting' adaptToCollection:andSend: adaptToNumber:andSend: adaptToPoint:andSend: asByteString asCamelCase asDate asFileName asIdentifier: asLowercase asMonth asNumber asPlainString asSmalltalkComment asString asText asUnHtml asUnaccented asUnicodeString asUnicodeStringOrText asUppercase asUtf8Bytes asUtf8BytesOrByteString asUtf8BytesOrByteStringOrByteArray asValidInstanceVariableName asValidSelector asWeek asYear base64Decoded capitalized contractTo: correctAgainst: correctAgainst:continuedFrom: correctAgainstDictionary:continuedFrom: displayStringOrText findPositiveInteger findSelector firstNonSeparator fixLineEndings initialIntegerOrNil integerOrNilAt: keywords lastNonSeparator prefixAndSuffix: romanNumber squeezedTo: translated truncateTo: truncateWithElipsisTo: uncapitalized withBlanksCondensed withBlanksTrimmed withCuisLineEndings withDescriptiveLineEndings withDoubleColonsDoubled withDoubleColonsUndoubled withNewLines withoutEnclosing: withoutLeadingBlanks withoutPossiblePrefix: withoutPossibleSuffix: withoutPrefix: withoutSeparators withoutSuffix: withoutTrailingBlanks)
('enumerating' collect: collectInArray:)
('printing' print printAsLiteralOn: printOn: storeOn:)
('testing' isAlphaNumeric isLineEndAt: isRemote isSeparators isString)
('user interface' edit editLabel:)
('fileman-converting' asDirectoryEntry asDriveName asFileEntry asFullDirectoryEntry asFullFileEntry asPathComponents asPathTokens withoutWindowsDriveName)
('fileman-testing' beginsWithPathSeparator beginsWithWindowsDriveName isAbsolutePathName isDriveName isPathSeparator isRelativeMark isRelativePathName)
('fileman-accessing' indexOfFirstPathSeparator indexOfLastPathSeparator pathAndLocalName upToFirstPathSeparator upToLastPathSeparator)
('arithmetic' * + - / // \\)
('text conversion emphasis' bold italic pointSize: struck sub super under)
('text conversion attributes' centered justified leftFlush rightFlush)
('text conversion color' black blue color: cyan gray green magenta red yellow)
('displaying' displayAt: displayOn: displayOn:at: displayOn:at:textColor: displayProgressAt:from:to:during:)
('private' correctAgainstEnumerator:continuedFrom: evaluateExpression:parameters: getEnclosedExpressionFrom:)
('formatting' format:)
('uCompletion' separateKeywords)
('paragraph support' endOfParagraphBefore: indentationIfBlank:)
('grammar' article asPlural exceptionalPlural isUninflictedNoun lowercasePlural pluralize: withArticle)
('fileman-actions' fileContents fileContents:)
('Unicode normalization' asNFC asNFD canonicalComposition canonicalDecomposition)
('as yet unclassified' writeStream)
('[mk] accessing' string)
('[mkSymbol] converting' asSymbol)
!


!String reorganize!
('accessing' at: at:put: byteSize findString:startingAt: findStringCaseInsensitive:startingAt: indexOf:startingAt:ifAbsent: indexOfAnyIn:startingAt:ifAbsent: isLastLineSeparator isLastSeparator lineIndicesDo: replaceFrom:to:with:startingAt: utf8BytesSize)
('comparing' < = caseInsensitiveLessOrEqual: crc16 hash is:substringAt: sameAs:)
('copying' , appendToString: copyReplaceFrom:to:with: copyWith:)
('converting' asByteString asByteStringIfAscii asCodePoints asHex asLowercase asUppercase asUtf8Bytes asUtf8BytesOrByteString asUtf8BytesOrByteStringOrByteArray capitalized fixLineEndings substrings surroundedBySingleQuotes thatCanHoldAnyCodePoint translateToLowercase translateToUppercase uncapitalized withLineEndings:)
('enumerating' collect:)
('printing' isLiteral)
('services' numArgs)
('testing' canHoldAnyCodePoint is: isAscii isByteString isEmpty isInCanonicalCompositionForm isIntegerDenotation)
('paragraph support' encompassLine: encompassParagraph:)
('[mk] accessing' size)
!


!String class reorganize!
('instance creation' crString crlfString fromUtf8Bytes: fromUtf8Bytes:trimLastNull: lfString new: new:withAll: newFrom: newLineString percentEscapingNonAscii: percentEscapingNonAsciiAndThoseIn:bytes: tab with:with: with:with:with: withAll:)
('primitives' translate:from:to:table:)
('character collation' does:caseInsensitiveCollateBefore: does:collateAfter: does:collateBefore:)
('stream creation' writeStream)
('services' compare:with: compare:with:collated: compareIgnoreCase:with: findStringIgnoreCase:in:startingAt: findSubstring:in:startingAt:matchTable: is:equalTo: isAscii: string:lineIndicesDo: string:withLineEndings: substringsIn: translate:table: translateToLowercase: translateToUppercase:)
('accessing' findString:in:startingAt:)
('static state' basicCaseInsensitiveOrder basicCaseSensitiveOrder caseInsensitiveOrder caseSensitiveOrder lowercasingTable tokenishTable uppercasingTable)
('system' symbolClassToUse)
('[mk] instance creation' with:)
!


!Symbol reorganize!
('accessing' precedence)
('converting' asPlainString asString capitalized numArgs: uncapitalized)
('printing' print storeOn:)
('testing' isInfix isInitializePvtSelector isKeyword isLiteral isPvtSelector isSymbol isUnary isValidBinarySelector isValidGlobalName isValidKeywordSelector isValidSelector isValidUnarySelector)
('uCompletion' separateKeywords)
('evaluating' value: value:value: value:value:value: value:value:value:value: valueWithArguments:)
('comparing' =)
('copying')
('private' flushCache)
('[mk] accessing' at:put: replaceFrom:to:with:startingAt:)
('[mk] copying' shallowCopy)
('[mk] private' species)
('[mkSymbol] converting' asSymbol)
('[mkSymbol] private' errorNoModification string:)
!


!Set reorganize!
('accessing' atRandom: like:)
('adding' add: add:withOccurrences:)
('enumerating' collect: do: union:)
('private' array atNewIndex:put: findElementOrNil: fixCollisionsFrom: fullCheck grow growSize init: keyAt: noCheckAdd: privateSwapArrayElement:with: rehash scanFor:)
('removing' copyWithout: remove:ifAbsent: removeAll)
('testing' includes: is: occurrencesOf:)
('inspecting' inspectorClass)
('comparing' = emptyCollectionHash)
('lookup' lookup:)
('copying' postCopy)
('object serialization' comeFullyUpOnReload:)
('[mkHashedCollections] accessing' size)
!


!Dictionary reorganize!
('accessing' associationAt: associationAt:ifAbsent: associationClass associations at: at:ifAbsent: at:ifAbsentOrNilPut: at:ifAbsentPut: at:ifPresent: at:ifPresent:ifAbsent: at:put: bindingOf: keyAtIdentityValue: keyAtIdentityValue:ifAbsent: keyAtValue: keyAtValue:ifAbsent: keys keysSortedSafely values)
('testing' hasBindingThatBeginsWith: includesKey: is: keyForIdentity: occurrencesOf:)
('adding' add: addAll:)
('removing' keysAndValuesRemove: remove: remove:ifAbsent: removeKey: removeKey:ifAbsent: removeUnreferencedKeys unreferencedKeys)
('enumerating' collect: keysAndValuesDo: keysDo: valuesDo:)
('printing' printElementsOn: storeOn:)
('private' errorKeyNotFound: errorValueNotFound keyAt: noCheckAdd: postCopy rehash scanFor:)
('inspecting' customizeExplorerContents explorerContents explorerContentsWithIndexCollect: inspectorClass)
('compiling' bindingsDo:)
('lookup' lookup:)
('comparing' = emptyCollectionHash hash)
('variable bindings' declare:from:)
('[mkHashedCollections] enumerating' associationsDo: do: select:)
!


!PositionableStream reorganize!
('reading' back contents contentsOfEntireFile crLfNextLine next: next:into: next:into:startingAt: nextAvailable: nextInto: nextInto:startingAt: nextKeyword nextLine nextWordsInto: peek peekBack peekFor: untilAnySatisfying: upTo: upTo:delimiterIsTerminator: upToAll: upToAny:)
('writing' next:putAll: next:putAll:startingAt:)
('testing' atEnd atStart isBinary isEmpty isFileStream isText notEmpty)
('positioning' match: matchStart: padTo:put: padToNextLongPut: reset resetContents setToEnd skip skip: skipBack skipTo:)
('fileIn/Out' backChunk checkForPreamble: compileNextChunk compileNextChunkHandlingExceptions compileNextChunkWhenDoesNotStartWithExclamationMark compileNextChunkWhenStartsWithExclamationMark evaluate:printingErrorWith: fileIn fileInAnnouncing: fileInInformingTo: nextChunk skipSeparators unCommand)
('private' collectionSpecies on: positionError setFrom:to:)
('gui' untilEnd:displayingProgress:)
('accessing' originalContents)
('mode' useBytes)
('[mk] positioning' position position:)
!


!PositionableStream class reorganize!
('instance creation')
('[mk] instance creation' on:)
!


!WriteStream reorganize!
('reading' contents next: nextWordsInto: peek upTo:delimiterIsTerminator:)
('writing' next:into:startingAt: next:putAll:startingAt: nextPut: nextPutAll: nextPutAllString:withAttributes:)
('testing' isEmpty isWriteOnly)
('positioning' resetToStart setToEnd)
('character writing' cr ensureASpace ensureNoSpace lf newLine newLineTab newLineTab: newLineTab:when: newLineWhen: nextPutKeyword:withArg: peekLast space: space:when: spaceWhen: tab tab: tab:when: tabWhen:)
('fileIn/Out' nextChunkPut: writeSystemTimeStamp)
('accessing' size)
('printing' store:)
('private' braceArray braceArray: growTo: isCompatibleWithContents: on: pastEndPut: with: withAttribute:do: withAttributes:do:)
('services' padToEndIfCantTruncate)
('[mk] reading' next)
('[mk] writing' space)
('[mk] positioning' position: reset)
!


!ContextPart reorganize!
('accessing' at: at:put: basicAt: basicAt:put: basicSize client contextForLocalVariables depthBelow home method methodNode methodReturnContext receiver size)
('instruction decoding' directedSuperSend:numArgs: doDup doNop doPop jump: jump:if: methodReturnConstant: methodReturnReceiver methodReturnTop popIntoLiteralVariable: popIntoReceiverVariable: popIntoRemoteTemp:inVectorAt: popIntoTemporaryVariable: pushActiveContext pushClosureCopyNumCopiedValues:numArgs:blockSize: pushConstant: pushLiteralVariable: pushNewArrayOfSize: pushReceiver pushReceiverVariable: pushRemoteTemp:inVectorAt: pushTemporaryVariable: return:from: send:super:numArgs: storeIntoLiteralVariable: storeIntoReceiverVariable: storeIntoRemoteTemp:inVectorAt: storeIntoTemporaryVariable:)
('debugger access' arguments contextStack depthBelow: errorReportOn: hasSender: messageForYourself methodClass namedTempAt: namedTempAt:put: print:on: releaseTo: selector shortErrorReportOn: shortStack singleRelease sourceCode stack stackOfSize: swapSender: tempNames tempsAndValues tempsAndValuesLimitedTo:indent:)
('controlling' activateMethod:withArgs:receiver: closureCopy:copiedValues: executeMethod:forSelector:withArgs:receiver: jump pop push: quickSend:to:with:lookupIn: restart resume resume: resumeEvaluating: return return: return:to: returnEvaluating: runUntilErrorOrReturnFrom: send:to:with: send:to:with:lookupIn: send:to:with:super: terminate terminateTo: top)
('printing' printDetails: printOn: printStack:)
('system simulation' completeCallee: quickStep runSimulated:contextAtEachStep: step stepToCallee stepToSendOrReturn)
('private' activateReturn:value: cannotReturn:to: cut: doPrimitive:method:receiver:args: finishIncompleteUnwindsUpTo: insertSender: isPrimFailToken: privSender: push:fromIndexable: runUntilReturnFrom: stackPtr stackp: stepToCalleeOrNil stepToSenderOrNil tryNamedPrimitiveIn:for:withArgs: unwindAndResume:evaluating: unwindAndStop unwindTo:safely: willFailReturn)
('private-exceptions' canHandleSignal: evaluateSignal: exceptionClass exceptionHandlerBlock findNextHandlerContext findNextHandlerOrSignalingContext findNextUnwindContextUpTo: handleSignal: isHandlerContext isHandlerOrSignalingContext isUnwindContext nextHandlerContext unwindTo:)
('objects from disk' storeDataOn:)
('query' bottomContext copyStack copyTo: findContextSuchThat: findSecondToOldestSimilarSender findSimilarSender hasContext: isBottomContext isContext isDead isDeadOrAtEnd isDone secondFromBottom)
('mirror primitives' object:basicAt: object:basicAt:put: object:eqeq: object:instVarAt: object:instVarAt:put: object:perform:withArguments:inClass: objectClass: objectSize:)
('closure support' contextTag)
('temporaries' tempAt: tempAt:put:)
('instruction decoding implicit literals' methodReturnSpecialConstant: pushSpecialConstant: sendSpecial:numArgs:)
('read-only objects' modificationForbiddenFor:at:put: modificationForbiddenFor:instVarAt:put: simulatedObject:instVarAt:put:)
('[mk] debugger access' sender)
!


!MethodContext reorganize!
('accessing' activeHome activeOuterContext closure contextForLocalVariables hasNonLocalReturn home isExecutingBlock methodReturnContext outerContext outerContextsDo: removeSelf tempAt: tempAt:put:)
('closure support' asContext)
('initialization' privRefresh privRefreshWith:)
('inspecting' inspectorClass)
('instruction decoding (closures)' blockReturnTop callPrimitive: pushConsArrayWithElements:)
('printing' printDetails: printOn: printString)
('system simulation' failPrimitiveWith: pushArgs:from:)
('testing' atEnd:)
('private' aboutToReturn:through: cannotReturn: endpc instVarAt:put: setSender:receiver:method:arguments: setSender:receiver:method:closure:startpc: startpc)
('private-exceptions' isHandlerOrSignalingContext isUnwindContext receiver: restartWithNewReceiver: swapReceiver:)
('[mk] accessing' method receiver)
!


!Message reorganize!
('accessing' argument arguments fullName keywords lookupClass numArgs sends:)
('printing' printOn: storeOn:)
('private' lookupClass: setSelector: setSelector:arguments:)
('sending' sendTo: sentTo:)
('stub creation' addGetterCodeOn: addSetterCodeOn:with: argumentNameAt:havingNamed: createStubMethodFor: writeMessageNameOn: writeOn:argumentNameAt:havingNamed: writeShouldBeImplementedOn:)
('testing' hasArguments hasRightNumberOfArguments isGetterFor: isSetterFor:)
('comparing' = analogousCodeTo: hasIdenticalContentsAs: hash)
('[mk] accessing' selector)
!


!Boolean reorganize!
('logical operations' eqv: xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or:)
('printing' isLiteral printAsLiteralOn: storeOn:)
('user interface support' asMenuItemTextPrefix)
('copying')
('testing' is:)
('[mk] logical operations' & | not)
('[mk] copying' shallowCopy)
!


!False reorganize!
('logical operations' xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or: orNot:)
('printing')
('user interface support' asMenuItemTextPrefix)
('[mk] logical operations' & | not)
('[mk] printing' printOn:)
!


!True reorganize!
('logical operations' xor:)
('controlling' and: and:and: and:and:and: and:and:and:and: ifFalse: ifFalse:ifTrue: ifTrue: ifTrue:ifFalse: or: or:or: or:or:or: or:or:or:or: orNot:)
('printing')
('user interface support' asMenuItemTextPrefix)
('[mk] logical operations' & | not)
('[mk] printing' printOn:)
!


!Magnitude reorganize!
('comparing' >= min:max:)
('[mk] comparing' < <= = > between:and: hash max: min:)
!


!Number reorganize!
('arithmetic' * + - / // \\ ^ div: mod: quo: reciprocal rem:)
('mathematical functions' arCosh arSinh arTanh arcCos arcSin arcTan arcTan: copySignTo: cos cosh cubed degreeCos degreeSin exp floorLog: lg ln log log: log2 magnitude nthRoot: raisedTo: raisedToFraction: raisedToInteger: raisedToNegativeInteger: sign: sin sinh sqrt squared tan tanh)
('interpolation' advanceFractionOf:towards: interpolateTo:at:)
('comparing' % = hash)
('truncation and round off' ceiling detentBy:atMultiplesOf:snap: fractionPart integerPart round4perMagnitudeOrder roundDownTo: roundTo: roundUpTo: rounded roundedHAFZ roundedHTE roundedToEven truncateTo: truncated)
('converting' @ adaptToCollection:andSend: adaptToFloat:andSend: adaptToFraction:andSend: adaptToInteger:andSend: adaptToPoint:andSend: asFloat asIntegerOrFloat asNumber asPoint asSmallAngleDegrees asSmallPositiveDegrees days degreesToRadians degreesToRadiansMinutes:seconds: hours inMinusPiToPiRange milliSeconds minutes moduloTwoPiAsFloat nanoSeconds radiansToDegrees seconds weeks withNegativeSign)
('intervals' to: to:by: to:by:do: to:count: to:do: to:do:separatedBy: toSelfPlus:)
('printing' isOrAreStringWith: printOn: printOn:base: printOn:fractionDigits: printOn:integerDigits:fractionDigits: printOn:integerDigits:padWith:fractionDigits:positiveIndicator: printString printStringBase: printStringFractionDigits: storeOn: storeOn:base: storeStringBase: withBinaryUnitPrefixAndValue: withDecimalUnitPrefixAndValue:)
('testing' even ifNotZero: is: isDivisibleBy: isFinite isInfinite isNaN isNumber isZero negative odd positive sign strictlyPositive)
('[mk] arithmetic' abs negated)
('[mk] truncation and round off' floor)
('[mk] converting' asInteger)
!


!Float class reorganize!
('instance creation' basicNew basicNew: fromIEEE32Bit: fromIEEE64Bit: new new: readFrom: signBit:mantissaBits:exponentBits: signPart:mantissaPart:exponentPart:)
('constants' e emax emin epsilon fmax fmin fminDenormalized fminNormalized halfPi infinity ln10 ln2 maxExactInteger nan negativeInfinity negativeZero one precision radix twoPi zero)
('class initialization' initClassCachedState initialize releaseClassCachedState)
('testing' denormalized nativeWordOrdering)
('[mkFloat] constants' pi)
!


!Integer reorganize!
('arithmetic' * + - / // alignedTo: crossSumBase: gcd: quo: reciprocalModulo: reciprocalModulo2: xgcd:)
('benchmarks' benchFib benchmark tinyBenchmarks)
('bit manipulation' << >> allMask: anyBitOfMagnitudeFrom:to: anyMask: bitAnd: bitAt: bitAt:put: bitClear: bitInvert bitInvert16 bitInvert32 bitOr: bitReverse: bitShift: bitShiftMagnitude: bitXor: highBit highBitOfMagnitude lowBit noMask:)
('comparing' < <= = > >= hash hashMultiply)
('converting' adaptToFraction:andSend: asColorOfDepth: asFloat asFraction asHexDigit asIntegerOrFloat moduloTwoPiAsFloat)
('enumerating' timesRepeat:)
('inspecting' customizeExplorerContents explorerContents hasContentsInExplorer)
('intervals' integersTo:count:)
('mathematical functions' factorial floatLog floorLog: ifMultipleOf2And5Do:otherwise: lcm: log log: log2 nthRoot: nthRootRounded: nthRootTruncated: productTo: raisedTo:modulo: raisedToFraction: raisedToInteger:modulo: sqrt sqrtFloor sqrtRounded sumTo: take:)
('printing' hex hex8 isLiteral numberOfDigitsInBase: printAsLiteralOn: printOn:base:length:padded: printOn:base:nDigits: printOn:fractionDigits: printOn:length:zeroPadded: printOn:thousandSeparator:includePlusSign: printStringAsBytes printStringAsBytesDecimal printStringBase:length:padded: printStringHex printStringRadix: printStringRoman printStringWithCommas printStringWords storeOn:base: storeStringHex)
('system primitives' lastDigit replaceFrom:to:with:startingAt:)
('testing' even is: isPowerOfTwo isPrime isProbablyPrime nextPrime)
('truncation and round off' atRandom atRandom: ceiling floor normalize roundedHAFZ roundedHTE truncated)
('private' copyto: digitAdd: digitCompare: digitDiv:neg: digitLogic:op:length: digitLshift: digitMultiply:neg: digitRshift:bytes:lookfirst: digitSubtract: growby: growto: isProbablyPrimeWithK:andQ: montgomeryDigitBase montgomeryDigitLength montgomeryDigitMax montgomeryNumberOfDigits montgomeryRaisedTo:times:modulo:mInvModB: montgomeryTimes:modulo:mInvModB: print:on:prefix:length:padded: romanDigits:for:on: slidingLeftRightRaisedTo:modulo:)
('[mk] converting' asInteger)
('[mk] testing' isInteger)
!


!SmallInteger reorganize!
('arithmetic' * + - / // \\ gcd: quo:)
('bit manipulation' bitAnd: bitOr: bitShift: bitXor: byteReversed lowBit)
('testing' isLarge odd)
('printing' decimalDigitLength numberOfDigitsInBase: printOn:base: printOn:base:length:padded: printOn:base:nDigits: printString printStringBase: printStringBase:nDigits: threeDigitName)
('system primitives' digitAt: digitAt:put: digitLength nextInstance nextObject)
('mathematical functions' sqrt)
('comparing' hashMultiply identityHash)
('copying' shallowCopy)
('converting' asFloat)
('[mk] comparing' < <= = > >= ~=)
('[mk] testing' even)
!


!LookupKey reorganize!
('accessing' canAssign name)
('testing' isSpecialReadBinding isVariableBinding)
('bindings' beBindingOfType:announcing: beReadOnlyBinding beReadOnlyBindingAnnouncing: beReadWriteBinding beReadWriteBindingAnnouncing: recompileBindingsAnnouncing:)
('literals' allLiteralsDo:)
('comparing' = hash)
('printing' printOn:)
('[mk] accessing' key key:)
('[mk] comparing' <)
!


!LookupKey class reorganize!
('instance creation')
('[mk] instance creation' key:)
!


!Association reorganize!
('printing' printOn: storeOn:)
('objects from disk' objectForDataStream:)
('testing' analogousCodeTo: isSpecialWriteBinding literalEqual:)
('accessing')
('comparing' = hash)
('[mk] accessing' key:value: value value:)
!


!Association class reorganize!
('instance creation')
('[mk] instance creation' key:value:)
!


!UndefinedObject reorganize!
('evaluating' valueWithPossibleArgs: valueWithPossibleArgument: valueWithPossibleArgument:and:)
('printing' printAsLiteralOn: storeOn:)
('testing' ifNil: ifNil:ifNotNil: ifNotNil: ifNotNil:ifNil: isEmptyOrNil isLiteral)
('dependents access' addDependent: suspend)
('class hierarchy' addSubclass: allSuperclassesUpTo: literalScannedAs:notifying: removeSubclass: subclass:instanceVariableNames:classVariableNames:poolDictionaries:category: subclasses subclassesDo: superclassNotValidErrorDescriptionFor: typeOfClass withAllSuperclassesDownFrom:)
('bottom context' canHandleSignal: handleSignal:)
('accessing instances and variables' whichClassDefinesInstanceVariable:ifNone:)
('copying')
('[mk] testing' isNil notNil)
('[mk] printing' printOn:)
('[mk] copying' shallowCopy)
!


!Character class reorganize!
('class initialization' accentedLetters initialize initializeNamedCharactersMap uppercaseLowercaseAndUnaccentedLetters vowels)
('instance creation' binarySelectorCharacters codePoint: digitValue: separators value:)
('accessing untypeable characters' backQuote backspace cr delete doubleQuote end escape home insert lf newLineCharacter newPage noBreakSpace null pageDown pageUp singleQuote smalltalkDownArrow smalltalkLeftArrow smalltalkRightArrow smalltalkUpArrow space tab zeroWidthSpace)
('converting' evaluate:withUtf16WordsOf: evaluate:withUtf8BytesOf: evaluateOnce:withUtf8BytesOf: isValidUtf8: macRomanToLatin1: utf8BytesAndCodePointAt:in:into:into: utf8BytesAndCodePointFor:byte2:byte3:byte4:into:into:assumeLatinIfInvalid: utf8BytesAndCodePointForIso8859s1:into:into: utf8BytesOf:)
('static state' basicLookupTables lowercaseMappingTable unaccentedTable uppercaseMappingTable)
('services' canonicalCompositionOf:and: canonicalDecomposition:on: combiningClass: isCombiningStarter: isDiacriticalMark:)
('constants' namedCharactersMap)
('private' constantNameFor: constantNames)
('[mk] instance creation' new)
!

