'From Cuis 5.0 [latest update: #4695] on 26 July 2021 at 9:24:54 am'!

!InputSensor methodsFor: 'mouse' stamp: 'ST-80 5/31/1983 9:10:35'!
mousePoint
	"Answer a Point indicating the coordinates of the current mouse location."
	^self primMousePt! !

!InputSensor methodsFor: 'mouse' stamp: 'ST-80 5/31/1983 9:10:35'!
waitClickButton
	"Wait for the user to click (press and then release) any mouse button and then
	answer with the current location of the cursor."
	self waitButton.
	^self waitNoButton! !


!ProcessorScheduler methodsFor: 'removing' stamp: 'ST-80 5/31/1983 9:10:35'!
remove: aProcess ifAbsent: aBlock 
	"Remove aProcess from the list on which it is waiting for the processor. If 
	it is not waiting, evaluate aBlock."

	(quiescentProcessLists at: aProcess priority)
		remove: aProcess ifAbsent: aBlock.
	^aProcess! !

!ProcessorScheduler methodsFor: 'process state change' stamp: 'ST-80 5/31/1983 9:10:35'!
suspendFirstAt: aPriority ifNone: noneBlock 
	"Suspend the first Process that is waiting to run with priority aPriority.  
	If no Process is waiting, evaluate noneBlock"

	| aList |
	aList _ quiescentProcessLists at: aPriority.
	aList isEmpty
		ifTrue: [^noneBlock value]
		ifFalse: [^aList first suspend]! !


!Collection methodsFor: 'adding' stamp: 'ST-80 5/31/1983 9:10:35'!
add: newObject 
	"Include newObject as one of the receiver's elements.  Answer newObject.
	This message should not be sent to instances of subclasses of ArrayedCollection."

	self subclassResponsibility! !

!Collection methodsFor: 'adding' stamp: 'ST-80 5/31/1983 9:10:35'!
addAll: aCollection 
	"Include all the elements of aCollection as the receiver's elements.  Answer
	aCollection."

	aCollection do: [:each | self add: each].
	^aCollection! !

!Collection methodsFor: 'enumerating' stamp: 'ST-80 5/31/1983 9:10:35'!
collect: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.  Collect the 
	resulting values into a collection that is like the receiver.  Answer the new 
	collection. "

	| newCollection |
	newCollection _ self species new.
	self do: [:each | newCollection add: (aBlock value: each)].
	^newCollection! !

!Collection methodsFor: 'enumerating' stamp: 'ST-80 5/31/1983 9:10:35'!
detect: aBlock ifNone: exceptionBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Answer the first element for which aBlock evaluates to true."

	self do: [:each | (aBlock value: each) ifTrue: [^each]].
	^exceptionBlock value! !

!Collection methodsFor: 'enumerating' stamp: 'ST-80 5/31/1983 9:10:35'!
inject: thisValue into: binaryBlock 
	"Accumulate a running value associated with evaluating the argument,
	binaryBlock, with the current value and the receiver as block arguments. 
	The initial value is the value of the argument, thisValue.
		For instance, to sum a collection, use: 
			collection inject: 0 into: [:subTotal :next | subTotal + next]."

	| nextValue |
	nextValue _ thisValue.
	self do: [:each | nextValue _ binaryBlock value: nextValue value: each].
	^nextValue! !

!Collection methodsFor: 'enumerating' stamp: 'ST-80 5/31/1983 9:10:35'!
reject: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect into a new collection like the receiver, only those elements for which
	aBlock evaluates to false.  Answer the new collection."

	^self select: [:element | (aBlock value: element) == false]! !

!Collection methodsFor: 'enumerating' stamp: 'ST-80 5/31/1983 9:10:35'!
select: aBlock 
	"Evaluate aBlock with each of the receiver's elements as the argument. 
	Collect into a new collection like the receiver, only those elements for which
	aBlock evaluates to true.  Answer the new collection."

	| newCollection |
	newCollection _ self species new.
	self do: [:each | (aBlock value: each) ifTrue: [newCollection add: each]].
	^newCollection! !

!Collection methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
storeOn: aStream 
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet _ true.
	self do: 
		[:each | 
		noneYet
			ifTrue: [noneYet _ false]
			ifFalse: [aStream nextPut: $;].
		aStream nextPutAll: ' add: '.
		aStream store: each].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)! !

!Collection methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
emptyCheck
	self isEmpty ifTrue: [self errorEmptyCollection]! !

!Collection methodsFor: 'removing' stamp: 'ST-80 5/31/1983 9:10:35'!
remove: oldObject ifAbsent: anExceptionBlock 
	"Remove oldObject as one of the receiver's elements.  If several of the
	elements are equal to oldObject, only one is removed. If no element is equal to
	oldObject, answer the result of evaluating anExceptionBlock.  Otherwise,
	answer the argument, oldObject.

	SequenceableCollections can not respond to this message."

	self subclassResponsibility! !

!Collection methodsFor: 'removing' stamp: 'ST-80 5/31/1983 9:10:35'!
removeAll: aCollection 
	"Remove each element of aCollection from the receiver.  If successful for each,
	answer aCollection."

	aCollection do: [:each | self remove: each].
	^aCollection! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
indexOf: anElement 
	"Answer the index of anElement within the receiver.  If the receiver does
	not contain anElement, answer 0."

	^self indexOf: anElement ifAbsent: [0]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
indexOfSubCollection: aSubCollection startingAt: anIndex 
	"Answer the index of the receiver's first element, such that that element 
	equals the first element of aSubCollection, and the next elements equal the rest of
	the elements of aSubCollection.  Begin the search at element anIndex of the 
	receiver.  If no such match is found, answer 0."

	^self
		indexOfSubCollection: aSubCollection
		startingAt: anIndex
		ifAbsent: [0]! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
replaceFrom: start to: stop with: replacement 
	"This destructively replaces elements from start to stop in the receiver.  
	Answer the receiver itself.
	Use copyReplaceFrom:to:with: for insertion/deletion which may alter the 
	size of the result."

	replacement size = (stop - start + 1)
		ifFalse: [self error: 'Size of replacement doesnt match'].
	^self replaceFrom: start to: stop with: replacement startingAt: 1! !

!SequenceableCollection methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	"This destructively replaces elements from start to stop in the receiver
	starting at index, repStart, in the collection, replacement.  Answer the
	receiver.  No range checks are performed - this may be primitively implemented."

	| index repOff |
	repOff _ repStart - start.
	index _ start - 1.
	[(index _ index + 1) <= stop]
		whileTrue: [self at: index put: (replacement at: repOff + index)]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'ST-80 5/31/1983 9:10:35'!
reverseDo: aBlock
	"Evaluate aBlock with each of the receiver's elements as the argument, starting
	with the last element and taking each in sequence up to the first.  For
	SequenceableCollections, this is the reverse of the enumeration in do:."

	self size to: 1 by: -1 do: [:index | aBlock value: (self at: index)]! !

!SequenceableCollection methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
errorOutOfBounds
	self error: 'indices are out of bounds'! !


!ArrayedCollection methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
size
	"Answer the number of indexable fields in the receiver. This value is the 
	same as the largest legal subscript. Primitive is specified here to override 
	SequenceableCollection size. Essential. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 62>
	^self basicSize! !

!ArrayedCollection methodsFor: 'adding' stamp: 'ST-80 5/31/1983 9:10:35'!
add: newObject
	self shouldNotImplement! !

!ArrayedCollection methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
storeOn: aStream 
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new: '.
	aStream store: self size.
	aStream nextPut: $).
	(self storeElementsFrom: 1 to: self size on: aStream)
		ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)! !

!ArrayedCollection methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
defaultElement
	^nil! !

!ArrayedCollection methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
storeElementsFrom: firstIndex to: lastIndex on: aStream 
	| noneYet defaultElement arrayElement |
	noneYet _ true.
	defaultElement _ self defaultElement.
	firstIndex to: lastIndex do: 
		[:index | 
		arrayElement _ self at: index.
		arrayElement = defaultElement
			ifFalse: 
				[noneYet
					ifTrue: [noneYet _ false]
					ifFalse: [aStream nextPut: $;].
				aStream nextPutAll: ' at: '.
				aStream store: index.
				aStream nextPutAll: ' put: '.
				aStream store: arrayElement]].
	^noneYet! !


!ArrayedCollection class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
new: size withAll: value 
	"Answer a new instance of me, whose every element is equal to the argument,
	value."

	^(self new: size) atAllPut: value! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
with: firstObject with: secondObject with: thirdObject 
	"Answer a new instance of me, containing only these three objects."

	| newCollection |
	newCollection _ self new: 3.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	^newCollection! !

!ArrayedCollection class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
with: firstObject with: secondObject with: thirdObject with: fourthObject 
	"Answer a new instance of me, containing the four arguments as the elements."

	| newCollection |
	newCollection _ self new: 4.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	^newCollection! !


!String methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
string
	"Answer the receiver itself.  This is for compatibility with other text classes."
	^self! !

!String methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asString
	"Answer the receiver itself."
	^self! !

!String methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asSymbol
	"Answer the unique symbol whose characters are the characters of the string."
	^Symbol intern: self! !

!String methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asText
	"Answer a Text whose string is the receiver."
	^Text fromString: self! !

!String methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
isLiteral
	^true! !


!String class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
readFrom: inStream
	"Answer a new String that is determined by reading the stream, inStream.  Embedded
	double quotes become the quote Character."

	| outStream char done |
	outStream _ WriteStream on: (String new: 16).
	"go to first quote"
	inStream skipTo: $'.
	done _ false.
	[done or: [inStream atEnd]]
		whileFalse: 
			[char _ inStream next.
			char = $'
				ifTrue: 
					[char _ inStream next.
					char = $'
						ifTrue: [outStream nextPut: char]
						ifFalse: [done _ true]]
				ifFalse: [outStream nextPut: char]].
	^outStream contents! !


!Symbol methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
at: anInteger put: anObject 
	"you can not modify the receiver."

	self errorNoModification! !

!Symbol methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
replaceFrom: start to: stop with: replacement startingAt: repStart 
	self errorNoModification! !

!Symbol methodsFor: 'copying' stamp: 'ST-80 5/31/1983 9:10:35'!
shallowCopy
	"Answer with me, because Symbols are unique."! !

!Symbol methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asString
	| newString |
	newString _ String new: self size.
	1 to: self size do: [:index | newString at: index put: (self at: index)].
	^newString! !

!Symbol methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asSymbol! !

!Symbol methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
errorNoModification
	self error:  'symbols can not be modified.'! !

!Symbol methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
species
	^String! !

!Symbol methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
string: aString 
	1 to: aString size do: [:j | super at: j put: (aString at: j)].
	^self! !


!ByteArray methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
defaultElement
	^0! !


!CompiledMethod methodsFor: 'literals' stamp: 'ST-80 5/31/1983 9:10:35'!
header
	"Answer the word containing the information about the form of the
	receiver and the form of the context needed to run the receiver."

	^self objectAt: 1! !

!CompiledMethod methodsFor: 'literals' stamp: 'ST-80 5/31/1983 9:10:35'!
literalAt: index put: value 
	"Replace the literal indexed by the first argument with the
	second argument."

	^self objectAt: index + 1 put: value! !

!CompiledMethod methodsFor: 'literals' stamp: 'ST-80 5/31/1983 9:10:35'!
objectAt: index 
	"Answer with the method header (if index=1) or a literal (if index >1) from the 
	receiver.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 68>
	self primitiveFailed! !

!CompiledMethod methodsFor: 'literals' stamp: 'ST-80 5/31/1983 9:10:35'!
objectAt: index put: value 
	"Store the value argument into a literal in the receiver.   An index of 2 
	corresponds to the first literal.  Fails if the index is less than 2 or greater than 
	the number of literals.  Answer the value as the result.  Normally only the
	compiler sends this message, because only the compiler stores values in 
	CompiledMethods.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 69>
	self primitiveFailed! !

!CompiledMethod methodsFor: 'scanning' stamp: 'ST-80 5/31/1983 9:10:35'!
scanLongLoad: extension 
	"Answer whether the receiver contains a long load whose extension is the
	argument."

	| scanner |
	scanner _ InstructionStream on: self.
	^scanner scanFor: [:instr | instr = 128 and: [scanner followingByte = extension]]! !


!Interval methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
first
	^start! !

!Interval methodsFor: 'adding' stamp: 'ST-80 5/31/1983 9:10:35'!
add: newObject 
	"Adding to an Interval is not allowed."
	self shouldNotImplement! !

!Interval methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
storeOn: aStream 
	"This is possible because we know numbers store and print the same"

	self printOn: aStream! !

!Interval methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
species
	^Array! !


!LinkedList methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
first
	"Answer the first link;  create an error if the receiver is empty."

	self emptyCheck.
	^firstLink! !

!LinkedList methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
last
	"Answer the last link;  create an error if the receiver is empty."

	self emptyCheck.
	^lastLink! !

!LinkedList methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
isEmpty
	^firstLink == nil! !

!LinkedList methodsFor: 'adding' stamp: 'ST-80 5/31/1983 9:10:35'!
add: aLink 
	"Add aLink to the end of the receiver's list."

	^self addLast: aLink! !

!LinkedList methodsFor: 'adding' stamp: 'ST-80 5/31/1983 9:10:35'!
addFirst: aLink 
	"Add aLink to the beginning of the receiver's list."

	self isEmpty ifTrue: [lastLink _ aLink].
	aLink nextLink: firstLink.
	firstLink _ aLink.
	^aLink! !

!LinkedList methodsFor: 'adding' stamp: 'ST-80 5/31/1983 9:10:35'!
addLast: aLink 
	"Add aLink to the end of the receiver's list."

	self isEmpty
		ifTrue: [firstLink _ aLink]
		ifFalse: [lastLink nextLink: aLink].
	lastLink _ aLink.
	^aLink! !

!LinkedList methodsFor: 'removing' stamp: 'ST-80 5/31/1983 9:10:35'!
removeFirst
	"Remove the first element.  If the receiver is empty, cause an error;
	otherwise answer the removed element.  Using the sequence addFirst:/removeFirst
	causes the receiver to behave as a stack; using addLast:/removeFirst causes the
	receiver to behave as a queue."

	| oldLink |
	self emptyCheck.
	oldLink _ firstLink.
	firstLink == lastLink
		ifTrue: [firstLink _ nil. lastLink _ nil]
		ifFalse: [firstLink _ oldLink nextLink].
	oldLink nextLink: nil.
	^oldLink! !

!LinkedList methodsFor: 'removing' stamp: 'ST-80 5/31/1983 9:10:35'!
removeLast
	"Remove the receiver's last element.  If the receiver is empty, cause an error;
	otherwise answer the removed element.  Using addLast:/removeLast causes the
	receiver to behave as a stack; using addFirst:/removeLast causes the receiver to
	behave as a queue."

	| oldLink aLink |
	self emptyCheck.
	oldLink _ lastLink.
	firstLink == lastLink
		ifTrue: [firstLink _ nil. lastLink _ nil]
		ifFalse: [aLink _ firstLink.
				[aLink nextLink == oldLink] whileFalse:
					[aLink _ aLink nextLink].
				 aLink nextLink: nil.
				 lastLink _ aLink].
	oldLink nextLink: nil.
	^oldLink! !

!LinkedList methodsFor: 'enumerating' stamp: 'ST-80 5/31/1983 9:10:35'!
do: aBlock
	| aLink |
	aLink _ firstLink.
	[aLink == nil] whileFalse:
		[aBlock value: aLink.
		 aLink _ aLink nextLink]! !


!Semaphore methodsFor: 'communication' stamp: 'ST-80 5/31/1983 9:10:35'!
signal
	"Send a signal through the receiver. If one or more processes have been 
	suspended trying to receive a signal, allow the first one to proceed. If no 
	process is waiting, remember the excess signal. Essential. See Object documentation 
	whatIsAPrimitive. "

	<primitive: 85>
	self primitiveFailed

	"self isEmpty    
		ifTrue: [excessSignals _ excessSignals+1]    
		ifFalse: [Processor resume: self removeFirstLink]"! !

!Semaphore methodsFor: 'communication' stamp: 'ST-80 5/31/1983 9:10:35'!
wait
	"The active Process must receive a signal through the receiver before 
	proceeding.  If no signal has been sent, the active Process will be suspended
	until one is sent.  Essential.  See  
	Object whatIsAPrimitive."

	<primitive: 86>
	self primitiveFailed

	"excessSignals>0  
		ifTrue: [excessSignals _ excessSignals-1]  
		ifFalse: [self addLastLink: Processor activeProcess suspend]"! !

!Semaphore methodsFor: 'initialize-release' stamp: 'ST-80 5/31/1983 9:10:35'!
initSignals
	"Consume any excess signals the receiver may have accumulated."

	excessSignals _ 0! !


!Semaphore class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
forMutualExclusion
	"Answer a new instance of me that contains a single signal.  
	This new instance can now be used for mutual exclusion (see the 
	critical: message to Semaphore)."

	^self new signal! !


!OrderedCollection methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
size
	^lastIndex - firstIndex + 1! !

!OrderedCollection methodsFor: 'adding' stamp: 'ST-80 5/31/1983 9:10:35'!
add: newObject 
	^self addLast: newObject! !

!OrderedCollection methodsFor: 'adding' stamp: 'ST-80 5/31/1983 9:10:35'!
add: newObject after: oldObject 
	"Add the argument, newObject, as an element of the receiver.  Put it
	in the position just succeeding oldObject.  Answer newObject."
	
	| index |
	index _ self find: oldObject.
	self insert: newObject before: index + 1.
	^newObject! !

!OrderedCollection methodsFor: 'adding' stamp: 'ST-80 5/31/1983 9:10:35'!
add: newObject before: oldObject 
	"Add the argument, newObject, as an element of the receiver.  Put it
	in the position just preceding oldObject.  Answer newObject."
	
	| index |
	index _ self find: oldObject.
	self insert: newObject before: index.
	^newObject! !

!OrderedCollection methodsFor: 'adding' stamp: 'ST-80 5/31/1983 9:10:35'!
addAllFirst: anOrderedCollection 
	"Add each element of anOrderedCollection at the beginning of the receiver.	
	Answer anOrderedCollection."

	anOrderedCollection reverseDo: [:each | self addFirst: each].
	^anOrderedCollection! !

!OrderedCollection methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
errorNoSuchElement
	self error: 'attempt to index non-existent element in an ordered collection'! !


!OrderedCollection class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
new
	^self new: 10! !


!RunArray methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
values
	"Answer the values in the receiver."
	^values! !

!RunArray methodsFor: 'copying' stamp: 'ST-80 5/31/1983 9:10:35'!
copyReplaceFrom: start to: stop with: replacement 
	^(self copyFrom: 1 to: start - 1)
		, replacement 
		, (self copyFrom: stop + 1 to: self size)! !

!RunArray methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
storeOn: aStream
	aStream nextPut: $(.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' runs: '.
	runs storeOn: aStream.
	aStream nextPutAll: ' values: '.
	values storeOn: aStream.
	aStream nextPut: $)! !

!RunArray methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
runs
	^runs! !


!RunArray class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
new
	^self runs: Array new values: Array new! !

!RunArray class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
new: size withAll: value 
	"Answer a new instance of me, whose every element is equal to the argument,
	value."

	size = 0 ifTrue: [^self new].
	^self runs: (Array with: size) values: (Array with: value)! !

!RunArray class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
runs: newRuns values: newValues 
	"Answer a new instance of RunArray with runs and values specified
	by the arguments."

	| instance |
	instance _ self basicNew.
	instance setRuns: newRuns setValues: newValues.
	^instance! !


!Text methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
at: index 
	^string at: index! !

!Text methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
findString: aString startingAt: start 
	"Answer the index of subString within the receiver, starting at position start.
	If the receiver does not contain subString, answer 0."

	^string findString: aString asString startingAt: start! !

!Text methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
size 
	^string size! !

!Text methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
string
	"Answer the string representation of the receiver."
	^string! !

!Text methodsFor: 'copying' stamp: 'ST-80 5/31/1983 9:10:35'!
copyFrom: start to: stop 
	"Answer with a copied subrange of this text"

	| realStart realStop |
	stop > self size
		ifTrue: [realStop _ self size]		"handle selection at end of string"
		ifFalse: [realStop _ stop].
	start < 1
		ifTrue: [realStart _ 1]			"handle selection before start of string"
		ifFalse: [realStart _ start].
	^Text 
		string: (string copyFrom: realStart to: realStop)
		runs: (runs copyFrom: realStart to: realStop)! !

!Text methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asNumber
	"Answer the number created by interpreting the receiver as the textual
	representation of a number."

	^string asNumber! !

!Text methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asString
	"Answer a String representation of the textual receiver."
	^string! !

!Text methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asText	
	"Answer the receiver itself."
	^self! !

!Text methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
storeOn: aStream 
	aStream nextPutAll: '(Text string: ';
		store: string;
		nextPutAll: ' runs: ';
		store: runs;
		nextPut: $)! !

!Text methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
runs
	^runs! !


!Text class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
new: stringSize 
	^self fromString: (String new: stringSize)! !

!Text class methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
string: aString runs: anArray  
	^self basicNew setString: aString setRuns: anArray! !


!Bag methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
at: index 
	self errorNotKeyed! !

!Bag methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
at: index put: anObject
	self errorNotKeyed! !

!Bag methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
sortedCounts
	"Answer with a collection of counts with elements, sorted by decreasing count."
	| counts |
	counts _ SortedCollection sortBlock: [:x :y | x >= y].
	contents associationsDo:
		[:assn |
		counts add: (Association key: assn value value: assn key)].
	^ counts! !

!Bag methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
sortedElements
	"Answer with a collection of elements with counts, sorted by element."
	| elements |
	elements _ SortedCollection new.
	contents associationsDo: [:assn | elements add: assn].
	^ elements! !

!Bag methodsFor: 'adding' stamp: 'ST-80 5/31/1983 9:10:35'!
add: newObject 
	^self add: newObject withOccurrences: 1! !

!Bag methodsFor: 'enumerating' stamp: 'ST-80 5/31/1983 9:10:35'!
do: aBlock 
	contents associationsDo: [:assoc | assoc value timesRepeat: [aBlock value: assoc key]]! !

!Bag methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
includes: anObject 
	^contents includesKey: anObject! !

!Bag methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
occurrencesOf: anObject 
	(self includes: anObject)
		ifTrue: [^contents at: anObject]
		ifFalse: [^0]! !


!Set methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
size
	^tally! !


!Dictionary methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
associationAt: key 
	"Answer the association at key.  If key is not found, create an error message."

	^self associationAt: key ifAbsent: [self errorKeyNotFound]! !

!Dictionary methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
at: key 
	"Answer the value at key.  If key is not found, create an error message."

	^self at: key ifAbsent: [self errorKeyNotFound]! !

!Dictionary methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
keyAtValue: value 
	"Answer the key whose value equals the argument, value.  If there is none,
	cause an error."

	^self keyAtValue: value ifAbsent: [self errorValueNotFound]! !

!Dictionary methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
includes: anObject 
	self do: [:each | anObject = each ifTrue: [^true]].
	^false! !

!Dictionary methodsFor: 'removing' stamp: 'ST-80 5/31/1983 9:10:35'!
remove: anObject 
	self shouldNotImplement! !

!Dictionary methodsFor: 'removing' stamp: 'ST-80 5/31/1983 9:10:35'!
remove: anObject ifAbsent: exceptionBlock 
	self shouldNotImplement! !

!Dictionary methodsFor: 'removing' stamp: 'ST-80 5/31/1983 9:10:35'!
removeKey: key 
	"Remove key from the receiver.  If key is not in the receiver, create an error
	message.  Otherwise, answer the value associated with key."

	^self removeKey: key ifAbsent: [self errorKeyNotFound]! !

!Dictionary methodsFor: 'enumerating' stamp: 'ST-80 5/31/1983 9:10:35'!
associationsDo: aBlock 
	"Evaluate aBlock for each of the receiver's key/value associations."

	super do: aBlock! !

!Dictionary methodsFor: 'enumerating' stamp: 'ST-80 5/31/1983 9:10:35'!
do: aBlock 
	super do: [:assoc | aBlock value: assoc value]! !

!Dictionary methodsFor: 'enumerating' stamp: 'ST-80 5/31/1983 9:10:35'!
select: aBlock 
	"Evaluate aBlock with each of my values as the argument.  Collect into a new 
	dictionary, only those associations for which aBlock evaluates to true."

	| newCollection |
	newCollection _ self species new.
	self associationsDo: 
		[:each | 
		(aBlock value: each value) ifTrue: [newCollection add: each]].
	^newCollection! !

!Dictionary methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
errorValueNotFound
	self error: 'value not found'! !


!SystemDictionary methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
exitToDebugger
	"Enter the machine language debugger, if one exists.  Essential.  See Object 
	documentation whatIsAPrimitive. "

	<primitive: 114>
	self primitiveFailed! !

!SystemDictionary methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
quitPrimitive
	"Exit to another operating system on the host machine, if one exists.  All 
	state changes in the object space since the last snapshot are lost.  Essential.  
	See Object documentation whatIsAPrimitive."

	<primitive: 113>
	self primitiveFailed! !

!SystemDictionary methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
specialSelectors
	"Used by SystemTracer only"

	^SpecialSelectors! !

!SystemDictionary methodsFor: 'special selectors' stamp: 'ST-80 5/31/1983 9:10:35'!
hasSpecialSelector: aLiteral ifTrueSetByte: aBlock
	1 to: self specialSelectorSize do:
		[:index | 
		(self specialSelectorAt: index) == aLiteral
			ifTrue: [aBlock value: index + 16rAF. ^true]].
	^false! !


!SharedQueue methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
nextPut: value 
	"Send value through the receiver.  If a Process has been suspended waiting to 
	receive a value through the receiver, allow it to proceed."

	accessProtect
		critical: [writePosition > contentsArray size
						ifTrue: [self makeRoomAtEnd].
				 contentsArray at: writePosition put: value.
				 writePosition _ writePosition + 1].
	readSynch signal.
	^value! !

!SharedQueue methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
isEmpty
	"Answer whether any objects have been sent through the receiver
	and not yet received by anyone."

	^readPosition = writePosition! !


!Stream methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
contents
	"Answer the contents of the receiver."
	self subclassResponsibility! !


!Stream class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
new
	self error: 'Streams are created with on: and with:'! !


!Semaphore reorganize!
('communication' signal wait waitTimeoutMSecs: waitTimeoutSeconds:)
('mutual exclusion' critical: critical:ifCurtailed: critical:ifError: critical:ifLocked:)
('comparing' = hash)
('testing' isSignaled)
('initialization' terminateProcess)
('private' species)
('printing' printOn:)
('initialize-release' initSignals)
!


!RunArray reorganize!
('accessing' = at: canJoinMessage: first last runLengthAt: size values withStartStopAndValueDo:)
('adding' add: addFirst: addLast: coalesce rangeOf:startingAt:)
('copying' , copy copyFrom:to: copyReplaceFrom:to:with:)
('printing' printOn: storeOn: writeOn:)
('private' at:setRunOffsetAndValue: basicReplaceAttributesFrom:to:with: canJoin:and: find: mapValues: runs setRuns:setValues:)
('enumerating' runsAndValuesDo: runsFrom:to:do:)
('converting' reversed)
('testing' is:)
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


!SystemDictionary reorganize!
('accessing' classes description name organization summary)
('class names' classNamed: fillCaches flushClassNameCache forgetClass:logged: hasClassNamed: prepareToRenameClass:as: prepareToRenameClass:from:to: removeClassNamed: removeClassNamedIfInBaseSystem: renameAndUpdateReferences:as: renameClassNamed:as: renamedClass:from:)
('dictionary access' associationOrUndeclaredAt: at:put:)
('housekeeping' browseEqEqSentToSmallIntegerConstants browseObsoleteMethodReferences browseUndeclaredReferences cleanCompactObsoleteClasses cleanOutUndeclared condenseChanges condenseSources macroBenchmark1 macroBenchmark3 obsoleteBehaviors obsoleteClasses obsoleteMethodReferences recompileAllFrom: removeEmptyMessageCategories testDecompiler testFormatter testFormatter2 verifyChanges)
('image, changes name' alternativeSourcesName changeImageNameTo: currentChangesName currentSourcesName defaultChangesName defaultSourcesName defaultUserChangesName fullNameForChangesNamed: fullNameForImageNamed: imageName imageName: imagePath primImageName primVmPath vmPath)
('memory space' bytesLeft bytesLeft: bytesLeftString createStackOverflow garbageCollect garbageCollectMost growMemoryByAtLeast: installLowSpaceWatcher lowSpaceThreshold lowSpaceWatcher lowSpaceWatcherProcess okayToProceedEvenIfSpaceIsLow primBytesLeft primLowSpaceSemaphore: primSignalAtBytesLeft: primitiveGarbageCollect signalLowSpace stopLowSpaceWatcher useUpMemory useUpMemoryWithArrays useUpMemoryWithContexts useUpMemoryWithTinyObjects)
('code authors' allContributors contributionsOf: knownInitialsAndNames unknownContributors)
('miscellaneous' cogitClass detectBuiltInModule: extraVMMemory extraVMMemory: getCurrentWorkingDirectory getSystemAttribute: getVMParameters handleUserInterrupt interpreterClass isDevelopmentEnvironmentPresent isHeadless isModuleAvailable: listBuiltinModule: listBuiltinModules listLoadedModule: listLoadedModules logError:inContext:to: maxExternalSemaphores maxExternalSemaphores: osVersion platformName platformSubtype primGetCurrentWorkingDirectoryUnix primGetCurrentWorkingDirectoryWindows primVmFileName unloadModule: vmFileName vmOptionsDo: vmVersion voidCogVMState)
('object serialization' objectForDataStream: storeDataOn:)
('printing' printElementsOn: printOn:)
('profiling' clearProfile dumpProfile profile: startProfiling stopProfiling)
('retrieving' allBehaviorsDo: allCallsOn: allCallsOn:and: allClasses allClassesDo: allClassesImplementing: allGlobalRefs allGlobalRefsWithout: allImplementedMessages allImplementedMessagesWithout: allImplementorsOf: allImplementorsOf:localTo: allMethodsInCategory: allMethodsSourceStringMatching: allMethodsWithSourceString:matchCase: allMethodsWithString: allObjects allObjectsDo: allObjectsOrNil allPrimitiveMessages allPrimitiveMethodsInCategories: allReferencesToLiteral: allSelect: allSentMessages allSentMessagesWithout: allUnSentMessages allUnSentMessagesIn: allUnSentMessagesWithout: allUnimplementedCalls allUnusedClassesWithout: hierachySortedAllClassesDo: isThereAReferenceTo: isThereAnImplementorOf: isThereAnImplementorOf:exceptFor: numberOfImplementorsOf: numberOfSendersOf: pointersTo: pointersTo:except: poolUsers unimplemented)
('shrinking' abandonSources presumedSentMessages reduceCuis removeAllUnSentMessages removeSelector: removedUnusedClassesAndMethods reportClassAndMethodRemovalsFor: unusedClasses unusedClassesAndMethodsWithout:)
('snapshot and quit' add:toList:after: addToShutDownList: addToShutDownList:after: addToStartUpList: addToStartUpList:after: lastQuitLogPosition logSnapshot:andQuit: nopTag okayToDiscardUnsavedCode okayToSave printStuffToCleanOnImageSave processShutDownList: processStartUpList: quitNoSaveTag quitPrimitive: quitTag removeFromShutDownList: removeFromStartUpList: saveAs saveAs:andQuit:clearAllClassState: saveAsNewVersion saveSession send:toClassesNamedIn:with: setGCParameters setPlatformPreferences snapshot:andQuit: snapshot:andQuit:clearAllClassState: snapshot:andQuit:embedded: snapshot:andQuit:embedded:clearAllClassState: snapshotEmbeddedPrimitive snapshotMessageFor:andQuit: snapshotPrimitive snapshotTag snapshotTagFor:andQuit: storeOn: tagHeader tagTail unbindExternalPrimitives)
('sources, change log' aboutThisSystem assureStartupStampLogged calcEndianness classRemoved:fromCategory: closeSourceFiles endianness evaluated:context: externalizeSources forceChangesToDisk internalizeChangeLog internalizeSources isBigEndian isLittleEndian lastUpdateString logChange: logChange:preamble: methodRemoved:selector:inProtocol:class: openSourceFiles openSourcesAndChanges systemInformationString timeStamp: version)
('special objects' clearExternalObjects compactClassesArray compactClassesArrayIncludes: externalObjects primitiveErrorTable recreateSpecialObjectsArray registerExternalObject: specialNargsAt: specialObjectsArray specialSelectorAt: specialSelectorSize unregisterExternalObject:)
('ui' beep confirmRemovalOf:on: globals inspectGlobals primitiveBeep systemCategoryFromUserWithPrompt:)
('browsing' browseAllAccessesTo:from: browseAllCallsOn: browseAllCallsOn:and: browseAllCallsOn:localTo: browseAllCallsOnClass: browseAllImplementorsOf: browseAllImplementorsOf:localTo: browseAllImplementorsOfList: browseAllImplementorsOfList:title: browseAllMethodsInCategory: browseAllPrimitives browseAllReferencesToLiteral: browseAllSelect: browseAllSelect:name:autoSelect: browseAllStoresInto:from: browseAllUnSentMessages browseAllUnimplementedCalls browseClassCommentsWithString: browseClassesWithNamesContaining:caseSensitive: browseEqSmallConstant browseInstVarDefs: browseInstVarRefs: browseLikelyUnnededRedefinitions browseMessageList:name: browseMessageList:name:autoSelect: browseMessageList:ofSize:name:autoSelect: browseMethodsWithSourceString: browseMethodsWithString: browseMethodsWithString:matchCase: browseMyChanges browseObsoleteReferences browseViewReferencesFromNonViews showMenuOf:withFirstItem:ifChosenDo: showMenuOf:withFirstItem:ifChosenDo:withCaption:)
('private' allSymbolsIn:do: baseLabel exitToDebugger quitPrimitive specialSelectors)
('Closure measurements' browseMethodsWithClosuresThatAccessOuterTemps browseMethodsWithClosuresThatOnlyReadOuterTemps browseMethodsWithClosuresThatWriteOuterTemps browseMethodsWithClosuresThatWriteOuterTempsButCleanOtherwise browseMethodsWithEmptyClosures browseMethodsWithMoreThanOneClosure browseMethodsWithOnlyCleanClosures closuresInfoStringForClass:selector: eliotsClosureMeasurements eliotsClosureMeasurements2 eliotsClosureMeasurements2On: eliotsClosureMeasurementsOn:over:)
('removing' removeKey:ifAbsent:)
('system attributes' flagInterpretedMethods: isRunningCog isRunningCogit isSpur maxIdentityHash processHasThreadIdInstVar: processPreemptionYields processPreemptionYields:)
('query' hierarchySorted:do: methodsWithUnboundGlobals unusedBlocks)
('testing' hasBindingThatBeginsWith: isLiveSmalltalkImage isLiveTypingInstalled)
('classes and traits' classNames nonClassNames)
('image format' imageFormatVersion imageFormatVersionFromFile imageFormatVersionFromFileAsIs)
('AndreasProfiler-profiling' interruptChecksPerMSec longRunningPrimitive longRunningPrimitiveSemaphore: profilePrimitive profileSample profileSemaphore: profileStart:)
('startup' doStartUp: processCommandLineArgument:storeStartUpScriptArgsOn: processCommandLineArguments readCommandLineArguments setStartupStamp startUpArguments startUpScriptArguments)
('startup - restore lost changes' hasToRestoreChanges hasToRestoreChangesFrom: isQuitNoSaveRecord: isQuitRecord: isSnapshotQuitOrQuitNoSaveRecord: isSnapshotRecord: lostChangesDetectedCaption restoreLostChanges restoreLostChangesAutomatically restoreLostChangesAutomaticallyFrom: restoreLostChangesIfNecessary restoreLostChangesManually restoreLostChangesOptions restoringChangesHasErrorsCaption shouldShowFileInErrors withChangesFileDo:)
('image' wordSize)
('vm parameters' doMixedArithmetic doMixedArithmetic: vmParameterAt: vmParameterAt:put:)
('special selectors' hasSpecialSelector:ifTrueSetByte:)
!

