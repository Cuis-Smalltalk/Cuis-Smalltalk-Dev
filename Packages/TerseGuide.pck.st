'From Cuis 5.0 [latest update: #4219] on 12 June 2020 at 11:41:15 am'!
'Description Add ability to dynamically add topic pages from other sources.'!
!provides: 'TerseGuide' 1 141!
SystemOrganization addCategory: #TerseGuide!


!classDefinition: #TerseGuideHelp category: #TerseGuide!
Workspace subclass: #TerseGuideHelp
	instanceVariableNames: 'topics topicListIndex selectedTopic topicList window textPane'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TerseGuide'!
!classDefinition: 'TerseGuideHelp class' category: #TerseGuide!
TerseGuideHelp class
	instanceVariableNames: 'topics'!

!classDefinition: #TerseGuideWindow category: #TerseGuide!
WorkspaceWindow subclass: #TerseGuideWindow
	instanceVariableNames: 'textModelM list1'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TerseGuide'!
!classDefinition: 'TerseGuideWindow class' category: #TerseGuide!
TerseGuideWindow class
	instanceVariableNames: ''!

!classDefinition: #TerseGuideTopic category: #TerseGuide!
Object subclass: #TerseGuideTopic
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TerseGuide'!
!classDefinition: 'TerseGuideTopic class' category: #TerseGuide!
TerseGuideTopic class
	instanceVariableNames: ''!


!TerseGuideHelp commentStamp: 'dhn 3/7/2015 13:20' prior: 0!
TerseGuideHelp is adapted from the Squeak terse guide by Chris Rathman (http://www.angelfire.com/tx4/cus/notes/smalltalk.html), maintained on the swiki at http://wiki.squeak.org/squeak/5699. Copyrights and credit for the original document belong to Chris Rathman.

Examples of use include:

TerseGuideHelp display: #arithmetic
TerseGuideHelp display: #array
TerseGuideHelp display: #transcript

TerseGuideHelp also serves as the model for TerseGuideWindow, which can be opened by:

TerseGuideWindow openTerseGuide
!

!TerseGuideWindow commentStamp: 'dhn 3/5/2016 21:50' prior: 0!
Show a terse guide to Cuis in a system window. The window has a text pane with a list pane beside it. The text pane is a kind of Workspace which has the behavior that statements in it can be Done (Cmd-d) or Printed (Cmd-p) in order to observe their effect.

TerseGuideWindow openTerseGuide
!

!TerseGuideTopic commentStamp: '<historical>' prior: 0!
I represent a topic that can be added to the TerseGuide. Subclasses of me should implement #title and #examples. For instance:

>>title
    ^ 'Numbers'

>> examples
     ^ {
            { #code -> '4 factorial' . #category -> 'Common functions' . #description -> 'factorial' } as: Dictionary.
            { #code -> '2 even' . #category -> 'Common functions' . #description -> 'check if a number is even' } as: Dictionary.
        }!

!TerseGuideHelp methodsFor: 'topics' stamp: 'RNG 7/15/2018 01:31:53'!
addAdditionalTopicsTo: topicsList
	
	self class additionalTopics do: [ :each |
		topicsList at: each title put: each
	].! !

!TerseGuideHelp methodsFor: 'topics' stamp: 'jmv 5/6/2020 11:10:35'!
coreTopics
	
	^ `Dictionary new
	at: 'Arithmetic' put: #arithmetic;
	at: 'Arrays' put: #array;
	at: 'Assignment' put: #assignment;
	at: 'Associations' put: #association;
	at: 'Bags' put: #bag;
	at: 'Bit-Wise' put: #bitwise;
	at: 'Blocks' put: #block;
	at: 'Booleans' put: #boolean;
	at: 'Canvas' put: #canvas;
	at: 'Character' put: #character;
	at: 'Coding' put: #coding;
	at: 'Colors' put: #colors;
	at: 'Conditional Statements' put: #conditionalStatement;
	at: 'Conversion' put: #conversion;
	at: 'Date' put: #date;
	at: 'Debugging' put: #debugging;
	at: 'Dictionary' put: #dictionary;
	at: 'Dynamic Messages' put: #dynamic;
	at: 'Exceptions' put: #exceptions;
	at: 'Features' put: #feature;
	at: 'File Operations' put: #fileMan;
	at: 'Fonts' put: #font;
	at: 'Forms' put: #form;
	at: 'Heap' put: #heap;
	at: 'Internal Streams' put: #internalStream;
	at: 'Intervals' put: #interval;
	at: 'Introduction' put: #introduction;
	at: 'Iterations' put: #iterationStatement;
	at: 'Layout Morph' put: #layoutMorph;
	at: 'Layout Spec' put: #layoutSpec;
	at: 'Literals' put: #literals;
	at: 'Metaclass' put: #metaclass;
	at: 'Method Calls' put: #methodCall;
	at: 'Miscellaneous' put: #misc;
	at: 'Morphs' put: #morph;
	at: 'Object' put: #anObject;
	at: 'Ordered Collections' put: #orderedCollection;
	at: 'Point' put: #point;
	at: 'PopUpMenu' put: #popUpMenu;
	at: 'Primitives' put: #primitive;
	at: 'Rectangle' put: #rectangle;
	at: 'Sets' put: #set;
	at: 'Sorted Collections' put: #sortedCollection;
	at: 'Strings' put: #string;
	at: 'Symbols' put: #symbol;
	at: 'Syntax' put: #syntax;
	at: 'Time' put: #time;
	at: 'Transcript' put: #transcript;	
	at: 'Trie' put: #trie;
	at: 'Custom Browsers' put: #customBrowsers;
	yourself`! !

!TerseGuideHelp methodsFor: 'topics' stamp: 'RNG 7/15/2018 01:31:23'!
terseTopics
	"Translate a list item to a class method selector"
	|allTopics|
	allTopics _ self coreTopics.
	self addAdditionalTopicsTo: allTopics.
	^ allTopics! !

!TerseGuideHelp methodsFor: 'initialization' stamp: 'dhn 5/17/2016 16:50'!
initialize
	super initialize.
	topics _ self terseTopics.
	topicList _ self class pages.
! !

!TerseGuideHelp methodsFor: 'user interface support' stamp: 'dhn 6/15/2016 18:29'!
selectedTopic: aTopic
	"Set the value of selectedTopic and set the contents of the text pane"
	
	selectedTopic _ aTopic.
	textPane model actualContents: self updateTopicText.
	self triggerEvent: #topicListIndex! !

!TerseGuideHelp methodsFor: 'user interface support' stamp: 'dhn 2/26/2015 07:59'!
topicList
	^ topicList! !

!TerseGuideHelp methodsFor: 'user interface support' stamp: 'dhn 2/26/2015 08:42'!
topicListIndex
	topicList ifNil: [ ^ topicListIndex _ 0 ].
	^ topicList indexOf: selectedTopic! !

!TerseGuideHelp methodsFor: 'user interface support' stamp: 'dhn 10/18/2015 20:20'!
topicListIndex: index
	"Set the vakue of topicListIndex and set the value of selectedTopic"
	
	topicListIndex _ index.
	self selectedTopic: 
		(topicList at: index ifAbsent: nil)! !

!TerseGuideHelp methodsFor: 'user interface support' stamp: 'RNG 7/15/2018 01:45:10'!
updateTopicText
	"Answer the text of the selected topic and change the window label accordingly"
	| topic topicTitle |
	
	topicListIndex = 0 ifTrue: [ window setLabel: 'Terse Guide'. ^ ''].
	topic _ topics at: selectedTopic.
	topicTitle _ topic isSymbol ifTrue: [ self terseTopics keyAtValue: topic ] ifFalse: [ topic title ].
	window setLabel: ('Terse Guide to ', topicTitle).
	^ topic isSymbol ifTrue: [ self class perform: topic ] ifFalse: [ topic script ]! !

!TerseGuideHelp methodsFor: 'accessing' stamp: 'dhn 10/18/2015 20:01'!
textPane: anObject
	"Set the value of textPane"
	
	textPane _ anObject! !

!TerseGuideHelp methodsFor: 'accessing' stamp: 'dhn 2/27/2015 21:07'!
window: anObject
	"Set the value of window"

	window _ anObject! !

!TerseGuideHelp methodsFor: 'testing' stamp: 'jmv 5/29/2020 16:30:16'!
styleByParagraphs
	"Answer true if each paragraph should be styled independent of the others.
	We want to allow mutil-paragraph comments, and we are careful for our contents to be valid Smalltalk."

	^false! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 13:18:04'!
anObject
	"Object"
	^
'| x y |
x _ self activeHand.
	"Answer a hand for the morphic world that is the current UI focus.
	This is the UI root animated by the active Process.
	This method could answer nil, if not in a UI process!!"

y _ self as: HandMorph.
	"Create an object of class aSimilarClass that has similar contents to the receiver."

y _ x asString.
	"Answer a string that represents the receiver."

x _ #foo.
y become: x. 
	"Swap the object pointers of the receiver and the argument.
	Fails if either object is a SmallInteger"

self break.
	"This is a simple message to use for inserting breakpoints during debugging.
	The debugger is opened by sending a signal. This gives a chance to restore
	invariants related to multiple processes."

y changed.
	"Receiver changed in a general way; inform all the dependents by 
	sending each dependent an update: message."

x class.
	"Answer the object which is the receiver''s class."

x className.
	"Answer a string characterizing the receiver''s class, for use in list views for example"

nil confirm: ''Are you hungry?''.	
	"Put up a yes/no menu with a caption. Answer true if the response 
	is yes, false if no. This is a modal question--the user must respond 
	yes or no."

x _ nil confirm: ''Are you hungry?'' orCancel: [x _ ''perhaps''].
	"Put up a yes/no/cancel menu with caption aString. Answer true if  
	the response is yes, false if no. If cancel is chosen, evaluate  
	cancelBlock. This is a modal question--the user must respond yes, no, or cancel."

y disableCode: [nil confirm: ''Is this necessary?''.].
	"Use this method instead of commenting temporarily disabled code.
	This way, it will be accessible with senders, references to variables, etc.
	Besides, you''ll avoid problems when the code to disable has comments!!"

y disableCode: [self executeMethod: #fullPrintString. "Execute compiledMethod against the receiver with no args"].

x fullPrintString.
	"Answer a String whose characters are a description of the receiver."

self halt.
	"This is the typical message to use for inserting breakpoints during 
	debugging. It behaves like halt:, but does not call on halt: in order to 
	avoid putting this message on the stack. Halt is especially useful when 
	the breakpoint message is an arbitrary one."

self halt: ''something to aid debugging''.
	"This is the typical message to use for inserting breakpoints during 
	debugging. It creates and schedules a Notifier with a label."

self inboundPointers.  "Answers a collection of all objects in the system that point to myself"

self inboundPointersExcluding: #().  "Answer a list of all objects in the system that point to me, excluding those in the collection of objectsToExclude. I do my best to avoid creating any temporary objects that point to myself"

nil inform: ''Nothing exceeds like excess.''.  "Display a message for the user to read and then dismiss."

x inspectWithLabel: ''Interesting Instance''.  "Create and schedule an Inspector labelled aLabel."

x isKindOf: Set. 	"Answer whether the argument is a superclass or class of the receiver."

y isMemberOf: Symbol. 
	"Answer whether the receiver is an instance of the argument class"

y longPrintString.
	"Answer a String whose characters are a description of the receiver."

nil notify: ''this is a notice''. 
	"Create and schedule a Notifier with the argument as the message in 
	order to request confirmation before a process can proceed."

x _ y outboundPointers.
"Answers a list of all objects I am causing not to be garbage-collected"

x perform: #asBag. 
	"Send the unary selector, a symbol, to the receiver.
	Fail if the number of arguments expected by the selector is not zero."

x perform: #at: with: 2. 
	"Send the selector, a symbol, to the receiver with an argument.
	Fail if the number of arguments expected by the selector is not one."

x perform: #at:put: with: 2 with: ''bar''. 
	"Send the selector, a symbol, to the receiver with the given arguments.
	Fail if the number of arguments expected by the selector is not two."

x perform: #at:put: withArguments: {2. 17.6}. 
	"Send the selector, a symbol, to the receiver with arguments in an array.
	Fail if the number of arguments expected by the selector does not match 
	the size of the array."

y printOn: String new writeStream.
	"Append to the argument, a stream, a sequence of characters that  
	identifies the receiver."

nil shouldBeImplemented.
	"Announce that this message should be implemented"

nil shouldNotImplement.
	"Announce that, although the receiver inherits this message, it should 
	not implement it."

y storeOn: String new writeStream. 
	"Append to the argument a sequence of characters that is an 
	expression whose evaluation creates an object similar to the receiver."

y storeString.
	"Answer a String representation of the receiver from which the receiver 
	can be reconstructed."

self subclassResponsibility.
	"This message sets up a framework for the behavior of the class'' subclasses.
	 Announce that the subclass should have implemented this message."

self triggerEvent: #mouseOver.
	"Evaluate all actions registered for the argument, an EventSelector. Return the 
	value of the last registered action."

x yourself.

y ~= #foo. 
	"Answer whether the receiver and the argument do not represent the 
	same object."

y ~~ #foo. 
	"Answer whether the receiver and the argument are not the same object 
	(do not have the same object pointer)."

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 13:43:44'!
arithmetic
	"Arithmetic Expressions"
	^
'| x y |
x _ 6 + 3.								"addition"
x _ 6 - 3.								"subtraction"
x _ 6 * 3.								"multiplication"
x _ 1 + 2 * 3.							"evaluation always left to right (1 + 2) * 3"
x _ 5 / 3.								"division with fractional result"
x _ 5 / 3.0.							"division with float result"
x _ 5.0 // 3.0.						"integer divide"
x _ 5.0 \\ 3.0.						"integer remainder"
x _ -5.									"unary minus"
x _ 5 sign.								"numeric sign (1, -1 or 0)"
x _ 5 negated.							"negate receiver"
x _ 1.2 integerPart.					"integer part of number (1.0)"
x _ 1.2 fractionPart.				"fractional part of number (0.2)"
x _ 5 reciprocal.						"reciprocal function"
x _ 6 * 3.1.							"auto convert to float"
x _ 5 squared.							"square function"
x _ 25 sqrt.							"square root"
x _ 5 raisedTo: 2.					"power function"
x _ 5 raisedToInteger: 2.			"power function with integer"
x _ 5 exp.								"exponential"
x _ -5 abs.								"absolute value"
x _ 3.99 rounded.						"round"
x _ 3.99 truncated.					"truncate"
x _ 3.99 roundTo: 1.					"round to specified decimal places"
x _ 3.99 truncateTo: 1.				"truncate to specified decimal places"
x _ 3.99 floor.						"integer nearest the receiver toward negative infinity"
x _ 3.99 ceiling.						"integer nearest the receiver toward  infinity"
x _ 5 factorial.						"factorial"
x _ -5 quo: 3.							"integer divide rounded toward zero"
x _ -5 rem: 3.							"integer remainder rounded toward zero"
x _ 28 gcd: 12.						"greatest common divisor"
x _ 28 lcm: 12.						"least common multiple"
x _ 28 mod: 12.						"modulo"
x _ 100 ln.								"natural logarithm"
x _ 100 log.							"base 10 logarithm"
x _ 100 log: 10	.					"logarithm with specified base"
x _ 100 floorLog: 10.				"floor of the log"
x _ 180 degreesToRadians.			"convert degrees to radians"
x _ 3.14 radiansToDegrees.			"convert radians to degrees"
x _ 0.7 sin.							"sine"
x _ 0.7 cos.							"cosine"
x _ 0.7 tan.							"tangent"
x _ 0.7 arcSin.						"arcsine"
x _ 0.7 arcCos.						"arccosine"
x _ 0.7 arcTan.						"arctangent"
x _ 10 max: 20.						"get maximum of two numbers"
x _ 10 min: 20.						"get minimum of two numbers"
x _ Float pi.							"pi"
x _ Float e.							"exp constant"
x _ Float infinity.					"infinity"
x _ Float nan.							"not-a-number"
x _ Random new next; yourself.	"establish a local pseudo-random number generator"
x next.									"random number on the interval (0.0,1.0)"
y _ 100 atRandom.						"random integer on the interval (1,receiver)"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 14:04:52'!
array
	"Arrays"
	^
'"
	Array:              Fixed length collection
	ByteArray:     Array limited to byte elements (0-255)
	WordArray:    Array limited to word elements (0-2^32)
"
| b x y sum max |	"For efficient viewing, have a Transcript open and use Cmd-d on these statements"
x _ #(4 3 2 1).								"constant array"
x _ Array with: 5 with: 4 with: 3 with: 2.         "create array with 4 elements"
x _ Array new: 4.								"allocate an array with specified size"
x													"set array elements"
   at: 1 put: 5;
   at: 2 put: 4;
   at: 3 put: 3;
   at: 4 put: 7;
   yourself.									"without this, x will be 7 (the last element)"

b _ x isEmpty.									"test if array is empty"
y _ x size.										"array size"
y _ x at: 4.									"get array element at index"
b _ x includes: 3.							"test if element is in array"
y _ x copyFrom: 2 to: 4.					"subarray"
y _ x indexOf: 3 ifAbsent: [0].			"first position of element within array"
y _ x occurrencesOf: 3.						"number of times object in collection"
x do: [:a | Transcript show: a printString; newLine].  "iterate over the array"
b _ x allSatisfy: [:a | (a >= 1) & (a <= 4)].	"test if all elements meet condition"
y _ x select: [:a | a > 2].				"return collection of elements that pass test"
y _ x reject: [:a | a < 2].				"return collection of elements that fail test"
y _ x collect: [:a | a + a].				"transform each element for new collection"
y _ x detect: [:a | a > 3] ifNone: [].	"return first element that passes test"
y _ x findFirst: [:a | a < 3].			"find position of first element that passes test"
sum _ 0. x do: [:a | sum _ sum + a]. sum.	"sum array elements"
sum _ 0. 1 to: (x size) do: [:a | sum _ sum + (x at: a)].  "sum array elements"
sum _ x inject: 0 into: [:a :c | a + c].	"sum array elements"
max _ x inject: 0 into: [:a :c | (a > c)	"find max element in array"
   ifTrue: [a]
   ifFalse: [c]].
y _ x shuffled.								"randomly shuffle collection"
y _ x asArray.									"convert to array"
y _ x asByteArray.							"convert to byte array"
y _ x asWordArray.							"convert to word array"
y _ x asOrderedCollection.					"convert to ordered collection"
y _ x asSortedCollection.					"convert to sorted collection"
y _ x asBag.									"convert to bag collection"
y _ x asSet.									"convert to set collection"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 5/11/2020 17:09:56'!
assignment
	"Assignment"
	^
'| x y z |
"either the Cuis or the ASCII convention can be used for assignment"
x _ 4.								"assignment (Cuis) using the underline character ($_)"
z := 7.								"assignment (ASCII)"
x _ y _ z _ 6.						"compound assignment"
x _ (y _ 6) + 1.
x _ Object new.					"bind to an allocated instance of a class"
x _ 123 class.						"discover the class of the object"
x _ Integer superclass.			"discover the superclass of a class"
x _ Object allInstances.		"get an array of all instances of a class"
x _ Integer allSuperclasses.	"get all superclasses of a class"
x _ 1.2 hash.						"hash value for object"
y _ x copy.							"copy the object"
y _ x shallowCopy.				"copy the object (not overridden)"
y _ x veryDeepCopy.				"complete tree copy using a dictionary"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 6/7/2015 12:37'!
association
	"Associations"
	^
'| x y |
x _ #myVar->''hello''.		"Create an association"
y _ x key.
y _ x value.

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 14:11:33'!
bag
	"Bags"
	^
'| b x y sum max |
x _ Bag with: 4 with: 3 with: 2 with: 1.	"create a Bag collection with 4 elements"
x _ Bag new.										"allocate collection"
x add: 4; add: 3; add: 1; add: 2; yourself.	"add element to collection; see NOTE"
x add: 3 withOccurrences: 2.					"add multiple copies to collection"
y _ x addAll: #(7 8 9).							"add elements to the collection and answer the added elements; see NOTE"
y _ x removeAll: #(7 8 9).						"remove elements from the collection and answer the removed elements; see NOTE"
y _ x remove: 4 ifAbsent: [].					"remove element from collection"
b _ x isEmpty.										"test if empty"
y _ x size.											"number of elements"
b _ x includes: 3.								"test if element is in collection"
y _ x occurrencesOf: 3.							"number of times object in collection"
x do: [:a | Transcript show: a printString; newLine].	"iterate over the collection"
b _ x allSatisfy: [:a | (a >= 1) & (a <= 4)].	"test if all elements meet condition"
y _ x select: [:a | a > 2].					"return collection of elements that pass test"
y _ x reject: [:a | a < 2].					"return collection of elements that fail test"
y _ x collect: [:a | a + a].					"transform each element for new collection"
y _ x detect: [:a | a > 3] ifNone: [].		"return first element that passes test"
sum _ 0. x do: [:a | sum _ sum + a]. sum.	"sum elements"
sum _ x inject: 0 into: [:a :c | a + c].	"sum elements"
max _ x inject: 0 into: [:a :c | (a > c)	"find max element in collection"
   ifTrue: [a]
   ifFalse: [c]].
y _ x asOrderedCollection.						"convert to ordered collection"
y _ x asSortedCollection.						"convert to sorted collection"
y _ x asBag.										"convert to bag collection"
y _ x asSet.										"convert to set collection"

"NOTE: methods in subclasses of Collection, of which Bag is one, answer
 the ARGUMENT instead of the resulting collection. Examples of such 
 methods are #addAll: and #removeAll:. Sending #yourself to the collection
 will answer the collection instead of the argument."

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 9/5/2015 20:34'!
bitwise
	"Bitwise Manipulation"
	^
'| b x |
x _ 16rFF bitAnd: 16r0F.			"and bits"
x _ 16rF0 bitOr: 16r0F.				"or bits"
x _ 16rFF bitXor: 16r0F.			"xor bits"
x _ 16rFF bitInvert.					"invert bits"
x _ 16r0F bitShift: 4.				"left shift"
x _ 16rF0 bitShift: -4.				"right shift"
x _ 16r80 bitAt: 8.					"bit at position (0|1)"
x _ 16r80 highBit.					"position of highest bit set"
b _ 16rFF allMask: 16r0F.			"test if all bits set in mask are set in receiver"
b _ 16rFF anyMask: 16r0F.			"test if any bits set in mask are set in receiver"
b _ 16rFF noMask: 16r0F.			"test if all bits set in mask are clear in receiver"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 14:17:04'!
block
	"Blocks"
	^
'"
	Blocks:
		- blocks are objects and may be assigned to a variable
		- value is last expression evaluated unless explicit return
		- blocks may be nested
		- specification [ arguments | | localvars | expressions ]	
		- ^expression terminates block & method (exits all nested blocks)
		- blocks intended for long term storage should not contain ^
"
| x y z fac |
x _ [ y _ 1. z _ 2. ]. x value.					"simple block usage"
x _ [ :argOne :argTwo |   argOne, '' and '' , argTwo.].     "set up block with argument passing"
Transcript show: (x value: ''First'' value: ''Second''); newLine.  "use block with argument passing"
x _ [:e | | v | v _ 1. e + v] value: 2.		"localvar in a block"
fac _ [ :n | n > 1 ifTrue:  [n * (fac value: n-1)] ifFalse: [1]].  "closure on block variable"
fac value: 5.											"closure variable scoped to its block"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 14:19:28'!
boolean
	"Booleans"
	^
'| b x y |
x _ 1. y _ 2.
b _ (x = y).							"equals"
b _ (x ~= y).							"not equals"
b _ (x == y).							"identical"
b _ (x ~~ y).							"not identical"
b _ (x > y).							"greater than"
b _ (x < y).							"less than"
b _ (x >= y).							"greater than or equal"
b _ (x <= y).							"less than or equal"
b _ b not.								"boolean not"
b _ (x < 5) & (y > 1).				"boolean and"
b _ (x < 5) | (y > 1).				"boolean or"
b _ (x < 5) and: [y > 1].			"boolean and (short-circuit)"
b _ (x < 5) or: [y > 1].			"boolean or (short-circuit)"
b _ (x < 5) eqv: (y > 1).			"test if both true or both false"
b _ (x < 5) xor: (y > 1).			"test if one true and other false"
b _ 5 between: 3 and: 12.			"between (inclusive)"
b _ 123 isKindOf: Number.			"test if object is class or subclass of"
b _ 123 isMemberOf: SmallInteger.	"test if object is type of class"
b _ 123 respondsTo: #sqrt.			"test if object responds to message"
b _ x isNil.							"test if object is nil"
b _ x isZero.							"test if number is zero"
b _ x positive.						"test if number is positive"
b _ x strictlyPositive.				"test if number is greater than zero"
b _ x negative.						"test if number is negative"
b _ x even.								"test if number is even"
b _ x odd.								"test if number is odd"
b _ x isLiteral.						"test if literal constant"
b _ x isInteger.						"test if object is integer"
b _ x isFloat.							"test if object is float"
b _ x isNumber.						"test if object is number"
b _ x isSymbol.						"test if object is a Symbol"
b _ $A isUppercase.					"test if upper case character"
b _ $A isLowercase.					"test if lower case character"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/19/2016 20:44'!
canvas
	"Canvas"
	^
'| x y |

"
	There is no Canvas class.
	
	A canvas is produced by sending #getCanvas to a Form.
"

x _ Form extent: 200@100 depth: 32.	"Answer a form with color depth 32"

y _ x getCanvas.							"convert form to canvas (a BitBltCanvas)"
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 14:20:26'!
character
	"Character"
	^
'| x y b |
x _ $A.										"character assignment"
y _ x isLowercase.						"test if lower case"
y _ x isUppercase.						"test if upper case"
y _ x isLetter.							"test if letter"
y _ x isDigit.								"test if digit"
y _ x isAlphaNumeric.					"test if alphanumeric"
y _ x isSeparator.						"test if seperator char"
y _ x isVowel.								"test if vowel"
y _ x digitValue.							"convert to numeric digit value"
y _ x asLowercase.						"convert to lower case"
y _ x asUppercase.						"convert to upper case"
y _ x asciiValue.							"convert to numeric ascii value"
y _ x asString.							"convert to string"
b _ $A <= $B.								"comparison"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 3/9/2019 11:52:37'!
coding
	"Code Patterns"
	^
'"
These are Cuis code patterns intended to help the programmer exploit some of the features of Cuis classes. The examples separate the model from the view and feature two styles: coupled and decoupled.

The coupled style employs the ''dependency mechanism'' and requires the model to change when the view changes and when views which use the model are added. The primary methods of the dependency mechanism are #changed: and #update: .

The decoupled style employs the ''observer pattern'' which ensures that the model can remain unaffected by changes to the view or by additional views. The primary methods of the observer pattern are #triggerEvent: and #when:send:to: . This is the preferred style for Cuis, although both styles can be found in the base.
"
| b n |

"To examine the patterns, in the parent directory which contains Cuis-Smalltalk-Dev* do the following:
	''git clone https://github.com/Cuis-Smalltalk/CodeExamples''
	
then, do the following:	
"
Feature require: #''Code-Patterns''.				"load the package"

"To browse the examples, do (Cmd-d) these statements..."

b _ BrowserWindow openBrowser.					"open a browser"
n _ b model systemCategoryList findFirst: [:a | a = #''Code-Patterns''].  "index to the above package"
b model systemCategoryListIndex: n.				"go to the system category"

"The following statements execute the examples..."

DependencyExamples relatedViews. 				"a decoupled example"
DependencyExamples unrelatedViews. 				"another decoupled example"
DependencyExamples coupledView. 					"a coupled example"
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 14:38:36'!
colors
	"Colors"
	^
'
| x t arr |			"For efficient viewing, have a Transcript open and use Cmd-d on these statements"

x _ (Color r: 0 g: 0 b: 0) name.		"answer the name of the color, if possible"
x _ (Color r: 1.0 g: 1.0 b: 1.0) name.

arr _ #((0.0 0.0 0.0) (0.0 0.0 1.0) (0.0 1.0 0.0) (0.0 1.0 1.0) (1.0 0.0 0.0) (1.0 0.0 1.0) (1.0 1.0 0.0) (1.0 1.0 1.0)).
(arr collect: [:a | Color colorFrom: a]) explore.

"In the parent directory which contains Cuis-Smalltalk-Dev* do the following:
	''git clone https://github.com/KenDickey/Cuis-Smalltalk-ColorEditor''
	''git clone https://github.com/KenDickey/Cuis-Smalltalk-NamedColors''
	''git clone https://github.com/KenDickey/Cuis-Smalltalk-Morphic-Misc1''

then, do the following:
"
Feature require: #''Morphic-ColorEditor''.	"load the ColorEditor feature and its dependencies"

"From the menu ''World>New morph...>ColorEditor'' select ColorEditorPanel or ColorPallet"
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 16:27:14'!
conditionalStatement
	"Conditional Statements"
	^
'| x switch result |
x _ 11.
x > 10 ifTrue: [Transcript show: ''ifTrue''; newLine].		"if then"
x > 10 ifFalse: [Transcript show: ''ifFalse''; newLine].	"if else"
x > 10										"if then else"
   ifTrue: [Transcript show: ''ifTrue''; newLine]
   ifFalse: [Transcript show: ''ifFalse''; newLine].
x > 10										"if else then"
   ifFalse: [Transcript show: ''ifFalse''; newLine]
   ifTrue: [Transcript show: ''ifTrue''; newLine].
Transcript
   show:
      (x > 10
         ifTrue: [''ifTrue'']
         ifFalse: [''ifFalse'']);
   newLine.
Transcript																"nested if then else"
   show:
      (x > 10
         ifTrue: [x > 5
            ifTrue: [''A'']
            ifFalse: [''B'']]
         ifFalse: [''C'']);
   newLine.
switch _ Dictionary new.											"switch (case) functionality"
switch at: $A put: [Transcript show: ''Case A''; newLine].
switch at: $B put: [Transcript show: ''Case B''; newLine].
switch at: $C put: [Transcript show: ''Case C''; newLine].
result _ (switch at: $B) value.

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 16:33:43'!
conversion
	"Conversion"
	^
'| x |
x _ 3.99 asInteger.			"convert number to integer (truncates in Cuis)"
x _ 3.99 asFraction.			"convert number to fraction"
x _ 3 asFloat.					"convert number to float"
x _ 65 asCharacter.			"convert integer to character"
x _ $A asciiValue.			"convert character to integer"
x _ 3.99 printString.		"convert object to string via printOn:"
x _ 3.99 storeString.		"convert object to string via storeOn:"
x _ 15 printStringBase: 16.
x _ 15 storeStringBase: 16.

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'RNG 12/13/2019 13:07:13'!
customBrowsers
	"Custom Browsers"
	^
'"Open a window with all methods from a particular category"
Smalltalk 
	browseMessageList: (Smalltalk allMethodsInCategory: ''testing'')
	name: ''Testing methods''
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 16:38:28'!
date
	"Date"
	^
'| w x y b block str fmt |		"For efficient viewing, have a Transcript open and use Cmd-d on these statements"
x _ Date today.								"create date for today"
w _ Date dateAndTimeNow.					"create array with current date and time"
x _ Date newDay: 12 month: #July year: 1999.	"create date from parts"
x _ Date fromDays: 36002.					"create date from elapsed days since 1/1/1901"
y _ Date dayOfWeek: #Monday.				"day of week as int (1-7)"
y _ Date indexOfMonth: #August.			"month of year as int (1-12)"
y _ Date daysInMonth: 2 forYear: 1996.	"day of month as int (1-31)"
y _ Date daysInYear: 1996.					"days in year (365|366)"
y _ Date nameOfDay: 1.						"weekday name (#Monday,...)"
y _ Date nameOfMonth: 3.					"month name (#January,...)"
y _ Date leapYear: 1997.					"1 if leap year; 0 if not leap year"
y _ x weekday.									"day of week (#Monday,...)"
y _ x previous: #Monday.					"date for previous day of week (one week ago)"
y _ x dayOfMonth.								"day of month (1-31)"
y _ x firstDayOfMonth.						"day of year for first day of month"
y _ x monthName.								"month of year (#January,...)"
y _ x monthIndex.								"month of year (1-12)"
y _ x daysInMonth.							"days in month (1-31)"
y _ x year.										"year (19xx)"
y _ x daysInYear.								"days in year (365|366)"
y _ x daysLeftInYear.						"days left in year (364|365)"
y _ x printFormat: #(2 1 3 $/ 1 1).		"print formatted date"
b _ (x < Date today).						"comparison"
y _ x julianDayNumber.
y _ x mmddyyyy.	"Answer the receiver rendered in standard fmt mm/dd/yyyy. Note that the name #mmddyyyy is slightly misleading -- the month and day numbers don''t show leading zeros, so that for example feb 1 1996 is 2/1/96"
y _ x month.
y _ x monthAbbreviation.
y _ x monthIndex.
y _ x monthName.
fmt _ #(1 2 3 $  3 1 ).		"Specify a format for the date, in the following scheme:"
									"#(item item item sep monthfmt yearfmt twoDigits) 
									items: 1=day 2=month 3=year will appear in the order given, 
									separated by sep which is either an ascii code or character. 
									monthFmt: 1=09 2=Sep 3=September 
									yearFmt: 1=1996 2=96 
									digits: (missing or)1=9 2=09. 
									See the examples in printOn: and mmddyy"
y _ x printFormat: fmt.	 	"Answer a String describing the receiver using the format denoted by the argument, fmt"
str _ ReadWriteStream on: String new.	"Specify a stream"
y _ x printOn: str.

str _ String new writeStream.	
y _ x printOn: str format: #(3 2 1 $.  1 2 ).  "the stream, not the receiver, contains the date in the specified format"
str contents.

y _ x secondsSinceSqueakEpoch.	"Answer the seconds since the Squeak epoch: 1 January 1901"
y _ x storeOn: str.
y _ x week.
y _ x weekday. 			"Answer the name of the day of the week on which the receiver falls."
y _ x weekdayIndex. 		"Sunday=1, ... , Saturday=7"
y _ x year.
y _ x yearNumber.
y _ x yyyymmdd. 			"Format the date in ISO 8601 standard like ''2002-10-22''."
block _ [ :d :m :y | {d. m. y} ].  "Specify an order for the date"
y _ x dayMonthYearDo: block. "Supply integers for day, month and year to aBlock and return the result"
y _ x dayMonthYearDo: [ :d :m :y | {y. m. d.} ].  "Order determined by block, not method name"
y _ x dayOfMonth. 		"Answer the day of the month represented by the receiver."
y _ x dayOfWeek. 			"Answer the day of the week represented by the receiver."
y _ x dayOfWeekName. 	"Answer the day of the week represented by the receiver."
y _ x dayOfYear.			"Answer the day of the year represented by the receiver."
y _ x daysInMonth.		"Answer the number of days in the month represented by the receiver."
y _ x daysInYear.			"Answer the number of days in the year represented by the receiver."
y _ x daysLeftInYear.
y _ x firstDayOfMonth.
y _ x isLeapYear.

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 4/12/2019 14:50:40'!
debugging
	"Debugging"
	^
'| a b x |
	"Some statements in this topic are designed to cause an error."
x _ Object new.
x yourself.									"returns receiver"
x inspect.									"open object inspector window"
x confirm: ''Is this correct?''.
x halt.										"breakpoint to open debugger window"
x halt: ''Halt message''.
x notify: ''Notify text''.
x error: ''Error string''.				"open up error window with title"
Smalltalk beep.
x shouldNotImplement.					"flag message should not be implemented"
x subclassResponsibility.				"flag message as abstract"
x errorImproperStore.					"flag an improper store into indexable object"
x errorNonIntegerIndex.					"flag only integers should be used as index"
x errorSubscriptBounds: 13.			"flag subscript out of bounds"
x primitiveFailed.						"system primitive failed"

a _ ''A1''. b _ ''B2''. a become: b.		"switch two objects, if neither is a SmallInteger"
Transcript show: a, b; newLine.

"Doing ''self halt.'' in Morphic code can be risky, because it can open an infinite sequence of debuggers."
{2. x. a.} print. 						"alternative to ''self halt.''"
thisContext printStack: 10.			"alternative to ''self halt.'' showing n lines of the call stack"

x doesNotUnderstand: (Message selector: #foo).  "flag that the message is not handled"

"Writing to-do notes to yourself can be done as follows:"
self flag: #someSymbol  "a reminder comment"
"...where you choose #someSymbol to be something you will not forget (your initials?)."
"Then, to retrieve all such messages, browse senders of #someSymbol using Cmd-n or the SmalltalkEditor menu."
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 16:52:18'!
dictionary
	"Dictionary"
	^
'"	
	Dictionary:
	IdentityDictionary:   uses identity test (== rather than =)
"
| b x y sum max aDx |		"For efficient viewing, have a Transcript open and use Cmd-d on these statements"
x _ Dictionary new.							"allocate collection"
x add: #a->4; add: #b->3; add: #c->1; add: #d->2; yourself.	"add element to collection"
x at: #e put: 3.								"set element at index; see NOTE"
b _ x isEmpty.									"test if empty"
y _ x size.										"number of elements"
y _ x at: #a ifAbsent: [].					"retrieve value for key #a"
y _ x keyAtValue: 3 ifAbsent: [].		"retrieve key for given value with error block"
y _ x removeKey: #e ifAbsent: [].		"remove element from collection"
b _ x includes: 3.							"test if element is in values collection"
b _ x includesKey: #a.						"test if element is in keys collection"
y _ x occurrencesOf: 3.						"number of times object in collection"
y _ x keys.										"set of keys"
y _ x values.									"bag of values"
x do: [:a | Transcript show: a printString; newLine].  "iterate over the values collection"
x keysDo: [:a | Transcript show: a printString; newLine].  "iterate over the keys collection"
x associationsDo: [:a | Transcript show: a printString; newLine].  "iterate over the associations"
x keysAndValuesDo: [:aKey :aValue | Transcript	"iterate over keys and values"
   show: aKey printString; space;
   show: aValue printString; newLine].
b _ x allSatisfy: [:a | (a >= 1) & (a <= 4)].  "test if all elements meet condition"
y _ x select: [:a | a > 2].				"return a dictionary with values that pass test"
y _ x reject: [:a | a < 2].				"return a dictionary with values that fail test"
y _ x collect: [:a | a + a].				"transform each element for new dictionary"
y _ x detect: [:a | a > 3] ifNone: [].	"return first value that passes test"
sum _ 0. x do: [:a | sum _ sum + a]. sum.	"sum the values"
sum _ x inject: 0 into: [:a :c | a + c].	"sum the values"
max _ x inject: 0 into: [:a :c | (a > c)	"find max value in collection"
   ifTrue: [a]
   ifFalse: [c]].
y _ x asArray.									"collect values in an array"
y _ x asOrderedCollection.					"collect values in an ordered collection"
y _ x asSortedCollection.					"collect values in a sorted collection"
y _ x asBag.									"collect values in a bag"
y _ x asSet.									"collect values in a set"

Smalltalk at: #CuisGlobal put: ''Cuis entry''.	"put global in Smalltalk Dictionary"
x _ Smalltalk at: #CuisGlobal.					"read global from Smalltalk Dictionary"
Transcript show: (CuisGlobal printString).	"entries are directly accessible by name"
Smalltalk keys do: [ :k |						"print out all non-class globals"
   ((Smalltalk at: k) isKindOf: Class)
      ifFalse: [Transcript show: k printString; newLine]].
aDx _ Dictionary new.						"set up user defined dictionary"
aDx at: #MyVar1 put: ''hello1''.			"put entry in dictionary; see NOTE"
aDx add: #MyVar2->''hello2''.				"add entry to dictionary use key->value combo; see NOTE"
aDx size.		"dictionary size"
aDx keys do: [ :k |							"print out keys in dictionary"
   Transcript show: k printString; newLine].
aDx values do: [ :k |						"print out values in dictionary"
   Transcript show: k printString; newLine].
aDx keysAndValuesDo: [:aKey :aValue |	"print out keys and values"
   Transcript
      show: aKey printString;
      space;
      show: aValue printString;
      newLine].
aDx associationsDo: [:aKeyValue |		"another iterator for printing key values"
   Transcript show: aKeyValue printString; newLine].
Smalltalk removeKey: #CuisGlobal ifAbsent: [].  "remove entry from Smalltalk dictionary"

"NOTE: methods in subclasses of Collection, of which Dictionary is one, answer
 the ARGUMENT instead of the resulting collection. Examples of such 
 methods are #add: and #at:put:. Sending #yourself to the collection
 will answer the collection instead of the argument."

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 14:38'!
dynamic
	"Dynamic Message Calling/Compiling"
	^
'| receiver message result argument keyword1 keyword2 argument1 argument2 |

"unary message"
receiver _ 5.
message _ ''factorial'' asSymbol.
result _ receiver perform: message.
result _ Compiler evaluate: ((receiver storeString), '' '', message).
result _ (Message new setSelector: message arguments: #()) sentTo: receiver.

"binary message"
receiver _ 1.
message _ ''+'' asSymbol.
argument _ 2.
result _ receiver perform: message withArguments: (Array with: argument).
result _ Compiler evaluate: ((receiver storeString), '' '', message, '' '', (argument storeString)).
result _ (Message new setSelector: message arguments: (Array with: argument)) sentTo: receiver.

"keyword messages"
receiver _ 12.
keyword1 _ ''between:'' asSymbol.
keyword2 _ ''and:'' asSymbol.
argument1 _ 10.
argument2 _ 20.
result _ receiver
   perform: (keyword1, keyword2) asSymbol
   withArguments: (Array with: argument1 with: argument2).
result _ Compiler evaluate:
   ((receiver storeString), '' '', keyword1, (argument1 storeString) , '' '', keyword2, (argument2 storeString)).
result _ (Message
   new
      setSelector: (keyword1, keyword2) asSymbol
      arguments: (Array with: argument1 with: argument2))
   sentTo: receiver.

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 12/7/2015 15:50'!
exceptions
	"Exceptions"
	^
'"
To see the variety of exceptions that can be useful in Cuis, open a hierarchy browser on Error.
"
| x y |

x _  1. y _ 0.
[x / y] on: ZeroDivide do: [^ ''that is big''].

[''testFile.txt'' asFileEntry fileContents] on: MessageNotUnderstood do: [^ nil].	"avoid the exception for Cancel during the dialog"	

[''testFile.txt'' asFileEntry fileContents] on: FileDoesNotExistException do: [nil confirm: ''Rather do it yourself?''].	"avoid the file-does-not-exist dialog"	

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/23/2016 12:15'!
feature
	"Features"
	^
'"Part of the Cuis strategy to keep a clean, small kernel is the separation of system features into smaller, more easily understood Packages.

The basic idea is that a Feature is implemented by the Package(s) it contains.

You can load Features in your code or by..."

Feature require: #''Feature-Name''.

"You can also use the File List window to install a feature which can be in any directory (World Menu -> Open-> File List). After selecting a feature, click on ''installPackage''.

A feature may depend on other features and particular versions of those features.  Cuis checks for dependencies and if all dependencies are satisfied will load packages in the proper order.

You can create your own packages and add/remove feature requirements using the Package Browser.  World Menu -> Open -> Installed Packages


There are many features for Cuis available at GitHub.

In a web browser, 
1. view https://github.com
2. type ''Cuis-Smalltalk'' in the search box
3. see the number of features available.

When you invoke:"

Feature require: #''Feature-Name''.

"Cuis will search for packages (*.pck.st) which supply the feature in...

1. the image directory and its subdirectories ''Packages'' and ''CompatibilityPackages'' 
2. siblings of the image directory whose names start with ''Cuis-Smalltalk*'' and their subdirectories named ''Packages'' and ''CompatibilityPackages''.
"

Smalltalk imagePath.		"Cuis image directory"


"Additional information is available on the web page:

''https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/Documentation/CuisAndGitHub.md''


For an example of specifying feature requirements, consider the package file ''Cuis-Smalltalk-Dev/Packages/Graphics-Files-Additional.pck.st'' which can be viewed with the File List browser. It provides feature ''Graphics-Files-Additional'' and requires Feature ''Compression''. If in code or in a Workspace you invoke (Cmd-d):"

Feature require: #''Graphics-Files-Additional''.

"...then the Compression package will be loaded, followed by Graphics-Files-Additional."


Feature require: #''Core-Packages''. 		"load all core Cuis Packages"
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 16:59:23'!
fileMan
	"File Operations"
	^
'| b c file dirEntry fullPath |	"For efficient viewing, have a Transcript open and use Cmd-d on these statements"

DirectoryEntry roots.						"answer the drive or drives"
c _ DirectoryEntry currentDirectory.	"answer the current directory"
''testFile.txt'' asFileEntry fileContents: ''Test me now''.  "answer the directory and name of a new file; set its contents"
''testFile.txt'' asFileEntry fileContents.	"print contents of a file"

''testFile.txt'' asFileEntry
	forceWriteStreamDo: [ :ios |			"overwrite old file contents"
		ios nextPut: $M.
		ios nextPutAll: ''y text ''.
		ios nextPutAll: ''data''.
		].
''testFile.txt'' asFileEntry fileContents.	"print contents of a file"

''testFile.txt'' asFileEntry appendStreamDo: [ :fileStream |
	fileStream newLine.
	fileStream nextPutAll: ''Second text line''; newLine.
	].
''testFile.txt'' asFileEntry fileContents. 	"print it"

''testFile.txt'' asFileEntry appendContents: ''The quick red fox''.

''testFile.txt'' asFileEntry fileContents. 	"print it"

''testFile.txt'' asFileEntry forceWriteStreamDo: [ :fileStream |  "overwrite the file"
	| refStyream sharedPoint input |
	"Construct the test data."
	refStyream _ ReferenceStream on: fileStream.
	input _ Array new: 9.
	input at: 1 put: nil.
	input at: 2 put: true.
	input at: 3 put: false.
	input at: 4 put: #(-4 -4.0 ''four'' four).
	input at: 5 put: (Form extent: 63 @ 50 depth: 8).
	(input at: 5) fillWithColor: Color lightOrange.
	input at: 6 put: 1024 @ -2048.
	input at: 7 put: input.							"a cycle"
	input at: 8 put: (Array with: (sharedPoint _ 0 @ -30000)).
	input at: 9 put: sharedPoint.
	refStyream nextPut: input
	].
(''testFile.txt'' asFileEntry readStreamDo: [ :fileStream |
	(ReferenceStream on: fileStream) next.
	]) explore.

''testFile.txt'' asFileEntry delete.			"remove the test file"

b _ ''testFile.txt'' asFileEntry exists.		"test for existence of the file"

''testFile.txt'' asFileEntry fileContents.	"get the file-not-found dialog; note what Cancel does"

[''testFile.txt'' asFileEntry fileContents] on: MessageNotUnderstood do: [^ nil].  "avoid the exception for Cancel during the dialog"	

''testFile.txt'' asFileEntry assureExistence.	"put it back empty"

''testFile.txt'' asFileEntry fileContents. 		"print it"

''testFile.txt'' asFileEntry delete.				"remove the test file"

fullPath _ c pathName, ''\TestDir''.				"identify a directory"
FileIOAccessor new createDirectory: fullPath.	"create a directory"
FileIOAccessor new deleteDirectory: fullPath.	"delete directory; must be empty"
FileIOAccessor new createDirectory: ''./\TestDir''.	"another way, using regex"
FileIOAccessor new deleteDirectory: fullPath.	"delete directory; must be empty"

"The following statements show one way to either create an empty file, or use the file as-is if it already exists"
c _ DirectoryEntry currentDirectory.				"answer the current directory"
dirEntry _ c pathName asDirectoryEntry / ''testFile.txt''.  "create a system-independent path expression"
fullPath _ dirEntry asString.							"convert path expression to string"
fullPath asFileEntry assureExistence.				"create testFile.txt if it does not exist"
fullPath asFileEntry assureExistence.				"do nothing if testFile.txt already exists"
''testFile.txt'' asFileEntry delete.					"remove the test file"

DirectoryEntry smalltalkImageDirectory.			"answer the Smalltalk image directory"
DirectoryEntry vmDirectory.							"answer the VM directory"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'jmv 6/11/2020 17:04:45'!
font
	"Fonts"
	^
'"
Cuis includes by default one StrikeFont, but can also use TrueType fonts.

To add additional included fonts
"

FontFamily readAdditionalTrueTypeFonts.

FontFamily familyNames.				"list the installed fonts"

((LabelMorph contents: ''Text in the selected font.'') changeFont) openInHand. "look at an example"
"
The selection of currently installed fonts may be viewed from the text editor:

World > Open... > Text Editor
...then use (Cmd-k) to see a list of installed fonts.

To use any TrueTypeFont, just open the FileList, select any .ttf file, and click on [read font].
"
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 17:01:26'!
form
	"Forms"
	^
'| x y |
x _ Form dotOfSize: 20.	"Make a form containing a dot (as yet unseen)."
x displayAt: 500@200.	"Cause the form to appear at a point on the display."
Display restore.			"Erase what was displayed."

x _ Form extent: 22@18.
	"Answer a Form with a blank bitmap of depth 1."

x _ Form extent: 22@18 depth: 8.
	"Answer a Form with blank bitmap 22 pixels wide and 18 pixels deep having color depth 8."
	
y _ x getCanvas.			"convert form to canvas (a BitBltCanvas)"

WidgetMorph new drawOn: y; morphExtent: 300@200; openInWorld.
	"Display a morph on a canvas"

x _ Form fromUser.
	"Answer a Form with bitmap initialized from the area of the display screen designated by the user."

x _ Form toothpaste: 15.
	"Draw with mouse button down; terminate by option-click."

x _ Form 
		verticalGradient: 120
		topColor: Color white
		bottomColor: (Color gray: 0.65).
	"Answer a fixed-width form, 120 pixels high, which illustrates shading."

x fillColor: Color green.
	"Set all pixels in the receiver to green."

y _ x icon.
	"Answer a 16 x 16 icon of the receiver"

y _ x magnifyBy: 1.5. 
	"Answer a Form created as a scaling of the receiver. Scale may be a Float, and may be greater or less than 1.0."

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 17:02:59'!
heap
	"Heap"
	^
'| x n sort |
x _ Heap new.						"Answer a heap of default size"
x _ Heap new: (n _ 7).			"Answer a heap of size n"
x _ Heap with: #purple with: #green.	"Answer a Heap with the two arguments as elements."
sort _ [:a :c | a > c].		"Define a sorting order"
x _ Heap withAll: #(11 32 19 21).	"Create a new heap with all the elements from a collection"
x sortBlock: sort.						"Specify how to sort the heap"
x _ Heap withAll: #(11 32 19 21) sortBlock: sort.	"Create a new heap sorted according to a sortBlock"
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 5/11/2020 17:59:02'!
internalStream
	"Internal  Streams"
	^
'| b x ios str |			"For efficient viewing, have a Transcript open and use Cmd-d on these statements"
ios _ ReadStream on: ''Hello read stream''.
ios _ ReadStream on: ''Hello read stream'' from: 1 to: 5.
[(x _ ios nextLine) notNil]
   whileTrue: [Transcript show: x; newLine].
ios position: 3.
ios position.
x _ ios next.
x _ ios peek.
x _ ios contents.
b _ ios atEnd.

ios _ ReadWriteStream on: ''Hello read stream''.
ios _ ReadWriteStream on: String new.
ios _ ReadWriteStream on: ''Hello read stream'' from: 1 to: 5.
ios _ ReadWriteStream with: ''Hello read stream''.
ios _ ReadWriteStream with: ''Hello read stream'' from: 1 to: 10.
ios position: 0.
[(x _ ios nextLine) notNil]
   whileTrue: [Transcript show: x; newLine].
ios position: 6.
ios position.
ios nextPutAll: ''Chris''.
x _ ios next.
x _ ios peek.
x _ ios contents.
b _ ios atEnd.

str _ String new writeStream.				"use a stream to form a string"
str nextPutAll: ''As of ''.					"more efficient than concatenation"
str nextPutAll: Date today asString.
str nextPut: $,; space.						"newLine and tab are also possible"
str nextPutAll: Float pi printString, '' radians = ''.  "concatenation can be used"
str nextPutAll: Float pi radiansToDegrees printString.
str nextPutAll: '' degrees.''; newLine.  "complete the formation"
x _ str contents.								"to get the results"

x _ String streamContents: [:string |	"a more compact way"
	string 
		nextPutAll: ''As of '';
		nextPutAll: Date today asString;
		nextPut: $,; space;
		nextPutAll: Float twoPi printString, '' radians = '';  "concatenation can be used"
		nextPutAll: Float twoPi radiansToDegrees printString;
		nextPutAll: '' degrees.''; newLine].  "complete the formation"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 17:07:36'!
interval
	"Intervals"
	^
'| b x y sum max |
x _ Interval from: 5 to: 10.					"create interval object"
x _ 5 to: 10.
x _ Interval from: 5 to: 10 by: 2.			"create interval object with specified increment"
x _ 5 to: 10 by: 2.
b _ x isEmpty.										"test if empty"
y _ x size.											"number of elements"
x includes: 9.										"test if element is in collection"
x do: [:k | Transcript show: k printString; newLine].	"iterate over interval"
b _ x allSatisfy: [:a | (a >= 1) & (a <= 4)].	"test if all elements meet condition"
y _ x select: [:a | a > 7].					"return collection of elements that pass test"
y _ x reject: [:a | a < 2].					"return collection of elements that fail test"
y _ x collect: [:a | a + a].					"transform each element for new collection"
y _ x detect: [:a | a > 3] ifNone: [].		"return first element that passes test"
y _ x findFirst: [:a | a > 6].				"find position of first element that passes test"
sum _ 0. x do: [:a | sum _ sum + a]. sum.	"sum elements"
sum _ 0. 1 to: (x size) do: [:a | sum _ sum + (x at: a)]. sum.	"sum elements"
sum _ x inject: 0 into: [:a :c | a + c].	"sum elements"
max _ x inject: 0 into: [:a :c | (a > c)	"find max element in collection"
   ifTrue: [a]
   ifFalse: [c]].
y _ x asArray.										"convert to array"
y _ x asOrderedCollection.						"convert to ordered collection"
y _ x asSortedCollection.						"convert to sorted collection"
y _ x asBag.										"convert to bag collection"
y _ x asSet.										"convert to set collection"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 5/11/2020 20:36:28'!
introduction
	"Using Terse Guide"
	^
'"
You are reading this in the text pane of the Terse Guide window. This pane is a kind of Workspace and is managed by the SmalltalkEditor of Cuis. This means that you can Do (Cmd-d) or Print (Cmd-p) statements in order to see the result. If you have a Transcript open, the result will appear in it.

The SmalltalkEditor enables you to Do or Print a statement when the cursor is in the statement. It is not necessary to first highlight the entire statement, unless it continues on more than one line.

This text pane is refreshed each time it is accessed. You can change the examples and run them without hurting anything. Go ahead and experiment!!

The Terse Guide does not attempt to provide the entire protocol of a topic. You are encouraged to explore further by using the Protocol Browser as follows:

Button 1>World>Open...>Browser 
then in the upper right pane:
Button 2>Message List>browse protocol

You can narrow the scope of protocol browsing by selecting a class in the upper left pane of the Protocol Browser.
"
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 17:12:40'!
iterationStatement
	"Iterations"
	^
'| x y |		"For efficient viewing, have a Transcript open and use Cmd-d on these statements"
x _ 4. y _ 1.

#(5 4 3) do: [:a | x _ x + a].					"iterate over a sequenceable collection"
x.

[x > 0] whileTrue: [x _ x - 1. y _ y * 2].	"while true loop"
y.

[x >= 4] whileFalse: [x _ x + 1. y _ y * 2].	"while false loop"
y.

x timesRepeat: [y _ y * 2].						"times repeat loop (i _ 1 to x)"
y.

1 to: x do: [:a | y _ y * 2].						"for loop"
y.

1 to: x by: 2 do: [:a | y _ y / 2].				"for loop with specified increment"
y.
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'KenD 6/2/2020 20:59:34'!
layoutMorph
	"Layout Morph"
	^
'| myLayout |
myLayout _ LayoutMorph new.		"this illustrates a mistake"

"A LayoutMorph places its submorphs in a horizontal Row or a vertical Column"
myLayout _ LayoutMorph newColumn.
myLayout _ LayoutMorph newRow color: Color cyan.
myLayout openInWorld.
myLayout morphExtent: (6 * myLayout morphExtent). "Make larger to hold submorphs"

myLayout addAllMorphs: { WidgetMorph initializedInstance. ImageMorph initializedInstance. WidgetMorph initializedInstance. }.

"When there is space left over, the submorphs can be shifted
 along the layout axis.  This is the axisEdgeWeight, a number from
 0 to 1, which can also be specified by a symbol."

myLayout axisEdgeWeight: #rowRight.
myLayout axisEdgeWeight: #center.
myLayout axisEdgeWeight: #rowLeft.

myLayout beColumn.
myLayout direction." #vertical "
myLayout axisEdgeWeight: #columnBottom.
myLayout axisEdgeWeight: #center.
myLayout axisEdgeWeight: #columnTop.

myLayout beRow.
myLayout direction. " #horizontal "

myLayout separation: 10@20. "set the amount of separation for submorphs"	
myLayout xSeparation.			"answer the x component of the specified separation"
myLayout ySeparation.			"answer the y component of the specified separation"
myLayout beColumn.

"Submorphs may also have a LayoutSpec to specify proportion and offAxis placement"
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'KenD 6/3/2020 15:31:03'!
layoutSpec
	"Layout Specification"
	^
'| layout image interiorLayout rect |
"A LayoutMorph places its submorphs along the layout axis.
 The submorphs may use a LayoutSpec to specify, for width and height,
 a fixed or porportional extent, a minimum extent, an offAxisEdgeWeight."

"Some Morphs to use"
rect _ WidgetMorph initializedInstance.
image _ ImageMorph initializedInstance.
rect layoutSpec: (LayoutSpec fixedWidth: 80 proportionalHeight: 0.6 offAxisEdgeWeight: #center).
"Layouts can contain layouts"
interiorLayout _ LayoutMorph newColumn.
interiorLayout separation: 10@20.
interiorLayout layoutSpec: (LayoutSpec proportionalWidth: 0.3 proportionalHeight: 60).
interiorLayout addMorph: WidgetMorph initializedInstance.
interiorLayout addAdjusterAndMorph: (WidgetMorph new) proportionalWidth: 1.0.
"A LayoutMorph to contain them"
layout _ LayoutMorph newRow.
layout addAllMorphs: { image. interiorLayout. rect. }.
layout openInWorld.
"Make it bigger"
layout morphExtent: (3 * layout morphExtent).
"Make it colorful"
interiorLayout color: Color cyan.
layout color: Color green lighter lighter.
"add handles; mouse grab lower-right yellow circle with tooltip Change Size."
layout addHalo.
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'jmv 5/6/2020 11:14:41'!
literals
	"Literals"
	^
'"Literals are objects that are created when you compile a method. They are always available when the method is run, and the same instance is used each time. Remember that when a method is run, the source code, compiled to create it, is no longer used at all. This includes the source code for the literals. In the example below, the CompiledMethod will not have a string ''3.14'', but the Float object built after it.
Note: It is considered bad practice to later modify them, as they would no longer match their source code."
| b x |
b _ true.									"true pseudo-variable"
b _ false.									"false pseudo-variable"
x _ nil.										"nil object pseudo-variable"
x _ 1.										"SmallInteger literal"
x _ 3.14.									"Float literal"
x _ 2e-2.									"Fraction literal"
x _ 2.0e-2.									"Float literal"
x _ 7/8.										"Fraction literal"
x _ 16r0F.									"SmallInteger literal".
x _ 16rFFFFFFFF.							"LargePositiveInteger literal".
x _ 16rFFFFFFFF negated.				"LargeNegativeInteger literal".
x _ -1.										"negative SmallInteger literal"
x _ ''Hello''.								"String literal"
x _ ''I''''m here''.							"single quote escape"
x _ $A.										"Character literal"
x _ $ .										"Character literal (space)"
x _ #aSymbol.								"Symbol literal"
x _ #(3 2 1).								"Array literal"
x _ #(''abc'' 2 $a).						"mixing of types allowed (all literal)"
x _ #[3 2 1 0].							"ByteArray literal"
x _ #[1.0 3.141592 6.02e23].			"Float64Array literal"

x _ {''Hello'' size. Float pi. 1.0 arcTan }.    "Warning: NOT a literal. Created on each run"
x _ `{''Hello'' size. Float pi. 1.0 arcTan }`.  "Backtick syntax. Anything can be a literal!!"
x _ `{ 1. 3. 5. 7. 11. 13. 17} asSet`.          "A literal Set"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 17:49:15'!
metaclass
	"Class / Metaclass"
	^
'| b x |
x _ String name.							"class name"
x _ String category.						"organization category"
x _ String comment.						"class comment"
x _ String kindOfSubclass.				"subclass type - subclass: variableSubclass, etc"
x _ String definition.					"class definition"
x _ String instVarNames.				"immediate instance variable names"
x _ String allInstVarNames.			"accumulated instance variable names"
x _ String classVarNames.				"immediate class variable names"
x _ String allClassVarNames.			"accumulated class variable names"
x _ String sharedPools.					"immediate dictionaries used as shared pools"
x _ String allSharedPools.				"accumulated dictionaries used as shared pools"
x _ String selectors.					"message selectors for class"
x _ String sourceCodeAt: #indexOf:.	"source code for specified method"
x _ String allInstances.				"collection of all instances of class"
x _ String superclass.					"immediate superclass"
x _ String allSuperclasses.			"accumulated superclasses"
x _ String withAllSuperclasses.		"receiver class and accumulated superclasses"
x _ String subclasses.					"immediate subclasses"
x _ String allSubclasses.				"accumulated subclasses"
x _ String withAllSubclasses.			"receiver class and accumulated subclasses"
b _ String instSize.						"number of named instance variables"
b _ String isFixed.						"true if no indexed instance variables"
b _ String isVariable.					"true if has indexed instance variables"
b _ String isPointers.					"true if index instance vars contain objects"
b _ String isBits.						"true if index instance vars contain bytes/words"
b _ String isBytes.						"true if index instance vars contain bytes"
b _ String isWords.						"true if index instance vars contain words"
Object withAllSubclasses size.		"get total number of class entries"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'KenD 5/1/2020 16:49:17'!
methodCall
	"Method Calls"
	^
'"
	Method calls:
		- unary methods are messages with no arguments	
		- binary methods
		- keyword methods are messages with selectors including colons
 
	standard categories/protocols:
		- initialize-release    (methods called for new instance)
		- accessing             (get/set methods)
		- testing               (boolean tests - is)
		- comparing             (boolean tests with parameter
		- displaying            (gui related methods)
		- printing              (methods for printing)
		- updating              (receive notification of changes)
		- private               (methods private to class)
		- instance-creation     (class methods for creating instance)
"
| x |
x _ 2 sqrt.									"unary message"
x _ 2 raisedTo: 10.						"keyword message"
x _ 194 * 9.								"binary message"
Transcript show: (194 * 9) printString; newLine.	"combination (chaining)"
x _ 2 perform: #sqrt.					"indirect method invocation"
Transcript									"cascading ; send multiple messages to first receiver"
   show: ''hello '' ;
   show: ''world'' ;
   newLine.
"Without semicolon cascading"
x := WidgetMorph new.
x color: Color yellow.
x openInWorld.
"Using semicolon cascade -- we don''t need the variable, x"
WidgetMorph new
	color: Color yellow; 
	openInWorld.
3 + 2; * 100.							"result=300. Sends message to same receiver (3)"
"Parentheses are used for grouping.  In many cases parentheses can be replaced by ::"
3 squared ; + 5.  " 8 -- semicolon -> cascade to _original_ receiver (3)"
(3 squared) + 5. " 14 -- use parens to group"
3 squared :: + 5. " 14 -- same but double-colon -> cascade to _result_ of expression at left (9)"
"The double colon is useful in eliding parens from complex keyword expressions"
(3 squared between: 2 squared and: 4 squared) ifTrue: [ #Great ] ifFalse: [ #Oops ].
3 squared between: 2 squared and: 4 squared :: ifTrue: [ #Great ] ifFalse: [ #Oops ].

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 3/5/2015 11:34'!
misc
	"Miscellaneous"
	^
'| x |
Smalltalk condenseChanges.					"compress the change file"
x _ FillInTheBlankMorph request: ''Prompt Me''.		"prompt user for input"
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 17:53:32'!
morph
	"Morphs"
	^
'| x y z w v |			"For efficient viewing, have a Transcript open and use Cmd-d on these statements"
x _ Morph new.				"a new but invisible morph"
x openInWorld.				"make visible in upper left of screen"
x openInHand.				"make visible at cursor location"
								"drag with the left button; show halos with the middle button"
x name: #MyName.			"used in the halos"
x name: ''My Name''.		"alternative, allowing imbedded blanks"
x morphPosition: 100@65.	"set the morph location"
y _ FillInTheBlankMorph request: ''Please Enter Something''.	"prompt user for input"
Feature require: #''Morphic-Widgets-Extras''.	"ensure a requirement is available for the following plot"
x _ FunctionGraphMorph initializedInstance openInHand.	"example, plotting two functions"
x _ SystemWindow new openInWorld.	"open a window"
x setLabel: ''Label for It''.
y _ LayoutMorph example10.				"organizing several morphs"
x addMorph: y.								"place morphs in a SystemWindow"

z _ x submorphs at: 1 :: submorphs at: 1 :: submorphs at: 2 :: submorphs at: 2.  "select a deep submorph"
	"The ''::'' syntax avoids nested parentheses"
z name.						"name of the deep submorph"
w _ z owningWindow.		"answer the first enclosing morph that is a kind of Window"
w class.
v _ x findDeepSubmorphThat: [:m | m name = #G] ifAbsent: [nil].	"find the submorph named #F"
v == z.						"verify that v and z are identical"

LayoutMorph includeInNewMorphMenu.
	"Return true for all classes that can be instantiated from the menu"

y _ Morph initializedInstance openInHand.
	"Answer an instance of the receiver which in some sense is initialized.  In the case of Morphs, this will yield an instance that can be attached to the Hand after having received the same kind of basic initialization that would be obtained from an instance chosen from the ''new morph'' menu.
	Return nil if the receiver is reluctant for some reason to return such a thing"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 17:56:38'!
orderedCollection
	"Ordered Collections"
	^
'| b x y sum max |			"For efficient viewing, have a Transcript open and use Cmd-d on these statements"
x _ OrderedCollection with: 4 with: 3 with: 2 with: 1.	"create collection with 4 elements"
x _ OrderedCollection new.						"allocate a collection"
x add: 3; add: 2; add: 1; add: 4; yourself.	"add element to collection"
y _ x addFirst: 5; yourself.					"add element at beginning of collection"
y _ x removeFirst;	yourself.				"remove first element in collection"
y _ x addLast: 6; yourself.					"add element at end of collection"
y _ x removeLast;	yourself.					"remove last element in collection"
y _ x addAll: #(7 8 9); yourself.			"add elements to the collection"
y _ x removeAll: #(1 8 7).						"remove elements from the collection and answer the removed elements"
x at: 2 put: 3; yourself.						"set element at index"
y _ x remove: 5 ifAbsent: [].					"remove element from collection"
b _ x isEmpty.										"test if empty"
y _ x size.											"number of elements"
y _ x at: 2.										"retrieve element at index"
y _ x first.										"retrieve first element in collection"
y _ x last.											"retrieve last element in collection"
b _ x includes: 5.								"test if element is in collection"
y _ x copyFrom: 2 to: 3.						"subcollection"
y _ x indexOf: 3 ifAbsent: [0].				"first position of element within collection"
y _ x occurrencesOf: 3.						"number of times object in collection"
x do: [:a | Transcript show: a printString; newLine].	"iterate over the collection"
b _ x allSatisfy: [:a | (a >= 1) & (a <= 4)].	"test if all elements meet condition"
y _ x select: [:a | a > 2].					"return collection of elements that pass test"
y _ x reject: [:a | a < 2].					"return collection of elements that fail test"
y _ x collect: [:a | a + a].					"transform each element for new collection"
y _ x detect: [:a | a > 3] ifNone: [].		"return first element that passes test"
y _ x findFirst: [:a | a < 2].				"find position of first element that passes test"
sum _ 0. x do: [:a | sum _ sum + a]. sum.	"sum elements"
sum _ 0. 1 to: (x size) do: [:a | sum _ sum + (x at: a)]. sum.  "sum elements"
sum _ x inject: 0 into: [:a :c | a + c].	"sum elements"
max _ x inject: 0 into: [:a :c | (a > c)	"find max element in collection"
   ifTrue: [a]
   ifFalse: [c]].
y _ x shuffled.									"randomly shuffle collection"
y _ x asArray.										"convert to array"
y _ x asOrderedCollection.						"convert to ordered collection"
y _ x asSortedCollection.						"convert to sorted collection"
y _ x asBag.										"convert to bag collection"
y _ x asSet.										"convert to set collection"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 17:57:06'!
point
	"Point"
	^
'| x y |
x _ 200@100.									"obtain a new point"
y _ x x.											"x coordinate"
y _ x y.											"y coordinate"
x _ 200@100 negated.							"negates y"
x _ (200@100) negated.						"negates x and y"
x _ (-200@ -100) abs.						"absolute value of x and y"
x _ (200.5@100.5) rounded.					"round x and y"
x _ (200.5@100.5) truncated.				"truncate x and y"
x _ 200@100 + 100.							"add scale to both x and y"
x _ 200@100 - 100.							"subtract scale from both x and y"
x _ 200@100 * 2.								"multiply x and y by scale"
x _ 200@100 / 2.								"divide x and y by scale"
x _ 200@100 // 2.								"divide x and y by scale"
x _ 200@100 \\ 3.								"remainder of x and y by scale"
x _ 200@100 + (50@25).						"add points"
x _ 200@100 - (50@25).						"subtract points"
x _ 200@100 * (3@4).							"multiply points"
x _ 200@100 // (3@4).						"divide points"
x _ 200@100 max: 50@200.					"max x and y"
x _ 200@100 min: 50@200.					"min x and y"
x _ 20@5 dotProduct: 10@2.					"sum of product (x1*x2 + y1*y2)"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 8/10/2018 18:30:51'!
popUpMenu
	"PopUpMenu"
	^
'| x y b arr lin |
b _ PopUpMenu confirm: ''Are you hungry?''.
b _ PopUpMenu confirm: ''Reboot universe'' orCancel: [''Nevermind''].
b _ PopUpMenu 
		confirm: ''Are you hungry?''
		trueChoice: ''yes, I''''m famished''
		falseChoice: ''no, I just ate''.
PopUpMenu inform: ''I like Cuis''.
arr _ #(''Alpha'' ''Beta'' ''Gamma'' ''something else'').
x _ (PopUpMenu labelArray: arr) startUpWithCaption: ''Please pick one.''.
lin _ #(3).
x _ (PopUpMenu labelArray: arr lines: lin) 
	startUpWithCaption: nil 
	at: 450@200
	allowKeyboard: false.
y _ (PopUpMenu withCaption: ''Special'' chooseFrom: ''Alpha\Beta\Gamma\something else'').

x _ FillInTheBlankMorph request: ''Enter the (whatever)'' initialAnswer: ''the default''.
		"when it is not possible to pre-determine an answer"
		
"For potentially long menus, the following includes ''more...'' and ''start over...'' behavior"
x _ (PopUpMenu labels: (String streamContents: [:s | 1 to: 100 do: [:i | s print: i; newLine]])
		lines: (5 to: 100 by: 5)) startUpWithCaption: ''Long Menu Example''.		
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 3/1/2015 12:58'!
primitive
	"Primitive"
	^
'	"Some messages in the system are responded to primitively. A primitive   
	response is performed directly by the interpreter rather than by evaluating   
	expressions in a method. The methods for these messages indicate the   
	presence of a primitive response by including <primitive: xx> before the   
	first expression in the method.   
	  
	Primitives exist for several reasons. Certain basic or ''primitive'' 
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
	If the primitive routine is not successful, the primitive ''fails'', and the 
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
	optional, the comment will say ''Optional''. An optional primitive that is not 
	implemented always fails, and the Smalltalk expressions do the work 
	instead.  
	 
	If a primitive is not optional, the comment will say, ''Essential''. Some 
	methods will have the comment, ''No Lookup''. See Object 
	howToModifyPrimitives for an explanation of special selectors which are 
	not looked up.  
	  
	For the primitives for +, -, *, and bitShift: in SmallInteger, and truncated 
	in Float, the primitive constructs and returns a 16-bit 
	LargePositiveInteger when the result warrants it. Returning 16-bit 
	LargePositiveIntegers from these primitives instead of failing is 
	optional in the same sense that the LargePositiveInteger arithmetic 
	primitives are optional. The comments in the SmallInteger primitives say, 
	''Fails if result is not a SmallInteger'', even though the implementor has the 
	option to construct a LargePositiveInteger. For further information on 
	primitives, see the ''Primitive Methods'' part of the chapter on the formal 
	specification of the interpreter in the Smalltalk book."'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 17:58:02'!
rectangle
	"Rectangle"
	^
'| x y b |
x _ Rectangle fromUser.	"Have the user designate the top left and bottom right corners of a rectangle."
y _ Form fromDisplay: x.	"Create a virtual bit map from the rectangle." 
y display.						"Show the rectangle at 0@0 on the display."
Display restore.				"Erase the rectangle."
		
b _ x containsPoint: 195@150.
y _ x encompass: 195@150.

y _ x expandBy: 20.
	"Answer a Rectangle that is outset from the receiver by a Rectangle, Point, or scalar."

y _ x extendBy: 55@77.
	"Answer a Rectangle with the same origin as the receiver, but whose corner is offset by a Rectangle, Point, or scalar."
	
y _ x extent.
	"Answer the receiver''s width @ the receiver''s height."

y _ x flippedBy: #vertical centerAt: 220@330. 
	"Answer a copy of the receiver flipped #vertical or #horizontal, about aPoint."

y _ x insetBy: 60. 
	"Answer a Rectangle that is inset from the receiver by delta. delta is a Rectangle, Point, or scalar."

y _ x insetOriginBy: 20 cornerBy: 20@60. 
	"Answer a Rectangle that is inset from the receiver by a given amount from the origin and corner."

y _ x left.
	"Answer the position of the receiver''s left vertical line."

y _ x left: 70.
	"Answer the receiver with it''s left side at the argument."
	
x _ Rectangle center: 40@50 extent: 200@150. 
	"Answer a rectangle whose center is at a point and whose width by height is a point.  "

x _ Rectangle encompassing: {(55@44). (33@22). (300@122)}.
	"Answer a rectangle which encompasses a list of points."

x _ Rectangle left: 65 right: 300 top: 120 bottom: 350. 
	"Answer a rectangle whose left, right, top, and bottom coordinates are determined by the arguments."

x _ Rectangle origin: 100@80 corner: 400@500. 
	"Answer a rectangle whose corners (top left and bottom right) are determined by the arguments."

x _ Rectangle origin: 100@80 extent: 300@200. 
	"Answer a rectangle whose top left corner is at a point and width by height is specified by a point."

x _ Rectangle originFromUser: 400@300.
	"Answer a rectangle that is determined by having the user designate the top left corner. The width and height are determined by the argument."

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 17:59:42'!
set
	"Sets"
	^
'| b x y sum max |
x _ Set with: 4 with: 3 with: 2 with: 1.	"create collection with 4 elements"
x _ Set new.										"allocate collection"
x add: 4; add: 3; add: 1; add: 2; yourself.	"add element to collection"
y _ x addAll: #(7 8 9).							"add elements to the collection and answer the added elements"
y _ x removeAll: #(7 8 9).						"remove elements from the collection and answer the removed elements"
y _ x remove: 4 ifAbsent: [].					"remove element from collection"
b _ x isEmpty.										"test if empty"
y _ x size.											"number of elements"
x includes: 4.										"test if element is in collection"
x do: [:a | Transcript show: a printString; newLine].	"iterate over the collection"
b _ x allSatisfy: [:a | (a >= 1) & (a <= 4)].	"test if all elements meet condition"
y _ x select: [:a | a > 2].					"return collection of elements that pass test"
y _ x reject: [:a | a < 2].					"return collection of elements that fail test"
y _ x collect: [:a | a + a].					"transform each element for new collection"
y _ x detect: [:a | a > 3] ifNone: [].		"return first element that passes test"
sum _ 0. x do: [:a | sum _ sum + a]. sum.	"sum elements"
sum _ x inject: 0 into: [:a :c | a + c].	"sum elements"
max _ x inject: 0 into: [:a :c | (a > c)	"find max element in collection"
   ifTrue: [a]
   ifFalse: [c]].
y _ x asArray.										"convert to array"
y _ x asOrderedCollection.						"convert to ordered collection"
y _ x asSortedCollection.						"convert to sorted collection"
y _ x asBag.										"convert to bag collection"
y _ x asSet.										"convert to set collection"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 18:03:26'!
sortedCollection
	"Sorted Collections"
	^
'| b x y sum max |		"For efficient viewing, have a Transcript open and use Cmd-d on these statements"
x _ SortedCollection with: 4 with: 3 with: 2 with: 1.	"create collection with 4 elements"
x _ SortedCollection new.						"allocate collection"
x _ SortedCollection sortBlock: [:a :c | a > c]."set sort criteria"
x add: 3; add: 2; add: 1; add: 4; yourself.	"add element to collection"
y _ x addFirst: 5.								"Error: Not Appropriate for a SortedCollection"
y _ x removeFirst.								"remove first element in collection"
y _ x addLast: 6.									"add element at end of collection"
y _ x removeLast.									"remove last element in collection"
y _ x addAll: #(7 8 9).							"add elements to the collection and answer the added elements"
x sortBlock: [:a :c | a > c].					"sort in descending order"
y _ x removeAll: #(7 8 9).						"remove elements from the collection and answer the removed elements"
y _ x remove: 5 ifAbsent: [].					"remove element from collection"
b _ x isEmpty.										"test if empty"
y _ x size.											"number of elements"
y _ x at: 2.											"retrieve element at index"
y _ x first.											"retrieve first element in collection"
y _ x last.											"retrieve last element in collection"
b _ x includes: 4.									"test if element is in collection"
y _ x copyFrom: 2 to: 3.							"subcollection"
y _ x indexOf: 3 ifAbsent: [0].				"first position of element within collection"
y _ x occurrencesOf: 3.							"number of times object in collection"
x do: [:a | Transcript show: a printString; newLine].	"iterate over the collection"
b _ x allSatisfy: [:a | (a >= 1) & (a <= 4)].	"test if all elements meet condition"
y _ x select: [:a | a > 2].					"return collection of elements that pass test"
y _ x reject: [:a | a < 2].					"return collection of elements that fail test"
y _ x collect: [:a | a + a].					"transform each element for new collection"
y _ x detect: [:a | a > 3] ifNone: [].		"return first element that passes test"
y _ x findFirst: [:a | a < 3].				"find position of first element that passes test"
sum _ 0. x do: [:a | sum _ sum + a]. sum.	"sum elements"
sum _ 0. 1 to: (x size) do: [:a | sum _ sum + (x at: a)]. sum.	"sum elements"
sum _ x inject: 0 into: [:a :c | a + c].	"sum elements"
max _ x inject: 0 into: [:a :c | (a > c)	"find max element in collection"
   ifTrue: [a]
   ifFalse: [c]].
y _ x asArray.										"convert to array"
y _ x asOrderedCollection.						"convert to ordered collection"
y _ x asSortedCollection.						"convert to sorted collection"
y _ x asBag.										"convert to bag collection"
y _ x asSet.										"convert to set collection"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 18:05:11'!
string
	"Strings"
	^
'| b x y |
x _ ''This is a string''.						"string assignment"
x _ ''String'', ''Concatenation''.			"string concatenation"
b _ x isEmpty.									"test if string is empty"
y _ x size.										"string size"
y _ x at: 2.									"char at location"
y _ x copyFrom: 2 to: 4.					"substring"
y _ x indexOf: $a ifAbsent: [0].			"first position of character within string"
x _ String new: 4.							"allocate string object"
x													"set string elements"
   at: 1 put: $a;
   at: 2 put: $b;
   at: 3 put: $c;
   at: 4 put: $e;
   yourself.
x _ String with: $a with: $b with: $c with: $d.	"set up 4 elements at a time"
x do: [:a | Transcript show: a printString; newLine].	"iterate over the string"
b _ x allSatisfy: [:a | (a >= $a) & (a <= $z)].	"test if all elements meet condition"
y _ x select: [:a | a > $a].				"return all elements that meet condition"

x _ String streamContents: [:str |		"using a stream to form a string"
	str 											"more efficient than concatenation"
		nextPutAll: ''This string'';
		nextPutAll: '' is formed today, '';
		nextPutAll: Date today asString;
		nextPutAll: '', without using concatenation.''].

y _ x asSymbol.								"convert string to symbol"
y _ x asArray.									"convert string to array"
x _ ''KAAPED'' asByteArray.					"convert string to byte array"
y _ x asOrderedCollection.					"convert string to ordered collection"
y _ x asSortedCollection.					"convert string to sorted collection"
y _ x asBag.									"convert string to bag collection"
y _ x asSet.									"convert string to set collection"
y _ ''world'' shuffled.						"randomly shuffle string"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 18:06:34'!
symbol
	"Symbols"
	^
'| b x y |
x _ #Hello.									"symbol assignment"
y _ #String , #Concatenation.			"symbol concatenation (result is string)"
b _ x isEmpty.								"test if symbol is empty"
y _ x size.									"string size"
y _ x at: 2.								"char at location"
y _ x copyFrom: 2 to: 4.				"substring"
y _ x indexOf: $e ifAbsent: [0].		"first position of character within string"
x do: [:a | Transcript show: a printString; newLine].	"iterate over the string"
b _ x allSatisfy: [:a | (a >= $a) & (a <= $z)].	"test if all elements meet condition"
y _ x select: [:a | a > $a].			"return all elements that meet condition"
y _ x asString.							"convert symbol to string"
y _ x asText.								"convert symbol to text"
y _ x asArray.								"convert symbol to array"
y _ x asOrderedCollection.				"convert symbol to ordered collection"
y _ x asSortedCollection.				"convert symbol to sorted collection"
y _ x asBag.								"convert symbol to bag collection"
y _ x asSet.								"convert symbol to set collection"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 5/11/2020 17:05:02'!
syntax
	"Syntax Notes"
	^
'"
	Statement form (basic binary):
		<receiver> <message selector> [<argument>]
		There are also unary and keyword forms.

	Allowable characters:
		- a-z
		- A-Z
		- 0-9
		- .+/\*~<=>@%|&?;:''#()[]{}_
		- blank, tab, cr, ff, lf

	Variables:
		- variables must be declared before use
		- shared vars must begin with uppercase
		- local vars must begin with lowercase
		- reserved names: nil, true, false, self, super, and Smalltalk

	Variable scope:
		- Global: defined in Dictionary Smalltalk and accessible by all objects in system
		- Special: (reserved) Smalltalk, super, self, true, false, & nil
		- Method Temporary: local to a method
		- Block Temporary: local to a block
		- Pool: variables in a Dictionary object
		- Method Parameters: automatic local vars created as a result of message call with params
		- Block Parameters: automatic local vars created as a result of value: message call	
		- Class: shared with all instances of one class & its subclasses
		- Class Instance: unique to each instance of a class
		- Instance Variables: unique to each instance

	Comments are enclosed in quotes
	Period (.) is the statement separator
	
	Expressions are evaluated left-to-right.
	Message precedence: unary, binary, then keyword.
	
	A message within parentheses is sent before any messages outside the parentheses.
	Expressions with parentheses can alternatively be written with ''::'' as in the following examples..."
	
BrowserWindow fullOnClass: Parser class selector: #chainExamplesDoubleColon.
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 18:07:25'!
time
	"Time"
	^
'| x y b d1 d2 w |		"For efficient viewing, have a Transcript open and use Cmd-d on these statements"
x _ Time dateAndTimeNow.			"answer the current date and time"
x _ Time now.							"answer the current time"
b _ (555 factorial) > 1.			"expend some time"
y _ Time now - x.						"time difference"
b _ x < Time now.						"compare times"
b _ y = x.								"check if equal"
d1 _ Time now asDurationSinceMidnight.
d2 _ Time now asDurationSinceMidnight.
y _ x hhmm24.							"Return a string of the form 2154 (for 9:54 pm), of exactly 4 digits"
y _ x hour.								"answer the number of hours in the receiver"
y _ x hour12.
y _ x hour24.
y _ x meridianAbbreviation.		"answer ''AM'' or ''PM''"
y _ x minute.							"answer the number of minutes in the receiver"
y _ x nanoSecond.						"answer the number of nanoSeconds in the receiver"
y _ x print24.							"Return microseconds expressed in 24-hour format"
String streamContents: [:s | Time now print24: true on: s].	"Format is ''hh:mm:ss'' if true or ''h:mm:ss am'' if false"
y _ x printMinutes.						"Return as string ''hh:mm pm''  "
String streamContents: [:s | Time now printOn: s].
y _ x second.							"answer the number of seconds in the receiver"
String streamContents: [:s | Time now storeOn: s].
x _ Time millisecondsToRun: [		"timing facility"
   1 to: 1000 do: [:index | y _ 400 factorial * index]].

w _ Time localMillisecondClock.	"milliseconds since the Smalltalk epoch"
"The Smalltalk epoch began at 00:00 on the morning of January 1, 1901, in the local host time zone."
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 10/6/2018 18:08:36'!
transcript
	"Transcript"
	^
'TranscriptWindow openTranscript.	"in case one is not open already"
Transcript clear.						"clear the transcript window"
Transcript show: ''Hello World''.	"output string in transcript window"
Transcript nextPutAll: ''Hello World''.	"output string in transcript window"
Transcript nextPut: $A.				"output character in transcript window"
Transcript space.						"output space character in transcript window"
Transcript tab.						"output tab character in transcript window"
Transcript newLine.					"linefeed"
''Hello'' printOn: Transcript.		"append print string into the window"
''Hello'' storeOn: Transcript.		"append store string into the window"
Transcript endEntry.					"flush the output buffer"

"NOTE: from the pop-up menu, you can open a snapshot of the transcript in a workspace window."
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/23/2016 12:21'!
trie
	"Trie"
	^
'| t |		"For efficient viewing, have a Transcript open and use Cmd-d on these statements"
t _ Trie new.
t add: ''car''.
t at: ''car'' put: Float pi.
t at: ''cat'' put: Date today.
t explore.
(t includesKey: ''car'') print.
(t includes: ''cat'') print.
t at: ''crow'' put: 7.
t forPrefix: ''c'' keysAndValuesDo: [:key :val | {key} print].
t forPrefix: ''ca'' keysAndValuesDo: [:key :val | {key} print].

t _ Trie new.
Smalltalk allImplementedMessages do: [ :s | t add: s ].
t explore.

'
! !

!TerseGuideHelp class methodsFor: 'displaying' stamp: 'dhn 11/14/2015 13:08'!
display: aSelector    
	"Open a Workspace on the text in aSelector"    
	| window |
	  
	window _ TerseGuideHelp new.    
	window 	actualContents: (self perform: aSelector).    
	window	openLabel: ('Terse Guide to ', (self class firstPrecodeCommentFor: aSelector))! !

!TerseGuideHelp class methodsFor: 'accessing' stamp: 'RNG 7/15/2018 01:33:41'!
additionalTopicTitles

	^ (self additionalTopics collect: [ :topic | topic title ]) asArray! !

!TerseGuideHelp class methodsFor: 'accessing' stamp: 'RNG 7/15/2018 01:34:27'!
additionalTopics
	"Topics that can be dynamically added by creating subclasses of TerseGuideTopic"

	^ TerseGuideTopic allSubclasses collect: [ :topicClass | topicClass new ]! !

!TerseGuideHelp class methodsFor: 'accessing' stamp: 'jmv 5/6/2020 11:11:26'!
pages
	"Answer the topics, in order, for the window list pane"
	^ #(
		'Introduction'
		'Syntax'
		'Transcript'
		'Assignment'
		'Literals'
		'Booleans'
		'Arithmetic'
		'Bit-Wise'
		'Conversion'
		'Blocks'
		'Exceptions'
		'Method Calls'
		'Conditional Statements'
		'Iterations'
		'Character'
		'Symbols'
		'Strings'
		'Fonts'
		'Arrays'
		'Ordered Collections'
		'Sorted Collections'
		'Bags'
		'Sets'
		'Intervals'
		'Associations'
		'Dictionary'
		'Trie'
		'Heap'
		'Internal Streams'
		'Coding'
		'File Operations'
		'Features'
		'Date'
		'Time'
		'Point'
		'Rectangle'
		'Forms'
		'Canvas'
		'Colors'
		'Morphs'
		'Layout Morph'
		'Layout Spec'
		'PopUpMenu'
		'Dynamic Messages'
		'Object'
		'Metaclass'
		'Debugging'
		'Miscellaneous'
		'Primitives'
		'Custom Browsers'
	) , self additionalTopicTitles! !

!TerseGuideWindow methodsFor: 'initialization' stamp: 'dhn 6/15/2016 19:03'!
buildMorphicWindow
	"Define the window layout for Terse Guide"
	| row |
	
	list1 _ (PluggableListMorph
				model: model
				listGetter: #topicList
				indexGetter: #topicListIndex
				indexSetter: #topicListIndex:
				mainView: self
				menuGetter: nil
				keystrokeAction: nil)
					name: 'Topic List'.
	textModelM _ TextModelMorph withModel: model.
	textModelM
		askBeforeDiscardingEdits: false;
		name: 'Work Space'.
	model when: #topicListIndex send: #update: to: list1 withArguments: #(#topicListIndex).
	row _ LayoutMorph newRow.
	row
		name: #Row;
		addMorph: list1 proportionalWidth: 0.2;
		addAdjusterAndMorph: textModelM proportionalWidth: 0.8.
	self layoutMorph
		addMorph: row.
	self setLabel: 'Terse Guide'! !

!TerseGuideWindow methodsFor: 'accessing' stamp: 'dhn 5/17/2016 21:21'!
textModelM
	"Answer the value of textModelM"

	^ textModelM! !

!TerseGuideWindow class methodsFor: 'instance creation' stamp: 'dhn 5/17/2016 21:21'!
openTerseGuide
"
	TerseGuideWindow openTerseGuide.
"
	| window help |

	(window _ TerseGuideWindow open: (help _ TerseGuideHelp new) label: 'Terse Guide')
		setWindowColor:  Theme current workspace.
	help textPane: window textModelM.
	help 
		contents: '';
		window: window! !

!TerseGuideTopic methodsFor: 'accessing' stamp: 'RNG 7/15/2018 01:02:39'!
examples

	^ #()! !

!TerseGuideTopic methodsFor: 'accessing' stamp: 'RNG 7/15/2018 01:37:27'!
script

	^ String streamContents: [ :stream |
		self examples do: [ :example |
			self append: example toScript: stream
		]
	]! !

!TerseGuideTopic methodsFor: 'accessing' stamp: 'RNG 7/15/2018 01:02:32'!
title

	self subclassResponsibility! !

!TerseGuideTopic methodsFor: 'private - script building' stamp: 'RNG 7/15/2018 01:38:22'!
append: example toScript: stream

	stream
		nextPutAll: (example at: #code);
		nextPutAll: '. ';
		nextPutAll: (self commentFor: example);
		newLine! !

!TerseGuideTopic methodsFor: 'private - script building' stamp: 'RNG 7/15/2018 01:38:32'!
commentFor: example

	^ '"' , (example at: #category) , ' - ' , (example at: #description) , '"'! !
