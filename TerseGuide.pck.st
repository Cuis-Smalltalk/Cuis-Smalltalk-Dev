'From Cuis 4.2 of 25 July 2013 [latest update: #2169] on 26 February 2015 at 1:40:35.94027 pm'!
'Description Displays topics intended to aid programmers in writing code for Cuis. Needs to be sanitized for changes in selectors, missing classes, etc. UI needs work.'!
!provides: 'TerseGuide' 1 19!
!classDefinition: #TerseGuideWindow category: #TerseGuide!
SystemWindow subclass: #TerseGuideWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TerseGuide'!
!classDefinition: 'TerseGuideWindow class' category: #TerseGuide!
TerseGuideWindow class
	instanceVariableNames: ''!

!classDefinition: #TerseGuideHelp category: #TerseGuide!
Object subclass: #TerseGuideHelp
	instanceVariableNames: 'topicListIndex selectedTopic topicList'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'TerseGuide'!
!classDefinition: 'TerseGuideHelp class' category: #TerseGuide!
TerseGuideHelp class
	instanceVariableNames: ''!


!TerseGuideHelp commentStamp: '<historical>' prior: 0!
TerseGuideHelp is adapted from the Squeak terse guide by Chris Rathman (http://www.angelfire.com/tx4/cus/notes/smalltalk.html), maintained on the swiki at http://wiki.squeak.org/squeak/5699. Copyrights and credit for the original document belong to Chris Rathman.

The original document was formatted for printing with a fixed font. Here it is reformatted for display in a help browser, with various updates and corrections to match the current state of the Squeak image. The contents of the help pages may be evaluated as workspace expressions.

TerseGuideHelp display: #arithmetic
TerseGuideHelp display: #array
TerseGuideHelp display: #transcript
!

!TerseGuideWindow methodsFor: 'as yet unclassified' stamp: 'dhn 2/25/2015 09:51'!
buildMorphicWindow
	"Answer a window for Terse Guide"
	| aTextMorph list1 row |
	list1 _ PluggableListMorph
				model: model
				listGetter: #topicList
				indexGetter: #topicListIndex
				indexSetter: #topicListIndex:
				mainView: self
				menuGetter: nil
				keystrokeAction: nil.
	aTextMorph _ TextModelMorph
				textProvider: model
				textGetter: #selectedTopic.
	aTextMorph askBeforeDiscardingEdits: false.
	row _ LayoutMorph newRow.
	row
		addMorph: list1 proportionalWidth: 0.2;
		addAdjusterAndMorph: aTextMorph proportionalWidth: 0.8.
	self layoutMorph
		addMorph: row.
	self setLabel: 'Terse Guide'! !

!TerseGuideWindow class methodsFor: 'as yet unclassified' stamp: 'dhn 2/24/2015 21:48'!
openTerseGuide
	TerseGuideWindow open: TerseGuideHelp new label: nil! !

!TerseGuideHelp methodsFor: 'as yet unclassified' stamp: 'dhn 2/25/2015 13:34'!
editorClass
	^SmalltalkEditor! !

!TerseGuideHelp methodsFor: 'as yet unclassified' stamp: 'dhn 2/26/2015 08:08'!
initialize
	topicList _ self class pages.
! !

!TerseGuideHelp methodsFor: 'as yet unclassified' stamp: 'dhn 2/26/2015 08:26'!
selectedTopic
	"Answer the text of the selected topic"
	^ self updateTopicText! !

!TerseGuideHelp methodsFor: 'as yet unclassified' stamp: 'dhn 2/26/2015 08:22'!
selectedTopic: aTopic
	selectedTopic _ aTopic.
	self updateTopicText.
	self changed: #topicListIndex! !

!TerseGuideHelp methodsFor: 'as yet unclassified' stamp: 'dhn 2/26/2015 09:04'!
text
	^ self selectedTopic
! !

!TerseGuideHelp methodsFor: 'as yet unclassified' stamp: 'dhn 2/26/2015 07:59'!
topicList
	^ topicList! !

!TerseGuideHelp methodsFor: 'as yet unclassified' stamp: 'dhn 2/26/2015 08:42'!
topicListIndex
	topicList ifNil: [ ^ topicListIndex _ 0 ].
	^ topicList indexOf: selectedTopic! !

!TerseGuideHelp methodsFor: 'as yet unclassified' stamp: 'dhn 2/26/2015 08:41'!
topicListIndex: index
	topicListIndex _ index.
	self selectedTopic: 
		(topicList at: index ifAbsent: nil)! !

!TerseGuideHelp methodsFor: 'as yet unclassified' stamp: 'dhn 2/26/2015 08:33'!
updateTopicText
	"Answer the text of the selected topic"
	self topicListIndex = 0 ifTrue: [^ ''].
	^ self class perform: (self topicList at: self topicListIndex)! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 16:29'!
arithmetic
	"Arithmetic Expressions"
	^
'| x |
x _ 6 + 3.								"addition"
x _ 6 - 3.								"subtraction"
x _ 6 * 3.								"multiplication"
x _ 1 + 2 * 3.							"evaluation always left to right (1 + 2) * 3"
x _ 5 / 3.								"division with fractional result"
x _ 5.0 / 3.0.							"division with float result"
x _ 5.0 // 3.0.							"integer divide"
x _ 5.0 \\ 3.0.							"integer remainder"
x _ -5.									"unary minus"
x _ 5 sign.								"numeric sign (1, -1 or 0)"
x _ 5 negated.							"negate receiver"
x _ 1.2 integerPart.					"integer part of number (1.0)"
x _ 1.2 fractionPart.					"fractional part of number (0.2)"
x _ 5 reciprocal.						"reciprocal function"
x _ 6 * 3.1.							"auto convert to float"
x _ 5 squared.							"square function"
x _ 25 sqrt.							"square root"
x _ 5 raisedTo: 2.						"power function"
x _ 5 raisedToInteger: 2.				"power function with integer"
x _ 5 exp.								"exponential"
x _ -5 abs.								"absolute value"
x _ 3.99 rounded.						"round"
x _ 3.99 truncated.					"truncate"
x _ 3.99 roundTo: 1.					"round to specified decimal places"
x _ 3.99 truncateTo: 1.					"truncate to specified decimal places"
x _ 3.99 floor.							"truncate"
x _ 3.99 ceiling.						"round up"
x _ 5 factorial.							"factorial"
x _ -5 quo: 3.							"integer divide rounded toward zero"
x _ -5 rem: 3.							"integer remainder rounded toward zero"
x _ 28 gcd: 12.						"greatest common denominator"
x _ 28 lcm: 12.							"least common multiple"
x _ 100 ln.								"natural logarithm"
x _ 100 log.							"base 10 logarithm"
x _ 100 log: 10	.					"logarithm with specified base"
x _ 100 floorLog: 10.					"floor of the log"
x _ 180 degreesToRadians.			"convert degrees to radians"
x _ 3.14 radiansToDegrees.			"convert radians to degrees"
x _ 0.7 sin.								"sine"
x _ 0.7 cos.							"cosine"
x _ 0.7 tan.							"tangent"
x _ 0.7 arcSin.							"arcsine"
x _ 0.7 arcCos.						"arccosine"
x _ 0.7 arcTan.							"arctangent"
x _ 10 max: 20.						"get maximum of two numbers"
x _ 10 min: 20.							"get minimum of two numbers"
x _ Float pi.							"pi"
x _ Float e.							"exp constant"
x _ Float infinity.						"infinity"
x _ Float nan.							"not-a-number"
x _ Random new next; yourself. x next.	"random number stream (0.0 to 1.0)"
x _ 100 atRandom.						"quick random number"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/25/2015 20:51'!
array
	"Arrays"
	^
'"
	Array:         Fixed length collection
	ByteArray:     Array limited to byte elements (0-255)
	WordArray:     Array limited to word elements (0-2^32)
"
| b x y sum max |
x _ #(4 3 2 1).									"constant array"
x _ Array with: 5 with: 4 with: 3 with: 2.		"create array with up to 4 elements"
x _ Array new: 4.								"allocate an array with specified size"
x												"set array elements"
   at: 1 put: 5;
   at: 2 put: 4;
   at: 3 put: 3;
   at: 4 put: 2.
b _ x isEmpty.									"test if array is empty"
y _ x size.										"array size"
y _ x at: 4.										"get array element at index"
b _ x includes: 3.								"test if element is in array"
y _ x copyFrom: 2 to: 4.						"subarray"
y _ x indexOf: 3 ifAbsent: [0].					"first position of element within array"
y _ x occurrencesOf: 3.						"number of times object in collection"
x do: [:a | Transcript show: a printString; newLine].	"iterate over the array"
b _ x allSatisfy: [:a | (a >= 1) & (a <= 4)].		"test if all elements meet condition"
y _ x select: [:a | a > 2].						"return collection of elements that pass test"
y _ x reject: [:a | a < 2].						"return collection of elements that fail test"
y _ x collect: [:a | a + a].						"transform each element for new collection"
y _ x detect: [:a | a > 3] ifNone: [].				"return first element that passes test"
y _ x findFirst: [:a | a < 3].						"find position of first element that passes test"
sum _ 0. x do: [:a | sum _ sum + a]. sum.		"sum array elements"
sum _ 0. 1 to: (x size) do: [:a | sum _ sum + (x at: a)].	"sum array elements"
sum _ x inject: 0 into: [:a :c | a + c].			"sum array elements"
max _ x inject: 0 into: [:a :c | (a > c)			"find max element in array"
   ifTrue: [a]
   ifFalse: [c]].
y _ x shuffled.									"randomly shuffle collection"
y _ x asArray.									"convert to array"
y _ x asByteArray.								"convert to byte array"
y _ x asWordArray.							"convert to word array"
y _ x asOrderedCollection.					"convert to ordered collection"
y _ x asSortedCollection.						"convert to sorted collection"
y _ x asBag.									"convert to bag collection"
y _ x asSet.									"convert to set collection"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 16:30'!
assignment
	"Assignment"
	^
'| x y z |
x _ 4.									"assignment (Cuis)"
x _ 5.									"assignment"
x _ y _ z _ 6.							"compound assignment"
x _ (y _ 6) + 1.
x _ Object new.						"bind to allocated instance of a class"
x _ 123 class.							"discover the object class"
x _ Integer superclass.				"discover the superclass of a class"
x _ Object allInstances.				"get an array of all instances of a class"
x _ Integer allSuperclasses.			"get all superclasses of a class"
x _ 1.2 hash.							"hash value for object"
y _ x copy.								"copy object"
y _ x shallowCopy.						"copy object (not overridden)"
y _ x deepCopy.						"copy object and instance vars"
y _ x veryDeepCopy.					"complete tree copy using a dictionary"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 16:30'!
association
	"Associations"
	^
'| x y |
x _ #myVar->''hello''.
y _ x key.
y _ x value.

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/25/2015 20:52'!
bag
	"Bag"
	^
'| b x y sum max |
x _ Bag with: 4 with: 3 with: 2 with: 1.			"create collection with up to 4 elements"
x _ Bag new.									"allocate collection"
x add: 4; add: 3; add: 1; add: 2; yourself.		"add element to collection; see NOTE"
x add: 3 withOccurrences: 2.					"add multiple copies to collection"
y _ x addAll: #(7 8 9).							"add elements to the collection and answer the added elements; see NOTE"
y _ x removeAll: #(7 8 9).						"remove elements from the collection and answer the removed elements; see NOTE"
y _ x remove: 4 ifAbsent: [].					"remove element from collection"
b _ x isEmpty.									"test if empty"
y _ x size.										"number of elements"
b _ x includes: 3.								"test if element is in collection"
y _ x occurrencesOf: 3.						"number of times object in collection"
x do: [:a | Transcript show: a printString; newLine].	"iterate over the collection"
b _ x allSatisfy: [:a | (a >= 1) & (a <= 4)].		"test if all elements meet condition"
y _ x select: [:a | a > 2].						"return collection of elements that pass test"
y _ x reject: [:a | a < 2].						"return collection of elements that fail test"
y _ x collect: [:a | a + a].						"transform each element for new collection"
y _ x detect: [:a | a > 3] ifNone: [].				"return first element that passes test"
sum _ 0. x do: [:a | sum _ sum + a]. sum.		"sum elements"
sum _ x inject: 0 into: [:a :c | a + c].			"sum elements"
max _ x inject: 0 into: [:a :c | (a > c)			"find max element in collection"
   ifTrue: [a]
   ifFalse: [c]].
y _ x asOrderedCollection.					"convert to ordered collection"
y _ x asSortedCollection.						"convert to sorted collection"
y _ x asBag.									"convert to bag collection"
y _ x asSet.									"convert to set collection"

"NOTE: methods in subclasses of Collection, of which Bag is one, answer
 the ARGUMENT instead of the resulting collection. Examples of such 
 methods are #addAll: and #removeAll:. Sending #yourself to the collection
 will answer the collection instead of the argument."

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 16:31'!
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
b _ 16rFF allMask: 16r0F.			"test if all bits set in mask set in receiver"
b _ 16rFF anyMask: 16r0F.			"test if any bits set in mask set in receiver"
b _ 16rFF noMask: 16r0F.			"test if all bits set in mask clear in receiver"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/25/2015 20:54'!
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
x _ [ y _ 1. z _ 2. ]. x value.						"simple block usage"
x _ [ :argOne :argTwo |   argOne, '' and '' , argTwo.].	"set up block with argument passing"
Transcript show: (x value: ''First'' value: ''Second''); newLine.	"use block with argument passing"
x _ [:e | | v | v _ 1. e + v] value: 2.					"localvar in a block"
fac _ [ :n | n > 1 ifTrue:  [n * (fac value: n-1)] ifFalse: [1]].	"closure on block variable"
fac value: 5.											"closure variable scoped to its block"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 16:32'!
boolean
	"Booleans"
	^
'| b x y |
x _ 1. y _ 2.
b _ (x = y).								"equals"
b _ (x ~= y).							"not equals"
b _ (x == y).							"identical"
b _ (x ~~ y).							"not identical"
b _ (x > y).								"greater than"
b _ (x < y).								"less than"
b _ (x >= y).							"greater than or equal"
b _ (x <= y).							"less than or equal"
b _ b not.								"boolean not"
b _ (x < 5) & (y > 1).					"boolean and"
b _ (x < 5) | (y > 1).					"boolean or"
b _ (x < 5) and: [y > 1].				"boolean and (short-circuit)"
b _ (x < 5) or: [y > 1].					"boolean or (short-circuit)"
b _ (x < 5) eqv: (y > 1).				"test if both true or both false"
b _ (x < 5) xor: (y > 1).				"test if one true and other false"
b _ 5 between: 3 and: 12.				"between (inclusive)"
b _ 123 isKindOf: Number.			"test if object is class or subclass of"
b _ 123 isMemberOf: SmallInteger.	"test if object is type of class"
b _ 123 respondsTo: #sqrt.			"test if object responds to message"
b _ x isNil.								"test if object is nil"
b _ x isZero.							"test if number is zero"
b _ x positive.							"test if number is positive"
b _ x strictlyPositive.					"test if number is greater than zero"
b _ x negative.							"test if number is negative"
b _ x even.							"test if number is even"
b _ x odd.								"test if number is odd"
b _ x isLiteral.							"test if literal constant"
b _ x isInteger.						"test if object is integer"
b _ x isFloat.							"test if object is float"
b _ x isNumber.						"test if object is number"
b _ $A isUppercase.					"test if upper case character"
b _ $A isLowercase.					"test if lower case character"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 16:32'!
character
	"Character"
	^
'| x y b |
x _ $A.										"character assignment"
y _ x isLowercase.							"test if lower case"
y _ x isUppercase.							"test if upper case"
y _ x isLetter.								"test if letter"
y _ x isDigit.								"test if digit"
y _ x isAlphaNumeric.						"test if alphanumeric"
y _ x isSeparator.							"test if seperator char"
y _ x isVowel.								"test if vowel"
y _ x digitValue.							"convert to numeric digit value"
y _ x asLowercase.						"convert to lower case"
y _ x asUppercase.						"convert to upper case"
y _ x asciiValue.							"convert to numeric ascii value"
y _ x asString.								"convert to string"
b _ $A <= $B.								"comparison"
y _ $A max: $B.

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/25/2015 20:56'!
conditionalStatement
	"Conditional Statement"
	^
'| x switch result |
x _ 11.
x > 10 ifTrue: [Transcript show: ''ifTrue''; newLine].	"if then"
x > 10 ifFalse: [Transcript show: ''ifFalse''; newLine].	"if else"
x > 10											"if then else"
   ifTrue: [Transcript show: ''ifTrue''; newLine]
   ifFalse: [Transcript show: ''ifFalse''; newLine].
x > 10											"if else then"
   ifFalse: [Transcript show: ''ifFalse''; newLine]
   ifTrue: [Transcript show: ''ifTrue''; newLine].
Transcript
   show:
      (x > 10
         ifTrue: [''ifTrue'']
         ifFalse: [''ifFalse'']);
   newLine.
Transcript									"nested if then else"
   show:
      (x > 10
         ifTrue: [x > 5
            ifTrue: [''A'']
            ifFalse: [''B'']]
         ifFalse: [''C'']);
   newLine.
switch _ Dictionary new.						"switch (case) functionality"
switch at: $A put: [Transcript show: ''Case A''; newLine].
switch at: $B put: [Transcript show: ''Case B''; newLine].
switch at: $C put: [Transcript show: ''Case C''; newLine].
result _ (switch at: $B) value.

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 16:33'!
constants
	"Constants"
	^
'| b x |
b _ true.								"true constant"
b _ false.								"false constant"
x _ nil.									"nil object constant"
x _ 1.									"integer constants"
x _ 3.14.								"float constants"
x _ 2e-2.								"fractional constants"
x _ 16r0F.								"hex constant".
x _ -1.									"negative constants"
x _ ''Hello''.								"string constant"
x _ ''I''''m here''.							"single quote escape"
x _ $A.									"character constant"
x _ $ .									"character constant (space)"
x _ #aSymbol.						"symbol constants"
x _ #(3 2 1).							"array constants"
x _ #(''abc'' 2 $a).						"mixing of types allowed"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 16:33'!
conversion
	"Conversion"
	^
'| x |
x _ 3.99 asInteger.				"convert number to integer (truncates in Squeak)"
x _ 3.99 asFraction.			"convert number to fraction"
x _ 3 asFloat.					"convert number to float"
x _ 65 asCharacter.			"convert integer to character"
x _ $A asciiValue.				"convert character to integer"
x _ 3.99 printString.			"convert object to string via printOn:"
x _ 3.99 storeString.			"convert object to string via storeOn:"
x _ 15 radix: 16.				"convert to string in given base"
x _ 15 printStringBase: 16.
x _ 15 storeStringBase: 16.

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 14:37'!
date
	"Date"
	^
'| x y b |
x _ Date today.								"create date for today"
x _ Date dateAndTimeNow.					"create date from current time/date"
x _ Date readFromString: ''01/02/1999''.		"create date from formatted string"
x _ Date newDay: 12 month: #July year: 1999.	"create date from parts"
x _ Date fromDays: 36000.					"create date from elapsed days since 1/1/1901"
y _ Date dayOfWeek: #Monday.				"day of week as int (1-7)"
y _ Date indexOfMonth: #January.			"month of year as int (1-12)"
y _ Date daysInMonth: 2 forYear: 1996.		"day of month as int (1-31)"
y _ Date daysInYear: 1996.					"days in year (365|366)"
y _ Date nameOfDay: 1.						"weekday name (#Monday,...)"
y _ Date nameOfMonth: 1.					"month name (#January,...)"
y _ Date leapYear: 1996.						"1 if leap year; 0 if not leap year"
y _ x weekday.								"day of week (#Monday,...)"
y _ x previous: #Monday.					"date for previous day of week"
y _ x dayOfMonth.								"day of month (1-31)"
y _ x day.										"day of year (1-366)"
y _ x firstDayOfMonth.							"day of year for first day of month"
y _ x monthName.								"month of year (#January,...)"
y _ x monthIndex.								"month of year (1-12)"
y _ x daysInMonth.								"days in month (1-31)"
y _ x year.										"year (19xx)"
y _ x daysInYear.								"days in year (365|366)"
y _ x daysLeftInYear.							"days left in year (364|365)"
y _ x asSeconds.								"seconds elapsed since 1/1/1901"
y _ x addDays: 10.								"add days to date object"
y _ x subtractDays: 10.						"subtract days to date object"
y _ x subtractDate: (Date today).				"subtract date (result in days)"
y _ x printFormat: #(2 1 3 $/ 1 1).				"print formatted date"
b _ (x <= Date today).						"comparison"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/25/2015 20:57'!
debugging
	"Debugging"
	^
'| a b x |
x _ Object new.
x yourself.								"returns receiver"
String browse.						"browse specified class"
x inspect.								"open object inspector window"
x confirm: ''Is this correct?''.
x halt.									"breakpoint to open debugger window"
x halt: ''Halt message''.
x notify: ''Notify text''.
x error: ''Error string''.					"open up error window with title"
x shouldNotImplement.					"flag message should not be implemented"
x subclassResponsibility.				"flag message as abstract"
x errorImproperStore.					"flag an improper store into indexable object"
x errorNonIntegerIndex.				"flag only integers should be used as index"
x errorSubscriptBounds: 13.			"flag subscript out of bounds"
x primitiveFailed.						"system primitive failed"

a _ ''A1''. b _ ''B2''. a become: b.			"switch two objects"
Transcript show: a, b; newLine.

x doesNotUnderstand: (Message selector: #foo).	"flag message is not handled"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/25/2015 21:48'!
dictionary
	"Dictionary"
	^
'"	
	Dictionary:
	IdentityDictionary:   uses identity test (== rather than =)
"
| b x y sum max aDx |
x _ Dictionary new.							"allocate collection"
x add: #a->4; add: #b->3; add: #c->1; add: #d->2; yourself.	"add element to collection"
x at: #e put: 3.									"set element at index; see NOTE"
b _ x isEmpty.									"test if empty"
y _ x size.										"number of elements"
y _ x at: #a ifAbsent: [].						"retrieve element at index"
y _ x keyAtValue: 3 ifAbsent: [].				"retrieve key for given value with error block"
y _ x removeKey: #e ifAbsent: [].				"remove element from collection"
b _ x includes: 3.								"test if element is in values collection"
b _ x includesKey: #a.							"test if element is in keys collection"
y _ x occurrencesOf: 3.						"number of times object in collection"
y _ x keys.										"set of keys"
y _ x values.									"bag of values"
x do: [:a | Transcript show: a printString; newLine].	"iterate over the values collection"
x keysDo: [:a | Transcript show: a printString; newLine].	"iterate over the keys collection"
x associationsDo: [:a | Transcript show: a printString; newLine].	"iterate over the associations"
x keysAndValuesDo: [:aKey :aValue | Transcript	"iterate over keys and values"
   show: aKey printString; space;
   show: aValue printString; newLine].
b _ x allSatisfy: [:a | (a >= 1) & (a <= 4)].		"test if all elements meet condition"
y _ x select: [:a | a > 2].						"return collection of elements that pass test"
y _ x reject: [:a | a < 2].						"return collection of elements that fail test"
y _ x collect: [:a | a + a].						"transform each element for new collection"
y _ x detect: [:a | a > 3] ifNone: [].				"return first element that passes test"
sum _ 0. x do: [:a | sum _ sum + a]. sum.		"sum elements"
sum _ x inject: 0 into: [:a :c | a + c].			"sum elements"
max _ x inject: 0 into: [:a :c | (a > c)			"find max element in collection"
   ifTrue: [a]
   ifFalse: [c]].
y _ x asArray.									"convert to array"
y _ x asOrderedCollection.					"convert to ordered collection"
y _ x asSortedCollection.						"convert to sorted collection"
y _ x asBag.									"convert to bag collection"
y _ x asSet.									"convert to set collection"

Smalltalk at: #CMRGlobal put: ''CMR entry''.	"put global in Smalltalk Dictionary"
x _ Smalltalk at: #CMRGlobal.				"read global from Smalltalk Dictionary"
Transcript show: (CMRGlobal printString).	"entries are directly accessible by name"
Smalltalk keys do: [ :k |						"print out all classes"
   ((Smalltalk at: k) isKindOf: Class)
      ifFalse: [Transcript show: k printString; newLine]].
aDx _ Dictionary new.						"set up user defined dictionary"
aDx at: #MyVar1 put: ''hello1''.					"put entry in dictionary; see NOTE"
aDx add: #MyVar2->''hello2''.					"add entry to dictionary use key->value combo; see NOTE"
aDx size.		"dictionary size"
aDx keys do: [ :k |								"print out keys in dictionary"
   Transcript show: k printString; newLine].
aDx values do: [ :k |								"print out values in dictionary"
   Transcript show: k printString; newLine].
aDx keysAndValuesDo: [:aKey :aValue |			"print out keys and values"
   Transcript
      show: aKey printString;
      space;
      show: aValue printString;
      newLine].
aDx associationsDo: [:aKeyValue |				"another iterator for printing key values"
   Transcript show: aKeyValue printString; newLine].
Smalltalk removeKey: #CMRGlobal ifAbsent: [].	"remove entry from Smalltalk dictionary"

"NOTE: methods in subclasses of Collection, of which Dictionary is one, answer
 the ARGUMENT instead of the resulting collection. Examples of such 
 methods are #add: and #at:put:. Sending #yourself to the collection
 will answer the collection instead of the argument."

'! !

!TerseGuideHelp class methodsFor: 'accessing' stamp: 'dhn 2/25/2015 19:54'!
display: aSelector
	"Open a Workspace on the text in aSelector"
	| window |
	window _ Workspace new.
	window 	contents: (self perform: aSelector).
	window	openLabel: ('Terse Guide to ', (self class firstCommentAt: aSelector))! !

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

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/25/2015 18:00'!
fileStream
	"FileStream"
	^
'| b x ios |
ios _ FileStream newFileNamed: ''ios.txt''.
ios nextPut: $H; cr.
ios nextPutAll: ''Hello File''; cr.
''Hello File'' printOn: ios.
''Hello File'' storeOn: ios.
ios close.

ios _ FileStream oldFileNamed: ''ios.txt''.
[(x _ ios nextLine) notNil]
   whileTrue: [Transcript show: x; cr].
ios position: 3.
x _ ios position.
x _ ios next.
x _ ios peek.
b _ ios atEnd.
ios close.

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 14:38'!
internalStream
	"Internal Stream"
	^
'| b x ios |
ios _ ReadStream on: ''Hello read stream''.
ios _ ReadStream on: ''Hello read stream'' from: 1 to: 5.
[(x _ ios nextLine) notNil]
   whileTrue: [Transcript show: x; cr].
ios position: 3.
ios position.
x _ ios next.
x _ ios peek.
x _ ios contents.
b _ ios atEnd.

ios _ ReadWriteStream on: ''Hello read stream''.
ios _ ReadWriteStream on: ''Hello read stream'' from: 1 to: 5.
ios _ ReadWriteStream with: ''Hello read stream''.
ios _ ReadWriteStream with: ''Hello read stream'' from: 1 to: 10.
ios position: 0.
[(x _ ios nextLine) notNil]
   whileTrue: [Transcript show: x; cr].
ios position: 6.
ios position.
ios nextPutAll: ''Chris''.
x _ ios next.
x _ ios peek.
x _ ios contents.
b _ ios atEnd.

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/25/2015 22:03'!
interval
	"Interval"
	^
'| b x y sum max |
x _ Interval from: 5 to: 10.					"create interval object"
x _ 5 to: 10.
x _ Interval from: 5 to: 10 by: 2.				"create interval object with specified increment"
x _ 5 to: 10 by: 2.
b _ x isEmpty.									"test if empty"
y _ x size.										"number of elements"
x includes: 9.									"test if element is in collection"
x do: [:k | Transcript show: k printString; newLine].	"iterate over interval"
b _ x allSatisfy: [:a | (a >= 1) & (a <= 4)].		"test if all elements meet condition"
y _ x select: [:a | a > 7].						"return collection of elements that pass test"
y _ x reject: [:a | a < 2].						"return collection of elements that fail test"
y _ x collect: [:a | a + a].						"transform each element for new collection"
y _ x detect: [:a | a > 3] ifNone: [].				"return first element that passes test"
y _ x findFirst: [:a | a > 6].						"find position of first element that passes test"
sum _ 0. x do: [:a | sum _ sum + a]. sum.		"sum elements"
sum _ 0. 1 to: (x size) do: [:a | sum _ sum + (x at: a)].	"sum elements"
sum _ x inject: 0 into: [:a :c | a + c].			"sum elements"
max _ x inject: 0 into: [:a :c | (a > c)			"find max element in collection"
   ifTrue: [a]
   ifFalse: [c]].
y _ x asArray.									"convert to array"
y _ x asOrderedCollection.					"convert to ordered collection"
y _ x asSortedCollection.						"convert to sorted collection"
y _ x asBag.									"convert to bag collection"
y _ x asSet.									"convert to set collection"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 14:39'!
introduction
	"General Introduction"
	^
'"
	Allowable characters:
		- a-z
		- A-Z
		- 0-9
		- .+/\*~<>@%|&?
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
	Period (.) is the statement seperator"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 14:39'!
iterationStatement
	"Iterations"
	^
'| x y |
x _ 4. y _ 1.
[x > 0] whileTrue: [x _ x - 1. y _ y * 2].	"while true loop"
[x >= 4] whileFalse: [x _ x + 1. y _ y * 2].	"while false loop"
x timesRepeat: [y _ y * 2].					"times repeat loop (i _ 1 to x)"
1 to: x do: [:a | y _ y * 2].					"for loop"
1 to: x by: 2 do: [:a | y _ y / 2].				"for loop with specified increment"
#(5 4 3) do: [:a | x _ x + a].				"iterate over array elements"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 14:39'!
metaclass
	"Class / Metaclass"
	^
'| b x |
x _ String name.						"class name"
x _ String category.					"organization category"
x _ String comment.					"class comment"
x _ String kindOfSubclass.			"subclass type - subclass: variableSubclass, etc"
x _ String definition.					"class definition"
x _ String instVarNames.				"immediate instance variable names"
x _ String allInstVarNames.			"accumulated instance variable names"
x _ String classVarNames.			"immediate class variable names"
x _ String allClassVarNames.			"accumulated class variable names"
x _ String sharedPools.				"immediate dictionaries used as shared pools"
x _ String allSharedPools.				"accumulated dictionaries used as shared pools"
x _ String selectors.					"message selectors for class"
x _ String sourceCodeAt: #indexOf:.	"source code for specified method"
x _ String allInstances.				"collection of all instances of class"
x _ String superclass.				"immediate superclass"
x _ String allSuperclasses.			"accumulated superclasses"
x _ String withAllSuperclasses.		"receiver class and accumulated superclasses"
x _ String subclasses.				"immediate subclasses"
x _ String allSubclasses.				"accumulated subclasses"
x _ String withAllSubclasses.			"receiver class and accumulated subclasses"
b _ String instSize.					"number of named instance variables"
b _ String isFixed.					"true if no indexed instance variables"
b _ String isVariable.					"true if has indexed instance variables"
b _ String isPointers.					"true if index instance vars contain objects"
b _ String isBits.						"true if index instance vars contain bytes/words"
b _ String isBytes.					"true if index instance vars contain bytes"
b _ String isWords.					"true if index instance vars contain words"
Object withAllSubclasses size.		"get total number of class entries"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 14:40'!
methodCall
	"Method Call"
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
x _ 2 raisedTo: 10.							"keyword message"
x _ 194 * 9.								"binary message"
Transcript show: (194 * 9) printString; cr.	"combination (chaining)"
x _ 2 perform: #sqrt.						"indirect method invocation"
Transcript								"cascading - send multiple messages to receiver"
   show: ''hello '';
   show: ''world'';
   cr.
x _ 3 + 2; * 100.							"result=300. Sends message to same receiver (3)"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 14:40'!
misc
	"Miscellaneous"
	^
'| x |
Smalltalk condenseChanges.					"compress the change file"
x _ FillInTheBlank request: ''Prompt Me''.		"prompt user for input"
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/25/2015 19:53'!
orderedCollection
	"OrderedCollection"
	^
'| b x y sum max |
x _ OrderedCollection with: 4 with: 3 with: 2 with: 1.	"create collection with up to 4 elements"
x _ OrderedCollection new.					"allocate collection"
x add: 3; add: 2; add: 1; add: 4; yourself.		"add element to collection"
y _ x addFirst: 5.								"add element at beginning of collection"
y _ x removeFirst.								"remove first element in collection"
y _ x addLast: 6.								"add element at end of collection"
y _ x removeLast.								"remove last element in collection"
y _ x addAll: #(7 8 9).							"add elements to the collection and answer the added elements"
y _ x removeAll: #(7 8 9).						"remove elements from the collection and answer the removed elements"
x at: 2 put: 3.									"set element at index"
y _ x remove: 5 ifAbsent: [].					"remove element from collection"
b _ x isEmpty.									"test if empty"
y _ x size.										"number of elements"
y _ x at: 2.										"retrieve element at index"
y _ x first.										"retrieve first element in collection"
y _ x last.										"retrieve last element in collection"
b _ x includes: 5.								"test if element is in collection"
y _ x copyFrom: 2 to: 3.						"subcollection"
y _ x indexOf: 3 ifAbsent: [0].					"first position of element within collection"
y _ x occurrencesOf: 3.						"number of times object in collection"
x do: [:a | Transcript show: a printString; newLine].	"iterate over the collection"
b _ x allSatisfy: [:a | (a >= 1) & (a <= 4)].		"test if all elements meet condition"
y _ x select: [:a | a > 2].						"return collection of elements that pass test"
y _ x reject: [:a | a < 2].						"return collection of elements that fail test"
y _ x collect: [:a | a + a].						"transform each element for new collection"
y _ x detect: [:a | a > 3] ifNone: [].				"return first element that passes test"
y _ x findFirst: [:a | a < 2].						"find position of first element that passes test"
sum _ 0. x do: [:a | sum _ sum + a]. sum.		"sum elements"
sum _ 0. 1 to: (x size) do: [:a | sum _ sum + (x at: a)].	"sum elements"
sum _ x inject: 0 into: [:a :c | a + c].			"sum elements"
max _ x inject: 0 into: [:a :c | (a > c)			"find max element in collection"
   ifTrue: [a]
   ifFalse: [c]].
y _ x shuffled.									"randomly shuffle collection"
y _ x asArray.									"convert to array"
y _ x asOrderedCollection.					"convert to ordered collection"
y _ x asSortedCollection.						"convert to sorted collection"
y _ x asBag.									"convert to bag collection"
y _ x asSet.									"convert to set collection"

'! !

!TerseGuideHelp class methodsFor: 'accessing' stamp: 'dhn 2/24/2015 17:01'!
pages

	^ #( introduction transcript assignment constants boolean arithmetic
		bitwise conversion block methodCall conditionalStatement
		iterationStatement character symbol string array orderedCollection
		sortedCollection bag set interval association dictionary internalStream
		fileStream date time point rectangle pen dynamic metaclass debugging
		misc )! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 14:45'!
pen
	"Pen"
	^
'| myPen |
Display restoreAfter: [
   Display fillWhite.

myPen _ Pen new.								"get graphic pen"
myPen squareNib: 1.							"set shape and size of the nib"
myPen color: (Color blue).						"set pen color"
myPen home.									"position pen at center of display"
myPen up.										"makes nib unable to draw"
myPen down.									"enable the nib to draw"
myPen north.									"points direction towards top"
myPen turn: -180.								"add specified degrees to direction"
myPen direction.								"get current angle of pen"
myPen go: 50.									"move pen specified number of pixels"
myPen location.									"get the pen position"
myPen goto: 200@200.							"move to specified point"
myPen place: 250@250.						"move to specified point without drawing"
myPen print: ''Hello World'' withFont: (TextStyle default fontAt: 1).
Display extent.								"get display width@height"
Display width.									"get display width"
Display height.								"get display height"
]
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 14:49'!
point
	"Point"
	^
'| x y |
x _ 200@100.									"obtain a new point"
y _ x x.										"x coordinate"
y _ x y.										"y coordinate"
x _ 200@100 negated.						"negates x and y"
x _ (-200@ -100) abs.							"absolute value of x and y"
x _ (200.5@100.5) rounded.					"round x and y"
x _ (200.5@100.5) truncated.					"truncate x and y"
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

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 14:50'!
rectangle
	"Rectangle"
	^
'Rectangle fromUser.
'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/25/2015 20:07'!
set
	"Set"
	^
'| b x y sum max |
x _ Set with: 4 with: 3 with: 2 with: 1.			"create collection with up to 4 elements"
x _ Set new.									"allocate collection"
x add: 4; add: 3; add: 1; add: 2; yourself.		"add element to collection"
y _ x addAll: #(7 8 9).							"add elements to the collection and answer the added elements"
y _ x removeAll: #(7 8 9).						"remove elements from the collection and answer the removed elements"
y _ x remove: 4 ifAbsent: [].					"remove element from collection"
b _ x isEmpty.									"test if empty"
y _ x size.										"number of elements"
x includes: 4.									"test if element is in collection"
x do: [:a | Transcript show: a printString; cr].	"iterate over the collection"
b _ x allSatisfy: [:a | (a >= 1) & (a <= 4)].		"test if all elements meet condition"
y _ x select: [:a | a > 2].						"return collection of elements that pass test"
y _ x reject: [:a | a < 2].						"return collection of elements that fail test"
y _ x collect: [:a | a + a].						"transform each element for new collection"
y _ x detect: [:a | a > 3] ifNone: [].				"return first element that passes test"
sum _ 0. x do: [:a | sum _ sum + a]. sum.		"sum elements"
sum _ x inject: 0 into: [:a :c | a + c].			"sum elements"
max _ x inject: 0 into: [:a :c | (a > c)			"find max element in collection"
   ifTrue: [a]
   ifFalse: [c]].
y _ x asArray.									"convert to array"
y _ x asOrderedCollection.					"convert to ordered collection"
y _ x asSortedCollection.						"convert to sorted collection"
y _ x asBag.									"convert to bag collection"
y _ x asSet.									"convert to set collection"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/25/2015 20:07'!
sortedCollection
	"SortedCollection"
	^
'| b x y sum max |
x _ SortedCollection with: 4 with: 3 with: 2 with: 1.	"create collection with up to 4 elements"
x _ SortedCollection new.						"allocate collection"
x _ SortedCollection sortBlock: [:a :c | a > c].	"set sort criteria"
x add: 3; add: 2; add: 1; add: 4; yourself.			"add element to collection"
"y _ x addFirst: 5."									"add element at beginning of collection"
y _ x removeFirst.									"remove first element in collection"
y _ x addLast: 6.									"add element at end of collection"
y _ x removeLast.									"remove last element in collection"
y _ x addAll: #(7 8 9).								"add elements to the collection and answer the added elements"
y _ x removeAll: #(7 8 9).							"remove elements from the collection and answer the removed elements"
y _ x remove: 5 ifAbsent: [].						"remove element from collection"
b _ x isEmpty.										"test if empty"
y _ x size.											"number of elements"
y _ x at: 2.											"retrieve element at index"
y _ x first.											"retrieve first element in collection"
y _ x last.											"retrieve last element in collection"
b _ x includes: 4.									"test if element is in collection"
y _ x copyFrom: 2 to: 3.							"subcollection"
y _ x indexOf: 3 ifAbsent: [0].						"first position of element within collection"
y _ x occurrencesOf: 3.							"number of times object in collection"
x do: [:a | Transcript show: a printString; cr].		"iterate over the collection"
b _ x allSatisfy: [:a | (a >= 1) & (a <= 4)].			"test if all elements meet condition"
y _ x select: [:a | a > 2].							"return collection of elements that pass test"
y _ x reject: [:a | a < 2].							"return collection of elements that fail test"
y _ x collect: [:a | a + a].							"transform each element for new collection"
y _ x detect: [:a | a > 3] ifNone: [].					"return first element that passes test"
y _ x findFirst: [:a | a < 3].							"find position of first element that passes test"
sum _ 0. x do: [:a | sum _ sum + a]. sum.			"sum elements"
sum _ 0. 1 to: (x size) do: [:a | sum _ sum + (x at: a)].	"sum elements"
sum _ x inject: 0 into: [:a :c | a + c].				"sum elements"
max _ x inject: 0 into: [:a :c | (a > c)				"find max element in collection"
   ifTrue: [a]
   ifFalse: [c]].
y _ x asArray.										"convert to array"
y _ x asOrderedCollection.						"convert to ordered collection"
y _ x asSortedCollection.							"convert to sorted collection"
y _ x asBag.										"convert to bag collection"
y _ x asSet.										"convert to set collection"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 16:08'!
string
	"String"
	^
'| b x y |
x _ ''This is a string''.						"string assignment"
x _ ''String'', ''Concatenation''.				"string concatenation"
b _ x isEmpty.								"test if string is empty"
y _ x size.									"string size"
y _ x at: 2.									"char at location"
y _ x copyFrom: 2 to: 4.					"substring"
y _ x indexOf: $a ifAbsent: [0].				"first position of character within string"
x _ String new: 4.							"allocate string object"
x											"set string elements"
   at: 1 put: $a;
   at: 2 put: $b;
   at: 3 put: $c;
   at: 4 put: $e.
x _ String with: $a with: $b with: $c with: $d.	"set up to 4 elements at a time"
x do: [:a | Transcript show: a printString; cr].	"iterate over the string"
b _ x allSatisfy: [:a | (a >= $a) & (a <= $z)].	"test if all elements meet condition"
y _ x select: [:a | a > $a].					"return all elements that meet condition"
y _ x asSymbol.							"convert string to symbol"
y _ x asArray.								"convert string to array"
x _ ''ABCD'' asByteArray.					"convert string to byte array"
y _ x asOrderedCollection.				"convert string to ordered collection"
y _ x asSortedCollection.					"convert string to sorted collection"
y _ x asBag.								"convert string to bag collection"
y _ x asSet.								"convert string to set collection"
y _ x shuffled.								"randomly shuffle string"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 16:13'!
symbol
	"Symbol"
	^
'| b x y |
x _ #Hello.								"symbol assignment"
y _ ''String'', ''Concatenation''.				"symbol concatenation (result is string)"
b _ x isEmpty.								"test if symbol is empty"
y _ x size.									"string size"
y _ x at: 2.									"char at location"
y _ x copyFrom: 2 to: 4.					"substring"
y _ x indexOf: $e ifAbsent: [0].				"first position of character within string"
x do: [:a | Transcript show: a printString; cr].	"iterate over the string"
b _ x allSatisfy: [:a | (a >= $a) & (a <= $z)].	"test if all elements meet condition"
y _ x select: [:a | a > $a].					"return all elements that meet condition"
y _ x asString.								"convert symbol to string"
y _ x asText.								"convert symbol to text"
y _ x asArray.								"convert symbol to array"
y _ x asOrderedCollection.				"convert symbol to ordered collection"
y _ x asSortedCollection.					"convert symbol to sorted collection"
y _ x asBag.								"convert symbol to bag collection"
y _ x asSet.								"convert symbol to set collection"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 17:50'!
time
	"Time"
	^
'| x y b |
x _ Time now.									"answer the current time"
x _ Time dateAndTimeNow.					"answer the current date and time"
x _ Time readFrom: ''3:47:26 pm''.				"answer the time from formatted string"
x _ Time fromSeconds: (60 * 60 * 4).			"answer the time from elapsed time from midnight"
y _ Time millisecondClockValue.				"answer the milliseconds since midnight"
y _ Time localSecondClock.					"total seconds since 1/1/1901"
y _ x addTime: (Time now).					"add time to time object"
y _ x subtractTime: (Time now).				"subtract time to time object"
y _ x asSeconds.								"convert time to seconds"
x _ Time millisecondsToRun: [					"timing facility"
   1 to: 1000 do: [:index | y _ 3.14 * index]].
b _ (x <= Time now).							"comparison"

'! !

!TerseGuideHelp class methodsFor: 'pages' stamp: 'dhn 2/24/2015 16:20'!
transcript
	"Transcript"
	^
'Transcript clear.						"clear to transcript window"
Transcript show: ''Hello World''.		"output string in transcript window"
Transcript nextPutAll: ''Hello World''.	"output string in transcript window"
Transcript nextPut: $A.				"output character in transcript window"
Transcript space.						"output space character in transcript window"
Transcript tab.						"output tab character in transcript window"
Transcript cr.							"carriage return / linefeed"
''Hello'' printOn: Transcript.			"append print string into the window"
''Hello'' storeOn: Transcript.			"append store string into the window"
Transcript endEntry.					"flush the output buffer"

'! !
