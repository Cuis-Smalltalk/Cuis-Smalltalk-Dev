'From Cuis 5.0 [latest update: #4695] on 26 July 2021 at 9:38:32 am'!

!Object methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
= anObject 
	"Answer true if the receiver and the argument represent the same object 
	and false otherwise. If = is redefined in any subclass, consider also
	redefining the message hash."

	^self == anObject! !

!Object methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
~= anObject 
	"Answer true if the receiver and the argument do not represent the same 
	object and false otherwise."

	^self = anObject == false! !

!Object methodsFor: 'error handling' stamp: 'ST-80 5/31/1983 9:10:35'!
shouldNotImplement
	"Announce that although the receiver inherits this message, it
	should not implement it."

	self error: 'This message is not appropriate for this object'! !

!Object methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
storeOn: aStream 
	"Append to the argument aStream a sequence of characters that is an expression 
	whose evaluation creates an object similar to the receiver."

	aStream nextPut: $(.
	self class isVariable
		ifTrue: [aStream nextPutAll: '(', self class name, ' basicNew: ';
					store: self basicSize;
					nextPutAll: ') ']
		ifFalse: [aStream nextPutAll: self class name, ' basicNew'].
	1 to: self class instSize do:
		[:i |
		aStream nextPutAll: ' instVarAt: ';
			store: i;
			nextPutAll: ' put: ';
			store: (self instVarAt: i);
			nextPut: $;].
	1 to: self basicSize do:
		[:i |
		aStream nextPutAll: ' basicAt: ';
			store: i;
			nextPutAll: ' put: ';
			store: (self basicAt: i);
			nextPut: $;].
	aStream nextPutAll: ' yourself)'! !


!Switch methodsFor: 'state' stamp: 'ST-80 5/31/1983 9:10:35'!
isOff
	"Answer whether the receiver is set off or not."
	^on not! !

!Switch methodsFor: 'state' stamp: 'ST-80 5/31/1983 9:10:35'!
isOn
	"Answer whether the receiver is set on or not."
	^on! !

!Switch methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
initializeOff
	on _ false. 
	onAction _ nil.
	offAction _ nil! !

!Switch methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
initializeOn
	on _ true. 
	onAction _ nil.
	offAction _ nil! !


!Switch class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
newOff
	"Answer an instance of me such that the on and off actions are set to nil
	('no action'), and the state is set to 'off'."

	^super new initializeOff! !

!Switch class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
newOn
	"Answer an instance of me such that the on and off actions are set to nil
	('no action'), and the state is set to 'on'."

	^super new initializeOn! !


!Behavior methodsFor: 'accessing instances and variables' stamp: 'ST-80 5/31/1983 9:10:35'!
someInstance
	"Answer the first instance of this receiver.  See Object nextInstance.  Fails
	if there are none.  Essential.  See Object documentation whatIsAPrimitive."

	<primitive: 77>
	^nil! !


!ClassDescription methodsFor: 'compiling' stamp: 'ST-80 5/31/1983 9:10:35'!
compile: code classified: heading 
	"Compile the argument, code, as source code in the context of the receiver and
	install the result in the receiver's method dictionary under the classification 
	indicated by the second argument, heading. nil is to be notified if an error occurs.
	The argument code is either a string or an object that converts to a string or a
	PositionableStream on an object that converts to a string."

	^self
		compile: code
		classified: heading
		notifying: nil! !

!ClassDescription methodsFor: 'copying' stamp: 'ST-80 5/31/1983 9:10:35'!
copyAll: selArray from: class 
	"Install all the methods found in the method dictionary of the second argument, class,
	as the receiver's methods.  Classify the messages under -as yet not classified-"

	self copyAll: selArray
		from: class
		classified: nil! !

!ClassDescription methodsFor: 'copying' stamp: 'ST-80 5/31/1983 9:10:35'!
copyAll: selArray from: class classified: cat 
	"Install all the methods found in the method dictionary of the second argument, class,
	as the receiver's methods.  Classify the messages under the third argument, cat."

	selArray do: 
		[:s | self copy: s
				from: class
				classified: cat]! !

!ClassDescription methodsFor: 'copying' stamp: 'ST-80 5/31/1983 9:10:35'!
copyAllCategoriesFrom: aClass 
	"Specify that the categories of messages for the receiver include all of those found
	in the class, aClass.  Install each of the messages found in these categories into the
	method dictionary of the receiver, classified under the appropriate categories."

	aClass organization categories do: [:cat | self copyCategory: cat from: aClass]! !

!ClassDescription methodsFor: 'copying' stamp: 'ST-80 5/31/1983 9:10:35'!
copyCategory: cat from: aClass classified: newCat 
	"Specify that one of the categories of messages for the receiver is the third argument,
	newCat.  Copy each message found in the category cat in class aClass into this
	new category."

	self copyAll: (aClass organization listAtCategoryNamed: cat)
		from: aClass
		classified: newCat! !

!ClassDescription methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
printOn: aStream 
	aStream nextPutAll: self name! !


!Metaclass methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
soleInstance
	"The receiver has only one instance.  Answer it."

	^thisClass! !

!Metaclass methodsFor: 'instance variables' stamp: 'ST-80 5/31/1983 9:10:35'!
removeInstVarName: aString 
	"Remove the argument, aString, as one of the receiver's instance variables."


	| newArray newString |
	(self instVarNames includes: aString)
		ifFalse: [self error: aString , ' is not one of my instance variables'].
	newArray _ self instVarNames copyWithout: aString.
	newString _ ''.
	newArray do: [:aString2 | newString _ aString2 , ' ' , newString].
	self instanceVariableNames: newString! !


!Magnitude methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
< aMagnitude 
	"Compare the receiver with the argument and answer with true if the 
	receiver is less than the argument. Otherwise answer false."

	^self subclassResponsibility! !

!Magnitude methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
<= aMagnitude 
	"Compare the receiver with the argument and answer with true if the 
	receiver is less than or equal to the argument. Otherwise answer false."

	^(self > aMagnitude) not! !

!Magnitude methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
> aMagnitude 
	"Compare the receiver with the argument and answer with true if the 
	receiver is greater than the argument. Otherwise answer false."

	^aMagnitude < self! !

!Magnitude methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
between: min and: max 
	"Answer whether the receiver is less than or equal to the argument, max,
	and greater than or equal to the argument, min."

	^self >= min and: [self <= max]! !

!Magnitude methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
max: aMagnitude 
	"Answer the receiver or the argument, whichever has the greater magnitude."

	self > aMagnitude
		ifTrue: [^self]
		ifFalse: [^aMagnitude]! !

!Magnitude methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
min: aMagnitude 
	"Answer the receiver or the argument, whichever has the lesser magnitude."

	self < aMagnitude
		ifTrue: [^self]
		ifFalse: [^aMagnitude]! !


!Float methodsFor: 'mathematical functions' stamp: 'ST-80 5/31/1983 9:10:35'!
arcCos
	"Answers with the angle in radians."

	^Halfpi - self arcSin! !

!Float methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asFloat
	"Answer with the receiver itself."
	^self! !

!Float methodsFor: 'truncation and round off' stamp: 'ST-80 5/31/1983 9:10:35'!
integerPart
	"Answer with a new Float whose value is the receiver's truncated value."

	^self - self fractionPart! !


!Fraction methodsFor: 'arithmetic' stamp: 'ST-80 5/31/1983 9:10:35'!
negated
	^Fraction numerator: numerator negated denominator: denominator! !


!Integer methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
even
	^((self digitAt: 1) bitAnd: 1) = 0! !

!Integer methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
isInteger
	^true! !


!Character methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asSymbol
	^Symbol internCharacter: self! !


!WriteStream methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
store: anObject 
	"Have anObject print on me for rereading."

	anObject storeOn: self! !

!WriteStream methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
on: aCollection 
	super on: aCollection.
	readLimit _ 0.
	writeLimit _ aCollection size! !

!WriteStream methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
on: aCollection from: firstIndex to: lastIndex 
	| len |
	collection _ aCollection.
	readLimit _ 
		writeLimit _ lastIndex > (len _ collection size)
						ifTrue: [len]
						ifFalse: [lastIndex].
	position _ firstIndex <= 1
				ifTrue: [0]
				ifFalse: [firstIndex - 1]! !

!WriteStream methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
with: aCollection 
	super on: aCollection.
	position _ readLimit _ writeLimit _ aCollection size! !


!Magnitude reorganize!
('comparing' < <= = > >= between:and: hash max: min: min:max:)
!

