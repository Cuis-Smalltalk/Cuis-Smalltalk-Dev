'From Cuis 5.0 [latest update: #4691] on 26 July 2021 at 9:08:22 am'!

!Boolean methodsFor: 'logical operations' stamp: 'ST-80 5/31/1983 9:10:35'!
& aBoolean 
	"Evaluating conjunction -- Evaluate the argument.  Then answer true if both the
	receiver and the argument are true."
	self subclassResponsibility! !

!Boolean methodsFor: 'logical operations' stamp: 'ST-80 5/31/1983 9:10:35'!
| aBoolean 
	"Evaluating disjunction (OR) -- Evaluate the argument.  Then answer true if
	either the receiver or the argument is true."
	self subclassResponsibility! !

!Boolean methodsFor: 'logical operations' stamp: 'ST-80 5/31/1983 9:10:35'!
not
	"Negation-- answer true if the receiver is false, answer false if the receiver is true."
	self subclassResponsibility! !

!Boolean methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
and: alternativeBlock 
	"Nonevaluating conjunction -- if the receiver is true, answer the value of
	the argument, alternativeBlock; otherwise answer false without evaluating the
	argument."
	self subclassResponsibility! !

!Boolean methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
ifFalse: alternativeBlock 
	"If the receiver is true (i.e., the condition is true), then the value is the true
	alternative, which is nil.  Otherwise answer the result of evaluating the argument,
	alternativeBlock.  Create an error if the receiver is nonBoolean.  Execution does not
	actually reach here because the expression is compiled in-line."
	self subclassResponsibility! !

!Boolean methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock 
	"Same as ifTrue:ifFalse:"
	self subclassResponsibility! !

!Boolean methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
ifTrue: alternativeBlock 
	"If the receiver is false (i.e., the condition is false), then the value is the false
	alternative, which is nil.  Otherwise answer the result of evaluating the argument,
	alternativeBlock.  Create an error if the receiver is nonBoolean.  Execution does not
	actually reach here because the expression is compiled in-line."
	self subclassResponsibility! !

!Boolean methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock
	"If the receiver is true (i.e., the condition is true), then answer the value of the
	argument trueAlternativeBlock.  If the receiver is false, answer the result of
	evaluating the argument falseAlternativeBlock.  If the receiver is a nonBoolean
	then create an error message.  Execution does not actually reach here because the
	expression is compiled in-line."
	self subclassResponsibility! !

!Boolean methodsFor: 'controlling' stamp: 'ST-80 5/31/1983 9:10:35'!
or: alternativeBlock 
	"Nonevaluating disjunction -- if the receiver is false, answer the value of
	the argument, alternativeBlock; otherwise answer true without evaluating the
	argument."
	self subclassResponsibility! !

!Boolean methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
storeOn: aStream
	self printOn: aStream! !


!False methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
printOn: aStream 
	"Print false."
	aStream nextPutAll: 'false'! !


!True methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
printOn: aStream 
	aStream nextPutAll: 'true'! !


!UndefinedObject methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
printOn: aStream 
	aStream nextPutAll: 'nil'! !

!UndefinedObject methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
storeOn: aStream 
	aStream nextPutAll: 'nil'! !

!UndefinedObject methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
isNil
	^true! !

!UndefinedObject methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
notNil
	^false! !

!UndefinedObject methodsFor: 'dependents access' stamp: 'ST-80 5/31/1983 9:10:35'!
addDependent: ignored
	self error: 'Nil should not have dependents'! !


!Number methodsFor: 'arithmetic' stamp: 'ST-80 5/31/1983 9:10:35'!
* aNumber 
	"Answer the result of multiplying the receiver by aNumber."
	self subclassResponsibility! !

!Number methodsFor: 'arithmetic' stamp: 'ST-80 5/31/1983 9:10:35'!
+ aNumber 
	"Answer the sum of the receiver and aNumber."
	self subclassResponsibility! !

!Number methodsFor: 'arithmetic' stamp: 'ST-80 5/31/1983 9:10:35'!
- aNumber 
	"Answer the difference between the receiver and aNumber."
	self subclassResponsibility! !

!Number methodsFor: 'arithmetic' stamp: 'ST-80 5/31/1983 9:10:35'!
/ aNumber 
	"Answer the result of dividing receiver by aNumber."
	self subclassResponsibility! !

!Number methodsFor: 'arithmetic' stamp: 'ST-80 5/31/1983 9:10:35'!
abs
	"Answer a Number that is the absolute value (positive magnitude) of the receiver."

	self < 0
		ifTrue: [^self negated]
		ifFalse: [^self]! !

!Number methodsFor: 'arithmetic' stamp: 'ST-80 5/31/1983 9:10:35'!
negated
	"Answer a Number that is the negation of the receiver."
	^0 - self! !

!Number methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
@ y 
	"Answer a new Point whose x value is the receiver and whose y value is the 
	argument.  Optional.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 18>
	^Point x: self y: y! !

!Number methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asInteger
	"Answer an integer nearest the receiver toward zero."
	^self truncated! !

!Number methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asPoint
	"Answer a new Point with the receiver as both coordinates; 
	often used to supply the same value in two dimensions, as with
	symmetrical gridding or scaling."

	^self @ self! !

!Number methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
degreesToRadians
	"The receiver is assumed to represent degrees.  Answer the
	conversion to radians."
	^self asFloat degreesToRadians! !

!Number methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
radiansToDegrees
	"The receiver is assumed to represent radians.  Answer the
	conversion to degrees."
	^self asFloat radiansToDegrees! !

!Number methodsFor: 'intervals' stamp: 'ST-80 5/31/1983 9:10:35'!
to: stop
	"Answer an Interval from the receiver up to the argument, stop,
	incrementing by 1."

	^Interval from: self to: stop by: 1! !

!Number methodsFor: 'intervals' stamp: 'ST-80 5/31/1983 9:10:35'!
to: stop by: step
	"Answer an Interval from the receiver up to the argument, stop,
	incrementing by step."

	^Interval from: self to: stop by: step! !

!Number methodsFor: 'mathematical functions' stamp: 'ST-80 5/31/1983 9:10:35'!
exp
	"Answer the exponential of the receiver as a floating point number."
	^self asFloat exp! !

!Number methodsFor: 'mathematical functions' stamp: 'ST-80 5/31/1983 9:10:35'!
squared
	"Answer the receiver multipled by itself."
	^self * self! !

!Number methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
odd
	"Answer whether the receiver is an odd number."
	^self even == false! !

!Number methodsFor: 'truncation and round off' stamp: 'ST-80 5/31/1983 9:10:35'!
truncated
	"Answer an integer nearest the receiver toward zero."
	^self quo: 1! !


!Float methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
degreesToRadians
	^self * RadiansPerDegree! !

!Float methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
radiansToDegrees
	^self / RadiansPerDegree! !


!Float class methodsFor: 'constants' stamp: 'ST-80 5/31/1983 9:10:35'!
pi
	"Answer the constant, Pi."
	^Pi! !


!Fraction methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asFraction	
	"Answer with the receiver itself."
	^self! !

!Fraction methodsFor: 'truncation and round off' stamp: 'ST-80 5/31/1983 9:10:35'!
truncated
	^numerator quo: denominator! !

!Fraction methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
denominator
	^denominator! !

!Fraction methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
numerator
	^numerator! !

!Fraction methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
reduced
	| gcd numer denom |
	numerator = 0 ifTrue: [^0].
	gcd _ numerator gcd: denominator.
	numer _ numerator // gcd.
	denom _ denominator // gcd.
	denom = 1 ifTrue: [^numer].
	^Fraction numerator: numer denominator: denom! !


!Integer methodsFor: 'bit manipulation' stamp: 'ST-80 5/31/1983 9:10:35'!
allMask: mask 
	"Treat the argument as a bit mask.  Answer true if all of the
	bits that are 1 in the argument are 1 in the receiver."

	^mask = (self bitAnd: mask)! !

!Integer methodsFor: 'bit manipulation' stamp: 'ST-80 5/31/1983 9:10:35'!
anyMask: mask 
	"Treat the argument as a bit mask.  Answer true if any of the
	bits that are 1 in the argument are 1 in the receiver."

	^0 ~= (self bitAnd: mask)! !

!Integer methodsFor: 'bit manipulation' stamp: 'ST-80 5/31/1983 9:10:35'!
noMask: mask 
	"Treat the argument as a bit mask.  Answer true if none of the bits
	that are 1 in the argument are 1 in the receiver."

	^0 = (self bitAnd: mask)! !

!Integer methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asFraction
	"Answer with a Fraction that represents the receiver."

	^Fraction numerator: self denominator: 1! !

!Integer methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asInteger
	"Answer with the receiver itself."

	^self! !

!Integer methodsFor: 'enumerating' stamp: 'ST-80 5/31/1983 9:10:35'!
timesRepeat: aBlock 
	"Evaluate the argument, aBlock, the number of times represented by
	the receiver."

	| count |
	count _ 1.
	[count <= self]
		whileTrue: 
			[aBlock value.
			count _ count + 1]! !

!Integer methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
isLiteral
	^true! !

!Integer methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
growby: n 
	^self growto: self digitLength + n! !

!Integer methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
growto: n 
	^self copyto: (self species new: n)! !


!Integer class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
new: length neg: neg 
	"Answer an instance of a large integer whose size is length.  neg is a flag determining
	whether the integer is negative or not."

	neg 
		ifTrue: [^LargeNegativeInteger new: length]
		ifFalse: [^LargePositiveInteger new: length]! !


!LargeNegativeInteger methodsFor: 'arithmetic' stamp: 'ST-80 5/31/1983 9:10:35'!
abs
	^self negated! !

!LargeNegativeInteger methodsFor: 'arithmetic' stamp: 'ST-80 5/31/1983 9:10:35'!
negated
	^self copyto: (LargePositiveInteger new: self digitLength)! !


!SmallInteger methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
even
	^(self bitAnd: 1) = 0! !

!SmallInteger methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
odd
	^(self bitAnd: 1) = 1! !

!SmallInteger methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
< aNumber 
	"Compare the receiver with the argument and answer with true if the receiver is less 
	than the argument.  Otherwise answer false.  Fail if the argument is not a 
	SmallInteger.  Essential.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 3>
	^super < aNumber! !

!SmallInteger methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
<= aNumber 
	"Compare the receiver with the argument and answer true if the receiver is less 
	than or equal to the argument.  Otherwise answer false.  Fail if the argument is 
	not a SmallInteger.  Optional.  No Lookup.  See Object documentation 
	whatIsAPrimitive. "

	<primitive: 5>
	^super <= aNumber! !

!SmallInteger methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
= aNumber 
	"Compare the receiver with the argument and answer true if the receiver is 
	equal to the argument.  Otherwise answer false.  Fail if the argument is not a 
	SmallInteger.  Essential.  No Lookup.  See Object documentation 
	whatIsAPrimitive. "

	<primitive: 7>
	^super = aNumber! !

!SmallInteger methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
> aNumber 
	"Compare the receiver with the argument and answer true if the receiver is 
	greater than the argument.  Otherwise answer false.  Fail if the argument is not a 
	SmallInteger.  Essential.  No Lookup.  See Object documentation whatIsAPrimitive."

	<primitive: 4>
	^super > aNumber! !

!SmallInteger methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
>= aNumber 
	"Compare the receiver with the argument and answer true if the receiver is 
	greater than or equal to the argument.  Otherwise answer false.  Fail if the 
	argument is not a SmallInteger.  Optional.  No Lookup.  See Object 
	documentation whatIsAPrimitive."

	<primitive: 6>
	^super >= aNumber! !

!SmallInteger methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
~= aNumber 
	"Compare the receiver with the argument and answer true if the receiver is not 
	equal to the argument.  Otherwise answer false.  Fail if the argument is not a 
	SmallInteger.  Essential.  No Lookup.  See Object documentation 
	whatIsAPrimitive. "

	<primitive: 8>
	^super ~= aNumber! !


!LookupKey methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
key
	"Answer the lookup key of the receiver."
	^key! !

!LookupKey methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
key: anObject 
	"Store the argument, anObject, as the lookup key of the receiver."
	key _ anObject! !

!LookupKey methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
< aLookupKey 
	^key < aLookupKey key! !


!LookupKey class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
key: aKey 
	"Answer a new instance of me with the argument as the lookup up."
	^self new key: aKey! !


!Association methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
value
	"Answer the value of the receiver."
	^value! !

!Association methodsFor: 'accessing' stamp: 'ST-80 5/31/1983 9:10:35'!
value: anObject 
	"Store the argument, anObject, as the value of the receiver."
	value _ anObject! !


!Association class methodsFor: 'instance creation' stamp: 'ST-80 5/31/1983 9:10:35'!
key: newKey value: newValue
	"Answer a new instance of me with the arguments as the key and
	value of the association."
	^(super key: newKey) value: newValue! !


!MessageTally methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
< aMessageTally 
	^tally > aMessageTally tally! !

!MessageTally methodsFor: 'comparing' stamp: 'ST-80 5/31/1983 9:10:35'!
> aMessageTally 
	^tally < aMessageTally tally! !

!MessageTally methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
class: aClass method: aMethod 
	class _ aClass.
	method _ aMethod.
	tally _ 0.
	receivers _ Array new: 0! !

!MessageTally methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
method
	^method! !

!MessageTally methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
primitives: anInteger 
	tally _ anInteger.
	receivers _ nil! !

!MessageTally methodsFor: 'private' stamp: 'ST-80 5/31/1983 9:10:35'!
tally
	^tally! !


!Character methodsFor: 'testing' stamp: 'ST-80 5/31/1983 9:10:35'!
isAlphaNumeric
	"Answer whether the receiver is a letter or a digit."
	^self isLetter or: [self isDigit]! !

!Character methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
isLiteral
	^true! !

!Character methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
printOn: aStream 
	aStream nextPut: $$.
	aStream nextPut: self! !

!Character methodsFor: 'printing' stamp: 'ST-80 5/31/1983 9:10:35'!
storeOn: aStream
	"Character literals are preceded by '$'."
	aStream nextPut: $$; nextPut: self! !

!Character methodsFor: 'converting' stamp: 'ST-80 5/31/1983 9:10:35'!
asCharacter
	"Answer the receiver itself."
	^self! !


!Character class methodsFor: 'constants' stamp: 'ST-80 5/31/1983 9:10:35'!
characterTable
	"Answer the class variable in which unique Characters are stored."
	^CharacterTable! !


!MessageTally reorganize!
('collecting leaves' bump: bump:fromSender: into:fromSender: leavesInto:fromSender:)
('comparing' < = > hash isPrimitives sonsOver:)
('printing' fullPrintExactOn: leavesPrintExactOn: printOn: printSenderCountsOn: treePrintExactOn:tabs:thisTab:)
('reporting')
('tallying' bumpBy: tally:by: tallyPath:by:)
('private' class:method: copyWithTally: method primitives: tally)
!

