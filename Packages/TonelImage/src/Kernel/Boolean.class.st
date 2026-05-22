"
Boolean is an abstract class defining the protocol for logic testing operations and conditional control structures for the logical values represented by the instances of its subclasses True and False.

Boolean redefines #new so no instances of Boolean can be created. It also redefines several messages in the 'copying' protocol to ensure that only one instance of each of its subclasses True (the global true, logical assertion) and False (the global false, logical negation) ever exist in the system.
"
Class {
	#name : 'Boolean',
	#superclass : 'Object',
	#category : 'Kernel-Objects',
	#package : 'Kernel',
	#tag : 'Objects'
}

{ #category : 'instance creation' }
Boolean class >> initializedInstance [
	^ nil
]

{ #category : 'instance creation' }
Boolean class >> new [
	self error: 'You may not create any more Booleans - this is two-valued logic'
]

{ #category : 'logical operations' }
Boolean >> & aBoolean  [
	"Evaluating conjunction -- Evaluate the argument.  Then answer true if both the
	receiver and the argument are true."
	self subclassResponsibility
]

{ #category : 'logical operations' }
Boolean >> | aBoolean  [
	"Evaluating disjunction (OR) -- Evaluate the argument.  Then answer true if
	either the receiver or the argument is true."
	self subclassResponsibility
]

{ #category : 'controlling' }
Boolean >> and: alternativeBlock  [
	"Nonevaluating conjunction -- if the receiver is true, answer the value of
	the argument, alternativeBlock; otherwise answer false without evaluating the
	argument."
	self subclassResponsibility
]

{ #category : 'controlling' }
Boolean >> and: block1 and: block2 [
	"Nonevaluating conjunction without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as false, then return false immediately,
		without evaluating any further blocks.
	If all return true, then return true."

	self subclassResponsibility
]

{ #category : 'controlling' }
Boolean >> and: block1 and: block2 and: block3 [
	"Nonevaluating conjunction without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as false, then return false immediately,
		without evaluating any further blocks.
	If all return true, then return true."

	self subclassResponsibility
]

{ #category : 'controlling' }
Boolean >> and: block1 and: block2 and: block3 and: block4 [
	"Nonevaluating conjunction without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as false, then return false immediately,
		without evaluating any further blocks.
	If all return true, then return true."

	self subclassResponsibility
]

{ #category : 'user interface support' }
Boolean >> asMenuItemTextPrefix [
	"Answer '<Yes>' or  '<No>' to prefix a menu item text with a check box. "

	^ self subclassResponsibility
]

{ #category : 'logical operations' }
Boolean >> eqv: aBoolean  [
	"Answer true if the receiver is equivalent to aBoolean."

	^self == aBoolean
]

{ #category : 'controlling' }
Boolean >> ifFalse: alternativeBlock  [
	"If the receiver is true (i.e., the condition is true), then the value is the true
	alternative, which is nil.  Otherwise answer the result of evaluating the argument,
	alternativeBlock.  Create an error if the receiver is nonBoolean.  Execution does not
	actually reach here because the expression is compiled in-line."
	self subclassResponsibility
]

{ #category : 'controlling' }
Boolean >> ifFalse: falseAlternativeBlock ifTrue: trueAlternativeBlock  [
	"Same as ifTrue:ifFalse:"
	self subclassResponsibility
]

{ #category : 'controlling' }
Boolean >> ifTrue: alternativeBlock  [
	"If the receiver is false (i.e., the condition is false), then the value is the false
	alternative, which is nil.  Otherwise answer the result of evaluating the argument,
	alternativeBlock.  Create an error if the receiver is nonBoolean.  Execution does not
	actually reach here because the expression is compiled in-line."
	self subclassResponsibility
]

{ #category : 'controlling' }
Boolean >> ifTrue: trueAlternativeBlock ifFalse: falseAlternativeBlock [
	"If the receiver is true (i.e., the condition is true), then answer the value of the
	argument trueAlternativeBlock.  If the receiver is false, answer the result of
	evaluating the argument falseAlternativeBlock.  If the receiver is a nonBoolean
	then create an error message.  Execution does not actually reach here because the
	expression is compiled in-line."
	self subclassResponsibility
]

{ #category : 'testing' }
Boolean >> is: aSymbol [
	^#Boolean = aSymbol or: [ super is: aSymbol ]
]

{ #category : 'printing' }
Boolean >> isLiteral  [
	^ true
]

{ #category : 'logical operations' }
Boolean >> not [
	"Negation-- answer true if the receiver is false, answer false if the receiver is true."
	self subclassResponsibility
]

{ #category : 'controlling' }
Boolean >> or: alternativeBlock  [
	"Nonevaluating disjunction -- if the receiver is false, answer the value of
	the argument, alternativeBlock; otherwise answer true without evaluating the
	argument."
	self subclassResponsibility
]

{ #category : 'controlling' }
Boolean >> or: block1 or: block2 [
	"Nonevaluating alternation without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as true, then return true immediately,
		without evaluating any further blocks.
	If all return false, then return false."

	self subclassResponsibility
]

{ #category : 'controlling' }
Boolean >> or: block1 or: block2 or: block3 [
	"Nonevaluating alternation without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as true, then return true immediately,
		without evaluating any further blocks.
	If all return false, then return false."

	self subclassResponsibility
]

{ #category : 'controlling' }
Boolean >> or: block1 or: block2 or: block3 or: block4 [
	"Nonevaluating alternation without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as true, then return true immediately,
		without evaluating any further blocks.
	If all return false, then return false."

	self subclassResponsibility
]

{ #category : 'printing' }
Boolean >> printAsLiteralOn: aStream [
	^self printOn: aStream
]

{ #category : 'copying' }
Boolean >> shallowCopy  [
	"Receiver has two concrete subclasses, True and False.
	Only one instance of each should be made, so return self."
]

{ #category : 'printing' }
Boolean >> storeOn: aStream [
	self printOn: aStream
]

{ #category : 'logical operations' }
Boolean >> xor: aBoolean [

	self subclassResponsibility
]
