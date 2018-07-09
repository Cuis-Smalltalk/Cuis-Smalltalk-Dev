'From Cuis 5.0 of 7 November 2016 [latest update: #3365] on 9 July 2018 at 4:41:43 pm'!

!False methodsFor: 'controlling' stamp: 'HAW 7/8/2018 20:12:50'!
and: aBlock1 and: aBlock2

	^self! !

!False methodsFor: 'controlling' stamp: 'HAW 7/8/2018 20:13:01'!
and: aBlock1 and: aBlock2 and: aBlock3

	^self! !

!False methodsFor: 'controlling' stamp: 'HAW 7/8/2018 20:13:06'!
and: aBlock1 and: aBlock2 and: aBlock3 and: aBlock4

	^self! !

!False methodsFor: 'controlling' stamp: 'jmv 7/9/2018 16:37:10'!
or: aBlock1 or: aBlock2

	^aBlock1 value or: aBlock2! !

!False methodsFor: 'controlling' stamp: 'jmv 7/9/2018 16:39:33'!
or: aBlock1 or: aBlock2 or: aBlock3

	^aBlock1 value or: aBlock2 or: aBlock3! !

!False methodsFor: 'controlling' stamp: 'jmv 7/9/2018 16:39:40'!
or: aBlock1 or: aBlock2 or: aBlock3 or: aBlock4

	^aBlock1 value or: aBlock2 or: aBlock3 or: aBlock4! !


!True methodsFor: 'controlling' stamp: 'jmv 7/9/2018 16:38:32'!
and: aBlock1 and: aBlock2

	^aBlock1 value and: aBlock2! !

!True methodsFor: 'controlling' stamp: 'jmv 7/9/2018 16:39:09'!
and: aBlock1 and: aBlock2 and: aBlock3

	^aBlock1 value and: aBlock2 and: aBlock3! !

!True methodsFor: 'controlling' stamp: 'jmv 7/9/2018 16:39:14'!
and: aBlock1 and: aBlock2 and: aBlock3 and: aBlock4

	^aBlock1 value and: aBlock2 and: aBlock3 and: aBlock4 ! !

!True methodsFor: 'controlling' stamp: 'HAW 7/8/2018 20:13:31'!
or: aBlock1 or: aBlock2

	^self! !

!True methodsFor: 'controlling' stamp: 'HAW 7/8/2018 20:13:43'!
or: aBlock1 or: aBlock2 or: aBlock3

	^self! !

!True methodsFor: 'controlling' stamp: 'HAW 7/8/2018 20:13:48'!
or: aBlock1 or: aBlock2 or: aBlock3 or: aBlock4

	^self! !


!Boolean methodsFor: 'controlling' stamp: 'HAW 7/8/2018 20:16:44'!
and: block1 and: block2
	"Nonevaluating conjunction without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as false, then return false immediately,
		without evaluating any further blocks.
	If all return true, then return true."

	self subclassResponsibility! !

!Boolean methodsFor: 'controlling' stamp: 'HAW 7/8/2018 20:16:50'!
and: block1 and: block2 and: block3
	"Nonevaluating conjunction without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as false, then return false immediately,
		without evaluating any further blocks.
	If all return true, then return true."

	self subclassResponsibility! !

!Boolean methodsFor: 'controlling' stamp: 'HAW 7/8/2018 20:16:57'!
and: block1 and: block2 and: block3 and: block4
	"Nonevaluating conjunction without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as false, then return false immediately,
		without evaluating any further blocks.
	If all return true, then return true."

	self subclassResponsibility! !

!Boolean methodsFor: 'controlling' stamp: 'HAW 7/8/2018 20:17:04'!
or: block1 or: block2
	"Nonevaluating alternation without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as true, then return true immediately,
		without evaluating any further blocks.
	If all return false, then return false."

	self subclassResponsibility! !

!Boolean methodsFor: 'controlling' stamp: 'HAW 7/8/2018 20:17:09'!
or: block1 or: block2 or: block3
	"Nonevaluating alternation without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as true, then return true immediately,
		without evaluating any further blocks.
	If all return false, then return false."

	self subclassResponsibility! !

!Boolean methodsFor: 'controlling' stamp: 'HAW 7/8/2018 20:17:17'!
or: block1 or: block2 or: block3 or: block4
	"Nonevaluating alternation without deep nesting.
	The receiver is evaluated, followed by the blocks in order.
	If any of these evaluates as true, then return true immediately,
		without evaluating any further blocks.
	If all return false, then return false."

	self subclassResponsibility! !

