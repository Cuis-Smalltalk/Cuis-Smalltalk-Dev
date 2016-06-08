'From Cuis 4.2 of 25 July 2013 [latest update: #2795] on 7 June 2016 at 6:08:58.855019 pm'!

!Collection methodsFor: 'statistics' stamp: 'len 6/7/2016 09:37'!
max: aBlock
	"Answer the maximum of evaluating aBlock on all the elements of the receiver."
	| answer |
	self emptyCheck.
	self do: [:each| answer _ answer isNil ifTrue: [aBlock value: each] ifFalse: [answer max: (aBlock value: each)]].
	^ answer! !

!Collection methodsFor: 'statistics' stamp: 'len 6/7/2016 09:37'!
min: aBlock
	"Answer the minimum of evaluating aBlock on all the elements of the receiver."
	| answer |
	self emptyCheck.
	self do: [:each| answer _ answer isNil ifTrue: [aBlock value: each] ifFalse: [answer min: (aBlock value: each)]].
	^ answer! !

!Collection methodsFor: 'statistics' stamp: 'len 5/27/2016 21:24'!
product
	"Compute the product of all the elements in the receiver"

	^self collect: [ :each | each ] andFold: [ :a :b | a * b]! !

!Collection methodsFor: 'statistics' stamp: 'len 5/27/2016 21:26'!
product: aBlock
	"This is implemented using a variant of the normal inject:into: pattern. 
	The reason for this is that it is not known whether we're in the normal 
	number line, i.e. whether 1 is a good initial value for the product."
	^self collect: aBlock andFold: [ :a :b | a * b ]! !

