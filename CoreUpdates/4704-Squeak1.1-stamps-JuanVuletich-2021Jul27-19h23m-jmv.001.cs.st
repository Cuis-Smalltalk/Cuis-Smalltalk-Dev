'From Cuis 5.0 [latest update: #4702] on 27 July 2021 at 7:24:05 pm'!

!Object methodsFor: 'translation support' stamp: 'Squeak1.1 9/23/1996 20:34:59'!
var: varSymbol declareC: declString
	"For translation only; noop when running in Smalltalk."! !


!SequenceableCollection methodsFor: 'accessing' stamp: 'Squeak1.1 9/23/1996 20:34:59'!
indexOf: anElement startingAt: start ifAbsent: exceptionBlock
	"Answer the index of anElement within the receiver. If the receiver does 
	not contain anElement, answer the result of evaluating the argument, 
	exceptionBlock."
	start to: self size do:
		[:i | (self at: i) = anElement ifTrue: [^ i]].
	^ exceptionBlock value! !


!FileStream class methodsFor: 'concrete classes' stamp: 'Squeak1.1 9/23/1996 20:34:59'!
concreteStream
	"Who should we really direct class queries to?  9/21/96 tk"
	^ StandardFileStream! !

