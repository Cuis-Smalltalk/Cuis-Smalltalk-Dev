'From Cuis 5.0 [latest update: #3883] on 25 September 2019 at 5:05:25 pm'!

!BlockClosure methodsFor: 'accessing' stamp: 'jmv 9/25/2019 10:55:23'!
outerContextsDo: aBlock

	outerContext outerContextsDo: aBlock! !


!MethodContext methodsFor: 'accessing' stamp: 'jmv 9/25/2019 10:54:45'!
outerContextsDo: aBlock
	"Answer the context in which the receiver was defined."

	closureOrNil
		ifNotNil: [ closureOrNil outerContextsDo: aBlock ].
	aBlock value: self! !

