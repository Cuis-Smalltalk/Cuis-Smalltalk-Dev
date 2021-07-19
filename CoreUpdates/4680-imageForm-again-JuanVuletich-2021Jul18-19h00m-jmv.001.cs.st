'From Cuis 5.0 [latest update: #4675] on 18 July 2021 at 7:05:16 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 7/18/2021 19:05:09'!
imageForm: extentOrNil depth: depth

	| extent answer auxCanvas |
	extent _ self morphExtent.
	extentOrNil ifNotNil: [ extent _ extent min: extentOrNil * 4 ].
	auxCanvas _ MorphicCanvas depth: depth over: (self morphPosition floor extent: extent ceiling).
	auxCanvas fullDraw: self.
	answer _ auxCanvas form divideByAlpha.
	extentOrNil ifNotNil: [ answer _ answer magnifyTo: extentOrNil ].
	^answer.! !

