'From Cuis 5.0 [latest update: #4672] on 16 July 2021 at 6:35:31 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 7/16/2021 18:35:07'!
imageForm: depth

	^self imageForm: nil depth: depth.! !

!Morph methodsFor: 'drawing' stamp: 'jmv 7/16/2021 18:34:40'!
imageForm: extentOrNil depth: depth

	| extent answer auxCanvas |
	extent _ self morphExtent.
	extentOrNil ifNotNil: [ extent _ extent min: extentOrNil * 4 ].
	auxCanvas _ BitBltCanvas depth: depth over: (self morphPosition floor extent: extent ceiling).
	auxCanvas fullDraw: self.
	answer _ auxCanvas form divideByAlpha.
	extentOrNil ifNotNil: [ answer _ answer magnifyTo: extent ].
	^answer.! !

