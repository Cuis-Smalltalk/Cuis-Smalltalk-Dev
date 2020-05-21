'From Cuis 5.0 [latest update: #4161] on 21 May 2020 at 7:47:20 pm'!

!LayoutSpec methodsFor: 'accessing' stamp: 'jmv 5/21/2020 19:46:50'!
fixedWidth
	^proportionalWidth isNil ifTrue: [fixedWidth ifNil: [morph morphWidth]] ifFalse: [ 0 ]! !

