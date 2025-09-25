'From Cuis7.5 [latest update: #7610] on 25 September 2025 at 1:12:24 pm'!

!Theme methodsFor: 'tool colors' stamp: 'jmv 9/25/2025 13:11:46'!
changeList
	^ self useUniformColors
		ifTrue: [ self defaultWindowColor ]
		ifFalse: [ `Color r: 0.65 g: 0.6 b: 0.85` ]! !

