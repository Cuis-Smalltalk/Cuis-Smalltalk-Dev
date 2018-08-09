'From Cuis 5.0 of 7 November 2016 [latest update: #3399] on 9 August 2018 at 1:17:11 pm'!

!Form methodsFor: 'pixel access' stamp: 'jmv 8/8/2018 14:17:49'!
i: i j: j
	"Compatibility with Matrices"
	^ self colorAt: j@i -1! !

!Form methodsFor: 'pixel access' stamp: 'jmv 8/8/2018 14:18:01'!
i: i j: j put: aColor
	"Compatibility with Matrices"
	^ self colorAt: j@i -1 put: aColor! !

