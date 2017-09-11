'From Cuis 5.0 of 7 November 2016 [latest update: #3168] on 8 September 2017 at 6:27:38 pm'!

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'pb 9/8/2017 18:21:14'!
morphHeight

"Ensure everybody wants our coordinates!!"
	self flag: #jmvVer2.
	^ self morphExtent y! !

!RectangleLikeMorph methodsFor: 'geometry' stamp: 'pb 9/8/2017 18:21:21'!
morphWidth

"Ensure everybody wants our coordinates!!"
	self flag: #jmvVer2.
	^ self morphExtent x! !

