'From Cuis 4.2 of 25 July 2013 [latest update: #2456] on 15 August 2015 at 6:21:22.41792 pm'!

!Morph methodsFor: 'as yet unclassified' stamp: 'jmv 8/15/2015 18:09'!
icon
	^ (self imageForm: 32)
		ifNil: [ Theme current morphsIcon ]
		ifNotNil: [ :form |
			form magnifyTo: 16 @ 16 ]! !

