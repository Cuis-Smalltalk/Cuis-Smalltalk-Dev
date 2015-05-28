'From Cuis 4.2 of 25 July 2013 [latest update: #2338] on 27 May 2015 at 9:55:12.369749 am'!

!Morph methodsFor: 'drawing' stamp: 'jmv 5/26/2015 16:48'!
imageForm: depth
	| canvas |
	self morphFullBoundsInWorld ifNotNil: [ :r |
		canvas _ BitBltCanvas depth: depth over: r.
		canvas fullDraw: self.
		^ canvas form ].
	^ nil! !

