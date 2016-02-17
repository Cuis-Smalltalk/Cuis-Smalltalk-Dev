'From Cuis 4.2 of 25 July 2013 [latest update: #2682] on 17 February 2016 at 12:10:55.601779 pm'!

!Text class methodsFor: 'instance creation' stamp: 'jmv 2/16/2016 17:14'!
withForm: aForm
	"
	('Hello', (Text withForm: ImageMorph defaultForm), 'world') edit
	"
	^' ', (Text string: '*' attribute: (TextAnchor new anchoredFormOrMorph: aForm)), ' '! !


!Morph methodsFor: 'drawing' stamp: 'jmv 2/17/2016 12:10'!
imageForm: depth
	| canvas |
	canvas _ BitBltCanvas depth: depth over: (self morphPosition extent: self morphExtent).
	canvas fullDraw: self.
	^ canvas form! !

