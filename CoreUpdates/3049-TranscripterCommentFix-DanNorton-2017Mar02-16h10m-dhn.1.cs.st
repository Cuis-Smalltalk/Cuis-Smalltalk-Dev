'From Cuis 5.0 of 7 November 2016 [latest update: #3048] on 2 March 2017 at 4:11:32 pm'!

!Transcripter class methodsFor: 'instance creation' stamp: 'dhn 2/6/2017 13:38:40'!
newInFrame: frame
"
(Transcripter newInFrame: (0@0 extent: 100@200))
	nextPutAll: 'Hello there'; endEntry;
	newLine; print: 355.0/113; endEntry;
	readEvalPrint.
"
	| transcript |
	transcript _ self on: (String new: 100).
	transcript initInFrame: frame.
	^ transcript clear! !

