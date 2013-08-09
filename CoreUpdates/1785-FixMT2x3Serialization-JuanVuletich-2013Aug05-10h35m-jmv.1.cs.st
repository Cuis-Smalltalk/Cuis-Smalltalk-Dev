'From Cuis 4.2 of 25 July 2013 [latest update: #1783] on 5 August 2013 at 10:36:09.624219 am'!

!MatrixTransform2x3 class methodsFor: 'instance creation' stamp: 'jmv 8/4/2013 11:37'!
newFromStream: s
	"Only meant for my subclasses that are raw bits and word-like.  For quick unpack form the disk."
	^ s nextWordsInto: self new! !

