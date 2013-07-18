'From Cuis 4.1 of 12 December 2012 [latest update: #1535] on 3 January 2013 at 4:19:39 pm'!

!SystemDictionaryTest methodsFor: 'testing' stamp: 'jmv 1/3/2013 16:19'!
testPointersToEachIn
	"
10 timesRepeat: [SystemDictionaryTest new testPointersToEachIn ]
	"
	| p1 p2 o oa |
	o _ Browser.
	oa _ { o }.
	p1 _ (Smalltalk pointersTo: o).
	p2 _ (Smalltalk pointersToEachIn: oa) first.
	p1 remove: oa ifAbsent: [].
	p2 remove: oa ifAbsent: [].
	p1 remove: thisContext ifAbsent: [].
	p2 remove: thisContext ifAbsent: [].
	self assert: p1 = p2.
	o _ Float pi.
	oa _ { o }.
	p1 _ (Smalltalk pointersTo: o).
	p2 _ (Smalltalk pointersToEachIn: oa) first.
	p1 remove: oa ifAbsent: [].
	p2 remove: oa ifAbsent: [].
	p1 remove: thisContext ifAbsent: [].
	p2 remove: thisContext ifAbsent: [].
	self assert: p1 = p2.
	o _ SystemVersion current.
	oa _ { o }.
	p1 _ (Smalltalk pointersTo: o).
	p2 _ (Smalltalk pointersToEachIn: oa) first.
	p1 remove: oa ifAbsent: [].
	p2 remove: oa ifAbsent: [].
	p1 remove: thisContext ifAbsent: [].
	p2 remove: thisContext ifAbsent: [].
	self assert: p1 = p2.! !

