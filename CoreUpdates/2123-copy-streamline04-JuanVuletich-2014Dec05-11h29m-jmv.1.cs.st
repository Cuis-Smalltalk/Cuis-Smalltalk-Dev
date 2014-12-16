'From Cuis 4.2 of 25 July 2013 [latest update: #2120] on 5 December 2014 at 11:29:47.734368 am'!

!SequenceableCollection methodsFor: 'copying' stamp: 'jmv 12/5/2014 11:14'!
copy

	^self copyFrom: 1 to: self size! !

!methodRemoval: SequenceableCollection #shallowCopy!
SequenceableCollection removeSelector: #shallowCopy!
