'From Cuis 5.0 of 7 November 2016 [latest update: #3577] on 16 January 2019 at 8:40:56 am'!

!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 1/16/2019 08:38:06'!
allButFirstDo: block

	2 to: self size do:
		[:index | block value: (self at: index)]! !

!SequenceableCollection methodsFor: 'enumerating' stamp: 'jmv 1/16/2019 08:38:18'!
allButLastDo: block

	1 to: self size - 1 do:
		[:index | block value: (self at: index)]! !

