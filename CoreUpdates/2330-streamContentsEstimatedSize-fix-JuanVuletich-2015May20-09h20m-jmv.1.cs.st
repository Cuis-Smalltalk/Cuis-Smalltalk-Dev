'From Cuis 4.2 of 25 July 2013 [latest update: #2329] on 20 May 2015 at 9:21:22.504632 am'!

!SequenceableCollection class methodsFor: 'stream creation' stamp: 'jmv 5/20/2015 09:20'!
streamContents: blockWithArg estimatedSize: estimatedSize

	| stream originalContents |
	stream _ WriteStream on: (self new: estimatedSize).
	blockWithArg value: stream.
	originalContents _ stream originalContents.
	^stream position = originalContents size
		ifTrue: [ originalContents ]
		ifFalse: [ stream contents ]! !

