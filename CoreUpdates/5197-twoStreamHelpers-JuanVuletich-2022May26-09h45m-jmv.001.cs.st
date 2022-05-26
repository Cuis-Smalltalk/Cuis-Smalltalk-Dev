'From Cuis 6.0 [latest update: #5196] on 26 May 2022 at 9:46:03 am'!

!SequenceableCollection methodsFor: 'converting' stamp: 'jmv 5/22/2022 20:42:56'!
readStreamFrom: start to: stop
	^ ReadStream on: self from: start to: stop! !


!ArrayedCollection class methodsFor: 'stream creation' stamp: 'jmv 5/22/2022 21:07:37'!
writeStream
	^ WriteStream on: (self new: 100)! !

