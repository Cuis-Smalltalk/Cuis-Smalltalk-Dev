'From Cuis 6.0 [latest update: #5362] on 7 July 2022 at 12:03:33 pm'!

!SequenceableCollection methodsFor: 'converting' stamp: 'jmv 7/7/2022 10:37:44'!
thatCanBeModified
	"By default, SequenceableCollections can handle #at:put:"

	^self! !


!Utf8String methodsFor: 'converting' stamp: 'jmv 7/7/2022 10:36:08'!
thatCanBeModified
	"Because Utf8String instances don't support #at:put:"

	^ArrayOfCharactersAndCodePoints withAll: self! !


!WriteStream methodsFor: 'private' stamp: 'jmv 7/7/2022 10:44:06'!
on: aCollection
	"Initialize the stream to write over all of aCollection.
	The collection contents are considered useless (most likely nil).
	Overwrite them.
	In the case of ReadWriteStream, for read operations, stream appears empty until stuff is added."

	super on: aCollection thatCanBeModified.
	readLimit _ 0.
	writeLimit _ aCollection size! !

