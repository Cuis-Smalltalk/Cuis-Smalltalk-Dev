'From Cuis 6.0 [latest update: #5183] on 22 May 2022 at 7:17:34 pm'!

!ReadStream commentStamp: '<historical>' prior: 0!
I represent an accessor for a sequence of objects that can only read objects from the sequence.
Instances are created on: aCollection. Its elements will be iterated by us.!


!WriteStream commentStamp: '<historical>' prior: 0!
I represent an accessor for a sequence of objects that can only store objects in the sequence.
Instances are created on: aCollection, which is considered to be empty or filled with nil. Any contents will be ovewriten.
Instances can also be create with: aCollection. In this case, new contents will be added after this initial stuff.!


!ReadWriteStream commentStamp: '<historical>' prior: 0!
I represent an accessor for a sequence of objects. My instances can both read and store objects.
Instances are created on: aCollection, which is considered to be empty or filled with nil. Any contents will be ovewriten, and are not available for reading.
Instances can also be create with: aCollection. In this case, new contents will be added after this initial stuff. Reading them is only possible if position: is set appropriately (for instance aReadWriteStream position: 1).!


!ReadWriteStream class methodsFor: 'instance creation' stamp: 'jmv 5/22/2022 19:17:17'!
readingOn: aCollection
	"Answer an instance that can be used like a ReadStream, i.e. it is born ready to read over aCollection."

	| instance |
	instance _ self with: aCollection.
	instance position: 0.
	^instance! !


!PositionableStream methodsFor: 'private' stamp: 'jmv 5/22/2022 18:43:45'!
on: aCollection
	"Initialize the stream to read over all of aCollection.
	The collection is considered to have useful content."

	collection _ aCollection.
	readLimit _ aCollection size.
	position _ 0.
	self reset.! !


!WriteStream methodsFor: 'private' stamp: 'jmv 5/22/2022 19:15:47'!
on: aCollection
	"Initialize the stream to write over all of aCollection.
	The collection contents are considered useless (most likely nil).
	Overwrite them.
	In the case of ReadWriteStream, for read operations, stream appears empty until stuff is added."

	super on: aCollection.
	readLimit _ 0.
	writeLimit _ aCollection size! !

!WriteStream methodsFor: 'private' stamp: 'jmv 5/22/2022 19:17:09'!
with: aCollection
	"Initialize the stream to append after existing elements in aCollection.
	In the case of ReadWriteStream, for read operations, the stream appears empty
	until stuff is added, or position is set to before end. See #readingOn:"

	super on: aCollection.
	position _ readLimit _ writeLimit _ aCollection size! !


!WriteStream class methodsFor: 'instance creation' stamp: 'jmv 5/22/2022 19:16:56'!
with: aCollection 
	"Answer an instance of me on the argument, aCollection, positioned to 
	store objects at the end of aCollection.
	In the case of ReadWriteStream, for read operations, the stream appears empty
	until stuff is added, or position is set to before end. See #readingOn:"

	^self basicNew with: aCollection! !


!ReadWriteStream class reorganize!
('instance creation' readingOn:)
!

