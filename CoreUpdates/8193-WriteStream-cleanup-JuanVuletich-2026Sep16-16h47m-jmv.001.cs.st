'From Cuis7.9 [latest update: #8192] on 16 September 2026 at 4:49:18 pm'!
!WriteStream methodsFor: 'private' stamp: 'jmv 16/Sep/2026, 7:22:24 pm (UTC)'!
braceArray: anArray
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	collection := anArray.
	position := 0.
	readLimit := 0.! !

!WriteStream methodsFor: 'private' stamp: 'jmv 16/Sep/2026, 7:22:29 pm (UTC)'!
growTo: anInteger
	"Grow the collection by creating a new bigger collection and then
	copy over the contents from the old one. We grow by doubling the size.

	anInteger is the required minimal new size of the collection "

	| oldSize newSize |
	oldSize := collection size.
	newSize := anInteger + (oldSize max: 20).
	collection := collection grownTo: newSize.! !

!WriteStream methodsFor: 'private' stamp: 'jmv 16/Sep/2026, 7:22:49 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel01}!
on: aCollection
	"Initialize the stream to write over all of aCollection.
	The collection contents are considered useless (most likely nil).
	Overwrite them.
	In the case of ReadWriteStream, for read operations, stream appears empty until stuff is added."

	super on: aCollection thatCanBeModified.
	readLimit := 0.! !

!WriteStream methodsFor: 'private' stamp: 'jmv 16/Sep/2026, 7:48:39 pm (UTC)'!
with: aCollection
	"Initialize the stream to append after existing elements in aCollection.
	In the case of ReadWriteStream, for read operations, the stream appears empty
	until stuff is added, or position is set to before end. See #readingOn:"

	super on: aCollection.
	position := readLimit := aCollection size.! !

!classDefinition: #WriteStream category: #'Kernel-Collections'!
PositionableStream subclass: #WriteStream
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Kernel-Collections'!
