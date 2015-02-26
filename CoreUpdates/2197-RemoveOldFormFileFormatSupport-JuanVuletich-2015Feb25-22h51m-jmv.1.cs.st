'From Cuis 4.2 of 25 July 2013 [latest update: #2196] on 25 February 2015 at 10:59:22.147253 pm'!

!Form class methodsFor: 'instance creation' stamp: 'jmv 2/25/2015 22:58'!
fromBinaryStream: aBinaryStream
	"Read a Form or ColorForm from given file, using the first byte of the file to guess its format. Currently handles: GIF, uncompressed BMP, and both old and new DisplayObject writeOn: formats, JPEG, and PCX. Return nil if the file could not be read or was of an unrecognized format."

	| firstByte |
	firstByte _ aBinaryStream next.
	firstByte = 2 ifTrue: [
		"Squeak form format"
		^ self new readFrom: aBinaryStream].

	"Try for JPG, GIF, or PCX..."
	"Note: The following call closes the stream."
	^ ImageReadWriter formFromStream: aBinaryStream
! !

!methodRemoval: Form #readFromOldFormat:!
Form removeSelector: #readFromOldFormat:!
