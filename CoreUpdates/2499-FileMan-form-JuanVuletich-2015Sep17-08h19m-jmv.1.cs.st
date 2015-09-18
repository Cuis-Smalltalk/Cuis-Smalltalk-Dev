'From Cuis 4.2 of 25 July 2013 [latest update: #2498] on 17 September 2015 at 8:26:02.158218 am'!

!FmFileEntry methodsFor: 'cuis extensions' stamp: 'jmv 9/17/2015 08:12'!
form
	"Read a Form or ColorForm from the given file.
	'../../4.2.04.tiff' asFileEntry form display
	"

	| form |
	self readStreamContents: [ :stream |
		form _ Form fromBinaryStream: stream binary ].
	^ form! !

