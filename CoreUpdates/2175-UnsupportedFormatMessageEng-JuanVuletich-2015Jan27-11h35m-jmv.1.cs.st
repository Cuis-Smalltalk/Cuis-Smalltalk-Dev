'From Cuis 4.2 of 25 July 2013 [latest update: #2170] on 27 January 2015 at 11:39:13.134854 am'!

!ImageReadWriter class methodsFor: 'image reading/writing' stamp: 'jmv 1/27/2015 11:36'!
formFromStream: aBinaryStream
	"Answer a ColorForm stored on the given stream.  closes the stream"
	| reader readerClass form  |

	readerClass _ self withAllSubclasses
		detect: [ :subclass | subclass understandsImageFormat: aBinaryStream reset ]
		ifNone: [
			aBinaryStream close.
			^self error: 'Unsupported format. Try "Graphics-Files-Additional.pck.st".'].
	reader _ readerClass onBinaryStream: aBinaryStream reset.
	form _ reader nextImage.
	reader close.
	^ form! !


!PreDebugWindow methodsFor: 'GUI building' stamp: 'jmv 1/27/2015 11:38'!
initialExtent
	^ 640 @ 320! !

