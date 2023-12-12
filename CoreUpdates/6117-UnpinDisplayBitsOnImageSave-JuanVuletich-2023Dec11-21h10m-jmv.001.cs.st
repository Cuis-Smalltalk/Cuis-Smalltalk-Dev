'From Cuis6.0 [latest update: #6115] on 11 December 2023 at 9:12:11 pm'!

!DisplayScreen methodsFor: 'private' stamp: 'jmv 12/11/2023 21:11:33'!
prepareForImageSave
	"Use little memory in saved image"

	self setExtent: `240@120` depth: self nativeDepth.
	bits unpin! !


!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 12/11/2023 21:11:46'!
shutDown 
	self terminateScreenUpdater.
	"Minimize Display memory saved in image"
	Display prepareForImageSave.! !

