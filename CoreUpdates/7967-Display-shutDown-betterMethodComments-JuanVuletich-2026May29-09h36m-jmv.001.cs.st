'From Cuis7.7 [latest update: #7966] on 29 May 2026 at 9:40:36 am'!

!Form methodsFor: 'private' stamp: 'jmv 29/May/2026 09:39:09'!
prepareForImageSave
	"Use little memory in saved image file.
	Useful if there may be references to us that prevent us from being collected."

	bits unpin.
	self setExtent: `240@120` depth: self nativeDepth.
	bits unpin. "In case we ever change initialization to also pin."! !


!DisplayScreen class methodsFor: 'snapshots' stamp: 'jmv 29/May/2026 09:40:09'!
shutDown 
	"Minimize Display memory saved in image.
	Note: We are not removing global #Display or nil'ing it: it is stored
	in Smalltalk specialObjectsArray at: 15, and therefore the VM might assume it is there.
	Instead we make it use little memory.
	See also DisplayScreen class >> releaseClassCachedState"

	Display prepareForImageSave.! !

!DisplayScreen class methodsFor: 'deferred updating' stamp: 'jmv 29/May/2026 09:39:34'!
clearDeferredUpdatingDisplay

	DeferredUpdatingDisplay ifNotNil: [
		"In case there are still references left."
		DeferredUpdatingDisplay prepareForImageSave.
		DeferredUpdatingDisplay := nil ].! !

!DisplayScreen class methodsFor: 'cached state access' stamp: 'jmv 29/May/2026 09:36:35'!
releaseClassCachedState
	"DeferredUpdatingDisplay can be nil'ed and recreated lazily.
	Therefore it is considered 'cached state'.
	See also DisplayScreen class >> shutDown"

	self clearDeferredUpdatingDisplay! !

