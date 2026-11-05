'From Cuis7.9 [latest update: #8325] on 5 November 2026 at 5:31:59 pm'!

!DirectoryEntry methodsFor: 'actions-directory' stamp: 'jmv 5/Nov/2026, 8:30:09 pm (UTC)'!
assureExistence
	self exists ifFalse: [
		self parent assureExistenceDirNamed: self name ].! !

!DirectoryEntry methodsFor: 'private' stamp: 'jmv 5/Nov/2026, 8:28:42 pm (UTC)'!
assureExistenceDirNamed: localName
	
	localName isEmpty ifTrue: [ ^self ]. "Assumed to exist"

	self assureExistence.
	self fileAccessor createDirectory: (self / localName) pathName.
	self invalidateChildren.! !


!FileEntry methodsFor: 'enumeration' stamp: 'jmv 5/Nov/2026, 8:30:36 pm (UTC)'!
assureExistence
	self exists ifFalse: [
		self parent assureExistence.
		self forceWriteStreamDo: [ :stream | ]].! !

