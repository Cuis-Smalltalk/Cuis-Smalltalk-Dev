'From Cuis7.1 [latest update: #6872] on 28 November 2024 at 5:15:25 pm'!

!DirectoryEntry class methodsFor: 'user default directories' stamp: 'jmv 11/28/2024 17:08:17'!
fileOutsDirQuery: suggestedFileName writeStreamDo: writeBlock

	self userDirectory: 'FileOuts' queryFileName: suggestedFileName writeStreamDo: writeBlock! !


!DirectoryEntry class methodsFor: 'user default directories' stamp: 'jmv 11/28/2024 17:08:42'!
fileOutsDirectory

	^ self userDirectory: 'FileOuts'! !

