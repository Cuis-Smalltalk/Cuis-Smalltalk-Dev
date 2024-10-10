'From Cuis7.1 [latest update: #6765] on 10 October 2024 at 11:40:18 am'!

!FileEntry methodsFor: 'actions-file' stamp: 'jmv 10/10/2024 11:40:10'!
copyTo: filename 
	| targetEntry |
	
	targetEntry := filename asFileEntry.
	targetEntry existsAsDirectory
		ifTrue: [
			targetEntry := filename asDirectoryEntry // self name ].
	self assureExistence.
	targetEntry assureExistence.
	self fileAccessor copy: self to: targetEntry! !

