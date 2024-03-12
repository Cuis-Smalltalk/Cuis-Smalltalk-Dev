'From Cuis6.3 [latest update: #6265] on 12 March 2024 at 10:58:15 am'!
!classDefinition: #DirectoryEntry category: #'System-File System'!
FileSystemEntry subclass: #DirectoryEntry
	instanceVariableNames: 'children lastChildrenSync '
	classVariableNames: 'CuisBaseDirectory CurrentDirectory ImageDirectory ProjectBaseDirectory UserBaseDirectory VMDirectory '
	poolDictionaries: ''
	category: 'System-File System'!

!DirectoryEntry methodsFor: 'private' stamp: 'jmv 3/12/2024 10:50:06'!
initChildren

	self exists ifFalse: [ ^children := #()] .
	children := self fileAccessor entriesIn: self.
	lastChildrenSync := DateAndTime now.
	^children! !

!DirectoryEntry methodsFor: 'accessing' stamp: 'jmv 3/12/2024 10:51:14'!
children
	(lastChildrenSync isNil or: [
		(DateAndTime now - lastChildrenSync) totalSeconds > 2]) ifTrue: [
			children := nil ].
	children ifNil: [self initChildren].
	^children! !

!methodRemoval: DirectoryEntry #invalidate stamp: 'jmv 3/12/2024 10:57:25'!
DirectoryEntry removeSelector: #invalidate!
!classDefinition: #DirectoryEntry category: #'System-File System'!
FileSystemEntry subclass: #DirectoryEntry
	instanceVariableNames: 'children lastChildrenSync'
	classVariableNames: 'CuisBaseDirectory CurrentDirectory ImageDirectory ProjectBaseDirectory UserBaseDirectory VMDirectory'
	poolDictionaries: ''
	category: 'System-File System'!
