'From Cuis6.3 [latest update: #6276] on 15 March 2024 at 4:01:30 pm'!

!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 3/15/2024 15:59:03'!
asDirectoryEntry
	"Note: It is generally advisable to call one of the more specific messages:
		#/
		#withAbsolutePathName: 
		#withRelativePathName:to:"
	"See examples in #asFileEntry method comment"

	^ DirectoryEntry withPathName: self! !

!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 3/15/2024 16:00:45'!
asFileEntry
	"Note: It is generally advisable to call one of the more specific messages:
		#//
		#withAbsolutePathName: 
		#withRelativePathName:to:"
	"
	Windows	
		'C:\Windows' asFileEntry exists false
		'C:\Windows' asDirectoryEntry exists true
		'/' asFileEntry exists false
		'/' asDirectoryEntry exists false
		'C:\' asFileEntry exists false
		'C:\' asDirectoryEntry exists true
		('C:' asDirectoryEntry // 'Windows') exists false
		('C:' asDirectoryEntry / 'Windows') exists true
	Linux
		'/var' asFileEntry exists
		'/var' asDirectoryEntry exists true
		'/' asFileEntry exists false
		'/' asDirectoryEntry exists true
		'/media/cdrom' asFileEntry exists false
		'/media/cdrom' asDirectoryEntry exists true
		('/bin' asDirectoryEntry / 'more') exists false
		('/bin' asDirectoryEntry // 'more') exists true
	MacOsX
		'/var' asFileEntry exists false
		'/var' asDirectoryEntry exists true
		'/' asFileEntry exists false
		'/' asDirectoryEntry exists  true
		'/Volumes/SanDisk32-NTFS' asFileEntry exists false
		'/Volumes/SanDisk32-NTFS' asDirectoryEntry exists true
		'SanDisk32-NTFS' asFileEntry exists false
		'SanDisk32-NTFS' asDirectoryEntry exists false
	"

	self isRelativeMark ifTrue: [ ^self error: 'Maybe you need to call #asDirectoryEntry!!' ].
	^ FileEntry withPathName: self! !


!FileSystemEntry class methodsFor: 'instance creation' stamp: 'jmv 3/15/2024 15:58:20'!
withPathName: aString
	"Note: It is generally advisable to call one or the more specific messages:
	#withAbsolutePathName: or #withRelativePathName:to:"

	^aString isAbsolutePathName
		ifTrue: [ self withAbsolutePathName: aString ]
		ifFalse: [
			self
				withRelativePathName: aString
				to: DirectoryEntry userBaseDirectory ].! !

