'From Cuis6.3 [latest update: #6236] on 22 February 2024 at 10:11:07 am'!

!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 2/22/2024 10:10:10'!
asFullDirectoryEntry
	"Only for fully qualified path names"

	"See examples in #asFullFileEntry method comment"
	self isAbsolutePathName ifFalse: [ self error: 'Only for fully qualified (absolute) path names' ].
	^ DirectoryEntry withAbsolutePathName: self.! !

!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 2/22/2024 10:09:43'!
asFullFileEntry
	"Only for fully qualified path names"
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
	self isAbsolutePathName ifFalse: [ self error: 'Only for fully qualified (absolute) path names' ].
	^ FileEntry withAbsolutePathName: self.! !


!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 2/22/2024 10:05:20'!
asDirectoryEntry
	"See examples in #asFileEntry method comment"

	^self isAbsolutePathName
		ifTrue: [
			DirectoryEntry withAbsolutePathName: self ]
		ifFalse: [
			DirectoryEntry withRelativePathName: self to: DirectoryEntry currentDirectory ].! !

!CharacterSequence methodsFor: 'fileman-converting' stamp: 'jmv 2/22/2024 10:05:42'!
asFileEntry
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
	^self isAbsolutePathName
		ifTrue: [
			FileEntry withAbsolutePathName: self ]
		ifFalse: [
			FileEntry withRelativePathName: self to: DirectoryEntry currentDirectory ].! !

