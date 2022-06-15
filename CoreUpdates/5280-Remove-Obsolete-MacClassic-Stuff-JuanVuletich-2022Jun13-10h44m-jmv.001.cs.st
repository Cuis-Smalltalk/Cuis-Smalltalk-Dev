'From Cuis 6.0 [latest update: #5276] on 13 June 2022 at 10:47:19 am'!

!String methodsFor: 'fileman-converting' stamp: 'jmv 6/13/2022 10:46:09'!
asDriveName
	"Answer a real drive name, or else answer nil.
	(Original FileMan implementation would answer first token on Mac even if it is not a Drive Name,
	and self in any case in other Unix variants)
	
Windows
	'C:\' asDriveName 'C:'
	'NotAChance' asDriveName nil
	
Linux
	'/media/cdrom' asDriveName nil

MacOsX
    '/SanDisk32-NTFS' asDriveName nil
	
	"
	FileIOAccessor default onWindows ifTrue: [
		self beginsWithWindowsDriveName ifTrue: [ 
		^self copyFrom: 1 to: 2 ]].
	
	^ nil! !

!String methodsFor: 'fileman-testing' stamp: 'jmv 6/13/2022 10:46:33'!
isAbsolutePathName
	"Note: On Windows, both 'C:\Users\Someone\file.txt' and '\Users\Someone\file.txt'
	and even '/Users/Someone/file.txt' are considered an absolute pathName.
	This is essentially because FilePlugin can handle them. The gained uniformity with Unix is nice."
	
	self isEmpty ifTrue: [^ false].
	self first isPathSeparator ifTrue: [^ true].

	FileIOAccessor default onWindows
		ifTrue: [
			^ self beginsWithWindowsDriveName and: [ self size = 2 or: [ (self at: 3) isPathSeparator ]]].

	^ false! !

!String methodsFor: 'fileman-testing' stamp: 'jmv 6/13/2022 10:46:43'!
isDriveName
	FileIOAccessor default onWindows
		ifTrue: [
			^ (self size between: 2 and: 3)
				and: [self beginsWithWindowsDriveName]].

	^false! !


!FileIOAccessor methodsFor: 'testing' stamp: 'jmv 6/13/2022 10:46:50'!
isDriveSupported
	^self onWindows! !

!methodRemoval: FileIOAccessor #onMacClassic stamp: 'jmv 6/13/2022 10:46:56'!
FileIOAccessor removeSelector: #onMacClassic!
