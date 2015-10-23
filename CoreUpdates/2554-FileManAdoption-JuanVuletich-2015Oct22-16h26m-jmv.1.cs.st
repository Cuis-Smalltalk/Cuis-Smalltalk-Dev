'From Cuis 4.2 of 25 July 2013 [latest update: #2553] on 22 October 2015 at 4:40:26.695411 pm'!

!FmFileIOAccessor methodsFor: 'testing' stamp: 'jmv 10/21/2015 17:33'!
onMacClassic
	^self pathNameDelimiter = $:! !

!FmFileIOAccessor methodsFor: 'testing' stamp: 'jmv 10/21/2015 17:34'!
onMacOsX
	^self onUnix and: [Smalltalk platformName = 'Mac OS']! !


!String methodsFor: '*fileman-core-converting' stamp: 'jmv 10/21/2015 16:54'!
asDirectoryEntry
	"See examples in #asFileEntry method comment"
	^FmDirectoryEntry pathName: self! !

!String methodsFor: '*fileman-core-converting' stamp: 'jmv 10/22/2015 16:31'!
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

	| candidate |
	FmFileIOAccessor default onWindows ifTrue: [
		self beginsWithWindowsDriveName ifTrue: [ 
		^self copyFrom: 1 to: 2 ]].

	(FmFileIOAccessor default onMacClassic) ifTrue: [
		candidate _ self upToFirstPathSeparator.
		"Aparently on Mac Classic, 
			xxx/yyy means xxx must be a drive name
			/xxx/yyy means xxx could be any folder in root. Check to make sure!!
		"
		('/' asDirectoryEntry directoryNames includes: candidate) ifTrue: [
			^candidate ]].
	
	^ nil! !

!String methodsFor: '*fileman-core-converting' stamp: 'jmv 10/22/2015 16:35'!
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
	^FmFileEntry pathName: self! !

!String methodsFor: '*fileman-core-testing' stamp: 'jmv 10/21/2015 17:36'!
isAbsolutePathName
	| upperName |
	self isEmpty ifTrue: [^ false].
	self first isPathSeparator ifTrue: [^ true].

	FmFileIOAccessor default onWindows
		ifTrue: [
			^ self beginsWithWindowsDriveName and: [ self size = 2 or: [ (self at: 3) isPathSeparator ]]].

	FmFileIOAccessor default onMacClassic ifTrue: [
		upperName := self asUppercase.
		^'/' asDirectoryEntry directoryNames anySatisfy: [ :each |
			(upperName beginsWith: each)
					and: [| nextPos | 
						nextPos := each size + 1 min: self size max: 1.
						(self at: nextPos) isPathSeparator ]]].

	^ false! !

!String methodsFor: '*fileman-core-testing' stamp: 'jmv 10/21/2015 17:36'!
isDriveName
	FmFileIOAccessor default onWindows
		ifTrue: [
			^ (self size between: 2 and: 3)
				and: [self beginsWithWindowsDriveName]].

	FmFileIOAccessor default onMacClassic ifTrue: [
		^'/' asDirectoryEntry directoryNames includes: self].

	^false! !


!FmDirectoryEntry methodsFor: 'accessing' stamp: 'jmv 10/21/2015 16:49'!
children
	"Caching this is prone to trouble. Files can be created and deleted anytime."
	children _ nil.

	children ifNil: [self initChildren].
	^children! !


!FmDirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 10/21/2015 17:41'!
roots
	"Appropriate for all systems, including those with several roots, each being a logical 'drive' (Like Windows),
	and for systems having a single root with file systems (i.e. 'drives') mounted anywhere in the tree (Unix, MacOS)
	"
	"
	FmDirectoryEntry roots
	"
	^ FmFileIOAccessor default drives
		ifEmpty: [
			"On Linux and MacOsX"
			{ '/' asDirectoryEntry } ]! !


!FmFileIOAccessor methodsFor: 'private' stamp: 'jmv 10/22/2015 16:38'!
entriesIn: parentEntryOrNil
	"
	Warning: Private. Only to be called from within FileMan.
	Accepts nil as argument, but behavior depends on platform.

Windows (nil means root)
FmFileIOAccessor default entriesIn: nil #(C:\ D:\)
(FmFileIOAccessor default entriesIn: '' asDirectoryEntry) = (FmFileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FmFileIOAccessor default entriesIn: '/' asDirectoryEntry #(\$Recycle.Bin \Config.Msi \Documents and Settings \gratMusic \hiberfil.sys \Intel \pagefile.sys \PerfLogs \Program Files \Program Files (x86) \ProgramData \Python27 \Recovery \SimuloHoy \System Volume Information \totalcmd \Users \Windows)

Linux  (nil means current dir, like '' and '.')
FmFileIOAccessor default entriesIn: nil #(Lots of stuff in current directory)
(FmFileIOAccessor default entriesIn: nil) = (FmFileIOAccessor default entriesIn: '.' asDirectoryEntry) true
(FmFileIOAccessor default entriesIn: '' asDirectoryEntry) = (FmFileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FmFileIOAccessor default entriesIn: '/' asDirectoryEntry #(/vmlinuz /boot /sbin /srv /lib /lib32 /tmp /sys /home /etc /initrd.img /bin /dev /opt /proc /lost+found /var /root /lib64 /mnt /usr /run /media)

MacOsX (nil means current dir, like '' and '.')
FmFileIOAccessor default entriesIn: nil #(/Volumes/SanDisk32-NTFS/CuisTest/2554-REVISAR-JuanVuletich-2015Oct21-16h40m-jmv.1.cs.st /Volumes/SanDisk32-NTFS/CuisTest/Cog.app /Volumes/SanDisk32-NTFS/CuisTest/Cog.app.tgz /Volumes/SanDisk32-NTFS/CuisTest/Cuis4.2-2553.changes /Volumes/SanDisk32-NTFS/CuisTest/Cuis4.2-2553.image /Volumes/SanDisk32-NTFS/CuisTest/CuisV4.sources)
(FmFileIOAccessor default entriesIn: '' asDirectoryEntry) = (FmFileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FmFileIOAccessor default entriesIn: '/' asDirectoryEntry #(/.dbfseventsd /.DocumentRevisions-V100 /.DS_Store /.file /.fseventsd /.hotfiles.btree /.Spotlight-V100 /.Trashes /.vol /Applications /bin /cores /dev /etc /home /installer.failurerequests /Library /net /Network /opt /private /sbin /System /tmp /Users /usr /var /Volumes)

	"
	| entries index done entryArray entry isDirectory |
	entries := OrderedCollection new: 200.
	index := 1.
	done := false.
	[done] whileFalse: [
		entryArray := self primLookupEntryIn: (parentEntryOrNil ifNil: [''] ifNotNil: [parentEntryOrNil pathName]) index: index.
		#badDirectoryPath = entryArray ifTrue: [
			^#()].
		entryArray == nil
			ifTrue: [done := true]
			ifFalse: [
				isDirectory _ entryArray at: 4.
				isDirectory
					ifTrue: [entry _ FmDirectoryEntry new]
					ifFalse: [
						entry _ FmFileEntry new.
						entry fileSize: (entryArray at: 5) ].
				entry name: (entryArray at: 1).
				entry creationTime: (entryArray at: 2).
				entry modificationTime: (entryArray at: 3).
				parentEntryOrNil ifNotNil: [
					entry parent: parentEntryOrNil ]
				ifNil: [
					entry pathName: entry name ].
				entries addLast: entry ].
		index := index + 1].

	^entries asArray! !

!FmFileIOAccessor methodsFor: 'utilities' stamp: 'jmv 10/22/2015 16:39'!
directoryNamesIn: anFmDirectoryEntry
	"
	FmFileIOAccessor default directoryNamesIn: 'C:\Windows' asDirectoryEntry
	"
	
	^(self entriesIn: anFmDirectoryEntry)
		select: [ :each | each isDirectory]
		thenCollect: [ :each | each name]! !

!FmFileIOAccessor methodsFor: 'testing' stamp: 'jmv 10/21/2015 17:35'!
isDriveSupported
	^self onWindows or: [self onMacClassic]! !

!methodRemoval: FmFileIOAccessor #onMac!
FmFileIOAccessor removeSelector: #onMac!
