'From Cuis 4.2 of 25 July 2013 [latest update: #2734] on 18 April 2016 at 3:46:04.286841 pm'!

!FmFileIOAccessor methodsFor: 'actions' stamp: 'jmv 4/18/2016 15:29'!
absolutePathComponentsFor: aString
	"Not complete, but in most cases it is OK"

	| tokens curDirPathComponents |
	tokens _ aString asPathTokens.

	aString isAbsolutePathName ifTrue: [ ^ tokens ].

	curDirPathComponents _ FmDirectoryEntry currentDirectory pathComponents.
	aString = '.' ifTrue: [ ^ curDirPathComponents copy ].
	aString = '..' ifTrue:  [^ curDirPathComponents allButLast ].

	[ tokens notEmpty and: [ tokens first = '..' ]] whileTrue: [
		curDirPathComponents _ curDirPathComponents allButLast.
		tokens removeFirst ].

	^ curDirPathComponents, tokens! !


!String methodsFor: '*fileman-core-converting' stamp: 'jmv 4/18/2016 11:34'!
asAbsolutePathName
	
	| slash |
	slash _ FmFileIOAccessor default slash.
	^ String streamContents: [ :childPath |
		childPath nextPutAll: slash.
		(FmFileIOAccessor default absolutePathComponentsFor: self)
			do: [ :each | childPath nextPutAll: each]
			separatedBy: [childPath nextPutAll: slash]]! !


!FmEntry methodsFor: 'accessing' stamp: 'jmv 4/18/2016 15:33'!
parent: aFmDirectory

	parent _ aFmDirectory.
	drive _ aFmDirectory drive. "harmless if no drive supported, as in Unix"
	pathComponents _ aFmDirectory pathComponents copyWith: name! !

!FmEntry methodsFor: 'accessing' stamp: 'jmv 4/18/2016 12:10'!
pathComponents
	pathComponents ifNil: [pathComponents _ #() ].
	^pathComponents! !

!FmEntry methodsFor: 'initialize-release' stamp: 'jmv 4/18/2016 15:34'!
pathComponents: tokens detectDrive: detectDrive
	| firstToken  |
	tokens isEmptyOrNil ifTrue: [ ^pathComponents _ nil ].
	(detectDrive and: [ (firstToken _ tokens first) isDriveName])
		ifTrue: [
			self drive: firstToken.
			self basicPathComponents: (tokens copyFrom: 2 to: tokens size)]
		ifFalse: [ self basicPathComponents: tokens ].

	pathComponents ifNotEmpty: [ self name: pathComponents last ]! !

!FmEntry methodsFor: 'initialize-release' stamp: 'jmv 4/18/2016 11:29'!
pathName: aString 
	| tokens guessedDriveName |
	tokens _ FmFileIOAccessor default absolutePathComponentsFor: aString.
	tokens ifEmpty: [^ nil].
	self fileAccessor isDriveSupported
		 ifTrue: [
			guessedDriveName := tokens first asDriveName.
			guessedDriveName ifNotNil: [
				self drive: guessedDriveName.
				tokens := tokens copyFrom: 2 to: tokens size ]].
	self pathComponents: tokens! !


!FmFileIOAccessor methodsFor: 'private' stamp: 'jmv 4/18/2016 10:59'!
basicDirectoryExists: fullPathName

	| result |
	result := self primLookupEntryIn: fullPathName index: 1.
 	^(result == #badDirectoryPath) not! !

!FmFileIOAccessor methodsFor: 'private' stamp: 'jmv 4/18/2016 12:29'!
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
	| entries index done entryArray entry isDirectory lookIn |
	entries := OrderedCollection new: 200.
	index := 1.
	done := false.
	lookIn _ parentEntryOrNil ifNil: [''] ifNotNil: [parentEntryOrNil pathName].
	[done] whileFalse: [
		entryArray := self primLookupEntryIn: lookIn index: index.
		#badDirectoryPath == entryArray ifTrue: [
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

!methodRemoval: FmFileIOAccessor #asAbsolutePathName:!
FmFileIOAccessor removeSelector: #asAbsolutePathName:!
