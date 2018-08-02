'From Cuis 5.0 of 7 November 2016 [latest update: #3385] on 1 August 2018 at 5:29:52 pm'!
!classDefinition: #FileSystemEntry category: #'System-FileMan-Core'!
Object subclass: #FileSystemEntry
	instanceVariableNames: 'drive pathComponents parent name creationTime modificationTime primEntryInParent exists lastSync '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-FileMan-Core'!

!FileSystemEntry methodsFor: 'accessing' stamp: 'jmv 8/1/2018 16:15:03'!
name: aString parent: parentEntryOrNil
	name _ aString.
	parentEntryOrNil
		ifNil: [
			self pathString: aString ]
		ifNotNil: [
			parent _ parentEntryOrNil.
			drive _ parentEntryOrNil drive. "harmless if no drive supported, as in Unix"
			pathComponents _ parentEntryOrNil pathComponents copyWith: name ].
	self refresh! !

!FileSystemEntry methodsFor: 'private' stamp: 'jmv 8/1/2018 16:20:13'!
updateFrom: primitiveArray entryInParent: index
	primEntryInParent _ index.
	lastSync _ DateAndTime now.
	exists _ true.
	creationTime _ DateAndTime fromSeconds: (primitiveArray at: 2).
	modificationTime _ DateAndTime fromSeconds: (primitiveArray at: 3)! !

!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 8/1/2018 16:12:40'!
pathComponents: tokens drive: driveStringOrNil
	| firstToken  |
	tokens isEmptyOrNil ifTrue: [ ^pathComponents _ nil ].
	(driveStringOrNil isNil and: [ (firstToken _ tokens first) isDriveName])
		ifTrue: [
			self drive: firstToken.
			pathComponents _ tokens copyFrom: 2 to: tokens size ]
		ifFalse: [
			self drive: driveStringOrNil.
			pathComponents _ tokens ].

	pathComponents ifNotEmpty: [ name _ pathComponents last ].
	self refresh! !

!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 8/1/2018 16:15:35'!
pathString: aString 
	| tokens guessedDriveName |
"esto esta detectando el drive si corresponde y despues pasa nil para que se vuelva a detectar. repasar"
	tokens _ FileIOAccessor default absolutePathComponentsFor: aString.
	tokens ifEmpty: [^ nil].
	self fileAccessor isDriveSupported
		 ifTrue: [
			guessedDriveName _ tokens first asDriveName.
			guessedDriveName ifNotNil: [
"Yo creo que aca habria que llamar a #pathComponents:drive: y salir...."
				self drive: guessedDriveName.
				tokens := tokens copyFrom: 2 to: tokens size ]].
	self pathComponents: tokens drive: nil! !


!FileSystemEntry class methodsFor: 'instance creation' stamp: 'jmv 8/1/2018 16:16:01'!
withPathComponents: comps drive: driveString
	"May exist or not"
	| instance |
	instance _ self new.
	instance pathComponents: comps drive: driveString.
	^instance! !

!FileSystemEntry class methodsFor: 'instance creation' stamp: 'jmv 8/1/2018 16:17:07'!
withPathName: aString
	"May exist or not"
	| instance |
	instance _ self new.
	instance pathString: aString.
	^instance! !


!DirectoryEntry methodsFor: 'private' stamp: 'jmv 8/1/2018 15:46:30'!
updateFrom: primitiveArray entryInParent: index
	super updateFrom: primitiveArray entryInParent: index.
	children _ nil.	"lazy initialization"! !


!FileEntry methodsFor: 'private' stamp: 'jmv 8/1/2018 10:38:53'!
primEntryInParent
	^ primEntryInParent! !

!FileEntry methodsFor: 'private' stamp: 'jmv 8/1/2018 16:20:02'!
updateFrom: primitiveArray entryInParent: index
	super updateFrom: primitiveArray entryInParent: index.
	fileSize _ primitiveArray at: 5! !


!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 7/31/2018 10:36:52'!
fileNameFormattedFrom: entry namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad
	"entry is a 5-element array of the form:
		(name creationTime modificationTime dirFlag fileSize)"
	| sizeStr nameStr paddedNameStr dateStr someSpaces sizeDigits sizeDigitsAndCommas spacesToAdd font spaceWidth |
	font _ Preferences standardListFont.
	spaceWidth _ font widthOf: $ .
	nameStr _ entry isDirectory
		ifTrue: [ entry name , self folderString ]
		ifFalse: [ entry name ].
	spacesToAdd _ namePad - (font widthOfString: nameStr) // spaceWidth.
	paddedNameStr _ nameStr ,
		(String
			new: spacesToAdd
			withAll: $ ).
	dateStr _ (entry modificationTime date printFormat: #(3 2 1 $/ 1 1 2 )) , '  ' ,
		(String streamContents: [ :s |
			entry modificationTime time
				print24: true
				showSeconds: true
				on: s ]).
	sizeDigits _ entry fileSize printString size.
	sizeStr _ entry fileSize printStringWithCommas.
	sizeDigitsAndCommas _ sizeStr size.
	spacesToAdd _ sizeWithCommasPad - sizeDigitsAndCommas.
	"Usually a space takes the same space as a comma, and half the space of a digit.
	Pad with 2 spaces for each missing digit and 1 space for each missing comma"
	(font widthOf: Character space) ~= (font widthOf: $, )
		ifTrue: [spacesToAdd _ spacesToAdd + sizePad - sizeDigits max: 0].
	sizeStr _ (String new: spacesToAdd withAll: $ ) , sizeStr.
	someSpaces _ String new: 6 withAll: $ .
	sortMode = #name ifTrue: [ ^ paddedNameStr , someSpaces , '( ' , dateStr , someSpaces , sizeStr , ' )' ].
	sortMode = #date ifTrue: [ ^ '( ' , dateStr , someSpaces , sizeStr , ' )' , someSpaces , nameStr ].
	sortMode = #size ifTrue: [ ^ '( ' , sizeStr , someSpaces , dateStr , ' )' , someSpaces , nameStr ]! !


!String methodsFor: 'fileman-converting' stamp: 'jmv 8/1/2018 16:17:21'!
asDirectoryEntry
	"See examples in #asFileEntry method comment"
	^DirectoryEntry withPathName: self! !

!String methodsFor: 'fileman-converting' stamp: 'jmv 8/1/2018 16:17:23'!
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
	^FileEntry withPathName: self! !


!FileIOAccessor methodsFor: 'private' stamp: 'jmv 8/1/2018 15:50:23'!
entriesIn: parentEntryOrNil
	"
	Warning: Private. Only to be called from within FileMan.
	Accepts nil as argument, but behavior depends on platform.

Windows (nil means root)
FileIOAccessor default entriesIn: nil #(C:\ D:\)
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(\$Recycle.Bin \Config.Msi \Documents and Settings \gratMusic \hiberfil.sys \Intel \pagefile.sys \PerfLogs \Program Files \Program Files (x86) \ProgramData \Python27 \Recovery \SimuloHoy \System Volume Information \totalcmd \Users \Windows)

Linux  (nil means current dir, like '' and '.')
FileIOAccessor default entriesIn: nil #(Lots of stuff in current directory)
(FileIOAccessor default entriesIn: nil) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(/vmlinuz /boot /sbin /srv /lib /lib32 /tmp /sys /home /etc /initrd.img /bin /dev /opt /proc /lost+found /var /root /lib64 /mnt /usr /run /media)

MacOsX (nil means current dir, like '' and '.')
FileIOAccessor default entriesIn: nil #(/Volumes/SanDisk32-NTFS/CuisTest/2554-REVISAR-JuanVuletich-2015Oct21-16h40m-jmv.1.cs.st /Volumes/SanDisk32-NTFS/CuisTest/Cog.app /Volumes/SanDisk32-NTFS/CuisTest/Cog.app.tgz /Volumes/SanDisk32-NTFS/CuisTest/Cuis4.2-2553.changes /Volumes/SanDisk32-NTFS/CuisTest/Cuis4.2-2553.image /Volumes/SanDisk32-NTFS/CuisTest/CuisV4.sources)
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(/.dbfseventsd /.DocumentRevisions-V100 /.DS_Store /.file /.fseventsd /.hotfiles.btree /.Spotlight-V100 /.Trashes /.vol /Applications /bin /cores /dev /etc /home /installer.failurerequests /Library /net /Network /opt /private /sbin /System /tmp /Users /usr /var /Volumes)

	"
	| entries index done entryArray entry isDirectory lookIn |
	entries _ OrderedCollection new: 200.
	index _ 1.
	done _ false.
	lookIn _ parentEntryOrNil ifNil: [''] ifNotNil: [parentEntryOrNil pathName].
	[done] whileFalse: [
		entryArray _ self primLookupEntryIn: lookIn index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^#()].
		entryArray == nil
			ifTrue: [done _ true]
			ifFalse: [
				isDirectory _ entryArray at: 4.
				entry _ isDirectory ifTrue: [DirectoryEntry new] ifFalse: [FileEntry new].
				entry name: (entryArray at: 1) parent: parentEntryOrNil.
				entry updateFrom: entryArray entryInParent: index.
				entries addLast: entry ].
		index _ index + 1].

	^entries asArray! !


!FileSystemEntry methodsFor: 'private' stamp: 'jmv 8/1/2018 16:16:21'!
ensureParent
	self pathComponents isEmpty
		ifTrue: [^ nil].
	parent _ DirectoryEntry
				withPathComponents: (self pathComponents copyFrom: 1 to: self pathComponents size - 1)
				drive: self drive.
	^ parent! !

!FileSystemEntry methodsFor: 'initialize-release' stamp: 'jmv 8/1/2018 09:19:11'!
refresh
	"Assume we know nothing about current state in the File System.
	This might be because we're just created.
	Or it might be because there is a chance the File System changed and we don't know current state."
	lastSync _ nil.
	exists _ nil.
	creationTime _ nil.
	modificationTime _ nil.! !


!DirectoryEntry methodsFor: 'actions-path' stamp: 'jmv 8/1/2018 16:16:10'!
concatPathComponentsAsDirectory: components
	| entry entryComponents parentEntry |
	components ifEmpty: [ ^self ].
	parentEntry := self isRoot ifFalse: [ self ].
	entryComponents := self pathComponents.

	components do: [ :eachComponent |
		entryComponents := entryComponents copyWith: eachComponent.
		entry := DirectoryEntry withPathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry ].
		parentEntry := entry ].

	^entry! !

!DirectoryEntry methodsFor: 'actions-path' stamp: 'jmv 8/1/2018 16:16:14'!
concatPathComponentsAsFile: components

	| entry entryComponents parentEntry |
	components ifEmpty: [ ^self ].
	parentEntry := self isRoot ifFalse: [ self ].
	entryComponents := self pathComponents.

	components allButLast do: [ :eachComponent |
		entryComponents := entryComponents copyWith: eachComponent.
		entry := DirectoryEntry withPathComponents: entryComponents drive: self drive.
		parentEntry ifNotNil: [
			entry setParent: parentEntry ].
		parentEntry := entry ].

	entryComponents := entryComponents copyWith: components last.
	entry := FileEntry withPathComponents: entryComponents drive: self drive.
	parentEntry ifNotNil: [
		entry setParent: parentEntry ].

	^entry! !


!DirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 8/1/2018 16:17:15'!
currentDirectory
	"Answer the current directory.

	In Unix it is the current directory in the OS shell that started us.
	In Windows the same happens if the image file is in a subree of the Windows current directory.

	But it defaults to the directory in wich this Smalltalk image was started (or last saved) if this fails
	(this usually happens, for example, if the image is dropped on the VM in a Windows explorer).
	See #getCurrentWorkingDirectory

	DirectoryEntry currentDirectory
	"

	CurrentDirectory ifNil: [
		CurrentDirectory _ self withPathName: (Smalltalk getCurrentWorkingDirectory ifNil: [ Smalltalk imagePath ]) ].
	^ CurrentDirectory! !

!DirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 8/1/2018 16:17:17'!
smalltalkImageDirectory
	"Answer the directory on which this Smalltalk image was started (or last saved)

	DirectoryEntry smalltalkImageDirectory
	"

	ImageDirectory ifNil: [
		ImageDirectory _ self withPathName: Smalltalk imagePath ].
	^ ImageDirectory! !

!DirectoryEntry class methodsFor: 'instance creation' stamp: 'jmv 8/1/2018 16:17:19'!
vmDirectory
	"Answer the directory containing the VM that runs us.

	DirectoryEntry vmDirectory
	"

	VMDirectory ifNil: [
		VMDirectory _ self withPathName: Smalltalk vmPath ].
	^ VMDirectory! !


!FileEntry methodsFor: 'initialize-release' stamp: 'jmv 8/1/2018 16:19:41'!
refresh
	super refresh.
	fileSize _ nil! !

!methodRemoval: FileSystemEntry class #pathComponents:!
FileSystemEntry class removeSelector: #pathComponents:!
!methodRemoval: FileSystemEntry class #pathComponents:drive:!
FileSystemEntry class removeSelector: #pathComponents:drive:!
!methodRemoval: FileSystemEntry class #pathName:!
FileSystemEntry class removeSelector: #pathName:!
!methodRemoval: FileSystemEntry #basicPathComponents:!
FileSystemEntry removeSelector: #basicPathComponents:!
!methodRemoval: FileSystemEntry #parent:!
FileSystemEntry removeSelector: #parent:!
!methodRemoval: FileSystemEntry #pathComponents:!
FileSystemEntry removeSelector: #pathComponents:!
!methodRemoval: FileSystemEntry #pathComponents:detectDrive:!
FileSystemEntry removeSelector: #pathComponents:detectDrive:!
!methodRemoval: FileSystemEntry #pathName:!
FileSystemEntry removeSelector: #pathName:!
!classDefinition: #FileSystemEntry category: #'System-FileMan-Core'!
Object subclass: #FileSystemEntry
	instanceVariableNames: 'drive pathComponents parent name primEntryInParent lastSync exists creationTime modificationTime'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'System-FileMan-Core'!
