'From Cuis7.9 [latest update: #8325] on 5 November 2026 at 5:45:05 pm'!

!DateAndTime class methodsFor: 'smalltalk-80' stamp: 'jmv 5/Nov/2026, 7:42:38 pm (UTC)'!
fromSeconds: seconds offset: anOffset
	"Answer a DateAndTime since the Squeak epoch: 1 January 1901"

	| integerSeconds nanos |
	integerSeconds := seconds truncated.
	nanos := integerSeconds = seconds
		ifTrue: [ 0 ]
		ifFalse: [ (seconds - integerSeconds * Time nanosInSecond) asInteger ].
	^ self
		julianDayNumber: Time squeakEpoch
		seconds: integerSeconds
		nanoseconds: nanos
		offset: anOffset! !


!FileSystemEntry methodsFor: 'private' stamp: 'jmv 5/Nov/2026, 7:43:08 pm (UTC)'!
updateFrom: primitiveArray entryInParent: index now: aDateTime
	primEntryInParent := index.
	lastSync := aDateTime ifNil: [DateAndTime now].
	exists := true.
	creationTime := DateAndTime fromSeconds: (primitiveArray at: 2) offset: lastSync offset.
	modificationTime := DateAndTime fromSeconds: (primitiveArray at: 3) offset: lastSync offset.! !


!DirectoryEntry methodsFor: 'private' stamp: 'jmv 5/Nov/2026, 8:38:29 pm (UTC)'!
updateFrom: primitiveArray entryInParent: index now: aDateTime
	super updateFrom: primitiveArray entryInParent: index now: aDateTime.
	children := nil.	"lazy initialization"! !


!FileEntry methodsFor: 'private' stamp: 'jmv 5/Nov/2026, 8:38:35 pm (UTC)'!
updateFrom: primitiveArray entryInParent: index now: aDateTime
	super updateFrom: primitiveArray entryInParent: index now: aDateTime.
	fileSize := primitiveArray at: 5! !


!DateAndTime class methodsFor: 'smalltalk-80' stamp: 'jmv 5/Nov/2026, 8:42:58 pm (UTC)'!
fromSeconds: seconds 
	"Answer a DateAndTime since the Squeak epoch: 1 January 1901"

	^self fromSeconds: seconds offset: self localOffset! !


!FileIOAccessor methodsFor: 'private' stamp: 'jmv 5/Nov/2026, 8:41:27 pm (UTC)'!
entriesIn: parentEntry
	"
	Warning: Private. Only to be called from within FileMan.
	Accepts nil as argument, but behavior depends on platform.
	parentEntryOrNil can only be nil if #isDriveSupported:

Windows (parentEntry = nil means root)
FileIOAccessor default entriesIn: nil #(C:\ D:\)
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(\$Recycle.Bin \Config.Msi \Documents and Settings \gratMusic \hiberfil.sys \Intel \pagefile.sys \PerfLogs \Program Files \Program Files (x86) \ProgramData \Python27 \Recovery \SimuloHoy \System Volume Information \totalcmd \Users \Windows)

Linux  (parentEntry = nil means current dir, like '' and '.')
FileIOAccessor default entriesIn: nil #(Lots of stuff in current directory)
(FileIOAccessor default entriesIn: nil) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
(FileIOAccessor default entriesIn: '' asDirectoryEntry) = (FileIOAccessor default entriesIn: '.' asDirectoryEntry) true
FileIOAccessor default entriesIn: '/' asDirectoryEntry #(/vmlinuz /boot /sbin /srv /lib /lib32 /tmp /sys /home /etc /initrd.img /bin /dev /opt /proc /lost+found /var /root /lib64 /mnt /usr /run /media)

macOS - parentEntry = nil answers an empty array
FileIOAccessor default entriesIn: nil #() .

	"
	| entries index done entryArray entry isDirectory lookIn lookForDrives now |
	parentEntry isNil
		ifTrue: [
			self isDriveSupported ifFalse: [ self error: 'Only for platforms that have the concept of Disk Drive' ].
			lookIn := ''.
			lookForDrives := true ]
		ifFalse: [
			lookIn := parentEntry pathName.
			lookForDrives := false ].
	
	entries := OrderedCollection new.
	index := 1.
	done := false.
	now := DateAndTime now.
	[done] whileFalse: [
		entryArray := self primFixedNameLookupEntryIn: lookIn asUtf8Bytes index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^#()].
		entryArray == nil
			ifTrue: [done := true]
			ifFalse: [
				isDirectory := entryArray at: 4.
				entry := isDirectory ifTrue: [DirectoryEntry new] ifFalse: [FileEntry new].
				lookForDrives
					ifTrue: [entry driveName: (entryArray at: 1)]
					ifFalse: [entry name: (entryArray at: 1) parent: parentEntry].
				entry updateFrom: entryArray entryInParent: index now: now.
				entries addLast: entry ].
		index := index + 1].

	^entries asArray! !

!FileIOAccessor methodsFor: 'private' stamp: 'jmv 5/Nov/2026, 8:40:46 pm (UTC)'!
updateEntry: aFileSystemEntry
	| now entryArray index lookIn isDirectory |

	now := DateAndTime now.
	
	"If the index in aFileSystemEntry is valid, use it. No need to iterate over all entries."
	aFileSystemEntry primEntryInParent ifNotNil: [ :tentativeIndex |
		(self primFixedNameLookupEntryIn: aFileSystemEntry parent pathName asUtf8Bytes index: tentativeIndex) ifNotNil: [ :found |
	 		found == #badDirectoryPath ifFalse: [
				aFileSystemEntry name = (found at: 1) ifTrue: [
					aFileSystemEntry updateFrom: found entryInParent: tentativeIndex now: now.
					^ self ]]]].

	"Otherwise, do a full iteration"
	lookIn := aFileSystemEntry parent pathName.
	index := 1.
	[
		entryArray := self primFixedNameLookupEntryIn: lookIn asUtf8Bytes index: index.
		#badDirectoryPath == entryArray ifTrue: [
			^ self].
		entryArray == nil ifTrue: [
			^ self].
		isDirectory := entryArray at: 4.
		aFileSystemEntry name = (entryArray at: 1) ifTrue: [
			isDirectory == aFileSystemEntry isDirectoryEntry ifTrue: [
				aFileSystemEntry updateFrom: entryArray entryInParent: index now: now ].
			"If found, exit even if invalid. No point to keep iterating."
			^ self ].
		index := index + 1] repeat! !


!FileSystemEntry methodsFor: 'private' stamp: 'jmv 5/Nov/2026, 8:39:49 pm (UTC)'!
updateFrom: primitiveArray entryInParent: index
	self updateFrom: primitiveArray entryInParent: index now: DateAndTime now.! !

!methodRemoval: FileEntry #updateFrom:entryInParent: stamp: 'jmv 5/Nov/2026, 8:39:36 pm (UTC)'!
FileEntry removeSelector: #updateFrom:entryInParent:!
!methodRemoval: DirectoryEntry #updateFrom:entryInParent: stamp: 'jmv 5/Nov/2026, 8:39:34 pm (UTC)'!
DirectoryEntry removeSelector: #updateFrom:entryInParent:!
