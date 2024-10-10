'From Cuis7.1 [latest update: #6765] on 10 October 2024 at 11:39:27 am'!

!FileSystemEntry methodsFor: 'testing' stamp: 'jmv 10/10/2024 11:32:20'!
isDirectoryEntry
	^false! !

!FileSystemEntry methodsFor: 'testing' stamp: 'jmv 10/10/2024 11:32:28'!
isFileEntry
	^false! !


!DirectoryEntry methodsFor: 'testing' stamp: 'jmv 10/10/2024 11:32:23'!
isDirectoryEntry
	^true! !


!FileEntry methodsFor: 'testing' stamp: 'jmv 10/10/2024 11:32:32'!
isFileEntry
	^true! !


!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 10/10/2024 11:34:24'!
fileNameFormattedFrom: entry namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad
	"entry is a 5-element array of the form:
		(name creationTime modificationTime dirFlag fileSize)"
	| sizeStr nameStr paddedNameStr dateStr someSpaces sizeDigits sizeDigitsAndCommas spacesToAdd font spaceWidth |
	font := Preferences at: #standardListFont.
	spaceWidth := font widthOf: $ .
	nameStr := entry isDirectoryEntry
		ifTrue: [ entry name , self folderString ]
		ifFalse: [ entry name ].
	spacesToAdd := namePad - (font widthOfString: nameStr) // spaceWidth.
	paddedNameStr := nameStr ,
		(String
			new: spacesToAdd
			withAll: $ ).
	dateStr := (entry modificationTime date printFormat: #(3 2 1 $/ 1 1 2 )) , '  ' ,
		(String streamContents: [ :s |
			entry modificationTime time
				print24: true
				showSeconds: true
				on: s ]).
	sizeDigits := entry fileSize printString size.
	sizeStr := entry fileSize printStringWithCommas.
	sizeDigitsAndCommas := sizeStr size.
	spacesToAdd := sizeWithCommasPad - sizeDigitsAndCommas.
	"Usually a space takes the same space as a comma, and half the space of a digit.
	Pad with 2 spaces for each missing digit and 1 space for each missing comma"
	(font widthOf: Character space) ~= (font widthOf: $, )
		ifTrue: [spacesToAdd := spacesToAdd + sizePad - sizeDigits max: 0].
	sizeStr := (String new: spacesToAdd withAll: $ ) , sizeStr.
	someSpaces := String new: 6 withAll: $ .
	"
	sortMode = #name ifTrue: [ ^ paddedNameStr , someSpaces , '( ' , dateStr , someSpaces , sizeStr , ' )' ].
	sortMode = #date ifTrue: [ ^ '( ' , dateStr , someSpaces , sizeStr , ' )' , someSpaces , nameStr ].
	sortMode = #size ifTrue: [ ^ '( ' , sizeStr , someSpaces , dateStr , ' )' , someSpaces , nameStr ].
	"
	^ paddedNameStr , someSpaces , '( ' , dateStr , someSpaces , sizeStr , ' )' .! !

!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 10/10/2024 11:34:30'!
listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject
	"Make the list be those file names which match the patterns."

	| sizePad selected newList namePad sizeWithCommasPad font |
	directory ifNil: [^#()].
	directory invalidateChildren.
	selected := Set new.
	patternsThatSelect do: [ :selPat |
		directory childrenDo: [ :entry |
			(entry isDirectoryEntry
				ifTrue: [ showDirsInFileList ]
				ifFalse: [ 
					(self doesPattern: selPat allow: entry) and: [
							patternsThatReject noneSatisfy: [ :rejPat | rejPat match: entry name ]]])
						ifTrue: [ selected add: entry ]]].
		
	newList := selected asArray sort: self sortBlock.
	font := Preferences at: #standardListFont.
	namePad := newList inject: 0 into: [ :mx :entry | mx max: (font widthOfString: entry name)].
	sizePad := (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printString size.
	sizeWithCommasPad := (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printStringWithCommas size.
	newList := newList collect: [ :e |
		self fileNameFormattedFrom: e namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad ].
	^ newList.! !

!FileList methodsFor: 'private' stamp: 'jmv 10/10/2024 11:34:39'!
sortBlock
	"Answer block to decide what order to display the directory entries."

	^ [ :xx :yy | | x y |
		(xx isDirectoryEntry == yy isDirectoryEntry) 
			ifTrue: [  
				sortAscending
					ifTrue: [ x := xx. y := yy ]
					ifFalse: [ x := yy. y := xx ].
				"sort by user-specified criterion"
				sortMode = #name 
					ifTrue: [x name <= y name]
					ifFalse: [ sortMode = #date
						ifTrue: [ x modificationTime = y modificationTime
								ifTrue: [ x name <= y name ]
								ifFalse: [ x modificationTime <= y modificationTime ] ]
						ifFalse: [ "size"
							x fileSize = y fileSize 
								ifTrue: [ x name <= y name ]
								ifFalse: [ x fileSize <= y fileSize ] ] ] ]
			ifFalse: [
				"directories always precede files"
				xx isDirectoryEntry ] ]! !


!FileIOAccessor methodsFor: 'private' stamp: 'jmv 10/10/2024 11:35:59'!
updateEntry: aFileSystemEntry
	| entryArray index lookIn isDirectory |

	"If the index in aFileSystemEntry is valid, use it. No need to iterate over all entries."
	aFileSystemEntry primEntryInParent ifNotNil: [ :tentativeIndex |
		(self primFixedNameLookupEntryIn: aFileSystemEntry parent pathName asUtf8Bytes index: tentativeIndex) ifNotNil: [ :found |
	 		found == #badDirectoryPath ifFalse: [
				aFileSystemEntry name = (found at: 1) ifTrue: [
					aFileSystemEntry updateFrom: found entryInParent: tentativeIndex.
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
				aFileSystemEntry updateFrom: entryArray entryInParent: index ].
			"If found, exit even if invalid. No point to keep iterating."
			^ self ].
		index := index + 1] repeat! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'jmv 10/10/2024 11:34:58'!
directoryNamesIn: aDirectoryEntry
	"
	FileIOAccessor default directoryNamesIn: 'C:\Windows' asDirectoryEntry
	"
	
	^(self entriesIn: aDirectoryEntry)
		select: [ :each | each isDirectoryEntry]
		thenCollect: [ :each | each name]! !

!FileIOAccessor methodsFor: 'utilities' stamp: 'jmv 10/10/2024 11:35:42'!
fileNamesIn: aDirectoryEntry
	"
	FileIOAccessor default fileNamesIn: 'C:\Windows' asDirectoryEntry
	"
	
	^(self entriesIn: aDirectoryEntry)
		select: [ :each | each isFileEntry ]
		thenCollect: [ :each | each name ]! !


!DirectoryEntry methodsFor: 'enumeration' stamp: 'jmv 10/10/2024 11:33:08'!
allFilesDo: aBlock
	self childrenDo: 
		[:child | 
		child isFileEntry ifTrue: [aBlock value: child] ifFalse: [child allFilesDo: aBlock]]! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'jmv 10/10/2024 11:33:14'!
allFilesDo: aBlock matches: selectionBlock
	self childrenDo: 
		[:child | 
		child isFileEntry
			ifTrue: [(selectionBlock value: child) ifTrue: [aBlock value: child]]
			ifFalse: [child allFilesDo: aBlock matches: selectionBlock]]! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'jmv 10/10/2024 11:33:34'!
directoriesDo: aBlock
	self childrenDo: [ :each |
		each isDirectoryEntry ifTrue: [
			aBlock value: each ]]! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'jmv 10/10/2024 11:33:46'!
filesDo: aBlock
	self childrenDo: [ :each |
		each isFileEntry ifTrue: [
			aBlock value: each ]]! !

!DirectoryEntry methodsFor: 'enumeration' stamp: 'jmv 10/10/2024 11:34:02'!
regularDirectoriesDo: aBlock
	self childrenDo: [ :each |
		each isDirectoryEntry ifTrue: [
			each isRegularDirectory ifTrue: [
				aBlock value: each ]]]! !

!DirectoryEntry methodsFor: 'accessing' stamp: 'jmv 10/10/2024 11:33:25'!
directories
	^self children select: [:each | each isDirectoryEntry]! !

!DirectoryEntry methodsFor: 'accessing' stamp: 'jmv 10/10/2024 11:33:42'!
files
	^self children select: [:each | each isFileEntry]! !


!FileEntry methodsFor: 'actions-file' stamp: 'jmv 10/10/2024 11:39:14'!
copyTo: filename 
	| targetEntry |
	
	targetEntry := filename asFileEntry.
	"
	targetEntry isDirectory
		ifTrue: [ targetEntry := targetEntry // self name ].
	"
	self assureExistence.
	targetEntry assureExistence.
	self fileAccessor copy: self to: targetEntry! !

