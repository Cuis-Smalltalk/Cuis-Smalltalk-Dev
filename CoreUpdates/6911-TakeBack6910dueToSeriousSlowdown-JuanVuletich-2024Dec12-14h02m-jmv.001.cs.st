'From Cuis7.3 [latest update: #6910] on 12 December 2024 at 2:05:26 pm'!
!classDefinition: #FileListWindow category: #'Morphic-Tool Windows'!
SystemWindow subclass: #FileListWindow
	instanceVariableNames: 'fileListPane '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tool Windows'!

!FileList methodsFor: 'as yet unclassified' stamp: 'jmv 10/10/2024 11:34:24'!
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


!FileList methodsFor: 'file list' stamp: 'jmv 11/21/2024 12:33:58'!
fileList
	"Answer the list of display strings for the files in the current volume."

	| font namePad sizePad sizeWithCommasPad |
	font := Preferences at: #standardListFont.
	namePad := list inject: 0 into: [ :mx :entry | mx max: (font widthOfString: entry name)].
	sizePad := (list inject: 0 into: [ :mx :entry | mx max: entry fileSize]) printString size.
	sizeWithCommasPad := (list inject: 0 into: [ :mx :entry | mx max: entry fileSize]) printStringWithCommas size.
	^list collect: [ :e |
		self fileNameFormattedFrom: e
			namePad: namePad
			sizePad: sizePad
			sizeWithCommasPad: sizeWithCommasPad ].! !


!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 6/4/2024 13:33:43'!
morphicFileListPane

	^(PluggableListMorph
		withModel: model 
		listGetter: #fileList 
		indexGetter: #fileListIndex
		indexSetter: #fileListIndex: )
			menuNshortcutsKey: #fileListMenuSpec.! !

!methodRemoval: FileListWindow #fileList stamp: 'jmv 12/12/2024 14:04:16'!
FileListWindow removeSelector: #fileList!
!methodRemoval: FileListWindow #fileListIndex stamp: 'jmv 12/12/2024 14:04:18'!
FileListWindow removeSelector: #fileListIndex!
!methodRemoval: FileListWindow #fileListIndex: stamp: 'jmv 12/12/2024 14:04:19'!
FileListWindow removeSelector: #fileListIndex:!
!methodRemoval: FileListWindow #update: stamp: 'jmv 12/12/2024 14:04:13'!
FileListWindow removeSelector: #update:!
!methodRemoval: FileListWindow #layoutSubmorphs stamp: 'jmv 12/12/2024 14:04:09'!
FileListWindow removeSelector: #layoutSubmorphs!
!methodRemoval: FileListWindow #dateTimeStringFor: stamp: 'jmv 12/12/2024 14:04:14'!
FileListWindow removeSelector: #dateTimeStringFor:!
!methodRemoval: FileListWindow #fileNameFormattedFor:nameWidth:sizeWidth: stamp: 'jmv 12/12/2024 14:04:22'!
FileListWindow removeSelector: #fileNameFormattedFor:nameWidth:sizeWidth:!
!classDefinition: #FileListWindow category: #'Morphic-Tool Windows'!
SystemWindow subclass: #FileListWindow
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tool Windows'!
