'From Cuis7.3 [latest update: #6908] on 11 December 2024 at 11:12:08 am'!
!classDefinition: #FileListWindow category: #'Morphic-Tool Windows'!
SystemWindow subclass: #FileListWindow
	instanceVariableNames: 'fileListPane '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tool Windows'!

!FileListWindow methodsFor: 'updating' stamp: 'jmv 12/11/2024 11:11:04'!
layoutSubmorphs
	super layoutSubmorphs.
	"We are acting as the model of fileListPane,
	and need it to update, as its items are custom built to available width."
	self changed: #fileList! !

!FileListWindow methodsFor: 'updating' stamp: 'jmv 12/11/2024 11:00:46'!
update: aSymbol
	super update: aSymbol.
	aSymbol = #fileList ifTrue: [
		"We are acting as the model of fileListPane"
		self changed: #fileList ].! !

!FileListWindow methodsFor: 'file list formatting' stamp: 'jmv 12/11/2024 10:50:33'!
dateTimeStringFor: aFileEntry

	^(aFileEntry modificationTime date printFormat: #(3 2 1 $/ 1 1 2 )) , '  ' ,
		(String streamContents: [ :s |
			aFileEntry modificationTime time
				print24: true
				showSeconds: true
				on: s ]).! !

!FileListWindow methodsFor: 'file list formatting' stamp: 'jmv 12/11/2024 10:35:13'!
fileList
	"Answer the list of display strings for the files in the current volume."

	| list font maxDateWidth maxSizeWidth totalWidth |
	list := model fileList.
	totalWidth := fileListPane ifNotNil: [ :l | l viewableWidth ] ifNil: [200]. "Only during pane creation."
	font := Preferences at: #standardListFont.
	maxDateWidth := list inject: 0 into:
		[ :prevMax :fileEntry |
			prevMax max: (font widthOfString: (self dateTimeStringFor: fileEntry))].
	maxSizeWidth := list inject: 0 into:
		[ :prevMax :fileEntry |
			prevMax max: (font widthOfString: fileEntry fileSize printStringWithCommas)].
	^list collect:
		[ :fileEntry |
			self fileNameFormattedFor: fileEntry
				nameWidth: totalWidth - maxDateWidth - maxSizeWidth
				sizeWidth: maxSizeWidth ].! !

!FileListWindow methodsFor: 'file list formatting' stamp: 'jmv 12/11/2024 10:13:30'!
fileListIndex
	^model fileListIndex! !

!FileListWindow methodsFor: 'file list formatting' stamp: 'jmv 12/11/2024 10:13:38'!
fileListIndex: i
	^model fileListIndex: i! !

!FileListWindow methodsFor: 'file list formatting' stamp: 'jmv 12/11/2024 10:06:59'!
fileNameFormattedFor: fileEntry nameWidth: nameWidth sizeWidth: sizeWidth

	| font dateTimeString nameString sizeString w columnSeparator columnSeparatorsWidth space spaceWidth c hairSpace hairSpaceWidth |
	font := Preferences at: #standardListFont.
	columnSeparator := '   '.
	columnSeparatorsWidth := (font widthOfString: columnSeparator) * 2.
	space := Character space asString.
	spaceWidth := font widthOfString: space.
	hairSpace := (Character codePoint: 16r200A) asString.
	hairSpaceWidth := font widthOfString: hairSpace.

	"Adjust file name string"
	nameString := fileEntry name.
	nameString := nameString.
	w := (font widthOfString: nameString) + columnSeparatorsWidth.
	"Truncate if too long"
	w > nameWidth ifTrue: [
		w := (font widthOfString: nameString, '...') + columnSeparatorsWidth.
		[ w > nameWidth and: [nameString notEmpty]] whileTrue: [
			nameString := nameString first: nameString size -1.
			w := (font widthOfString: nameString, '...') + columnSeparatorsWidth ].
		nameString := nameString, '...' ].
	"Pad with spaces and hairSpaces if too short"
	w := (font widthOfString: nameString) + columnSeparatorsWidth.
	w < (nameWidth-spaceWidth) ifTrue: [
		c := nameWidth-w // spaceWidth.
		c timesRepeat: [ nameString := nameString, space ]].
	w := (font widthOfString: nameString) + columnSeparatorsWidth.
	[ w < (nameWidth-hairSpaceWidth) ] whileTrue: [
		nameString := nameString, hairSpace.
		w := (font widthOfString: nameString) + columnSeparatorsWidth ].

	dateTimeString := self dateTimeStringFor: fileEntry.

	sizeString := fileEntry fileSize printStringWithCommas.
	"Pad with spaces and hairSpaces if too short"
	w := font widthOfString: sizeString.
	w < (sizeWidth-spaceWidth) ifTrue: [
		c := sizeWidth-w // spaceWidth.
		c timesRepeat: [ sizeString := space, sizeString ]].
	w := font widthOfString: sizeString.
	[ w < (sizeWidth-hairSpaceWidth) ] whileTrue: [
		sizeString := hairSpace, sizeString.
		w := font widthOfString: sizeString ].

	^nameString, columnSeparator, dateTimeString, columnSeparator, sizeString! !


!FileList methodsFor: 'file list' stamp: 'jmv 12/11/2024 10:32:14'!
fileList
	"Answer the list of files in the current volume."
	^list! !


!FileListWindow methodsFor: 'GUI building' stamp: 'jmv 12/11/2024 10:55:35'!
morphicFileListPane

	^ fileListPane := (PluggableListMorph
		withModel: self
		listGetter: #fileList 
		indexGetter: #fileListIndex
		indexSetter: #fileListIndex: )
			menuNshortcutsKey: #fileListMenuSpec;
			alwaysHideHorizontalScrollbar.! !

!methodRemoval: FileList #fileNameFormattedFrom:namePad:sizePad:sizeWithCommasPad: stamp: 'jmv 12/11/2024 10:55:47'!
FileList removeSelector: #fileNameFormattedFrom:namePad:sizePad:sizeWithCommasPad:!
!classDefinition: #FileListWindow category: #'Morphic-Tool Windows'!
SystemWindow subclass: #FileListWindow
	instanceVariableNames: 'fileListPane'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Morphic-Tool Windows'!
