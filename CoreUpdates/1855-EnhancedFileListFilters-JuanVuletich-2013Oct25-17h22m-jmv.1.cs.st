'From Cuis 4.2 of 25 July 2013 [latest update: #1854] on 25 October 2013 at 5:27:55.398802 pm'!
!classDefinition: #FileList category: #'Tools-FileList'!
TextProvider subclass: #FileList
	instanceVariableNames: 'acceptedContentsCache fileName directory list listIndex pattern sortMode brevityState sortAscending showDirsInFileList currentDirectorySelected fileSelectionBlock dirSelectionBlock '
	classVariableNames: 'FileReaderRegistry '
	poolDictionaries: ''
	category: 'Tools-FileList'!

!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 10/24/2013 18:43'!
doesPattern: pattern allow: entry

	^(pattern = '*' or: [ pattern match: entry name ]) and: [
		"Hide Mac resurce forks and folder service stores"
		(entry name = '.DS_Store') not and: [
		('._*' match: entry name) not ]]! !

!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 10/25/2013 17:26'!
listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject
	"Make the list be those file names which match the patterns."

	| sizePad selected newList namePad sizeWithCommasPad font |
	directory ifNil: [^#()].
	selected _ Set new.
	patternsThatSelect do: [ :pat |
		directory entries do: [ :entry |
			(entry isDirectory
				ifTrue: [ showDirsInFileList ]
				ifFalse: [ self doesPattern: pat allow: entry])
					ifTrue: [ selected add: entry ]]].
	newList _ selected copy.
	patternsThatReject do: [ :pat |
		selected do: [ :entry |
			(entry isDirectory not and: [ pat match: entry]) ifTrue: [
				newList remove: entry ]]].
		
	newList _ newList asArray sort: self sortBlock.
	font _ Preferences standardListFont.
	namePad _ newList inject: 0 into: [ :mx :entry | mx max: (font widthOfString: entry name)].
	sizePad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printString size.
	sizeWithCommasPad _ (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) asStringWithCommas size.
	newList _ newList collect: [ :e |
		self fileNameFormattedFrom: e namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad ].
	^ newList! !


!FileList methodsFor: 'initialization' stamp: 'jmv 10/24/2013 18:35'!
initialize

	showDirsInFileList _ false! !

!FileList methodsFor: 'private' stamp: 'jmv 10/24/2013 18:34'!
directoryNamesFor: item
	"item may be file directory or server directory"
	^item directoryNames! !

!FileList methodsFor: 'private' stamp: 'jmv 10/24/2013 19:10'!
updateFileList
	"Update my files list with file names in the current directory  
	that match the pattern.
	The pattern string may have embedded newlines or semicolons; these separate different patterns."
	| patterns patternsThatReject patternsThatSelect |
	patterns _ OrderedCollection new.
	(pattern findTokens: (String with: Character cr with: Character lf with: $;))
		do: [ :each |
			(each includes: $*) | (each includes: $?)
				ifTrue: [ patterns add: (each copyReplaceAll: '?' with: '#')]
				ifFalse: [
					each isEmpty
						ifTrue: [ patterns add: '*']
						ifFalse: [ patterns add: '*' , each , '*']]].
	"A pattern that starts with $/ is used to reject entries
	Rejecting patterns are applied after selecting patterns."
	patternsThatSelect _ patterns reject: [ :any | any first = $/ ].
	patternsThatSelect isEmpty ifTrue: [ patternsThatSelect add: '*' ]. 
	patternsThatReject _ patterns select: [ :any | any first = $/ ] thenCollect: [ :each | each copyFrom: 2 to: each size ].
	list _ self listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject.
	listIndex _ 0.
	fileName _ nil.
	acceptedContentsCache _ ''.
	self changed: #fileList.
	self changed: #updateButtonRow! !

!methodRemoval: FileList #listForPatterns:!
FileList removeSelector: #listForPatterns:!
!classDefinition: #FileList category: #'Tools-FileList'!
TextProvider subclass: #FileList
	instanceVariableNames: 'acceptedContentsCache fileName directory list listIndex pattern sortMode brevityState sortAscending showDirsInFileList currentDirectorySelected'
	classVariableNames: 'FileReaderRegistry'
	poolDictionaries: ''
	category: 'Tools-FileList'!
