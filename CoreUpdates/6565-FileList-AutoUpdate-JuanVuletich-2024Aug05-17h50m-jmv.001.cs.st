'From Cuis7.1 [latest update: #6564] on 5 August 2024 at 5:53:11 pm'!

!FileList methodsFor: 'stepping' stamp: 'jmv 8/5/2024 16:47:58'!
stepAt: millisecondSinceLast
	self updateFileList.! !

!FileList methodsFor: 'stepping' stamp: 'jmv 8/5/2024 15:43:45'!
wantsSteps
	^ true! !


!FileListWindow methodsFor: 'stepping' stamp: 'jmv 8/5/2024 17:25:53'!
stepTime
	^ 1000 "milliseconds"! !


!FileList methodsFor: 'initialization' stamp: 'jmv 8/5/2024 16:24:33'!
initialize
	listIndex := 0.
	showDirsInFileList := false.
	showHidden := false.! !

!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 8/5/2024 17:53:01'!
listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject
	"Make the list be those file names which match the patterns."

	| sizePad selected newList namePad sizeWithCommasPad font |
	directory ifNil: [^#()].
	directory invalidateChildren.
	selected := Set new.
	patternsThatSelect do: [ :pat |
		directory childrenDo: [ :entry |
			(entry isDirectory
				ifTrue: [ showDirsInFileList ]
				ifFalse: [ self doesPattern: pat allow: entry])
					ifTrue: [ selected add: entry ]]].
	newList := selected copy.
	patternsThatReject do: [ :pat |
		selected do: [ :entry |
			(entry isDirectory not and: [ pat match: entry name]) ifTrue: [
				newList remove: entry ]]].
		
	newList := newList asArray sort: self sortBlock.
	font := Preferences at: #standardListFont.
	namePad := newList inject: 0 into: [ :mx :entry | mx max: (font widthOfString: entry name)].
	sizePad := (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printString size.
	sizeWithCommasPad := (newList inject: 0 into: [ :mx :entry | mx max: (entry fileSize)]) printStringWithCommas size.
	newList := newList collect: [ :e |
		self fileNameFormattedFrom: e namePad: namePad sizePad: sizePad sizeWithCommasPad: sizeWithCommasPad ].
	^ newList! !

!FileList methodsFor: 'private' stamp: 'jmv 8/5/2024 17:48:03'!
put: aText
	"Private - put the supplied text onto the file"

	| type |
	brevityState == #fullFile ifTrue: [
		acceptedContentsCache := aText.
		directory // fileName writeStreamDo: [ :stream |
			stream nextPutAll: acceptedContentsCache asPlainString ].
		directory invalidateChildren.
		self updateFileList.
		^ true  "accepted"].
	listIndex = 0 ifTrue: [
		self inform: 'No fileName is selected'.
		^ false  "failed"].
	type := 'These'.
	brevityState = #briefFile ifTrue: [type := 'Abbreviated'].
	brevityState = #briefHex ifTrue: [type := 'Abbreviated'].
	brevityState = #fullHex ifTrue: [type := 'Hexadecimal'].
	brevityState = #FileList ifTrue: [type := 'Directory'].
	self inform: ('{1} contents cannot
meaningfully be saved at present.' format:{type}).
	^ false  "failed"
! !

!FileList methodsFor: 'private' stamp: 'jmv 8/5/2024 16:44:13'!
updateFileList
	"Update my files list with file names in the current directory  
	that match the pattern.
	The pattern string may have embedded newlines or semicolons; these separate different patterns."
	| patterns patternsThatReject patternsThatSelect newList |
	patterns := OrderedCollection new.
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
	patternsThatSelect := patterns reject: [ :any | any first = $/ ].
	patternsThatSelect isEmpty ifTrue: [ patternsThatSelect add: '*' ]. 
	patternsThatReject := patterns select: [ :any | any first = $/ ] thenCollect: [ :each | each copyFrom: 2 to: each size ].
	newList := self listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject.

	newList = list ifFalse: [
		"If we had a selection, ensure it is valid, or discard it."
		listIndex = 0 ifFalse: [
			(listIndex > newList size or: [(list at: listIndex) ~= (newList at: listIndex)]) ifTrue: [
				listIndex := newList indexOf: (list at: listIndex) ]].
		list := newList.
		listIndex > 0 ifFalse: [ "Had no selection or it was discarded"
			fileName := nil.
			acceptedContentsCache := ''.
			self acceptedContentsChanged.
			self changed: #fileListIndex ].
		self changed: #fileList.
		self triggerEvent: #updateButtonRow ].! !

