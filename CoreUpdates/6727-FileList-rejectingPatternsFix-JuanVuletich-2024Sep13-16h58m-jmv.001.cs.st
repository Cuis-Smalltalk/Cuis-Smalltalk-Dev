'From Cuis7.1 [latest update: #6719] on 13 September 2024 at 5:00:22 pm'!

!FileList methodsFor: 'volume list and pattern' stamp: 'jmv 9/13/2024 16:58:41'!
listForSelectingPatterns: patternsThatSelect rejectingPatterns: patternsThatReject
	"Make the list be those file names which match the patterns."

	| sizePad selected newList namePad sizeWithCommasPad font |
	directory ifNil: [^#()].
	directory invalidateChildren.
	selected := Set new.
	patternsThatSelect do: [ :selPat |
		directory childrenDo: [ :entry |
			(entry isDirectory
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

