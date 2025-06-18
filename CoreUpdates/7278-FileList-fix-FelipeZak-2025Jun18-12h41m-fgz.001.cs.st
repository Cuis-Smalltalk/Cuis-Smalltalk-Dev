'From Cuis7.5 [latest update: #7270] on 18 June 2025 at 12:48:09 pm'!

!FileList methodsFor: 'file list' stamp: 'fgz 6/18/2025 12:44:43'!
fileList
	"Answer the list of display strings for the files in the current volume."

	| font namePad sizePad sizeWithCommasPad |
	font := Preferences at: #standardListFont.
	namePad := list inject: 0 into: [ :mx :entry | mx max: (font widthOfString: entry name)].
	sizePad := (list inject: 0 into: [ :mx :entry | entry fileSize ifNil: [mx] ifNotNil: [:fs| mx max: fs]]) printString size.
	sizeWithCommasPad := (list inject: 0 into: [ :mx :entry | mx max: (entry fileSize ifNil: [0])]) printStringWithCommas size.
	^list collect: [ :e |
		self fileNameFormattedFrom: e
			namePad: namePad
			sizePad: sizePad
			sizeWithCommasPad: sizeWithCommasPad ].! !

