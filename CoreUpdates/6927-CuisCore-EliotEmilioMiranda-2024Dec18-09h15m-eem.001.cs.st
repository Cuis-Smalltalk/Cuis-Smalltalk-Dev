'From Cuis7.3 [latest update: #6926] on 18 December 2024 at 9:17:49 am'!

!FileList methodsFor: 'file list' stamp: 'eem 12/18/2024 09:15:55'!
fileList
	"Answer the list of display strings for the files in the current volume."

	| font namePad sizePad sizeWithCommasPad |
	font := Preferences at: #standardListFont.
	namePad := list inject: 0 into: [ :mx :entry | mx max: (font widthOfString: entry name)].
	sizePad := (list inject: 0 into: [ :mx :entry | entry fileSize ifNil: [mx] ifNotNil: [:fs| mx max: fs]]) printString size.
	sizeWithCommasPad := (list inject: 0 into: [ :mx :entry | mx max: entry fileSize]) printStringWithCommas size.
	^list collect: [ :e |
		self fileNameFormattedFrom: e
			namePad: namePad
			sizePad: sizePad
			sizeWithCommasPad: sizeWithCommasPad ].! !

