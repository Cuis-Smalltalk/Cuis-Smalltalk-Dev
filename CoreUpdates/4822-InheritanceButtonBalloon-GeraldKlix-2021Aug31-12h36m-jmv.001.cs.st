'From Cuis 5.0 [latest update: #4821] on 31 August 2021 at 12:39:07 pm'!

!CodeWindow methodsFor: 'GUI building' stamp: 'KLG 8/31/2021 12:53:05'!
optionalButtonTuples
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"

	| aList textConstructor |
	textConstructor _ [ :string :backgroundColor | 
		string asText addAttribute: (TextBackgroundColor color: backgroundColor) ].

	aList _ #(
	(10	'browse'			browseMethodFull				'view this method in a browser')
	(11	'senders' 			browseSendersOfMessages	'browse senders of...')
	(16	'implementors'		browseMessages				'browse implementors of...')
	(12	'versions'			browseVersions					'browse versions')), 

	(Preferences decorateBrowserButtons
		ifTrue:
			[{{13	. 'inheritance'.	 #methodInheritance. 	'browse method inheritance

', (textConstructor value:'green' value: `Color green muchLighter`),': sends to super
', (textConstructor value: 'tan' value: `Color tan`), ': has override(s)
', (textConstructor value: 'mauve' value: `Color blue muchLighter`), ': both of the above
', (textConstructor value: 'pink' value: `Color red muchLighter`), ': is an override but doesn''t call super
', (textConstructor value: 'pinkish tan' value: `Color r: 0.94 g: 0.823 b: 0.673`), ': has override(s), also is an override but doesn''t call super'}}]
		ifFalse:
			[{#(13	'inheritance'		methodInheritance	'browse method inheritance')}]),

	#(
	(12	'hierarchy'			browseHierarchy				'browse class hierarchy')
	(10	'inst vars'			browseInstVarRefs				'inst var refs...')
	(11	'class vars'			browseClassVarRefs			'class var refs...')
	(10	'show...'				offerWhatToShowMenu		'menu of what to show in lower pane')).

	^ aList! !

