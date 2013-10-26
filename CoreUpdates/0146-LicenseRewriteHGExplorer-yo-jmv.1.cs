'From Cuis 1.0 of 6 March 2009 [latest update: #5989] on 23 March 2009 at 7:43:13 pm'!!Object methodsFor: 'accessing' stamp: 'yo 8/27/2008 23:16'!customizeExplorerContents	^ false.! !!Collection methodsFor: 'enumerating' stamp: 'yo 8/27/2008 23:45'!explorerContents	^self explorerContentsWithIndexCollect: [:value :index |		ObjectExplorerWrapper			with: value			name: index printString			model: self]! !!Collection methodsFor: 'enumerating' stamp: 'yo 8/27/2008 23:29'!explorerContentsWithIndexCollect: twoArgBlock	^ self asOrderedCollection withIndexCollect: twoArgBlock! !!ObjectExplorer methodsFor: 'accessing' stamp: 'jmv 3/23/2009 19:41'!explorerFor: anObject 
	| window listMorph |
	rootObject := anObject.
	window := (OldSystemWindow labelled: (rootObject printStringLimitedTo: 64)) model: self.
	window addMorph: (listMorph := OldSimpleHierarchicalListMorph 
						on: self
						list: #getList
						selected: #getCurrentSelection
						changeSelected: #noteNewSelection:
						menu: #genericMenu:
						keystroke: #explorerKey:from:)
		frame: (0 @ 0 corner: 1 @ 0.8).
	window 
		addMorph: ((OldPluggableTextMorph 
				on: self
				text: #trash
				accept: #trash:
				readSelection: #contentsSelection
				menu: #codePaneMenu:shifted:) askBeforeDiscardingEdits: false)
		frame: (0 @ 0.8 corner: 1 @ 1).
	listMorph autoDeselect: false.
	^window! !!ObjectExplorerWrapper methodsFor: 'as yet unclassified' stamp: 'yo 8/27/2008 23:39'!contents	(item customizeExplorerContents) ifTrue: [^item explorerContents].	"For all others, show named vars first, then indexed vars"	^(item class allInstVarNames asOrderedCollection withIndexCollect: [:each :index |		self class			with: (item instVarAt: index)			name: each			model: item			parent: self]) ,	((1 to: item basicSize) collect: [:index |		self class			with: (item basicAt: index)			name: index printString			model: item			parent: self])! !!OldSimpleButtonMorph methodsFor: 'visual properties' stamp: 'jmv 3/23/2009 19:37'!updateVisualState: evt		oldColor ifNotNil: [		 self color: 			((self containsPoint: evt cursorPoint)				ifTrue: [oldColor mixed: 0.5 with: Color white]				ifFalse: [oldColor])]! !!SequenceableCollection methodsFor: 'accessing' stamp: 'yo 8/27/2008 23:17'!customizeExplorerContents	^ true.! !!Set methodsFor: 'explorer' stamp: 'yo 8/27/2008 23:09'!hasContentsInExplorer	^self notEmpty! !!Dictionary methodsFor: 'accessing' stamp: 'yo 8/27/2008 23:16'!customizeExplorerContents	^ true.! !!Dictionary methodsFor: 'user interface' stamp: 'yo 8/27/2008 23:44'!explorerContentsWithIndexCollect: twoArgBlock	| sortedKeys |	sortedKeys _ self keys asSortedCollection: [:x :y |		((x isString and: [y isString])			or: [x isNumber and: [y isNumber]])			ifTrue: [x < y]			ifFalse: [x class == y class				ifTrue: [x printString < y printString]				ifFalse: [x class name < y class name]]].	^ sortedKeys collect: [:k | twoArgBlock value: (self at: k) value: k].! !Dictionary removeSelector: #explorerContents!Set removeSelector: #explorerContents!SequenceableCollection removeSelector: #explorerContents!ObjectExplorer removeSelector: #label!