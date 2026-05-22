"
Morphic view for CodeFileBrowser models. See category 'GUI building'.
"
Class {
	#name : 'CodeFileBrowserWindow',
	#superclass : 'BrowserWindow',
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'menu commands' }
CodeFileBrowserWindow >> browseMethodFull [
	"Create and schedule a full Browser and then select
	the current class and message."

	|  answer myClass |
	answer := nil.
	(myClass := model selectedClassOrMetaClass) ifNotNil: [
		answer := Smalltalk browseFull: myClass realClass
			selector: model selectedMessageName ].
	^answer
]

{ #category : 'menu commands' }
CodeFileBrowserWindow >> browseVersions [
	"Create and schedule a message set browser on all versions of the 
	currently selected message selector."
	| class selector |
	(selector := model selectedMessageName) ifNotNil: [
		class := model selectedClassOrMetaClass.
		(class exists and: [class realClass includesSelector: selector]) ifTrue: [
			VersionsBrowserWindow
				browseVersionsOf: (class realClass compiledMethodAt: selector)
				class: class realClass theNonMetaClass
				meta: class realClass isMeta
				category: model selectedMessageCategoryName
				selector: selector]]
]

{ #category : 'GUI building' }
CodeFileBrowserWindow >> buildMorphicWindow [
	"Create a pluggable version of all the views for a Browser, using Morphic widgets."

	| sysCatList msgCatList upperPanes clsLayout clsList msgList |
	model systemCategoryList ifNotEmpty: [ 
		model setSelectedSystemCategory: model systemCategoryList first ].
	sysCatList := (PluggableListMorph
			withModel: model 
			listGetter: #systemCategorySingleton
			indexGetter: #indexIsOne 
			indexSetter: #indexIsOne:)
				menuNshortcutsKey: #codeFileListMenuSpec.
	sysCatList hideScrollBarsIndefinitely.
	
	msgCatList := (PluggableListMorph
			withModel: model 
			listGetter: #messageCategoryList
			indexGetter: #messageCategoryListIndex 
			indexSetter: #messageCategoryListIndex:)
				menuNshortcutsKey: #messageCatListMenuSpec.

	clsList := self buildMorphicClassList.
	clsLayout := self buildMorphicClassColumnWith: clsList.
	msgList := self buildMorphicMessageList.

	upperPanes := LinearLayoutMorph newRow.
	upperPanes
		addMorph: clsLayout proportionalWidth: 0.3;
		addAdjusterAndMorph: msgCatList proportionalWidth: 0.3;
		addAdjusterAndMorph: msgList proportionalWidth: 0.4.

	self layoutMorph
		addMorph: sysCatList fixedHeight: (Preferences at: #standardCodeFont) lineSpacing + 10;
		addAdjusterAndMorph: upperPanes proportionalHeight: 0.3;
		addAdjusterAndMorph: self buildLowerPanes proportionalHeight: 0.7.
	model changed: #editSelection
]

{ #category : 'menu & kbd shortcuts' }
CodeFileBrowserWindow >> classListMenuSpec [
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^ model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Class List' 			false)
				#(10	'Definition' 				nil 	model 		editClass 							editFindReplaceIcon)
				#(20	'Comment' 				nil 	model 		editComment 						editFindReplaceIcon)
				#(30      'Show Hierarchically' 		nil   model          enableListClassesHierarchically 	goTopIcon)		
				#(40 	'Show Alphabetically' 	nil 	model 		enableListClassesAlphabetically 	sendReceiveIcon)
				#(50 	'-----')
				#(60	'Browse Full (b)' 			$b 	window 	browseMethodFull 					editFindReplaceIcon)
				#(70	'Class References (N)' 	$N 	window 	browseClassRefs 					classIcon)
				#(80 	'-----')
				#(90	'File In' 					nil 	model 		fileInClass 							updateIcon)
				#(100	'File Out' 				nil 	model 		fileOutClass 						fileOutIcon)
				#(110    'Browse Changes'		nil   model          browseCodeFileChanges  			changesIcon)
				#(120 	'-----')
				#(130	'Rename...' 				nil 	model 		renameClass 						saveAsIcon)
				#(140	'Remove' 				nil 	model 		removeClass 						listRemoveIcon)
				#(150 	'-----')
				#(160	'Remove Up to Date' 		nil 	model 		removeUnmodifiedClasses 			deleteIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Class List' 			false)
				#(10	'Definition' 				nil 	model 		editClass 							editFindReplaceIcon)
				#(20	'Comment' 				nil 	model 		editComment 						editFindReplaceIcon)
				#(30      'Show Hierarchically' 		nil   model          enableListClassesHierarchically 	goTopIcon)		
				#(40 	'Show Alphabetically' 	nil 	model 		enableListClassesAlphabetically 	sendReceiveIcon)
				#(50 	'-----')
				#(80	'File Out' 				nil 	model 		fileOutClass 						fileOutIcon)
				#(90 	'-----')
				#(100	'Rename...' 				nil 	model 		renameClass 						saveAsIcon)
				#(110	'Remove' 				nil 	model 		removeClass 						listRemoveIcon)
				#(120 	'-----')
				#(130	'Remove Up to Date' 		nil 	model 		removeUnmodifiedClasses 			deleteIcon)
			) ].
]

{ #category : 'menu & kbd shortcuts' }
CodeFileBrowserWindow >> codeFileListMenuSpec [
	"Spec for the menu and shortcut keys for the single entry filename list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := 
			#(
				#(0 		'Code File' 			false)
				#(10	'Find Class... (f)' 			$f 	window 	findClass)
				#(20 	'-----') ).
	model baseCodeSource isLiveSmalltalkImage ifTrue: [
			answer := answer, #(
				#(30	'File In' 					nil 	model 		fileIn) )].
	answer := answer, 
			#(
				#(40	'File Out' 				nil 	model 		fileOut) ).
	model caseCodeSource isLiveSmalltalkImage ifFalse: [
		answer := answer, #(
				#(50	'Remove Up to Date' 		nil 	model 		removeUnmodifiedClasses) )].
	^answer
]

{ #category : 'menu commands' }
CodeFileBrowserWindow >> findClass [
	| pattern foundClass classNames index foundCodeFile |
	self okToChange ifFalse: [^ self flash].
	self request: 'Class name?' do: [:aString|
		aString isEmpty ifFalse:
			[pattern := aString asLowercase.
			classNames := Set new.
			classNames addAll: model caseCodeSource classDictionary keys.
			classNames := classNames asArray select: 
				[:n | (n asLowercase indexOfSubCollection: pattern startingAt: 1) > 0].
			classNames isEmpty ifFalse:
				[index := classNames size = 1
					ifTrue: [1]
					ifFalse: [(PopUpMenu labelArray: classNames lines: #()) startUpMenu].
				index = 0 ifFalse:
					[foundCodeFile := nil.
					foundClass := nil.
					(model caseCodeSource classDictionary includesKey: (classNames at: index))
						ifTrue:
							[foundClass := model caseCodeSource classDictionary at: (classNames at: index).
							foundCodeFile := model caseCodeSource].
					foundClass ifNotNil:
						[model setSelectedSystemCategory: foundCodeFile name asSymbol.
						model classListIndex: (model classList indexOf: foundClass name)]]]]]
]

{ #category : 'menu & kbd shortcuts' }
CodeFileBrowserWindow >> menuAndShortcutRegistryRoot: widgetKey [
	"Answer the topmost class whose menu and shortcut registry we honor.
	It is SystemWindow by default.
	But classes redefining menus may want to ignore providers registered assuming menus that they redefine.
	See inheritance."
	(#(classListMenuSpec messageCatListMenuSpec messageListMenuSpec) pointsTo: widgetKey)
		ifTrue: [ ^CodeFileBrowserWindow ].
	^super menuAndShortcutRegistryRoot: widgetKey
]

{ #category : 'menu & kbd shortcuts' }
CodeFileBrowserWindow >> messageCatListMenuSpec [
	"Spec for the menu and shortcut keys for the method categories list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Message Categories' false)
				#(10	'File In' 				nil 	model 		fileInMessageCategories 		updateIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Message Categories' false)
			)].
	answer := answer, 
			#(
				#(20	'File Out' 			nil 	model 		fileOutMessageCategories 		fileOutIcon)
			).
	model caseCodeSource isLiveSmalltalkImage ifFalse: [
		answer := answer, #(
				#(100 	'-----')
				#(100	'Remove Existing' 	nil 	model 		removeUnmodifiedMethods 	deleteIcon)
		) ].
	^answer
]

{ #category : 'menu & kbd shortcuts' }
CodeFileBrowserWindow >> messageListMenuSpec [
	"Spec for the menu and shortcut keys for the messages list.
	See coment at Morph >> #getMenu
	"
	^ model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Messages' 				false)
				#(10	'File In' 					nil 	model 		fileInMessage 			updateIcon)
				#(20	'File Out' 				nil 	model 		fileOutMessage 		fileOutIcon)
				#(30 	'-----')
				#(40	'Senders (n)' 			$n 	window 	browseSenders 		mailForwardIcon)
				#(50	'Implementors (m)' 		$m 	window 	browseImplementors 	developmentIcon)
				#(60	'Method Inheritance (h)' 	$h 	window 	methodInheritance 	goDownIcon)
				#(70	'Browse Full (b)' 			$b 	window 	browseMethodFull 		editFindReplaceIcon)
				#(80	'Versions (v)' 			nil 	window 	browseVersions 		clockIcon)
				#(90 	'-----')
				#(100	'Remove Method (x)' 		$x 	model 		removeMessage 		deleteIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Messages' 				false)
				#(10	'File Out' 				nil 	model 		fileOutMessage 		fileOutIcon)
				#(20 	'-----')
				#(30	'Remove Method (x)' 		$x 	model 		removeMessage 		deleteIcon)
			)].
]

{ #category : 'menu commands' }
CodeFileBrowserWindow >> methodInheritance [
	(model selectedClassOrMetaClass isNil or:
		[model selectedClassOrMetaClass hasDefinition])
			ifFalse: [^super methodInheritance].
	^nil
]

{ #category : 'GUI building' }
CodeFileBrowserWindow >> optionalButtonTuples [
	"Answer a tuple buttons, in the format:
			button label
			selector to send
			help message"
	^ (model baseCodeSource isLiveSmalltalkImage and: [ model caseCodeSource isLiveSmalltalkImage not ])
		ifTrue: [ super optionalButtonTuples, 
			#((17	'remove up to date'		removeUnmodifiedClasses 	'remove up to date methods and classes...')) ]
		ifFalse: [
			"For non-standard browser configurations assume most of the default buttons are invalid"
			#(
			) ].
]

{ #category : 'menu commands' }
CodeFileBrowserWindow >> removeUnmodifiedClasses [
	model removeUnmodifiedClasses
]

{ #category : 'GUI building' }
CodeFileBrowserWindow >> windowColor [
	^ Theme current fileContentsBrowser
]
