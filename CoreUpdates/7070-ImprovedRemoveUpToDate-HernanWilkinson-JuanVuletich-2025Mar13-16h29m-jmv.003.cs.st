'From Cuis7.3 [latest update: #7069] on 13 March 2025 at 4:48:28 pm'!

!CodeFileBrowserWindow methodsFor: 'menu commands' stamp: 'jmv 3/13/2025 16:41:58'!
removeUnmodifiedClasses
	model removeUnmodifiedClasses! !


!CodeFileBrowserWindow methodsFor: 'GUI building' stamp: 'jmv 3/13/2025 16:38:35'!
optionalButtonTuples
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
			) ].! !

!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 3/13/2025 16:46:30'!
classListMenuSpec
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
			) ].! !

!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 3/13/2025 16:38:03'!
codeFileListMenuSpec
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
	^answer! !


!PseudoClass methodsFor: 'testing' stamp: 'HAW 3/6/2025 18:56:12'!
hasChanges

	self sourceCode isEmpty ifFalse:[^true].
	self organization hasNoComment ifFalse:[^true].
	definition ifNotNil: [ self realClass ifNotNil: [ :realClass | realClass definition = definition ifFalse: [ ^true]]].
	metaClass ifNotNil: [ ^metaClass hasChanges].
	^false! !

!methodRemoval: PseudoClass #removeUpToDateMethods stamp: 'jmv 3/13/2025 16:39:15'!
PseudoClass removeSelector: #removeUpToDateMethods!
!methodRemoval: PseudoClass #removeIfUpToDate: stamp: 'jmv 3/13/2025 16:39:20'!
PseudoClass removeSelector: #removeIfUpToDate:!
!methodRemoval: CodeFileBrowserWindow #removeUpToDate stamp: 'jmv 3/13/2025 16:39:11'!
CodeFileBrowserWindow removeSelector: #removeUpToDate!
!methodRemoval: CodeFileBrowser #removeUpToDate stamp: 'jmv 3/13/2025 16:39:13'!
CodeFileBrowser removeSelector: #removeUpToDate!

!CodeFileBrowserWindow reorganize!
('GUI building' buildMorphicWindow optionalButtonTuples windowColor)
('menu commands' browseMethodFull browseVersions findClass methodInheritance removeUnmodifiedClasses)
('menu & kbd shortcuts' classListMenuSpec codeFileListMenuSpec messageCatListMenuSpec messageListMenuSpec)
!

