'From Cuis7.1 [latest update: #6793] on 28 October 2024 at 3:37:11 pm'!

!CodeFileBrowser methodsFor: 'change list' stamp: 'VGF 10/28/2024 15:34:04'!
browseCodeFileChanges

	caseCodeSource browseFileChanges.! !


!CodeFile methodsFor: 'change list' stamp: 'VGF 10/28/2024 15:34:42'!
browseFileChanges

	ChangeList browseContents: (FileEntry withAbsolutePathName: fullName).! !


!CodePackageFile methodsFor: 'change list' stamp: 'VGF 10/28/2024 15:35:19'!
browseFileChanges

	ChangeList browsePackageContents: (FileEntry withAbsolutePathName: fullName).! !


!CodeFileBrowserWindow methodsFor: 'menu & kbd shortcuts' stamp: 'VGF 10/28/2024 15:30:23'!
classListMenuSpec
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^ model baseCodeSource isLiveSmalltalkImage
		ifTrue: [
			#(
				#(0 		'Class List' 			false)
				#(10		'definition' 					nil 	model 			editClass 						editFindReplaceIcon)
				#(20		'comment' 					nil 	model 			editComment 						editFindReplaceIcon)
				#(30      'show hierarchically' 							nil   model          enableListClassesHierarchically 									  goTopIcon)		
				#(40 		'show alphabetically' 					nil 	model 			enableListClassesAlphabetically 								 sendReceiveIcon)
				#(50 		'-----')
				#(60		'browse full (b)' 					$b 	window 			browseMethodFull 						editFindReplaceIcon)
				#(70		'class refs (N)' 					$N 	window 			browseClassRefs 						classIcon)
				#(80 		'-----')
				#(90		'fileIn' 					nil 	model 			fileInClass 						updateIcon)
				#(100		'fileOut' 					nil 	model 			fileOutClass 						fileOutIcon)
				#(110    'browse changes'					       nil   model          browseCodeFileChanges  changesIcon)
				#(120 		'-----')
				#(130		'rename...' 					nil 	model 			renameClass 						saveAsIcon)
				#(140		'remove' 					nil 	model 			removeClass 						listRemoveIcon)
				#(150 		'-----')
				#(160		'remove existing' 					nil 	model 			removeClass 						deleteIcon)
			)]
		ifFalse: [
			#(
				#(0 		'Class List' 			false)
				#(10		'definition' 					nil 	model 			editClass 						editFindReplaceIcon)
				#(20		'comment' 					nil 	model 			editComment 						editFindReplaceIcon)
				#(30      'show hierarchically' 							nil   model          enableListClassesHierarchically 									  goTopIcon)		
				#(40 		'show alphabetically' 					nil 	model 			enableListClassesAlphabetically 								 sendReceiveIcon)
				#(50 		'-----')
				#(80		'fileOut' 					nil 	model 			fileOutClass 						fileOutIcon)
				#(90 		'-----')
				#(100		'rename...' 					nil 	model 			renameClass 						saveAsIcon)
				#(110		'remove' 					nil 	model 			removeClass 						listRemoveIcon)
				#(120 		'-----')
				#(130		'remove existing' 					nil 	model 			removeClass 						deleteIcon)
			) ].! !


!CodeFileBrowser reorganize!
('accessing' acceptedStringOrText baseCodeSource baseCodeSource: caseCodeSource caseCodeSource: contents:notifying: contentsSymbol)
('class list' classList createHierarchyTreeOf: hierarchicalClassList renameClass selectedClass)
('diffs' methodDiffFor:selector: modifiedClassDefinition)
('edit pane' selectedBytecodes selectedMessage)
('fileIn/fileOut' fileIn fileInClass fileInMessage fileInMessageCategories fileOut fileOutClass fileOutMessage fileOutMessageCategories)
('infoView' annotation extraInfo infoViewContents updateInfoView)
('message list' messageList)
('metaclass' selectedClassOrMetaClass setClassOrganizer)
('other' changeMessageCategories: didCodeChangeElsewhere labelString)
('removing' removeClass removeMessage removeMessageCategory removeUnmodifiedCategories removeUnmodifiedClasses removeUnmodifiedMethods)
('shout styling' allowsStylingFor:isTextDirty:)
('private' pvtBaseClassOrMetaclass pvtBaseSelectedMessageSourceCodeFor: pvtCaseSelectedMessageSourceCodeFor:)
('controls' whatToShowSubmenuSpec)
('change list' browseCodeFileChanges)
!


!CodeFile reorganize!
('accessing' classDictionary classNamed: classes description fullName fullName: name organization removeClass: removeDoIts renameClass:to: sourceSystem summary)
('change record types' classComment: classDefinition: classRemoval: doIt: method: preamble:)
('fileIn/fileOut' askForDoits fileIn fileInDoits fileOut fileOutDoits: fileOutOn:)
('initialization' initialize)
('reading' buildFrom: buildFrom:informingTo:)
('testing' isLiveSmalltalkImage)
('xtras' allMethodReferences)
('private' baseLabel classDefinition:with: getClass: metaClassDefinition:with: msgClassComment:with: possibleSystemSource: removedMethod:with:)
('change list' browseFileChanges)
!


!CodePackageFile reorganize!
('services' install)
('accessing' classesToRemove description featureSpec methodsToRemove packageDescription packageName provides requires summary)
('change record types' doIt: possibleDescription: provides: requires:)
('initialization' buildFileStream:packageName:fullName: initialize)
('change list' browseFileChanges)
!

