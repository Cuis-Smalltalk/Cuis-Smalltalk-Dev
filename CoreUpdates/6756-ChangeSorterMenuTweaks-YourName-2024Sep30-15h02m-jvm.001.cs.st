'From Cuis7.1 [latest update: #6755] on 30 September 2024 at 3:12:27 pm'!

!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jvm 9/30/2024 15:07:49'!
changeSetListMenuSpec
	"Spec for the menu and shortcut keys for the changesets list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
			#(0 		'Change Set' 						true)
			#(10 		'File out + Start an empty one (o)' 							$o 	model 			fileOutAndRemove 								fileOutIcon)
			#(20 		'File out + keep adding to same (k)' 							$k 	model 			fileOutAndKeep 								fileOutIcon)
			#(30 		'-----')
			#(40 		'Rename change set (R)' 					 		$R 	model 			rename 								saveAsIcon)
			#(50 		'Destroy change set (x)' 			 				$x 	model 			remove 								warningIcon)
	).
	model currentCanHavePreambleAndPostscript ifTrue: [
		model currentHasPreamble
			ifTrue: [ answer  := answer, #(
				#(60 		'-----')
				#(70 		'Edit preamble (p)' 						$p 	model 			addPreamble 								textEditorIcon)
				#(80 		'Remove preamble' 						nil 	model 			removePreamble 								listRemoveIcon)) ]
			ifFalse: [ answer  := answer, #(
				#(60 		'-----')
				#(70 		'Add preamble (p)' 						$p 	model 			addPreamble 								listAddIcon) )].
		model currentHasPostscript
			ifTrue: [ answer  := answer, #(
				#(90 		'Edit postscript...' 						nil 	model 			editPostscript 								textEditorIcon)
				#(100 		'Remove postscript' 						nil 	model 			removePostscript 								listRemoveIcon)) ]
			ifFalse: [ answer  := answer, #(
				#(90 		'Add postscript...' 						nil 	model 			editPostscript 								listAddIcon) )].
	].

	answer  := answer, #(
		#(110 		'-----')
		#(120 		'Conflicts with other change sets' 								nil 	window			browseMethodConflicts 								emblemImportantIcon
			'Browse all methods that occur both in this change set and in at least one other change set.')
		#(130 		'-----')
		#(140 		'Trim history' 								nil 	model			trimHistory 								clockIcon
			'Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes.
 NOTE: can cause confusion if later filed in over an earlier version of these changes.')
		#(150 		'View affected class categories' 								nil 	model 			viewAffectedClassCategories 								packageIcon
			'Show class categories affected by any contained change')
	).
	model csIsForBase ifFalse: [ "Maybe disable a few: Copy them the same as above, but without selector."
		answer := answer, #(
			#(40 		'Rename change set (R)' 					 		$R 	model 			nil 								saveAsIcon)
			#(50 		'Destroy change set (x)' 			 				$x 	model 			nil 								warningIcon)
			#(140 		'Trim history' 							nil 	model			nil 								clockIcon)) ].
	^answer! !

!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jvm 9/30/2024 15:11:07'!
classListMenuSpec
	"Spec for the menu and shortcut keys for the class list.
	See coment at Morph >> #getMenu
	"
	^#(
			#(0 		'Class List' 						false)
			#(10 		'Delete class from change set (d)' 								$d 	model 			forgetClass 						warningIcon)
			#(20 		'Remove class from system (x)' 								$x 	model 			removeClass 						deleteIcon)
			#(30 		'-----')
			#(40 		'Browse full (b)' 								$b 	window 			browseMethodFull 						editFindReplaceIcon)
			#(50 		'browse hierarchy (h)' 								$h 	window 			browseHierarchy 						goTopIcon)
			#(60 		'browse protocol (p)' 								$p 	window 			browseFullProtocol 						spreadsheetIcon)
			#(70 		'-----')		
			#(80 		'Class refs (N)' 								$N 	window 			browseClassRefs 						classIcon)
	).
! !

!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jvm 9/30/2024 15:12:03'!
messageListMenuSpec
	"Spec for the menu and shortcut keys for the message list.
	See coment at Morph >> #getMenu
	"
	^#(
			#(0 		'Message List' 						true)
			#(10 		'Delete method from change set (d)' 									$d 	model 			forget 									warningIcon)
			#(20 		'Remove method from system (x)' 									$x 	model 			removeMessage 									deleteIcon)
			#(30 		'-----')
			#(40 		'Browse full (b)' 									$b 	window 			browseMethodFull 									editFindReplaceIcon)
			#(50 		'Browse hierarchy (h)' 									$h 	window 			browseHierarchy 									goTopIcon)
			#(60 		'Browse method (O)' 									$O 	window 			openSingleMessageBrowser 									scriptIcon)
			#(80 		'-----')		
			#(90 		'FileOut (o)' 									$o 	model 			fileOutMessage 									fileOutIcon)
			#(100 		'-----')		
			#(110		'Senders of... (n)' 									nil 	window 			browseSendersOfMessages 									mailForwardIcon)
			#(120		'Implementors of... (m)' 									nil 	window 			browseImplementorsOfMessages 									developmentIcon)
			#(130 		'Inheritance (i)' 									$i 	window 			methodInheritance 									goDownIcon)
			#(140 		'Versions (v)' 									$v 	window 			browseVersions 									clockIcon)

			"Shortcut only. No menu entry."
			#(991		nil 									$m 	window 			browseImplementors)
			#(992		nil 									$n 	window 			browseSenders)
	).
! !

