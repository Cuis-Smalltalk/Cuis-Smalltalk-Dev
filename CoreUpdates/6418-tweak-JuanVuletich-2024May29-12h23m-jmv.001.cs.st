'From Cuis7.1 [latest update: #6417] on 29 May 2024 at 12:24:07 pm'!

!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 5/29/2024 11:30:19'!
changeSetListMenuSpec
	"Spec for the menu and shortcut keys the changesets list.
	Each entry is
		#('menu item label'    shortcut    receiverGetter    selector     iconOrNil(optional)    'help text (optional)')
		- label is a string or symbol. Symbol means self updating menu entry.
		- shortcut is a Character or nil
		- receiverGetter sent to the widget to get the actual receiver for selector.
			Tipically #window or #model. If nil, receiver is the widget itself.
		- selector is #aSymbol. No arguments.
		- isEnabledSelector if not nil, it is #aSymbol, and is sent to the receiver (see receierGetter) to ask whether to enable the item.
		- icon may be nil. It is also optional (but must be there if there is help text)
		- help text is optional
	Entries of size 2 are separators, unless first character is $X, meaning 'remove menu item'.
	An entry of size 3 (usually with key 0 and at the first position), if present, tells a menu title, whether to #addStayUpIcons.
	"
	| answer |
	answer := #(
			#(0 		'Change Set' 						true)
			#(10 		'File out and remove (o)' 							$o 	model 			fileOutAndRemove 							nil 				fileOutIcon)
			#(20 		'File out and keep (k)' 							$k 	model 			fileOutAndKeep 							nil 				fileOutIcon)
			#(30 		'-----')
			#(40 		'Rename change set (R)' 					 		$R 	model 			rename 							csIsForBase 				saveAsIcon)
			#(50 		'Destroy change set (x)' 			 				$x 	model 			remove 							csIsForBase 				warningIcon)
	).
	model currentCanHavePreambleAndPostscript ifTrue: [
		model currentHasPreamble
			ifTrue: [ answer  := answer, #(
				#(60 		'-----')
				#(70 		'Edit preamble (p)' 						$p 	model 			addPreamble 							nil 				textEditorIcon)
				#(80 		'Remove preamble' 						nil 	model 			removePreamble 							nil 				listRemoveIcon)) ]
			ifFalse: [ answer  := answer, #(
				#(60 		'-----')
				#(70 		'Add preamble (p)' 						$p 	model 			addPreamble 							nil 				listAddIcon) )].
		model currentHasPostscript
			ifTrue: [ answer  := answer, #(
				#(90 		'Edit postscript...' 						nil 	model 			editPostscript 							nil 				textEditorIcon)
				#(100 		'Remove postscript' 						nil 	model 			removePostscript 							nil 				listRemoveIcon)) ]
			ifFalse: [ answer  := answer, #(
				#(90 		'Add postscript...' 						nil 	model 			editPostscript 							nil 				listAddIcon) )].
	].

	answer  := answer, #(
		#(110 		'-----')
		#(120 		'conflicts with other change sets' 								nil 	window			browseMethodConflicts 							nil 				emblemImportantIcon
			'Browse all methods that occur both in this change set and in at least one other change set.')
		#(130 		'-----')
		#(140 		'trim history' 								nil 	model			trimHistory 							csIsForBase 				clockIcon
			'Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes.
 NOTE: can cause confusion if later filed in over an earlier version of these changes.')
		#(150 		'view affected class categories' 								nil 	model 			viewAffectedClassCategories 								nil 			packageIcon
			'Show class categories affected by any contained change')
	).
	^answer! !

