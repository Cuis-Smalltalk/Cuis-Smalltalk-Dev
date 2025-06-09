'From Cuis7.5 [latest update: #7262] on 9 June 2025 at 9:38:08 am'!

!ChangeSorterWindow methodsFor: 'menu commands' stamp: 'NPM 6/9/2025 09:23:31'!
trimHistory
	
	(self confirm: self trimHistoryConfirmationMessage)
		ifFalse: [ ^ self ].
	
	model trimHistory
	
	! !

!ChangeSorterWindow methodsFor: 'menu commands' stamp: 'NPM 6/9/2025 09:23:19'!
trimHistoryConfirmationMessage
	
	^ String streamContents: [ :confirmationMessage |
		confirmationMessage
			nextPutAll: 'You are about to permanently remove non-essential history,';
			newLine;
			nextPutAll: 'including methods that were added and then removed,';
			newLine;
			nextPutAll: 'as well as renames and reorganizations of newly added classes.';
			newLine;
			newLine;
			nextPutAll: 'This action cannot be undone.';
			newLine;
			newLine;
			nextPutAll: 'Do you want to continue?'
		]! !


!ChangeSorter methodsFor: 'changeSet menu' stamp: 'NPM 6/9/2025 09:28:00'!
trimHistory
	"Drop non-essential history (rename, reorg, method removals) from newly-added classes."

	myChangeSet trimHistory.
	
	self showChangeSet: myChangeSet

! !


!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'NPM 6/9/2025 09:35:33'!
changeSetListMenuSpec
	"Spec for the menu and shortcut keys for the changesets list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
			#(0 		'Change Set' 						true)
			#(10 		'File out, start a new cs (o)' 			$o 	model 		fileOutAndRemove 				fileOutIcon)
			#(20 		'File out, keep current cs (k)' 			$k 	model 		fileOutAndKeep 				fileOutIcon)
			#(30 		'-----')
			#(40 		'Rename change set (R)' 				$R 	model 		rename 						saveAsIcon)
			#(50 		'Destroy change set (x)' 			 	$x 	model 		remove 						warningIcon)
	).
	model currentCanHavePreambleAndPostscript ifTrue: [
		model currentHasPreamble
			ifTrue: [ answer  := answer, #(
				#(60 	'-----')
				#(70 	'Edit preamble (p)' 					$p 	model 		addPreamble 					textEditorIcon)
				#(80 	'Remove preamble' 					nil 	model 		removePreamble 				listRemoveIcon)) ]
			ifFalse: [ answer  := answer, #(
				#(60 	'-----')
				#(70 	'Add preamble (p)' 					$p 	model 		addPreamble 					listAddIcon) )].
		model currentHasPostscript
			ifTrue: [ answer  := answer, #(
				#(90 	'Edit postscript...' 					nil 	model 		editPostscript 					textEditorIcon)
				#(100 	'Remove postscript' 					nil 	model 		removePostscript 				listRemoveIcon)) ]
			ifFalse: [ answer  := answer, #(
				#(90 	'Add postscript...' 					nil 	model 		editPostscript 					listAddIcon) )].
	].

	answer  := answer, #(
		#(110 			'-----')
		#(120 			'Conflicts with other change sets' 		nil 	window		browseMethodConflicts 		emblemImportantIcon
			'Browse all methods that occur both in this change set and in at least one other change set.')
		#(130 			'-----')
		#(140 			'Trim history' 						nil 	window		trimHistory 					clockIcon
			'Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes.
 NOTE: can cause confusion if later filed in over an earlier version of these changes.')
		#(150 			'View affected class categories' 		nil 	model 		viewAffectedClassCategories	packageIcon
			'Show class categories affected by any contained change')
	).
	model csIsForBase ifFalse: [ "Maybe disable a few: Copy them the same as above, but without selector."
		answer := answer, #(
			#(40 		'Rename change set (R)' 				$R 	model 		nil 								saveAsIcon)
			#(50 		'Destroy change set (x)' 		 		$x 	model 		nil 								warningIcon)) ].
	^answer! !

