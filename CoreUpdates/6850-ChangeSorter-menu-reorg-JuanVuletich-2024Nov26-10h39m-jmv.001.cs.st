'From Cuis7.1 [latest update: #6843] on 26 November 2024 at 10:43:19 am'!

!ChangeSorterWindow methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/26/2024 10:43:13'!
changeSetListMenuSpec
	"Spec for the menu and shortcut keys for the changesets list.
	See coment at Morph >> #getMenu
	"
	| answer |
	answer := #(
			#(0 		'Change Set' 						true)
			#(10 		'File out, start an empty one (o)' 		$o 	model 		fileOutAndRemove 				fileOutIcon)
			#(20 		'File out, keep adding to same (k)' 		$k 	model 		fileOutAndKeep 				fileOutIcon)
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
		#(140 			'Trim history' 						nil 	model		trimHistory 					clockIcon
			'Drops any methods added and then removed, as well as renaming and reorganization of newly-added classes.
 NOTE: can cause confusion if later filed in over an earlier version of these changes.')
		#(150 			'View affected class categories' 		nil 	model 		viewAffectedClassCategories	packageIcon
			'Show class categories affected by any contained change')
	).
	model csIsForBase ifFalse: [ "Maybe disable a few: Copy them the same as above, but without selector."
		answer := answer, #(
			#(40 		'Rename change set (R)' 				$R 	model 		nil 								saveAsIcon)
			#(50 		'Destroy change set (x)' 		 		$x 	model 		nil 								warningIcon)
			#(140 		'Trim history' 						nil 	model		nil 								clockIcon)) ].
	^answer! !

