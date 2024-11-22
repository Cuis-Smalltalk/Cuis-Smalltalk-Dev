'From Cuis7.1 [latest update: #6837] on 22 November 2024 at 10:31:53 am'!

!SmalltalkEditor methodsFor: 'menu & kbd shortcuts' stamp: 'jmv 11/22/2024 10:29:06'!
refactoringsSubmenuSpec
	" 'Refactorings' SubMenu"

	^#(
		#(2010 	'-----')
		#(2020	'Rename... (R)' 						$R 	editor 	contextualRename 						saveAsIcon 	'Renames what is under cursor')
		#(2030	'Extract as Parameter... (V)' 			$V 	editor 	contextualExtractAsParameter 		listAddIcon 	'Extracts the selected code as parameter')
		#(2040	'Add Parameter... (A)' 				$A 	editor 	contextualAddParameter 				listAddIcon 	'Adds parameter to message that is under cursor')
		#(2050	'Remove Parameter... (S)' 			$S 	editor 	contextualRemoveParameter 			listRemoveIcon 'Removes parameter to message that is under cursor')
		#(2060 	'-----')
		#(2070	'Extract Temporary... (J)' 				$J 	editor 	extractToTemporary 					saveAsIcon 	'Extracts the selected code into a temporary variable')
		#(2080	'Extract Method... (K)' 				$K 	editor 	extractMethod 							saveAsIcon 	'Extracts the selected code into a separate method')
		#(2090 	'-----')
		#(2100	'Inline Temporary... (O)' 				$O 	editor 	inlineTemporaryVariable 				saveAsIcon 	'Inlines the selected temporary variable into each usage')
		#(2110	'Inline Method... (L)' 					$L 	editor 	contextualInlineMethod 				saveAsIcon  	'Inlines the selected message send into desired colaborations')
		#(2120 	'-----')
		#(2130	'Temporary to Instance Variable (G)' 	$G 	editor 	temporaryToInstanceVariable 			saveAsIcon 	'Extracts temporary variable to instance variable')
		#(2140	'Push Up Instance Variable' 			nil 	editor 	contextualPushUpInClassDefinition 	goTopIcon)
		#(2150	'Push Down Instance Variable' 		nil 	editor 	contextualPushDownInClassDefinition 	goBottomIcon)
	).! !

