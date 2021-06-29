'From Cuis 5.0 [latest update: #4649] on 29 June 2021 at 11:30:08 am'!

!ObjectExplorerWindow methodsFor: 'building menus' stamp: 'MM 6/29/2021 09:56:27'!
genericMenu
	"Borrow a menu from my inspector"
	
	| aMenu |
	aMenu _ MenuMorph new defaultTarget: self.
	model getCurrentSelection
		ifNil: [
			aMenu
				add: '*nothing selected*'
				target: self
				action: #yourself]
		ifNotNil: [
			aMenu
				addItemsFromDictionaries: `{
					{
						#label 			-> 		'inspect (i)'.
						#selector 			-> 		#inspectSelection.
						#icon 			-> 		#inspectIcon
					} asDictionary.
					{
						#label 			-> 		'explore (I)'.
						#selector 			-> 		#exploreSelection.
						#icon 			-> 		#exploreIcon
					} asDictionary.
					{
						#label 			-> 		'copy to clipboard (c)'.
						#selector 			-> 		#copySelectionToClipboard.
						#icon 			-> 		#copyIcon
					} asDictionary.
					{
						#label 			-> 		'basic inspect'.
						#selector 			-> 		#inspectBasic.
						#icon 			-> 		#inspectIcon
					} asDictionary.
					{
						#label 			-> 		'references finder'.
						#selector 			-> 		#openReferencesFinder.
						#icon 			-> 		#exploreIcon
					} asDictionary.
					{
						#label 			-> 		'weight explorer'.
						#selector 			-> 		#openWeightExplorer.
						#icon 			-> 		#exploreIcon
					} asDictionary.
					nil.
					{
						#label 			-> 		'browse full (b)'.
						#selector 			-> 		#browseMethodFull.
						#icon 			-> 		#editFindReplaceIcon
					} asDictionary.
					{
						#label 			-> 		'browse hierarchy (h)'.
						#selector 			-> 		#browseHierarchy.
						#icon 			-> 		#goTopIcon
					} asDictionary.
					{
						#label 			-> 		'browse protocol (p)'.
						#selector 			-> 		#browseFullProtocol.
						#icon 			-> 		#spreadsheetIcon
					} asDictionary.
				}`.
			model getCurrentSelection ifNotNil: [ :currSel |
				(currSel item is: #Morph) ifTrue: [
					aMenu addLine.
					aMenu add: 'show morph halo' target: currSel item action: #addHalo]].
			aMenu addLine;
				add: 'monitor changes'
				target: self
				action: #monitor:
				argument: model getCurrentSelection.
			model class == ReferencesExplorer ifTrue: [
				aMenu addLine;
					add: 'rescan'
					target: self
					action: #rescan ]].
	model basicMonitorList isEmptyOrNil
		ifFalse: [
			aMenu addLine;
				add: 'stop monitoring all'
				target: self
				action: #stopMonitoring ].
	^ aMenu! !

