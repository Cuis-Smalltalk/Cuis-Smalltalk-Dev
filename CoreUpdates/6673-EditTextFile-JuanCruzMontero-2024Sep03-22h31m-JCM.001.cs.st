'From Cuis7.0 [latest update: #6452] on 3 September 2024 at 10:52:15 pm'!

!FileList methodsFor: 'own services' stamp: 'JCM 9/3/2024 22:35:59'!
serviceViewContentsInTextEditor
	"Answer a service for viewing the contents of a file in a TextEditor"
	
	^ SimpleServiceEntry 
		provider: self 
		label: 'Text Editor with contents' 
		selector: #viewContentsInTextEditor
		description: 'open a new TextEditor whose contents are set to the contents of this file'
		icon: #textEditorIcon! !

!FileList methodsFor: 'own services' stamp: 'JCM 9/3/2024 22:35:15'!
viewContentsInTextEditor
	"View the contents of my selected file in a new text editor"

	| fileEntry fileContents textEditor|
	fileEntry := directory // fileName.
	fileContents := fileEntry textContents.
	
	textEditor := TextEditor open.
	textEditor model actualContents:fileContents.
	textEditor forceSaveContentsTo: fileEntry.! !


!FileList methodsFor: 'file list menu' stamp: 'JCM 9/3/2024 22:41:18'!
itemsForAnyFile2
	"Answer a list of universal services that could apply to any file"
	
	| services |
	services := OrderedCollection new.
	(#(fullHex briefHex needToGetFullHex needToGetBriefHex) includes: brevityState) ifFalse: [
		services add: self serviceGetHex ].
	services add: self serviceCopyName.
	services add: self serviceViewContentsInWorkspace.
	services add: self serviceViewContentsInTextEditor.
	^ services! !

