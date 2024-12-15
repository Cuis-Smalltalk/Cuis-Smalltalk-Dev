'From Cuis7.3 [latest update: #6912] on 15 December 2024 at 7:04:31 pm'!

!HierarchyBrowserWindow class methodsFor: 'GUI building' stamp: 'KLG 12/12/2024 16:04:08'!
onClass: aClass selector: aSymbol
	"Open a new full browser set to class.
	HierarchyBrowserWindow onClass: Morph selector: #submorphs
	HierarchyBrowserWindow onClass: Morph selector: nil
	"

	| newBrowser newBrowserWindow |
	newBrowser := HierarchyBrowser for: aClass selector: aSymbol.
	newBrowserWindow := self openNoSysCat: newBrowser label: newBrowser labelString.
	newBrowser assureSelectionsShow.
	^ newBrowserWindow! !

