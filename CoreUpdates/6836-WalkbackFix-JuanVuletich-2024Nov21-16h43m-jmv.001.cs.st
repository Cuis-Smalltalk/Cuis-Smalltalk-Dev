'From Cuis7.1 [latest update: #6835] on 21 November 2024 at 4:44:37 pm'!

!HierarchyBrowserWindow class methodsFor: 'GUI building' stamp: 'jmv 11/21/2024 16:21:03'!
onClass: aClass selector: aSymbol
	"Open a new full browser set to class.
	HierarchyBrowserWindow onClass: Morph selector: #submorphs
	HierarchyBrowserWindow onClass: Morph selector: nil
	"

	| newBrowser |
	newBrowser := HierarchyBrowser for: aClass selector: aSymbol.
	self openNoSysCat: newBrowser label: newBrowser labelString.
	newBrowser assureSelectionsShow.
	^newBrowser! !

