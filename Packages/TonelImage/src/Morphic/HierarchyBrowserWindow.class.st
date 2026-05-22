"
Morphic view for HierarchyBrowser models. See category 'GUI building'.
"
Class {
	#name : 'HierarchyBrowserWindow',
	#superclass : 'BrowserWindow',
	#category : 'Morphic-Tool Windows',
	#package : 'Morphic',
	#tag : 'Tool Windows'
}

{ #category : 'GUI building' }
HierarchyBrowserWindow class >> browseClassesSatisfying: classBlock title: aTitle [
	"Put up a ClassListBrowser showing all classes that satisfy the classBlock."

	self
		forClassesNamed:
			(Smalltalk allClasses select:
					[:c | (classBlock value: c) == true]
				thenCollect:
					[:c | c name])
		title:
			aTitle
]

{ #category : 'examples' }
HierarchyBrowserWindow class >> example1 [
	"Put up a ClassListBrowser that shows all classes that have the string 'Pluggable' in their names"

	self browseClassesSatisfying: [:cl | cl name includesSubString: 'Pluggable'] title: 'Pluggables'

"HierarchyBrowserWindow example1"
]

{ #category : 'examples' }
HierarchyBrowserWindow class >> example2 [
	"Put up a ClassListBrowser that shows all classes whose names start with the letter S"

	self forClassesNamed: (Smalltalk allClasses collect: [:c | c name] thenSelect: [:aName | aName first == $S]) title: 'All classes starting with S'

"HierarchyBrowserWindow example2"
]

{ #category : 'examples' }
HierarchyBrowserWindow class >> example3 [
	"Put up a ClassListBrowser that shows all Variable classes"

	self browseClassesSatisfying:  [:c | c isVariable] title: 'All Variable classes'

"HierarchyBrowserWindow example3"
]

{ #category : 'examples' }
HierarchyBrowserWindow class >> example4 [
	"Put up a ClassListBrowser that shows all classes implementing more than 100 methods"

	self browseClassesSatisfying:
		[:c | (c selectors size + c class selectors size) > 100] title: 'Classes with more than 100 methods'

"HierarchyBrowserWindow example4"
]

{ #category : 'examples' }
HierarchyBrowserWindow class >> example5 [
	"Put up a ClassListBrowser that shows all classes that lack class comments"

	self
		browseClassesSatisfying: 
			[:c | c organization classComment isEmptyOrNil] 
		title: 'Classes lacking class comments'

"HierarchyBrowserWindow example5"
]

{ #category : 'examples' }
HierarchyBrowserWindow class >> example6 [
	"Put up a ClassListBrowser that shows all classes that have class instance variables"

	self
		browseClassesSatisfying: 
			[:c | c class instVarNames size > 0]
		title:
			'Classes that define class-side instance variables'

"HierarchyBrowserWindow example6"
]

{ #category : 'GUI building' }
HierarchyBrowserWindow class >> forClassesNamed: nameList title: aTitle [
	"Initialize the receiver for the class-name-list and title provided"

	| browser |
	browser := ClassListBrowser new initForClassesNamed: nameList title: aTitle.
	self openNoSysCat: browser label: aTitle

	"HierarchyBrowserWindow forClassesNamed: #(Browser Morph Float) title: 'Frogs'"
]

{ #category : 'GUI building' }
HierarchyBrowserWindow class >> onClass: aClass selector: aSymbol [
	"Open a new full browser set to class.
	HierarchyBrowserWindow onClass: Morph selector: #submorphs
	HierarchyBrowserWindow onClass: Morph selector: nil
	"

	| newBrowser newBrowserWindow |
	newBrowser := HierarchyBrowser for: aClass selector: aSymbol.
	newBrowserWindow := self openNoSysCat: newBrowser label: newBrowser labelString.
	newBrowser assureSelectionsShow.
	^ newBrowserWindow
]
