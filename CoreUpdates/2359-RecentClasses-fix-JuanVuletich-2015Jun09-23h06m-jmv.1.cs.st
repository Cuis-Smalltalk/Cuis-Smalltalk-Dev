'From Cuis 4.2 of 25 July 2013 [latest update: #2358] on 10 June 2015 at 6:59:23.943517 am'!

!Browser methodsFor: 'class list' stamp: 'jmv 6/10/2015 06:58'!
recent
	"Let the user select from a list of recently visited classes.  11/96 stp.
	 12/96 di:  use class name, not classes themselves.
	 : dont fall into debugger in empty case"

	| className class recentList |
	recentList _ self class recentClasses select: [:n | Smalltalk includesKey: n].
	recentList size = 0 ifTrue: [^ Smalltalk beep].
	className := (SelectionMenu selections: recentList) startUpMenu.
	className
		ifNil: [^ self].
	class := Smalltalk at: className.
	self selectCategoryForClass: class.
	self selectClass: class! !

