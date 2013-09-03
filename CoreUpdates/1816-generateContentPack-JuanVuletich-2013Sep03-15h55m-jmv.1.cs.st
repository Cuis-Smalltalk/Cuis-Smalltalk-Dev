'From Cuis 4.2 of 25 July 2013 [latest update: #1815] on 3 September 2013 at 3:55:24.187275 pm'!

!Form class methodsFor: 'import / export' stamp: 'jmv 9/3/2013 15:51'!
generateContentPack
	"
	Ensure you have an empty ChangeSet and evaluate

	Form generateContentPack

	Then file out the ChangeSet.
	"
	| forms packClsName |
	packClsName _ #GUIFormsPack.
	ContentPack
		variableSubclass: packClsName
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Morphic-Support'.

	forms _ Dictionary new.
	forms at: 'gradientUpArrow' put: (FormCanvas arrowWithGradientOfDirection: #up).
	forms at: 'gradientDownArrow' put: (FormCanvas arrowWithGradientOfDirection: #down).
	forms at: 'gradientLeftArrow' put: (FormCanvas arrowWithGradientOfDirection: #left).
	forms at: 'gradientRightArrow' put: (FormCanvas arrowWithGradientOfDirection: #right).
	forms at: 'biggerNormalCursor' put: CursorWithAlpha biggerNormal.


	(Smalltalk at: packClsName)
		withDictionary: forms.

	self notify: 'ContentPack created. You can file out current ChangeSet'! !

