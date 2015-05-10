'From Cuis 4.2 of 25 July 2013 [latest update: #2310] on 8 May 2015 at 11:53:51.305116 am'!
!classDefinition: #PointArray category: #'Collections-Balloon'!
IntegerArray variableWordSubclass: #PointArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Balloon'!
!classDefinition: #ShortIntegerArray category: #'Collections-Balloon'!
ArrayedCollection variableWordSubclass: #ShortIntegerArray
	instanceVariableNames: ''
	classVariableNames: 'LastSaveOrder'
	poolDictionaries: ''
	category: 'Collections-Balloon'!
!classDefinition: #ShortPointArray category: #'Collections-Balloon'!
ShortIntegerArray variableWordSubclass: #ShortPointArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Balloon'!
!classDefinition: #ShortWordArray category: #'Collections-Balloon'!
ShortIntegerArray variableWordSubclass: #ShortWordArray
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Collections-Balloon'!
!classDefinition: #ShortRunArray category: #'Collections-Balloon'!
ArrayedCollection variableWordSubclass: #ShortRunArray
	instanceVariableNames: ''
	classVariableNames: 'LastSaveOrder'
	poolDictionaries: ''
	category: 'Collections-Balloon'!

"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points.
Be sure to put any further comments in double-quotes, like this one."
SystemOrganization removeEmptyCategories; sortCategories.
PasteUpMorph allInstancesDo: [ :w | w hideTaskbar ]
