'From Cuis 5.0 [latest update: #4019] on 11 January 2020 at 3:22:11 pm'!
!classDefinition: #BitBltCanvas category: #'Morphic-Support'!
MorphicCanvas subclass: #BitBltCanvas
	instanceVariableNames: 'port '
	classVariableNames: 'AccessProtect AuxBlitter AuxForm CachedForms '
	poolDictionaries: ''
	category: 'Morphic-Support'!

!BitBltCanvas methodsFor: 'private' stamp: 'jmv 1/11/2020 15:21:45'!
resetEngine
	"Private!! Create a new BitBltCanvasEngine for a new copy."

	engine _ BitBltCanvasEngine toForm: form.
	"Init BitBlt so that the first call to the 'primitiveDisplayString' primitive will not fail"
	engine sourceX: 0; width: 0! !

!classDefinition: #BitBltCanvas category: #'Morphic-Support'!
MorphicCanvas subclass: #BitBltCanvas
	instanceVariableNames: ''
	classVariableNames: 'AccessProtect AuxBlitter AuxForm CachedForms'
	poolDictionaries: ''
	category: 'Morphic-Support'!
