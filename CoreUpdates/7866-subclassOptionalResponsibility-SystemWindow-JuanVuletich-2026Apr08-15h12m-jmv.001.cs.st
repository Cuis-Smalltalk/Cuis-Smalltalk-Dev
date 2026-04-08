'From Cuis7.7 [latest update: #7864] on 8 April 2026 at 3:16:11 pm'!

!SystemWindow methodsFor: 'GUI building' stamp: 'jmv 8/Apr/2026 15:15:07'!
buildMorphicWindow
	"Many subclasses redefine this method. But this is not mandatory. Some subclasses choose instead some
	other class creation method that doesn't require #buildMorphicWindow (and not use, for instance,
	#open:label:"

	self subclassOptionalResponsibility! !

