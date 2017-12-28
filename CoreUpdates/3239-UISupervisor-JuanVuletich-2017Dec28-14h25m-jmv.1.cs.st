'From Cuis 5.0 of 7 November 2016 [latest update: #3237] on 28 December 2017 at 2:26:10 pm'!
Smalltalk renameClassNamed: #ProjectX as: #UISupervisor!

!SystemDictionary methodsFor: 'miscellaneous' stamp: 'jmv 12/28/2017 14:25:52'!
privateMorphicTopLevelRendererClass
	"The class controlling the outermost rendering process for Morphic and other critical methods for handling interrupts.  Dangerous to change as any bugs will effectively crash the UI for the entire image.
	
	Using 'private' prefix to reflect the danger even though it will likely be called externally."
	^ UISupervisor! !

