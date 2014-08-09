'From Cuis 4.2 of 25 July 2013 [latest update: #2062] on 6 August 2014 at 9:20:10.8434 am'!

!PluggableMorph methodsFor: 'initialization' stamp: 'jmv 8/6/2014 09:15'!
defaultColor
	"answer the default color/fill style for the receiver"
	^ Color lightGray! !


!InnerHierarchicalListMorph methodsFor: 'drawing' stamp: 'jmv 8/6/2014 09:14'!
drawOn: aCanvas

	Preferences showLinesInHierarchyViews ifTrue:[
		self drawLinesOn: aCanvas ]! !

!methodRemoval: InnerPluggableMorph #defaultColor!
InnerPluggableMorph removeSelector: #defaultColor!
!methodRemoval: ScrollBar #defaultColor!
ScrollBar removeSelector: #defaultColor!
!methodRemoval: PluggableScrollPane #defaultBorderColor!
PluggableScrollPane removeSelector: #defaultBorderColor!
!methodRemoval: PluggableScrollPane #defaultColor!
PluggableScrollPane removeSelector: #defaultColor!
!methodRemoval: PluggableButtonMorph #defaultColor!
PluggableButtonMorph removeSelector: #defaultColor!
!methodRemoval: PluggableMorph #defaultBorderColor!
PluggableMorph removeSelector: #defaultBorderColor!
