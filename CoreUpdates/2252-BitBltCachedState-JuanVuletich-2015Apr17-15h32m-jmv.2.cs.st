'From Cuis 4.2 of 25 July 2013 [latest update: #2253] on 17 April 2015 at 3:39:08.686855 pm'!

!BitBlt class methodsFor: 'cached state access' stamp: 'jmv 4/17/2015 15:37'!
releaseClassCachedState

	CachedFontColorMaps _ ColorConvertingMaps _ nil! !

!methodRemoval: BitBlt class #recreateColorMaps!
BitBlt class removeSelector: #recreateColorMaps!

!BitBlt class reorganize!
('instance creation' bitPeekerFromForm: bitPokerToForm: destForm:sourceForm:combinationRule:destOrigin:sourceOrigin:extent:clipRect: destForm:sourceForm:fillColor:combinationRule:destOrigin:sourceOrigin:extent:clipRect: toForm:)
('examples' alphaBlendDemo exampleColorMap)
('class initialization')
('cached state access' releaseClassCachedState)
!

