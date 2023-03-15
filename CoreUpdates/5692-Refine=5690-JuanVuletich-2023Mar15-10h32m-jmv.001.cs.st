'From Cuis 6.0 [latest update: #5691] on 15 March 2023 at 10:35:25 am'!

!MenuItemMorph class methodsFor: 'cached state access' stamp: 'jmv 3/15/2023 10:26:04'!
initializeSubMenuMarker

	SubMenuMarker := BitBltCanvas buildArrowOfDirection: #right size: FontFamily defaultPointSize.! !

!methodRemoval: BitBltCanvas class #buildArrowOfDirection:size:depth: stamp: 'jmv 3/15/2023 10:34:12'!
BitBltCanvas class removeSelector: #buildArrowOfDirection:size:depth:!
