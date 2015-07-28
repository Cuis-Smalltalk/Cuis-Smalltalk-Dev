'From Cuis 4.2 of 25 July 2013 [latest update: #2435] on 28 July 2015 at 8:22:05.735533 am'!

!DisplayScreen methodsFor: 'copying' stamp: 'jmv 7/28/2015 08:21'!
copy: destRectangle from: sourcePt in: sourceForm rule: rule
	super copy: destRectangle from: sourcePt in: sourceForm rule: rule.
	DisplayScreen screenUpdateRequired: destRectangle! !

!DisplayScreen methodsFor: 'copying' stamp: 'jmv 7/28/2015 08:21'!
copyBits: sourceForm at: destOrigin translucent: factor
	super copyBits: sourceForm at: destOrigin translucent: factor.
	DisplayScreen screenUpdateRequired: (destOrigin extent: sourceForm extent)! !

!DisplayScreen methodsFor: 'copying' stamp: 'jmv 7/28/2015 08:21'!
copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule
	super copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule.
	DisplayScreen screenUpdateRequired: clipRect! !

!DisplayScreen methodsFor: 'copying' stamp: 'jmv 7/28/2015 08:21'!
copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule map: map
	super copyBits: sourceRect from: sourceForm at: destOrigin clippingBox: clipRect rule: rule map: map.
	DisplayScreen screenUpdateRequired: clipRect! !

!DisplayScreen methodsFor: 'copying' stamp: 'jmv 7/28/2015 08:21'!
eraseShape: bwForm
	super eraseShape: bwForm.
	DisplayScreen screenUpdateRequired: nil! !

!DisplayScreen methodsFor: 'copying' stamp: 'jmv 7/28/2015 08:21'!
fill: aRectangle rule: anInteger fillColor: aForm 
	super fill: aRectangle rule: anInteger fillColor: aForm.
	DisplayScreen screenUpdateRequired: aRectangle! !

!DisplayScreen methodsFor: 'copying' stamp: 'jmv 7/28/2015 08:21'!
fillFromXYColorBlock: colorBlock
	super fillFromXYColorBlock: colorBlock.
	DisplayScreen screenUpdateRequired: nil! !

!DisplayScreen methodsFor: 'copying' stamp: 'jmv 7/28/2015 08:21'!
fillShape: aShapeForm fillColor: aColor at: location
	super fillShape: aShapeForm fillColor: aColor at: location.
	DisplayScreen screenUpdateRequired: nil! !

