'From Cuis 4.1 of 12 December 2012 [latest update: #1616] on 25 February 2013 at 4:57:33 pm'!

!Integer class reorganize!
('instance creation' basicNew byte1:byte2:byte3:byte4: initializedInstance new new:neg: readBaseOneFrom: readFrom: readFrom:base:)
('prime numbers' largePrimesUpTo: largePrimesUpTo:do: primesUpTo: primesUpTo:do: verbosePrimesUpTo: verbosePrimesUpTo:do:)
('class initialization' initialize initializeLowBitPerByteTable)
('compatibility' readFrom:radix:)
!


!HaloMorph reorganize!
('WiW support' morphicLayerNumber)
('accessing' haloBox: setTarget: target target:)
('drawing' drawOn:)
('dropping/grabbing' startDrag:with:)
('events' mouseButton3Down:localPosition:)
('event handling' popUpFor:event: staysUpWhenMouseIsDownIn: transferHalo:localPosition:)
('events-processing' containsPoint:event: rejectsEvent:)
('geometry testing' isOrthoRectangularMorph)
('handles' addCollapseHandle: addDebugHandle: addDismissHandle: addDragHandle: addDupHandle: addFontEmphHandle: addFontSizeHandle: addGrabHandle: addGrowHandle: addHelpHandle: addMenuHandle: addRecolorHandle: addRotateHandle: positionIn:horizontalPlacement:verticalPlacement:)
('initialization' defaultColor initialize)
('stepping' step)
('testing' stepTime)
('private' addHandle: addHandles addNameString: basicBox doDebug:with: doDrag:with: doDup:with: doGrab:with: doGrow:with: doMenu:with: doRecolor:with: doRot:with: endInteraction handleSize maybeCollapse:with: maybeDismiss:with: maybeDoDup:with: mouseDownInCollapseHandle:with: removeAllHandlesBut: setDismissColor:with: startGrow:with: startRot:with:)
('forward to target' chooseEmphasisOrAlignment chooseFont deleteBalloon mouseDownOnHelpHandle:)
!


!FileStream class reorganize!
('instance creation' fileNamed: forceNewFileNamed: fullName: isAFileNamed: new newFileNamed: oldFileNamed: oldFileOrNoneNamed: readOnlyFileNamed:)
('concrete classes' concreteStream)
('browser requests' post:target:url:ifError: post:url:ifError: requestURL:target: requestURLStream: requestURLStream:ifError:)
('dnd requests' requestDropStream:)
('class initialization' unload)
('directory services' creationAndModificationTimeOf:)
!


!ChangeList class reorganize!
('public access' browseContents: browsePackageContents: browseRecent:on: browseRecentLog browseRecentLogOn: browseRecentLogOn:startingFrom: browseRecentLogOnPath:)
('fileIn/Out' browseMCZContents: fileReaderServicesForFile:suffix: serviceContents serviceMCZContents servicePackageContents serviceRecentChanges)
('class initialization' unload)
!

