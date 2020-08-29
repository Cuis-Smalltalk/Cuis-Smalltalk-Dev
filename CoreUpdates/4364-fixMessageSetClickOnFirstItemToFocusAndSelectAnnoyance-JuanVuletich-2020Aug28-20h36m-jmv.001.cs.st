'From Cuis 5.0 [latest update: #4362] on 28 August 2020 at 8:58:47 pm'!
!methodRemoval: MessageSetWindow #activateAndSendTopToBack: stamp: 'jmv 8/28/2020 20:56:31'!
MessageSetWindow removeSelector: #activateAndSendTopToBack:!

!MessageSetWindow reorganize!
('GUI building' buildLowerPanes buildMorphicWindow windowColor)
('menu building' addExtraMenu2ItemsTo:)
('menu commands' filterMessageList)
!

